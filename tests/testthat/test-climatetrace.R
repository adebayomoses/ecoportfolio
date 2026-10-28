test_that("Climate TRACE asset pages are parsed, with owners de-duplicated", {
  page <- jsonlite::fromJSON(test_path("fixtures", "ct_assets_page.json"),
                             simplifyVector = FALSE)
  a <- parse_ct_assets(page$assets, 2022)
  expect_equal(nrow(a), 3)
  expect_equal(a$owner_name, c("RWE Generation UK PLC", "Drax Group PLC", "Uniper SE"))
  expect_equal(a$emissions, c(4111000, 3858200, 3588400))
  expect_equal(a$n_owners, c(1L, 1L, 1L))
  expect_equal(a$share, c(1, 1, 1))
  expect_equal(unique(a$subsector), "electricity-generation")
  expect_equal(nrow(parse_ct_assets(list(), 2022)), 0)
})

test_that("assets without owners and with several owners are handled", {
  assets <- list(
    list(Id = 1, Name = "Orphan", Country = "USA", Sector = "cement",
         EmissionsSummary = list(list(Gas = "co2e_100yr", EmissionsQuantity = 10)),
         Owners = NULL),
    list(Id = 2, Name = "Joint", Country = "USA", Sector = "cement",
         EmissionsSummary = list(list(Gas = "co2", EmissionsQuantity = 5),
                                 list(Gas = "co2e_100yr", EmissionsQuantity = 40)),
         Owners = list(list(CompanyId = "x", CompanyName = "X Inc"),
                       list(CompanyId = "y", CompanyName = "Y Corp"),
                       list(CompanyId = "x", CompanyName = "X Inc")))
  )
  a <- parse_ct_assets(assets, 2023)
  expect_equal(nrow(a), 3)
  expect_true(is.na(a$owner_id[1]))
  expect_equal(a$share[2:3], c(0.5, 0.5))
  expect_equal(a$emissions[2], 40)

  o <- owner_emissions(a)
  expect_equal(o$owner_id, c("x", "y"))
  expect_equal(o$emissions, c(20, 20))
  expect_error(owner_emissions(a[1, ]), "owner")
  expect_error(owner_emissions(data.frame(x = 1)), "ct_assets")
})

test_that("company names normalise to shared keys", {
  expect_equal(normalise_company(c("Drax Group PLC", "drax group", "DRAX GROUP p.l.c")),
               rep("drax group", 3))
  expect_equal(normalise_company("AT&T Inc."), "at and t")
  expect_equal(normalise_company("The Coca-Cola Company"), "coca cola")
  expect_true(is.na(normalise_company(NA)))
})

test_that("holdings are matched by name and alias", {
  h <- climate_portfolio(data.frame(id = 1:3, name = c("Drax Group", "RWE AG", "Tesco PLC"),
                                    weight = c(0.3, 0.3, 0.4), scope1 = c(NA, NA, 99)))
  em <- data.frame(owner_name = c("Drax Group PLC", "RWE Generation UK PLC", "RWE Renewables"),
                   emissions = c(100, 60, 40))
  expect_message(m <- match_holdings(h, em), "Matched 1 of 3")
  expect_equal(m$scope1, c(100, NA, 99))
  al <- data.frame(from = c("RWE Generation UK PLC", "RWE Renewables"), to = "RWE AG")
  m2 <- suppressMessages(match_holdings(h, em, aliases = al))
  expect_s3_class(m2, "climate_portfolio")
  expect_equal(m2$scope1, c(100, 100, 99))
  expect_match(m2$matched_name[2], "RWE Generation UK PLC")
  expect_error(match_holdings(h, em, aliases = data.frame(a = 1)), "from")
  expect_error(match_holdings(h, data.frame(x = 1)), "owner_name")
})

fake_assets <- function(ids) {
  lapply(ids, function(i) {
    list(Id = i, Name = paste("Plant", i), Country = "GBR", Sector = "electricity-generation",
         EmissionsSummary = list(list(Gas = "co2e_100yr", EmissionsQuantity = 100 * i)),
         Owners = list(list(CompanyId = paste0("c", i), CompanyName = paste("Owner", i))))
  })
}

test_that("ct_assets pages through results and sends filters (mocked)", {
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    q <- httr2::url_parse(req$url)$query
    seen[[length(seen) + 1L]] <<- q
    offset <- as.numeric(q$offset)
    ids <- if (offset < 6) offset + seq_len(3) else integer()
    httr2::response_json(body = list(assets = fake_assets(ids)))
  })
  a <- ct_assets(countries = c("GBR", "IRL"), subsectors = "electricity-generation",
                 year = 2022, limit = 5, page_size = 3)
  expect_equal(a$asset_id, as.character(1:5))
  expect_equal(length(seen), 2)
  expect_equal(seen[[1]]$countries, "GBR,IRL")
  expect_equal(seen[[1]]$subsectors, "electricity-generation")
  expect_equal(seen[[2]]$offset, "3")
  expect_null(seen[[1]]$sectors)

  b <- ct_assets(year = 2022, limit = 50, page_size = 3)  # stops on a short page
  expect_equal(nrow(b), 6)
})

test_that("API failures raise a classed, informative error (mocked)", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 500))
  expect_error(ct_sectors(), class = "ecoPortfolio_api_error")
  expect_error(ct_sectors(), "Climate TRACE request failed")
})

test_that("the live Climate TRACE API returns assets", {
  # Opt-in, so CRAN and CI never depend on an external service.
  skip_if_not(identical(Sys.getenv("ECOPORTFOLIO_LIVE_TESTS"), "true"),
              "Set ECOPORTFOLIO_LIVE_TESTS=true to run live API tests")
  skip_if_offline("api.climatetrace.org")
  a <- ct_assets(countries = "GBR", subsectors = "electricity-generation",
                 year = 2022, limit = 5, page_size = 3)
  expect_lte(length(unique(a$asset_id)), 5)
  expect_true(all(c("asset_id", "owner_name", "emissions") %in% names(a)))
  expect_true(all(a$country == "GBR"))
})

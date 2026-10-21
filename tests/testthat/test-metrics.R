toy <- function() {
  data.frame(
    id = c("A", "B", "C"),
    sector = c("Utilities", "Utilities", "Information Technology"),
    value = c(50, 30, 20),
    scope1 = c(1000, 200, 10),
    scope2 = c(100, 50, 40),
    scope3 = c(NA, 500, 300),
    revenue = c(10, 5, 20),
    evic = c(200, 100, 400),
    market_cap = c(150, 80, 350),
    ebitda = c(4, 2, 6)
  )
}

test_that("WACI matches a hand calculation", {
  p <- climate_portfolio(toy())
  # intensities: 1100/10 = 110, 250/5 = 50, 50/20 = 2.5
  expect_equal(as.numeric(waci(p)), 0.5 * 110 + 0.3 * 50 + 0.2 * 2.5)
  expect_equal(attr(waci(p), "coverage"), 1)
  expect_equal(as.numeric(waci(p, scopes = 1)), 0.5 * 100 + 0.3 * 40 + 0.2 * 0.5)
  # Scope 3 is missing for A, so A drops out and weights are rescaled
  w3 <- waci(p, scopes = 1:3)
  expect_equal(attr(w3, "coverage"), 0.5)
  expect_equal(as.numeric(w3), (0.3 * 750 / 5 + 0.2 * 350 / 20) / 0.5)
  expect_output(print(w3), "coverage 50%")
})

test_that("financed emissions and footprint follow PCAF attribution", {
  p <- climate_portfolio(toy())
  fe <- 50 / 200 * 1100 + 30 / 100 * 250 + 20 / 400 * 50
  expect_equal(as.numeric(financed_emissions(p)), fe)
  expect_equal(as.numeric(carbon_footprint(p)), fe / 100)
  m <- climate_metrics(p)
  expect_equal(m$metric, c("waci", "financed_emissions", "carbon_footprint"))
  expect_equal(m$value[2], fe)
  expect_equal(m$scopes[1], "Scope 1+2")
})

test_that("financed emissions need holding values", {
  p <- climate_portfolio(data.frame(id = 1:2, weight = c(.5, .5), scope1 = 1:2,
                                    evic = c(10, 10)))
  expect_error(financed_emissions(p), "portfolio_value")
  expect_error(waci(p, scopes = 4), "scopes")
})

test_that("contributions add up to WACI", {
  p <- climate_portfolio(toy())
  h <- waci_contribution(p)
  expect_equal(sum(h$contribution), as.numeric(waci(p)))
  expect_equal(h$id[1], "A")
  s <- waci_contribution(p, by = "sector")
  expect_equal(sum(s$share), 1)
  expect_equal(s$weight[s$sector == "Utilities"], 0.8)
})

test_that("attribution effects sum to the WACI gap", {
  p <- climate_portfolio(example_portfolio)
  b <- climate_portfolio(example_benchmark)
  a <- waci_attribution(p, b)
  expect_s3_class(a, "waci_attribution")
  gap <- as.numeric(waci(p)) - as.numeric(waci(b))
  expect_equal(sum(a$total), gap)
  expect_equal(sum(a$weight_portfolio), 1)
  expect_output(print(a), "allocation")

  # A sector held only by the portfolio is pure allocation
  p2 <- climate_portfolio(toy())
  b2 <- climate_portfolio(toy()[1:2, ])
  a2 <- waci_attribution(p2, b2)
  it <- a2[a2$sector == "Information Technology", ]
  expect_equal(it$selection, 0)
  expect_equal(sum(a2$total), as.numeric(waci(p2)) - as.numeric(waci(b2)))
})

test_that("benchmark comparison handles different portfolio sizes", {
  p <- climate_portfolio(example_portfolio)
  same <- compare_benchmark(p, climate_portfolio(example_benchmark, portfolio_value = 500))
  expect_false(is.na(same$benchmark[same$metric == "financed_emissions"]))
  diff <- compare_benchmark(p, climate_portfolio(example_benchmark, portfolio_value = 900))
  expect_true(is.na(diff$benchmark[diff$metric == "financed_emissions"]))
  expect_equal(diff$benchmark[diff$metric == "carbon_footprint"],
               same$benchmark[same$metric == "carbon_footprint"])
})

test_that("example data cover the advertised gaps", {
  cov <- data_coverage(climate_portfolio(example_portfolio))
  expect_lt(cov$coverage[cov$item == "scope1"], 1)
  expect_equal(nrow(example_benchmark), 60)
})

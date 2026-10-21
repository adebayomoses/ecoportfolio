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

test_that("climate_portfolio computes weights and standard columns", {
  p <- climate_portfolio(toy())
  expect_s3_class(p, "climate_portfolio")
  expect_equal(p$weight, c(0.5, 0.3, 0.2))
  expect_true(all(std_cols %in% names(p)))
  expect_true(all(is.na(p$name)))
  expect_output(print(p), "3 holdings")
})

test_that("columns can be renamed and weights derive values", {
  d <- data.frame(t = c("X", "Y"), w = c(0.25, 0.75), s1 = c(10, 20))
  p <- climate_portfolio(d, columns = c(id = "t", weight = "w", scope1 = "s1"),
                         portfolio_value = 200)
  expect_equal(p$id, c("X", "Y"))
  expect_equal(p$value, c(50, 150))
  expect_error(climate_portfolio(d, columns = c(id = "nope")), "not found")
  expect_error(climate_portfolio(d, columns = c(foo = "t")), "names are among")
})

test_that("bad holdings are rejected and odd weights are rescaled", {
  expect_error(climate_portfolio(list()), "data frame")
  expect_error(climate_portfolio(data.frame(x = 1)), "id")
  expect_error(climate_portfolio(data.frame(id = c(1, 1), weight = c(.5, .5))), "unique")
  expect_error(climate_portfolio(data.frame(id = 1)), "weight")
  expect_error(climate_portfolio(data.frame(id = 1, weight = 1, scope1 = -1)), "negative")
  expect_error(climate_portfolio(data.frame(id = 1, weight = 1, scope1 = "a")), "numeric")
  expect_warning(p <- climate_portfolio(data.frame(id = 1:2, weight = c(2, 2))), "rescaling")
  expect_equal(p$weight, c(0.5, 0.5))
})

test_that("data coverage is weight-based", {
  cov <- data_coverage(climate_portfolio(toy()))
  expect_equal(cov$coverage[cov$item == "scope1"], 1)
  expect_equal(cov$coverage[cov$item == "scope3"], 0.5)
  expect_equal(cov$holdings[cov$item == "scope3"], 2L)
})

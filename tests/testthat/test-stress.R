test_that("stress test matches a hand calculation", {
  p <- climate_portfolio(data.frame(
    id = c("A", "B"), value = c(60, 40), scope1 = c(1e6, 1e5),
    market_cap = c(1000, 500), ebitda = c(200, 50)
  ))
  sc <- data.frame(scenario = "s", year = 2030, carbon_price = 100)
  st <- carbon_stress_test(p, sc, multiple = 10)
  h <- st$holdings
  expect_equal(h$carbon_cost, c(100, 10))          # USD m
  expect_equal(h$earnings_at_risk, c(0.5, 0.2))
  expect_equal(h$value_shock, c(-1, -0.2))         # A capped at -100%
  expect_equal(st$portfolio$value_loss, 0.6 * -1 + 0.4 * -0.2)
  expect_equal(st$portfolio$loss_amount, (0.6 * -1 + 0.4 * -0.2) * 100)
  expect_equal(st$portfolio$coverage, 1)

  st2 <- carbon_stress_test(p, sc, pass_through = 0.5, abatement = c(0.5, 0))
  expect_equal(st2$holdings$carbon_cost, c(25, 5))
})

test_that("default scenarios and year filters work", {
  p <- climate_portfolio(example_portfolio)
  st <- carbon_stress_test(p)
  expect_s3_class(st, "climate_stress")
  expect_setequal(unique(st$portfolio$scenario), c("orderly", "disorderly", "hot_house"))
  expect_equal(nrow(st$holdings), nrow(p) * nrow(carbon_price_paths))
  expect_true(all(st$portfolio$value_loss <= 0))
  st30 <- carbon_stress_test(p, years = 2030)
  expect_equal(unique(st30$portfolio$year), 2030)
  ord <- st30$portfolio[order(st30$portfolio$carbon_price), ]
  expect_true(all(diff(ord$value_loss) <= 0))      # higher price, bigger loss
  expect_lt(st$portfolio$coverage[1], 1)            # two holdings lack emissions
  expect_output(print(st), "stress test")
})

test_that("stress test arguments are validated", {
  p <- climate_portfolio(example_portfolio)
  expect_error(carbon_stress_test(p, data.frame(x = 1)), "scenario")
  expect_error(carbon_stress_test(p, years = 1999), "No scenario rows")
  expect_error(carbon_stress_test(p, pass_through = 2), "between 0 and 1")
  expect_error(carbon_stress_test(p, abatement = c(0.1, 0.2)), "one per holding")
  expect_error(carbon_stress_test(p, multiple = 0), "multiple")
})

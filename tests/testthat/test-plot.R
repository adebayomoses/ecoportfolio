test_that("plots build without error", {
  p <- climate_portfolio(example_portfolio)
  b <- climate_portfolio(example_benchmark)
  plots <- list(
    plot_contribution(p),
    plot_contribution(p, by = "sector", top = 4),
    plot_sector_intensity(p, b),
    plot_attribution(waci_attribution(p, b)),
    plot_stress(carbon_stress_test(p))
  )
  for (g in plots) {
    expect_s3_class(g, "ggplot")
    expect_no_error(ggplot2::ggplot_build(g))
  }
  expect_error(plot_stress(list()), "carbon_stress_test")
  expect_error(plot_attribution(list()), "waci_attribution")
})

test_that("scenario colours stay fixed per scenario", {
  a <- scenario_colours(c("orderly", "hot_house"))
  b <- scenario_colours(c("hot_house", "orderly", "disorderly"))
  expect_equal(a[["hot_house"]], b[["hot_house"]])
  expect_equal(length(unique(scenario_colours(c("orderly", "x", "y")))), 3)
})

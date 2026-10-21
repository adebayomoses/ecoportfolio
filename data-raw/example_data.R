# Builds the fictional example datasets shipped with ecoPortfolio.
# All companies and numbers are synthetic. Run from the package root:
#   Rscript data-raw/example_data.R

set.seed(2026)

# Rough sector profiles: scope 1+2 intensity (tCO2e per USD m revenue),
# share of scope 1 in scope 1+2, scope 3 multiple, EBITDA margin, EV/revenue.
profiles <- data.frame(
  sector = c("Utilities", "Energy", "Materials", "Industrials", "Consumer Staples",
             "Information Technology", "Financials", "Health Care", "Real Estate"),
  intensity = c(1800, 650, 900, 120, 55, 15, 4, 18, 40),
  s1_share = c(0.95, 0.85, 0.80, 0.55, 0.50, 0.25, 0.20, 0.40, 0.30),
  s3_mult = c(1.5, 8, 3, 6, 9, 5, 30, 6, 3),
  margin = c(0.35, 0.25, 0.20, 0.15, 0.17, 0.30, 0.40, 0.25, 0.50),
  ev_rev = c(3.0, 1.2, 1.5, 2.0, 2.2, 6.0, 3.5, 4.0, 9.0),
  stringsAsFactors = FALSE
)

make_universe <- function(n_per_sector) {
  rows <- lapply(seq_len(nrow(profiles)), function(i) {
    p <- profiles[i, ]
    n <- n_per_sector[i]
    revenue <- round(exp(rnorm(n, log(8000), 0.8)))
    intensity <- p$intensity * exp(rnorm(n, 0, 0.5))
    s12 <- revenue * intensity
    evic <- round(revenue * p$ev_rev * exp(rnorm(n, 0, 0.3)))
    data.frame(
      sector = p$sector,
      revenue = revenue,
      scope1 = round(s12 * p$s1_share),
      scope2 = round(s12 * (1 - p$s1_share)),
      scope3 = round(s12 * p$s3_mult * exp(rnorm(n, 0, 0.4))),
      ebitda = round(revenue * p$margin * exp(rnorm(n, 0, 0.2))),
      evic = evic,
      market_cap = round(evic * runif(n, 0.6, 0.9)),
      stringsAsFactors = FALSE
    )
  })
  u <- do.call(rbind, rows)
  u$id <- sprintf("FIC%03d", seq_len(nrow(u)))
  u$name <- paste("Fictional", u$sector, "Co", ave(seq_len(nrow(u)), u$sector,
                                                    FUN = function(x) LETTERS[seq_along(x)]))
  u$country <- sample(c("USA", "GBR", "DEU", "FRA", "JPN", "CAN", "AUS"), nrow(u),
                      replace = TRUE, prob = c(0.45, 0.1, 0.1, 0.1, 0.1, 0.08, 0.07))
  u[, c("id", "name", "sector", "country", "revenue", "evic", "market_cap", "ebitda",
        "scope1", "scope2", "scope3")]
}

universe <- make_universe(c(6, 6, 6, 8, 6, 8, 8, 6, 6))

# Benchmark: cap-weighted universe.
example_benchmark <- universe
example_benchmark$weight <- example_benchmark$market_cap / sum(example_benchmark$market_cap)

# Portfolio: 20 holdings, tilted away from Utilities and Energy, worth USD 500m.
pick <- c(sample(which(universe$sector == "Utilities"), 1),
          sample(which(universe$sector == "Energy"), 1),
          sample(which(universe$sector == "Materials"), 2),
          sample(which(universe$sector == "Industrials"), 3),
          sample(which(universe$sector == "Consumer Staples"), 2),
          sample(which(universe$sector == "Information Technology"), 4),
          sample(which(universe$sector == "Financials"), 3),
          sample(which(universe$sector == "Health Care"), 2),
          sample(which(universe$sector == "Real Estate"), 2))
example_portfolio <- universe[pick, ]
w <- runif(length(pick), 0.5, 1.5)
example_portfolio$value <- round(500 * w / sum(w), 2)
# Two holdings without reported emissions, to illustrate data coverage.
gap <- which(example_portfolio$sector %in% c("Industrials", "Health Care"))[c(1, 4)]
example_portfolio[gap, c("scope1", "scope2", "scope3")] <- NA
rownames(example_portfolio) <- NULL
rownames(example_benchmark) <- NULL

# Illustrative carbon price paths (USD per tCO2e). These are NOT NGFS data:
# they only mimic the shape of the three NGFS scenario families.
carbon_price_paths <- data.frame(
  scenario = rep(c("orderly", "disorderly", "hot_house"), each = 6),
  year = rep(c(2025, 2030, 2035, 2040, 2045, 2050), 3),
  carbon_price = c(40, 130, 220, 320, 430, 550,
                   10, 20, 180, 320, 450, 600,
                   10, 15, 20, 25, 30, 35),
  stringsAsFactors = FALSE
)

save(example_portfolio, file = "data/example_portfolio.rda", compress = "xz")
save(example_benchmark, file = "data/example_benchmark.rda", compress = "xz")
save(carbon_price_paths, file = "data/carbon_price_paths.rda", compress = "xz")

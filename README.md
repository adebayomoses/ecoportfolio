# ecoPortfolio

Climate risk metrics for equity portfolios in R.

`ecoPortfolio` provides:

- **Disclosure metrics** recommended by TCFD and ISSB: weighted average carbon
  intensity (`waci()`), PCAF-style financed emissions (`financed_emissions()`)
  and carbon footprint (`carbon_footprint()`), always reported with data
  coverage.
- **Attribution.** Find the holdings and sectors driving exposure
  (`waci_contribution()`), and split the gap to a benchmark into sector
  allocation and stock selection (`waci_attribution()`).
- **Carbon price stress tests.** Earnings at risk and valuation shocks under
  scenario price paths (`carbon_stress_test()`).
- **Open data.** Asset-level emissions and owners from the Climate TRACE API
  (`ct_assets()`, `owner_emissions()`), matched to holdings by normalised name
  (`match_holdings()`).
- **Charts.** TCFD/ISSB-aligned disclosure plots built with ggplot2
  (`plot_contribution()`, `plot_sector_intensity()`, `plot_attribution()`,
  `plot_stress()`).

## Installation

```r
# install.packages("remotes")
remotes::install_github("adebayomoses/ecoPortfolio")
```

## Example

```r
library(ecoPortfolio)

p <- climate_portfolio(example_portfolio)    # fictional data
b <- climate_portfolio(example_benchmark, portfolio_value = 500)

climate_metrics(p)
compare_benchmark(p, b)
plot_attribution(waci_attribution(p, b))

st <- carbon_stress_test(p, pass_through = 0.2)
plot_stress(st)
```

See `vignette("ecoPortfolio")` for the full workflow.

## Caveats

- The example portfolio, benchmark and `carbon_price_paths` are **synthetic
  and illustrative**. For real stress tests, use official NGFS scenario paths
  from <https://data.ece.iiasa.ac.at/ngfs/>.
- The stress test is a transparent first-order model (cost × multiple, capped
  at market value). It is meant for teaching and screening, not as a forecast.
- Climate TRACE emissions are independent physical estimates for individual
  assets. Owners are often subsidiaries, and ownership shares are not
  published (equal shares are assumed). Check matches before relying on them.
- The plots support disclosure aligned with TCFD and ISSB recommendations.
  They do not by themselves make a report compliant with any regulation.

## Data credits

Climate TRACE data are available under CC BY 4.0: <https://climatetrace.org>.

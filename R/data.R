#' Example portfolio and benchmark (fictional)
#'
#' A 20-holding equity portfolio worth 500 (currency millions) and a
#' 60-company cap-weighted benchmark. **All companies and numbers are
#' synthetic**, generated to have realistic orders of magnitude for each
#' sector. Two portfolio holdings have no emissions data, to illustrate
#' coverage reporting. The script is in `data-raw/example_data.R`.
#'
#' @format Data frames with columns `id`, `name`, `sector`, `country`,
#'   `revenue`, `evic`, `market_cap`, `ebitda` (currency millions), `scope1`,
#'   `scope2`, `scope3` (tCO2e), and `value` (portfolio, currency millions)
#'   or `weight` (benchmark).
#' @examples
#' head(example_portfolio)
#' climate_portfolio(example_benchmark)
#' @name example-data
NULL

#' @rdname example-data
"example_portfolio"

#' @rdname example-data
"example_benchmark"

#' Illustrative carbon price paths
#'
#' Carbon prices (currency per tCO2e) for three stylised scenario families,
#' shaped after the Network for Greening the Financial System (NGFS)
#' categories: `orderly` (early, steadily rising prices), `disorderly`
#' (prices stay low, then jump after 2030) and `hot_house` (little new
#' policy).
#'
#' **These numbers are illustrative and are not NGFS data.** For real
#' analysis, download the official scenario paths from the NGFS Scenario
#' Explorer (<https://data.ece.iiasa.ac.at/ngfs/>) and pass them to
#' [carbon_stress_test()] in the same three-column format.
#'
#' @format A data frame with columns `scenario`, `year` and `carbon_price`.
#' @examples
#' carbon_price_paths
"carbon_price_paths"

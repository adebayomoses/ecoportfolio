#' Carbon price stress test
#'
#' Translates carbon price paths into costs, earnings at risk and valuation
#' shocks for each holding, and aggregates them to the portfolio.
#'
#' For holding *i*, scenario *s* and year *t*:
#'
#' * carbon cost = emissions x (1 - abatement) x carbon price x
#'   (1 - pass_through), converted to currency millions;
#' * earnings at risk = carbon cost / EBITDA;
#' * value shock = -min(1, carbon cost x `multiple` / market cap), which
#'   treats the cost as a permanent loss of earnings capitalised at a
#'   valuation multiple.
#'
#' The portfolio loss is the weighted average value shock over holdings with
#' the data needed, with weights rescaled to those holdings; its `coverage`
#' is reported alongside.
#'
#' This is a deliberately simple, transparent first-order model for teaching
#' and screening. It ignores second-round effects such as demand changes,
#' competitors' pass-through and the cost of capital.
#'
#' @inheritParams carbon-metrics
#' @param scenarios A data frame of carbon price paths with columns
#'   `scenario`, `year` and `carbon_price` (currency per tCO2e). `NULL` uses
#'   the illustrative [carbon_price_paths].
#' @param years Optional years to keep.
#' @param scopes Emission scopes that bear the carbon price. Scope 1 by
#'   default; add 2 to model indirect costs through electricity prices.
#' @param pass_through Share of the cost passed on to customers, between 0
#'   and 1. A single number or one per holding.
#' @param abatement Share of emissions cut by the stress year, between 0 and
#'   1. A single number or one per holding.
#' @param multiple Valuation multiple applied to the annual carbon cost.
#' @return An object of class `climate_stress`: a list with
#'   * `portfolio`: one row per scenario and year with `carbon_price`,
#'     `value_loss` (a share; negative is a loss), `loss_amount` (currency
#'     millions, if holding values are known), `earnings_at_risk` (weighted
#'     average) and `coverage`;
#'   * `holdings`: one row per holding, scenario and year;
#'   * `settings`: the assumptions used.
#' @seealso [plot_stress()]
#' @examples
#' p <- climate_portfolio(example_portfolio)
#' st <- carbon_stress_test(p)
#' st
#' carbon_stress_test(p, years = 2030, pass_through = 0.3)$portfolio
#' @export
carbon_stress_test <- function(portfolio, scenarios = NULL, years = NULL,
                               scopes = 1, pass_through = 0, abatement = 0,
                               multiple = 10) {
  p <- check_portfolio(portfolio)
  scopes <- check_scopes(scopes)
  if (is.null(scenarios)) scenarios <- ecoPortfolio::carbon_price_paths
  sc <- check_scenarios(scenarios, years)
  n <- nrow(p)
  pass_through <- check_share(pass_through, n, "pass_through")
  abatement <- check_share(abatement, n, "abatement")
  if (!is.numeric(multiple) || length(multiple) != 1L || multiple <= 0) {
    stop("`multiple` must be a single positive number.", call. = FALSE)
  }

  e <- scope_emissions(p, scopes)
  rows <- lapply(seq_len(nrow(sc)), function(k) {
    cost <- e * (1 - abatement) * sc$carbon_price[k] * (1 - pass_through) / 1e6
    data.frame(
      id = p$id,
      name = p$name,
      sector = p$sector,
      weight = p$weight,
      scenario = sc$scenario[k],
      year = sc$year[k],
      carbon_price = sc$carbon_price[k],
      carbon_cost = cost,
      earnings_at_risk = ifelse(!is.na(p$ebitda) & p$ebitda > 0, cost / p$ebitda, NA_real_),
      value_shock = ifelse(!is.na(p$market_cap) & p$market_cap > 0,
                           -pmin(1, cost * multiple / p$market_cap), NA_real_),
      stringsAsFactors = FALSE
    )
  })
  h <- do.call(rbind, rows)

  total_value <- if (all(is.na(p$value))) NA_real_ else sum(p$value, na.rm = TRUE)
  key <- paste(h$scenario, h$year, sep = "\r")
  port <- lapply(split(h, factor(key, unique(key))), function(d) {
    ok <- !is.na(d$value_shock)
    ok_e <- !is.na(d$earnings_at_risk)
    loss <- if (any(ok)) sum(d$weight[ok] * d$value_shock[ok]) / sum(d$weight[ok]) else NA_real_
    data.frame(
      scenario = d$scenario[1],
      year = d$year[1],
      carbon_price = d$carbon_price[1],
      value_loss = loss,
      loss_amount = loss * total_value,
      earnings_at_risk = if (any(ok_e)) {
        sum(d$weight[ok_e] * d$earnings_at_risk[ok_e]) / sum(d$weight[ok_e])
      } else NA_real_,
      coverage = sum(d$weight[ok]),
      stringsAsFactors = FALSE
    )
  })
  port <- do.call(rbind, port)
  rownames(port) <- NULL
  rownames(h) <- NULL

  structure(
    list(portfolio = port, holdings = h,
         settings = list(scopes = scopes, multiple = multiple,
                         pass_through = unique(pass_through),
                         abatement = unique(abatement))),
    class = "climate_stress"
  )
}

#' @export
print.climate_stress <- function(x, ...) {
  s <- x$settings
  cat("Carbon price stress test (", scope_label(s$scopes), ", multiple ", s$multiple,
      ")\n", sep = "")
  d <- x$portfolio
  d$value_loss <- sprintf("%.2f%%", 100 * d$value_loss)
  d$earnings_at_risk <- sprintf("%.2f%%", 100 * d$earnings_at_risk)
  d$coverage <- sprintf("%.0f%%", 100 * d$coverage)
  d$loss_amount <- round(d$loss_amount, 2)
  print(d, row.names = FALSE)
  invisible(x)
}

check_scenarios <- function(sc, years) {
  if (!is.data.frame(sc) || !all(c("scenario", "year", "carbon_price") %in% names(sc))) {
    stop("`scenarios` must be a data frame with columns `scenario`, `year` and ",
         "`carbon_price`.", call. = FALSE)
  }
  if (!is.numeric(sc$carbon_price) || any(sc$carbon_price < 0, na.rm = TRUE) ||
      anyNA(sc$carbon_price)) {
    stop("`carbon_price` must be non-negative numbers without missing values.",
         call. = FALSE)
  }
  if (!is.null(years)) sc <- sc[sc$year %in% years, , drop = FALSE]
  if (!nrow(sc)) stop("No scenario rows left for the chosen `years`.", call. = FALSE)
  sc <- sc[order(sc$scenario, sc$year), c("scenario", "year", "carbon_price")]
  sc$scenario <- as.character(sc$scenario)
  rownames(sc) <- NULL
  sc
}

check_share <- function(x, n, arg) {
  if (!is.numeric(x) || !length(x) %in% c(1L, n) || anyNA(x) || any(x < 0 | x > 1)) {
    stop("`", arg, "` must be between 0 and 1, as a single number or one per holding.",
         call. = FALSE)
  }
  rep_len(x, n)
}

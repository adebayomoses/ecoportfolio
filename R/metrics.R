#' Carbon metrics for a portfolio
#'
#' * `waci()` is the weighted average carbon intensity: the sum over holdings
#'   of portfolio weight times company emissions per unit of revenue
#'   (tCO2e per currency million of revenue). It measures exposure to
#'   carbon-intensive companies and needs only weights, emissions and
#'   revenue.
#' * `financed_emissions()` attributes each company's emissions to the
#'   portfolio in proportion to the share of the company it owns, measured by
#'   enterprise value including cash: `value / evic * emissions` (tCO2e), as
#'   in the PCAF standard for listed equity.
#' * `carbon_footprint()` is financed emissions per currency million invested
#'   (tCO2e per million), which lets portfolios of different sizes be
#'   compared.
#' * `climate_metrics()` returns all three as a table.
#'
#' Holdings without the required data are left out and the remaining weights
#' are rescaled; the share of portfolio weight actually used is returned as
#' the `coverage` attribute (or column) and should be reported with the
#' metric.
#'
#' @param portfolio A [climate_portfolio()] (or a data frame of holdings in
#'   the standard columns).
#' @param scopes Emission scopes to include: any of 1, 2 and 3.
#' @return `waci()`, `financed_emissions()` and `carbon_footprint()` return a
#'   number of class `climate_metric` with attributes `unit`, `scopes` and
#'   `coverage`. `climate_metrics()` returns a data frame with columns
#'   `metric`, `scopes`, `value`, `unit` and `coverage`.
#' @references Task Force on Climate-related Financial Disclosures (2021).
#'   *Implementing the Recommendations of the TCFD*, annex section on asset
#'   owners and managers.
#'
#'   Partnership for Carbon Accounting Financials (2022). *The Global GHG
#'   Accounting and Reporting Standard Part A: Financed Emissions*, second
#'   edition.
#' @examples
#' p <- climate_portfolio(example_portfolio)
#' waci(p)
#' waci(p, scopes = 1:3)
#' financed_emissions(p)
#' carbon_footprint(p)
#' climate_metrics(p)
#' @name carbon-metrics
NULL

#' @rdname carbon-metrics
#' @export
waci <- function(portfolio, scopes = 1:2) {
  p <- check_portfolio(portfolio)
  scopes <- check_scopes(scopes)
  e <- scope_emissions(p, scopes)
  ok <- !is.na(e) & !is.na(p$revenue) & p$revenue > 0
  cov <- sum(p$weight[ok])
  val <- if (cov > 0) sum(p$weight[ok] * e[ok] / p$revenue[ok]) / cov else NA_real_
  new_metric(val, "tCO2e / m revenue", scopes, cov, "WACI")
}

#' @rdname carbon-metrics
#' @export
financed_emissions <- function(portfolio, scopes = 1:2) {
  p <- check_portfolio(portfolio)
  scopes <- check_scopes(scopes)
  ok <- financed_ok(p, scopes)
  e <- scope_emissions(p, scopes)
  val <- if (any(ok)) sum(p$value[ok] / p$evic[ok] * e[ok]) else NA_real_
  new_metric(val, "tCO2e", scopes, sum(p$weight[ok]), "Financed emissions")
}

#' @rdname carbon-metrics
#' @export
carbon_footprint <- function(portfolio, scopes = 1:2) {
  p <- check_portfolio(portfolio)
  scopes <- check_scopes(scopes)
  ok <- financed_ok(p, scopes)
  e <- scope_emissions(p, scopes)
  val <- if (any(ok)) sum(p$value[ok] / p$evic[ok] * e[ok]) / sum(p$value[ok]) else NA_real_
  new_metric(val, "tCO2e / m invested", scopes, sum(p$weight[ok]), "Carbon footprint")
}

#' @rdname carbon-metrics
#' @export
climate_metrics <- function(portfolio, scopes = 1:2) {
  p <- check_portfolio(portfolio)
  ms <- list(waci(p, scopes), financed_emissions(p, scopes), carbon_footprint(p, scopes))
  data.frame(
    metric = c("waci", "financed_emissions", "carbon_footprint"),
    scopes = scope_label(check_scopes(scopes)),
    value = vapply(ms, as.numeric, numeric(1)),
    unit = vapply(ms, attr, character(1), "unit"),
    coverage = vapply(ms, attr, numeric(1), "coverage"),
    stringsAsFactors = FALSE
  )
}

#' Contributions to a portfolio's WACI
#'
#' Splits the weighted average carbon intensity into the part contributed by
#' each holding or sector. A few holdings usually account for most of a
#' portfolio's carbon exposure, which this makes visible.
#'
#' @inheritParams carbon-metrics
#' @param by `"holding"` or `"sector"`.
#' @return A data frame sorted by contribution, with `weight` (rescaled to
#'   covered holdings), `intensity` (tCO2e per m revenue; the sector WACI for
#'   `by = "sector"`), `contribution` to WACI and `share` of WACI.
#' @examples
#' p <- climate_portfolio(example_portfolio)
#' waci_contribution(p, by = "sector")
#' head(waci_contribution(p))
#' @export
waci_contribution <- function(portfolio, scopes = 1:2, by = c("holding", "sector")) {
  p <- check_portfolio(portfolio)
  scopes <- check_scopes(scopes)
  by <- match.arg(by)
  d <- intensity_table(p, scopes)
  if (!nrow(d)) stop("No holdings have both emissions and revenue.", call. = FALSE)
  d$contribution <- d$weight * d$intensity
  if (by == "sector") {
    d$sector[is.na(d$sector)] <- "Unknown"
    w <- tapply(d$weight, d$sector, sum)
    cc <- tapply(d$contribution, d$sector, sum)
    d <- data.frame(sector = names(w), weight = as.numeric(w),
                    intensity = as.numeric(cc / w), contribution = as.numeric(cc),
                    stringsAsFactors = FALSE)
  } else {
    d <- d[, c("id", "name", "sector", "weight", "intensity", "contribution")]
  }
  d$share <- d$contribution / sum(d$contribution)
  d <- d[order(-d$contribution), , drop = FALSE]
  rownames(d) <- NULL
  d
}

#' Compare a portfolio with its benchmark
#'
#' @inheritParams carbon-metrics
#' @param benchmark A [climate_portfolio()] for the benchmark.
#' @return A data frame with one row per metric: `portfolio`, `benchmark`
#'   (financed emissions are `NA` for the benchmark unless both have the same
#'   total value, because they scale with portfolio size),
#'   `difference` (`portfolio - benchmark`), `relative` (`portfolio /
#'   benchmark - 1`) and both coverages.
#' @examples
#' p <- climate_portfolio(example_portfolio)
#' b <- climate_portfolio(example_benchmark, portfolio_value = 500)
#' compare_benchmark(p, b)
#' @export
compare_benchmark <- function(portfolio, benchmark, scopes = 1:2) {
  p <- check_portfolio(portfolio)
  b <- check_portfolio(benchmark, "benchmark")
  mp <- climate_metrics(p, scopes)
  mb <- climate_metrics(b, scopes)
  # Financed emissions scale with portfolio size, so comparing them only makes
  # sense when both have the same total value.
  vp <- sum(p$value, na.rm = TRUE)
  vb <- sum(b$value, na.rm = TRUE)
  if (!isTRUE(abs(vp - vb) <= 1e-3 * max(vp, vb))) {
    mb$value[mb$metric == "financed_emissions"] <- NA_real_
  }
  out <- data.frame(
    metric = mp$metric,
    scopes = mp$scopes,
    unit = mp$unit,
    portfolio = mp$value,
    benchmark = mb$value,
    difference = mp$value - mb$value,
    relative = mp$value / mb$value - 1,
    coverage_portfolio = mp$coverage,
    coverage_benchmark = mb$coverage,
    stringsAsFactors = FALSE
  )
  out
}

#' Attribute the WACI gap to sector allocation and stock selection
#'
#' Decomposes the difference between the portfolio's and the benchmark's
#' WACI, in the spirit of Brinson attribution:
#'
#' * **allocation** for sector *s* is `(wp_s - wb_s) * (Ib_s - Ib)`: carbon
#'   saved or added by over- or underweighting sectors that are more intensive
#'   than the benchmark as a whole;
#' * **selection** is `wp_s * (Ip_s - Ib_s)`: carbon saved or added by
#'   choosing cleaner or dirtier companies within the sector.
#'
#' Here `w` are sector weights, `I` sector WACIs and `Ib` the benchmark WACI.
#' The effects sum exactly to `WACI(portfolio) - WACI(benchmark)`. A sector held
#' only by the portfolio is assigned its own intensity as the benchmark
#' intensity, so all of its effect is allocation.
#'
#' @inheritParams compare_benchmark
#' @return A data frame of class `waci_attribution` with, per sector, the
#'   weights, sector WACIs and the `allocation`, `selection` and `total`
#'   effects (tCO2e per m revenue). The portfolio and benchmark WACIs are
#'   stored as attributes.
#' @examples
#' p <- climate_portfolio(example_portfolio)
#' b <- climate_portfolio(example_benchmark)
#' a <- waci_attribution(p, b)
#' a
#' sum(a$total)
#' @export
waci_attribution <- function(portfolio, benchmark, scopes = 1:2) {
  scopes <- check_scopes(scopes)
  dp <- intensity_table(check_portfolio(portfolio), scopes)
  db <- intensity_table(check_portfolio(benchmark, "benchmark"), scopes)
  if (!nrow(dp) || !nrow(db)) {
    stop("Both portfolio and benchmark need holdings with emissions and revenue.",
         call. = FALSE)
  }
  dp$sector[is.na(dp$sector)] <- "Unknown"
  db$sector[is.na(db$sector)] <- "Unknown"
  sectors <- sort(union(dp$sector, db$sector))
  agg <- function(d) {
    w <- tapply(d$weight, factor(d$sector, sectors), sum)
    c <- tapply(d$weight * d$intensity, factor(d$sector, sectors), sum)
    w[is.na(w)] <- 0
    c[is.na(c)] <- 0
    list(w = as.numeric(w), i = ifelse(w > 0, as.numeric(c / w), NA_real_))
  }
  ap <- agg(dp)
  ab <- agg(db)
  waci_p <- sum(dp$weight * dp$intensity)
  waci_b <- sum(db$weight * db$intensity)
  ib <- ifelse(is.na(ab$i), ap$i, ab$i)
  ip <- ifelse(is.na(ap$i), 0, ap$i)
  allocation <- (ap$w - ab$w) * (ib - waci_b)
  selection <- ifelse(ap$w > 0, ap$w * (ip - ib), 0)
  out <- data.frame(
    sector = sectors,
    weight_portfolio = ap$w,
    weight_benchmark = ab$w,
    waci_portfolio = ap$i,
    waci_benchmark = ab$i,
    allocation = allocation,
    selection = selection,
    total = allocation + selection,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$total), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, class = c("waci_attribution", "data.frame"),
            waci_portfolio = waci_p, waci_benchmark = waci_b, scopes = scopes)
}

#' @export
print.waci_attribution <- function(x, ...) {
  cat("WACI attribution (", scope_label(attr(x, "scopes")), ", tCO2e / m revenue)\n",
      sep = "")
  cat(sprintf("  portfolio %.1f - benchmark %.1f = %.1f  (allocation %.1f, selection %.1f)\n\n",
              attr(x, "waci_portfolio"), attr(x, "waci_benchmark"),
              attr(x, "waci_portfolio") - attr(x, "waci_benchmark"),
              sum(x$allocation), sum(x$selection)))
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], round, 3)
  print(df, row.names = FALSE)
  invisible(x)
}

#' @export
print.climate_metric <- function(x, ...) {
  cat(attr(x, "label"), " (", scope_label(attr(x, "scopes")), "): ",
      format(as.numeric(x), big.mark = ",", digits = 6), " ", attr(x, "unit"),
      "  [coverage ", sprintf("%.0f%%", 100 * attr(x, "coverage")), "]\n", sep = "")
  invisible(x)
}

new_metric <- function(value, unit, scopes, coverage, label) {
  structure(value, unit = unit, scopes = scopes, coverage = coverage, label = label,
            class = "climate_metric")
}

financed_ok <- function(p, scopes) {
  e <- scope_emissions(p, scopes)
  if (all(is.na(p$value))) {
    stop("Financed emissions need holding values. Add a `value` column or set ",
         "`portfolio_value` in climate_portfolio().", call. = FALSE)
  }
  !is.na(e) & !is.na(p$value) & !is.na(p$evic) & p$evic > 0
}

# Covered holdings with rescaled weights and carbon intensity.
intensity_table <- function(p, scopes) {
  e <- scope_emissions(p, scopes)
  ok <- !is.na(e) & !is.na(p$revenue) & p$revenue > 0
  data.frame(
    id = p$id[ok],
    name = p$name[ok],
    sector = as.character(p$sector[ok]),
    weight = p$weight[ok] / sum(p$weight[ok]),
    intensity = e[ok] / p$revenue[ok],
    stringsAsFactors = FALSE
  )
}

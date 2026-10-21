#' Create a climate portfolio
#'
#' Validates holdings data and puts it in the standard form used by every
#' metric in the package. Columns can be renamed on the way in with
#' `columns`.
#'
#' @section Standard columns:
#' | Column | Meaning | Unit |
#' |---|---|---|
#' | `id` | Holding identifier (required) | |
#' | `name`, `sector`, `country` | Descriptors (optional) | |
#' | `weight` | Portfolio weight | share, sums to 1 |
#' | `value` | Market value held | currency, millions |
#' | `scope1`, `scope2`, `scope3` | Company emissions | tCO2e per year |
#' | `revenue` | Annual revenue | currency, millions |
#' | `evic` | Enterprise value including cash | currency, millions |
#' | `market_cap` | Equity market capitalisation | currency, millions |
#' | `ebitda` | Annual EBITDA | currency, millions |
#'
#' All monetary columns must use the same currency. Missing emissions or
#' financials are allowed: metrics skip holdings that lack the data they
#' need and report the share of the portfolio they cover.
#'
#' Weights are taken from `weight` if present, otherwise computed from
#' `value`. If only weights are given, supply `portfolio_value` to derive
#' `value`, which financed emissions and carbon footprint need.
#'
#' @param data A data frame of holdings, one row per company.
#' @param columns Optional named character vector mapping standard names to
#'   the column names in `data`, for example `c(scope1 = "S1_tCO2e")`.
#' @param portfolio_value Optional total portfolio value (currency, millions)
#'   used to compute `value` from `weight`.
#' @return A data frame of class `climate_portfolio`.
#' @examples
#' p <- climate_portfolio(example_portfolio)
#' p
#'
#' # Holdings with your own column names
#' my <- data.frame(ticker = c("AAA", "BBB"), w = c(0.6, 0.4),
#'                  s1 = c(1200, 300), s2 = c(400, 100), sales = c(50, 80))
#' climate_portfolio(my, columns = c(id = "ticker", weight = "w", scope1 = "s1",
#'                                   scope2 = "s2", revenue = "sales"))
#' @export
climate_portfolio <- function(data, columns = NULL, portfolio_value = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!is.null(columns)) {
    if (!is.character(columns) || is.null(names(columns)) ||
        any(!names(columns) %in% std_cols)) {
      stop("`columns` must be a named character vector whose names are among: ",
           paste(std_cols, collapse = ", "), ".", call. = FALSE)
    }
    miss <- setdiff(columns, names(data))
    if (length(miss)) {
      stop("Column(s) not found in `data`: ", paste(miss, collapse = ", "), ".",
           call. = FALSE)
    }
    for (nm in names(columns)) data[[nm]] <- data[[columns[[nm]]]]
  }
  if (!"id" %in% names(data)) {
    stop("`data` needs an `id` column (or map one with `columns`).", call. = FALSE)
  }
  if (anyDuplicated(data$id)) {
    stop("Holding ids must be unique.", call. = FALSE)
  }
  for (nm in intersect(num_cols, names(data))) {
    x <- data[[nm]]
    if (!is.numeric(x)) {
      stop("Column `", nm, "` must be numeric.", call. = FALSE)
    }
    if (any(x < 0, na.rm = TRUE)) {
      stop("Column `", nm, "` must not contain negative values.", call. = FALSE)
    }
  }
  has_w <- "weight" %in% names(data)
  has_v <- "value" %in% names(data)
  if (!has_w && !has_v) {
    stop("`data` needs a `weight` or `value` column.", call. = FALSE)
  }
  if (has_w && anyNA(data$weight)) stop("`weight` must not be missing.", call. = FALSE)
  if (!has_w) {
    if (anyNA(data$value)) stop("`value` must not be missing.", call. = FALSE)
    data$weight <- data$value / sum(data$value)
  } else if (abs(sum(data$weight) - 1) > 1e-6) {
    warning("Weights sum to ", format(sum(data$weight), digits = 4),
            "; rescaling them to sum to 1.", call. = FALSE)
    data$weight <- data$weight / sum(data$weight)
  }
  if (!is.null(portfolio_value)) {
    if (!is.numeric(portfolio_value) || length(portfolio_value) != 1L ||
        portfolio_value <= 0) {
      stop("`portfolio_value` must be a single positive number.", call. = FALSE)
    }
    data$value <- data$weight * portfolio_value
  }
  for (nm in setdiff(c("name", "sector", "country"), names(data))) data[[nm]] <- NA_character_
  for (nm in setdiff(num_cols, names(data))) data[[nm]] <- NA_real_
  data <- data[, c(std_cols, setdiff(names(data), std_cols)), drop = FALSE]
  rownames(data) <- NULL
  class(data) <- c("climate_portfolio", "data.frame")
  data
}

#' @export
print.climate_portfolio <- function(x, ...) {
  cat("<climate_portfolio> ", nrow(x), " holdings", sep = "")
  if (!all(is.na(x$value))) {
    cat(", value ", format(sum(x$value, na.rm = TRUE), big.mark = ",", nsmall = 1),
        " m", sep = "")
  }
  cat("\n")
  cov <- data_coverage(x)
  cat("  coverage (by weight): ",
      paste0(cov$item, " ", sprintf("%.0f%%", 100 * cov$coverage), collapse = ", "),
      "\n", sep = "")
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  show <- intersect(c("id", "name", "sector", "weight", "scope1", "scope2", "revenue"),
                    names(df))
  print(utils::head(df[, show, drop = FALSE], 10), row.names = FALSE)
  if (nrow(x) > 10) cat("  ... and ", nrow(x) - 10, " more\n", sep = "")
  invisible(x)
}

#' Data coverage of a portfolio
#'
#' The share of portfolio weight for which each input is available. Coverage
#' should always be disclosed next to carbon metrics, because metrics are
#' calculated on covered holdings only.
#'
#' @param portfolio A [climate_portfolio()].
#' @return A data frame with `item`, `holdings` (number with data) and
#'   `coverage` (share of weight with data).
#' @examples
#' data_coverage(climate_portfolio(example_portfolio))
#' @export
data_coverage <- function(portfolio) {
  p <- check_portfolio(portfolio)
  items <- c("scope1", "scope2", "scope3", "revenue", "evic", "market_cap", "ebitda")
  data.frame(
    item = items,
    holdings = vapply(items, function(i) sum(!is.na(p[[i]])), integer(1)),
    coverage = vapply(items, function(i) sum(p$weight[!is.na(p[[i]])]), numeric(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

std_cols <- c("id", "name", "sector", "country", "weight", "value", "scope1", "scope2",
              "scope3", "revenue", "evic", "market_cap", "ebitda")
num_cols <- c("weight", "value", "scope1", "scope2", "scope3", "revenue", "evic",
              "market_cap", "ebitda")

check_portfolio <- function(p, arg = "portfolio") {
  if (inherits(p, "climate_portfolio")) return(p)
  if (is.data.frame(p)) return(climate_portfolio(p))
  stop("`", arg, "` must be a climate_portfolio or a data frame of holdings.",
       call. = FALSE)
}

check_scopes <- function(scopes) {
  if (!is.numeric(scopes) || !length(scopes) || any(!scopes %in% 1:3) ||
      anyDuplicated(scopes)) {
    stop("`scopes` must be distinct values among 1, 2 and 3.", call. = FALSE)
  }
  sort(scopes)
}

# Sum of the chosen scopes; NA when any of them is missing.
scope_emissions <- function(p, scopes) {
  m <- as.matrix(as.data.frame(unclass(p))[paste0("scope", scopes)])
  rowSums(m)
}

scope_label <- function(scopes) paste0("Scope ", paste(scopes, collapse = "+"))

#' Plot the largest contributors to WACI
#'
#' A horizontal bar chart of each holding's or sector's share of the
#' portfolio's weighted average carbon intensity.
#'
#' @inheritParams waci_contribution
#' @param top Number of bars to show; the rest are folded into "Other".
#' @return A `ggplot` object.
#' @examples
#' p <- climate_portfolio(example_portfolio)
#' plot_contribution(p, by = "sector")
#' plot_contribution(p, top = 8)
#' @export
plot_contribution <- function(portfolio, scopes = 1:2, by = c("holding", "sector"),
                              top = 10) {
  by <- match.arg(by)
  d <- waci_contribution(portfolio, scopes, by)
  d$label <- if (by == "sector") d$sector else ifelse(is.na(d$name), d$id, d$name)
  if (nrow(d) > top) {
    rest <- d[-seq_len(top - 1L), ]
    d <- d[seq_len(top - 1L), ]
    d <- rbind(d[, c("label", "share")],
               data.frame(label = paste0("Other (", nrow(rest), ")"), share = sum(rest$share)))
  }
  d$label <- factor(d$label, rev(d$label))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$share, y = .data$label)) +
    ggplot2::geom_col(fill = eco_pal[["series1"]], width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", 100 * .data$share)),
                       hjust = -0.15, size = 3.2, colour = eco_ink[["secondary"]]) +
    ggplot2::scale_x_continuous(labels = pct_lab, expand = ggplot2::expansion(c(0, 0.12))) +
    ggplot2::labs(
      title = paste0("Share of WACI by ", by),
      subtitle = paste0(scope_label(check_scopes(scopes)), ", WACI ",
                        format(round(as.numeric(waci(portfolio, scopes)), 1), big.mark = ","),
                        " tCO2e / m revenue"),
      x = NULL, y = NULL
    ) +
    theme_eco() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot sector carbon intensity against a benchmark
#'
#' Side-by-side bars of sector WACI for the portfolio and benchmark.
#'
#' @inheritParams compare_benchmark
#' @return A `ggplot` object.
#' @examples
#' plot_sector_intensity(climate_portfolio(example_portfolio),
#'                       climate_portfolio(example_benchmark))
#' @export
plot_sector_intensity <- function(portfolio, benchmark, scopes = 1:2) {
  a <- waci_attribution(portfolio, benchmark, scopes)
  d <- rbind(
    data.frame(sector = a$sector, who = "Portfolio", waci = a$waci_portfolio),
    data.frame(sector = a$sector, who = "Benchmark", waci = a$waci_benchmark)
  )
  ord <- a$sector[order(ifelse(is.na(a$waci_benchmark), a$waci_portfolio, a$waci_benchmark))]
  d$sector <- factor(d$sector, ord)
  d$who <- factor(d$who, c("Portfolio", "Benchmark"))
  d <- d[!is.na(d$waci), ]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$waci, y = .data$sector, fill = .data$who)) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(reverse = TRUE, padding = 0.1,
                                                          preserve = "single"),
                      width = 0.75) +
    ggplot2::scale_fill_manual(values = c(Portfolio = eco_pal[["series1"]],
                                          Benchmark = eco_pal[["series2"]]), name = NULL) +
    ggplot2::scale_x_continuous(labels = num_lab, expand = ggplot2::expansion(c(0, 0.05))) +
    ggplot2::labs(title = "Carbon intensity by sector",
                  subtitle = paste0(scope_label(check_scopes(scopes)),
                                    ", tCO2e per m revenue"),
                  x = NULL, y = NULL) +
    theme_eco() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "top", legend.justification = "left")
}

#' @rdname plot_attribution
#' @export
plot.waci_attribution <- function(x, ...) {
  print(plot_attribution(x))
  invisible(x)
}

#' Plot a WACI attribution
#'
#' Shows, for each sector, how much of the WACI gap to the benchmark comes
#' from sector allocation and from stock selection. Bars to the left reduce
#' the portfolio's carbon intensity relative to the benchmark.
#'
#' @param x A `waci_attribution` from [waci_attribution()].
#' @param ... Unused.
#' @return `plot_attribution()` returns a `ggplot` object.
#' @examples
#' a <- waci_attribution(climate_portfolio(example_portfolio),
#'                       climate_portfolio(example_benchmark))
#' plot_attribution(a)
#' @export
plot_attribution <- function(x) {
  if (!inherits(x, "waci_attribution")) {
    stop("`x` must come from waci_attribution().", call. = FALSE)
  }
  d <- rbind(
    data.frame(sector = x$sector, effect = "Allocation", value = x$allocation),
    data.frame(sector = x$sector, effect = "Selection", value = x$selection)
  )
  d$sector <- factor(d$sector, x$sector[order(-x$total)])
  d$effect <- factor(d$effect, c("Allocation", "Selection"))
  gap <- attr(x, "waci_portfolio") - attr(x, "waci_benchmark")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$value, y = .data$sector, fill = .data$effect)) +
    ggplot2::geom_vline(xintercept = 0, colour = eco_ink[["muted"]]) +
    ggplot2::geom_col(position = ggplot2::position_dodge2(reverse = TRUE, padding = 0.1),
                      width = 0.75) +
    ggplot2::scale_fill_manual(values = c(Allocation = eco_pal[["series1"]],
                                          Selection = eco_pal[["series2"]]), name = NULL) +
    ggplot2::scale_x_continuous(labels = num_lab) +
    ggplot2::labs(title = "Where the WACI gap to the benchmark comes from",
                  subtitle = sprintf("Portfolio minus benchmark: %s tCO2e per m revenue",
                                     format(round(gap, 1), big.mark = ",")),
                  x = "tCO2e per m revenue", y = NULL) +
    theme_eco() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "top", legend.justification = "left")
}

#' Plot a carbon price stress test
#'
#' Lines of the portfolio's valuation loss over time, one per scenario, with
#' direct labels at the end of each line.
#'
#' @param x A `climate_stress` from [carbon_stress_test()].
#' @param ... Unused.
#' @return `plot_stress()` returns a `ggplot` object.
#' @examples
#' st <- carbon_stress_test(climate_portfolio(example_portfolio))
#' plot_stress(st)
#' @export
plot_stress <- function(x) {
  if (!inherits(x, "climate_stress")) {
    stop("`x` must come from carbon_stress_test().", call. = FALSE)
  }
  d <- x$portfolio
  known <- c("orderly", "disorderly", "hot_house")
  sc <- c(intersect(known, d$scenario), setdiff(unique(d$scenario), known))
  d$scenario <- factor(d$scenario, sc)
  cols <- scenario_colours(sc)
  last <- do.call(rbind, lapply(split(d, d$scenario), function(g) g[which.max(g$year), ]))
  last$label_y <- spread_labels(last$value_loss, diff(range(c(0, d$value_loss), na.rm = TRUE)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$year, y = .data$value_loss,
                                  colour = .data$scenario)) +
    ggplot2::geom_hline(yintercept = 0, colour = eco_ink[["muted"]]) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.4, stroke = 0) +
    ggplot2::geom_text(data = last, ggplot2::aes(y = .data$label_y, label = .data$scenario),
                       hjust = -0.15, size = 3.2, colour = eco_ink[["secondary"]]) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    ggplot2::scale_y_continuous(labels = pct_lab) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(c(0.03, 0.18))) +
    ggplot2::labs(
      title = "Portfolio value loss under carbon price scenarios",
      subtitle = sprintf("%s costs, valuation multiple %s, pass-through %s",
                         scope_label(x$settings$scopes), x$settings$multiple,
                         paste(format(x$settings$pass_through), collapse = "/")),
      x = NULL, y = "Change in value"
    ) +
    theme_eco() +
    ggplot2::theme(legend.position = "top", legend.justification = "left")
}

#' @rdname plot_stress
#' @export
plot.climate_stress <- function(x, ...) {
  print(plot_stress(x))
  invisible(x)
}

# ---- Theme and palette ------------------------------------------------------
# Categorical slots follow a palette validated for colour-vision deficiency:
# the first three hues stay distinguishable even when all pairs are compared.

eco_pal <- c(series1 = "#2a78d6", series2 = "#eb6834", series3 = "#1baf7a",
             series4 = "#eda100", series5 = "#e87ba4", series6 = "#008300",
             series7 = "#6250d6", series8 = "#e34948")
eco_ink <- c(primary = "#0b0b0b", secondary = "#52514e", muted = "#8a8984",
             grid = "#e6e5e0")

scenario_colours <- function(sc) {
  known <- c(orderly = eco_pal[["series1"]], disorderly = eco_pal[["series2"]],
             hot_house = eco_pal[["series3"]])
  out <- known[sc]
  other <- is.na(out)
  spare <- setdiff(unname(eco_pal), known)
  if (sum(other) > length(spare)) {
    stop("At most 8 scenarios can be plotted.", call. = FALSE)
  }
  out[other] <- spare[seq_len(sum(other))]
  stats::setNames(unname(out), sc)
}

theme_eco <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = eco_ink[["primary"]]),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = eco_ink[["secondary"]]),
      axis.text = ggplot2::element_text(colour = eco_ink[["secondary"]]),
      axis.title = ggplot2::element_text(colour = eco_ink[["secondary"]]),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = eco_ink[["grid"]], linewidth = 0.3),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title.position = "plot"
    )
}

# Nudge end-of-line labels apart so that none are closer than 6% of the range.
spread_labels <- function(y, range) {
  gap <- 0.06 * if (is.finite(range) && range > 0) range else 1
  o <- order(y)
  z <- y[o]
  for (i in seq_along(z)[-1]) z[i] <- max(z[i], z[i - 1] + gap)
  z <- z - (mean(z) - mean(y[o]))
  out <- y
  out[o] <- z
  out
}

pct_lab <- function(x) paste0(format(round(100 * x, 1), trim = TRUE), "%")
num_lab <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

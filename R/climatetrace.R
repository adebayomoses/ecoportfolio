#' Download asset-level emissions from Climate TRACE
#'
#' Climate TRACE publishes independent, satellite- and model-based estimates
#' of emissions from individual physical assets (power plants, steel mills,
#' oil fields and so on), together with the companies that own them. This
#' function queries its open API and returns one row per asset and owner.
#'
#' Climate TRACE does not publish ownership shares. When an asset has several
#' owners, `share` assumes equal ownership; replace it if you know better.
#' Asset emissions are physical (roughly scope 1) and are a proxy, not a
#' substitute, for company-reported figures.
#'
#' @param countries Optional character vector of ISO 3166-1 alpha-3 country
#'   codes, such as `"USA"`.
#' @param subsectors Optional character vector of Climate TRACE subsectors,
#'   such as `"electricity-generation"`; see [ct_subsectors()].
#' @param sectors Optional character vector of Climate TRACE sectors, such as
#'   `"power"`.
#' @param year Year of the emissions estimates.
#' @param limit Maximum number of assets to return.
#' @param page_size Assets requested per API call.
#' @return A data frame with `asset_id`, `asset_name`, `country`,
#'   `subsector`, `year`, `emissions` (tCO2e, 100-year GWP), `owner_id`,
#'   `owner_name`, `n_owners` and `share`.
#' @seealso [owner_emissions()], [match_holdings()]
#' @source Climate TRACE, <https://climatetrace.org>. Data are released under
#'   the Creative Commons Attribution 4.0 licence; cite Climate TRACE when you
#'   use them.
#' @examples
#' \dontrun{
#' uk_power <- ct_assets(countries = "GBR", subsectors = "electricity-generation",
#'                       year = 2022, limit = 50)
#' owner_emissions(uk_power)
#' }
#' @export
ct_assets <- function(countries = NULL, subsectors = NULL, sectors = NULL,
                      year = 2022, limit = 1000, page_size = 100) {
  if (!is.numeric(year) || length(year) != 1L) {
    stop("`year` must be a single year.", call. = FALSE)
  }
  if (!is.numeric(limit) || length(limit) != 1L || limit < 1) {
    stop("`limit` must be a positive number.", call. = FALSE)
  }
  page_size <- min(page_size, limit)
  query <- list(
    year = year,
    countries = csv_or_null(countries),
    subsectors = csv_or_null(subsectors),
    sectors = csv_or_null(sectors)
  )
  pages <- list()
  offset <- 0
  repeat {
    body <- ct_request("assets", c(query, list(limit = page_size, offset = offset)))
    got <- body$assets
    pages[[length(pages) + 1L]] <- parse_ct_assets(got, year)
    offset <- offset + length(got)
    if (length(got) < page_size || offset >= limit) break
  }
  out <- do.call(rbind, pages)
  ids <- unique(out$asset_id)[seq_len(min(limit, length(unique(out$asset_id))))]
  out <- out[out$asset_id %in% ids, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Climate TRACE sector and subsector names
#'
#' @return A character vector.
#' @examples
#' \dontrun{
#' ct_subsectors()
#' }
#' @export
ct_subsectors <- function() {
  unlist(ct_request("definitions/subsectors", list()))
}

#' @rdname ct_subsectors
#' @export
ct_sectors <- function() {
  unlist(ct_request("definitions/sectors", list()))
}

#' Total asset emissions by owner
#'
#' Sums asset emissions to their owning companies, weighting by `share`.
#'
#' @param assets A data frame from [ct_assets()].
#' @return A data frame with `owner_id`, `owner_name`, `emissions` (tCO2e),
#'   `n_assets` and `subsectors`, sorted by emissions.
#' @examples
#' assets <- data.frame(
#'   asset_id = c(1, 2, 3, 3), asset_name = c("A", "B", "C", "C"),
#'   country = "GBR", subsector = "electricity-generation", year = 2022,
#'   emissions = c(100, 50, 80, 80), owner_id = c("x", "x", "x", "y"),
#'   owner_name = c("X plc", "X plc", "X plc", "Y Ltd"), n_owners = c(1, 1, 2, 2),
#'   share = c(1, 1, 0.5, 0.5)
#' )
#' owner_emissions(assets)
#' @export
owner_emissions <- function(assets) {
  need <- c("asset_id", "emissions", "owner_id", "owner_name", "share", "subsector")
  if (!is.data.frame(assets) || !all(need %in% names(assets))) {
    stop("`assets` must be a data frame from ct_assets().", call. = FALSE)
  }
  a <- assets[!is.na(assets$owner_id), , drop = FALSE]
  if (!nrow(a)) {
    stop("None of the assets has an owner.", call. = FALSE)
  }
  a$attributed <- a$emissions * a$share
  sp <- split(a, a$owner_id)
  out <- do.call(rbind, lapply(sp, function(d) {
    data.frame(
      owner_id = d$owner_id[1],
      owner_name = d$owner_name[1],
      emissions = sum(d$attributed, na.rm = TRUE),
      n_assets = length(unique(d$asset_id)),
      subsectors = paste(sort(unique(d$subsector)), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  out <- out[order(-out$emissions), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Match holdings to emissions data by company name
#'
#' Joins an emissions table (for example from [owner_emissions()]) to
#' holdings by a normalised company name: lower case, punctuation removed and
#' common legal suffixes ("plc", "inc", "ltd", "corp", "sa", "ag" and so on)
#' dropped, see [normalise_company()]. Names that still differ, such as a
#' subsidiary and its listed parent, can be linked with `aliases`.
#'
#' Always check the `matched_name` column: name matching is a convenience,
#' not a substitute for identifiers such as LEIs or ISINs.
#'
#' @param holdings A data frame or [climate_portfolio()].
#' @param emissions A data frame with a name column and an emissions column.
#' @param holding_name,emissions_name Name columns in `holdings` and
#'   `emissions`.
#' @param emissions_col Column of `emissions` to bring across.
#' @param into Column of `holdings` to write the matched emissions into.
#'   Existing values are kept where there is no match.
#' @param aliases Optional data frame with columns `from` (a name in
#'   `emissions`) and `to` (the matching holding name). Emissions of several
#'   rows mapped to one holding are summed.
#' @return `holdings` with `into` filled in and a `matched_name` column
#'   (`NA` when unmatched). The number of matches is reported in a message.
#' @examples
#' holdings <- data.frame(id = 1:3, name = c("Drax Group", "RWE AG", "Tesco PLC"),
#'                        weight = c(0.3, 0.3, 0.4))
#' em <- data.frame(owner_name = c("Drax Group PLC", "RWE Generation UK PLC"),
#'                  emissions = c(3858200, 6671400))
#' match_holdings(holdings, em,
#'                aliases = data.frame(from = "RWE Generation UK PLC", to = "RWE AG"))
#' @export
match_holdings <- function(holdings, emissions, holding_name = "name",
                           emissions_name = "owner_name", emissions_col = "emissions",
                           into = "scope1", aliases = NULL) {
  if (!is.data.frame(holdings) || !holding_name %in% names(holdings)) {
    stop("`holdings` must be a data frame with a `", holding_name, "` column.",
         call. = FALSE)
  }
  if (!is.data.frame(emissions) || !all(c(emissions_name, emissions_col) %in% names(emissions))) {
    stop("`emissions` must be a data frame with `", emissions_name, "` and `",
         emissions_col, "` columns.", call. = FALSE)
  }
  src <- as.character(emissions[[emissions_name]])
  if (!is.null(aliases)) {
    if (!is.data.frame(aliases) || !all(c("from", "to") %in% names(aliases))) {
      stop("`aliases` must be a data frame with columns `from` and `to`.", call. = FALSE)
    }
    hit <- match(normalise_company(src), normalise_company(aliases$from))
    src[!is.na(hit)] <- as.character(aliases$to[hit[!is.na(hit)]])
  }
  key_src <- normalise_company(src)
  vals <- tapply(emissions[[emissions_col]], key_src, sum, na.rm = TRUE)
  orig <- tapply(as.character(emissions[[emissions_name]]), key_src,
                 function(x) paste(unique(x), collapse = "; "))
  key_h <- normalise_company(holdings[[holding_name]])
  m <- match(key_h, names(vals))
  cls <- class(holdings)
  out <- as.data.frame(unclass(holdings), stringsAsFactors = FALSE)
  if (!into %in% names(out)) out[[into]] <- NA_real_
  out[[into]][!is.na(m)] <- as.numeric(vals[m[!is.na(m)]])
  out$matched_name <- as.character(orig[m])
  message("Matched ", sum(!is.na(m)), " of ", nrow(out), " holdings.")
  class(out) <- cls
  out
}

#' Normalise company names for matching
#'
#' @param x A character vector of company names.
#' @return A character vector of normalised keys.
#' @examples
#' normalise_company(c("Drax Group PLC", "drax group", "Siemens AG", "AT&T Inc."))
#' @export
normalise_company <- function(x) {
  missing <- is.na(x)
  x <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT", sub = ""))
  x <- gsub("[.']", "", x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^a-z0-9 ]", " ", x)
  suffix <- c("plc", "inc", "incorporated", "ltd", "limited", "llc", "lp", "corp",
              "corporation", "co", "company", "sa", "ag", "nv", "se", "spa", "ab",
              "asa", "oyj", "bv", "gmbh", "kk", "the")
  words <- strsplit(trimws(x), " +")
  out <- vapply(words, function(w) paste(w[!w %in% suffix], collapse = " "), character(1))
  out[missing] <- NA_character_
  out
}

# ---- Internal ---------------------------------------------------------------

ct_base_url <- "https://api.climatetrace.org/v6"

ct_request <- function(path, query) {
  query <- Filter(Negate(is.null), query)
  req <- httr2::request(ct_base_url)
  req <- httr2::req_url_path_append(req, path)
  req <- httr2::req_url_query(req, !!!query)
  req <- httr2::req_user_agent(req, "ecoPortfolio (https://github.com/adebayomoses/ecoPortfolio)")
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_retry(req, max_tries = 3)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("Climate TRACE request failed: ", conditionMessage(e), call. = FALSE)
    }
  )
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

parse_ct_assets <- function(assets, year) {
  empty <- data.frame(asset_id = character(), asset_name = character(),
                      country = character(), subsector = character(), year = numeric(),
                      emissions = numeric(), owner_id = character(),
                      owner_name = character(), n_owners = integer(), share = numeric(),
                      stringsAsFactors = FALSE)
  if (!length(assets)) return(empty)
  rows <- lapply(assets, function(a) {
    em <- NA_real_
    for (s in a$EmissionsSummary) {
      if (identical(s$Gas, "co2e_100yr") && !is.null(s$EmissionsQuantity)) {
        em <- as.numeric(s$EmissionsQuantity)
      }
    }
    own <- a$Owners
    ids <- vapply(own, function(o) null_to_na(o$CompanyId), character(1))
    nms <- vapply(own, function(o) null_to_na(o$CompanyName), character(1))
    keep <- !duplicated(ids) & !is.na(ids)
    ids <- ids[keep]
    nms <- nms[keep]
    n <- length(ids)
    if (!n) {
      ids <- NA_character_
      nms <- NA_character_
    }
    data.frame(
      asset_id = as.character(a$Id),
      asset_name = null_to_na(a$Name),
      country = null_to_na(a$Country),
      subsector = null_to_na(a$Sector),
      year = year,
      emissions = em,
      owner_id = ids,
      owner_name = nms,
      n_owners = n,
      share = if (n) 1 / n else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

null_to_na <- function(x) if (is.null(x)) NA_character_ else as.character(x)

csv_or_null <- function(x) if (is.null(x)) NULL else paste(x, collapse = ",")

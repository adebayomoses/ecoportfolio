## Submission

This is a new submission.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

The local check also reported messages about tools missing on the local
machine (qpdf, HTML Tidy, an online time check); these are not package issues.

## Test environments

* Local: Windows 10, R 4.5.1 (`R CMD check --as-cran`, vignettes and PDF
  manual built)
* GitHub Actions: Windows (release), macOS (release), Ubuntu (devel, release,
  oldrel-1) [TODO: confirm all passed before submitting]
* win-builder: R-devel [TODO: confirm result from the e-mail before submitting]

## Web services

The optional helpers `ct_assets()`, `ct_sectors()` and `ct_subsectors()`
download data from the open Climate TRACE API (<https://climatetrace.org>),
which needs no API key.

* Their examples use `@examplesIf interactive()`, so they are not run on CRAN.
* Tests never touch the network: API calls are mocked with
  `httr2::local_mocked_responses()`. A live test runs only when the
  environment variable `ECOPORTFOLIO_LIVE_TESTS` is `"true"`.
* When the service is unavailable, functions fail with an informative error
  of class `ecoPortfolio_api_error`.
* Nothing is cached or written to the user's home file space.

## Data

The example datasets (`example_portfolio`, `example_benchmark`,
`carbon_price_paths`) are synthetic and documented as such; the script that
creates them is in `data-raw/` (excluded from the build).

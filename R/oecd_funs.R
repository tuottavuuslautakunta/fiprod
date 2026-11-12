
utils::globalVariables(c(
  "time",
  "values",
  "obsValue"
))

#' Clean and reshape OECD SDMX data
#'
#' This function parses dates, converts character columns to factors,
#' selects and renames specified variables, creates a `values` column
#' from `obsValues` (or `obsValue`), and drops all other columns.
#' It warns if any dropped factor variables have more than one level.
#'
#' @param df A data.frame containing OECD SDMX data.
#' @param vars A named character vector specifying variables to keep and rename,
#'   in the form \code{c(new_name = "old_name")}. The old names must exist in \code{df}.
#' @param freq Data frequency: \code{"A"} for annual data, \code{"Q"} for quarterly data.
#'   Determines the date parsing format for the \code{obsTime} column.
#'
#' @return A tibble with renamed variables from \code{vars} and a \code{values} column.
#' @examples
#' \dontrun{
#' vars <- c(geo = "LOCATION", measure = "MEASURE")
#' cleaned <- oecd_clean_data(smdx_data, vars = vars, freq = "A")
#' }
#' @export
oecd_clean_data <- function(df = NULL, sdmx_obj = NULL, drop_vars = NULL, vars = c(), freq = "A") {


  # Backward compatibility: allow passing an SDMX object via `sdmx_obj`
  if (!is.null(sdmx_obj)) {
    if (!is.null(df)) {
      warning("Both `df` and `sdmx_obj` were supplied; using `sdmx_obj`.")
    }
    df <- as.data.frame(sdmx_obj)
  }

  if (is.null(df)) {
    stop("Provide either `df` or `sdmx_obj`.")
  }

  # Validate 'vars' is named (or empty)
  if (length(vars) > 0) {
    if (is.null(names(vars)) || any(is.na(names(vars))) || any(names(vars) == "")) {
      stop("'vars' must be a named character vector in the form c(new_name = old_name).")
    }
  }

  # Decide date format based on frequency argument
  d_format <- switch(
    freq,
    "A" = "Y",
    "Q" = "y-q",
    stop("Invalid 'freq' argument. Use 'A' for annual or 'Q' for quarterly.")
  )

  # Parse dates, convert character columns to factors and rename values column.
  df2 <- df |>
    dplyr::select(-tidyselect::all_of(drop_vars)) |>
    dplyr::rename(tidyselect::any_of(c(obsTime = "TIME_PERIOD"))) |>
    pxwebtools::parse_dates(date_format = c(obsTime = d_format)) |>
    dplyr::rename(tidyselect::any_of(c(obsValue = "ObsValue"))) |>
    dplyr::mutate(
      values = as.numeric(obsValue),
      .keep = "unused"
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), forcats::as_factor))

  # Validate that old names in vars exist in the data
  old_names <- unname(vars)
  new_names <- names(vars)
  missing_old <- setdiff(old_names, names(df2))
  if (length(missing_old)) {
    stop("The following old names from 'vars' were not found in the data: ",
         paste(missing_old, collapse = ", "))
  }

  # Columns that will be dropped (all except vars' old names and value_col)
  keep_raw <- c("time", old_names, "values")
  drop_cands <- setdiff(names(df2), keep_raw)

  # Check: factor columns to be dropped
  factor_drops <- drop_cands[vapply(drop_cands, function(s) is.factor(df2[[s]]), logical(1))]
  multi_level <- factor_drops[vapply(factor_drops, function(s) {
    nlevels(droplevels(df2[[s]])) > 1
  }, logical(1))]

  if (length(multi_level)) {
    warning(
      "The following factor columns to be dropped have more than 1 level: ",
      paste(multi_level, collapse = ", "),
      ". Consider keeping or handling them before dropping."
    )
  }

  # Keep only selected columns + values, rename, and drop others
  out <- df2 |>
    dplyr::select(dplyr::all_of(keep_raw)) |>
    # Rename according to vars (new = old)
    dplyr::rename(!!!stats::setNames(old_names, new_names)) |>
    # Order: selected vars first, then values
    dplyr::select(time, dplyr::all_of(new_names), values)

  out
}

#' Fetch a dataset from the OECD SDMX API
#'
#' Builds an OECD "Data Explorer" REST data request directly
#' (\code{https://sdmx.oecd.org/public/rest/data/...}) and reads it with
#' \code{rsdmx::readSDMX()}, used in place of the \code{OECD} package
#' (\code{github::expersso/OECD}): that package is not on CRAN and has not
#' kept up with this newer API. \code{rsdmx} is used only as a generic
#' SDMX-ML reader here (\code{file = <url>, isURL = TRUE}), not through its
#' own bundled \code{"OECD"} service provider profile: that profile still
#' points at OECD's older \code{stats.oecd.org} warehouse, which does not
#' know the dataflow ids (such as \code{"DSD_PDB@DF_PDB"}) this package's
#' `data-raw/get_oecd_*.R` scripts already use.
#'
#' Returns a plain data frame, shaped like \code{\link{oecd_clean_data}}
#' expects: one row per observation, a column per SDMX dimension,
#' \code{TIME_PERIOD} and \code{ObsValue}.
#'
#' @param dataset The dataflow identifier, exactly as it already appears in
#'   the `data-raw/get_oecd_*.R` scripts, e.g.
#'   \code{"OECD.SDD.TPS,DSD_PDB@DF_PDB,"} (agency, dataflow id and version,
#'   comma separated; an empty version means "latest"). Used as-is as the
#'   request URL's dataflow path segment.
#' @param filter The dimension filter, built by \code{\link{oecd_make_filter}}
#'   (a dot separated string, with \code{+} joining several codes within one
#'   dimension). Used as-is as the request URL's key path segment.
#' @param start,end First/last period to fetch (a year, for annual data), sent
#'   as the request's \code{startPeriod}/\code{endPeriod}. \code{NULL} (the
#'   default) fetches every period, as the existing `get_oecd_*.R` scripts do.
#' @param base_url The OECD SDMX REST API's base URL. Exposed for testing;
#'   there should be no reason to change it otherwise.
#'
#' @return A data frame of the raw SDMX observations.
#'
#' @seealso \code{\link{oecd_clean_data}} to reshape the result,
#'   \code{\link{oecd_make_filter}} to build \code{filter}.
#'
#' @examples
#' \dontrun{
#' dat <- oecd_get_dataset(
#'   dataset = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
#'   filter  = oecd_make_filter(list(c("FIN", "USA"), "A", "GVAHRS"))
#' )
#' }
#'
#' @export
oecd_get_dataset <- function(dataset, filter, start = NULL, end = NULL,
                             base_url = "https://sdmx.oecd.org/public/rest") {
  url <- .oecd_dataset_url(dataset, filter, start, end, base_url)
  sdmx <- rsdmx::readSDMX(file = url, isURL = TRUE)
  as.data.frame(sdmx)
}

#' The OECD SDMX REST request URL for a dataset and filter
#'
#' Split out of [oecd_get_dataset()] so the URL it builds can be tested
#' without a network call.
#'
#' @inheritParams oecd_get_dataset
#' @return The request URL, a single string.
#' @keywords internal
.oecd_dataset_url <- function(dataset, filter, start = NULL, end = NULL,
                              base_url = "https://sdmx.oecd.org/public/rest") {
  url <- paste0(base_url, "/data/", dataset, "/", filter)

  period <- c(startPeriod = start, endPeriod = end)
  if (length(period)) {
    url <- paste0(url, "?",
                  paste(names(period), period, sep = "=", collapse = "&"))
  }

  url
}

#' Create an OECD Filter String
#'
#' This function takes a list of filtering arguments and constructs a filter string for an OECD API call.
#'
#' @param x A list of strings representing different filter criteria for the OECD API.
#'
#' @return A single string with the filter criteria concatenated using '+' and '.' as separators.
#'
#' @export
#'
#' @examples
#' make_oecd_filter(list("A", "sna_geo", "", "", "sna6a_transact", "", "sna_activity", "", "", "sna_measures", "", ""))
oecd_make_filter <- function(x) {
  x |>
    purrr::map(~paste(.x, collapse = "+")) |>
    paste(collapse = ".")
}

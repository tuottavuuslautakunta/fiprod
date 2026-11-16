
utils::globalVariables(c(
  "time",
  "values",
  "obsValue"
))

#' Parse a year or year-quarter period column into a `time` Date column
#'
#' A small stand-in for \code{pxwebtools::parse_dates()}, covering only the
#' two period formats this package's own code needs: \code{"Y"} (a 4-digit
#' year, e.g. \code{"2021"}) and \code{"y-q"} (year and quarter, e.g.
#' \code{"2021-Q1"}, as OECD's quarterly data uses), each mapped to the first
#' day of that year or quarter. As with \code{parse_dates()}, the parsed
#' column always ends up named \code{time}, replacing whatever column
#' \code{date_format}'s name pointed at.
#'
#' @param df A data frame.
#' @param date_format A single named character vector, \code{c(<column> =
#'   "Y")} or \code{c(<column> = "y-q")}: the column to parse and which
#'   format it is in.
#'
#' @return \code{df}, with \code{date_format}'s column parsed into a
#'   \code{Date} and renamed to \code{time}.
#'
#' @examples
#' parse_period_date(data.frame(year = c("2020", "2021")), c(year = "Y"))
#' parse_period_date(data.frame(obsTime = c("2021-Q1", "2021-Q2")),
#'                   c(obsTime = "y-q"))
#'
#' @export
parse_period_date <- function(df, date_format) {
  col <- names(date_format)
  fmt <- unname(date_format)
  x <- as.character(df[[col]])

  time <- switch(fmt,
    "Y" = as.Date(paste0(x, "-01-01")),
    "y-q" = {
      parts <- regmatches(x, regexec("^(\\d{4})-Q([1-4])$", x))
      if (any(lengths(parts) == 0)) {
        stop("Not all values of '", col, "' match the \"y-q\" format ",
             "(e.g. \"2021-Q1\"): ",
             paste(x[lengths(parts) == 0], collapse = ", "))
      }
      year <- as.integer(vapply(parts, `[`, character(1), 2L))
      quarter <- as.integer(vapply(parts, `[`, character(1), 3L))
      as.Date(sprintf("%d-%02d-01", year, (quarter - 1L) * 3L + 1L))
    },
    stop("Unknown date_format: \"", fmt, "\". Only \"Y\" and \"y-q\" are supported.")
  )

  df[[col]] <- NULL
  df$time <- time
  df
}

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
    parse_period_date(date_format = c(obsTime = d_format)) |>
    dplyr::rename(tidyselect::any_of(c(obsValue = "ObsValue"))) |>
    dplyr::mutate(
      values = as.numeric(obsValue),
      .keep = "unused"
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                                ~ factor(.x, levels = unique(.x))))

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
#' (\code{https://sdmx.oecd.org/public/rest/data/...}), reads the response
#' with \code{xml2} and flattens its SDMX-ML Generic data into a plain data
#' frame. Used in place of the \code{OECD} package
#' (\code{github::expersso/OECD}, not on CRAN and not kept up with this
#' newer API) and, as of this version, without \code{rsdmx} either: its own
#' bundled \code{"OECD"} service provider profile still points at OECD's
#' older \code{stats.oecd.org} warehouse, which does not know the dataflow
#' ids (such as \code{"DSD_PDB@DF_PDB"}) this package's
#' `data-raw/get_oecd_*.R` scripts already use, and using it only as a
#' generic SDMX-ML reader (as an earlier version of this function did) was
#' one more dependency for what \code{xml2} alone already does.
#'
#' Returns a plain data frame, shaped like \code{\link{oecd_clean_data}}
#' expects: one row per observation, a column per SDMX dimension and series
#' or observation level attribute, \code{obsTime} and \code{obsValue}.
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
#' @param connecttimeout,timeout Connection and total request time limits (in
#'   seconds), passed straight to \code{\link[curl]{new_handle}}. Plain
#'   \code{xml2::read_xml(url)} opens the connection through \code{curl}
#'   without any time limit of its own, so a stalled server (or a network
#'   path that accepts the connection but never finishes sending data) would
#'   otherwise hang indefinitely instead of raising an error.
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
                             base_url = "https://sdmx.oecd.org/public/rest",
                             connecttimeout = 30, timeout = 180) {
  url <- .oecd_dataset_url(dataset, filter, start, end, base_url)
  handle <- curl::new_handle(connecttimeout = connecttimeout, timeout = timeout)
  con <- curl::curl(url, handle = handle)
  doc <- xml2::read_xml(con)
  .sdmx_generic_to_df(doc)
}

#' Flatten an SDMX-ML Generic data document into a data frame
#'
#' A minimal SDMX-ML 2.1 "Generic" data reader, covering just the structure
#' OECD's SDMX REST API returns for a plain \code{Accept: application/xml}
#' request (no \code{dsd} or structure-specific negotiation): one
#' \verb{<Series>} per dimension combination, each with a \verb{<SeriesKey>}
#' and optional series level \verb{<Attributes>}, holding one or more
#' \verb{<Obs>}, each with a single \verb{<ObsDimension>} (the time period),
#' an \verb{<ObsValue>} and optional observation level \verb{<Attributes>}.
#' Elements are matched by local name only, so the result does not depend on
#' which namespace prefix a particular server happens to use.
#'
#' @param doc An \code{xml2} document or node, as read by
#'   \code{\link[xml2]{read_xml}}.
#'
#' @return A data frame with one row per observation: a column per SDMX
#'   dimension and series/observation attribute, \code{obsTime} (the raw
#'   period string, e.g. \code{"2021"} or \code{"2021-Q1"}) and
#'   \code{obsValue} (numeric).
#'
#' @details
#' A query that returns many thousands of \verb{<Series>} (the OECD PDB
#' industry table, for instance, can be well over 100000 series, since it
#' leaves the industry dimension unfiltered) makes this function's own
#' performance matter: each \code{xml2}/\code{local-name()} XPath search
#' here costs several milliseconds in a document that size, seemingly
#' regardless of how small its own match is, so running one such search
#' per \verb{<Series>} or \verb{<Obs>} individually turned such a response
#' into many minutes of parsing (and, with a CI timeout in place, a failed
#' build). Everything below instead runs a handful of XPath searches
#' against the whole document (or, for \code{xml_find_first()}, against a
#' whole nodeset at once, which is internally vectorised over every node in
#' it), counts each \verb{<Series>}'s \verb{<Obs>} from its already-known
#' child count rather than a further search, and reassembles the result
#' with ordinary vector/data-frame operations - no XPath search is run
#' per-Series or per-Obs at all.
#'
#' @keywords internal
.sdmx_generic_to_df <- function(doc) {
  series_nodes <- xml2::xml_find_all(doc, ".//*[local-name()='Series']")
  if (!length(series_nodes)) {
    stop("No <Series> found in the SDMX-ML response; the query may have ",
         "no matching data, or the response is not SDMX-ML Generic data.")
  }
  n_series <- length(series_nodes)

  # SeriesKey always enumerates the same dimensions, in the same order, for
  # every Series in one response (they share the same DSD), so this can be
  # reshaped straight into a one-row-per-series matrix.
  key_values <- xml2::xml_find_all(doc, ".//*[local-name()='Series']/*[local-name()='SeriesKey']/*[local-name()='Value']")
  k <- length(key_values) / n_series
  if (k != round(k) || k == 0) {
    stop("Every <Series> was expected to have the same number of <SeriesKey> dimensions.")
  }
  key_id  <- xml2::xml_attr(key_values, "id")
  key_val <- xml2::xml_attr(key_values, "value")
  key_id_mat <- matrix(key_id, ncol = k, byrow = TRUE)
  if (!all(apply(key_id_mat, 2, function(x) length(unique(x)) == 1L))) {
    stop("<SeriesKey> dimensions were not in the same order in every <Series>.")
  }
  key_df <- as.data.frame(matrix(key_val, ncol = k, byrow = TRUE), stringsAsFactors = FALSE)
  names(key_df) <- key_id_mat[1, ]

  # Series/Obs level Attributes are optional and can vary from one
  # Series/Obs to the next (unlike SeriesKey above), so they can't be
  # reshaped into a fixed-width matrix; widen each into one row per
  # Series/Obs instead. A Series or Obs has at most one <Attributes>
  # wrapper, so xml_find_first() (vectorised: one call for the whole
  # nodeset) gives NA for those with none, with zero or more <Value>
  # children each.
  widen_attrs <- function(parent_nodes, wrap) {
    present <- !is.na(wrap)
    values  <- xml2::xml_find_all(wrap[present], "./*[local-name()='Value']")
    owner   <- rep(which(present), times = xml2::xml_length(wrap[present]))
    long <- data.frame(.row = owner,
                        id = xml2::xml_attr(values, "id"),
                        value = xml2::xml_attr(values, "value"),
                        stringsAsFactors = FALSE)

    if (!nrow(long)) {
      return(as.data.frame(matrix(nrow = length(parent_nodes), ncol = 0)))
    }
    wide <- tidyr::pivot_wider(long, id_cols = ".row", names_from = "id", values_from = "value")
    out  <- dplyr::left_join(data.frame(.row = seq_along(parent_nodes)), wide, by = ".row")
    out[, setdiff(names(out), ".row"), drop = FALSE]
  }

  sattr_wrap <- xml2::xml_find_first(series_nodes, "./*[local-name()='Attributes']")
  sattr_df   <- widen_attrs(series_nodes, sattr_wrap)

  # <Series> has exactly one <SeriesKey> child, an optional <Attributes>
  # child (just looked up above) and otherwise only <Obs> children, so its
  # Obs count falls out of xml_length() (a cheap direct child count, not an
  # XPath search) without any search at all - the OECD PDB industry table,
  # for instance, can have well over 100 000 <Series>, at which point even
  # one XPath search per Series adds up.
  obs_count <- xml2::xml_length(series_nodes) - 1L - as.integer(!is.na(sattr_wrap))
  obs_series_idx <- rep(seq_len(n_series), times = obs_count)

  obs_nodes <- xml2::xml_find_all(doc, ".//*[local-name()='Series']/*[local-name()='Obs']")
  obs_time  <- xml2::xml_attr(xml2::xml_find_first(obs_nodes, "./*[local-name()='ObsDimension']"), "value")
  obs_value <- as.numeric(xml2::xml_attr(xml2::xml_find_first(obs_nodes, "./*[local-name()='ObsValue']"), "value"))
  oattr_wrap <- xml2::xml_find_first(obs_nodes, "./*[local-name()='Attributes']")
  oattr_df   <- widen_attrs(obs_nodes, oattr_wrap)

  dplyr::bind_cols(
    key_df[obs_series_idx, , drop = FALSE],
    sattr_df[obs_series_idx, , drop = FALSE],
    data.frame(obsTime = obs_time, obsValue = obs_value, stringsAsFactors = FALSE),
    oattr_df
  )
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

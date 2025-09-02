#' Convert national currency values into another currency (with optional baseline date)
#'
#' Converts values expressed in national currency (identified by `geo`)
#' into a target currency using an EUR-anchored exchange rate table.
#' Optionally, a `base_time` can be provided so that all conversions use
#' exchange rates from that single date.
#'
#' @param values Numeric vector of values in national currency.
#' @param geo Character or factor vector of country/area codes (same length as values).
#' @param time Date vector of observation dates (same length as values).
#' @param to Target currency code (e.g. "USD").
#' @param exch Exchange rate table, defaults to \code{exh_eur_a}. Must contain
#'   columns: `time` (Date), `geo` (chr), `currency` (chr), `values` (numeric),
#'   where `values` is the rate "1 EUR = values units of currency".
#' @param euro_geos Character vector of geo codes that are already in EUR
#'   (default: c("EA","EA19","EA20")). For these, the EUR->nat factor is 1.
#' @param base_time Optional baseline date (Date or coercible). If supplied,
#'   all conversions use exchange rates from this date for both EUR->nat and
#'   EUR->to. If \code{NULL} (default), rates are taken at each row's `time`.
#' @param warn_if_missing Logical; warn when required rates are missing. Default TRUE.
#'
#' @return Numeric vector of converted values (same length as `values`).
#'
#' @examples
#' \dontrun{
#' # Per-row dates:
#' df <- dplyr::mutate(df,
#'   values_usd = convert_currency(values, geo, time, to = "USD")
#' )
#'
#' # Using baseline date (e.g., convert everything using 2020-01-01 rates):
#' df <- dplyr::mutate(df,
#'   values_usd_2020 = convert_currency(values, geo, time, to = "USD",
#'                                      base_time = as.Date("2020-01-01"))
#' )
#' }
#' @export
convert_currency <- function(values, geo, time,
                             to,
                             exch = exh_eur_a,
                             euro_geos = c("EA","EA19","EA20"),
                             base_time = NULL,
                             warn_if_missing = TRUE) {
  stopifnot(!missing(to), nzchar(to))
  n <- length(values)
  if (!(length(geo) == n && length(time) == n)) {
    stop("'values', 'geo', and 'time' must have the same length.")
  }
  if (!all(c("time", "geo", "currency", "values") %in% names(exch))) {
    stop("'exch' must contain columns: time, geo, currency, values.")
  }
  if (is.factor(geo)) geo <- as.character(geo)
  if (!inherits(time, "Date")) time <- as.Date(time)

  # Coerce/validate base_time if provided
  if (!is.null(base_time)) {
    if (!inherits(base_time, "Date")) {
      base_time <- try(as.Date(base_time), silent = TRUE)
      if (inherits(base_time, "try-error") || is.na(base_time)) {
        stop("'base_time' must be a Date or coercible to Date.")
      }
    }
  }

  # Prepare outputs
  r_nat <- rep(NA_real_, n)
  r_to  <- rep(NA_real_, n)

  # EUR->nat: euro area geos = 1
  euro_idx <- geo %in% euro_geos
  if (any(euro_idx)) r_nat[euro_idx] <- 1

  if (is.null(base_time)) {
    # --- Per-row date mode -----------------------------------------------
    # Lookup EUR->nat by (time, geo)
    if (any(!euro_idx)) {
      key_vec  <- paste0(as.integer(time[!euro_idx]), "||", geo[!euro_idx])
      exch_key <- paste0(as.integer(exch$time), "||", exch$geo)
      m        <- match(key_vec, exch_key)
      r_nat[!euro_idx] <- exch$values[m]
    }
    # EUR->to depends only on time
    sub_to     <- exch[exch$currency == to, c("time", "values")]
    sub_to_key <- as.integer(sub_to$time)
    r_to       <- sub_to$values[match(as.integer(time), sub_to_key)]
  } else {
    # --- Baseline date mode ----------------------------------------------
    # EUR->nat at base_time: build a per-geo map from that date
    exch_bt <- exch[exch$time == base_time, ]
    if (nrow(exch_bt) == 0L && warn_if_missing) {
      warning("No exchange rate rows found at base_time = ", format(base_time), ".")
    }
    # Make a simple geo -> rate lookup (if multiple rows per geo, take first)
    # euro geos already set to 1
    if (any(!euro_idx)) {
      # choose first occurrence by geo
      bt_order <- !duplicated(exch_bt$geo)
      geo_map  <- exch_bt$values[bt_order]
      names(geo_map) <- exch_bt$geo[bt_order]
      r_nat[!euro_idx] <- geo_map[geo[!euro_idx]]
    }
    # EUR->to at base_time: single scalar rate for all rows
    r_to_scalar <- exch$values[exch$currency == to & exch$time == base_time]
    if (length(r_to_scalar) > 0L) {
      r_to[] <- r_to_scalar[[1]]
    } else {
      r_to[] <- NA_real_
    }
  }

  if (warn_if_missing) {
    if (anyNA(r_nat)) warning("Missing EUR->nat rate for some (time, geo) pairs.")
    if (anyNA(r_to))  warning("Missing EUR->", to, " rate for some time(s).")
  }

  # Convert: nat -> EUR -> target
  (values / r_nat) * r_to
}

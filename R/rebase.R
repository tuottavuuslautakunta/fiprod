#' Rebase a numeric series to an index with a chosen base year
#'
#' This function converts a numeric time series into an index, where the level
#' in the specified `baseyear` equals `basevalue`. The base-year value is the
#' mean of all observations in that year. Works in both base R and inside
#' `dplyr::mutate()`.
#'
#' @param x Numeric vector of values.
#' @param time A vector of dates or years corresponding to `x`.
#' @param baseyear Numeric year or vector of years to use as base.
#' @param basevalue Numeric index value assigned to the base period.
#'   If `NULL`, the base value becomes the mean of the base-year observations.
#'
#' @return A numeric vector of rebased index values.
#'
#' @examples
#' df |> mutate(index = rebase_index(values, time, 2007))
#' df |> mutate(index = rebase_index(values, time, baseyear = c(2006, 2007)))
#'
#' @export
rebase_index <- function(x, time, baseyear, basevalue = 100) {

  time_year <- if (lubridate::is.Date(time)) {
    lubridate::year(time)
  } else {
    time
  }

  x_base <- x[time_year %in% baseyear]
  x_base <- x_base[is.finite(x_base)]

  if (length(x_base) == 0) {
    stop("No finite values found for the selected baseyear(s).")
  }

  base_mean <- mean(x_base)

  if (is.null(basevalue)) {
    basevalue <- base_mean
  }

  if (base_mean == 0) {
    stop("Cannot rebase: mean of base-year values is zero.")
  }

  basevalue * x / base_mean
}

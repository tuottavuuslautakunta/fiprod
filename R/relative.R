library(dplyr)
library(rlang)

#' Relative to peer geometric mean (one column, tidy-eval)
#'
#' For each base observation (time × geo × variable slice), computes one
#' numeric column relative to the weighted geometric mean of its peers
#' (other countries) as defined by `weights`.
#'
#' All peers with positive weight must have finite and strictly positive
#' values; otherwise the peer mean is set to NA for that base observation.
#'
#' @param dat Data frame with at least columns `time` (Date), `geo`,
#'   the slice variables in `by_vars`, and the value column `value_col`.
#' @param weights Weight table with columns:
#'   `geo_base`, `time` (numeric year), `geo` (peer), `weight`.
#' @param value_col Unquoted column name in `dat` to use as value
#'   (e.g. `values`), must be numeric.
#' @param by_vars Character vector: slice variables that must match
#'   between base and peers. Default
#'   `c("var_id","measure","unit_measure","price_base")`.
#' @param out_col String: name of the output relative column.
#'   Defaults to `paste0("rel_", value_col_name)`.
#' @param mean_col String: name of the peer-mean column.
#'   Defaults to `paste0("geom_mean_peers_", value_col_name)`.
#'
#' @return `dat` with two extra columns:
#'   - `mean_col`: weighted geometric mean of peers
#'   - `out_col`: value_col / mean_col
#'
#' @examples
#' # relative ULC:
#' # dat_rel <- relative_to_peer_geom_mean(
#' #   dat_oecd_pdb_ulc,
#' #   values,
#' #   weights_ecfin20,
#' #   out_col = "ulc_rel"
#' # )
#' @export
relative_to_peer_geom_mean <- function(
    dat,
    value_col,
    weights,
    by_vars = c("var_id","measure","unit_measure","price_base"),
    out_col = NULL,
    mean_col = NULL
) {
  val_quo  <- enquo(value_col)
  val_name <- as_name(val_quo)

  if (is.null(out_col))  out_col  <- paste0("rel_", val_name)
  if (is.null(mean_col)) mean_col <- paste0("geom_mean_peers_", val_name)

  # Basic checks
  need_dat <- c("time", "geo", by_vars, val_name)
  if (!all(need_dat %in% names(dat))) {
    stop("Missing columns in 'dat': ",
         paste(setdiff(need_dat, names(dat)), collapse = ", "))
  }
  if (!all(c("geo_base", "time", "geo", "weight") %in% names(weights))) {
    stop("Missing columns in 'weights': geo_base, time, geo, weight.")
  }

  # Prepare data: year + character geo
  dat2 <- dat %>%
    mutate(
      year = as.integer(format(.data$time, "%Y")),
      geo  = as.character(.data$geo)
    )

  # Base rows (each country/time/slice combination)
  base_rows <- dat2 %>%
    select(.data$time, .data$year, base_geo = .data$geo, all_of(by_vars)) %>%
    distinct()

  # Attach weights (per base_geo × year)
  base_with_weights <- base_rows %>%
    left_join(
      weights %>% rename(year = .data$time, peer_geo = .data$geo),
      by = c("base_geo" = "geo_base", "year" = "year")
    )

  # Peer values for the chosen column
  peers_values <- dat2 %>%
    transmute(
      .data$time,
      peer_geo = .data$geo,
      !!!syms(by_vars),
      peer_value = !!val_quo
    )

  base_peer <- base_with_weights %>%
    left_join(
      peers_values,
      by = c("time", "peer_geo", by_vars)
    )

  # Compute peer geometric mean per base row (time × base_geo × slice)
  peer_means <- base_peer %>%
    group_by(.data$time, .data$base_geo, across(all_of(by_vars))) %>%
    summarise(
      geom_mean_peers = {
        # Peers with positive weight are required
        req <- (.data$weight > 0) & is.finite(.data$weight)
        w_req <- .data$weight[req]
        x_req <- .data$peer_value[req]

        # All required peers must have finite, strictly positive values
        complete <- length(w_req) > 0 &&
          all(is.finite(x_req)) &&
          all(x_req > 0)

        if (!complete) {
          NA_real_
        } else {
          wsum <- sum(w_req)
          if (wsum <= 0) {
            NA_real_
          } else {
            exp(sum(w_req * log(x_req)) / wsum)
          }
        }
      },
      .groups = "drop"
    )

  # Join back and compute relative value
  out <- dat2 %>%
    left_join(
      peer_means,
      by = c("time", "geo" = "base_geo", by_vars)
    ) %>%
    mutate(
      !!mean_col := .data$geom_mean_peers,
      !!out_col  := (!!val_quo) / .data[[mean_col]]
    ) %>%
    select(-.data$geom_mean_peers)  # keep only named mean_col

  out
}

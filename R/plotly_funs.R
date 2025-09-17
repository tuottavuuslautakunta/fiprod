#' Customize Plotly chart: keep only download button and preselect geos
#'
#' This helper modifies a Plotly object (typically created with
#' \code{\link[plotly]{ggplotly}}) so that:
#' \itemize{
#'   \item Only the "Download as PNG" button is shown in the modebar.
#'   \item The Plotly logo is hidden.
#'   \item Only the geos listed in \code{keep_geos} are visible at start;
#'     all other traces are set to \code{visible = "legendonly"}.
#' }
#'
#' @param p A Plotly object.
#' @param keep_geos Character vector of geo codes to be visible initially.
#'
#' @return A modified Plotly object with customized modebar and visibility.
#'
#' @examples
#' \dontrun{
#' pp <- ggplot2::ggplot(dat, ggplot2::aes(time, values, colour = geo)) +
#'   ggplot2::geom_line()
#' p <- plotly::ggplotly(pp)
#' p2 <- plotly_hidden(p, keep_geos = c("FI","SE","EA20","US"))
#' p2
#' }
#' @export
plotly_hidden <- function(p, keep_geos) {
  # Keep only downloadImage, hide logo
  p <- plotly::config(
    p,
    modeBarButtonsToKeep = c("downloadImage"),
    displaylogo = FALSE
  )

  # Adjust visibility of traces based on geo
  p$x$data <- lapply(p$x$data, function(tr) {
    nm <- tr$name
    if (is.null(nm) || !nzchar(nm)) nm <- tr$legendgroup
    # Clean up labels (ggplotly sometimes uses "geo=FI" etc.)
    nm <- sub("^geo=", "", nm)
    nm <- sub(",.*$", "", nm)

    if (!is.null(nm) && !(nm %in% keep_geos)) {
      tr$visible <- "legendonly"
    }
    tr$showlegend <- TRUE
    tr
  })

  p
}

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
    modeBarButtonsToRemove = c(
      "zoom2d", "pan2d", "select2d", "lasso2d",
      "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines"
    ),
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


#' Prefade non-selected geos in a Plotly chart and toggle color/legend on click
#'
#' Takes a Plotly object (e.g., from \code{\link[plotly]{ggplotly}}),
#' keeps traces listed in \code{keep_geos} with their original colors and visible
#' in the legend, and renders all other traces initially in a light gray color
#' (hidden from the legend). Clicking a gray line toggles it to its original
#' color and adds it to the legend; clicking again fades it back and hides it.
#'
#' Trace name detection is based on \code{trace$name} or \code{trace$legendgroup}
#' after stripping common prefixes (e.g. \code{"geo="}) and anything after a comma.
#'
#' @param p A Plotly object created e.g. by \code{\link[plotly]{ggplotly}}.
#' @param keep_geos Character vector of geo codes that should start colored and
#'   visible in the legend.
#' @param gray_color Hex color used for faded traces (default \code{"#d3d3d3"}).
#' @param keep_download_only Logical; if \code{TRUE} (default) keep only the
#'   "Download as PNG" button and hide the Plotly logo.
#'
#' @return A modified Plotly object with custom initial styling and an interactive
#'   click handler to toggle trace color and legend visibility.
#'
#' @examples
#' \dontrun{
#' pp <- ggplot2::ggplot(dat, ggplot2::aes(time, values, colour = geo)) +
#'   ggplot2::geom_line()
#' p  <- plotly::ggplotly(pp)
#' p2 <- plotly_prefade_others(p, keep_geos = c("FI","SE","EA20","US"))
#' p2
#' }
#' @export
plotly_prefade_others <- function(p,
                                  keep_geos,
                                  gray_color = "#d3d3d3",
                                  keep_download_only = TRUE) {
  stopifnot(inherits(p, "plotly"))

  # Optional: only keep the download button
  if (isTRUE(keep_download_only)) {
    p <- plotly::config(
      p,
      modeBarButtonsToRemove = c(
        "zoom2d", "pan2d", "select2d", "lasso2d",
        "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines"
      ),
      displaylogo = FALSE
    )
  }

  # Pre-process traces: set faded style for non-kept and store state in 'meta'
  p$x$data <- lapply(p$x$data, function(tr) {
    nm <- tr$name
    if (is.null(nm) || !nzchar(nm)) nm <- tr$legendgroup
    nm <- sub("^geo=", "", nm %||% "")
    nm <- sub(",.*$", "", nm)

    # ensure meta is a list we can extend
    if (is.null(tr$meta)) tr$meta <- list()

    # try to capture current original color (if present)
    orig_col <- tryCatch(tr$line$color, error = function(...) NULL)

    if (!is.null(nm) && !(nm %in% keep_geos)) {
      # fade non-kept: gray, slightly transparent, hide from legend
      if (is.null(tr$line)) tr$line <- list()
      tr$line$color <- gray_color
      tr$opacity    <- if (is.null(tr$opacity)) 0.6 else tr$opacity
      tr$showlegend <- FALSE
      # store state in allowed 'meta'
      tr$meta$faded     <- TRUE
      tr$meta$origColor <- orig_col
    } else {
      tr$showlegend <- TRUE
      tr$meta$faded     <- FALSE
      tr$meta$origColor <- orig_col
    }

    tr
  })

  # Attach a click handler that toggles using only valid attributes
  js <- paste0(
    'function(el, x) {
  var gd = el;

  el.on("plotly_click", function(e) {
    if (!e || !e.points || !e.points.length) return;
    var i  = e.points[0].curveNumber;
    var tr = gd.data[i];
    if (!tr) return;

    // defaults
    tr.line = tr.line || {};
    tr.meta = tr.meta || {};

    var faded = !!tr.meta.faded;
    var newColor, newOpacity, newShowLegend, newMeta;

    if (faded) {
      // restore original color (if known) else remove color to let Plotly recolor
      newColor      = (tr.meta.origColor != null) ? tr.meta.origColor : null;
      newOpacity    = 1;
      newShowLegend = true;
      newMeta       = Object.assign({}, tr.meta, { faded: false });
    } else {
      // save current color if not yet saved
      if (tr.meta.origColor == null && tr.line && tr.line.color != null) {
        tr.meta.origColor = tr.line.color;
      }
      newColor      = "', gray_color, '";
      newOpacity    = 0.6;
      newShowLegend = false;
      newMeta       = Object.assign({}, tr.meta, { faded: true });
    }

    // Apply changes to this trace only
    Plotly.restyle(gd, {
      "line.color": [newColor],
      "opacity":    [newOpacity],
      "showlegend": [newShowLegend],
      "meta":       [newMeta]
    }, [i]);
  });
}'
  )

  p <- htmlwidgets::onRender(p, js)
  p
}

# internal null-coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a



## Saving report figures ------------------------------------------------------
##
## The productivity board's report is written elsewhere and takes the figures as
## files, one per year of publication: a png to look at and a pdf for the
## layout. The defaults below are what the report wants; `set_fig_defaults()` is
## there so that a change of size, format or folder is a one line change in the
## vignette rather than an edit to every figure.
##
## save_fig_captioned() writes a second, standalone version of the same figure:
## a wider png with the title, subtitle and source drawn on the image itself,
## for uses where the figure travels without the report's own running text.

.fig_defaults <- list(
  dir    = "figures",
  year   = NULL,       # NULL: the current year
  width  = 13.5,
  height = 8.5,
  units  = "cm",
  dpi    = 300,
  device = c("png", "pdf")   # one file per format
)

.fig_captioned_defaults <- list(
  dir    = file.path("figures", "otsikoilla"),
  year   = NULL,       # NULL: the current year
  width  = 16,
  height = 12,
  units  = "cm",
  dpi    = 300,
  device = "png",
  # Text sizes in points. A captioned figure is bigger than the report's own,
  # so its text needs to grow with it, the title most of all; NULL leaves that
  # element at whatever size the plot already has.
  title_size    = 20,
  subtitle_size = 14,
  text_size     = 12,   # axis text, legend text, facet strip text
  caption_size  = 10,
  # Line width in characters for title/subtitle/caption, tuned for the sizes
  # above at the width set for the figure; NULL turns wrapping off for that
  # element. A bigger title/subtitle/caption or a narrower figure needs a
  # smaller number here, or the text runs off the edge instead of wrapping.
  title_wrap    = 40,
  subtitle_wrap = 65,
  caption_wrap  = 90
)

#' Defaults for saving report figures
#'
#' The settings [save_fig()] uses when it is not told otherwise: the folder, the
#' year that names the subfolder, and the size, resolution and format of the
#' files.
#'
#' @param ... Named settings to change: `dir`, `year`, `width`, `height`,
#'   `units`, `dpi` or `device`. `year = NULL` means the current year and
#'   `device` may name several formats, such as `c("png", "pdf")`.
#'
#' @return `set_fig_defaults()` returns the previous settings invisibly, so they
#'   can be restored. `fig_defaults()` returns the settings in force.
#'
#' @seealso [save_fig()]
#'
#' @examples
#' fig_defaults()
#'
#' old <- set_fig_defaults(dir = "kuviot", year = 2026)
#' fig_defaults()$dir
#'
#' set_fig_defaults(!!!old)
#'
#' @export
set_fig_defaults <- function(...) {
  new <- rlang::list2(...)
  if (length(new) && (is.null(names(new)) || any(!nzchar(names(new))))) {
    stop("All settings must be named, e.g. `set_fig_defaults(year = 2026)`.")
  }
  unknown <- setdiff(names(new), names(.fig_defaults))
  if (length(unknown)) {
    stop("Unknown setting(s): ", paste(unknown, collapse = ", "),
         ". Known: ", paste(names(.fig_defaults), collapse = ", "), ".")
  }

  old <- fig_defaults()
  # a NULL is a value here (year = NULL means "this year"), so modifyList,
  # which would drop it, is not what we want
  set <- getOption("fiprod.fig", list())
  for (nm in names(new)) set[nm] <- list(new[[nm]])
  options(fiprod.fig = set)

  invisible(old)
}

#' @rdname set_fig_defaults
#' @export
fig_defaults <- function() {
  set <- getOption("fiprod.fig", list())
  out <- .fig_defaults
  for (nm in intersect(names(set), names(out))) out[nm] <- list(set[[nm]])
  out
}

#' Defaults for saving standalone, captioned report figures
#'
#' The settings [save_fig_captioned()] uses when it is not told otherwise: the
#' folder, the year that names the subfolder, the size and resolution of the
#' file, the point size of the title, subtitle, axis/legend/strip text and
#' caption, and the line width each of the title, subtitle and caption is
#' wrapped at.
#'
#' @inheritParams set_fig_defaults
#' @param ... Named settings to change: `dir`, `year`, `width`, `height`,
#'   `units`, `dpi`, `device`, `title_size`, `subtitle_size`, `text_size`,
#'   `caption_size`, `title_wrap`, `subtitle_wrap` or `caption_wrap`.
#'   `year = NULL` means the current year; any `_size` or `_wrap` set to
#'   `NULL` leaves that element as the plot already has it (for a `_size`) or
#'   turns off its wrapping (for a `_wrap`).
#'
#' @return `set_fig_captioned_defaults()` returns the previous settings
#'   invisibly, so they can be restored. `fig_captioned_defaults()` returns the
#'   settings in force.
#'
#' @seealso [save_fig_captioned()]
#'
#' @examples
#' fig_captioned_defaults()
#'
#' old <- set_fig_captioned_defaults(dir = "kuviot", year = 2026)
#' fig_captioned_defaults()$dir
#'
#' set_fig_captioned_defaults(!!!old)
#'
#' @export
set_fig_captioned_defaults <- function(...) {
  new <- rlang::list2(...)
  if (length(new) && (is.null(names(new)) || any(!nzchar(names(new))))) {
    stop("All settings must be named, e.g. `set_fig_captioned_defaults(year = 2026)`.")
  }
  unknown <- setdiff(names(new), names(.fig_captioned_defaults))
  if (length(unknown)) {
    stop("Unknown setting(s): ", paste(unknown, collapse = ", "),
         ". Known: ", paste(names(.fig_captioned_defaults), collapse = ", "), ".")
  }

  old <- fig_captioned_defaults()
  set <- getOption("fiprod.fig_captioned", list())
  for (nm in names(new)) set[nm] <- list(new[[nm]])
  options(fiprod.fig_captioned = set)

  invisible(old)
}

#' @rdname set_fig_captioned_defaults
#' @export
fig_captioned_defaults <- function() {
  set <- getOption("fiprod.fig_captioned", list())
  out <- .fig_captioned_defaults
  for (nm in intersect(names(set), names(out))) out[nm] <- list(set[[nm]])
  out
}

#' Write a plot to `<dir>/<year>/<name>.<device>` for each device
#'
#' Shared by [save_fig()] and [save_fig_captioned()]: validates `name`,
#' creates the year folder and writes one file per format in `opts$device`.
#'
#' @param plot The already themed plot to write.
#' @param name File name without the extension.
#' @param opts A settings list with `dir`, `year`, `width`, `height`, `units`,
#'   `dpi` and `device`, as returned by [fig_defaults()] or
#'   [fig_captioned_defaults()].
#' @keywords internal
.save_fig_files <- function(plot, name, opts) {
  if (basename(name) != name) {
    stop("`name` is a file name, not a path: ", name,
         ". Use the `dir` setting for the folder.")
  }

  year <- opts$year %||% format(Sys.Date(), "%Y")
  dir <- file.path(opts$dir, as.character(year))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  devices <- as.character(opts$device)
  if (!length(devices) || any(!nzchar(devices))) {
    stop("`device` must name at least one file format, e.g. c(\"png\", \"pdf\").")
  }

  for (device in devices) {
    ggplot2::ggsave(file.path(dir, paste0(name, ".", device)),
                    plot = plot,
                    width = opts$width, height = opts$height, units = opts$units,
                    dpi = opts$dpi, device = device)
  }

  invisible(NULL)
}

#' Save a report figure
#'
#' Writes a figure to `<dir>/<year>/<name>.<device>`, one file for each format
#' in `device` and by default both a png and a pdf, creating the folder if it is
#' not there. The year is the year of publication, so that the figures of
#' successive reports stay side by side instead of overwriting each other.
#'
#' The plot is returned, so a chunk that ends in `save_fig()` both writes the
#' file and shows the figure:
#'
#' ```
#' dat |>
#'   ggplot2::ggplot(...) |>
#'   save_fig("bkt-per-capita")
#' ```
#'
#' @param plot A plot object, normally a `ggplot`.
#' @param name File name without the extension.
#' @param ... Settings for this call only, overriding [fig_defaults()]: `dir`,
#'   `year`, `width`, `height`, `units`, `dpi`, `device`. `device` may name
#'   several formats; each one is written as its own file.
#'
#' @return `plot`, so that the figure is still drawn.
#'
#' @seealso [set_fig_defaults()] to change the defaults for every figure at
#'   once.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#' save_fig(p, "wt-mpg", dir = tempdir())
#'
#' # only one format for this figure
#' save_fig(p, "wt-mpg", dir = tempdir(), device = "png")
#' }
#'
#' @export
save_fig <- function(plot, name, ...) {
  if (!rlang::is_string(name) || !nzchar(name)) {
    stop("`name` must be a single non empty string.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Saving a figure needs the ggplot2 package.")
  }

  opts <- fig_defaults()
  new <- rlang::list2(...)
  unknown <- setdiff(names(new), names(opts))
  if (length(unknown)) {
    stop("Unknown setting(s): ", paste(unknown, collapse = ", "), ".")
  }
  for (nm in names(new)) opts[nm] <- list(new[[nm]])

  # the margin is the same in every file, so the plot is built once
  p <- plot + ggplot2::theme(plot.margin = ggplot2::margin(4, 2, 2, 2))
  .save_fig_files(p, name, opts)

  plot
}

#' Save a standalone, captioned report figure
#'
#' Writes a second version of a report figure to
#' `<dir>/<year>/<name>.<device>`, wider than [save_fig()]'s and with `title`,
#' `subtitle` and `caption` drawn on the plot itself (via [ggplot2::labs()]),
#' for uses where the figure travels on its own, without the running text that
#' carries the title, subtitle and source next to [save_fig()]'s output.
#' `dir` defaults to a sibling of [save_fig()]'s own folder
#' (`figures/otsikoilla/<year>/` next to `figures/<year>/`), so the two never
#' collide, and the file name is normally the same as the one passed to
#' [save_fig()] for the same plot.
#'
#' A chunk that already ends in `save_fig()` gets the captioned version with
#' one more line:
#'
#' ```
#' p <- dat |> ggplot2::ggplot(...)
#' save_fig(p, "bkt-per-capita")
#' save_fig_captioned(p, "bkt-per-capita",
#'   title = "BKT per capita",
#'   subtitle = "Vuoden 2020 $ hinnoin ostovoimakorjattuna",
#'   caption = "Lähde: Eurostat, OECD, Tuottavuuslautakunta.")
#' ```
#'
#' Blanking the axis titles and legend title with `the_title_blank("xyl")`
#' (as the report's figures do) leaves the plot title, subtitle and caption
#' alone, so the same plot can be passed to both functions without change.
#'
#' The figure is bigger than [save_fig()]'s, so its text is set bigger too,
#' the title by the most: by default the title grows from about 12pt to 20pt
#' (a plot built at the report's usual `theme_fpb(base_size = 11)`), against a
#' more modest lift for the axis text, legend text and facet strip text (to
#' 12pt) and the caption (to 10pt). A figure's own `theme()` overrides for
#' these elements (a smaller legend text to fit a long legend, say) are
#' replaced along with the rest, so every captioned figure ends up with the
#' same text sizes regardless of what the small report figure needed.
#'
#' Each of `title`, `subtitle` and `caption` is wrapped to its own line width
#' (`title_wrap`, `subtitle_wrap`, `caption_wrap`) before being set, so that
#' growing the text (or narrowing the figure) doesn't just run it off the
#' edge; the defaults are sized for the default `title_size`/`subtitle_size`/
#' `caption_size` at the default `width`, so a call that changes one of those
#' should normally change the matching `_wrap` setting too.
#'
#' @param plot A plot object, normally a `ggplot`.
#' @param name File name without the extension, normally the same name used
#'   for the same plot's [save_fig()] call.
#' @param title,subtitle,caption Text for [ggplot2::labs()]. `NULL` leaves
#'   that label unset.
#' @param ... Settings for this call only, overriding
#'   [fig_captioned_defaults()]: `dir`, `year`, `width`, `height`, `units`,
#'   `dpi`, `device`, `title_size`, `subtitle_size`, `text_size`,
#'   `caption_size`, `title_wrap`, `subtitle_wrap`, `caption_wrap`.
#'
#' @return `plot`, so that the figure is still drawn.
#'
#' @seealso [set_fig_captioned_defaults()] to change the defaults for every
#'   captioned figure at once; [save_fig()] for the plain report figure.
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#' save_fig_captioned(p, "wt-mpg", dir = tempdir(),
#'   title = "Auton paino ja polttoaineenkulutus",
#'   subtitle = "Mailia gallonalla painon (1000 lbs) mukaan",
#'   caption = "Lähde: mtcars")
#' }
#'
#' @export
save_fig_captioned <- function(plot, name, title = NULL, subtitle = NULL,
                               caption = NULL, ...) {
  if (!rlang::is_string(name) || !nzchar(name)) {
    stop("`name` must be a single non empty string.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Saving a figure needs the ggplot2 package.")
  }

  opts <- fig_captioned_defaults()
  new <- rlang::list2(...)
  unknown <- setdiff(names(new), names(opts))
  if (length(unknown)) {
    stop("Unknown setting(s): ", paste(unknown, collapse = ", "), ".")
  }
  for (nm in names(new)) opts[nm] <- list(new[[nm]])

  wrap_lab <- function(x, width) {
    if (is.null(x) || is.null(width)) return(x)
    paste(strwrap(x, width = width), collapse = "\n")
  }
  title    <- wrap_lab(title,    opts$title_wrap)
  subtitle <- wrap_lab(subtitle, opts$subtitle_wrap)
  caption  <- wrap_lab(caption,  opts$caption_wrap)

  # element_text(size = NULL) leaves that property as the plot's own theme
  # already has it, so a NULL setting is "don't touch this element" here too.
  # axis.text.x/.y are set explicitly, not just their parent axis.text, so a
  # figure's own smaller override for one of them (to fit a long axis label in
  # the small report figure) is replaced as well, not left to shine through
  # underneath the bigger one. legend.title is left alone: most figures blank
  # it with the_title_blank("xyl"), and a non-blank element_text() here would
  # un-blank it (ggplot2 themes can turn element_blank() back on this way).
  et <- function(size) ggplot2::element_text(size = size)
  p <- plot +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme(
      plot.title      = et(opts$title_size),
      plot.subtitle   = et(opts$subtitle_size),
      plot.caption    = et(opts$caption_size),
      axis.text       = et(opts$text_size),
      axis.text.x     = et(opts$text_size),
      axis.text.y     = et(opts$text_size),
      legend.text     = et(opts$text_size),
      strip.text      = et(opts$text_size),
      plot.margin     = ggplot2::margin(4, 2, 2, 2)
    )
  .save_fig_files(p, name, opts)

  plot
}

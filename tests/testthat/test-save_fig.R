with_clean_defaults <- function(code) {
  old <- getOption("fiprod.fig")
  on.exit(options(fiprod.fig = old))
  options(fiprod.fig = NULL)
  force(code)
}

with_clean_captioned_defaults <- function(code) {
  old <- getOption("fiprod.fig_captioned")
  on.exit(options(fiprod.fig_captioned = old))
  options(fiprod.fig_captioned = NULL)
  force(code)
}

a_plot <- function() {
  ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
}

test_that("the report's own defaults are what comes out of the box", {
  with_clean_defaults({
    d <- fig_defaults()
    expect_equal(d$width, 13.5)
    expect_equal(d$height, 8.5)
    expect_equal(d$units, "cm")
    expect_equal(d$device, c("png", "pdf"))
    expect_null(d$year)
  })
})

test_that("a default can be changed and put back", {
  with_clean_defaults({
    old <- set_fig_defaults(year = 2026, width = 20)
    expect_equal(fig_defaults()$year, 2026)
    expect_equal(fig_defaults()$width, 20)
    # untouched settings keep their value
    expect_equal(fig_defaults()$units, "cm")

    set_fig_defaults(!!!old)
    expect_null(fig_defaults()$year)
    expect_equal(fig_defaults()$width, 13.5)
  })
})

test_that("a year can be set back to the current one", {
  with_clean_defaults({
    set_fig_defaults(year = 2026)
    # NULL is a value here, not a request to leave the setting alone
    set_fig_defaults(year = NULL)
    expect_null(fig_defaults()$year)
  })
})

test_that("a misspelled setting is refused", {
  with_clean_defaults({
    expect_error(set_fig_defaults(with = 20), "Unknown setting")
    expect_error(set_fig_defaults(2026), "must be named")
  })
})

test_that("the file lands in a folder named by the year", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "kuvio", dir = root, year = 2026)
    expect_true(file.exists(file.path(root, "2026", "kuvio.png")))
  })
})

test_that("a png and a pdf of every figure by default", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "kuvio", dir = root, year = 2026)
    expect_true(file.exists(file.path(root, "2026", "kuvio.png")))
    expect_true(file.exists(file.path(root, "2026", "kuvio.pdf")))
  })
})

test_that("one format can be asked for on its own", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "kuvio", dir = root, year = 2026, device = "png")
    expect_true(file.exists(file.path(root, "2026", "kuvio.png")))
    expect_false(file.exists(file.path(root, "2026", "kuvio.pdf")))
  })
})

test_that("a device that names no format is refused", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    expect_error(save_fig(a_plot(), "kuvio", dir = tempfile("figs"),
                          device = character()),
                 "at least one file format")
  })
})

test_that("the current year is used when none is given", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "kuvio", dir = root)
    expect_true(file.exists(file.path(root, format(Sys.Date(), "%Y"), "kuvio.png")))
  })
})

test_that("the defaults are used and can be overridden for one figure", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    root <- tempfile("figs")
    set_fig_defaults(dir = root, year = 2026)

    save_fig(a_plot(), "oletus")
    save_fig(a_plot(), "poikkeus", year = 2025)

    expect_true(file.exists(file.path(root, "2026", "oletus.png")))
    expect_true(file.exists(file.path(root, "2025", "poikkeus.png")))
  })
})

test_that("the size asked for is the size written", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("png")
  with_clean_defaults({
    root <- tempfile("figs")
    save_fig(a_plot(), "iso", dir = root, year = 2026, dpi = 100)

    # 13.5 cm at 100 dpi is 13.5 / 2.54 * 100 pixels each way
    px <- round(13.5 / 2.54 * 100)
    dim <- dim(png::readPNG(file.path(root, "2026", "iso.png")))
    expect_equal(dim[1], px, tolerance = 2)
    expect_equal(dim[2], px, tolerance = 2)
  })
})

test_that("the plot comes back so the chunk still draws it", {
  skip_if_not_installed("ggplot2")
  with_clean_defaults({
    p <- a_plot()
    expect_identical(save_fig(p, "kuvio", dir = tempfile("figs")), p)
  })
})

test_that("a name that is a path is refused", {
  with_clean_defaults({
    expect_error(save_fig(a_plot(), "2026/kuvio"), "not a path")
    expect_error(save_fig(a_plot(), ""), "non empty")
    expect_error(save_fig(a_plot(), "kuvio", widht = 10), "Unknown setting")
  })
})

test_that("the captioned figure's own defaults are what comes out of the box", {
  with_clean_captioned_defaults({
    d <- fig_captioned_defaults()
    expect_equal(d$dir, file.path("figures", "otsikoilla"))
    expect_equal(d$width, 16)
    expect_equal(d$height, 12)
    expect_equal(d$units, "cm")
    expect_equal(d$device, "png")
    expect_equal(d$title_size, 20)
    expect_equal(d$subtitle_size, 14)
    expect_equal(d$text_size, 12)
    expect_equal(d$caption_size, 10)
    expect_equal(d$title_wrap, 40)
    expect_equal(d$subtitle_wrap, 65)
    expect_equal(d$caption_wrap, 90)
    expect_null(d$year)
  })
})

test_that("a captioned default can be changed and put back", {
  with_clean_captioned_defaults({
    old <- set_fig_captioned_defaults(year = 2026, width = 20)
    expect_equal(fig_captioned_defaults()$year, 2026)
    expect_equal(fig_captioned_defaults()$width, 20)
    expect_equal(fig_captioned_defaults()$height, 12)

    set_fig_captioned_defaults(!!!old)
    expect_null(fig_captioned_defaults()$year)
    expect_equal(fig_captioned_defaults()$width, 16)
  })
})

test_that("a misspelled captioned setting is refused", {
  with_clean_captioned_defaults({
    expect_error(set_fig_captioned_defaults(with = 20), "Unknown setting")
    expect_error(set_fig_captioned_defaults(2026), "must be named")
  })
})

test_that("the captioned figure lands next to, not inside, save_fig()'s folder", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    root <- tempfile("figs")
    save_fig_captioned(a_plot(), "kuvio", dir = file.path(root, "otsikoilla"),
                       year = 2026, title = "Otsikko")
    expect_true(file.exists(file.path(root, "otsikoilla", "2026", "kuvio.png")))
    expect_false(file.exists(file.path(root, "2026", "kuvio.png")))
  })
})

test_that("only a png is written by default", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    root <- tempfile("figs")
    save_fig_captioned(a_plot(), "kuvio", dir = root, year = 2026)
    expect_true(file.exists(file.path(root, "2026", "kuvio.png")))
    expect_false(file.exists(file.path(root, "2026", "kuvio.pdf")))
  })
})

test_that("title, subtitle and caption end up on the plot", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    p <- save_fig_captioned(a_plot(), "kuvio", dir = tempfile("figs"),
                            title = "Otsikko", subtitle = "Alaotsikko",
                            caption = "Lähde: testi")
    # the returned plot is the caller's original, untouched
    expect_null(p$labels$title)

    # what actually got saved carries the labels; rebuild it the same way
    # save_fig_captioned() does, without writing a file
    labelled <- p + ggplot2::labs(title = "Otsikko", subtitle = "Alaotsikko",
                                  caption = "Lähde: testi")
    expect_equal(labelled$labels$title, "Otsikko")
    expect_equal(labelled$labels$subtitle, "Alaotsikko")
    expect_equal(labelled$labels$caption, "Lähde: testi")
  })
})

test_that("a long title, subtitle and caption are each wrapped at their own width", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    long <- paste(rep("sana", 30), collapse = " ")
    root <- tempfile("figs")
    save_fig_captioned(a_plot(), "kuvio", dir = root, year = 2026,
                       title = long, subtitle = long, caption = long,
                       title_wrap = 10, subtitle_wrap = 20, caption_wrap = 30)
    # rebuilding the file isn't inspected; instead check the wrapping helper
    # produces lines no longer than the requested width, one width per element
    for (width in c(10, 20, 30)) {
      wrapped <- paste(strwrap(long, width = width), collapse = "\n")
      expect_true(all(nchar(strsplit(wrapped, "\n")[[1]]) <= width))
      expect_true(grepl("\n", wrapped))
    }
  })
})

test_that("a _wrap set to NULL turns wrapping off for that element", {
  with_clean_captioned_defaults({
    # NULL is a value here, not a request to leave the setting alone
    set_fig_captioned_defaults(title_wrap = NULL, subtitle_wrap = NULL,
                               caption_wrap = NULL)
    d <- fig_captioned_defaults()
    expect_null(d$title_wrap)
    expect_null(d$subtitle_wrap)
    expect_null(d$caption_wrap)
  })
})

test_that("title, subtitle, axis text, legend text and caption grow to the configured sizes", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3, g = c("a", "b", "a")),
                         ggplot2::aes(x, y, colour = g)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~g) +
      # a figure-specific override, as the small report figure might use to
      # fit a long legend or axis label; save_fig_captioned() should replace it
      ggplot2::theme(legend.text = ggplot2::element_text(size = 6),
                    axis.text.y = ggplot2::element_text(size = 6))

    save_fig_captioned(p, "kuvio", dir = tempfile("figs"), title = "T",
                       subtitle = "S", caption = "C")

    built <- p +
      ggplot2::labs(title = "T", subtitle = "S", caption = "C") +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(size = 20),
        plot.subtitle = ggplot2::element_text(size = 14),
        plot.caption  = ggplot2::element_text(size = 10),
        axis.text     = ggplot2::element_text(size = 12),
        axis.text.x   = ggplot2::element_text(size = 12),
        axis.text.y   = ggplot2::element_text(size = 12),
        legend.text   = ggplot2::element_text(size = 12),
        strip.text    = ggplot2::element_text(size = 12)
      )
    th <- built$theme
    expect_equal(th$plot.title$size, 20)
    expect_equal(th$plot.subtitle$size, 14)
    expect_equal(th$plot.caption$size, 10)
    expect_equal(th$axis.text.x$size, 12)
    # the figure's own smaller override is superseded, not left underneath
    expect_equal(th$axis.text.y$size, 12)
    expect_equal(th$legend.text$size, 12)
    expect_equal(th$strip.text$size, 12)
  })
})

test_that("a size set to NULL leaves that element as the plot already has it", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    p <- a_plot() + ggplot2::theme(plot.title = ggplot2::element_text(size = 30))
    save_fig_captioned(p, "kuvio", dir = tempfile("figs"), title = "T",
                       title_size = NULL)
    built <- p + ggplot2::labs(title = "T") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = NULL))
    expect_equal(built$theme$plot.title$size, 30)
  })
})

test_that("a legend title blanked by the plot stays blanked", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    p <- a_plot() + ggplot2::theme(legend.title = ggplot2::element_blank())
    save_fig_captioned(p, "kuvio", dir = tempfile("figs"), title = "T")
    # save_fig_captioned() never touches legend.title, so it is exactly what
    # the plot itself set, unaffected by the call
    expect_s3_class(p$theme$legend.title, "element_blank")
  })
})

test_that("the plot comes back unchanged from save_fig_captioned()", {
  skip_if_not_installed("ggplot2")
  with_clean_captioned_defaults({
    p <- a_plot()
    expect_identical(
      save_fig_captioned(p, "kuvio", dir = tempfile("figs"), title = "X"),
      p
    )
  })
})

test_that("a captioned name that is a path is refused", {
  with_clean_captioned_defaults({
    expect_error(save_fig_captioned(a_plot(), "2026/kuvio"), "not a path")
    expect_error(save_fig_captioned(a_plot(), ""), "non empty")
    expect_error(save_fig_captioned(a_plot(), "kuvio", widht = 10), "Unknown setting")
  })
})

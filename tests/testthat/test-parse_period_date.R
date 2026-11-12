test_that("a 4-digit year is parsed as the first of that year", {
  out <- parse_period_date(data.frame(year = c("2020", "2021")), c(year = "Y"))
  expect_identical(out$time, as.Date(c("2020-01-01", "2021-01-01")))
  expect_false("year" %in% names(out))
})

test_that("a year-quarter string is parsed as the first day of that quarter", {
  out <- parse_period_date(
    data.frame(obsTime = c("2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4")),
    c(obsTime = "y-q")
  )
  expect_identical(
    out$time,
    as.Date(c("2021-01-01", "2021-04-01", "2021-07-01", "2021-10-01"))
  )
  expect_false("obsTime" %in% names(out))
})

test_that("the source column is renamed to time, other columns are kept", {
  out <- parse_period_date(
    data.frame(geo = c("FI", "SE"), year = c("2020", "2021"), values = c(1, 2)),
    c(year = "Y")
  )
  expect_setequal(names(out), c("geo", "time", "values"))
  expect_identical(out$geo, c("FI", "SE"))
  expect_identical(out$values, c(1, 2))
})

test_that("a numeric year column works the same as a character one", {
  out <- parse_period_date(data.frame(year = c(2020, 2021)), c(year = "Y"))
  expect_identical(out$time, as.Date(c("2020-01-01", "2021-01-01")))
})

test_that("an unknown format is refused", {
  expect_error(
    parse_period_date(data.frame(year = "2020"), c(year = "M")),
    "Unknown date_format"
  )
})

test_that("a year-quarter value that does not match the format is refused", {
  expect_error(
    parse_period_date(data.frame(obsTime = c("2021-Q1", "not-a-quarter")),
                      c(obsTime = "y-q")),
    "y-q"
  )
})

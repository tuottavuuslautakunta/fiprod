test_that("the request URL is built from the dataset, filter and base_url", {
  url <- fiprod:::.oecd_dataset_url(
    dataset  = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
    filter   = "FIN+USA.A.GVAHRS",
    base_url = "https://example.test/rest"
  )
  expect_identical(
    url,
    "https://example.test/rest/data/OECD.SDD.TPS,DSD_PDB@DF_PDB,/FIN+USA.A.GVAHRS"
  )
})

test_that("start and end become startPeriod/endPeriod query parameters", {
  url <- fiprod:::.oecd_dataset_url(
    dataset  = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
    filter   = "FIN.A.GVAHRS",
    start    = 2020,
    end      = 2021,
    base_url = "https://example.test/rest"
  )
  expect_identical(
    url,
    "https://example.test/rest/data/OECD.SDD.TPS,DSD_PDB@DF_PDB,/FIN.A.GVAHRS?startPeriod=2020&endPeriod=2021"
  )
})

test_that("start alone becomes just startPeriod", {
  url <- fiprod:::.oecd_dataset_url(
    dataset  = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
    filter   = "FIN.A.GVAHRS",
    start    = 2020,
    base_url = "https://example.test/rest"
  )
  expect_identical(
    url,
    "https://example.test/rest/data/OECD.SDD.TPS,DSD_PDB@DF_PDB,/FIN.A.GVAHRS?startPeriod=2020"
  )
})

test_that("neither start nor end adds a query string", {
  url <- fiprod:::.oecd_dataset_url(
    dataset  = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
    filter   = "FIN.A.GVAHRS",
    base_url = "https://example.test/rest"
  )
  expect_false(grepl("?", url, fixed = TRUE))
})

test_that("oecd_get_dataset() fetches the built URL and returns a data frame", {
  skip_if_not_installed("rsdmx")

  fake_sdmx <- structure(list(), class = "data.frame")
  called_with <- NULL
  testthat::local_mocked_bindings(
    readSDMX = function(file, isURL) {
      called_with <<- list(file = file, isURL = isURL)
      fake_sdmx
    },
    .package = "rsdmx"
  )

  out <- oecd_get_dataset(
    dataset  = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
    filter   = "FIN.A.GVAHRS",
    base_url = "https://example.test/rest"
  )

  expect_identical(
    called_with$file,
    "https://example.test/rest/data/OECD.SDD.TPS,DSD_PDB@DF_PDB,/FIN.A.GVAHRS"
  )
  expect_true(called_with$isURL)
  expect_s3_class(out, "data.frame")
})

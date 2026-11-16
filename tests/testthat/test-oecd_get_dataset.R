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

test_that("oecd_get_dataset() fetches the built URL and parses the response", {
  skip_if_not_installed("xml2")

  fake_doc <- xml2::read_xml(a_sdmx_generic_xml())
  called_with <- NULL
  testthat::local_mocked_bindings(
    read_xml = function(x) {
      called_with <<- x
      fake_doc
    },
    .package = "xml2"
  )

  out <- oecd_get_dataset(
    dataset  = "OECD.SDD.TPS,DSD_PDB@DF_PDB,",
    filter   = "FIN.A.GVAHRS",
    base_url = "https://example.test/rest"
  )

  expect_identical(
    called_with,
    "https://example.test/rest/data/OECD.SDD.TPS,DSD_PDB@DF_PDB,/FIN.A.GVAHRS"
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
})

# A trimmed but real SDMX-ML 2.1 Generic data response, as OECD's SDMX REST
# API returns it: two <Series>, one with two <Obs> and a series level
# attribute the other does not have, to exercise the case where series or
# observations don't all carry the same attributes.
a_sdmx_generic_xml <- function() {
  '<?xml version="1.0" encoding="utf-8"?>
  <message:GenericData
    xmlns:generic="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/data/generic"
    xmlns:message="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message">
    <message:DataSet>
      <generic:Series>
        <generic:SeriesKey>
          <generic:Value id="REF_AREA" value="FIN" />
          <generic:Value id="MEASURE" value="GVAHRS" />
        </generic:SeriesKey>
        <generic:Attributes>
          <generic:Value id="UNIT_MULT" value="0" />
        </generic:Attributes>
        <generic:Obs>
          <generic:ObsDimension id="TIME_PERIOD" value="2020" />
          <generic:ObsValue value="51.64985" />
          <generic:Attributes>
            <generic:Value id="OBS_STATUS" value="A" />
          </generic:Attributes>
        </generic:Obs>
        <generic:Obs>
          <generic:ObsDimension id="TIME_PERIOD" value="2021" />
          <generic:ObsValue value="51.94389" />
        </generic:Obs>
      </generic:Series>
      <generic:Series>
        <generic:SeriesKey>
          <generic:Value id="REF_AREA" value="USA" />
          <generic:Value id="MEASURE" value="GVAHRS" />
        </generic:SeriesKey>
        <generic:Obs>
          <generic:ObsDimension id="TIME_PERIOD" value="2021-Q1" />
          <generic:ObsValue value="90.38023" />
        </generic:Obs>
      </generic:Series>
    </message:DataSet>
  </message:GenericData>'
}

test_that(".sdmx_generic_to_df() flattens series and observations into one row each", {
  out <- fiprod:::.sdmx_generic_to_df(xml2::read_xml(a_sdmx_generic_xml()))

  expect_equal(nrow(out), 3)
  expect_setequal(names(out),
                  c("REF_AREA", "MEASURE", "UNIT_MULT", "obsTime", "obsValue", "OBS_STATUS"))
  expect_identical(out$REF_AREA, c("FIN", "FIN", "USA"))
  expect_identical(out$obsTime, c("2020", "2021", "2021-Q1"))
  expect_equal(out$obsValue, c(51.64985, 51.94389, 90.38023))
  expect_true(is.numeric(out$obsValue))
})

test_that(".sdmx_generic_to_df() fills NA where a series or obs lacks an attribute", {
  out <- fiprod:::.sdmx_generic_to_df(xml2::read_xml(a_sdmx_generic_xml()))

  # UNIT_MULT only exists on the FIN series; OBS_STATUS only on its first obs
  expect_identical(out$UNIT_MULT, c("0", "0", NA_character_))
  expect_identical(out$OBS_STATUS, c("A", NA_character_, NA_character_))
})

test_that(".sdmx_generic_to_df() errors clearly when there is no <Series>", {
  empty <- '<message:GenericData
    xmlns:generic="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/data/generic"
    xmlns:message="http://www.sdmx.org/resources/sdmxml/schemas/v2_1/message">
    <message:DataSet></message:DataSet>
  </message:GenericData>'
  expect_error(
    fiprod:::.sdmx_generic_to_df(xml2::read_xml(empty)),
    "No <Series>", fixed = TRUE
  )
})

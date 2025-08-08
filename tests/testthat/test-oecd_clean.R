test_that("oecd_clean_data works with OECD PDB annual feed", {
  # Skip on CRAN/CI or offline, and if rsdmx isn't installed
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("rsdmx")

  # Fetch OECD data (annual, countries FIN & USA, measure GVAHRS, etc.)
  sdmx <- rsdmx::readSDMX(
    providerId = "OECD",
    resource   = "data",
    flowRef    = "DSD_PDB@DF_PDB",
    key        = list(c("FIN", "USA"), "A", "GVAHRS", c("_T", "C"),
                      "XDC_H", "LR", "N", NULL, NULL),
    start      = 2020,
    end        = 2021
  )

  # Clean with our function: keep and rename REF_AREA -> geo, ACTIVITY -> nace
  out <- oecd_clean_data(
    sdmx_obj = sdmx,
    vars = c(geo = "REF_AREA", nace = "ACTIVITY"),
    freq = "A"
  )

  # Expect structure: only the selected vars + values + time
  expect_true(is.data.frame(out))
  expect_gt(nrow(out), 0)
  expect_identical(sort(names(out)), sort(c("time", "geo", "nace", "values")))

  # Types: factors for dims, numeric for values
  expect_true(is.factor(out$geo))
  expect_true(is.factor(out$nace))
  expect_true(is.numeric(out$values))

  # Geo should be FIN and/or USA (depending on returned rows)
  expect_true(all(unique(as.character(out$geo)) %in% c("FIN", "USA")))
})

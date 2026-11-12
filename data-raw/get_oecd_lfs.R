## code to get working age population data from OECD LFS database

library(tidyverse)
library(countrycode)



wap_dataset <- "OECD.SDD.TPS,DSD_LFS@DF_IALFS_WAP_Q,1.0"

wap_key <- oecd_make_filter(
  list(
    geos_oecd,  # REF_AREA
    "WAP",      # MEASURE
    NULL,       # ADJUSTMENT
    NULL,       # TRANSFORMATION
    "Y",        # Adjustment
    "_T",       # SEX
    "Y15T64",   # AGE
    NULL,       # UNIT_MEASURE
    "A"         # FREQ
  )
)

dat_oecd_wap_0 <- oecd_get_dataset(
  dataset = wap_dataset,
  filter  = wap_key
)

dat_oecd_wap <-
  dat_oecd_wap_0 |>
  oecd_clean_data(
    drop_vars = c("UNIT_MULT", "OBS_STATUS", "SEX"),
    vars = c(
      geo        = "REF_AREA",
      "measure"  = "MEASURE",
      "activity" = "ACTIVITY",  # "_Z"
      "age"      = "AGE"
    )
  ) |>
  mutate(geo = as_factor(countrycode(geo, "iso3c", "eurostat", nomatch = NULL)))

save_dat(dat_oecd_wap, overwrite = TRUE)

## code to prepare competetiveness datasets

library(tidyverse)
library(countrycode)
library(OECD)
devtools::load_all()

geos_main <- c("FI", "SE", "NO", "DK", "BE", "NL", "AT", "PT", "DE", "IT", "FR", "ES", "US", "JP")
geos_oecd <- c("EA20", countrycode(geos_main, "eurostat", "iso3c"))

pdb_dataset <- "OECD.SDD.TPS,DSD_PDB@DF_PDB,"

## Main data

pdb_ulc_key <- oecd_make_filter(
  list(geos_oecd, "A", c("GDPEMP", "GDPHRS", "LCEMP", "LCHRS", "LCTOT", "LCTOTE", "ULCE", "ULCH"),
       c("_T"),      # Economic activity Total
       NULL,         # Unit of measure
       c("V", "LR"), # Price base, current price and volume rebased.
       "N",          # Transformation None
       NULL,         # Asset type
       "_Z"))        # Conversion type, exclude PPP

dat_oecd_pdb_ulc_0 <- get_dataset(
  dataset = pdb_dataset, filter = pdb_ulc_key)

dat_oecd_pdb_ulc <-
  dat_oecd_pdb_ulc_0 |>
  oecd_clean_data(drop_vars = c("UNIT_MULT", "OBS_STATUS"),
                  vars = c(geo = "REF_AREA",
                           "measure"        = "MEASURE",
                           "unit_measure"   = "UNIT_MEASURE",
                           "price_base"     = "PRICE_BASE")) |>
  mutate(geo = as_factor(countrycode(geo, "iso3c", "eurostat", nomatch = NULL))) |>
  mutate(time = year(time)) |>
  unite("var_id", measure, unit_measure, price_base, sep = "-", remove = FALSE) |>
  mutate(var_id = as_factor(var_id)) |>
  group_by(across(where(is.factor))) |>
  filter(time > 1994) |>
  mutate(index15 = rebase_index(values, time, 2015)) |>
  ungroup() |>
  group_by(across(where(is.factor) & !all_of("geo")), time) |>
  mutate(rel = weight_index2(index15, geo, time, geos = geos_main, weight_df = weights_ecfin37)) |>
  ungroup()




save_dat(dat_oecd_pdb_ulc, overwrite = TRUE)


## Quarterly data


ulcq_dataset <- "OECD.SDD.TPS,DSD_PDB@DF_PDB_ULC_Q,1.0"

ulcq_key <- oecd_make_filter(
  list(geos_oecd, "Q", c("LCEMP", "GDPEMP", "ULCE"),
       NULL,
       "IX",         # Unit of measure
       c("V", "Q"),  # current and constant.
       "_Z",          # Transformation None
       "S",          # Seasonal adjusted
       NULL))

dat_oecd_ulcq_0 <- get_dataset(
  dataset = ulcq_dataset, filter = ulcq_key)


dat_oecd_ulcq <-
  dat_oecd_ulcq_0 |>
  oecd_clean_data(freq = "Q",
                  drop_vars = c("UNIT_MULT", "OBS_STATUS"),
                  vars = c(geo = "REF_AREA",
                           "measure"        = "MEASURE",
                           "unit_measure"   = "UNIT_MEASURE",
                           "price_base"     = "PRICE_BASE")) |>
  mutate(geo = as_factor(countrycode(geo, "iso3c", "eurostat", nomatch = NULL))) |>
  mutate(year = year(time)) |>
  unite("var_id", measure, unit_measure, price_base, sep = "-", remove = FALSE) |>
  mutate(var_id = as_factor(var_id)) |>
  filter(year > 1994) |>
  group_by(across(where(is.factor))) |>
  mutate(index15 = rebase_index(values, time, 2015)) |>
  ungroup() |>
  group_by(across(where(is.factor) & !all_of("geo")), time) |>
  mutate(rel = weight_index2(index15, geo, year, geos = geos_main, weight_df = weights_ecfin37)) |>
  ungroup()

save_dat(dat_oecd_ulcq, overwrite = TRUE)

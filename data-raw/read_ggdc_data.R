# The GGDC Productivity Level Database
# 2023 edition
# https://www.rug.nl/ggdc/productivity/pld/

library(tidyverse)

geos_ggdc <- c(c("BE", "DE", "EE", "IE", "EL", "ES", "FR", "IT", "CY", "LV",
                 "LT", "LU", "MT", "NL", "AT", "PT", "SI", "SK", "FI", "HR"),
               "SE", "DK", "NO", "UK", "US", "JP", "CA", "AU", "NZ")


ggdc_file <- here::here("data-raw", "pld2023_dataset.xlsx")

dat_ggdc_23<- readxl::read_xlsx(ggdc_file, sheet = "Data") |>
  pxwebtools::parse_dates(date_format = c(year = "Y")) |>
  statfitools::clean_names(to_lower = TRUE) |>
  mutate(geo = countrycode::countrycode(countrycode, "iso3c",
                                        destination = "eurostat",
                                        custom_match = c(EU27 = "EU27", EURO17 = "EA12")),
         geo_nimi = countrycode::countrycode(countrycode, "iso3c", destination = "cldr.short.fi")) |>

  mutate(across(where(is.character), as_factor)) |>
  select(-i_ppp) |>
  pivot_longer(cols = where(is.numeric),
               names_to = "vars", values_to = "values",
               names_transform = as_factor) |>
  filter(geo %in% geos_ggdc) |>
  droplevels()

save_dat(dat_ggdc_23, overwrite = TRUE)


## PPPs for va in OECD activities (industries)

# OECD activity mapping to GGDC sectors

key_oecd_ggdc <-
  list("_T" = c("agr", "bus", "con", "dwe", "fin", "man", "min", "oth", "pu", "pub", "tra", "trd"),
  "I" = "trd",
  "L" = "dwe",
  "C" = "man",
  "A" = "agr",
  "RTU" = "oth",
  "J" = "bus",
  "BTE" = c("min", "man", "pu"),
  "H" = "tra",
  "B_D_E" = c("min", "pu"),
  "K" = "fin",
  "S" = "oth",
  "M_N" = "bus",
  "BTNXL" = c("min", "man", "pu", "con", "trd", "tra", "bus", "fin"),
  "E" = "pu",
  "GTNXL" = c("trd", "tra", "bus", "fin"),
  "F" = "con",
  "GTI" = "trd",
  "OTQ" = "pub",
  "N" = "bus",
  "P" = "pub",
  "M" = "bus",
  "B" = "min",
  "O" = "pub",
  "Q" = "pub",
  "D" = "pu",
  "R" = "oth",
  "T" = "oth",
  "G" = "trd"
)

ind_ppps <- function(x, w, sector, key) {
  purrr::imap_dfr(key, function(grp, nm) {
    idx <- sector %in% grp
    tibble(
      activity = factor(nm, levels = names(key)),
      ppp_va   = if (any(idx)) stats::weighted.mean(x[idx], w[idx]) else NA_real_
    )
  })
}

dat_ppp_va_ggdc_oecd <-
  dat_ggdc_23 |>
  select(time, geo, sector, vars, values) |>
  filter_recode(vars = c("ppp_va", "va")) |>
  tidyr::pivot_wider(names_from = vars, values_from = values) %>%  # prefer pivot_wider over spread()
  # Calculate EA PPPs as va weighted mean.
  bind_rows(
    .,
    tibble(summarise(filter(., geo %in% geo_ea),
                     ppp_va = weighted.mean(ppp_va, va),
                     va = sum(va),
                     .by = c(time, sector)), geo = as_factor("EA20"))) |>
  # map ggdc PPPs to oecd industries and calculate industry group PPPs as weighted means.
  dplyr::group_by(time, geo) |>
  dplyr::reframe(ind_ppps(ppp_va, va, sector, key = key_oecd_ggdc))

save_dat(dat_ppp_va_ggdc_oecd, overwrite = TRUE)



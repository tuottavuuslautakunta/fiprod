# Raportin 2026 luvun 4.2 kuviodata -------------------------------------------
#
# Kuten lukujen 3, 5 ja 6 kuviot, luvun 4.2 sarjat tulevat valmiina laskettuina
# Excel-tiedostosta data-raw/rap_2026/Luku4_2.xlsx, jossa on yksi välilehti
# kuviota kohden ("Kuvio 14" - "Kuvio 16"). Kuvioiden 14 ja 15 mallikuviot ovat
# välilehdillä; kuviolla 16 ei ole mallikuviota, vaan sen otsikot, selitteet ja
# lähteet ovat välilehden sarakkeessa L tekstinä.
#
# Tulos on samanmuotoinen kuin luvuissa 3 ja 6 (data-raw/read_luku3.R):
#
#   figure  kuvio
#   panel   ei käytössä, NA
#   series  sarja selitteen mukaan
#   key     ei käytössä, NA
#   time    vuosi
#   values  arvo

library(tidyverse)

devtools::load_all()

luku4_2_file <- here::here("data-raw", "rap_2026", "Luku4_2.xlsx")

# Kuvion 14 vuosista kaksi viimeistä ovat ennakkotietoja ("2024*"), joten
# tähti poistetaan ennen vuoden lukemista
year_date <- function(x) {
  as.Date(paste0(as.integer(str_remove(x, "\\*")), "-01-01"))
}

longer_luku4_2 <- function(df, figure) {
  df |>
    pivot_longer(!time, names_to = "series", values_to = "values") |>
    mutate(figure = figure, panel = NA_character_, key = NA_character_,
           time = year_date(time)) |>
    select(figure, panel, series, key, time, values)
}


## Kuvio 14: työlliset sektoreittain ------------------------------------------
#
# Välilehti on Tilastokeskuksen kansantalouden tilinpidon poiminta, jossa
# vuodet ovat sarakkeilla ja sektorit riveillä. Mallikuviossa ovat sektoreista
# vain julkisyhteisöt ja yksityinen sektori, tässä järjestyksessä, eli
# kotitaloudet ja kotitalouksia palvelevat voittoa tavoittelemattomat yhteisöt
# jäävät pois.

luku4_2_sektorit <- c("Julkisyhteisöt", "Yksityinen sektori")

kuvio14 <-
  readxl::read_xlsx(luku4_2_file, sheet = "Kuvio 14", range = "C3:BB7") |>
  rename(series = 1) |>
  filter(series %in% luku4_2_sektorit) |>
  mutate(series = factor(series, levels = luku4_2_sektorit)) |>
  arrange(series) |>
  pivot_longer(!series, names_to = "time", values_to = "values") |>
  mutate(figure = "kuvio14", panel = NA_character_, key = NA_character_,
         time = year_date(time)) |>
  select(figure, panel, series, key, time, values)


## Kuvio 15: alkavat eläkkeet -------------------------------------------------

kuvio15 <-
  readxl::read_xlsx(luku4_2_file, sheet = "Kuvio 15", range = "A1:D21") |>
  rename(time = "Vuosi") |>
  longer_luku4_2("kuvio15")


## Kuvio 16: inhimillinen pääomakanta -----------------------------------------
#
# Välilehdellä on kolmen skenaarion pääomakanta ja lisäksi perusskenaarion
# osat koulutusasteittain (sarakkeet D:H). Kuvion tekstien mukaan kuviossa ovat
# vain skenaariot, eli sarakkeet B, C ja I. Selitteet ovat välilehden
# sarakkeessa L olevia kuvion tekstejä.

luku4_2_skenaariot <- c(
  "Muuttumattoman politiikan skenaario",
  "Perusskenaario: Vuoteen 2035 mennessä 50 %:lla 25-34-vuotiaista korkea-asteen tutkinto",
  "Optimistinen skenaario: Vuoteen 2040 mennessä 70 %:lla 25-34-vuotiaista korkea-asteen tutkinto"
)

kuvio16 <-
  readxl::read_xlsx(luku4_2_file, sheet = "Kuvio 16", range = "A3:I78",
                    col_names = c("time", luku4_2_skenaariot[1:2],
                                  "lukio", "amm", "amk", "ylempi", "tutkija",
                                  luku4_2_skenaariot[3]),
                    col_types = rep("numeric", 9)) |>
  select(time, all_of(luku4_2_skenaariot)) |>
  longer_luku4_2("kuvio16")


## Yhteen taulukkoon ----------------------------------------------------------

dat_luku4_2_2026 <-
  bind_rows(kuvio14, kuvio15, kuvio16) |>
  mutate(across(c(figure, panel, series, key), as_factor))

save_dat(dat_luku4_2_2026, overwrite = TRUE)

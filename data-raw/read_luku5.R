# Raportin 2026 luvun 5 kuviodata ---------------------------------------------
#
# Luvun 5 sarjat eivät tule tilastorajapinnoista vaan valmiina laskettuina
# Excel-tiedostosta data-raw/rap_2026/Luku5.xlsx. Siinä on yksi välilehti
# kuviota kohden ("Kuvio 16" - "Kuvio 21") ja jokaisella välilehdellä myös
# mallikuvio. Välilehdet luetaan tässä yhdeksi pitkäksi taulukoksi
# (dat_luku5_2026), jota vignettes/board_2026.qmd käyttää.
#
# Sarjojen nimet ovat mallikuvioiden selitteitä. Ne ovat yleensä samat kuin
# välilehden sarakeotsikot, mutta kuviossa 21 selitteet kertovat myös kumpaa
# asteikkoa sarja käyttää, joten ne poikkeavat otsikoista.
#
# Solualueet on kirjoitettu auki, koska välilehtien rakenne vaihtelee:
# otsikkorivi on eri kohdassa eri välilehdillä, kuvio 18 on käännetty (vuodet
# sarakkeilla) ja kuviossa 20 on kaksi paneelia rinnakkain. Alueet ovat
# pelkkää dataa, eli otsikkorivi jää niiden ulkopuolelle ja sarjojen nimet
# annetaan `col_names`-argumentissa.

library(tidyverse)

devtools::load_all()

luku5_file <- here::here("data-raw", "rap_2026", "Luku5.xlsx")

# Vuosi ja neljännes aikasarjan alkupäiväksi, kuten paketin muissa sarjoissa
year_date <- function(x) as.Date(paste0(as.integer(x), "-01-01"))

# "2000Q1" -> 2000-01-01
quarter_date <- function(x) {
  as.Date(sprintf("%s-%02d-01", substr(x, 1, 4),
                  3 * (as.integer(substr(x, 6, 6)) - 1) + 1))
}

# Yksi välilehti leveänä: ensimmäinen sarake on aika, loput sarjoja
read_luku5 <- function(sheet, range, series, time_type = "numeric") {
  readxl::read_xlsx(luku5_file, sheet = sheet, range = range,
                    col_names = c("time", series),
                    col_types = c(time_type, rep("numeric", length(series))))
}

# Pitkään muotoon. Sarjat pysyvät sarakejärjestyksessä, joka on myös niiden
# järjestys kuvion selitteessä.
longer_luku5 <- function(df, figure, panel = NA_character_) {
  df |>
    pivot_longer(!time, names_to = "series", values_to = "values") |>
    mutate(figure = figure, panel = panel) |>
    select(figure, panel, series, time, values)
}


## Kuvio 16: työn tuottavuus ja reaalipalkat, neljännesvuosittain -------------

kuvio16 <-
  read_luku5("Kuvio 16", "B4:D107",
             c("Työn tuottavuus", "Reaalipalkat (arvonlisäyksen hinnoin)"),
             time_type = "text") |>
  mutate(time = quarter_date(time)) |>
  longer_luku5("kuvio16")


## Kuvio 17: palkkatyön kannattavuussuhde ja työtunnit ------------------------

kuvio17 <-
  read_luku5("Kuvio 17", "C5:E48",
             c("Palkkatyön kannattavuussuhde",
               "Palkansaajien tehdyt työtunnit / 20-69 vuotiaat")) |>
  mutate(time = year_date(time)) |>
  longer_luku5("kuvio17")


## Kuvio 18: kokoluokittain ---------------------------------------------------
#
# Välilehti on käännetty: vuodet ovat sarakkeilla ja sarjat riveillä, joten
# otsikkorivi luetaan tässä sarakenimiksi ja ensimmäinen sarake on sarjan nimi.

kuvio18 <-
  readxl::read_xlsx(luku5_file, sheet = "Kuvio 18", range = "C5:X8") |>
  rename(series = "Vuosi") |>
  pivot_longer(!series, names_to = "time", values_to = "values") |>
  mutate(time = year_date(time), figure = "kuvio18", panel = NA_character_) |>
  select(figure, panel, series, time, values)


## Kuvio 19: tuottavuuden taso yrityssektorilla ja yrityksissä ----------------

kuvio19 <-
  read_luku5("Kuvio 19", "B4:D33",
             c("Yrityssektorin taso", "Yritysten taso")) |>
  mutate(time = year_date(time)) |>
  longer_luku5("kuvio19")


## Kuvio 20: teknologiatason mukaan, kaksi paneelia ---------------------------
#
# Vuodet ovat sarakkeessa C, ensimmäisen paneelin sarjat sarakkeissa D:G ja
# toisen samannimiset sarjat sarakkeissa I:L. Paneelien otsikot ovat
# mallikuvioiden otsikot.

luku5_tech <- c("Matala teknologia", "Keskimatala teknologia",
                "Keskikorkea teknologia", "Korkean teknologia")

kuvio20_time <-
  readxl::read_xlsx(luku5_file, sheet = "Kuvio 20", range = "C4:C33",
                    col_names = "time", col_types = "numeric") |>
  mutate(time = year_date(time))

read_kuvio20 <- function(range, panel) {
  bind_cols(
    kuvio20_time,
    readxl::read_xlsx(luku5_file, sheet = "Kuvio 20", range = range,
                      col_names = luku5_tech,
                      col_types = rep("numeric", length(luku5_tech)))
  ) |>
    longer_luku5("kuvio20", panel = panel)
}

kuvio20 <- bind_rows(
  read_kuvio20("D4:G33", "Yritysten tasolla"),
  read_kuvio20("I4:L33", "\"Luova tuho\"")
)


## Kuvio 21: divergenssikomponentti ja hajonnan muutos ------------------------
#
# Välilehden sarakejärjestys on vasen, oikea, vasen, oikea. Sarjat järjestetään
# uudelleen niin, että saman asteikon sarjat ovat selitteessä vierekkäin.

luku5_k21 <- c("Divergenssi-komponentti (vasen asteikko)",
               "Hajonnan muutos (oikea asteikko)",
               "Divergenssi-komponentti, HP-tasoitettu (vasen ast.)",
               "Hajonnan muutos, HP-tasoitettu (oikea ast.)")

kuvio21 <-
  read_luku5("Kuvio 21", "B4:F32", luku5_k21) |>
  mutate(time = year_date(time)) |>
  select(time, all_of(luku5_k21[c(1, 3, 2, 4)])) |>
  longer_luku5("kuvio21")


## Yhteen taulukkoon ----------------------------------------------------------
#
# Tekijätasojen järjestys on sama kuin välilehdillä, eli myös kuvioiden
# selitteiden järjestys.

dat_luku5_2026 <-
  bind_rows(kuvio16, kuvio17, kuvio18, kuvio19, kuvio20, kuvio21) |>
  mutate(across(c(figure, panel, series), as_factor))

save_dat(dat_luku5_2026, overwrite = TRUE)

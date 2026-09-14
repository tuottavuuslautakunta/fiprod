# Raportin 2026 luvun 6 kuviodata ---------------------------------------------
#
# Kuten lukujen 3 ja 5 kuviot, luvun 6 sarjat tulevat valmiina laskettuina
# Excel-tiedostosta data-raw/rap_2026/Luku6.xlsx. Työkirja on koottu luvun 6
# käsikirjoituksen mukana tulleista yksittäisistä datapistetiedostoista (yksi
# kuviota kohden) niin, että jokainen on omalla välilehdellään "Kuvio 23" -
# "Kuvio 31"; Sisältö-välilehdellä on kuvioiden otsikot ja lähdetiedostot.
# Mallikuviot ovat käsikirjoituksessa, eivät työkirjassa.
#
# Välilehdillä on tavallinen otsikkorivi, joten sarakkeiden nimet luetaan
# sellaisenaan ja nimetään tässä uudelleen kuvioiden selitteiden mukaan.
# Tulos on samanmuotoinen kuin luvussa 3 (data-raw/read_luku3.R):
#
#   figure  kuvio
#   panel   kuvioiden 24 ja 31 paneeli, muilla NA
#   series  sarja selitteen mukaan, NA kun kuviossa on vain yksi nimetön sarja
#   key     kuvion 23 maa, muilla NA
#   time    vuosi
#   values  arvo

library(tidyverse)

devtools::load_all()

luku6_file <- here::here("data-raw", "rap_2026", "Luku6.xlsx")

year_date <- function(x) as.Date(paste0(as.integer(x), "-01-01"))

# Leveä välilehti pitkäksi: ensimmäinen sarake on vuosi, loput sarjoja, jotka on
# jo nimetty selitteen mukaan.
longer_luku6 <- function(df, figure, panel = NA_character_) {
  df |>
    pivot_longer(!time, names_to = "series", values_to = "values") |>
    mutate(figure = figure, panel = panel, key = NA_character_,
           time = year_date(time)) |>
    select(figure, panel, series, key, time, values)
}


## Kuvio 23: toimialojen keskittyneisyys EU-maissa ----------------------------
#
# Välilehdellä on sarake maata kohden ja BKT-painotettu Eurooppa-keskiarvo.
# Mallikuviossa Suomi ja Eurooppa ovat omina sarjoinaan ja muut maat yhtenä
# harmaana joukkona, joten maa jää `key`-sarakkeeseen, joka erottaa viivat
# toisistaan.

kuvio23 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 23") |>
  rename(time = "Year") |>
  pivot_longer(!time, names_to = "key", values_to = "values") |>
  mutate(
    figure = "kuvio23",
    panel = NA_character_,
    series = case_when(
      key == "FIN" ~ "Suomi",
      key == "Europe (GDP-weighted average)" ~ "Eurooppa (BKT-painotettu)",
      .default = "Muut maat"
    ),
    time = year_date(time)
  ) |>
  select(figure, panel, series, key, time, values)


## Kuvio 24: keskittymisindeksit ----------------------------------------------
#
# Välilehti on jo pitkässä muodossa. Paneelit ovat siinä vasen ja oikea;
# mallikuviossa ne ovat HHI ja CR4.

kuvio24 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 24") |>
  mutate(
    figure = "kuvio24",
    panel = case_match(paneeli, "Vasen" ~ "HHI", "Oikea" ~ "CR4"),
    series = sarja,
    key = NA_character_,
    time = year_date(vuosi),
    values = arvo
  ) |>
  select(figure, panel, series, key, time, values)


## Kuvio 25: markkinoille tulo ja poistuminen ---------------------------------

kuvio25 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 25") |>
  mutate(figure = "kuvio25", panel = NA_character_, series = sarja,
         key = NA_character_, time = year_date(vuosi), values = arvo) |>
  select(figure, panel, series, key, time, values)


## Kuvio 26: suurimpien yritysten pysyvyys ------------------------------------
#
# Yksi sarja, jolla ei ole mallikuviossa selitettä.

kuvio26 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 26") |>
  mutate(figure = "kuvio26", panel = NA_character_, series = NA_character_,
         key = NA_character_, time = year_date(vuosi), values = arvo) |>
  select(figure, panel, series, key, time, values)


## Kuviot 27 ja 29: voittomarginaalit ja kokonaistuottavuus -------------------
#
# Samanlaiset: painotettu keskiarvo ja ala- ja yläneljänneksen rajat indeksinä.
# Kuvion 29 välilehdellä p75-sarakkeen nimi on tfp_p75_t10.

luku6_jakauma <- c("time", "Painotettu keskiarvo", "p25", "p75")

kuvio27 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 27") |>
  set_names(luku6_jakauma) |>
  longer_luku6("kuvio27")

kuvio29 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 29") |>
  set_names(luku6_jakauma) |>
  longer_luku6("kuvio29")


## Kuviot 28 ja 30: Suomi, Yhdysvallat ja Eurooppa ----------------------------

luku6_maat <- c("time", "Suomi", "Yhdysvallat", "Eurooppa")

kuvio28 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 28") |>
  set_names(luku6_maat) |>
  longer_luku6("kuvio28")

kuvio30 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 30") |>
  set_names(luku6_maat) |>
  longer_luku6("kuvio30")


## Kuvio 31: investoinnit suhteessa liikevaihtoon -----------------------------
#
# Kaksi paneelia, kummassakin yksi nimetön sarja. Paneelien otsikot ovat
# mallikuvion otsikot.

kuvio31 <-
  readxl::read_xlsx(luku6_file, sheet = "Kuvio 31") |>
  set_names(c("time", "Aineelliset investoinnit", "Aineettomat investoinnit")) |>
  pivot_longer(!time, names_to = "panel", values_to = "values") |>
  mutate(figure = "kuvio31", series = NA_character_, key = NA_character_,
         time = year_date(time)) |>
  select(figure, panel, series, key, time, values)


## Yhteen taulukkoon ----------------------------------------------------------

dat_luku6_2026 <-
  bind_rows(kuvio23, kuvio24, kuvio25, kuvio26, kuvio27, kuvio28, kuvio29,
            kuvio30, kuvio31) |>
  mutate(across(c(figure, panel, series, key), as_factor))

save_dat(dat_luku6_2026, overwrite = TRUE)

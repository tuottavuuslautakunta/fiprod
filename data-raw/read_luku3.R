# Raportin 2026 luvun 3 kuviodata ---------------------------------------------
#
# Kuten luvun 5 kuviot (data-raw/read_luku5.R), luvun 3 sarjat tulevat valmiina
# laskettuina Excel-tiedostosta data-raw/rap_2026/Luku3.xlsx. Siinä on yksi
# välilehti kuviota kohden ("kuvio1" - "kuvio4"), välilehdillä mallikuviot ja
# osalla myös kuvion otsikko ja huomautus.
#
# Kuviot ovat rakenteeltaan erilaisia: kuvio 1 on aikasarja, kuvio 2 maittainen
# hajontakuvio viideltä toimialalta, kuvio 3 toimialoittainen pylväskuvio ja
# kuvio 4 jaksoittainen hajotelma. Ne luetaan yhdeksi pitkäksi taulukoksi
# (dat_luku3_2026), jossa
#
#   figure  kuvio
#   panel   kuvion 2 toimiala, muilla NA
#   series  sarja tai muuttuja, kuviossa 3 NA (vain yksi nimetön sarja)
#   key     x-akselin luokka: kuviossa 2 maa, kuviossa 3 toimiala ja
#           kuviossa 4 jakso; kuviossa 1 NA, koska siinä x on aika
#   time    kuvion 1 vuosi, muilla NA
#   values  arvo
#
# Solualueet on kirjoitettu auki ja ne ovat pelkkää dataa, eli otsikkorivi jää
# alueen ulkopuolelle ja sarjojen nimet annetaan `col_names`-argumentissa.

library(tidyverse)

devtools::load_all()

luku3_file <- here::here("data-raw", "rap_2026", "Luku3.xlsx")

year_date <- function(x) as.Date(paste0(as.integer(x), "-01-01"))


## Kuvio 1: työn tuottavuus yksityisellä palvelusektorilla --------------------
#
# Sarakkeessa B tuottavuus euroina ja sarakkeessa C samat tuhansina euroina.
# Mallikuvio käyttää sarakkeen C lukuja, joten sarake B jätetään pois.

kuvio1 <-
  readxl::read_xlsx(luku3_file, sheet = "kuvio1", range = "A2:C26",
                    col_names = c("time", "euroa", "values"),
                    col_types = rep("numeric", 3)) |>
  mutate(figure = "kuvio1",
         panel = NA_character_,
         series = "Työn tuottavuus",
         key = NA_character_,
         time = year_date(time)) |>
  select(figure, panel, series, key, time, values)


## Kuvio 2: tuottavuus ja pääomaintensiteetti toimialoittain ------------------
#
# Viisi toimialaa vierekkäisinä lohkoina, joissa jokaisessa maa, työn
# tuottavuus ja pääomakanta suhteessa työtunteihin. Muuttujien nimet ovat
# välilehden omat; akselien otsikot ovat mallikuvioissa. Paneelien otsikot ovat
# mallikuvioiden otsikot ja lohkojen järjestys niiden järjestys.

luku3_k2 <- c(
  "Kauppa"                                        = "A8:C34",
  "Kuljetus ja varastointi"                       = "G8:I34",
  "Informaatio ja viestintä"                      = "M8:O34",
  "Rahoitus- ja vakuutustoiminta"                 = "S8:U34",
  "Ammatillinen, tieteellinen ja tekninen toiminta" = "Y8:AA34"
)

read_kuvio2 <- function(range, panel) {
  readxl::read_xlsx(luku3_file, sheet = "kuvio2", range = range,
                    col_names = c("key", "labprod_eur", "caplab_total_eur"),
                    col_types = c("text", "numeric", "numeric")) |>
    pivot_longer(!key, names_to = "series", values_to = "values") |>
    # maat, joilta tieto puuttuu (LU kaikilla toimialoilla, IE informaatio- ja
    # viestintäalalla), ovat välilehdellä tyhjinä riveinä
    filter(!is.na(values)) |>
    mutate(figure = "kuvio2", panel = panel, time = as.Date(NA)) |>
    select(figure, panel, series, key, time, values)
}

kuvio2 <- imap(luku3_k2, \(range, panel) read_kuvio2(range, panel)) |>
  list_rbind()


## Kuvio 3: työvoimakustannusten suhde rajatuottoon ---------------------------
#
# Yksi nimetön sarja, x-akselilla toimiala. Toimialat jäävät välilehden
# järjestykseen, joka on myös mallikuvion järjestys.

kuvio3 <-
  readxl::read_xlsx(luku3_file, sheet = "kuvio3", range = "A4:B11",
                    col_names = c("key", "values"),
                    col_types = c("text", "numeric")) |>
  mutate(figure = "kuvio3",
         panel = NA_character_,
         series = NA_character_,
         time = as.Date(NA)) |>
  select(figure, panel, series, key, time, values)


## Kuvio 4: tuottavuuden kasvun lähteet ---------------------------------------
#
# Viisi hajotelman osaa (sarakkeet E:I) ja niiden summana koko sektorin kasvu
# (sarake B). Osat tulevat ensin, joten ne ovat selitteessä välilehden
# järjestyksessä ja kokonaiskasvu viimeisenä.

luku3_k4 <- c("Jatkavat yritykset, ei yritysjärjestelyjä",
              "Yritysostoja tehneet jatkavat yritykset",
              "Aito markkinoille tulo ja poistuminen",
              "Yritysjärjestelyihin liittyvä markkinoille tulo ja poistuminen",
              "Työvoiman uudelleenkohdentuminen")

kuvio4_osat <-
  readxl::read_xlsx(luku3_file, sheet = "kuvio4", range = "D7:I10",
                    col_names = c("key", luku3_k4),
                    col_types = c("text", rep("numeric", length(luku3_k4)))) |>
  pivot_longer(!key, names_to = "series", values_to = "values")

kuvio4_yht <-
  bind_cols(
    readxl::read_xlsx(luku3_file, sheet = "kuvio4", range = "D7:D10",
                      col_names = "key", col_types = "text"),
    readxl::read_xlsx(luku3_file, sheet = "kuvio4", range = "B7:B10",
                      col_names = "values", col_types = "numeric")
  ) |>
  mutate(series = "Työn tuottavuus yksityisissä palveluissa")

kuvio4 <-
  bind_rows(kuvio4_osat, kuvio4_yht) |>
  mutate(figure = "kuvio4", panel = NA_character_, time = as.Date(NA)) |>
  select(figure, panel, series, key, time, values)


## Kuvio 11: digitaalisen intensiteetin luokat -------------------------------
#
# Välilehdellä on kaksi taulukkoa: vasemmalla luokkien osuudet vuosittain ja
# toimialoittain, oikealla mallikuvion käyttämä taulukko, jossa on yksi sarake
# toimialaa kohden ja rivi luokkaa kohden. Mallikuvio käyttää oikeaa taulukkoa
# (J4:Q8), jonka toimialat ovat suomeksi ja jonka järjestys on myös kuvion
# selitteen järjestys.
#
# Osuudet ovat välilehdellä desimaalilukuina, ja ne muutetaan tässä
# prosenteiksi, kuten muissakin raportin kuvioissa.

kuvio11 <-
  readxl::read_xlsx(luku3_file, sheet = "Kuvio11", range = "J4:Q8") |>
  rename(key = 1) |>
  pivot_longer(!key, names_to = "series", values_to = "values") |>
  mutate(figure = "kuvio11",
         panel = NA_character_,
         time = as.Date(NA),
         values = 100 * values) |>
  select(figure, panel, series, key, time, values)


## Yhteen taulukkoon ----------------------------------------------------------

dat_luku3_2026 <-
  bind_rows(kuvio1, kuvio2, kuvio3, kuvio4, kuvio11) |>
  mutate(across(c(figure, panel, series, key), as_factor))

save_dat(dat_luku3_2026, overwrite = TRUE)

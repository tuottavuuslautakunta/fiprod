
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fiprod

<!-- badges: start -->

<!-- badges: end -->

The fiprod is a new packege for Finnish Productivity Board data
management and figure production. The old package is
[pRoductivity](https://github.com/tuottavuuslautakunta/pRoductivity).

## Data

Paketti sisältää valmiiksi esiprosessoituja aineistoja, jotka tuotetaan
`data-raw/`-hakemistossa sijaitsevilla skripteillä. Data kirjoitetaan
joko paketin `data/`-hakemistoon (`.rda`-objekteina, joita voi ladata
`data()`- tai `load()`-funktioilla) tai `inst/extdata/`-hakemistoon
Parquet-tiedostoiksi, joita luetaan apufunktiolla `load_dat()`.

### OECD:n tuottavuustilastot (`data-raw/get_oecd_pdb.R`)

Skripti hakee OECD Productivity Statistics -tietokannasta koko talouden
ja markkinasektorin aggregaatit sekä toimialakohtaiset aikasarjat
(mittarit `GVA`, `GDP`, `GDPPOP`, `GVAEMP`, `GVAHRS`). Haetut tiedot
muunnetaan `oecd_clean_data()`-funktiolla yhtenäiseen rakenteeseen ja
talletetaan Parquet- muotoon `inst/extdata/`-hakemistoon. Päätaulu
`dat_oecd_pdb_main.parquet` sisältää 21 609 riviä ajanjaksolta 1950–2024
viidelletoista alueelle (EA20 sekä AT, BE, DE, DK, ES, FI, FR, IT, JP,
NL, NO, PT, SE ja US) ja kahdelle aktiviteettiluokalle: `_T` (koko
talous) ja `BTNXL` (markkinasektori ilman asuntopalveluja). Sarakkeet
ovat:

- `time`: vuosipäivä (1.1.) muodossa `Date`.
- `geo`: Eurostatin aluekoodi (faktori).
- `measure`: tarkasteltava suure (BKT, arvonlisäys, tuottavuus per
  työntekijä tai per tehty tunti).
- `activity`: OECD:n toimialaluokitus (kokonaisuus tai markkinasektori).
- `unit_measure`: mittayksikkö, esim. `XDC` (kansallinen valuutta),
  `USD_PPP` (PPP-mukautettu USD), `XDC_PS`/`USD_PPP_PS` (per työntekijä)
  ja `XDC_H`/`USD_PPP_H` (per tehty tunti).
- `price_base`: hintaperusta, jossa `V` = käyvin hinnoin, `LR` =
  kiinteähintainen (perusvuosi 2020) ja `L` = ketjutettu
  volyymi-indeksi.
- `conversion_type`: valuuttamuunnos (`_Z` = markkinavaluuttakurssi,
  `PPP` = ostovoimakorjattu).
- `values`: mittarin arvo annettua yhdistelmää kohti.

Sama skripti tuottaa myös toimialoittaisen taulun
`dat_oecd_pdb_ind.parquet`, jossa `activity`-sarake kattaa OECD:n
aggregoidut toimialat (esim. `BTE`, `F`, `GTNXL`, `OTQ`, `RTU`).
Molemmat taulut ovat oletuslähteitä paketin visualisointeihin ja ne
voidaan lukea esimerkiksi komennolla
`dat_oecd_pdb_main <- load_dat("dat_oecd_pdb_main")`.

### Eurostatin valuuttakurssit (`data-raw/get_exch.R`)

Eurostatin `ert_bil_eur_a` -aineistosta haetaan euroon ankkuroidut
vuosikeski- kurssit valuutoille AUD, CAD, CHF, DKK, JPY, NOK, NZD, SEK
ja USD. Skripti lisää lisäksi jokaiselle euroalueen maalle (ks.
`geo_ea`) sarjan, jossa arvo on aina yksi. Valmis data `exh_eur_a`
tallennetaan R:n `.rda`-muodossa ja sisältää seuraavat sarakkeet: `time`
(vuosipäivä `Date`-muodossa), `currency` (ISO 4217), `geo`
(kaksikirjaiminen aluekoodi) ja `values` (kuinka monta yksikköä
valuuttaa vastaa yhtä euroa). Aineisto kattaa vuodet 1971–2025 ja toimii
`convert_currency()`-apufunktion lähdedatana.

Samassa prosessissa syntyy myös `geo_ea`, joka on 20 jäsenen euroalueen
(BE, DE, EE, IE, EL, ES, FR, IT, CY, LV, LT, LU, MT, NL, AT, PT, SI, SK,
FI, HR) koodiluettelo. Vektori on tallennettu `.rda`-objektina ja sitä
käytetään sekä valuuttakonversioissa että muissa euroaluetta rajaavissa
kyselyissä.

### GGDC Productivity Level Database 2023 (`data-raw/read_ggdc_data.R`)

Hakemistossa on alkuperäinen `pld2023_dataset.xlsx`, josta luetaan
vuosien 2005–2017 sektorikohtaiset tuottavuusmittarit. Skripti siivoaa
sarake- ja maakoodit, muodostaa suomenkieliset nimet (`geo_nimi`) sekä
muuntaa tiedot pitkään muotoon taulukoksi `dat_ggdc_23`, jossa on
sarakkeet `time`, `geo`, `geo_nimi`, `sector`, `vars` (esim. `va`,
`emp`, `ppp_va`, `xr`) ja `values`. Aineisto rajataan euroalueen maihin
sekä vertailumaihin (SE, DK, NO, UK, US, JP, CA, AU, NZ) ja tallennetaan
Parquet-tiedostona `inst/extdata/`-hakemistoon.

Lisäksi skripti rakentaa taulun `dat_ppp_va_ggdc_oecd`, jossa GGDC:n
sektorit kartoitetaan OECD:n toimialaryhmiin ja lasketaan PPP-kertoimet
arvonlisäys- painotettuina. Lähteenä käytetään Groningen Growth and
Development Centren `Productivity Level Database 2023` -tietokannan
(Feenstra, Inklaar & Timmer) muuttujia `ppp_va` (arvonlisäyksen
PPP-hintataso) ja `va` (arvonlisäyksen arvo). Ensin lasketaan euroalueen
(EA20) hintataso painottamalla maiden toimialakohtaiset PPP:t maiden
arvonlisäyksillä. Tämän jälkeen GGDC:n sektorit ryhmitellään
`key_oecd_ggdc`-avainlistan avulla OECD:n käyttämiin aggregaatteihin
(`_T`, `BTNXL`, `BTE`, `RTU`, jne.), ja kullekin aggregaatille
muodostetaan painotettu keskiarvo. Lopputulos on aikasarja
PPP-kertoimista (perusvuosi 2017) jokaiselle maalle, euroalueelle ja
OECD:n toimialaryhmälle, ja sitä hyödynnetään OECD:n ja GGDC:n
hintatasotietojen yhdistämisessä.

### Kokoavat aikasarjat (`data-raw/data_main.R`)

`data_main.R` kokoaa valmiit Parquet-aineistot analyysia varten. Skripti
määrittelee euroalueen maiden luettelon (`geo_ea`) ja mahdollistaa
raakadataskriptien uudelleensuorituksen (`get_oecd_pdb.R`, `get_exch.R`)
asettamalla `update = TRUE`. Pääasiallisesti se lataa aiemmin tuotetut
taulut `dat_oecd_pdb_main`, `dat_oecd_pdb_ind`, `dat_ggdc_23` ja
`dat_ppp_va_ggdc_oecd`, joiden pohjalta rakennetaan `dat_gva_ind`.

`dat_gva_ind` syntyy, kun OECD:n toimialataulusta (`dat_oecd_pdb_ind`)
poimitaan käyvän hintatason (`V`) ja kiinteähintaisen volyymin
(ketjutettu perusvuosi 2020, `LR`) sarjat. Ne nimetään uudelleen (`cp`,
`fp_2020_lc`) ja yhdistetään GGDC:n tuottamiin PPP-kertoimiin vuoden
2017 tasolla. Tällöin muodostetaan kaksi lisämuuttujaa: `fp_2020_ppp17`,
joka muuntaa kiinteähintaisen paikallisvaluutan PPP-tasoiseen USD:hen
(`fp_2020_lc / ppp_va`), ja `fp_2020_xr17`, joka muuntaa saman volyymin
markkinakurssiin perustuvaksi USD-arvoksi hyödyntäen
`convert_currency()`-funktiota ja Eurostatin valuuttadataa. Lopuksi
tiedot muutetaan pitkäksi muodoksi (`vars`, `values`) ja tallennetaan
`save_dat()`- funktiolla Parquet-muotoon, jolloin taulu on saatavilla
kutsulla `load_dat("dat_gva_ind")`.

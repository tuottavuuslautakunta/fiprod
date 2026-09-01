# Työtunnit: Eurostat ja OECD

Päivitetty: 2026-09-01

Yhdistetyssä aineistossa työn tuottavuus `GVAHRS` lasketaan EU- ja
ETA-maille Eurostatin luvuista, muille se tulee sellaisenaan OECD:ltä.
Osalle maista — selvimmin Ruotsille, Itävallalle ja Portugalille —
tuottavuuden **taso** poikkeaa tästä syystä huomattavasti vanhasta,
pelkkään OECD:hen perustuvasta aineistosta. Ero ei ole laskuvirhe vaan
seuraa siitä, että OECD ei käytä näiden maiden kansantalouden tilinpidon
työtunteja. Tämä muistio näyttää mistä ero syntyy, miten suuri se on ja
mitä siitä seuraa käytännössä.

Näytä koodi

``` r

if (interactive()) devtools::load_all(".") else library(fiprod)

library(tidyverse)
library(ggcustom)

set_gg(theme_fpb())

dat_gdp_main        <- load_dat("dat_gdp_main")
dat_oecd_pdb_main   <- load_dat("dat_oecd_pdb_main")
dat_eurostat_na_ind <- load_dat("dat_eurostat_na_ind")

act <- "_T"                                    # koko talous

chr <- function(d) mutate(d, across(where(is.factor), as.character))

# normitus mediaanilla: yksikkökerroin pois, jäljelle jää maakohtainen ero
nrm <- function(x) x / median(x, na.rm = TRUE)

# Maat, joiden työtunnit OECD korvaa omalla estimaatillaan (ks. lähteet alla)
geos_oecd_hours <- c("AT", "EE", "EL", "FI", "LV", "LT", "PL", "PT", "SE", "UK")
```

## Mistä ero syntyy

Lähteet ovat epäsymmetriset. Eurostat julkaisee arvonlisäyksen ja
työtunnit erikseen mutta ei niiden suhdetta; OECD julkaisee suhteen
mutta ei työtunteja.

|  | Eurostat-maat | OECD-maat |
|----|----|----|
| `GVA` | `nama_10_a10`, `B1G` | OECD, sellaisenaan |
| `HRS` | `nama_10_a10_e`, `EMP_DC` / `THS_HW` | ei julkaista, **laskettu**: `GVA / GVAHRS` |
| `GVAHRS` | **laskettu**: `GVA / HRS` | **suoraan tietokannasta** |

Suunta on siis päinvastainen: Eurostat-mailla tunnit ovat lähtötieto ja
suhde lasketaan, OECD-mailla suhde on lähtötieto ja tunnit lasketaan
takaisin.

Paketin tauluissa `HRS` on mukana molemmilla, mutta OECD-mailla se on
paketin itse laskema: `data-raw/get_oecd_pdb.R` jakaa `GVA / GVAHRS`
heti haun jälkeen ja tallentaa tuloksen `HRS`-mittariksi. Itse
tietokannassa työtunteja ei ole, vaikka ne haussa pyydetään.

Näytä koodi

``` r

bind_rows(
  chr(dat_oecd_pdb_main)   |> count(measure) |> mutate(taulu = "dat_oecd_pdb_main"),
  chr(dat_eurostat_na_ind) |> count(measure) |> mutate(taulu = "dat_eurostat_na_ind"),
  chr(dat_gdp_main)        |> count(measure) |> mutate(taulu = "dat_gdp_main")
) |>
  pivot_wider(names_from = taulu, values_from = n) |>
  arrange(measure) |>
  knitr::kable(caption = "Havaintojen määrä mittareittain")
```

| measure | dat_oecd_pdb_main | dat_eurostat_na_ind | dat_gdp_main |
|:--------|------------------:|--------------------:|-------------:|
| EMP     |              1310 |               30702 |         2664 |
| GDP     |              4354 |                  NA |         4326 |
| GDPPOP  |              6746 |                  NA |         3889 |
| GVA     |              5467 |               98532 |        10758 |
| GVAEMP  |              4965 |                  NA |         8126 |
| GVAHRS  |              4492 |                  NA |         7860 |
| HRS     |              1197 |               30702 |         2634 |
| HRSPOP  |               676 |                  NA |          944 |
| POP     |               871 |                  NA |          973 |
| WAP     |                NA |                  NA |          473 |

Havaintojen määrä mittareittain {.table .caption-top}

Yhdistetyssä `dat_gdp_main`-taulussa `HRS` on siis kaikilla mailla,
mutta se on eri tavalla tehty tieto: Eurostat-mailla tilinpidon
tuntisarja, OECD-mailla takaisinlaskettu `GVA / GVAHRS`.

Näin ollen `GVAHRS` on kahdessa aineistossa sama nimi kahdelle eri
asialle. Ne ovat yhtä suuret vain, jos OECD:n `GVAHRS` on itsekin
arvonlisäys jaettuna tilinpidon työtunneilla.

## OECD:n työtuntien estimointi

Ne eivät ole. OECD:n ensisijainen lähde työtunneille on kansantalouden
tilinpito, mutta osalle maista tilinpidon tunnit lasketaan suoralla
menetelmällä ilman korjauksia, jolloin niissä on systemaattinen
yläsuuntainen harha. Tammikuusta 2019 alkaen OECD on korvannut kymmenen
maan työtunnit omalla estimaatillaan, joka lasketaan EU:n
työvoimatutkimuksesta niin sanotulla yksinkertaistetulla
komponenttimenetelmällä: lähtökohtana ovat tavanomaiset viikkotyötunnit,
joihin tehdään korjaukset ylitöistä, joustavista työajoista ja
sivutöistä sekä tekemättömistä viikoista (lomat, sairaus- ja
perhevapaat).

Nämä kymmenen maata ovat **Itävalta, Viro, Suomi, Kreikka, Latvia,
Liettua, Puola, Portugali, Ruotsi ja Iso-Britannia**. Yhdistetyssä
aineistossa asia koskee niitä, jotka otetaan Eurostatista —
Iso-Britannian tiedot tulevat OECD:ltä, joten sillä eroa ei synny.

OECD:n oman arvion mukaan korjauksen vaikutus tuottavuuden
**kasvuvauhteihin on vähäinen**, mutta **tasovertailuihin merkittävä**:
suhteelliset tuottavuuserot kaventuvat useilla mailla luokkaa 10
prosenttiyksikköä, ja esimerkiksi Itävalta nousee vertailussa Ranskan,
Alankomaiden, Sveitsin ja Saksan ohi.

Lähteet:

- [OECD Productivity Statistics – Methodological
  notes](https://www.oecd.org/content/dam/oecd/en/data/methods/OECD-Productivity-Statistics-Methodological-note.pdf)
- [The revamp of the OECD Productivity Database – Technical
  Report](https://www.oecd.org/content/dam/oecd/en/publications/reports/2025/06/the-revamp-of-the-oecd-productivity-database_3e568598/f07c55d4-en.pdf)
- [OECD Compendium of Productivity Indicators 2025: Cross-country
  comparisons of labour productivity
  levels](https://www.oecd.org/en/publications/oecd-compendium-of-productivity-indicators-2025_b024d9e1-en/full-report/cross-country-comparisons-of-labour-productivity-levels_b2fdb493.html)
- [Statistical Insights: Are international productivity gaps as large as
  we
  thought?](https://www.oecd.org/sdd/productivity-stats/statistical-insights-are-international-productivity-gaps-as-large-as-we-thought.htm)

## Kuinka suuri ero on

OECD:n käyttämät työtunnit saa esiin jakolaskulla `GVA / GVAHRS`. Se
palauttaa täsmälleen ne tunnit, joilla OECD on tuottavuutensa laskenut.
Tunnit ovat valuuttamuunnoksesta riippumattomat, koska osoittaja ja
nimittäjä jaetaan samalla ostovoimapariteetilla — siksi jakolasku
onnistuu myös niille maille, joiden sarjat OECD julkaisee vain
ostovoimakorjattuina. Paketti tekee jaon jo haun yhteydessä, joten
tunnit löytyvät suoraan `HRS`-mittarina.

Näytä koodi

``` r

oecd_hrs <-
  chr(dat_oecd_pdb_main) |>
  filter(measure == "HRS", activity == act) |>
  filter(is.finite(values)) |>
  select(time, geo, hrs_oecd = values)

hrs_cmp <-
  chr(dat_eurostat_na_ind) |>
  filter(measure == "HRS", activity == act) |>
  select(time, geo, hrs_es = values) |>
  inner_join(oecd_hrs, by = c("time", "geo")) |>
  filter(is.finite(hrs_es), is.finite(hrs_oecd), hrs_oecd != 0) |>
  mutate(r = nrm(hrs_es / hrs_oecd),
         oecd_korjaa = if_else(geo %in% geos_oecd_hours,
                               "OECD korvaa tunnit", "tilinpidon tunnit"))
```

Seuraavassa Eurostatin työtunnit suhteessa OECD:n käyttämiin,
normitettuna niin että 1 tarkoittaa samaa tuntisarjaa. Väri kertoo,
kuuluuko maa niihin kymmeneen, joiden tunnit OECD korvaa.

Näytä koodi

``` r

hrs_cmp |>
  ggplot(aes(time, r, group = geo, colour = oecd_korjaa)) +
  geom_hline(yintercept = 1, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ geo) +
  scale_colour_manual(values = c("OECD korvaa tunnit" = "firebrick",
                                 "tilinpidon tunnit" = "grey45")) +
  the_title_blank("xyl") +
  labs(title = "Eurostatin työtunnit / OECD:n käyttämät työtunnit",
       subtitle = "Normitettu, 1 = sama tuntisarja",
       colour = NULL,
       caption = "Lähde: Eurostat, OECD, Tuottavuuslautakunta")
```

![](tyotunnit_files/figure-html/fig-hours-1.png)

Figure 1

Näytä koodi

``` r

hrs_cmp |>
  summarise(ero_pros = 100 * (median(r) - 1),
            vaihtelu = 100 * (max(r) - min(r)),
            .by = c(geo, oecd_korjaa)) |>
  arrange(desc(abs(ero_pros))) |>
  knitr::kable(digits = 1,
               caption = "Eurostatin tunnit suhteessa OECD:n tunteihin, % ero mediaanista")
```

| geo  | oecd_korjaa        | ero_pros | vaihtelu |
|:-----|:-------------------|---------:|---------:|
| SE   | OECD korvaa tunnit |     10.3 |      0.2 |
| AT   | OECD korvaa tunnit |      8.8 |      3.9 |
| PT   | OECD korvaa tunnit |      7.9 |      3.6 |
| FI   | OECD korvaa tunnit |      3.2 |      0.5 |
| EA20 | tilinpidon tunnit  |      0.0 |      0.1 |
| BE   | tilinpidon tunnit  |      0.0 |      0.0 |
| DK   | tilinpidon tunnit  |      0.0 |      1.0 |
| DE   | tilinpidon tunnit  |      0.0 |      0.7 |
| ES   | tilinpidon tunnit  |      0.0 |      0.0 |
| FR   | tilinpidon tunnit  |      0.0 |      0.3 |
| IT   | tilinpidon tunnit  |      0.0 |      0.0 |
| NL   | tilinpidon tunnit  |      0.0 |      0.4 |
| NO   | tilinpidon tunnit  |      0.0 |      0.0 |

Table 1: Eurostatin tunnit suhteessa OECD:n tunteihin, % ero mediaanista

## Ero jakautuu tunteihin, ei arvonlisäykseen

Tuottavuuden ero on määritelmän mukaan arvonlisäyksen ero jaettuna
työtuntien erolla. Jos arvonlisäys täsmää ja tunnit eivät, koko ero
tulee tunneista.

Näytä koodi

``` r

vrt <- function(meas, unit_oecd) {
  inner_join(
    chr(dat_gdp_main) |>
      filter(measure == meas, activity == act,
             price_base == "LR", conversion_type == "_Z") |>
      select(time, geo, uusi = values),
    chr(dat_oecd_pdb_main) |>
      filter(measure == meas, activity == act, price_base == "LR",
             conversion_type == "_Z", unit_measure == unit_oecd) |>
      select(time, geo, vanha = values),
    by = c("time", "geo")
  ) |>
    filter(is.finite(uusi), is.finite(vanha), vanha != 0) |>
    transmute(time, geo, !!meas := nrm(uusi / vanha))
}

decomp <-
  vrt("GVAHRS", "XDC_H") |>
  inner_join(vrt("GVA", "XDC"), by = c("time", "geo")) |>
  inner_join(select(hrs_cmp, time, geo, HRS = r), by = c("time", "geo"))

decomp |>
  pivot_longer(c(GVA, HRS, GVAHRS), names_to = "measure", values_to = "r") |>
  ggplot(aes(time, r, colour = measure)) +
  geom_hline(yintercept = 1, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ geo) +
  scale_y_log10() +
  the_title_blank("xyl") +
  labs(title = "Yhdistetty aineisto / vanha OECD-aineisto",
       subtitle = "Normitettu, 1 = ei eroa. GVAHRS-ero = GVA-ero jaettuna HRS-erolla",
       colour = NULL,
       caption = "Lähde: Eurostat, OECD, Tuottavuuslautakunta")
```

![](tyotunnit_files/figure-html/fig-decomp-1.png)

Figure 2

`GVA`-viiva pysyy ykkösessä: arvonlisäys on molemmissa aineistoissa
sama. `HRS`- ja `GVAHRS`-viivat ovat toistensa peilikuvat.

## Kasvuvauhteihin ero ei juuri vaikuta

OECD:n oma huomio siitä, että korjaus vaikuttaa tasoihin muttei
kasvuvauhteihin, on tarkistettavissa suoraan: verrataan tuottavuuden
vuosimuutoksia.

Näytä koodi

``` r

inner_join(
  chr(dat_gdp_main) |>
    filter(measure == "GVAHRS", activity == act,
           price_base == "LR", conversion_type == "_Z") |>
    select(time, geo, uusi = values),
  chr(dat_oecd_pdb_main) |>
    filter(measure == "GVAHRS", activity == act, price_base == "LR",
           conversion_type == "_Z", unit_measure == "XDC_H") |>
    select(time, geo, vanha = values),
  by = c("time", "geo")
) |>
  arrange(time) |>
  mutate(across(c(uusi, vanha), ~ 100 * (.x / lag(.x) - 1)), .by = geo) |>
  pivot_longer(c(uusi, vanha), names_to = "aineisto", values_to = "muutos") |>
  filter(is.finite(muutos)) |>
  ggplot(aes(time, muutos, colour = aineisto)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line() +
  facet_wrap(~ geo) +
  the_title_blank("xyl") +
  labs(title = "Työn tuottavuuden vuosimuutos, %",
       subtitle = "Koko talous, kiintein hinnoin",
       colour = NULL,
       caption = "Lähde: Eurostat, OECD, Tuottavuuslautakunta")
```

![](tyotunnit_files/figure-html/fig-growth-1.png)

Figure 3

## Mitä tästä seuraa

**Kasvu- ja indeksikuviot ovat kunnossa.** Tuntisarjojen tasoero
kumoutuu indeksoinnissa eikä juuri näy kasvuvauhdeissa, joten yhdistetty
aineisto käy niihin sellaisenaan — ja on ajantasaisempi kuin pelkkä
OECD.

**Tuottavuuden tasovertailuissa on oltava tarkkana.** Yhdistetyssä
aineistossa tason määritelmä vaihtuu maan mukaan: kymmenelle maalle
vanha aineisto käyttää OECD:n harmonisoituja tunteja, uusi tilinpidon
tunteja. Maiden asettaminen tasojärjestykseen ei siis ole
vertailukelpoista yhdistetyllä aineistolla. Tasovertailuihin kannattaa
käyttää `main_oecd.qmd`-dokumentin OECD-aineistoa, jossa määritelmä on
sama kaikille maille.

Kumpi tuntikäsite on “oikea”, ei ole tämän muistion asia ratkaista:
OECD:n estimaatti on rakennettu nimenomaan kansainvälistä
vertailukelpoisuutta varten, tilinpidon tunnit taas ovat kunkin maan oma
virallinen luku. Olennaista on, ettei niitä sekoiteta samaan
tasovertailuun.

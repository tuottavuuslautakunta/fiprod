
geo_names_list <- list(
  fi = c(
    PT   = "Portugali",
    SE   = "Ruotsi",
    NO   = "Norja",
    ES   = "Espanja",
    EA20 = "Euroalue (20)",
    US   = "Yhdysvallat",
    AT   = "Itävalta",
    BE   = "Belgia",
    IT   = "Italia",
    DE   = "Saksa",
    NL   = "Alankomaat",
    JP   = "Japani",
    FI   = "Suomi",
    DK   = "Tanska",
    FR   = "Ranska"
  ),
  en = c(
    PT   = "Portugal",
    SE   = "Sweden",
    NO   = "Norway",
    ES   = "Spain",
    EA20 = "Euro area (20)",
    US   = "United States",
    AT   = "Austria",
    BE   = "Belgium",
    IT   = "Italy",
    DE   = "Germany",
    NL   = "Netherlands",
    JP   = "Japan",
    FI   = "Finland",
    DK   = "Denmark",
    FR   = "France"
  )
)

usethis::use_data(geo_names_list, overwrite = TRUE)

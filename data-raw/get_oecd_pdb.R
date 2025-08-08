## code to prepare `DATASET` dataset goes here

library(rsdmx)

dat_oecd_pdb_main <- readSDMX(
  providerId = "OECD",
  resource = "data",
  flowRef = "DSD_PDB@DF_PDB",
  key = list(c("FIN", "USA", "JPN", "GBR"), "A", "GVAHRS", c("_T", "BTE", "BTNXL", "C"), "XDC_H", "LR", "N", NULL, NULL)) |>
  oecd_clean_data(vars = c(geo = "REF_AREA", nace = "ACTIVITY"))

usethis::use_data(oecd_pdb_main, overwrite = TRUE)



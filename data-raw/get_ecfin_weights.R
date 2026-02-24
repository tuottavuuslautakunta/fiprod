
library(dplyr)
library(tidyr)

# https://economy-finance.ec.europa.eu/economic-research-and-databases/economic-databases/price-and-cost-competitiveness/price-and-cost-competitiveness-data-section_en
ecfin_w_link <- "https://ec.europa.eu/economy_finance/db_indicators/competitiveness/documents/csv.zip"
tempf <- tempfile()
download.file(ecfin_w_link, tempf)

# weight files in zip-datafiles
w_files <- unzip(tempf, list = TRUE)$Name %>%
  grep("w", ., value = TRUE)
# weight file prefixs
w_inds <- unique(substr(w_files, 1, 3))


weights_ecfin19 <- read_ecfin_weights(zipfile = tempf, file_pre = "a19w")
weights_ecfin20 <- read_ecfin_weights(zipfile = tempf, file_pre = "a20w")
weights_ecfin27 <- read_ecfin_weights(tempf, file_pre = "a27w")
weights_ecfin37 <- read_ecfin_weights(tempf, "a37w")
weights_ecfin42 <- read_ecfin_weights(tempf, file_pre = "a42w")

unlink(tempf)

usethis::use_data(weights_ecfin19, weights_ecfin20, weights_ecfin27, weights_ecfin37, weights_ecfin42, overwrite = TRUE)

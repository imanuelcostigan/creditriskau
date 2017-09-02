library(tidyverse)
library(readxl)

# APS 180, Att. A, para. 18 (Aug 2017 draft)
cva_rts_to_wts <- setNames(c(0.7, 0.8, 1, 2, 3, 10, 2) / 100, c(1:6, NA))
# APS 112, Att. H, para. 30 (Aug 2017 draft)
collat_table <- read_xlsx("data-raw/collateral.xlsx")
collateral_to_haircut <- setNames(c(collat_table$haircut, 1.0), c(collat_table$tag, NA))

devtools::use_data(cva_rts_to_wts, collateral_to_haircut,
  overwrite = TRUE, internal = TRUE)

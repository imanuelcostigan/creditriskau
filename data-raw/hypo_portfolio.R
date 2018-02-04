library(dataonderivatives)
library(tidyverse)
library(lubridate)
library(stringr)
library(fmdates)

# Constants ---------------------------------------------------------------

CALCULATION_DATE <- ymd(20170630)

# Source -----------------------------------------------------------------

ir_trade_data <- ddr(ymd(20170630), "IR")
fx_trade_data <- ddr(ymd(20170630), "FX")
eq_trade_data <- ddr(ymd(20170630), "EQ")
co_trade_data <- ddr(ymd(20170630), "CO")
cr_trade_data <- ddr(ymd(20170630), "CR")


# Munge -------------------------------------------------------------------

ir_trades <- ir_trade_data %>%
  remove_invalid() %>%
  select_fields() %>%


all_trades <- bind_rows(
  IR = ir_trade_data,
  FX = fx_trade_data,
  EQ = eq_trade_data,
  CO = co_trade_data,
  CR = cr_trade_data
)

# Web scraping a public gold website
# This codes is last updated on 28 June 2025

# Install packages
install.packages("rvest")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("readr")
install.packages("purrr")

# Packages
library(rvest)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# Read  html
pg <- read_html("https://publicgold.com.my/")


# Extract data ------------------------------------------------------------

# Date
date_updated <- 
  pg %>% 
  html_nodes("th") %>% 
  html_text() %>% 
  first() %>% 
  parse_date_time("dmy")

# GAP
gap_price <- 
  pg %>% 
  html_nodes("a") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(11:12) %>% 
  map_df(str_remove_all, "\\s") %>% 
  separate(value, into = c("pg_sell", "weight_dinar"), sep = "=") %>% 
  mutate(across(c(pg_sell, weight_dinar), parse_number)) %>% 
  mutate(pg_buy = 0,
         unit = "gram", 
         product = "gap", 
         date = date_updated)

# Gold bar
gold_bar_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(16:33) %>% 
  mutate(value = parse_number(value), 
         id = rep(c("weight_dinar", "pg_sell", "pg_buy"), times = 6), 
         id2 = rep(1:6, each = 3)) %>% 
  pivot_wider(id_cols = id2, names_from = id, values_from = value) %>% 
  select(-id2) %>% 
  mutate(unit = "gram", 
         product = "gold_bar", 
         date = date_updated)

# Gold wafer (24k)
gold_wafer24k_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(4:12) %>% 
  mutate(value = parse_number(value), 
         id = rep(c("weight_dinar", "pg_sell", "pg_buy"), times = 3), 
         id2 = rep(1:3, each = 3)) %>% 
  pivot_wider(id_cols = id2, names_from = id, values_from = value) %>% 
  select(-id2) %>% 
  mutate(unit = "dinar", 
         product = "gold_wafer_24k", 
         date = date_updated)

# Small bar / wafer
small_bar_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(36:45) %>% 
  mutate(value = parse_number(value)) %>%  
  mutate(
    row  = row_number(),
    grp  = (row + 1) %/% 2,                           # group every two rows
    name = if_else(row %% 2 == 1,                     # odd → weight, even → sell
                   "weight_dinar",
                   "pg_sell")
  ) %>%
  select(-row) %>%
  pivot_wider(
    id_cols   = grp,
    names_from = name,
    values_from = value
  ) %>%
  select(-grp) %>% 
  mutate(weight_dinar = replace(weight_dinar, row_number() == 3, 0.25),
         weight_dinar = replace(weight_dinar, row_number() == 4, 0.5)) %>% 
  mutate(pg_buy = 0,
         unit = c("gram", "gram", "dinar", "dinar", "gram"), 
         product = "small_bar", 
         date = date_updated)

# Gold wafer (22k)
gold_wafer22k_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(55:57) %>%
  mutate(value = parse_number(value)) %>%
  mutate(
    weight_dinar = first(value),     # row 1 → weight_dinar
    pg_sell      = nth(value, 2),    # row 2 → pg_sell
    pg_buy       = nth(value, 3)     # row 3 → pg_buy
  ) %>%
  slice(1) %>%                       # keep only the new first row
  select(weight_dinar, pg_sell, pg_buy) %>% 
  mutate(unit = "dinar", 
         product = "gold_wafer_22k", 
         date = date_updated)

# Jewellery (24k)
price_24k_jewel <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(61:63) %>% 
  mutate(value = parse_number(value)) %>%
  mutate(
    weight_dinar = first(value),     # row 1 → weight_dinar
    pg_sell      = nth(value, 2),    # row 2 → pg_sell
    pg_buy       = nth(value, 3)     # row 3 → pg_buy
  ) %>%
  slice(1) %>%                       # keep only the new first row
  select(weight_dinar, pg_sell, pg_buy) %>% 
  mutate(unit = "gram", 
         product = "jewellary_24k", 
         date = date_updated)


# Combine all -------------------------------------------------------------

pg_price <- 
  bind_rows(gap_price, gold_bar_price, gold_wafer24k_price, small_bar_price, 
            gold_wafer22k_price, price_24k_jewel) %>% 
  relocate(date, product, unit, weight_dinar, pg_sell, pg_buy)


# Write data --------------------------------------------------------------

write_csv(pg_price, paste0('data/', Sys.Date(), '_pg-data', '.csv'))

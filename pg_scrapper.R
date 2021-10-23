# Web scrapping a public gold website

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
  slice(13:14) %>% 
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
  slice(51:58) %>% 
  mutate(value = parse_number(value), 
         id = rep(c("weight_dinar", "pg_sell"), times = 4), 
         id2 = rep(1:4, each = 2)) %>% 
  pivot_wider(id_cols = id2, names_from = id, values_from = value) %>% 
  select(-id2) %>% 
  mutate(pg_buy = 0,
         unit = "gram", 
         product = "small_bar", 
         date = date_updated)

# Bungamas
bungamas_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(37:48) %>% 
  mutate(value = parse_number(value), 
         id = rep(c("weight_dinar", "pg_sell", "pg_buy"), times = 4), 
         id2 = rep(1:4, each = 3)) %>% 
  pivot_wider(id_cols = id2, names_from = id, values_from = value) %>% 
  select(-id2) %>% 
  mutate(unit = "gram", 
         product = "bungamas", 
         date = date_updated)

# Flexibar
flexibar_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(62:64) %>% 
  mutate(value = parse_number(value), 
         id = rep(c("weight_dinar", "pg_sell", "pg_buy"), times = 1)) %>% 
  pivot_wider(names_from = id, values_from = value) %>% 
  mutate(unit = "gram", 
         product = "flexibar", 
         date = date_updated)

# Gold wafer (22k)
gold_wafer22k_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(68:70) %>% 
  mutate(value = parse_number(value), 
         id = rep(c("weight_dinar", "pg_sell", "pg_buy"), times = 1)) %>% 
  pivot_wider(names_from = id, values_from = value) %>% 
  mutate(unit = "dinar", 
         product = "gold_wafer_22k", 
         date = date_updated)

# Gold jewellery (22k)
gold_jewel_price <- 
  pg %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  as_tibble() %>% 
  slice(73:74) %>% 
  mutate(value = parse_number(value), 
         id = rep(c("weight_dinar", "pg_buy"), times = 1)) %>% 
  pivot_wider(names_from = id, values_from = value) %>% 
  mutate(pg_sell = 0,
         unit = "gram", 
         product = "gold_jewel", 
         date = date_updated)


# Combine all -------------------------------------------------------------

pg_price <- 
  bind_rows(gap_price, gold_bar_price, gold_wafer24k_price, small_bar_price, bungamas_price, 
            flexibar_price, gold_wafer22k_price, gold_jewel_price) %>% 
  relocate(date, product, unit, weight_dinar, pg_sell, pg_buy)


# Write data --------------------------------------------------------------

write_csv(pg_price, paste0('data/', Sys.Date(), '_pg-data', '.csv'))


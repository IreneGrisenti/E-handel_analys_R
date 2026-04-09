library(tidyverse)

df <- read_csv("data/ecommerce_orders.csv")

#View(df)

head(df)

glimpse(df)

colSums(is.na(df))
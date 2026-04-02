library(tidyverse)

df <- read_csv("data/ecommerce_orders.csv")

head(df)

glimpse(df)

colSums(is.na(df))
  # visa data sett storlek
  # beskriva vilka typer av variabler som finns
  # identifiera saknade värden
  # beskriva kort vilka delar av datan som verkar viktigast för er analysgit
View(df)

sapply(df, class)

# Cleaning variables
categorical_columns <- c("customer_segment", "customer_type", "region", "city", 
                         "product_category", "product_subcategory", 
                         "payment_method", "campaign_source")

checking_inconsistencies <- categorical_columns %>%
  set_names() %>%
  map(~ sort(unique(df[[.x]])))

checking_inconsistencies


df_clean <- df %>% 
  mutate(
    order_id = str_trim(order_id),
    order_id = str_to_upper(order_id),
    order_date = as.Date(order_date),
    customer_id = str_trim(customer_id),
    customer_id = str_to_upper(customer_id),
    city = str_trim(city),
    city = str_to_title(city),
    payment_method = str_trim(payment_method),
    payment_method = str_to_title(payment_method),
    campaign_source = str_trim(campaign_source),
    campaign_source = str_to_title(campaign_source)
  )

# fix special caracters in city?

checking_inconsistencies <- categorical_columns %>%
  set_names() %>%
  map(~ sort(unique(df_clean[[.x]])))
  
checking_inconsistencies





  # hantera eventuella saknade värden på ett rimligt sätt
    # city - 21
    # payment methos - 25 
    # campaign source 31 
    # discount pct 27

  # skapa minst 2 nya variabler som hjälper er analys
  
    # Exempel:
      # ordervärde
      # pris efter rabatt
      # leveranskategori
      # rabattgrupp
      # kundgrupp
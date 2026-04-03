View(df)

# Cleaning relevant variables
categorical_columns <- c("customer_segment", "customer_type", "region", 
                         "city", "product_category", "product_subcategory", 
                         "campaign_source")

checking_inconsistencies <- categorical_columns %>%
  set_names() %>%
  map(~ sort(unique(df[[.x]])))

checking_inconsistencies


df_clean <- df %>% 
  mutate(
    # making sure the id is standardized
    order_id = str_trim(order_id),
    order_id = str_to_upper(order_id),
    customer_id = str_trim(customer_id),
    customer_id = str_to_upper(customer_id),
    
    # making sure the date is data type
    order_date = as.Date(order_date),
    
    # adjusted city spelling
    city = str_trim(city),
    city = str_to_title(city),
    city = str_replace_all(city, "Boras", "Borås"),
    city = str_replace_all(city, "Gothenburg", "Göteborg"),
    city = str_replace_all(city, "Linkoping", "Linköping"),
    city = str_replace_all(city, "Malmo", "Malmö"),
    city = str_replace_all(city, "Norrkoping", "Norrköping"),
    city = str_replace_all(city, "Orebro", "Örebro"),
    city = str_replace_all(city, "Vasteras", "Västerås"),
    
    # standardized campaign_source to avoid repeating values
    campaign_source = str_trim(campaign_source),
    campaign_source = str_to_title(campaign_source),
    
    # turning return into 0-1
    returned = str_trim(returned),
    returned = str_to_title(returned),
    returned = if_else(returned == "Yes", 1, 0, missing = NA),
    
    # adjusting datatype for int cols
    quantity = as.integer(quantity),
    shipping_days = as.integer(shipping_days)
  )

checking_inconsistencies <- categorical_columns %>%
  set_names() %>%
  map(~ sort(unique(df_clean[[.x]])))
  
checking_inconsistencies

glimpse(df_clean)

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
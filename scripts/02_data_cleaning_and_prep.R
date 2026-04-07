### Cleaning relevant variables ### 
categorical_columns <- c("customer_segment", "customer_type", "region", 
                         "city", "product_category", "product_subcategory", 
                         "campaign_source")

checking_inconsistencies <- categorical_columns %>%
  set_names() %>%
  map(~ sort(unique(df[[.x]])))

checking_inconsistencies


df_clean <- df %>% 
  mutate(
    # standardized id formatting to avoid duplicates from spacing or casing
    order_id = str_trim(order_id),
    order_id = str_to_upper(order_id),
    customer_id = str_trim(customer_id),
    customer_id = str_to_upper(customer_id),
    
    # making sure date is date type to enable calculation and sorting
    order_date = as.Date(order_date),
    
    # standardized city names to avoid duplicated groups in future analysis
    city = str_trim(city),
    city = str_to_title(city),
    city = str_replace_all(city, "Boras", "Borås"),
    city = str_replace_all(city, "Gothenburg", "Göteborg"),
    city = str_replace_all(city, "Linkoping", "Linköping"),
    city = str_replace_all(city, "Malmo", "Malmö"),
    city = str_replace_all(city, "Norrkoping", "Norrköping"),
    city = str_replace_all(city, "Orebro", "Örebro"),
    city = str_replace_all(city, "Vasteras", "Västerås"),
    
    # standardized campaign_source and payment_method to avoid duplicates
    campaign_source = str_trim(campaign_source),
    campaign_source = str_to_title(campaign_source),
    payment_method = str_trim(payment_method),
    payment_method = str_to_title(payment_method),
    
    # encoded returned into binary int to allow return rate calculations
    returned = str_trim(returned),
    returned = str_to_title(returned),
    returned = if_else(returned == "Yes", 1L, 0L, missing = NA),
    
    # adjusting datatype for int cols
    quantity = as.integer(quantity),
    shipping_days = as.integer(shipping_days)
  )

checking_inconsistencies <- categorical_columns %>%
  set_names() %>%
  map(~ sort(unique(df_clean[[.x]])))
  
checking_inconsistencies

glimpse(df_clean)


### Handling missing values ### 

# Helper function to find the most frequent category to make imputation cleaner
most_frequent_cat <- function(df, column) {
  names(sort(table(pull(df, {{column}})), decreasing = TRUE))[1]
}

df_clean <- df_clean %>% 
  mutate(
    # imputed cat cols with most frequent value to preserve categorical integrity. 
    # the pct of missing values is low and I assumed low risk
    city = replace_na(city, most_frequent_cat(df_clean, city)),
    campaign_source = replace_na(campaign_source, most_frequent_cat(df_clean, campaign_source)),
    payment_method = replace_na(payment_method, most_frequent_cat(df_clean, payment_method)),
    
    # imputed num cols with median to preserve data integrity, as median is 
    # more robust than mean
    shipping_days = replace_na(shipping_days, median(shipping_days, na.rm = TRUE)),
    discount_pct = replace_na(discount_pct, median(discount_pct, na.rm = TRUE))
  )

colSums(is.na(df_clean))


### Create new variables to help analysis ### 

df_clean <- df_clean %>% 
  # created an order_value variable to analyse how it impacts returns, 
  # categories, regions and cm type
  mutate(
    order_value = quantity * unit_price * (1 - discount_pct),
    .after = discount_pct) %>% 
  # labelled orders by value tier to make it easier to visualize
  mutate(
    order_value_tier = case_when(
      order_value <= quantile(order_value, 0.25) ~ "Low",
      order_value <= quantile(order_value, 0.50) ~ "Medium",
      order_value <= quantile(order_value, 0.75) ~ "High",
      TRUE                                        ~ "Premium"
    ),
    order_value_tier = factor(order_value_tier,
                              levels = c("Low", "Medium", "High", "Premium"),
                              ordered = TRUE),
    .after = order_value
  ) %>% 
  # labelled delivery speed to investigate how it impacts returns, categories, 
  # regions and cm type
  mutate(
    delivery_group = case_when(
      shipping_days <=2 ~ "Fast (1-2d)",
      shipping_days <=5 ~ "Medium (3-5d)",
      shipping_days <=9 ~ "Slow(6-9d)",
      TRUE ~ "Very slow (9d+)"
      ),
    delivery_group = factor(delivery_group, 
                            levels = c("Fast (1-2d)", "Medium (3-5d)",
                                       "Slow(6-9d)", "Very slow (9d+)"),
                            ordered = TRUE),
    .after = shipping_days
  ) %>% 
  # labelled discount tier to investigate how it impacts returns
  mutate(
    discount_tier = case_when(
      discount_pct == 0 ~ "No discount",
      discount_pct <= 0.10 ~ "Low (1-10%)",
      discount_pct <= 0.25 ~ "Medium (11-25%)",
      TRUE ~ "High (25%+)"
    ),
    discount_tier = factor(discount_tier, 
                            levels = c("No discount", "Low (1-10%)", 
                                       "Medium (11-25%)", "High (25%+)"),
                            ordered = TRUE),
    .after = discount_pct
  )

glimpse(df_clean)

# savig the cleaned df into a ready to use file
write_csv(df_clean, "data/df_clean.csv", col_names = TRUE)
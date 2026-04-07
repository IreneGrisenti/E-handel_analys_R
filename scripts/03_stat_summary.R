df <- read_csv("data/df_clean.csv",
               col_types = cols(
                 returned   = col_integer(),
                 shipping_days = col_integer(),
                 order_value_tier = col_factor(levels = c("Low","Medium","High","Premium"), ordered = TRUE),
                 delivery_group   = col_factor(levels = c("Fast (1-2d)","Medium (3-5d)","Slow(6-9d)","Very slow (9d+)"), ordered = TRUE)
               ))

total_orders     <- nrow(df)
unique_customers <- n_distinct(df$customer_id)
total_categories <- n_distinct(df$product_category)

cat("Total orders:", total_orders, "\n")
cat("Unique customers:", unique_customers, "\n")
cat("Product categories:", total_categories, "\n")

numeric_summary <- df %>%
  summarise(
    mean_order_value     = mean(order_value, na.rm = TRUE),
    median_order_value   = median(order_value, na.rm = TRUE),
    mean_shipping_days   = mean(shipping_days, na.rm = TRUE),
    median_shipping_days = median(shipping_days, na.rm = TRUE)
  )
print(numeric_summary)

return_rate <- mean(df$returned, na.rm = TRUE)
cat("Overall return rate:", scales::percent(return_rate, accuracy = 0.1), "\n")

# Share by customer segment
share_segment <- df %>%
  count(customer_segment) %>%
  mutate(share = n / sum(n)) %>%
  arrange(desc(share))
print(share_segment)

# Share by customer type
share_customer_type <- df %>%
  count(customer_type) %>%
  mutate(share = n / sum(n)) %>%
  arrange(desc(share))
print(share_customer_type)

# Share by product category
share_category <- df %>%
  count(product_category) %>%
  mutate(share = n / sum(n)) %>%
  arrange(desc(share))
print(share_category)

# Share by delivery group
share_delivery_group <- df %>%
  count(delivery_group) %>%
  mutate(share = n / sum(n)) %>%
  arrange(desc(share))
print(share_delivery_group)

# Return rate by product category
returns_by_category <- df %>%
  group_by(product_category) %>%
  summarise(
    count       = n(),
    returns     = sum(returned, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(desc(return_rate))
print(returns_by_category)

# Return rate by region
returns_by_region <- df %>%
  group_by(region) %>%
  summarise(
    count       = n(),
    returns     = sum(returned, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(desc(return_rate))
print(returns_by_region)

# Return rate by customer type
returns_by_customer_type <- df %>%
  group_by(customer_type) %>%
  summarise(
    count       = n(),
    returns     = sum(returned, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(desc(return_rate))
print(returns_by_customer_type)

# Return rate by delivery group
returns_by_delivery <- df %>%
  group_by(delivery_group) %>%
  summarise(
    count       = n(),
    returns     = sum(returned, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    .groups     = "drop"
  )
print(returns_by_delivery)

# Return rate by order value tier
returns_by_order_value_tier <- df %>%
  group_by(order_value_tier) %>%
  summarise(
    count       = n(),
    returns     = sum(returned, na.rm = TRUE),
    return_rate = mean(returned, na.rm = TRUE),
    .groups     = "drop"
  )
print(returns_by_order_value_tier)

# Order value by customer segment
value_by_segment <- df %>%
  group_by(customer_segment) %>%
  summarise(
    count              = n(),
    mean_order_value   = mean(order_value, na.rm = TRUE),
    median_order_value = median(order_value, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  arrange(desc(mean_order_value))
print(value_by_segment)

# Shipping days by region
shipping_by_region <- df %>%
  group_by(region) %>%
  summarise(
    count                = n(),
    mean_shipping_days   = mean(shipping_days, na.rm = TRUE),
    median_shipping_days = median(shipping_days, na.rm = TRUE),
    .groups              = "drop"
  ) %>%
  arrange(desc(mean_shipping_days))
print(shipping_by_region)

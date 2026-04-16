
library(tidyverse)
library(scales)
install.packages("viridis")
library(viridis)
install.packages("ggrepel")
library(ggrepel)


# Share by customer segment
p1 <- share_segment %>%
  ggplot(aes(x = "", y = share, fill = customer_segment)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_viridis_d() +
  geom_text(
    aes(label = scales::percent(share, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",         
    size = 4
  ) +
  labs(title = "Share by customer segment") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right"
  )
print(p1)

# Tolkning: 
# Diagrammet visar fördelningen av kunder mellan olika kundsegment. 
# Resultatet visar att segmentet Consumer är det största (52,9%)
# Detta följs av Small Business (30,2%) och Corporate (16,9%). 
# Detta indikerar att företaget främst har en konsumentdriven kundbas.


# Share by customer type
p2 <- share_customer_type %>%
  ggplot(aes(x = "", y = share, fill = customer_type)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_viridis_d() +
  geom_text(
    aes(label = scales::percent(share, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",          
    size = 4
  ) +
  labs(title = "Share by customer type") +
  theme_void()
print(p2)

# Tolkning: 
# Detta diagram visar fördelningen av kundtyper i datasetet.
# Returning customers utgör den största andelen (61.3%),
# följt av New customers (24.5%) och VIP customers (14.2%).
# Detta indikerar att återkommande kunder dominerar kundbasen.



# Share by product category
p3 <- share_category %>%
  ggplot(aes(x = "", y = share, fill = product_category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_viridis_d() +
  geom_text(
    aes(label = scales::percent(share, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",         
    size = 4
  ) +
  labs(title = "Share by product category") +
  theme_void()
print(p3)
# Tolkning: 
# Diagrammet visar fördelningen av försäljning mellan olika produktkategorier.
# Electronics är den största kategorin (24.3%), följt av Fashion (23.5%),
# Home (20.7%), Beauty (15.9%) och Sports (15.6%).
# Detta visar att försäljningen är relativt jämnt fördelad mellan kategorierna.
# Men Electronics och Fashion dominerar något.


# Share by delivery group
p4 <- share_delivery_group %>%
  ggplot(aes(x = "", y = share, fill = delivery_group)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_viridis_d() +
  geom_text(
    aes(label = scales::percent(share, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",         
    size = 4
  ) +
  labs(title = "Share by Delivery Group ") +
  theme_void()
print(p4)

# Tolkning:
# Diagrammet visar fördelningen av ordrar baserat på leveranstid.
# Majoriteten av beställningarna tillhör kategorin Medium (3–5 dagar) med 66.2%.
# Därefter följer Fast (1–2 dagar) med 23.2%. 
# Slow (6–9 dagar) utgör den minsta andelen med 10.6%.


# Return rate by product category
p5 <- returns_by_category %>%
  ggplot(aes(x = reorder(product_category, return_rate), 
             y = return_rate, 
             fill = return_rate)) +
  geom_col(color = "black") +
  geom_text(
  aes(label = paste0(sprintf("%.1f", return_rate * 100), "%")),
  color = "white",
  fontface = "bold",
  size = 3.5,
  vjust = 0.5,
  hjust = 1.2
) +
  geom_text(
    aes(
      y = return_rate,
      label = paste0("Sales: ", count,
                     "\nReturns: ", returns)
    ),
    hjust = -0.15,
    size = 3.2
  ) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.45))  
  ) +
  labs(
    title = "Return rate by Product Category",
    x = "Product Category",
    y = "Return Rate"
  ) +
  theme_minimal()
print(p5)

# Tolkning
# Diagrammet visar returfrekvensen för olika produktkategorier.
# Fashion har den högsta returgraden (16.2%), följt av Home (15.9%)
# och Beauty (15.1%).
# Electronics har en något lägre returgrad (13.2%),
# medan Sports har den lägsta returgraden (10.9%).


# Return rate by region
p6 <- returns_by_region %>%
  ggplot(aes(x = reorder(region, return_rate), 
             y = return_rate, 
             fill = return_rate)) +
  geom_col(color = "black") +
  geom_text(
    aes(label = paste0(sprintf("%.1f", return_rate * 100), "%")),
    color = "white",
    fontface = "bold",
    size = 3.5,
    vjust = 0.5,
    hjust = 1.2
  ) +
  geom_text(
    aes(
      y = return_rate,
      label = paste0("Sales: ", count,
                     "\nReturns: ", returns)
    ),
    hjust = -0.15,
    size = 3.2
  ) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Return Rate by Region",
    x = "Region",
    y = "Return Rate"
  ) +
  theme_minimal()
print(p6)

# Tolkning:
# Diagrammet visar returfrekvensen för olika regioner.
# Regionen West har den högsta returgraden (18.8%), 
# följt av South (15.6%) och East (13.0%).
# North har den lägsta returgraden (11.3%), vilket tyder på färre returer
# i denna region jämfört med övriga.


# Return rate by customer type
p7 <- returns_by_customer_type %>%
  ggplot(aes(x = reorder(customer_type, return_rate), 
             y = return_rate, 
             fill = return_rate)) +
  geom_col(color = "black") +
  geom_text(
    aes(label = paste0(sprintf("%.1f", return_rate * 100), "%")),
    color = "white",
    fontface = "bold",
    size = 3.5,
    vjust = 0.5,
    hjust = 1.2
  ) +
  geom_text(
    aes(
      y = return_rate,
      label = paste0("Sales: ", count,
                     "\nReturns: ", returns)
    ),
    hjust = -0.15,
    size = 3.2
  ) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Return Rate by Customer Type",
    x = "Customer Type",
    y = "Return Rate"
  ) +
  theme_minimal()
print(p7)

#Tolkning: 
# Diagrammet visar returfrekvensen för olika kundtyper.
# New customers har den högsta returgraden (18.0%), följt av VIP-kunder (13.4%)
# och Returning customers (13.2%).
# Detta indikerar att nya kunder tenderar att returnera produkter oftare
# än mer etablerade kunder. En möjlig förklaring kan vara att nya kunder
# har mindre erfarenhet av produkter eller att deras förväntningar inte
# alltid matchar verkligheten.



# Return rate by delivery group
p8 <- returns_by_delivery %>%
  ggplot(aes(x = reorder(delivery_group, return_rate), 
             y = return_rate, 
             fill = return_rate)) +
  geom_col(color = "black") +
  geom_text(
    aes(label = paste0(sprintf("%.1f", return_rate * 100), "%")),
    color = "white",
    fontface = "bold",
    size = 3.5,
    vjust = 0.5,
    hjust = 1.2
  ) +
  geom_text(
    aes(
      y = return_rate,
      label = paste0("Sales: ", count,
                     "\nReturns: ", returns)
    ),
    hjust = -0.15,
    size = 3.2
  ) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Return Rate by Delivery Group",
    x = "Delivery Group",
    y = "Return Rate"
  ) +
  theme_minimal()
print(p8)
# Tolkning:
# Diagrammet visar returfrekvensen baserat på olika leveransgrupper.
# Slow (6–9 dagar) har den högsta returgraden (20.8%), 
# följt av Medium (3–5 dagar) med 15.0%.
# Fast (1–2 dagar) har den lägsta returgraden (9.9%), vilket tyder på
# att snabbare leveranser är kopplade till färre returer.
# Detta indikerar att längre leveranstider kan påverka kundnöjdheten negativt
# och öka sannolikheten för returer. Kunder kan bli mer otåliga eller
# ändra sin uppfattning om produkten när leveransen tar längre tid.
# Slutsatsen är att leveranstid verkar vara en viktig faktor för
# returbenägenhet och kundupplevelse.




# Return rate by order value tier
p9 <- returns_by_order_value_tier %>%
  ggplot(aes(x = reorder(order_value_tier, return_rate), 
             y = return_rate, 
             fill = return_rate)) +
  geom_col(color = "black") +
  geom_text(
    aes(label = paste0(sprintf("%.1f", return_rate * 100), "%")),
    color = "white",
    fontface = "bold",
    size = 3.5,
    vjust = 0.5,
    hjust = 1.2
  ) +
  geom_text(
    aes(
      y = return_rate,
      label = paste0("Sales: ", count,
                     "\nReturns: ", returns)
    ),
    hjust = -0.15,
    size = 3.2
  ) +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.35))
  ) +
  labs(
    title = "Return Rate by Order Value Tier",
    x = "Order Value Tier",
    y = "Return Rate"
  ) +
  theme_minimal()
print(p9)
# Tolkning:
# Diagrammet visar returfrekvensen för olika ordervärdesnivåer.
# Lågprisordrar (Low) har den högsta returgraden (16.4%), följt av
# Medium (15.6%).
# Premium har en något lägre returgrad (13.6%), 
# Medan High har den lägsta returgraden (12.0%).
# Detta tyder på att kunder som köper billigare produkter tenderar att
# returnera varor oftare än kunder som köper dyrare produkter.


# Average Order value by customer segment
p10 <- value_by_segment %>%
  ggplot(aes(x = reorder(customer_segment, mean_order_value), 
             y = mean_order_value, 
             fill = mean_order_value)) +
  
  geom_col(color = "black") +
  geom_text(
    aes(label = round(mean_order_value, 0)),
    color = "white",
    fontface = "bold",
    size = 3.5,
    vjust = 0.5,
    hjust = 1.2
  ) +
  geom_text(
    aes(
      y = mean_order_value,
      label = paste0("Orders: ", count)
    ),
    hjust = -0.15,
    size = 3.2
  ) +
  
  coord_flip() +
  
  scale_fill_viridis_c(option = "plasma") +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.35))
  ) +
  
  labs(
    title = "Average Order Value by Customer Segment",
    x = "Customer Segment",
    y = "Mean Order Value"
  ) +
  
  theme_minimal()
print(p10)
# Tolkning:
# Diagrammet visar genomsnittligt och median ordervärde för olika kundsegment.
# Small Business har det högsta genomsnittliga ordervärdet (346),
# följt av Consumer (322).
# Corporate har det lägsta genomsnittliga ordervärdet (235).
#
# Medianvärdena är relativt lika mellan segmenten (cirka 135–162),
# vilket indikerar att skillnader i medelvärde kan påverkas av
# vissa höga ordervärden (outliers).
#
# Detta tyder på att Small Business-kunder tenderar att göra större köp,
# medan Corporate-kunder i genomsnitt handlar mindre per order.
#
# Slutsatsen är att olika kundsegment har olika köpbeteenden,
# vilket kan användas för att anpassa marknadsföring och erbjudanden.


# Shipping days by region
p11 <- shipping_by_region %>%
  ggplot(aes(x = reorder(region, mean_shipping_days), 
             y = mean_shipping_days, 
             fill = mean_shipping_days)) +
  
  geom_col(color = "black") +
  geom_text(
    aes(label = paste0(round(mean_shipping_days, 1), " days")),
    color = "white",
    fontface = "bold",
    size = 3.5,
    vjust = 1.3,
    hjust = 1.2
  ) +
  geom_text(
    aes(
      y = mean_shipping_days,
      label = paste0("Orders: ", count)
    ),
    hjust = -0.15,
    size = 3.2
  ) +
  
  coord_flip() +
  
  scale_fill_viridis_c(option = "plasma") +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.35))
  ) +
  
  labs(
    title = "Average Shipping Time by Region",
    x = "Region",
    y = "Mean Shipping Days"
  ) +
  theme_minimal()
print(p11)

# Tolkning:
# Diagrammet visar genomsnittlig leveranstid för olika regioner.
# East har den längsta genomsnittliga leveranstiden (4.47 dagar),
# följt av South (3.75 dagar) och West (3.59 dagar).
# North har den kortaste leveranstiden (3.10 dagar), vilket indikerar
# mer effektiva leveranser i denna region.
# Slutsatsen är att förbättring av leveransprocesser i vissa regioner,
# särskilt East, kan bidra till bättre kundupplevelse och potentiellt
# minska returer.



### Suggested addition
# Combination of segment and customer type with return rate
p12 <- returns_by_segment_type %>%
  ggplot(aes(x = customer_segment, 
             y = return_rate, 
             fill = customer_type)) +
  
  geom_col(position = position_dodge(width = 0.7), color = "black") +
  geom_text(
    aes(label = paste0(sprintf("%.1f", return_rate * 100), "%")),
    position = position_dodge(width = 0.7),
    color = "white",
    fontface = "bold",
    size = 3,
    vjust = 0.5,
    hjust = 1.5
  ) +
  
  coord_flip() +
  
  scale_fill_viridis_d(option = "plasma") +
  
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.35))
  ) +
  
  labs(
    title = "Return Rate by Customer Segment and Type",
    subtitle = "Comparison across segments and customer types",
    x = "Customer Segment",
    y = "Return Rate",
    fill = "Customer Type"
  ) +
  theme_minimal()
print(p12)

# Tolkning:
# Diagrammet visar returfrekvensen uppdelat efter både kundsegment och kundtyp.
# Det ger en mer detaljerad bild av kundbeteendet.

# Bland New customers har Small Business den högsta returgraden (21.7%),
# följt av Corporate (18.0%) och Consumer (16.3%).

# Detta indikerar att nya kunder inom Small Business-segmentet är
# särskilt benägna att returnera produkter.

# För Returning customers är returgraderna generellt lägre och mer stabila,
# där Corporate (14.3%), Consumer (13.3%) och Small Business (12.7%)
# ligger nära varandra.

# VIP-kunder visar en varierande returgrad, där Corporate (14.3%)
# och Consumer (13.9%) ligger på liknande nivåer, medan Small Business
# har en betydligt lägre returgrad (7.7%).

# Detta tyder på att kundtyp har en stark påverkan på returbenägenhet,
# särskilt för nya kunder, medan mer etablerade kunder (Returning och VIP)
# uppvisar mer stabilt beteende.

# Slutsatsen är att kombinationen av kundsegment och kundtyp ger viktiga
# insikter, där fokus bör ligga på att minska returer bland nya kunder,
# särskilt inom Small Business-segmentet.




# Checking the avg time for returned and not returned orders
p13 <- shipping_vs_return %>%
  ggplot(aes(x = as.factor(returned), 
             y = mean_shipping_days, 
             fill = mean_shipping_days)) +
  geom_col(color = "black") +
  geom_text(aes(label = round(mean_shipping_days, 1)),
            vjust = -0.5, size = 3.5) +
  scale_fill_viridis_c(option = "plasma") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Shipping Time vs Return Behavior",
    x = "Returned (0 = No, 1 = Yes)",
    y = "Average Shipping Days"
  ) +
  theme_minimal()
print(p13)

#Tolkning
# Diagrammet visar genomsnittlig leveranstid för ordrar som returnerats
# jämfört med de som inte har returnerats.
# Ordrar som har returnerats (Yes) har en högre genomsnittlig leveranstid
# på 3.98 dagar, jämfört med 3.55 dagar för ordrar som inte returnerats (No).
# Medianvärdet är högre för returnerade ordrar (4 dagar) jämfört med
# icke returnerade ordrar (3 dagar).
# Detta tyder på att längre leveranstider kan vara kopplade till en högre
# sannolikhet för returer. En möjlig förklaring är att längre väntetid kan
# påverka kundnöjdheten negativt.
# Slutsatsen är att snabbare leveranser kan bidra till att minska returer
# och förbättra kundupplevelsen.






save_plot <- function(plot, filename) {
  ggsave(
    filename = paste0("figures/", filename),
    plot = plot,
    width = 10,
    height = 6,
    dpi = 300
  )
}



save_plot(p1, "Share by customer segment.png")
save_plot(p2, "Share by customer type.png")
save_plot(p3, "Share by product category.png")
save_plot(p4, "Share by Delivery Group.png")
save_plot(p5, "Return rate by Product Category.png")
save_plot(p6, "Return rate by Region.png")
save_plot(p7, "Return Rate by Customer Type.png")
save_plot(p8, "Return Rate by Delivery Group.png")
save_plot(p9, "Return Rate by Order Value Tier.png")
save_plot(p10, "Average Order Value by Customer Segment.png")
save_plot(p11, "Average Shipping Time by Region.png")
save_plot(p12, "Return Rate by Customer Segment and Type.png")
save_plot(p13, "Shipping Time vs Return Behavior.png")




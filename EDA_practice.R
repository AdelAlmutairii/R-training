# Exploratory data analysis of dummy data aiming to find purchases trends


library(ggplot2)
library(dplyr)
library(gt)
library(lubridate)

# Move to data directory
setwd("~/Desktop")

# Read the data
df1 <- read.csv("customers.csv")
df2 <- read.csv("orders.csv")

# Merge both dataframes
merged_df <- merge(df1,df2)


# Extract total amount per purchase
merged_df$order_value <- merged_df$price_per_unit * merged_df$quantity
merged_df$order_value

# Total amount for each country's purchases
for (i in levels(as.factor(merged_df$country))) {
  country_sum <- sum(merged_df$order_value[merged_df$country == i], na.rm = TRUE)
  cat("Country:", i, "- Total Order Value:", country_sum, "\n")
}

# Calculate how much each customer paid
high_spending <- merged_df %>%
  group_by(customer_id) %>%
  summarise(total_spent = sum(order_value, na.rm = TRUE))


# Show top 3 highest-paying customers
high_spending %>%
  arrange(desc(total_spent)) %>%
  head(3)

# Create a column to represent months.
new_merge <- merged_df %>%
  mutate(yy_mm = format(as.Date(order_date), "%y-%m"))


# Summarise the sales per month
monthly_summary <- new_merge %>%
  group_by(yy_mm) %>%
  summarise(monthly_revenue = sum(order_value, na.rm = TRUE))

# Plot a line chart to observe sales per months/trends
ggplot(monthly_summary, aes(x = yy_mm, y = monthly_revenue, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Month", y = "Total Revenue", title = "Monthly Sales Trend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extract day gaps between sign up date and the date of the first purchase
new_merge$from_signup_to_purchase <- as.Date(new_merge$order_date) - as.Date(new_merge$signup_date)
# Conver to numeric
new_merge$from_signup_to_purchase <- as.numeric(new_merge$from_signup_to_purchase)

# Summary for each customer
new_merge <- new_merge %>%
  group_by(customer_id) %>%
  summarise(from_signup_to_purchase = first(from_signup_to_purchase))

# Bar plot to detect customers with higher than 90 days gap between first purchase and sign up
ggplot(new_merge, aes(x = reorder(customer_id, from_signup_to_purchase), y = from_signup_to_purchase)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 90, color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(x = "Customer ID", y = "Days from Signup to Purchase") +
  scale_y_continuous(breaks = seq(0, max(new_merge$from_signup_to_purchase), by = 30))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

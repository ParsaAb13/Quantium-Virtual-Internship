#Load Library 
library(ggplot2)
library(dplyr)
install.packages("lubridate")
library(lubridate)

#Load Data
data = read.csv("F://Dataset/QVI_data.csv")
View(data)

data$DATE <- as.Date(data$DATE, format="%Y-%m-%d")
#Create monthly Summary
monthly_summary <- data %>%
  mutate(Month = floor_date(DATE, "month")) %>%
  group_by(STORE_NBR, Month) %>%
  summarise(
    total_sales = sum(TOT_SALES),
    total_customers = n_distinct(LYLTY_CARD_NBR),
    avg_txn_per_customer = n_distinct(TXN_ID) / n_distinct(LYLTY_CARD_NBR)
  ) %>%
  ungroup()
View(monthly_summary)
#Create plot for 77 , 86 ,88
library(ggplot2)
trial_stores <- c(77, 86, 88)

ggplot(monthly_summary %>% filter(STORE_NBR %in% trial_stores),
       aes(x=Month, y=total_sales, color=factor(STORE_NBR))) +
  geom_line(size=1.2) +
  labs(title="Total Sales for Trial Stores", color="Store Number") +
  theme_minimal()


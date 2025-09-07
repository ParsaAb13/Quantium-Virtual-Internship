library(readxl)
install.packages("dplyr")
library(dplyr)
trans <- read_excel("F:/Dataset/QVI_transaction_data.xlsx")
cust <- read.csv("F:/Dataset/QVI_purchase_behaviour.csv")
cust$LIFESTAGE <- as.factor(cust$LIFESTAGE)
cust$PREMIUM_CUSTOMER <- as.factor(cust$PREMIUM_CUSTOMER)
merged <- merge(trans,cust, by="LYLTY_CARD_NBR")
summary_table <- merged %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(
    total_sales = sum(TOT_SALES),
    avg_sales   = mean(TOT_SALES),
    transactions = n()
  )
print(summary_table)
library(ggplot2)
ggplot(summary_table, aes(x=LIFESTAGE, y=total_sales, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity", position="dodge") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Customer Purchasing Behaviour",
       x="Lifestage", y="Total Sales")

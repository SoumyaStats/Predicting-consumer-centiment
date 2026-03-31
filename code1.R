#1. Load Required Libraries
# Install packages if needed
# install.packages(c("tidyverse", "ggplot2", "corrplot", "car", "MASS", "effects"))

library(tidyverse)   # for data manipulation and plotting
library(ggplot2)     # for advanced plots
library(corrplot)    # for correlation plots
library(car)         # for VIF and assumption checks
library(MASS)        # for ordinal logistic regression (polr)
library(effects)     # for visualizing effects in models

#2. Read the Data
library(readxl)
df <- read_excel("C:/Users/Dell/Downloads/canteen_shop_data.xlsx")
View(df)

#3. Explore the Data Structure
str(df)
summary(df)
head(df)

#4. Data Cleaning and Feature Engineering
# Convert Date to Date class and extract weekday
df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df$Weekday <- weekdays(df$Date)
df$Weekday <- factor(df$Weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# Extract hour from Time
df$Hour <- as.numeric(format(strptime(df$Time, format = "%H:%M"), "%H"))

# Create peak hour indicator (12:00-14:00)
df$PeakHour <- ifelse(df$Hour >= 12 & df$Hour < 14, 1, 0)

# Convert categorical variables to factors
df$Item <- factor(df$Item)
df$Payment.Method <- factor(df$`Payment Method`)
df$Weather <- factor(df$Weather)
df$Special.Offers <- factor(df$`Special Offers`, levels = c("No","Yes"))  # set reference to "No"

# Ensure satisfaction is numeric (it is, but confirm)
df$Customer.Satisfaction <- as.numeric(df$`Customer Satisfaction`)

# Check the new structure
str(df)


#5. Exploratory Data Analysis (EDA)
#5.1 Satisfaction distribution
library(ggplot2)
ggplot(df, aes(x = Customer.Satisfaction)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Customer Satisfaction", x = "Satisfaction Score", y = "Count")
#5.2 Satisfaction by Item
ggplot(df, aes(x = Item, y = Customer.Satisfaction, fill = Item)) +
  geom_boxplot() +
  labs(title = "Satisfaction by Item Type", y = "Satisfaction Score")
#5.3 Satisfaction by Weather
ggplot(df, aes(x = Weather, y = Customer.Satisfaction, fill = Weather)) +
  geom_boxplot() +
  labs(title = "Satisfaction by Weather", y = "Satisfaction Score")
#5.4 Satisfaction by Special Offers
ggplot(df, aes(x = Special.Offers, y = Customer.Satisfaction, fill = Special.Offers)) +
  geom_boxplot() +
  labs(title = "Satisfaction by Presence of Special Offers", y = "Satisfaction Score")
#5.5 Satisfaction by Payment Method
ggplot(df, aes(x = Payment.Method, y = Customer.Satisfaction, fill = Payment.Method)) +
  geom_boxplot() +
  labs(title = "Satisfaction by Payment Method", y = "Satisfaction Score")
#5.6 Satisfaction vs. Total Spending
ggplot(df, aes(x = Total, y = Customer.Satisfaction)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Satisfaction vs. Total Spending", x = "Total Spent (Rs.)", y = "Satisfaction")
#5.7 Satisfaction by Peak Hour
ggplot(df, aes(x = factor(PeakHour), y = Customer.Satisfaction, fill = factor(PeakHour))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("0" = "Off-Peak", "1" = "Peak")) +
  labs(title = "Satisfaction by Peak Hour", x = "", y = "Satisfaction Score")


#6. Fit Linear Regression Model
# Model specification
lm_model <- lm(Customer.Satisfaction ~ Total + PeakHour + Item + Payment.Method + 
                 Weather + Special.Offers, data = df)

# Summary of the model
summary(lm_model)
#6.1 Check Assumptions of Linear Regression
#a) Residuals vs Fitted (check linearity and homoscedasticity)
plot(lm_model, which = 1)
#b) Normal Q-Q plot (check normality of residuals)
plot(lm_model, which = 2)
#c) Scale-Location plot (check homoscedasticity)
plot(lm_model, which = 3)
#d) Check for multicollinearity (VIF)
vif(lm_model)   # VIF > 5-10 indicates problematic collinearity

#7. Interpret Linear Regression Results
#8. Advanced: Ordinal Logistic Regression
#First, convert satisfaction to an ordered factor:
df$Satisfaction_ord <- ordered(df$Customer.Satisfaction, levels = c(3,4,5))
#Fit the model:
ord_model <- polr(Satisfaction_ord ~ Total + PeakHour + Item + Payment.Method + 
                    Weather + Special.Offers, data = df, Hess = TRUE)
summary(ord_model)
#To get p-values, we can compute them from the t-value (coefficient/std.error) using normal approximation:
coeftest <- coef(summary(ord_model))
pvals <- pnorm(abs(coeftest[, "t value"]), lower.tail = FALSE) * 2
cbind(coeftest, p_value = pvals)

#9. Model Comparison and Selection

#10. Write-Up for Your Project Report

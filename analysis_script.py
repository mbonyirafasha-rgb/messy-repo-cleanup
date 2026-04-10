import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Load sales data
data = pd.read_csv('data/sales_data.csv')

# Basic analysis
print("Sales Data Analysis")
print("===================")
print(f"Total records: {len(data)}")
print(f"Average sales: ${data['Sales'].mean():.2f}")

# Plot sales by region
plt.figure(figsize=(10, 6))
data.groupby('Region')['Sales'].sum().plot(kind='bar')
plt.title('Sales by Region')
plt.ylabel('Total Sales')
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

# Load libraries
library(tidyverse)

# Load dataset
data <- read.csv("data/raw/kigali_diarrhea_data.csv")

# View structure
str(data)

# Convert variables to factors
data$diarrhea <- as.factor(data$diarrhea)
data$unsafe_water <- as.factor(data$unsafe_water)
data$poor_sanitation <- as.factor(data$poor_sanitation)

# Summary statistics
summary(data)

# Prevalence calculation
diarrhea_prev <- data %>%
  count(diarrhea) %>%
  mutate(percent = n/sum(n)*100)

print(diarrhea_prev)

# Visualization: prevalence pie chart
ggplot(diarrhea_prev, aes(x="", y=percent, fill=diarrhea)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y") +
  labs(title="Prevalence of Diarrhea in Kigali",
       fill="Diarrhea Status") +
  theme_void()

# Logistic regression model
model <- glm(diarrhea ~ unsafe_water +
                           poor_sanitation +
                           handwashing +
                           malnutrition +
                           overcrowding +
                           healthcare_access,
             data=data,
             family="binomial")

summary(model)

# Odds ratios
exp(coef(model))

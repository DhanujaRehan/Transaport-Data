
library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(nortest)


data <- read.csv("C:/Users/Rehan/Downloads/auto_Info.csv")

cat("Dataset Overview:\n")
print(str(data))
print(summary(data))
print(head(data))


cat("\n=== TASK 3: CENTRAL TENDENCY ANALYSIS ===\n")

numeric_vars <- c("engine_size", "horsepower", "curb_weight", "price")


central_tendency <- data.frame(
  Variable = numeric_vars,
  Mean = sapply(numeric_vars, function(x) mean(data[[x]], na.rm = TRUE)),
  Median = sapply(numeric_vars, function(x) median(data[[x]], na.rm = TRUE)),
  Mode = sapply(numeric_vars, function(x) {
    ux <- unique(data[[x]])
    ux[which.max(tabulate(match(data[[x]], ux)))]
  }),
  SD = sapply(numeric_vars, function(x) sd(data[[x]], na.rm = TRUE))
)

print(central_tendency)


par(mfrow = c(2, 2))

for(var in numeric_vars) {
  hist(data[[var]], 
       main = paste("Distribution of", var),
       xlab = var,
       ylab = "Frequency",
       col = "darkblue",
       probability = TRUE)
  
  x <- seq(min(data[[var]], na.rm = TRUE), max(data[[var]], na.rm = TRUE), length = 100)
  y <- dnorm(x, mean = mean(data[[var]], na.rm = TRUE), sd = sd(data[[var]], na.rm = TRUE))
  lines(x, y, col = "red", lwd = 2)
}


cat("\n=== TASK 4: PRICE vs VEHICLE TYPE ANALYSIS ===\n")


anova_result <- aov(price ~ vehicle_type, data = data)
print(summary(anova_result))

if(summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  cat("ANOVA is significant. Performing post-hoc tests:\n")
  print(TukeyHSD(anova_result))
}

ggplot(data, aes(x = vehicle_type, y = price, fill = vehicle_type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Price Distribution by Vehicle Type",
       x = "Vehicle Type",
       y = "Price (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



cat("\n=== TASK 5: CORRELATION AND REGRESSION ANALYSIS ===\n")

numeric_data <- data[, c("engine_size", "horsepower", "curb_weight", "age", "price")]

shapiro_price <- shapiro.test(data$price)
print(shapiro_price)

ad_price <- ad.test(data$price)
print(ad_price)

qqnorm(data$price, main = "Q-Q Plot for Price")
qqline(data$price, col = "red")

numeric_data <- data[, c("engine_size", "horsepower", "curb_weight", "age", "price")]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)


correlation_matrix <- cor(numeric_data, use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

corrplot(correlation_matrix, method = "circle", type = "upper")

predictors <- c("engine_size", "horsepower", "curb_weight", "age")

for(predictor in predictors) {
  cat(paste("\n--- Correlation Test: Price vs", predictor, "---\n"))
  
  cor_test <- cor.test(data$price, data[[predictor]], method = "pearson")
  print(cor_test)
  
  p <- ggplot(data, aes_string(x = predictor, y = "price")) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    theme_minimal() +
    labs(title = paste("Price vs", predictor),
         x = predictor,
         y = "Price (USD)")
  print(p)
}

cor_test_engine <- cor.test(data$price, data$engine_size, method = "pearson")
print(cor_test_engine)

cor_test_hp <- cor.test(data$price, data$horsepower, method = "pearson")
print(cor_test_hp)

cor_test_weight <- cor.test(data$price, data$curb_weight, method = "pearson")
print(cor_test_weight)

cor_test_age <- cor.test(data$price, data$age, method = "pearson")
print(cor_test_age)


cat("\n=== NORMALITY TESTS ===\n")

# Test normality of price (dependent variable)
cat("Normality test for Price:\n")
shapiro_price <- shapiro.test(data$price)
print(shapiro_price)

# Anderson-Darling test (alternative)
ad_price <- ad.test(data$price)
print(ad_price)

# Q-Q plot for price
qqnorm(data$price, main = "Q-Q Plot for Price")
qqline(data$price, col = "red")

# ==========================================
# MULTIPLE REGRESSION ANALYSIS
# ==========================================

cat("\n=== MULTIPLE REGRESSION ANALYSIS ===\n")

model <- lm(price ~ engine_size + horsepower + curb_weight + age, data = data)
print(summary(model))

# Model diagnostics
par(mfrow = c(2, 2))
plot(model)

# Check for multicollinearity
vif_values <- vif(model)
cat("Variance Inflation Factors (VIF):\n")
print(vif_values)

cat("\n=== DESCRIPTIVE STATISTICS BY VEHICLE TYPE ===\n")

vehicle_stats <- data %>%
  group_by(vehicle_type) %>%
  summarise(
    Count = n(),
    Avg_Price = mean(price, na.rm = TRUE),
    Avg_Fuel_Efficiency = mean(fuel_efficiency, na.rm = TRUE),
    Avg_Engine_Size = mean(engine_size, na.rm = TRUE),
    Avg_Weight = mean(curb_weight, na.rm = TRUE),
    .groups = 'drop'
  )

print(vehicle_stats)

brand_stats <- data %>%
  group_by(brand) %>%
  summarise(
    Count = n(),
    Avg_Price = mean(price, na.rm = TRUE),
    Avg_Fuel_Efficiency = mean(fuel_efficiency, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Avg_Fuel_Efficiency))

print("Brand Analysis (sorted by fuel efficiency):")
print(brand_stats)

write.csv(central_tendency, "central_tendency_results.csv", row.names = FALSE)
write.csv(correlation_matrix, "correlation_matrix.csv")
write.csv(vehicle_stats, "vehicle_type_analysis.csv", row.names = FALSE)
write.csv(brand_stats, "brand_analysis.csv", row.names = FALSE)

capture.output(summary(model), file = "regression_model_summary.txt")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to CSV files in working directory\n")
cat("Remember to interpret these results in the context of Sri Lankan transport needs\n")

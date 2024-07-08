# Load necessary libraries
library(readr)  # For reading data
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting
library(forecast)  # For time series forecasting
library(tseries)  # For statistical tests
library(gridExtra)  # For arranging plots
library(MASS)  # For ACF and PACF plots
library(lmtest)  # For testing stationarity

# Function to check and handle stationarity of time series
check_stationarity <- function(series) {
  adf_test <- adf.test(series, alternative = "stationary", k = 0)  # Augmented Dickey-Fuller Test
  if (adf_test$p.value > 0.05) {  # If p-value > 0.05, series is not stationary
    series <- diff(series, differences = 1)  # Take first difference to make it stationary
  }
  return(series)
}

# Function to plot time series data
plot_time_series <- function(data, title) {
  ggplot(data, aes(x = Date, y = Average)) +  # Plotting time series
    geom_line(color = "#0072B2") +  # Adding line
    labs(title = title, x = "Date", y = "Average Price") +  # Adding labels
    theme_minimal() +  # Setting theme
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
          axis.text = element_text(size = 12, color = "#0072B2"),
          axis.title = element_text(size = 14, color = "#0072B2"))
}

# Function to calculate forecast accuracy
calculate_accuracy <- function(actual, forecast) {
  errors <- actual - forecast  # Calculating errors
  mae <- mean(abs(errors))  # Mean Absolute Error
  mse <- mean(errors^2)  # Mean Squared Error
  mape <- mean(abs(errors / actual))  # Mean Absolute Percentage Error
  accuracy <- list(MAE = mae, MSE = mse, MAPE = mape)  # Storing accuracy measures
  return(accuracy)
}

# Read data from CSV file
data <- read_csv("vegetable.csv")

# Convert Date column to Date format
data$Date <- as.Date(data$Date)

# Create directory for saving plots if it doesn't exist
if (!file.exists("Plots")) {
  dir.create("Plots")
}

# Display dataset summary
cat("Dataset Summary:\n")
summary(data)

# Display first few rows of the dataset
cat("\nFirst few rows of the dataset:\n")
print(head(data))

# Save dataset summary to a text file
sink("Plots/Data_Summary.txt")
cat("Dataset Summary:\n")
summary(data)
cat("\nFirst few rows of the dataset:\n")
print(head(data))
sink()

# Check for missing values and handle them
missing_values <- sum(is.na(data))
if (missing_values > 0) {
  cat("There are", missing_values, "missing values in the dataset.\n")
  cat("Handling missing values by removing rows with missing data.\n")
  data <- na.omit(data)
}

# Display summary statistics
cat("Summary Statistics:\n")
cat("Mean:", mean(data$Average, na.rm = TRUE), "\n")
cat("Median:", median(data$Average, na.rm = TRUE), "\n")
cat("Standard Deviation:", sd(data$Average, na.rm = TRUE), "\n")

# Save summary statistics to a text file
sink("Plots/Summary_Statistics.txt")
cat("Summary Statistics:\n")
cat("Mean:", mean(data$Average, na.rm = TRUE), "\n")
cat("Median:", median(data$Average, na.rm = TRUE), "\n")
cat("Standard Deviation:", sd(data$Average, na.rm = TRUE), "\n")
sink()

# Plot time series data
time_series_plot <- plot_time_series(data, "Average Vegetable Prices over Time")
ggsave("Plots/Time_Series_Plot.png", plot = time_series_plot, bg = "white")

# Create scatter plot
scatter_plot <- ggplot(data, aes(x = Date, y = Average)) +
  geom_point(color = "#0072B2") +
  labs(title = "Scatter Plot of Average Vegetable Prices",
       x = "Date",
       y = "Average Price") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
        axis.text = element_text(size = 12, color = "#0072B2"),
        axis.title = element_text(size = 14, color = "#0072B2"))

# Save scatter plot to file
ggsave("Plots/Scatter_Plot.png", plot = scatter_plot, bg = "white")

# Convert data to time series object
ts_data <- ts(data$Average, frequency = 1)

# Create ACF and PACF plots
acf_plot <- ggAcf(ts_data)
pacf_plot <- ggPacf(ts_data)

# Save ACF and PACF plots to files
ggsave("Plots/ACF_Plot.png", plot = acf_plot, bg = "white")
ggsave("Plots/PACF_Plot.png", plot = pacf_plot, bg = "white")


# Transforming data for modeling
data$Log_Average <- log(data$Average)  # Log transformation
lambda <- BoxCox.lambda(data$Average)  # Find lambda for Box-Cox transformation
data$BoxCox_Average <- BoxCox(data$Average, lambda)  # Box-Cox transformation

# Calculate extended ACF plot
extended_acf <- acf(data$Average, lag.max = 50, type = "correlation", plot = FALSE)
plot(extended_acf, main = "Extended ACF Plot")
ggsave("Plots/Extended_ACF_Plot.png", bg = "white")  # Save plot
cat("Extended ACF shows significant correlations at various lags, indicating potential seasonality or trends in the data.\n")

# Create bar plot of mean average vegetable prices before removing outliers
barplot_before <- ggplot(data, aes(x = "", y = Average)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#0072B2") +
  labs(title = "Mean Average Vegetable Prices (Before Removing Outliers)",
       y = "Average Price") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
        axis.text = element_text(size = 12, color = "#0072B2"),
        axis.title = element_text(size = 14, color = "#0072B2")) +
  coord_cartesian(ylim = c(0, 150))
ggsave("Plots/Bar_Plot_Before_Outliers.png", plot = barplot_before, bg = "white")  # Save plot

# Remove outliers
outliers <- boxplot.stats(data$Average)$out
if (length(outliers) > 0) {
  cat("There are", length(outliers), "outliers in the 'Average' column.\n")
  cat("Removing outliers.\n")
  data <- data[!data$Average %in% outliers, ]
}

# Create bar plot of mean average vegetable prices after removing outliers
barplot_after <- ggplot(data, aes(x = "", y = Average)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#0072B2") +
  labs(title = "Mean Average Vegetable Prices (After Removing Outliers)",
       y = "Average Price") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
        axis.text = element_text(size = 12, color = "#0072B2"),
        axis.title = element_text(size = 14, color = "#0072B2")) +
  coord_cartesian(ylim = c(0, 150))
ggsave("Plots/Bar_Plot_After_Outliers.png", plot = barplot_after, bg = "white")  # Save plot

# Plot cleaned time series data
time_series_plot_cleaned <- plot_time_series(data, "Cleaned Average Vegetable Prices over Time")
ggsave("Plots/Cleaned_Time_Series_Plot.png", plot = time_series_plot_cleaned, bg= "white")  # Save plot

# Define research question
research_question <- "Can we accurately forecast vegetable prices for the next 10 time periods?"

# Split data into training and testing sets
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Check stationarity of training data
train_data$Average <- check_stationarity(train_data$Average)
adf_test <- adf.test(train_data$Average, alternative = "stationary", k = 0)
cat("ADF Test p-value:", adf_test$p.value, "\n")

# Save ADF test result to file
sink("Plots/ADF_Test_Result.txt")
cat("ADF Test p-value:", adf_test$p.value, "\n")
sink()

# Fit ARIMA, ETS, log-transformed ARIMA, and Box-Cox ARIMA models
arima_model <- tryCatch({
  auto.arima(train_data$Average, seasonal = TRUE)
}, error = function(e) {
  cat("Error during ARIMA modeling:", e$message, "\n")
  return(NULL)
})

# Extract coefficient estimates and their standard errors
arima_coefficients <- coef(arima_model)
arima_standard_errors <- sqrt(diag(vcov(arima_model)))

# Calculate Z statistic for each coefficient
z_statistics <- arima_coefficients / arima_standard_errors

# Conduct the Z test (assuming two-sided test)
p_values <- 2 * (1 - pnorm(abs(z_statistics)))

# Print results
cat("Z Test for Parameter Estimates (ARIMA model):\n")
for (i in seq_along(arima_coefficients)) {
  cat("Coefficient", names(arima_coefficients)[i], ":\n")
  cat("  Estimate:", arima_coefficients[i], "\n")
  cat("  Standard Error:", arima_standard_errors[i], "\n")
  cat("  Z Statistic:", z_statistics[i], "\n")
  cat("  p-value:", p_values[i], "\n\n")
}

ets_model <- tryCatch({
  ets(train_data$Average)
}, error = function(e) {
  cat("Error during ETS modeling:", e$message, "\n")
  return(NULL)
})

log_arima_model <- tryCatch({
  auto.arima(log(train_data$Average), seasonal = TRUE)
}, error = function(e) {
  cat("Error during log-transformed ARIMA modeling:", e$message, "\n")
  return(NULL)
})

boxcox_arima_model <- tryCatch({
  auto.arima(BoxCox(train_data$Average, lambda), seasonal = TRUE)
}, error = function(e) {
  cat("Error during Box-Cox ARIMA modeling:", e$message, "\n")
  return(NULL)
})

# Print model summaries if models are fitted successfully
if (!is.null(arima_model)) {
  cat("ARIMA Model Summary:\n")
  print(summary(arima_model))
}
if (!is.null(ets_model)) {
  cat("ETS Model Summary:\n")
  print(summary(ets_model))
}
if (!is.null(log_arima_model)) {
  cat("Log-Transformed ARIMA Model Summary:\n")
  print(summary(log_arima_model))
}
if (!is.null(boxcox_arima_model)) {
  cat("Box-Cox Transformed ARIMA Model Summary:\n")
  print(summary(boxcox_arima_model))
}

# Calculate AIC and BIC for each model if they are fitted successfully
if (!is.null(arima_model) && !is.null(ets_model) && !is.null(log_arima_model) && !is.null(boxcox_arima_model)) {
  aic_arima <- AIC(arima_model)
  bic_arima <- BIC(arima_model)
  aic_ets <- AIC(ets_model)
  bic_ets <- BIC(ets_model)
  aic_log_arima <- AIC(log_arima_model)
  bic_log_arima <- BIC(log_arima_model)
  aic_boxcox_arima <- AIC(boxcox_arima_model)
  bic_boxcox_arima <- BIC(boxcox_arima_model)

  
  cat("\nModel AIC/BIC Comparison:\n")
  cat("ARIMA AIC:", aic_arima, ", BIC:", bic_arima, "\n")
  cat("ETS AIC:", aic_ets, ", BIC:", bic_ets, "\n")
  cat("Log ARIMA AIC:", aic_log_arima, ", BIC:", bic_log_arima, "\n")
  cat("Box-Cox ARIMA AIC:", aic_boxcox_arima, ", BIC:", bic_boxcox_arima, "\n")
}

sink("Plots/Model_Comparisons.txt")
cat("Model AIC/BIC Comparison:\n")
cat("ARIMA AIC:", aic_arima, ", BIC:", bic_arima, "\n")
cat("ETS AIC:", aic_ets, ", BIC:", bic_ets, "\n")
cat("Log ARIMA AIC:", aic_log_arima, ", BIC:", bic_log_arima, "\n")
cat("Box-Cox ARIMA AIC:", aic_boxcox_arima, ", BIC:", bic_boxcox_arima, "\n")
sink()

best_model <- arima_model
best_model_name <- "ARIMA"

if (aic_ets < AIC(best_model) && bic_ets < BIC(best_model)) {
  best_model <- ets_model
  best_model_name <- "ETS"
}
if (aic_log_arima < AIC(best_model) && bic_log_arima < BIC(best_model)) {
  best_model <- log_arima_model
  best_model_name <- "Log ARIMA"
}
if (aic_boxcox_arima < AIC(best_model) && bic_boxcox_arima < BIC(best_model)) {
  best_model <- boxcox_arima_model
  best_model_name <- "Box-Cox ARIMA"
}

cat("Best model selected:", best_model_name, "\n")
sink("Plots/Best_Model.txt")
cat("Best model selected:", best_model_name, "\n")
sink()

forecasts <- forecast(best_model, h = length(test_data$Average))
forecast_plot <- autoplot(forecasts) +
  labs(title = paste("Forecasts using the Best Model:", best_model_name),
       x = "Date",
       y = "Average Price") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
        axis.text = element_text(size = 12, color = "#0072B2"),
        axis.title = element_text(size = 14, color = "#0072B2"))
ggsave("Plots/Forecast_Plot.png", plot = forecast_plot, bg = "white")

standardized_residuals <- residuals(best_model) / sd(residuals(best_model))

hist_plot <- ggplot(data.frame(standardized_residuals), aes(x = standardized_residuals)) +
  geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "white") +
  labs(title = "Histogram of Standardized Residuals",
         x = "Standardized Residuals",
         y = "Frequency") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
        axis.text = element_text(size = 12, color = "#0072B2"),
        axis.title = element_text(size = 14, color = "#0072B2"))
ggsave("Plots/Histogram_Standardized_Residuals.png", plot = hist_plot, bg = "white")

cat("Forecast Accuracy Metrics:\n")
cat("MAE:", accuracy_metrics$MAE, "\n")
cat("MSE:", accuracy_metrics$MSE, "\n")
cat("MAPE:", accuracy_metrics$MAPE, "\n")

sink("Plots/Forecast_Accuracy_Metrics.txt")
cat("Forecast Accuracy Metrics:\n")
cat("MAE:", accuracy_metrics$MAE, "\n")
cat("MSE:", accuracy_metrics$MSE, "\n")
cat("MAPE:", accuracy_metrics$MAPE, "\n")
sink()

residuals_plot <- autoplot(residuals(best_model)) +
  labs(title = "Residuals of the Best Model",
       x = "Date",
       y = "Residuals") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
        axis.text = element_text(size = 12, color = "#0072B2"),
        axis.title = element_text(size = 14, color = "#0072B2"))
ggsave("Plots/Residuals_Plot.png", plot = residuals_plot, bg = "white")

acf_residuals_plot <- ggAcf(residuals(best_model)) +
  labs(title = "ACF of Residuals") +
  theme_minimal()
ggsave("Plots/ACF_Residuals_Plot.png", plot = acf_residuals_plot, bg = "white")

pacf_residuals_plot <- ggPacf(residuals(best_model)) +
  labs(title = "PACF of Residuals") +
  theme_minimal()
ggsave("Plots/PACF_Residuals_Plot.png", plot = pacf_residuals_plot, bg = "white")

qq_plot <- ggplot(data.frame(residuals = residuals(best_model)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "#0072B2"),
        axis.text = element_text(size = 12, color = "#0072B2"),
        axis.title = element_text(size = 14, color = "#0072B2"))
ggsave("Plots/QQ_Plot_Residuals.png", plot = qq_plot, bg = "white")

if (best_model_name == "ARIMA" || best_model_name == "Log ARIMA" || best_model_name == "Box-Cox ARIMA") {
  z_test <- coeftest(best_model)
  cat("Z Test for Parameter Estimates:\n")
  print(z_test)
  
  sink("Plots/Z_Test_Parameter_Estimates.txt")
  cat("Z Test for Parameter Estimates:\n")
  print(z_test)
  sink()
}



# Time-Series-Analysis-and-Forecasting
This project analyzes and forecasts time series data. Key steps include data preparation, outlier detection, model fitting, and selection. The best model is used to forecast future values, and accuracy metrics are calculated. The project provides insights into trends and patterns.

Introduction
This project involves the analysis and forecasting of vegetable prices using time series data. The objective is to develop an accurate model to forecast future prices and provide insights into the price trends and patterns.

Libraries Used
readr: For reading data.
dplyr: For data manipulation.
ggplot2: For plotting.
forecast: For time series forecasting.
tseries: For statistical tests.
gridExtra: For arranging plots.
MASS: For ACF and PACF plots.
lmtest: For testing stationarity.
Data Preparation and Exploration
Read Data: The data is read from a CSV file named "vegetable.csv".
Convert Date Column: Convert the Date column to Date format.
Handle Missing Values: Identify and handle missing values by removing rows with missing data.
Summary Statistics: Calculate and display summary statistics such as mean, median, and standard deviation.
Plot Time Series: Visualize the time series data and identify any trends or patterns.
Time Series Analysis
Check Stationarity: Use the Augmented Dickey-Fuller (ADF) test to check the stationarity of the time series. If the series is not stationary, take the first difference to make it stationary.
ACF and PACF Plots: Generate ACF (Autocorrelation Function) and PACF (Partial Autocorrelation Function) plots to identify the order of the ARIMA model.
Outlier Detection and Handling
Bar Plots: Create bar plots to visualize the mean average vegetable prices before and after removing outliers.
Remove Outliers: Identify and remove outliers from the dataset.
Model Fitting and Selection
Split Data: Split the data into training and testing sets.
Fit Models: Fit ARIMA, ETS, log-transformed ARIMA, and Box-Cox ARIMA models to the training data.
Model Comparison: Compare models using AIC (Akaike Information Criterion) and BIC (Bayesian Information Criterion) to select the best model.
Best Model Selection: Select the best model based on the comparison.
Forecasting
Generate Forecasts: Use the best model to forecast vegetable prices for the next 10 time periods.
Plot Forecasts: Visualize the forecasts along with the actual data.
Model Diagnostics
Residual Analysis: Analyze residuals of the best model to check for any patterns or correlations.
ACF and PACF of Residuals: Generate ACF and PACF plots for residuals.
Normal Q-Q Plot: Create a Q-Q plot to check the normality of residuals.
Accuracy Metrics
Calculate Accuracy: Calculate forecast accuracy metrics such as Mean Absolute Error (MAE), Mean Squared Error (MSE), and Mean Absolute Percentage Error (MAPE).
Results
Best Model: The best model selected for forecasting vegetable prices.
Forecasts: Forecasts for the next 10 time periods.
Accuracy Metrics: MAE, MSE, and MAPE values for the forecasts.
Conclusion
This project successfully developed a model to forecast vegetable prices with reasonable accuracy. The analysis included checking stationarity, handling outliers, fitting multiple models, and selecting the best model based on AIC and BIC criteria. The forecasts and accuracy metrics provide valuable insights into future price trends.

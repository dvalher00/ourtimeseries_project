📈 Time Series Analysis: Food Expenditure in Australia (Apr 1980–2015)

This project conducts a detailed analysis of a time series corresponding to the total monthly expenditure on cafés, restaurants, and takeaway food services in Australia from April 1980 to April 2015.

🔍 Main Objectives:

Perform an exploratory analysis of the series.

Transform and difference the series to achieve stationarity.

Identify suitable SARIMA models to model the series.

Compare models based on statistical metrics (AIC, BIC, RMSE, MAE).

Diagnose residuals to validate model assumptions.

Generate a 30-month forecast and evaluate model fit.

🛠 Tools and Packages Used:

forecast: for modeling and forecasting.

astsa, portes: for advanced statistical analysis.

Custom functions for Box-Cox transformation and diagnostic testing.

🔧 Analysis Phases:

Initial exploration: visualization, detection of seasonality and heteroscedasticity.

Box-Cox transformation: λ = 0.2 obtained.

Differencing: both regular and seasonal to achieve stationarity.

SARIMA modeling: 7 different models tested.

Model comparison: best model selected based on AIC, BIC, and accuracy.

Residual diagnostics: Ljung-Box tests, non-parametric tests, and normality checks.

Forecasting: 30-month projection using the selected model.

📊 Result:
The selected SARIMA model (Model 3) demonstrates a solid fit and provides a reliable foundation for future forecasts of food sector consumption in Australia.

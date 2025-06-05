#' ---
#' title: "Time Series Analysis: Food Services Expenditure in Australia (1980-2015)"
#' author: "David Bravo Perez, Daniel Sanchez Pagan, David Valcarcel Herrera"
#' date: "`r Sys.Date()`"
#' output: 
#'   html_document:
#'     toc: true
#'     toc_depth: 3
#'     theme: journal
#'     highlight: tango
#'     code_folding: hide
#' ---
#' 
#' ## Loading Packages and Data

#+ setup, include=FALSE
knitr::opts_chunk$set(encoding = "UTF-8")

library(forecast)
library(ggplot2)
source("BoxCoxTransformation.R")
require(astsa)
source("Diagnostic.R")
require(portes)

#' Importing the time series data
data = read.table("data9.txt", header = TRUE)

#' ## Exploratory Data Analysis (EDA)

#' Creating the time series object
z <- ts(data[,1], start = c(1980, 4), frequency = 12)

#' ### Original Series Visualization
ts.plot(z, main = "Total Monthly Expenditure on Cafes, Restaurants and Takeaway Food Services in Australia (Apr 1980 - Apr 2015)", 
        ylab = "Consumption (in billions of dollars)", 
        xlim = c(1980, 2016))

#' ### Seasonal Plot
ggseasonplot(z, main = "Seasonal Plot: Monthly Food Services Expenditure", 
             year.labels = TRUE, year.labels.left = TRUE) +
  theme(legend.position = "bottom")

#' **Initial observations:**
#' - Clear upward trend component
#' - Annual seasonality evident (s=12)
#' - Increasing variance over time (heteroscedasticity)
#' - Need for variance-stabilizing transformation

#' ## Box-Cox Transformation
Plot.var(z, 12)
BoxCox(z, 12)

#' **Transformation results:**
#' - Optimal lambda = 0.2
#' - Not close enough to zero for log transformation
X.tilde = (z**0.2 - 1)/0.2
ts.plot(X.tilde, main = "Box-Cox Transformed Series (λ=0.2)", 
        xlim = c(1980, 2016))

#' ### Autocorrelation Function (ACF)
Acf(X.tilde, lag.max = 72, 
    main = "ACF of the Transformed Series", 
    xlab = "Lag (months)", ylab = "ACF")

#' ## Differencing for Stationarity

#' ### First Regular Difference
Wt.1 = diff(X.tilde, lag = 1, differences = 1)
ts.plot(Wt.1, 
        main = "First Regular Difference of Transformed Series", 
        xlim = c(1980, 2016))

#' ### First Seasonal Difference
Wt.2 = diff(Wt.1, lag = 12, differences = 1)
ts.plot(Wt.2, 
        main = "Regular and Seasonal Differences (1,1)", 
        xlim = c(1980, 2016))

#' ### ACF and PACF of Differenced Series
par(mfrow = c(2, 1))
Acf(Wt.2, lag.max = 72, 
    main = "ACF: Regular and Seasonal Differences", 
    xlab = "Lag (months)", ylab = "ACF")
Pacf(Wt.2, lag.max = 72, 
     main = "PACF: Regular and Seasonal Differences", 
     xlab = "Lag (months)", ylab = "PACF")
par(mfrow = c(1, 1))

#' **Model identification:**
#' - Regular MA(1) component (lag 1 spike)
#' - Seasonal MA(1) component (lag 12 spike)
#' - Possible seasonal AR component (geometric decay at seasonal lags)
#' - Significant spike at lag 24 suggests SMA(2) component

#' ## SARIMA Model Fitting

#' Fitting candidate SARIMA models
mod1 = Arima(z, order = c(2,1,1), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)
mod2 = Arima(z, order = c(2,1,1), seasonal = list(order = c(5,1,1), period = 12), lambda = 0.2)
mod3 = Arima(z, order = c(3,1,1), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)
mod4 = Arima(z, order = c(0,1,1), seasonal = list(order = c(2,1,2), period = 12), lambda = 0.2)
mod5 = Arima(z, order = c(0,1,1), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)
mod6 = Arima(z, order = c(0,1,1), seasonal = list(order = c(3,1,2), period = 12), lambda = 0.2)
mod7 = Arima(z, order = c(1,1,0), seasonal = list(order = c(3,1,1), period = 12), lambda = 0.2)

models <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
names(models) <- paste0("mod", 1:7)

#' ## Model Comparison

#' Creating comparison table
modelos <- list(
  mod1 = mod1,
  mod2 = mod2,
  mod3 = mod3,
  mod4 = mod4,
  mod5 = mod5,
  mod6 = mod6,
  mod7 = mod7
)

# Create dataframe with metrics
tabla_resultados <- data.frame()
for (nombre in names(modelos)) {
  mod <- modelos[[nombre]]
  acc <- accuracy(mod)
  
  rmse_val <- acc["Training set", "RMSE"]
  
  tabla_resultados <- rbind(tabla_resultados, data.frame(
    Model = nombre,
    AIC = round(mod$aic, 2),
    AICc = round(mod$aicc, 2),
    BIC = round(mod$bic, 2),
    RMSE = round(rmse_val, 6),
    MAE = round(acc["Training set", "MAE"], 4)
  ))
}

# Identify best values
best_AIC <- min(tabla_resultados$AIC)
best_AICc <- min(tabla_resultados$AICc)
best_BIC <- min(tabla_resultados$BIC)
best_RMSE <- min(tabla_resultados$RMSE)
best_MAE <- min(tabla_resultados$MAE)

# Define colors
color_texto <- "#111111"  # Very dark gray (almost black)
color_resaltado <- "#90EE90"  # Light green for highlighting
color_encabezado <- "#3498db"  # Blue for header

# Create table with highlighted cells and dark text
tabla_formateada <- tabla_resultados %>%
  mutate(
    AIC = cell_spec(AIC, "html", 
                    background = ifelse(AIC == best_AIC, color_resaltado, "white"),
                    color = color_texto),
    AICc = cell_spec(AICc, "html", 
                     background = ifelse(AICc == best_AICc, color_resaltado, "white"),
                     color = color_texto),
    BIC = cell_spec(BIC, "html", 
                    background = ifelse(BIC == best_BIC, color_resaltado, "white"),
                    color = color_texto),
    RMSE = cell_spec(RMSE, "html", 
                     background = ifelse(RMSE == best_RMSE, color_resaltado, "white"),
                     color = color_texto),
    MAE = cell_spec(MAE, "html", 
                    background = ifelse(MAE == best_MAE, color_resaltado, "white"),
                    color = color_texto)
  ) %>%
  kable(align = "c", escape = FALSE, format = "html",
        caption = "Monthly Expenditure on Food Services (Australia, Apr 1980–Apr 2015) - Model comparison") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE, background = color_encabezado, color = "white") %>%
  column_spec(1, bold = TRUE, color = color_texto) %>%
  add_header_above(c(" " = 1, "Likelihood Criteria" = 3, "Error Metrics" = 2),
                   background = color_encabezado, color = "white") %>%
  footnote(general = "The RMSE represents the square root of the MSE<br>Green cells highlight optimal values (lowest for all metrics)",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)

tabla_formateada

#' ## Residual Diagnostics

#' ### Custom Ljung-Box Tests
My.Ljung.Box.FixedK <- function(x, np, k) {
  n <- length(x)
  SampleACF <- as.numeric(Acf(x, lag.max = k, plot = FALSE)$acf)[2:(k + 1)]
  Q.ML <- n * (n + 2) * sum((SampleACF^2) / (n - seq_len(k)))
  pval <- 1 - pchisq(Q.ML, df = k - np)
  data.frame(k = k, Test_Statistic = Q.ML, P_value = pval)
}

#' ### Non-parametric Tests
NonParametric.Tests(mod2$residuals)    
NonParametric.Tests(mod3$residuals)    
NonParametric.Tests(mod5$residuals)    

#' ### Normality Tests
Check.normality(mod2$residuals)
Check.normality(mod3$residuals)
Check.normality(mod5$residuals)

#' ## Validation and Forecasting

#' ### Observed vs Fitted Values (Model 3)
plot(window(z, start = c(2010, 1)), 
     ylab = "Consumption (billions AUD)",
     xlab = "", type = "l", lwd = 1.5,
     main = "Model 3: Actual vs Fitted Values (2010-2015)")
lines(window(fitted(mod3), start = c(2010, 1)), 
      col = "red", lwd = 1.5)
legend("topleft", legend = c("Actual", "Fitted"), 
       col = c("black", "red"), lty = 1, lwd = 1.5)

#' ### 30-Month Forecast (Starting from 2010)
Predic.mod = forecast(mod3, h = 30)

# Create plot starting from 2010
autoplot(Predic.mod, include = 5*12,  # Show last 5 years of historical data
         main = "30-Month Forecast of Food Services Expenditure",
         ylab = "Consumption (billions AUD)",
         xlab = "Year") +
  theme_minimal() +
  scale_x_continuous(limits = c(2010, 2018), 
                     breaks = seq(2010, 2018, by = 1)) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12))


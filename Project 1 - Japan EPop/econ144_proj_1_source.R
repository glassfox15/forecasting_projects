# ~~~~~~~ Project 1 ~~~~~~~
# Econ 144
# Spring 2023
# Jacob Titcomb
# UID: 705799758
# ~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(knitr)
library(patchwork)
library(fpp3)
library(tsibble)
library(seasonal)
library(fabletools)
library(feasts)
library(forecast)


# ===== Initial Data Cleaning ====================

# Import data
japan_raw <- read_csv("japan_epop2.csv")                # starts 1975 Q1, ends 2022 Q4

# Create data frame
japan_df <- japan_raw %>%
  filter(classif1.label == "Age (Youth, adults): 15+",  # age range of 15+
         sex.label == "Sex: Total") %>%                 # include men and women
  rename(EPop = obs_value, Quarter = time) %>%
  select(Quarter, EPop) %>%                             # remove unnecessary variables
  mutate_at(vars(Quarter), yq) %>%                      # convert dates to Date objects
  arrange(Quarter)                                      # arrange oldest to most recent

# Create tsibble object
japan <- japan_df %>%
  mutate_at(vars(Quarter), yearquarter) %>%             # convert dates to tsibble year-quarter objects
  tsibble(index = Quarter)                              # quarterly frequency

# Applying Box-Cox Transformations
lambda <- japan %>%
  features(EPop, features = guerrero) %>%               # finding lambda using Guerrero method
  pull(lambda_guerrero)

japan <- japan %>%
  mutate_at(vars(EPop), function(x) {
    (sign(x) * (abs(x) ^ lambda) - 1) / lambda          # applying the Box-Cox transformation to tsibble
  })

japan_df <- japan_df %>%
  mutate_at(vars(EPop), function(x) {
    (sign(x) * (abs(x) ^ lambda) - 1) / lambda          # applying the Box-Cox transformation to data frame
  })

# Create time series object
japan_ts <- ts(japan, start = c(1975, 1), frequency = 4) # quarterly frequency from 1975 Q1


# ===== Plot of Time Series ====================

plot1 <- japan %>%
  autoplot(EPop) +
  labs(y = "Employment-Population Ratio, transformed",
       title = "Japan Employment-Population Ratio") +
  geom_hline(yintercept = mean(japan$EPop), lty = 2, col = "grey") # dashed line for mean

# Other exploratory analyses
plot17 <- japan %>% ggplot(aes(x = EPop)) +             # histogram plot
  geom_histogram(aes(y = after_stat(density)),
                 color = "dodgerblue4", fill = "dodgerblue3",
                 alpha = 0.75, binwidth = 0.0003) +
  geom_density(color = "midnightblue") +
  labs(title = "Japan Employment-Population Ratio histogram",
       y = "Frequency", x = "EPop, transformed")

plot18 <- japan %>% ggplot(aes(x = EPop)) +             # box plot
  geom_boxplot(color = "dodgerblue4", fill = "dodgerblue3", alpha = 0.75) +
  labs(title = "Japan Employment-Population Ratio box plot",
       x = "E-Pop, transformed") + coord_fixed(0.0009)


# ===== Autocorrelation Plots ====================

# Autocorrelation plot
plot2 <- japan %>%
  ACF(EPop, lag_max = 32) %>%
  autoplot() +
  labs(title = "ACF of E-Pop Ratio",
       y = "ACF")

# Partial autocorrelation plot
plot3 <- japan %>%
  ACF(EPop, lag_max = 32, type = "partial") %>%
  autoplot() +
  labs(title = "PACF of E-Pop Ratio",
       y = "PACF")


# ===== Trend Regressions ====================

lin_model <- lm(EPop ~ Quarter, data = japan_ts)        # simple linear regression
cubic_model <- lm(EPop ~ Quarter + I(Quarter^2) + I(Quarter^3), # cubic regression
                  data = japan_ts)

# Time series plot with linear model
plot4 <- ggplot(data = japan_df, aes(x = Quarter, y = EPop)) +
  geom_line() +
  geom_line(aes(y = lin_model$fitted.values), col = "dodgerblue4", linewidth = 1.25) +
  labs(y = "Employment-Population Ratio, transformed",
       title = "Japan E-Pop Ratio",
       subtitle = "with fitted linear trend model")

# Time series plot with cubic model
plot5 <- ggplot(data = japan_df, aes(x = Quarter, y = EPop)) +
  geom_line() +
  geom_line(aes(y = cubic_model$fitted.values), col = "dodgerblue4", linewidth = 1.25) +
  labs(y = "Employment-Population Ratio, transformed",
       title = "Japan E-Pop Ratio",
       subtitle = "with fitted cubic trend model")

# Residuals vs fitted plot for linear model
plot6 <- lin_model %>% ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  labs(title = "Residuals vs. fitted plot",
       subtitle = "with linear model",
       y = "Residuals",
       x = "Fitted values")

# Residuals vs fitted plot for cubic model
plot7 <- cubic_model %>% ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2, color = "grey50") +
  labs(title = "Residuals vs. fitted plot",
       subtitle = "with cubic model",
       y = "Residuals",
       x = "Fitted Values")

# Histogram of residuals for linear model
plot8 <- lin_model %>% ggplot(aes(x = .resid)) +
  geom_histogram(aes(y = after_stat(density)),
                 color = "dodgerblue4", fill = "dodgerblue3",
                 alpha = 0.75, binwidth = 0.00025) +
  geom_density(color = "midnightblue") +                # includes density curve
  labs(title = "Residual histogram",
       subtitle = "with linear model",
       y = "Frequency",
       x = "Residuals")


# Histogram of residuals for cubic model
plot9 <- cubic_model %>% ggplot(aes(x = .resid)) +
  geom_histogram(aes(y = after_stat(density)),
                 color = "dodgerblue4", fill = "dodgerblue3",
                 alpha = 0.75, binwidth = 0.00025) +
  geom_density(color = "midnightblue") +                # includes density curve
  labs(title = "Residual histogram",
       subtitle = "with cubic model",
       y = "Frequency",
       x = "Residuals")

# Function for summary statistics
model_statistics <- function(model) {
  # printing which model is being studied
  if (length(coef(model)) == 2) {
    cat("~~~ Linear Model ~~~\n\n")
  } else if (length(coef(model)) == 4) {
    cat("~~~ Cubic Model ~~~\n\n")
  }
  
  cat("Parameter estimates and their significance:\n")
  print(summary(model)[[4]])                            # summary of parameters
  f_stat <- unname(unlist(summary(model)[10]))          # f-statistic
  f_p_value <- 1 - pf(f_stat[1], f_stat[2], f_stat[3])  # p-value
  if (f_p_value == 0) f_p_value <- "< 2.2e-16"
  adj_r.sq <- summary(model)[[9]]                       # adjusted R^2
  adj_mse <- sum(model$residuals ^ 2) / length(model$residuals) # MSE
  
  # printing statistics
  cat(sep = "", "\nF-statistic: ", f_stat[1], ", p-value: ", f_p_value, "\n",
      "Adjusted R^2: ", adj_r.sq, "\n",
      "MSE: ", adj_mse, "\n\n")
}

# AIC and BIC calculations
aic_bic <- AIC(lin_model, cubic_model) %>%              # calculating AIC
  rownames_to_column() %>%
  rename(Model = rowname) %>%
  left_join(BIC(lin_model, cubic_model), by = "df") %>% # calculating BIC and combining the data frames
  select(Model, AIC, BIC)


# ===== Forecasting Trend ====================

# Creating forecasting time indices in a vector
h <- 20                                                 # 20 steps ahead
years_to_predict <- rep(yearquarter(NA), 192 + h)       # initialize vector for future time values
for (i in seq(0, 191 + h)) {
  year <- (i %/% 4) + 1975                              # calculating year
  quarter <- (i %% 4) + 1                               # calculating quarter
  future_date <- paste(sep = "", year, "Q", quarter)    # join year and quarter
  years_to_predict[[i + 1]] <- yearquarter(future_date) # convert to tsibble year-quarter objects
}

# Time indices as a data frame
years_to_predict_df <- data.frame(Quarter = years_to_predict)

# Time indices as a time series object
years_to_predict <- ts(years_to_predict_df, start = c(1975, 1), frequency = 4) 

# Estimate trend model at each time index
predicted <- predict(cubic_model, newdata = years_to_predict,
                     interval = 'prediction', level = 0.95) # includes 95% prediction interval

# Join predictions with time index in data frame
predicted_df <- as.data.frame(cbind(years_to_predict, predicted)) %>%
  mutate_at(vars(years_to_predict), function(x){
    # Again turn time into a Date object
    year <- 1970 + (x %/% 4)                            # years
    quarter <- (x %% 4) + 1                             # quarters
    return(yq(paste(sep = "", year, "Q", quarter)))
  }) %>%
  rename(Quarter = years_to_predict, EPop = predicted.fit, # make variable names more clear
         Lower = predicted.lwr, Upper = predicted.upr)

# Removing 95% prediction interval for data we do have (2022 Q4 and before)
not_forecasted <- predicted_df[["Quarter"]] < as.Date("2023-01-01") # from before Jan 1, 2023
predicted_df[not_forecasted, c("Lower", "Upper")] <- NA
predicted_df <- predicted_df %>%
  mutate(Predictions = !is.na(Lower))                   # removes missing values from predictions

# Plot of time series with forecast overlay
plot10 <- ggplot(data = predicted_df, aes(x = Quarter, y = EPop)) +
  geom_line(data = japan_df, aes(x = Quarter, y = EPop)) +
  geom_line(aes(x = Quarter, y = EPop), col = "dodgerblue4", linewidth = 1.25) +
  geom_line(aes(x = Quarter, y = Lower), col = "grey40",  # lower bound of prediction interval
            lty = 2, na.rm = TRUE) +
  geom_line(aes(x = Quarter, y = Upper), col = "grey40",  # upper bound of prediction interval
            lty = 2, na.rm = TRUE) + 
  labs(y = "Employment-Population Ratio, transformed",
       title = "Japanese Employment-Population Ratio forecast",
       subtitle = "with 20 steps ahead and prediction intervals for cubic model")


# ===== Preparing to Seasonally Adjust Data ====================

# Create columns of dummy variables for quarter, and fitted values from trend model
japan_model <- japan_df %>%
  mutate(Q1 = as.numeric(quarter(Quarter) == 1),        # quarter 1
         Q2 = as.numeric(quarter(Quarter) == 2),        # quarter 2
         Q3 = as.numeric(quarter(Quarter) == 3),        # quarter 3
         Q4 = as.numeric(quarter(Quarter) == 4)) %>%    # quarter 4
  bind_cols(trend = cubic_model$fitted.values)          # add column of fitted data from cubic model

 # Preparing additive decomposition
japan_add_df <- japan_model %>%                         # detrended data, as a data frame
  mutate(add_detrend = EPop - trend)                    # Y - T
japan_add_ts <- japan_add_df %>%                        # detrended data, as a time series object
  ts(start = c(1975, 1), frequency = 4)

# Preparing multiplicative decomposition
japan_mult_df <- japan_model %>%                        # detrended data, as a data frame
  mutate(log_detrend = log(EPop / trend))               # log(Y / T), log so we can use linear regression
japan_mult_ts <- japan_mult_df %>%                      # detrended data, as a time series object
  ts(start = c(1975, 1), frequency = 4)


# ===== Seasonally Adjust Data ====================

## --- Additive Adjustment ------------------------
japan_add_model <- lm(add_detrend ~ Q2 + Q3 + Q4,       # regress on Q2, Q3, Q4
                      data = japan_add_ts)

add_coeff <- c(0, unname(coefficients(japan_add_model))[2:4]) # seasonal adjustments are (0, beta1, beta2, beta3)
add_coeff_vec <- rep(add_coeff, nrow(japan_model) / 4)  # repeat vector so can append it to detrended data frame

japan_add <- japan_add_df %>%
  bind_cols(season_coeff = add_coeff_vec) %>%           # add coefficient vector as a column
  mutate(season_adjust = add_detrend - season_coeff) %>% # Y - T - S
  mutate_at(vars(Quarter), yearquarter) %>%             # convert dates to tsibble year-quarter objects
  select(Quarter, season_adjust) %>%
  tsibble(index = Quarter)                              # turn into tsibble


## --- Multiplicative Adjustment ------------------
japan_mult_model <- lm(log_detrend ~ Q2 + Q3 + Q4,      # regress log(Y / T) on Q2, Q3, Q4
                       data = japan_mult_ts)

mult_coeff <- c(0, unname(coefficients(japan_mult_model))[2:4]) # seasonal adjustments are (0, beta1, beta2, beta3)
mult_coeff_vec <- rep(mult_coeff, nrow(japan_model) / 4) # repeat vector so can append it to detrended data frame

japan_mult <- japan_mult_df %>%
  bind_cols(season_coeff = mult_coeff_vec) %>%          # add coefficient vector as a column
  # Seasonally adjust with: Y / (ST) = exp(log(Y / T)) / exp(S)
  mutate(season_adjust = exp(log_detrend) / exp(season_coeff)) %>%
  mutate_at(vars(Quarter), yearquarter) %>%             # convert dates to tsibble year-quarter objects
  select(Quarter, season_adjust) %>%
  tsibble(index = Quarter)                              # turn into tsibble


# ===== Seasonally Adjusted Plots ====================

# Plot with additive seasonal adjustment
plot11 <- japan_add %>% autoplot(season_adjust) +
  labs(title = "Employment-Population Ratio residuals",
       subtitle = "with additive adjustment",
       y = "E-Pop Ratio residuals") + 
  geom_hline(yintercept = 0, lty = 2, color = "grey50")

# ACF plot for additive seasonal adjustment
plot12 <- japan_add %>%
  ACF(season_adjust, lag_max = 32) %>%
  autoplot() + labs(title = "ACF of E-Pop Ratio residuals",
                    y = "ACF", subtitle = "with additive seasonality adjustment")

# PACF plot for additive seasonal adjustment
plot13 <- japan_add %>%
  ACF(season_adjust, lag_max = 32, type = "partial") %>%
  autoplot() + labs(title = "PACF of E-Pop Ratio residuals",
                    y = "PACF", subtitle = "with additive seasonality adjustment")

# Plot with multiplicative seasonal adjustment
plot14 <- japan_mult %>% autoplot(season_adjust) +
  labs(title = "Employment-Population Ratio residuals",
       subtitle = "with multiplicative adjustment",
       y = "E-Pop Ratio residuals") +
  geom_hline(yintercept = 1, lty = 2, color = "grey50")

# ACF plot for multiplicative seasonal adjustment
plot15 <- japan_mult %>%
  ACF(season_adjust, lag_max = 32) %>%
  autoplot() + labs(title = "ACF of E-Pop Ratio residuals",
                    y = "ACF", subtitle = "with multiplicative adjustment")

# PACF plot for multiplicative seasonal adjustment
plot16 <- japan_mult %>%
  ACF(season_adjust, lag_max = 32, type = "partial") %>%
  autoplot() + labs(title = "PACF of E-Pop Ratio residuals",
                    y = "PACF", subtitle = "with multiplicative adjustment")

# Comparing ACF and PACF of the two adjustments
ACF_diff <- japan_add %>%
  ACF(season_adjust, lag_max = 32) %>%
  left_join(ACF(japan_mult, season_adjust, lag_max = 32), by = "lag") %>%
  rename(add_ACF = acf.x, mult_ACF = acf.y) %>%
  mutate_at(vars(add_ACF, mult_ACF), abs)

#sum(ACF_diff$add_ACF) - sum(ACF_diff$mult_ACF)         # absolute ACF values are *slightly* higher for additive
# OUTPUT: 0.0002204933

PACF_diff <- japan_add %>%
  ACF(season_adjust, lag_max = 32, type = "partial") %>%
  left_join(ACF(japan_mult, season_adjust, lag_max = 32, type = "partial"), by = "lag") %>%
  rename(add_ACF = acf.x, mult_ACF = acf.y) %>%
  mutate_at(vars(add_ACF, mult_ACF), abs)


#sum(PACF_diff$add_ACF) - sum(PACF_diff$mult_ACF)       # absolute PACF values are *slightly* higher for additive
# OUTPUT: 0.001575493


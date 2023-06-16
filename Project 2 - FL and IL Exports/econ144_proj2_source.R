# ~~~~~~~ Project 2 ~~~~~~~
# Econ 144
# Spring 2023
# Jacob Titcomb
# UID: 705799758
# ~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(knitr)
library(lubridate)
library(patchwork)
library(fpp3)
library(tsibble)
library(seasonal)
library(fabletools)
library(feasts)
library(forecast)
library(strucchange)
library(vars)

# ===== Initial Data Cleaning ====================

# Import the data
FL_raw <- read_csv("fl_exp.csv")                  # importing Florida
IL_raw <- read_csv("il_exp.csv")                  # importing Illinois

# Transform with log
FL_raw <- FL_raw %>%
  rename(Month = DATE, Exp = EXPTOTFL) %>%
  mutate_at(vars(Exp), log)

IL_raw <- IL_raw %>%
  rename(Month = DATE, Exp = EXPTOTIL) %>%
  mutate_at(vars(Exp), log)

# Convert to tsibble
FL_tsib <- FL_raw %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)

IL_tsib <- IL_raw %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)

# Convert to ts object
FL_ts <- ts(FL_raw, start = c(1995, 8), frequency = 12)[,-1]
IL_ts <- ts(IL_raw, start = c(1995, 8), frequency = 12)[,-1]


# ===== Auxiliary Code ====================
## NBER recessions for ggplot2 ------------

# auxiliary function to extract dates
nber_to_date <- function(string){
  year <- paste(unlist(strsplit(as.character(string), ""))[1:4], collapse = "")
  month <- paste(unlist(strsplit(as.character(string), ""))[5:6], collapse = "")
  day <- paste(unlist(strsplit(as.character(string), ""))[7:8], collapse = "")
  return(paste(sep = "-", year, month, day))
}

# extracting dates from `nberDates()`
recession_df <- tis::nberDates() %>% data.frame() %>%
  mutate_at(vars(Start, End), function(x){
    as.Date(vapply(x, nber_to_date, character(1)))
  })
recessions <- recession_df[32:34,]                # save as time series

## Uncomment for plotting
  # annotate(geom = "rect", xmin = recessions$Start[1], xmax = recessions$End[1],
  #        ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  # annotate(geom = "rect", xmin = recessions$Start[2], xmax = recessions$End[2],
  #        ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  # annotate(geom = "rect", xmin = recessions$Start[3], xmax = recessions$End[3],
  #        ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25)


## Objects for time period ----------------

### for adding to data frames

# as data frame
Months_df <- bind_cols(year = as.numeric(time(FL_ts)) %/% 1, # extract year
                       month = round(12 * (as.numeric(time(FL_ts)) %% 1) + 1)) %>% # extract month
  mutate(Month = yearmonth(paste(year, month))) %>% # concatenate
  dplyr::select(Month)

# data frame for observed months + FORECASTED
Months_pred_df <- bind_rows(Months_df[,1],
                            data.frame(Month = yearmonth(seq(as.Date("2023-04-05"),
                                                             as.Date("2024-03-05"),
                                                             length.out = 12)
                            )))
# as time series
Months_pred_ts <- time(ts(as.Date(Months_pred_df$Month),
                          start = c(1995, 8), freq = 12))


# ===== Results Section ===================
## Part (A) -------------------------------

### Florida ###

# Time series
plot1 <- FL_tsib %>% autoplot(color = "dodgerblue3") +
  labs(title = "FL exports time plot",
       subtitle = "with recession bands",
       y = "Log-millions of $") +
  # NBER recession bands
  annotate(geom = "rect", xmin = recessions$Start[1], xmax = recessions$End[1],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[2], xmax = recessions$End[2],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[3], xmax = recessions$End[3],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  theme_light()

# ACF + PACF
plot2 <- ACF(FL_tsib, lag_max = 36) %>% autoplot() + ggtitle("ACF of FL exports") +
  theme_light() +
  PACF(FL_tsib, lag_max = 36) %>% autoplot() + ggtitle("PACF of FL exports") +
  theme_light()


### Illinois ###

# Time series
plot3 <- IL_tsib %>% autoplot(color = "firebrick4") +
  labs(title = "IL exports time plot",
       subtitle = "with recession bands",
       y = "Log-millions of $") +
  # NBER recession bands
  annotate(geom = "rect", xmin = recessions$Start[1], xmax = recessions$End[1],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[2], xmax = recessions$End[2],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[3], xmax = recessions$End[3],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  theme_light()

# ACF + PACF
plot4 <- ACF(IL_tsib, lag_max = 36) %>% autoplot() + ggtitle("ACF of IL exports") +
  theme_light() +
  PACF(IL_tsib, lag_max = 36) %>% autoplot() + ggtitle("PACF of IL exports") +
  theme_light()


### Combined ###

plot5 <- ggplot() +
  geom_line(data = FL_tsib, aes(x = Month, y = Exp), color = "dodgerblue3") + # Florida
  geom_line(data = IL_tsib, aes(x = Month, y = Exp), color = "firebrick4") + # Illinois
  labs(title = "FL and IL exports time plot",
       subtitle = "FL in blue, IL in red",
       y = "Log-millions of $", x = "Month [1M]") +
  # NBER recession bands
  annotate(geom = "rect", xmin = recessions$Start[1], xmax = recessions$End[1],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[2], xmax = recessions$End[2],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[3], xmax = recessions$End[3],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  theme_light()


## Part (B) -------------------------------

### Florida ###

# STL decomposition
FL_stl <- stl(FL_ts, t.window = 24, s.window = 15, robust = TRUE)

plot6 <- FL_stl %>% autoplot(range.bars = FALSE) +
  ggtitle("FL STL decomposition") + theme_light()


### Illinois ###

# STL decomposition
IL_stl <- stl(IL_ts, t.window = 24, s.window = 15, robust = TRUE)

plot7 <- IL_stl %>% autoplot(range.bars = FALSE) +
  ggtitle("IL STL decomposition") + theme_light()


## Part (C) -------------------------------

### Florida ###

# fitting ARMA model
FL_stl2 <- stl(FL_ts, s.window = 'periodic', robust = TRUE)
FL_resid <- remainder(FL_stl2) # remainder component
FL_arma <- auto.arima(FL_resid,
                      max.P = 0, max.D = 0, max.Q = 0) # no seasonal

# print table of seasonal components
FL_seas <- window(seasonal(FL_stl2), start = c(1996, 1), end = c(1996, 12))
FL_seas_components <- data.frame(month.name, FL_seas) %>%
  mutate_at(vars(month.name), function(x){
    str_sub(x, start = 1, end = 3)
  }) %>%
  column_to_rownames("month.name") %>% t()
rownames(FL_seas_components) <- NULL
FL_seas_components_print <- kable(FL_seas_components, digits = 3,
                                  caption = "FL Seasonal Component")

# print table of cycle components
FL_cycl <- data.frame(FL_arma$coef) %>% 
  rename(Coefficients = FL_arma.coef)  %>% t()
colnames(FL_cycl) <- c("AR(1)", "MA(1)")
FL_cycl_print <- kable(FL_cycl, digits = 4,
                       caption = "FL Cycle Component")

# plot model
FL_fitted <- FL_stl2$time.series[,2] + seasonal(FL_stl2) + FL_arma$fitted # fitted values

plot8 <- ggplot() +
  geom_line(data = FL_tsib, aes(x = Month, y = Exp), lwd = 0.5) +
  geom_line(aes(x = FL_tsib$Month, y = FL_fitted), color = "blue", alpha = 0.7) +
  labs(title = "FL time series and model",
       subtitle = "data in black, model in blue",
       y = "Log-millions of $", x = "Month [1M]") +
  theme_light()


### Illinois ###

# fitting ARMA model
IL_stl2 <- stl(IL_ts, s.window = 'periodic', robust = TRUE)
IL_resid <- remainder(IL_stl2) # remainder component
IL_arma <- auto.arima(IL_resid,
                      max.P = 0, max.D = 0, max.Q = 0) # no seasonal

# print table of seasonal components
IL_seas <- window(seasonal(IL_stl2), start = c(1996, 1), end = c(1996, 12))
IL_seas_components <- data.frame(month.name, IL_seas) %>%
  mutate_at(vars(month.name), function(x){
    str_sub(x, start = 1, end = 3)
  }) %>%
  column_to_rownames("month.name") %>% t()
rownames(IL_seas_components) <- NULL
IL_seas_components_print <- kable(IL_seas_components, digits = 3,
                                  caption = "IL Seasonal Component")

# print table of cycle components
IL_cycl <- data.frame(IL_arma$coef) %>% 
  rename(Coefficients = IL_arma.coef)  %>% t()
colnames(IL_cycl) <- c("AR(1)")
IL_cycl_print <- kable(IL_cycl, digits = 4,
                       caption = "IL Cycle Component")

# plot model
IL_fitted <- IL_stl2$time.series[,2] + seasonal(IL_stl2) + IL_arma$fitted # fitted values

plot9 <- ggplot() +
  geom_line(data = IL_tsib, aes(x = Month, y = Exp), lwd = 0.5) +
  geom_line(aes(x = IL_tsib$Month, y = IL_fitted), color = "red", alpha = 0.7) +
  labs(title = "IL time series and model",
       subtitle = "data in black, model in red",
       y = "Log-millions of $", x = "Month [1M]") +
  theme_light()


## Part (D) -------------------------------

### Florida ###

# data frame with fitted + residuals
FL_res_fitted_df <- data.frame(resid = FL_arma$residuals,
                               fitted = FL_fitted)

# residual plot
plot10 <- ggplot(data = FL_res_fitted_df, aes(y = resid, x = fitted)) +
  geom_point() +
  labs(title = "FL model residual plot", y = "Residuals", x = "Fitted") +
  geom_hline(yintercept = 0, color = "grey30", lty = 2) + theme_light()


### Illinois ###

# data frame with fitted + residuals
IL_res_fitted_df <- data.frame(resid = IL_arma$residuals,
                               fitted = IL_fitted)

# residual plot
plot11 <- ggplot(data = IL_res_fitted_df, aes(y = resid, x = fitted)) +
  geom_point() +
  labs(title = "IL model residual plot", y = "Residuals", x = "Fitted") +
  geom_hline(yintercept = 0, color = "grey30", lty = 2) + theme_light()


## Part (E) -------------------------------

### Florida ###

# ARMA residuals
FL_resid_tsib <-  bind_cols(FL_tsib$Month, FL_arma$residuals) %>%
  rename(Month = "...1", resid = "...2") %>%
  tsibble(index = Month)

# ACF + PACF
plot12 <- ACF(FL_resid_tsib, lag_max = 36) %>% autoplot() + ggtitle("ACF of FL residuals") +
  theme_light() +
  PACF(FL_resid_tsib, lag_max = 36) %>% autoplot() + ggtitle("PACF of FL residuals") +
  theme_light()


### Illinois ###

# ARMA residuals
IL_resid_tsib <-  bind_cols(IL_tsib$Month, IL_arma$residuals) %>%
  rename(Month = "...1", resid = "...2") %>%
  tsibble(index = Month)

# ACF + PACF
plot13 <- ACF(IL_resid_tsib, lag_max = 36) %>% autoplot() + ggtitle("ACF of IL residuals") +
  theme_light() +
  PACF(IL_resid_tsib, lag_max = 36) %>% autoplot() + ggtitle("PACF of IL residuals") +
  theme_light()


## Part (F) -------------------------------

### Florida ###

# CUSUM plot (ggplot)
plot14 <- efp(FL_arma$resid ~ 1, type = "Rec-CUSUM")$process %>% autoplot() + ylim(-3,3) + 
  geom_curve(aes(x = (1995 + 8 /12), y = 1, xend = 2023.25, yend = 3), color = "red3",
             lwd = 0.25, curvature = -0.01) + 
  geom_curve(aes(x = (1995 + 8 /12), y = -1, xend = 2023.25, yend = -3), color = "red3",
             lwd = 0.25, curvature = 0.01) +
  geom_hline(yintercept = 0, color = "grey50", alpha = 0.5) +
  labs(title = "FL recursive CUSUM test",
       y = "Empirical fluctuation process", x = "Time") + theme_light()


### Illinois ###

# CUSUM plot (ggplot)
plot15 <- efp(IL_arma$resid ~ 1, type = "Rec-CUSUM")$process %>% autoplot() + ylim(-3,3) + 
  geom_curve(aes(x = (1995 + 8 /12), y = 1, xend = 2023.25, yend = 3), color = "red3",
             lwd = 0.25, curvature = -0.01) + 
  geom_curve(aes(x = (1995 + 8 /12), y = -1, xend = 2023.25, yend = -3), color = "red3",
             lwd = 0.25, curvature = 0.01) +
  geom_hline(yintercept = 0, color = "grey50", alpha = 0.5) +
  labs(title = "IL recursive CUSUM test",
       y = "Empirical fluctuation process", x = "Time") + theme_light()


## Part (G) -------------------------------

### Florida ###

# diagnostic statistics
FL_MAPE <- MAPE(.resid = FL_resid_tsib$resid, .actual = FL_ts) # MAPE
FL_RMSE <- RMSE(.resid = FL_resid_tsib$resid, .actual = FL_ts) # RMSE
FL_ME <- ME(.resid = FL_resid_tsib$resid) # ME

# Ljung-Box test
FL_ljung <- Box.test(FL_resid_tsib$resid, lag = 12, type = "Ljung-Box")


### Illinois ###

# diagnostic statistics
IL_MAPE <- MAPE(.resid = IL_resid_tsib$resid, .actual = IL_ts) # MAPE
IL_RMSE <- RMSE(.resid = IL_resid_tsib$resid, .actual = IL_ts) # RMSE
IL_ME <- ME(.resid = IL_resid_tsib$resid) # ME

# Ljung-Box test
IL_ljung <- Box.test(IL_resid_tsib$resid, lag = 12, type = "Ljung-Box")


## Part (H) -------------------------------

### Florida ###

# forecasting components
FL_fore_ST <- as.numeric(forecast(FL_stl2, h = 12,
                                  method = "naive")[["mean"]]) # naïve forecast for Trend + Seasonality
FL_fore_arma <- forecast(FL_arma, h = 12) # cycle component forecast

# combining the forecasts into one
FL_fore <- bind_cols(Month = seq(as.Date("2023-04-10"), as.Date("2024-03-10"), length.out = 12),
                     "estimate" = FL_fore_arma[["mean"]], # point estimate
                     FL_fore_arma[["lower"]], # lower error bands
                     FL_fore_arma[["upper"]], FL_fore_ST) %>% # upper error bands
  # error bands
  rename("lower_80" = "80%...3",
         "lower_95" = "95%...4",
         "upper_80" = "80%...5",
         "upper_95" = "95%...6",
         seas_trend = "...7") %>%
  mutate_at(vars(estimate:upper_95), function(x) x + FL_fore_ST) %>%
  dplyr::select(-seas_trend) %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)

FL_fore_ts <- ts(FL_fore, start = c(2023, 4), frequency = 12)[,-1]

# plotting
plot16 <- autoplot(FL_fore_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = FL_fore_ts[,3], ymax = FL_fore_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = FL_fore_ts[,2], ymax = FL_fore_ts[,4]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = FL_fore_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "FL exports forecast",
       subtitle = "with error bands",
       y = "Log-millions of $") +
  geom_line(data = FL_ts) + xlim(2018, NA) + ylim(7.75, NA) +
  theme_light()


### Illinois ###

# forecasting components
IL_fore_ST <- as.numeric(forecast(IL_stl2, h = 12,
                                  method = "naive")[["mean"]]) # naïve forecast for Trend + Seasonality
IL_fore_arma <- forecast(IL_arma, h = 12) # cycle component forecast

# combining the forecasts into one
IL_fore <- bind_cols(Month = seq(as.Date("2023-04-10"), as.Date("2024-03-10"), length.out = 12),
                     "estimate" = IL_fore_arma[["mean"]], # point estimate
                     IL_fore_arma[["lower"]], # lower error bands
                     IL_fore_arma[["upper"]], IL_fore_ST) %>% # upper error bands
  # error bands
  rename("lower_80" = "80%...3",
         "lower_95" = "95%...4",
         "upper_80" = "80%...5",
         "upper_95" = "95%...6",
         seas_trend = "...7") %>%
  mutate_at(vars(estimate:upper_95), function(x) x + IL_fore_ST) %>%
  dplyr::select(-seas_trend) %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)

IL_fore_ts <- ts(IL_fore, start = c(2023, 4), frequency = 12)[,-1]

# plotting
plot17 <- autoplot(IL_fore_ts[,1], color = "orange4") +
  geom_ribbon(aes(ymin = IL_fore_ts[,3], ymax = IL_fore_ts[,5]),
              fill = "orange2", alpha = 0.5) +
  geom_ribbon(aes(ymin = IL_fore_ts[,2], ymax = IL_fore_ts[,4]),
              fill = "orange3", alpha = 0.5) +
  geom_line(aes(y = IL_fore_ts[,1]), color = "orange4", lwd = 0.7) +
  labs(title = "IL exports forecast",
       subtitle = "with error bands",
       y = "Log-millions of $") +
  geom_line(data = IL_ts) + xlim(2018, NA) + ylim(7.75, NA) +
  theme_light()


## Part (I) -------------------------------

### Florida ###

# ARIMA model
FL_arima <- auto.arima(FL_ts)
FL_arima.summary <- summary(FL_arima) # summary output
FL_arima_fcst <-  forecast(FL_arima, h = 12) %>% # forecast
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
FL_arima_fcst_ts <- ts(FL_arima_fcst, start = c(2023, 4), freq = 12)

# plotting
plot18 <- autoplot(FL_arima_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = FL_arima_fcst_ts[,4], ymax = FL_arima_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = FL_arima_fcst_ts[,2], ymax = FL_arima_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = FL_arima_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "FL exports ARIMA forecast",
       subtitle = "with error bands",
       y = "Log-millions of $") +
  geom_line(data = FL_ts) + xlim(2018, NA) +
  theme_light()


### Illinois ###

# ARIMA model
IL_arima <- auto.arima(IL_ts)
IL_arima.summary <- summary(IL_arima) # summary output
IL_arima_fcst <-  forecast(IL_arima, h = 12) %>% # forecast
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
IL_arima_fcst_ts <- ts(IL_arima_fcst, start = c(2023, 4), freq = 12)

# plotting
plot19 <- autoplot(IL_arima_fcst_ts[,1], color = "orange4") +
  geom_ribbon(aes(ymin = IL_arima_fcst_ts[,4], ymax = IL_arima_fcst_ts[,5]),
              fill = "orange2", alpha = 0.5) +
  geom_ribbon(aes(ymin = IL_arima_fcst_ts[,2], ymax = IL_arima_fcst_ts[,3]),
              fill = "orange3", alpha = 0.5) +
  geom_line(aes(y = IL_arima_fcst_ts[,1]), color = "orange4", lwd = 0.7) +
  labs(title = "IL exports ARIMA forecast",
       subtitle = "with error bands",
       y = "Log-millions of $") +
  geom_line(data = IL_ts) + xlim(2018, NA) +
  theme_light()


## Part (J) -------------------------------

### Florida ###

# combining models with regression weighting scheme
FL_combined <- lm(FL_ts ~ FL_fitted + FL_arima$fitted)
FL_combined_resid <- FL_combined$resid

# MAPE comparison
FL_comb_MAPE <- MAPE(.resid = FL_combined_resid, .actual = FL_ts)
FL_arima_MAPE <- MAPE(.resid = FL_arima$residuals, .actual = FL_ts)


### Illinois ###

# combining models with regression weighting scheme
IL_combined <- lm(IL_ts ~ IL_fitted + IL_arima$fitted)
IL_combined_resid <- IL_combined$resid
# MAPE comparison
IL_comb_MAPE <- MAPE(.resid = IL_combined_resid, .actual = IL_ts)
IL_arima_MAPE <- MAPE(.resid = IL_arima$residuals, .actual = IL_ts)


## Part (K) -------------------------------

# Make time series stationary (first-order difference)
FL_diff <- diff(FL_ts)
IL_diff <- diff(IL_ts)
combined_df <- data.frame(cbind(FL_diff, IL_diff))

combined_tsib <- bind_cols(Months_df[-1,], combined_df) %>%
  tsibble(index = Month)

# CCF
plot20 <- CCF(combined_tsib) %>% autoplot() +
  labs(title = "CCF for differenced FL and IL") +
  theme_light()

# VAR model
VAR_order <- VARselect(combined_df, lag.max = 10)$selection
VAR_model <- VAR(combined_df, p = 4)

# VAR information criteria table
VAR_order_print <- kable(t(VAR_order), caption = "VAR lag-order criteria")

# VAR components table
VAR_coefs <- rbind(coef(VAR_model$varresult[["FL_diff"]]),
                   coef(VAR_model$varresult[["IL_diff"]]))
rownames(VAR_coefs) <- c("FL endog",
                         "IL endog")
colnames(VAR_coefs) <- c("FL lag1", "IL lag1",
                         "FL lag2", "IL lag2",
                         "FL lag3", "IL lag3",
                         "FL lag4", "IL lag4",
                         "Const")
VAR_coef_print <- kable(VAR_coefs, caption = "VAR model estimates", digits = 4)

## plot fitted VAR model ##

### Florida ###

# undifference VAR model
VAR_FL <- bind_cols(Month = time(FL_diff)[-(1:4)],
                    x = FL_diff[-(1:4)],
                    fitted = fitted(VAR_model)[,1],
                    resid = residuals(VAR_model)[,1]
                    )

# fitted model and residuals
plot21 <- (ggplot(data = VAR_FL) +
             geom_hline(yintercept = 0, color = "grey30", alpha = 0.7) +
             geom_line(data = VAR_FL, aes(x = Month, y = x)) +
             geom_line(data = VAR_FL, aes(x = Month, y = fitted),
                       color = "blue", alpha = 0.7) +
             labs(title = "FL differenced data and VAR model",
                  subtitle = "data in black, model in blue",
                  y = "Differences", x = "Month [1M]") +
             theme_light() )/
  (ggplot(data = VAR_FL) +
     geom_hline(yintercept = 0, color = "grey30", alpha = 0.7) +
     geom_line(data = VAR_FL, aes(x = Month, y = resid)) +
     labs(title = "FL VAR model residuals",
          y = "Residuals", x = "Month [1M]") +
     theme_light())

# ACF and PACF
plot23 <- ACF(.data = FL_tsib,
              x = acf(x=residuals(VAR_model)[,1], plot = FALSE)$acf,
              lag_max = 25) %>% autoplot() +
  ggtitle("ACF for FL VAR Residuals") + theme_light() +
  PACF(.data = FL_tsib,
       x = acf(x=residuals(VAR_model)[,1], plot = FALSE)$acf,
       lag_max = 25) %>% autoplot() +
  ggtitle("PACF for FL VAR Residuals") + theme_light() 


### Illinois ###

# undifference VAR model
VAR_IL <- bind_cols(Month = time(IL_diff)[-(1:4)],
                    x = IL_diff[-(1:4)],
                    fitted = fitted(VAR_model)[,1],
                    resid = residuals(VAR_model)[,1]
                    )

# fitted model and residuals
plot22 <- (ggplot(data = VAR_IL) +
             geom_hline(yintercept = 0, color = "grey30", alpha = 0.7) +
             geom_line(data = VAR_IL, aes(x = Month, y = x)) +
             geom_line(data = VAR_IL, aes(x = Month, y = fitted),
                       color = "red", alpha = 0.7) +
             labs(title = "IL differenced data and VAR model",
                  subtitle = "data in black, model in red",
                  y = "Differences", x = "Month [1M]") +
             theme_light() )/
  (ggplot(data = VAR_IL) +
     geom_hline(yintercept = 0, color = "grey30", alpha = 0.7) +
     geom_line(data = VAR_IL, aes(x = Month, y = resid)) +
     labs(title = "IL VAR model residuals",
          y = "Residuals", x = "Month [1M]") +
     theme_light())

# ACF and PACF
plot24 <- ACF(.data = IL_tsib,
              x = acf(x=residuals(VAR_model)[,2], plot = FALSE)$acf,
              lag_max = 25) %>% autoplot() +
  ggtitle("ACF for IL VAR Residuals") + theme_light() +
  PACF(.data = IL_tsib,
       x = acf(x=residuals(VAR_model)[,2], plot = FALSE)$acf,
       lag_max = 25) %>% autoplot() +
  ggtitle("PACF for IL VAR Residuals") + theme_light() 


## Part (L) -------------------------------

# IRF of VAR model
combined_IRF <- irf(VAR_model, n.ahead = 12,
                    runs = 200, boot = TRUE)

# shock to FL exports data frame
IRF_FL <- data.frame(
  x = 0:12,
  irf.fl = combined_IRF$irf[["FL_diff"]][,1],
  irf.il = combined_IRF$irf[["FL_diff"]][,2],
  lower.fl = combined_IRF$Lower[["FL_diff"]][,1],
  lower.il = combined_IRF$Lower[["FL_diff"]][,2],
  upper.fl = combined_IRF$Upper[["FL_diff"]][,1],
  upper.il = combined_IRF$Upper[["FL_diff"]][,2]
)

# shock to IL exports data frame
IRF_IL <- data.frame(
  x = 0:12,
  irf.fl = combined_IRF$irf[["IL_diff"]][,1],
  irf.il = combined_IRF$irf[["IL_diff"]][,2],
  lower.fl = combined_IRF$Lower[["IL_diff"]][,1],
  lower.il = combined_IRF$Lower[["IL_diff"]][,2],
  upper.fl = combined_IRF$Upper[["IL_diff"]][,1],
  upper.il = combined_IRF$Upper[["IL_diff"]][,2]
)

# IRF for FL shock (data prep)
IRF_FL_long <- IRF_FL %>%
  pivot_longer(cols = !x,
               names_to = c("interval", "state"),
               names_sep = "\\.",
               values_to = "value") %>%
  pivot_wider(values_from = "value",
              names_from = "interval") %>%
  mutate_at(vars(state), toupper)

# plot IRF
plot25 <- ggplot(data = IRF_FL_long, aes(x = x)) +
  facet_grid(rows = vars(state)) +
  labs(title = "Orthogonal Impulse Response from FL exports",
       subtitle = "95% Bootstrap CI, 200 runs",
       x = NULL, y = "IRF") +
  geom_hline(yintercept = 0, color = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "indianred", alpha = 0.5) +
  geom_line(aes(y = irf), color = "indianred4") +
  theme_light()

# IRF for IL shock (data prep)
IRF_IL_long <- IRF_IL %>%
  pivot_longer(cols = !x,
               names_to = c("interval", "state"),
               names_sep = "\\.",
               values_to = "value") %>%
  pivot_wider(values_from = "value",
              names_from = "interval") %>%
  mutate_at(vars(state), toupper)

# plot IRF
plot26 <- ggplot(data = IRF_IL_long, aes(x = x)) +
  facet_grid(rows = vars(state)) +
  labs(title = "Orthogonal Impulse Response from IL exports",
       subtitle = "95% Bootstrap CI, 200 runs",
       x = NULL, y = "IRF") +
  geom_hline(yintercept = 0, color = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "indianred", alpha = 0.5) +
  geom_line(aes(y = irf), color = "indianred4") +
  theme_light()


## Part (M) -------------------------------

il_cause_fl <- grangertest(FL_ts ~ IL_ts, order = 4) # IL causing FL
fl_cause_il <- grangertest(IL_ts ~ FL_ts, order = 4) # FL causing IL

## Part (N) -------------------------------

### Florida ###

# VAR forecast
FL_VAR_fcst <- data.frame(
  Month = yearmonth(seq(as.Date("2023-04-05"), as.Date("2024-03-05"), length.out = 12)),
  forecast = predict(VAR_model, n.ahead = 12, ci = 0.8)$fcst[["FL_diff"]][,1],
  lower.80 = predict(VAR_model, n.ahead = 12, ci = 0.8)$fcst[["FL_diff"]][,2],
  upper.80 = predict(VAR_model, n.ahead = 12, ci = 0.8)$fcst[["FL_diff"]][,3],
  lower.95 = predict(VAR_model, n.ahead = 12, ci = 0.95)$fcst[["FL_diff"]][,2],
  upper.95 = predict(VAR_model, n.ahead = 12, ci = 0.95)$fcst[["FL_diff"]][,3]
)
FL_VAR_fcst[1,c(2:6)] <- FL_VAR_fcst[1,c(2:6)] + FL_ts[length(FL_ts)]
FL_VAR_fcst <- FL_VAR_fcst %>%
  mutate_at(vars(forecast:upper.95), cumsum) # convert to undifferenced data

# as time series
FL_VAR_fcst_ts <- ts(FL_VAR_fcst[,-1], start = c(2023, 4), freq = 12)

# plotting
plot27 <- autoplot(FL_VAR_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = FL_VAR_fcst_ts[,4], ymax = FL_VAR_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = FL_VAR_fcst_ts[,2], ymax = FL_VAR_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = FL_VAR_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "FL exports VAR forecast",
       subtitle = "with error bands",
       y = "Log-millions of $") +
  geom_line(data = FL_ts) + xlim(2018, NA) +
  theme_light()


### Illinois ###

# VAR forecast
IL_VAR_fcst <- data.frame(
  Month = yearmonth(seq(as.Date("2023-04-05"), as.Date("2024-03-05"), length.out = 12)),
  forecast = predict(VAR_model, n.ahead = 12, ci = 0.8)$fcst[["IL_diff"]][,1],
  lower.80 = predict(VAR_model, n.ahead = 12, ci = 0.8)$fcst[["IL_diff"]][,2],
  upper.80 = predict(VAR_model, n.ahead = 12, ci = 0.8)$fcst[["IL_diff"]][,3],
  lower.95 = predict(VAR_model, n.ahead = 12, ci = 0.95)$fcst[["IL_diff"]][,2],
  upper.95 = predict(VAR_model, n.ahead = 12, ci = 0.95)$fcst[["IL_diff"]][,3]
)
IL_VAR_fcst[1,c(2:6)] <- IL_VAR_fcst[1,c(2:6)] + IL_ts[length(IL_ts)]
IL_VAR_fcst <- IL_VAR_fcst %>%
  mutate_at(vars(forecast:upper.95), cumsum) # convert to undifferenced data

# as time series
IL_VAR_fcst_ts <- ts(IL_VAR_fcst[,-1], start = c(2023, 4), freq = 12)

# plotting
plot28 <- autoplot(IL_VAR_fcst_ts[,1], color = "orange4") +
  geom_ribbon(aes(ymin = IL_VAR_fcst_ts[,4], ymax = IL_VAR_fcst_ts[,5]),
              fill = "orange2", alpha = 0.5) +
  geom_ribbon(aes(ymin = IL_VAR_fcst_ts[,2], ymax = IL_VAR_fcst_ts[,3]),
              fill = "orange3", alpha = 0.5) +
  geom_line(aes(y = IL_VAR_fcst_ts[,1]), color = "orange4", lwd = 0.7) +
  labs(title = "IL exports VAR forecast",
       subtitle = "with error bands",
       y = "Log-millions of $") +
  geom_line(data = IL_ts) + xlim(2018, NA) +
  theme_light()



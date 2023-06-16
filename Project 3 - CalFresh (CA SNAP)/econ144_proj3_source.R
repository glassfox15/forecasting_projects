# ~~~~~~~ Project 3 ~~~~~~~
# Jacob Titcomb
# UCLA Economics 144
# Professor: Randall Rojas
# Spring 2023
# ~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(knitr)
library(scales)
library(lubridate)
library(patchwork)
library(fpp3)
library(tsibble)
library(seasonal)
library(fabletools)
library(feasts)
library(forecast)
library(strucchange)
library(lmtest)
library(vars)
library(prophet)
library(rugarch)

# ===== Initial Data Cleaning ====================

# Import the data
CA_raw <- read_csv("CA_SNAP.csv")
CA_raw <- CA_raw %>%
  rename(Month = DATE,
         raw = BRCA06M647NCEN)

# Change scale of "Persons"
CA_raw <- CA_raw %>%
  mutate(Persons = c(NA, diff(raw))) %>%                  # differencing
  mutate_at(vars(Persons), function(x) x / 1000) %>%      # scale of 1000s of persons
  dplyr::select(Month, Persons) %>%
  filter(!is.na(Persons))

# Set aside testing set
## Jul 2016 - Jun 2021
test_df <- tail(CA_raw, 60)                             
test_tsib <- test_df %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)
test_ts <- ts(test_df, start = c(2016, 7), freq = 12)[,-1]

# Training set
## Feb 1981 - Jun 2016
CA_df <- head(CA_raw, -60)
CA_tsib <- CA_df %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)
CA_ts <- ts(CA_df, start = c(1981, 2), freq = 12)[,-1]

# Months being used
Months_df <- CA_df[,1]


# ~~~ Washington data set ~~~

# Importing and transforming
WA_raw <- read_csv("WA_SNAP.csv")
WA_raw <- WA_raw %>%
  rename(Month = DATE,
         raw = BRWA53M647NCEN) %>%
  mutate(Persons = c(NA, diff(raw))) %>%                  # differencing
  mutate_at(vars(Persons), function(x) x / 1000) %>%      # scale of 1000s of persons
  dplyr::select(Month, Persons) %>%
  filter(!is.na(Persons))

## Feb 1981 - Jun 2016
WA_df <- head(WA_raw, -60)
WA_ts <- ts(WA_df, start = c(1981, 2), freq = 12)[,-1]



# ===== Auxiliary Code ====================

# +++++++++++++++++++++++++++++++++++++++++
# These functions help facilitating plotting using ggplot2.
# They have their counterparts without ggplot2, but I wanted
# the ability to customize/work with multiple object types
# and not just ts objects. I also think it looks nicer.
# +++++++++++++++++++++++++++++++++++++++++


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
recessions <- recession_df[30:33,]                # save as time series

## Uncomment for plotting
# annotate(geom = "rect", xmin = recessions$Start[1], xmax = recessions$End[1],
#        ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
# annotate(geom = "rect", xmin = recessions$Start[2], xmax = recessions$End[2],
#        ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
# annotate(geom = "rect", xmin = recessions$Start[3], xmax = recessions$End[3],
#        ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25)


## CheckResiduals function ----------------

### +++++++++++++++++++++++++++++++++++++++
# Analogous to forecast::checkresiduals().
# Outputs residual time series plot, residual ACF,
# and residual PACF.
### +++++++++++++++++++++++++++++++++++++++

myCheckResiduals <- function(model, name = NULL, residuals = FALSE){
  resid_plt_title <- paste(name, "Model Residual Plot")
  ACF_plt_title <- paste("ACF of", name, "Model")
  PACF_plt_title <- paste("PACF of", name, "Model")
  
  # residuals
  if (residuals){
    resids <- model # when just residuals
  } else {
    resids <- model[['residuals']] # when model has residual attribute
  }
  
  months <- CA_tsib[,'Month']
  if (length(resids) != length(months[[1]])) { # need lengths to agree
    shortest <- min(length(resids), length(months[[1]]))
    resids <- resids[1:shortest]
    months <- months[1:shortest,]
  }
  
  resid_tsib <- bind_cols(months, resids) %>%
    rename(resid = "...2")
  
  # plotting
  ## residual time plot
  (ggplot(data = resid_tsib, aes(y = resids, x = Month)) +
      geom_line() +
      labs(title = resid_plt_title, y = "Residuals", x = "Time") +
      geom_hline(yintercept = 0, color = "grey30", lty = 2) + theme_light()) /
    ## ACF
    (ACF(resid_tsib, lag_max = 36) %>% autoplot() +
       ggtitle(ACF_plt_title) +
       theme_light() +
       ## PACF
       PACF(resid_tsib, lag_max = 36) %>% autoplot() +
       ggtitle(PACF_plt_title) +
       theme_light())
}


## CUSUM function -------------------------

### +++++++++++++++++++++++++++++++++++++++
# Analogous to `plot(strucchange::efc(, type = "Rec-CUSUM"))`.
# Outputs residual CUSUM plot with significance boundaries.
### +++++++++++++++++++++++++++++++++++++++

myCUSUM <- function(model, name = NULL, residuals = FALSE){
  plt.title <- paste(name, "Recursive CUSUM Test")
  
  if (residuals){
    resids <- model
  } else {
    resids <- model[['residuals']]
  }
  
  # plotting
  efp(resids ~ 1, type = "Rec-CUSUM")$process %>% autoplot() + ylim(-3,3) +
    geom_curve(aes(x = (1981 + 2 /12), y = 1, xend = 2016.5, yend = 3), color = "red3",
               lwd = 0.25, curvature = -0.01) +
    geom_curve(aes(x = (1981 + 2 /12), y = -1, xend = 2016.5, yend = -3), color = "red3",
               lwd = 0.25, curvature = 0.01) +
    geom_hline(yintercept = 0, color = "grey50", alpha = 0.5) +
    labs(title = plt.title,
         y = "Empirical fluctuation process", x = "Time") + theme_light()
}


## Accuracy function ------------

myAccuracy <- function(model, residuals = FALSE, test = FALSE, vec = FALSE) {
  # test: whether accuracy wrt to testing set (as opposed to training)
  # vec: TRUE returns a vector, FALSE prints the values
  
  if (residuals){
    resids <- model
  } else {
    resids <- model[['residuals']]
  }
  
  if (test) {
    data <- test_ts
  } else {
    data <- CA_ts
  }
  
  if (length(resids) != length(data)) {
    shortest <- min(length(resids), length(data))
    resids <- resids[1:shortest]
    data <- data[1:shortest]
  }
  
  if (vec) {
    return(c(MAE(.resid = resids, .actual = data),
             RMSE(.resid = resids, .actual = data),
             ME(.resid = resids)))
  }
  if (!test) {
    cat("--- Training Dataset ---")
  } else cat("--- Testing Dataset ---")
  cat(sep = "",
      "\nMAE:  ", MAE(.resid = resids, .actual = data),
      "\nRMSE: ", RMSE(.resid = resids, .actual = data),
      "\nME:   ", ME(.resid = resids), "\n")
  if (!test) Box.test(resids, lag = 24, type = "Ljung")
}


# ===== Results Section ===================

plot1 <- CA_tsib %>% autoplot(Persons, color = "dodgerblue3") +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "with recession bands",
       y = "Persons (Thousands)",
       x = "Time") +
  geom_hline(yintercept = 0, color = "grey10", lty = 2, alpha = 0.5) +
  # NBER recession bands
  annotate(geom = "rect", xmin = recessions$Start[1], xmax = recessions$End[1],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[2], xmax = recessions$End[2],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[3], xmax = recessions$End[3],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  annotate(geom = "rect", xmin = recessions$Start[4], xmax = recessions$End[4],
           ymin = -Inf, ymax = Inf, fill = "grey30", alpha = 0.25) +
  theme_light()

plot2 <- ACF(CA_tsib, lag_max = 48) %>% autoplot() + ggtitle("ACF of Change in CA SNAP") +
  theme_light() +
  PACF(CA_tsib, lag_max = 48) %>% autoplot() + ggtitle("PACF of Change in CA SNAP") +
  theme_light()

CA_stl <- stl(CA_ts, robust = TRUE,
              s.window = 7, t.window = 15)

plot3 <- CA_stl %>% autoplot() +
  labs(title = "STL Decomposition of Change in CA SNAP") +
  theme_light()



# fitting ARMA model
CA_stl_rand <- CA_stl[['time.series']][,'remainder']
CA_stl_ARMA <- auto.arima(CA_stl_rand,
                          max.d = 0, max.P = 0, max.D = 0, max.Q = 0)

# print table of seasonal components
CA_seas_comp <- bind_cols(Month = time(seasonal(CA_stl)), Seas = seasonal(CA_stl)) %>%
  mutate_at(vars(Month), function(x) round(x %% 1, digits = 4)) %>%
  group_by(Month) %>%
  summarise('Seasonal Component' = mean(Seas)) %>%
  bind_cols(month.name = month.name) %>%
  mutate_at(vars(month.name), function(x){
    str_sub(x, start = 1, end = 3)
  }) %>%
  dplyr::select('month.name', 'Seasonal Component') %>%
  column_to_rownames("month.name") %>% t()
rownames(CA_seas_comp) <- NULL

# print table of cycle components
CA_cycl <- data.frame(CA_stl_ARMA$coef) %>%
  rename(Coefficients = CA_stl_ARMA.coef)  %>% t()
colnames(CA_cycl) <- c("MA(1)")


# fitted values
CA_stl_arma_fitted <- CA_stl$time.series[,2] + seasonal(CA_stl) + CA_stl_ARMA$fitted

# data frame with fitted + residuals
CA_stl_arma_resid_fitted_df <- data.frame(resid = CA_stl_ARMA$residuals,
                                          fitted = CA_stl_arma_fitted)
# plot model
plot4 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = CA_stl_arma_fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, STL + ARMA model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()


# forecasting components
CA_stl_fcst <- as.numeric(forecast(CA_stl, h = 60,
                                   method = "naive")[["mean"]]) # naïve forecast for Trend + Seasonality
CA_stl_ARMA_fcst <- forecast(CA_stl_ARMA, h = 60) # cycle component forecast

# combining the forecasts into one
CA_mod1_fcst <- bind_cols(Month = seq(as.Date("2016-07-10"), as.Date("2021-06-10"), length.out = 60),
                          "estimate" = CA_stl_ARMA_fcst[["mean"]], # point estimate
                          CA_stl_ARMA_fcst[["lower"]], # lower error bands
                          CA_stl_ARMA_fcst[["upper"]], CA_stl_fcst) %>% # upper error bands
  # error bands
  rename("lower_80" = "80%...3",
         "lower_95" = "95%...4",
         "upper_80" = "80%...5",
         "upper_95" = "95%...6",
         seas_trend = "...7") %>%
  mutate_at(vars(estimate:upper_95), function(x) x + CA_stl_fcst) %>%
  dplyr::select(-seas_trend) %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)

CA_mod1_fcst_ts <- ts(CA_mod1_fcst, start = c(2016, 7), frequency = 12)[,-1]

# plotting
plot5 <- autoplot(CA_mod1_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_mod1_fcst_ts[,3], ymax = CA_mod1_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_mod1_fcst_ts[,2], ymax = CA_mod1_fcst_ts[,4]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_mod1_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: STL + ARMA",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

mod1_resid <- test_ts - CA_mod1_fcst_ts[,'estimate']


# fitting ARIMA model
CA_ARIMA <- auto.arima(CA_ts)

# plot model
plot6 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = CA_ARIMA$fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, ARIMA model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()

# forecasting
CA_ARIMA_fcst <-  forecast(CA_ARIMA, h = 60) %>%
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
CA_ARIMA_fcst_ts <- ts(CA_ARIMA_fcst, start = c(2016, 7), freq = 12)

# plotting
plot8 <- autoplot(CA_ARIMA_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_ARIMA_fcst_ts[,4], ymax = CA_ARIMA_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_ARIMA_fcst_ts[,2], ymax = CA_ARIMA_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_ARIMA_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: ARIMA",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()


mod2_resid <- test_ts - CA_ARIMA_fcst_ts[,'forecast']




# fitting ETS model
CA_ETS <- ets(CA_ts, lambda = "auto", damped = FALSE)

# plot model
plot9 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = CA_ETS$fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, ETS(A,N,A) model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()


# forecasting
CA_ETS_fcst <-  forecast(CA_ETS, h = 60) %>%
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
CA_ETS_fcst_ts <- ts(CA_ETS_fcst, start = c(2016, 7), freq = 12)

# plotting
plot10 <- autoplot(CA_ETS_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_ETS_fcst_ts[,4], ymax = CA_ETS_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_ETS_fcst_ts[,2], ymax = CA_ETS_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_ETS_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: ETS",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()


mod3_resid <- test_ts - CA_ETS_fcst_ts[,'forecast']


# fit Holt-Winters model
CA_HW <- hw(CA_ts, seasonal = "additive", damped = FALSE, h = 60)

# plot model
plot11 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = CA_HW$fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, Holt-Winters model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()

# forecasting
CA_HW_fcst <-  forecast(CA_HW, h = 60) %>%
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
CA_HW_fcst_ts <- ts(CA_HW_fcst, start = c(2016, 7), freq = 12)

# plotting
plot12 <- autoplot(CA_HW_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_HW_fcst_ts[,4], ymax = CA_HW_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_HW_fcst_ts[,2], ymax = CA_HW_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_HW_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: Holt-Winters",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

mod4_resid <- test_ts - CA_HW_fcst_ts[,'forecast']


# fitting TBATS model
CA_TBATS <- tbats(CA_ts)

# plot model
plot13 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = CA_TBATS$fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, TBATS model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()

# forecasting
CA_TBATS_fcst <-  forecast(CA_TBATS, h = 60) %>%
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
CA_TBATS_fcst_ts <- ts(CA_TBATS_fcst, start = c(2016, 7), freq = 12)

# plotting
plot14 <- autoplot(CA_TBATS_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_TBATS_fcst_ts[,4], ymax = CA_TBATS_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_TBATS_fcst_ts[,2], ymax = CA_TBATS_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_TBATS_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: TBATS",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

mod5_resid <- test_ts - CA_TBATS_fcst_ts[,'forecast']


### VAR MODEL
CA_WA <- cbind(CA = CA_ts, WA = WA_ts)
CA_WA_tsib <- bind_cols(Months_df, CA_WA) %>%
  mutate_at(vars(Month), yearmonth) %>%
  tsibble(index = Month)

plot15 <- CCF(CA_WA_tsib) %>% autoplot() +
  labs(title = "CCF for Changes to CA and WA SNAP") +
  ylim(0, NA) +
  theme_light()

# finding order of VAR model
VAR_order <- VARselect(CA_WA)$selection
names(VAR_order) <- c("AIC", "HQ", "BIC", "FPE")


# fitting VAR model
VAR_model <- VAR(CA_WA, p = 2)
CA_VAR <- VAR_model[['varresult']][['CA']]

# CA VAR coefficient table
CA_VAR_coef <- data.frame(VAR_model[['varresult']][['CA']]$coef) %>%
  rename(Coefficients = VAR_model...varresult......CA....coef)  %>% t()

colnames(CA_VAR_coef) <- c("CA L1", "WA L1", "CA L2", "WA L2", "Const")


# WA VAR coefficient table
WA_VAR_coef <- data.frame(VAR_model[['varresult']][['WA']]$coef) %>%
  rename(Coefficients = VAR_model...varresult......WA....coef)  %>% t()

colnames(WA_VAR_coef) <- c("CA L1", "WA L1", "CA L2", "WA L2", "Const")

# fitted VAR values as time series
CA_VAR_fitted <- ts(CA_VAR$fitted.values, start = start(CA_ts), freq = 12)

# plot model
plot16 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = head(CA_tsib$Month, -2), y = CA_VAR_fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, VAR model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()

# fitted VAR values as time series
CA_VAR_resid <- ts(CA_VAR$resid, start = start(CA_ts), freq = 12)

# calculating IRF
combined_IRF <- irf(VAR_model, n.ahead = 12,
                    runs = 200, boot = TRUE)

# shock to CA data frame
IRF_CA <- data.frame(
  x = 0:12,
  irf.ca = combined_IRF$irf[["CA"]][,'CA'],
  irf.wa = combined_IRF$irf[["CA"]][,'WA'],
  lower.ca = combined_IRF$Lower[["CA"]][,'CA'],
  lower.wa = combined_IRF$Lower[["CA"]][,'WA'],
  upper.ca = combined_IRF$Upper[["CA"]][,'CA'],
  upper.wa = combined_IRF$Upper[["CA"]][,'WA']
)

# shock to WA data frame
IRF_WA <- data.frame(
  x = 0:12,
  irf.ca = combined_IRF$irf[["WA"]][,'CA'],
  irf.wa = combined_IRF$irf[["WA"]][,'WA'],
  lower.ca = combined_IRF$Lower[["WA"]][,'CA'],
  lower.wa = combined_IRF$Lower[["WA"]][,'WA'],
  upper.ca = combined_IRF$Upper[["WA"]][,'CA'],
  upper.wa = combined_IRF$Upper[["WA"]][,'WA']
)

# IRF for CA shock (data prep)
IRF_CA_long <- IRF_CA %>%
  pivot_longer(cols = !x,
               names_to = c("interval", "state"),
               names_sep = "\\.",
               values_to = "value") %>%
  pivot_wider(values_from = "value",
              names_from = "interval") %>%
  mutate_at(vars(state), toupper)

# IRF for WA shock (data prep)
IRF_WA_long <- IRF_WA %>%
  pivot_longer(cols = !x,
               names_to = c("interval", "state"),
               names_sep = "\\.",
               values_to = "value") %>%
  pivot_wider(values_from = "value",
              names_from = "interval") %>%
  mutate_at(vars(state), toupper)

# plot IRF
plot17 <- ggplot(data = IRF_CA_long, aes(x = x)) +
  facet_grid(rows = vars(state)) +
  labs(title = "Orthogonal Impulse Response: CA",
       subtitle = "95% Bootstrap CI, 200 runs",
       x = NULL, y = "IRF") +
  geom_hline(yintercept = 0, color = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "indianred", alpha = 0.5) +
  geom_line(aes(y = irf), color = "indianred4") +
  theme_light() +
  ggplot(data = IRF_WA_long, aes(x = x)) +
  facet_grid(rows = vars(state)) +
  labs(title = "Orthogognal Impulse Response: WA",
       subtitle = "95% Bootstrap CI, 200 runs",
       x = NULL, y = "IRF") +
  geom_hline(yintercept = 0, color = "grey20", alpha = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "indianred", alpha = 0.5) +
  geom_line(aes(y = irf), color = "indianred4") +
  theme_light()


# forecasting
CA_VAR_fcst <- data.frame(
  Month = yearmonth(seq(as.Date("2016-07-10"), as.Date("2021-06-10"), length.out = 60)),
  forecast = predict(VAR_model, n.ahead = 60, ci = 0.8)$fcst[["CA"]][,'fcst'],
  lower.80 = predict(VAR_model, n.ahead = 60, ci = 0.8)$fcst[["CA"]][,'lower'],
  upper.80 = predict(VAR_model, n.ahead = 60, ci = 0.8)$fcst[["CA"]][,'upper'],
  lower.95 = predict(VAR_model, n.ahead = 60, ci = 0.95)$fcst[["CA"]][,'lower'],
  upper.95 = predict(VAR_model, n.ahead = 60, ci = 0.95)$fcst[["CA"]][,'upper']
)

# as time series
CA_VAR_fcst_ts <- ts(CA_VAR_fcst[,-1], start = start(test_ts), freq = 12)

# plotting
plot18 <- autoplot(CA_VAR_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_VAR_fcst_ts[,4], ymax = CA_VAR_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_VAR_fcst_ts[,2], ymax = CA_VAR_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_VAR_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: VAR",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

mod6_resid <- test_ts - CA_VAR_fcst_ts[,'forecast']



## PROPHET MODEL

# initialize data frames
CA_Proph_df <- CA_df %>%
  rename(ds = Month, y = Persons)

test_Proph_df <- test_df %>%
  dplyr::select(Month) %>%
  rename(ds = Month)

# create Prophet model
CA_Proph <- prophet()
CA_Proph <- add_seasonality(CA_Proph,
                            name = "monthly", period = 30.5,
                            fourier.order = 49)

# fit model to data
CA_Proph <- fit.prophet(CA_Proph, CA_Proph_df)

CA_Proph_fitted <- predict(CA_Proph)$yhat

# plot model
plot19 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = CA_Proph_fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, Prophet model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()

# residuals
CA_Proph_resid <- CA_ts - CA_Proph_fitted

# forecasting
CA_Proph_fcst.80 <- predict(CA_Proph, test_Proph_df, interval_width = 0.8)
CA_Proph_fcst.95 <- predict(CA_Proph, test_Proph_df, interval_width = 0.95)

# combining error bands
CA_Proph_fcst <- ts(cbind(forecast = CA_Proph_fcst.80$yhat,
                          lower.80 = CA_Proph_fcst.80$yhat_lower,
                          upper.80 = CA_Proph_fcst.80$yhat_upper,
                          lower.95 = CA_Proph_fcst.95$yhat_lower,
                          upper.95 = CA_Proph_fcst.95$yhat_upper),
                    start = start(test_ts), frequency = 12)

# plotting
plot20 <- autoplot(CA_Proph_fcst[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_Proph_fcst[,4], ymax = CA_Proph_fcst[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_Proph_fcst[,2], ymax = CA_Proph_fcst[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_Proph_fcst[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: Prophet",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

mod7_resid <- test_ts - CA_Proph_fcst[,'forecast']


## NNETAR MODEL

# fitting model
CA_NNET <- nnetar(CA_ts, size = 3)

# plot model
plot21 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = CA_NNET$fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, NNETAR model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()

# plotting CUSUM
plot22 <- ts(efp(CA_NNET[['residuals']] ~ 1, type = "Rec-CUSUM")$process,
             start = start(CA_ts), freq = 12) %>%
  autoplot() + ylim(-3,3) +
  geom_curve(aes(x = (1981 + 2 /12), y = 1, xend = 2016.5, yend = 3), color = "red3",
             lwd = 0.25, curvature = -0.01) +
  geom_curve(aes(x = (1981 + 2 /12), y = -1, xend = 2016.5, yend = -3), color = "red3",
             lwd = 0.25, curvature = 0.01) +
  geom_hline(yintercept = 0, color = "grey50", alpha = 0.5) +
  labs(title = "NNETAR Recursive CUSUM Test",
       y = "Empirical fluctuation process", x = "Time") + theme_light()


# forecasting
CA_NNET_fcst <-  forecast(CA_NNET, h = 60, PI = TRUE, level = c(80, 95)) %>%
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
CA_NNET_fcst_ts <- ts(CA_NNET_fcst, start = c(2016, 7), freq = 12)

# plotting
plot23 <- autoplot(CA_NNET_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = CA_NNET_fcst_ts[,4], ymax = CA_NNET_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = CA_NNET_fcst_ts[,2], ymax = CA_NNET_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = CA_NNET_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: NNETAR",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

mod8_resid <- test_ts - CA_NNET_fcst_ts[,'forecast']


## COMBINED MODEL

# fixing missing values
CA_NNET_fitted <- CA_NNET$fitted
CA_NNET_fitted[is.na(CA_NNET$fitted)] <- 0

# fixing names
CA_ARIMA_fitted <- CA_ARIMA$fitted
CA_ETS_fitted <- CA_ETS$fitted
CA_HW_fitted <- CA_HW$fitted
CA_TBATS_fitted <- CA_TBATS$fitted.values
CA_VAR_fitted2 <- c(CA_VAR_fitted, 0, 0)

combined_lm <- lm(CA_ts ~
                    CA_stl_arma_fitted +
                    CA_ARIMA_fitted +
                    CA_ETS_fitted +
                    CA_HW_fitted +
                    CA_TBATS_fitted +
                    CA_VAR_fitted2 +
                    CA_Proph_fitted +
                    CA_NNET_fitted)

# print table of weights
combined_lm_coef <- data.frame(combined_lm$coefficients) %>%
  rename(Coefficients = combined_lm.coefficients)  %>% t()
combined_lm_coef <- combined_lm_coef[,-1]
names(combined_lm_coef) <- c("STL+ARMA", "ARIMA", "ETS", "Holt-Winters",
                             "TBATS", "VAR", "Prophet", "NNETAR")
combined_lm_coef <- t(combined_lm_coef)

# plot model
plot24 <- ggplot() +
  geom_line(data = CA_tsib, aes(x = Month, y = Persons), lwd = 0.5) +
  geom_line(aes(x = CA_tsib$Month, y = combined_lm$fitted), color = "blue", alpha = 0.7) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, Combined model in blue",
       y = "Persons (Thousands)", x = "Time") +
  theme_light()


# plotting CUSUM
plot25 <- ts(efp(combined_lm$resid ~ 1, type = "Rec-CUSUM")$process,
             start = start(CA_ts), freq = 12) %>%
  autoplot() + ylim(-3,3) +
  geom_curve(aes(x = (1981 + 2 /12), y = 1, xend = 2016.5, yend = 3), color = "red3",
             lwd = 0.25, curvature = -0.01) +
  geom_curve(aes(x = (1981 + 2 /12), y = -1, xend = 2016.5, yend = -3), color = "red3",
             lwd = 0.25, curvature = 0.01) +
  geom_hline(yintercept = 0, color = "grey50", alpha = 0.5) +
  labs(title = "Combined Recursive CUSUM Test",
       y = "Empirical fluctuation process", x = "Time") + theme_light()

# more data preparation
CA_stl_ARMA_fcst_df <-  CA_stl_ARMA_fcst %>%
  data.frame() %>%
  rename(forecast = Point.Forecast,
         lower.80 = Lo.80,
         upper.80 = Hi.80,
         lower.95 = Lo.95,
         upper.95 = Hi.95)
CA_Proph_fcst_df <- data.frame(CA_Proph_fcst)
rownames(CA_Proph_fcst_df) <- rownames(CA_NNET_fcst)
CA_VAR_fcst_df <- CA_VAR_fcst %>% dplyr::select(-Month)
rownames(CA_VAR_fcst_df) <- rownames(CA_NNET_fcst)

# combine into one data frame
combined_input_df <- bind_cols(
  "CA_stl_arma_fitted" = CA_stl_ARMA_fcst_df[["forecast"]],
  'CA_ARIMA_fitted' = CA_ARIMA_fcst[["forecast"]],
  "CA_ETS_fitted" = CA_ETS_fcst[["forecast"]],
  "CA_HW_fitted" = CA_HW_fcst[["forecast"]],
  "CA_TBATS_fitted" = CA_TBATS_fcst[["forecast"]],
  "CA_VAR_fitted2" = CA_VAR_fcst_df[["forecast"]],
  "CA_Proph_fitted" = CA_Proph_fcst_df[["forecast"]],
  "CA_NNET_fitted" = CA_NNET_fcst[["forecast"]])

# forecasting
Combined_fcst <- predict(combined_lm, newdata = combined_input_df,
                         interval = "prediction", level = 0.80) %>%
  data.frame() %>%
  rename(forecast = fit,
         lower.80 = lwr,
         upper.80 = upr) %>%
  bind_cols(data.frame(predict(combined_lm,
                               newdata = combined_input_df,
                               interval = "prediction",
                               level = 0.95)) %>%
              rename(lower.95 = lwr,
                     upper.95 = upr) %>%
              dplyr::select(lower.95, upper.95))
Combined_fcst_ts <- ts(Combined_fcst, start = c(2016, 7), freq = 12)

# plotting
plot26 <- autoplot(Combined_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = Combined_fcst_ts[,4], ymax = Combined_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = Combined_fcst_ts[,2], ymax = Combined_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = Combined_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: Combined",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

mod9_resid <- test_ts - Combined_fcst_ts[,'forecast']


## Model comparison
comparison_matrix <- bind_cols(
  "STL+ARMA" = myAccuracy(mod1_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "ARIMA" = myAccuracy(mod2_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "ETS" = myAccuracy(mod3_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "Holt-Winters" = myAccuracy(mod4_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "TBATS" = myAccuracy(mod5_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "VAR" = myAccuracy(mod6_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "Prophet" = myAccuracy(mod7_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "NNETAR" = myAccuracy(mod8_resid, residuals = TRUE, test = TRUE, vec = TRUE),
  "Combined" = myAccuracy(mod9_resid, residuals = TRUE, test = TRUE, vec = TRUE)
) %>% t()
colnames(comparison_matrix) <- c("MAE", "RMSE", "ME")
comparison_df <- data.frame(comparison_matrix) %>%
  arrange(RMSE)


## GARCH MODEL

# ACF and PACF of Y^2
CA_tsib_sq <- CA_tsib %>%
  mutate_at(vars(Persons), function(x) x^2)

plot27 <- ACF(CA_tsib_sq, lag_max = 48) %>% autoplot() + ggtitle("ACF of Y^2") +
  theme_light() +
  PACF(CA_tsib_sq, lag_max = 48) %>% autoplot() + ggtitle("PACF of Y^2") +
  theme_light()

# define optimal ARMA model
GARCH_optim_ARMA <- auto.arima(CA_ts, max.d=0, max.P=0, max.D=0, max.Q=0)

# finding optimal GARCH model
GARCH_p <- 1:8
GARCH_q <- 0:8

best_pq <- rep(NA, 8)
best_AIC <- rep(NA, 8)

for (p in GARCH_p) {
  best_q <- rep(NA, 9)
  
  for (q in GARCH_q) {
    # GARCH specification
    GARCH_opt <- ugarchspec(
      variance.model = list(model = 'sGARCH', garchOrder = c(p, q)),
      mean.model = list(armaOrder = c(1,1), include.mean = FALSE),
      distribution.model = 'sstd')
    # GARCH fitting
    GARCH_opt_fit <- ugarchfit(spec = GARCH_opt, data = CA_ts)
    best_q[q + 1] <- infocriteria(GARCH_opt_fit)[1]
  }
  best_pq[p] <- which.min(best_q) - 1
  best_AIC[p] <- best_q[which.min(best_q)]
}

# best p and q
optimal_p <- which.min(best_AIC)
optimal_q <- best_pq[which.min(best_AIC)]

# GARCH specification
GARCH_model <- ugarchspec(
  variance.model = list(model = 'sGARCH', garchOrder = c(optimal_p, optimal_q)),
  mean.model = list(armaOrder = c(1,1), include.mean = FALSE),
  distribution.model = 'sstd'
)

# GARCH fitting
GARCH_fit <- ugarchfit(spec = GARCH_model, data = CA_ts)

# print table of GARCH parameters
GARCH_coef <- data.frame(GARCH_fit@fit$coef) %>%
  rename(Estimates = GARCH_fit.fit.coef)  %>% t()
colnames(GARCH_coef) <- c("AR(1)", "MA(1)", "Omega", "Alpha", "Beta", "Skew", "Shape")

# ACF and PACF of e^2 / sigma^2
GARCH_resid_sigma <- GARCH_fit@fit$residuals ^ 2 / GARCH_fit@fit$var
GARCH_resid_sigma_tsib <- bind_cols(CA_tsib_sq[,1],
                                    GARCH_resid_sigma)

plot28 <- ACF(GARCH_resid_sigma_tsib, lag_max = 48) %>% autoplot() +
  ggtitle("ACF of (e^2 / sigma^2)") +
  theme_light() +
  PACF(GARCH_resid_sigma_tsib, lag_max = 48) %>% autoplot() +
  ggtitle("PACF of (e^2 / sigma^2)") +
  theme_light()

# computing fitted values and st dev
GARCH_fit_sigma_ts <- ts(bind_cols(fitted = GARCH_fit@fit$fitted,
                                   lower = GARCH_fit@fit$fitted - GARCH_fit@fit$sigma,
                                   upper = GARCH_fit@fit$fitted + GARCH_fit@fit$sigma),
                         start = start(CA_ts), freq = 12)

# plotting
plot29 <- autoplot(GARCH_fit_sigma_ts[,1], color = "blue") +
  geom_line(data = CA_ts) +
  geom_ribbon(aes(ymin = GARCH_fit_sigma_ts[,2], ymax = GARCH_fit_sigma_ts[,3]),
              fill = "cyan4", alpha = 0.5) +
  geom_line(aes(y = GARCH_fit_sigma_ts[,1]), color = "blue", lwd = 0.4) +
  labs(title = "Change in CA SNAP Recipients",
       subtitle = "data in black, GARCH model in blue, with standard deviation bands",
       y = "Persons (Thousands)") +
  theme_light()


# computing variance
GARCH_var_ts <- ts(bind_cols(GARCH_fit@fit$var), start = start(CA_ts), freq = 12)

# plotting variance
plot30 <- autoplot(GARCH_var_ts, color = "cyan4") +
  geom_ribbon(aes(ymin = 0, ymax = GARCH_var_ts), fill = "cyan4", alpha = 0.75) +
  labs(title = "Variance of GARCH Model",
       y = "Squared Persons (Millions)") +
  theme_light()

# forecast
GARCH_fcst <- ugarchforecast(GARCH_fit, n.ahead = 60)
GARCH_fcst_df <- bind_cols(data.frame(fitted(GARCH_fcst)),
                           data.frame(sigma(GARCH_fcst))) %>%
  rename(forecast = Jun.2016...1,
         sigma = Jun.2016...2) %>%
  mutate(lower.80 = forecast + qnorm(0.1) * sigma,
         upper.80 = forecast + qnorm(0.9) * sigma,
         lower.95 = forecast + qnorm(0.025) * sigma,
         upper.95 = forecast + qnorm(0.975) * sigma) %>%
  dplyr::select(-sigma)

GARCH_fcst_ts <- ts(GARCH_fcst_df, start = c(2016, 7), freq = 12)

# plotting
plot31 <- autoplot(GARCH_fcst_ts[,1], color = "dodgerblue4") +
  geom_ribbon(aes(ymin = GARCH_fcst_ts[,4], ymax = GARCH_fcst_ts[,5]),
              fill = "dodgerblue2", alpha = 0.5) +
  geom_ribbon(aes(ymin = GARCH_fcst_ts[,2], ymax = GARCH_fcst_ts[,3]),
              fill = "dodgerblue3", alpha = 0.5) +
  geom_line(aes(y = GARCH_fcst_ts[,1]), color = "dodgerblue4", lwd = 0.7) +
  labs(title = "Change in CA SNAP Forecast: GARCH",
       subtitle = "with error bands, actual in red",
       y = "Persons (Thousands)") +
  geom_line(data = CA_ts) + xlim(1995, NA) +
  geom_line(data = test_ts, alpha = 0.5, color = "red") +
  theme_light()

# computing forecasted variance
GARCH_Var_fcst_ts <- ts(sigma(GARCH_fcst)^2, start = c(2016, 7), freq = 12)

# plotting frecasted variance
plot32 <-autoplot(GARCH_Var_fcst_ts, color = "cyan4") +
  geom_ribbon(aes(ymin = 0, ymax = GARCH_Var_fcst_ts), fill = "cyan4", alpha = 0.75) +
  labs(title = "Forecasted Variance of GARCH Model",
       y = "Squared Persons (Millions)") +
  theme_light()

mod10_resid <- test_ts - GARCH_fcst_ts[,'forecast']




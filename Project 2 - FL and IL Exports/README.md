# Project 2: Trend-Seasonality-Cycles, ARIMA, and VAR Models

**Description:** Ramping up the difficulty, the second project looked at the monetary value of exports for two US states: Florida and Illinois. The data was again monthly. And this time, we not only wanted to study the individual time series, but also the dynamics between them.

1. I initally fit a model using an STL decomposition with an ARMA component for cycles. I did this for both time series.
2. My second model was an ARIMA model. Again, one for both time series.
3. The third model was a VAR model, which incorporated data from both time series.
4. To assess "causality," I constructed plots of the impulse response functions (IRF) for the two series. I also performed a Granger causality test to see if one series "Granger-causes" the other.

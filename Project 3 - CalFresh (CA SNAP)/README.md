# Project 3: Many (Many) Models

**Description:** For the third project, I had free rein to use whatever models I wanted to forecast a time series. The data I chose was Supplemental Nutrition Assistance Program (SNAP) benefits recipients in California. (Note that in California the program is called "CalFresh," and more generally, it is referred to as "Food Stamps.")

The data had a monthly frequency. And to compare my models, I performed a train-test split of the data and compared model accuracy based on a prediction of the final 5 years of the time series. I used two metrics for accuracy: mean absolute error (MAE) and root mean squared error (RMSE).

## The Models
* An STL decomposition with an ARMA component for cycles
* An ARIMA model
* An ETS model
* A Holt-Winters model
* A TBATS model
  + Trigonometric seasonality, Box-Cox transformation, ARMA errors, Trend and Seasonal components
* A VAR model
  + I used SNAP recipients from Washington as an exogenous variable
  + I also assessed causality with IRF plots and a Granger causality test
* A Prophet model
* A Neural Network Autoregression
* A Combination model with static weights
  + The weighting scheme was determined with a linear regression
* A GARCH model for conditional variance, with an ARMA mean model

Further work that I would have liked to have included would be a GARCH component for each model, not just with an ARMA model. I also would have liked to incorporate a temporal aggregation mdoel like THIEF or MAPA. Lastly, the inclusion of a state space model -- i.e. Kalman filtering -- would be another option to explore.

As for the combination model, I could have used a time-variant aggregation technique (e.g. Kalman filtering), and that might have been more effective than the linear regression static weighting scheme. If I wanted to improve the linear model, I could have chosen an optimization method for feature selection (like a forward/backward regression or a best subsets regression) so as to use only the models that would be optimally relevant. I might try it out in future projects...

# Forecasting Projects (Econ 144)
### by Jacob Titcomb

* This repository is a collection of the projects I completed for Economics 144 at UCLA with Dr. Randall Rojas, for Spring 2023.

* While the formal class title is "Economic Forecasting," the course covers time series analysis, modeling of time series data, and forecasting, with applications across many disciplines, not just economics.

* There were three projects, each with increasing complexity. Thus the third project is most representative of the skills I learned.

* For projects 2 and 3, I decided that I disliked the look of the standard graphing functions in the libraries I was using, so I created my own visualizations from scratch using `ggplot2`.

* All work is my own (i.e. analyses, models, visualizations), and it was all done in R. I also found the data sets myself.

In the descriptions below, I will go into more detail into the components of each project.

## Project 1: Trend and Seasonality

**Description:** The first project modeled the employment-population ratio of Japan, which is the proportion of all people 15 and older who are employed. The data had a monthly frequency.

1. To explore the idea of capturing the "trend" of the data, I fit a basic polynomial model to the time series, using OLS.
2. To explore seasonality, I performed *manual* monthly seasonal adjustments, both additive and multiplicative.

## Project 2: Trend-Seasonality-Cycles, ARIMA, and VAR Models

**Description:** Ramping up the difficulty, the second project looked at the monetary value of exports for two US states: Florida and Illinois. The data was again monthly. And this time, we not only wanted to study the individual time series, but also the dynamics between them.

1. I initally fit a model using an STL decomposition with an ARMA component for cycles. I did this for both time series.
2. My second model was an ARIMA model. Again, one for both time series.
3. The third model was a VAR model, which incorporated data from both time series.
4. To assess "causality," I constructed plots of the impulse response functions (IRF) for the two series. I also performed a Granger causality test to see if one series "Granger-causes" the other.

## Project 3: Many (Many) Models

**Description:** For the third project, I had free rein to use whatever models I wanted to forecast a time series. The data I chose was Supplemental Nutrition Assistance Program (SNAP) benefits recipients in California. (Note that in California the program is called "CalFresh," and more generally, it is referred to as "Food Stamps.")

The data had a monthly frequency. And to compare my models, I performed a train-test split of the data and compared model accuracy based on a prediction of the final 5 years of the time series. I used two metrics for accuracy: mean absolute error (MAE) and root mean squared error (RMSE).

### The Models
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
* A GARCH model for conditional variance




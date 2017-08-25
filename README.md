# economy-forecast
Forecasting economy using random forest, ARIMA and crossvalidation. https://martenveskimae.shinyapps.io/economy-forecast/

A forecast model, based on aggregate survey information that allows users to predict sector specific variables.

The model is combined of LASSO, randomforest and ARIMA (used on prediction errors) and once again randomforest for combining. The model enables users to adjust the training period by a modified cross-validation technique or by manually selecting the starting point.

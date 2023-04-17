## @knitr loading_package

library(tsibble)
library(dplyr)
library(fable)
library(feasts)

## @knitr loading_data

# Manufacturer's Stocks of evaporated and sweetened condensed milk.
data(condmilk, package="fma")
condmilk <- as_tsibble(condmilk)

## @knitr transform_data

# Transformando: se usa el log en base 10
cm_transf <- condmilk %>%
  mutate(prop_change=difference(log10(value)), .keep = "unused") %>%
  tidyr::drop_na()

# This one is for functions outside tidy packages
univariate_ts <- cm_transf %>%
  as.ts() %>%
  zoo::na.approx()

## @knitr description_serie

adf_test <- tseries::adf.test(univariate_ts)

## @knitr full_models

# Full arima model estimated
full <- cm_transf %>%
  model(non_stationaty=ARIMA(prop_change, 
  	  stepwise=FALSE, 
  	  method="CSS-ML", optim.method="BFGS"),
	stationary=ARIMA(prop_change,
  	  order_constraint=(d + D) == 0,
  	  stepwise=FALSE,
  	  method="CSS-ML", optim.method="BFGS"))

## @knitr first_fitting

selected_mod <- cm_transf %>%
  model(`ARMA(1,0,0)(0,1,1)_{12}`=ARIMA(prop_change ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 1, 12), 
  	stepwise=FALSE, method="CSS-ML", optim.method="BFGS")) 

all_models <- selected_mod %>%
  bind_cols(select(full, non_stationaty))

## @knitr ljung_box_test

box_test <- augment(selected_mod) %>%
  features(.innov, ljung_box, dof = 4, lag = 12)

## @knitr goodness_fit

information_based <- glance(all_models) %>%
  select(-ar_roots, -ma_roots)

error_based <- accuracy(all_models)

## @knitr diagnostics

res_sd <- information_based %>% 
  filter(.model != "non_stationaty") %>% 
  pull(sigma2) %>%
  sqrt()
augmented_data <- augment(selected_mod) %>%
  mutate(.std.resid=.resid / res_sd)

## @knitr cleanin_4_lag_plot

tscleaned <- augmented_data %>%
  filter(
    !(.std.resid < quantile(.std.resid, 0.25) - 1.96 * IQR(.std.resid)) |
    !(.std.resid > quantile(.std.resid, 0.75) + 1.96 * IQR(.std.resid))
  )

## @knitr lagged_regressors_ts

ts_w_lagged_reg <- cm_transf[-(1:5),] %>%
  bind_cols(
  	ts.intersect(as.ts(cm_transf), 
	  pcL5=as.ts(cm_transf) %>% stats::lag(-5),
	  dL5=ifelse(as.ts(cm_transf) < .05, 0, 1) %>% stats::lag(-5), dframe=TRUE) %>%
    select(pcL5, dL5)
  ) %>%
  mutate(pcL5sq=pcL5 ** 2)

## @knitr lagged_regression

new_models <- ts_w_lagged_reg %>%
  model(
  	`Lagged with sq`=ARIMA(prop_change ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 1, 12) + pcL5 + pcL5sq, 
  	  stepwise=FALSE, method="CSS-ML", optim.method="BFGS"),
	`Lagged by pieces`=ARIMA(prop_change ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 1, 12) + pcL5 * dL5, 
 	  stepwise=FALSE, method="CSS-ML", optim.method="BFGS")) %>%
  bind_cols(all_models)

## @knitr goodness_of_fit_lagged

information_based_2 <- new_models %>%
  glance() %>%
  arrange(AIC) %>%
  select(-ar_roots, -ma_roots)

error_based_2 <- accuracy(new_models)

## @knitr diagnostics_new 

res_sd <- information_based_2 %>% 
  filter(.model == "Lagged with sq") %>%
  pull(sigma2) %>%
  sqrt()
augmented_data_2 <- new_models %>%
  select(`Lagged with sq`) %>%
  augment() %>%
  mutate(.std.resid=.resid / res_sd)

## @knitr generating_new_data

first_forecast <- selected_mod %>%
  forecast(h=24)
to_forecast <- first_forecast %>%
  bind_cols(
  	bind_rows(setNames(cm_transf[115:119, "prop_change"], "pcL5"),
  	ts.intersect(as.ts(first_forecast[c("index", ".mean")]), 
	  pcL5=as.ts(first_forecast[c("index", ".mean")]) %>% 
	    stats::lag(-5), dframe=TRUE) %>%
    select(pcL5))
  ) %>%
  mutate(pcL5sq=pcL5 ** 2) %>%
  select(-.model) 

## @knitr forecasting

forecast_24_month <- new_models %>%
  select(`Lagged with sq`) %>%
  forecast(new_data=to_forecast)

## @knitr ets_modeling

ets_models <- cm_transf %>%
  model(`ETS model`=ETS(prop_change ~ error("A") + trend("A") + season("A", period=12), opt_crit="mse")) 

## @knitr ets_forecasting

ets_forecast <- ets_models %>% forecast(h=24) 
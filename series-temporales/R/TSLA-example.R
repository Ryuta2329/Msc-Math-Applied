## @knitr LoadingLibraries

library(quantmod)
library(tsibble)
library(feasts)
library(fable)
library(tseries)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(rugarch)

## @knitr LoadingData

xts_tsla <- getSymbols("TSLA",
    from = as.Date("2015", "%Y"),
    to = Sys.Date()) |>
  get()

# Estos son los retornos, dado que el modelo GARCH trabaja es con estos.
tsla_returns <- dailyReturn(xts_tsla, type = "log") |>
  fortify.zoo() |>
  within({
    Index = lubridate::ymd(Index)
  }) |>
  complete(Index = seq(min(Index), max(Index), by = "day")) |>
  fill(daily.returns) %>%
  as_tsibble(index = Index)

## @knitr raiz-Unitaria.

unit_root_tests <- list(
    adf=adf.test, pp=pp.test, kpss=kpss.test) |>
  map(exec,
    x=tsla_returns |> as.ts()) |>
  map(broom::tidy) |>
  list_rbind()

## @knitr acf-pacf-tsla-square

acf_pacf_df <- tsla_returns |>
  within({
    mreturn_square = daily.returns ** 2
  }) |>
  ACF(mreturn_square, lag_max = 50) |>
  within({
    pacf = PACF(tsla_returns, daily.returns ** 2,
      lag_max = 50)$pacf
  })

## @knitr AR-Model

return_models <- tsla_returns %>%
  mutate(sq_returns = daily.returns ** 2) %>%
  model(
    ar1_model = AR(sq_returns ~ 0 + order(1), stepwise = FALSE),
    ar2_model = AR(sq_returns ~ 0 + order(2), stepwise = FALSE),
    sarima_model1 = ARIMA(
      sq_returns ~ 0 + pdq(2,0,0) + PDQ(2,0,0,7),
      stepwise = FALSE),
    sarima_model2 = ARIMA(
      sq_returns ~ 0 + pdq(2,0,0) + PDQ(3,0,0,7),
      stepwise = FALSE),
    sarima_model3 = ARIMA(
      sq_returns ~ 0 + pdq(2,0,1) + PDQ(3,0,0,7),
      stepwise = FALSE),
    sarima_model4 = ARIMA(
      sq_returns ~ 0 + pdq(2,0,2) + PDQ(3,0,0,7),
      stepwise = FALSE)
  ) 

return_models %<>%
  pivot_longer(cols = 1:6,
    names_to = "Model", values_to = "Fit") %>%
  mutate(glance = map(Fit, glance)) %>%
  unnest(glance) %>%
  arrange(sigma2) %>%
  slice(1:2) %>%
  select(Model:Fit) %>%
  mutate(tidied = map(Fit, tidy)) %>%
  unnest(tidied)

return_models <- tsla_returns %>%
  mutate(sq_returns = daily.returns ** 2) %>%
  model(arima_model = ARIMA(
      sq_returns ~ 0 + pdq(2,0,0) + PDQ(3,0,0,7),
      stepwise = FALSE))

## @knitr var-evaluation

month_window_agg <- augment(return_models) %>%
  index_by(year_month = ~ yearmonth(.)) %>%
  summarise(
    month_var = var(.innov, na.rm = TRUE),
    mean_sq_return = mean(sq_returns, na.rm = TRUE)
  )

## @knitr Arch-Test

# Resultado significativo (hay efectos de volatilidad importantes).
arch_test <- augment(return_models) %$%
  FinTS::ArchTest(.innov, lags = 4, demean = FALSE)

## @knitr specifying_model

# El orden ARMA se esepcifica a 2 como en el SARIMA
garch_fitting <- tibble(list_pars = list(
    c(1, 0), c(1, 1), c(2, 1)
  )) %>%
  mutate(mod_spec = map(list_pars, 
    ~ugarchspec(mean.model = list(armaOrder=c(2, 0), include.mean = FALSE),
      variance.model=list(model = "sGARCH", garchOrder=.),
      distribution.model = "std")
  ))

## @knitr model_fitting

garch_fitting %<>%
  mutate(mod_fit = map(mod_spec,
    ~ugarchfit(., tsla_returns)
  ))
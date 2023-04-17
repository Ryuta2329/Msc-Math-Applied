## @knitr LoadingLibraries

library(quantmod)
library(tsibble)
library(feasts)
library(tseries)
library(purrr)


## @knitr LoadingData

xts_tsla <- getSymbols("TSLA",
	  from=as.Date("2015", "%Y"), 
	  to=Sys.Date()) |>
	get()

# Estos son los retornos, dado que el modelo GARCH trabaja es con estos.
tsla_returns <- dailyReturn(xts_tsla, type="log") |>
	fortify.zoo() |>
	within({Index=lubridate::ymd(Index)}) |>
	tidyr::complete(Index = seq(min(Index), max(Index), by = 'day')) |>
	tidyr::fill(daily.returns) %>%
	as_tsibble(index = Index) 

## @knitr raiz-Unitaria.

unit_root_tests <- list(
	  adf=adf.test, pp=pp.test, kpss=kpss.test) |>	
	map(exec, 
	  x=tsla_returns |> as.ts()) |>
	map(broom::tidy) |>
	list_rbind()

tibble::column_to_rownames(unit_root_tests, "method")[, 1:3]

## @knitr acf-pacf-tsla-square

acf_pacf_df <- tsla_returns |> 
	within({mReturn_square=daily.returns ** 2}) |> 
	ACF(mReturn_square, lag_max=50) |> 
	within({pacf=PACF(tsla_returns, daily.returns ** 2, lag_max=50)$pacf})

## @knitr Arch-Test

# Aqui se supone haga un histograma de los residuales al cuadrado 
# siguiendo algun modelo, y luego ajustar una distribucion normal y 
# un kernel para ver la distribucion subyacente.

# Resultado significativo (hay efectos de volatilidad importantes).
arch_test <- FinTS::ArchTest(tsla_returns$daily.returns, 
	lags=4, demean = FALSE)

## @knitr 

https://albertoalmuinha.github.io/garchmodels/articles/getting-started.html
https://duckduckgo.com/?t=ffab&q=garch+model+in+tidy+way&atb=v306-1&ia=web
https://www.rdocumentation.org/packages/fGarch/versions/4022.89
https://www.rdocumentation.org/packages/rugarch/versions/1.4-9
https://www.idrisstsafack.com/post/garch-models-with-r-programming-a-practical-example-with-tesla-stock
https://stats.stackexchange.com/questions/177758/imputing-missing-values-of-finance-data
https://business-science.github.io/timetk/reference/index.html
https://fabletools.tidyverts.org/reference/index.html
https://fable.tidyverts.org/reference/index.html
https://feasts.tidyverts.org/reference/index.html
https://tsibble.tidyverts.org/reference/index.html
https://rpubs.com/Ana_JP/TESLA_ARCHGARCH
https://finance.yahoo.com/quote/TSLA/chart?p=TSLA#eyJpbnRlcnZhbCI6IndlZWsiLCJwZXJpb2RpY2l0eSI6MSwidGltZVVuaXQiOm51bGwsImNhbmRsZVdpZHRoIjo0LjM0ODY1OTAwMzgzMTQxNzUsImZsaXBwZWQiOmZhbHNlLCJ2b2x1bWVVbmRlcmxheSI6dHJ1ZSwiYWRqIjp0cnVlLCJjcm9zc2hhaXIiOnRydWUsImNoYXJ0VHlwZSI6ImxpbmUiLCJleHRlbmRlZCI6ZmFsc2UsIm1hcmtldFNlc3Npb25zIjp7fSwiYWdncmVnYXRpb25UeXBlIjoib2hsYyIsImNoYXJ0U2NhbGUiOiJsaW5lYXIiLCJzdHVkaWVzIjp7IuKAjHZvbCB1bmRy4oCMIjp7InR5cGUiOiJ2b2wgdW5kciIsImlucHV0cyI6eyJpZCI6IuKAjHZvbCB1bmRy4oCMIiwiZGlzcGxheSI6IuKAjHZvbCB1bmRy4oCMIn0sIm91dHB1dHMiOnsiVXAgVm9sdW1lIjoiIzAwYjA2MSIsIkRvd24gVm9sdW1lIjoiI2ZmMzMzYSJ9LCJwYW5lbCI6ImNoYXJ0IiwicGFyYW1ldGVycyI6eyJ3aWR0aEZhY3RvciI6MC40NSwiY2hhcnROYW1lIjoiY2hhcnQifX19LCJwYW5lbHMiOnsiY2hhcnQiOnsicGVyY2VudCI6MSwiZGlzcGxheSI6IlRTTEEiLCJjaGFydE5hbWUiOiJjaGFydCIsImluZGV4IjowLCJ5QXhpcyI6eyJuYW1lIjoiY2hhcnQiLCJwb3NpdGlvbiI6bnVsbH0sInlheGlzTEhTIjpbXSwieWF4aXNSSFMiOlsiY2hhcnQiLCLigIx2b2wgdW5kcuKAjCJdfX0sInNldFNwYW4iOnsibXVsdGlwbGllciI6NSwiYmFzZSI6InllYXIiLCJwZXJpb2RpY2l0eSI6eyJwZXJpb2QiOjEsImludGVydmFsIjoid2VlayJ9fSwibGluZVdpZHRoIjoyLCJzdHJpcGVkQmFja2dyb3VuZCI6dHJ1ZSwiZXZlbnRzIjp0cnVlLCJjb2xvciI6IiMwMDgxZjIiLCJzdHJpcGVkQmFja2dyb3VkIjp0cnVlLCJldmVudE1hcCI6eyJjb3Jwb3JhdGUiOnsiZGl2cyI6dHJ1ZSwic3BsaXRzIjp0cnVlfSwic2lnRGV2Ijp7fX0sImN1c3RvbVJhbmdlIjpudWxsLCJzeW1ib2xzIjpbeyJzeW1ib2wiOiJUU0xBIiwic3ltYm9sT2JqZWN0Ijp7InN5bWJvbCI6IlRTTEEiLCJxdW90ZVR5cGUiOiJFUVVJVFkiLCJleGNoYW5nZVRpbWVab25lIjoiQW1lcmljYS9OZXdfWW9yayJ9LCJwZXJpb2RpY2l0eSI6MSwiaW50ZXJ2YWwiOiJ3ZWVrIiwidGltZVVuaXQiOm51bGwsInNldFNwYW4iOnsibXVsdGlwbGllciI6NSwiYmFzZSI6InllYXIiLCJwZXJpb2RpY2l0eSI6eyJwZXJpb2QiOjEsImludGVydmFsIjoid2VlayJ9fX1dfQ--

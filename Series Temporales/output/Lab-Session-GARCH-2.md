# Sesión de Laboratorio: Modelos GARCH.

_Marcelo J Molinatti S_



La sesión de laboratorio trata de analizar los retornos de las acciones de Tesla, con la finalidad de obtener un modelo que explique la volatilidad de la serie, tal como se observa en la <a href="#tsla-series">figura 1</a>, a la derecha. 
Las acciones de TSLA mostrada en la <a href="#tsla-series">figura 1</a>, a la izquierda, permite ver que la la serie se mantuvo bastante estable durante toda la primera mitad de la serie, hasta el último trimestre del año 2019, momento a partir del cual la serie comienza a crecer rápidamente, debido a la atención de muchos inversionistas ya que la acción rebasó los 100 $USD y en el primer trimestre de 2020, a pesar de la emergencia sanitaria provocada por el COVID-19, la acción alcanzó su máximo histórico de aproximadamente 300 $USD. Luego, hubo una caída en la primera mitad del 2021, momento después del cual las acciones de TESLA alcanzaron un nuevo máximo que superó los 400 $USD. 

Respecto a los rendimientos registrados para TESLA, se pueden observar 4 _clusters_ de volatilidad en la serie: el primero se presenta a finales de 2015 (a principios de la serie) donde se registraron rendimientos de $\pm10$%; la siguiente aglomeración es en octubre de 2018 donde TESLA alcanzó a registrar rendimientos de $\pm15$% y, su _cluster_ de volatilidad más acentuado es durante el primer trimestre de 2020 cuando TESLA comenzó a registrar incrementos considerables llegando a obtener rendimientos de $\pm20$% en un solo día. Finalmente, un _cluster_ de volatilidad persistente que se registra a desde el último trimestre del 2021, hasta el final de la serie.

<a name="tsla-series"></a>

```r
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
library(astsa)
library(ggplot2)
library(kableExtra)

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

tsla_close <- TSLA[,"TSLA.Close"] |> 
  fortify.zoo() |>
  as_tsibble()

cowplot::plot_grid(
  autoplot(tsla_close, colour="orange") +
    theme_light() + xlab("") + ylab("Precios de Cierre ($USD)") +
    ggtitle("Precio de Cierre de TESLA"),
  autoplot(tsla_returns, colour="orange") +
    theme_light() + xlab("") + ylab("Rendimiento") +
    ggtitle("Rendimientos logarítmicos de TESLA"),
  nrow=1
)
```

<div class="figure" style="text-align: center">
<img src="Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/tsla-series-1.png" alt="A la izquierda: Precio de Cierre de TESLA (Mayo 2015 - Mayo 2023). A la derecha: Rendimientos logarítmicos de TESLA (Mayo 2015 - Mayo 2023)."  />
<p class="caption">(\#fig:tsla-series)A la izquierda: Precio de Cierre de TESLA (Mayo 2015 - Mayo 2023). A la derecha: Rendimientos logarítmicos de TESLA (Mayo 2015 - Mayo 2023).</p>
</div>

Los resultados de las pruebas de estacionaridad mostrados en la <a href="#stationary-tests">tabla 1</a>, donde se muestra que todas las pruebas arrojan que los rendimientos es una serie estacionaria con un nivel de confianza del 95%. 

<a name="stationary-tests"></a>

```r
unit_root_tests <- list(
    adf=adf.test, pp=pp.test, kpss=kpss.test) |>
  map(exec,
    x=tsla_returns |> as.ts()) |>
  map(broom::tidy) |>
  list_rbind()

tibble::column_to_rownames(unit_root_tests, "method")[, 1:3] %>%
  kbl(
    row.names=TRUE,
    col.names=c("Estadístico", "$p$", "df"),
    escape=FALSE
  )
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Estadístico </th>
   <th style="text-align:right;"> $p$ </th>
   <th style="text-align:right;"> df </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Augmented Dickey-Fuller Test </td>
   <td style="text-align:right;"> -13,1118664 </td>
   <td style="text-align:right;"> 0,01 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Phillips-Perron Unit Root Test </td>
   <td style="text-align:right;"> -2412,2851086 </td>
   <td style="text-align:right;"> 0,01 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KPSS Test for Level Stationarity </td>
   <td style="text-align:right;"> 0,1039833 </td>
   <td style="text-align:right;"> 0,10 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
</tbody>
</table>

Al revisar las ACF y PACF de los retornos al cuadrado se puede observar que la ACF es bastante fluctuante y con correlaciones significativas que se extienden hasta retrasos grandes, y con un carácter decayente en $7k$ ($k=1,2,\ldots$). Junto con la PACF, parece que el proceso es autoregresivo estacional de, al menos, segundo orden en el componente ARIMA, y de tercer orden en el componente estacional (ya que las correlaciones significativas ocurren en $7P$, para $P=1, 2, y 3$ y luego se corta). 
Es por ello que se escoge realizar un ajuste de los modelos $SARIMA(2,0,0)(2,0,0)_7$ y $SARIMA(2,0,0)(3,0,0)_7$ como alternativas posible para modelar los retornos al cuadrado. 

<a name="acf-pacf"></a>

```r
acf_pacf_df <- tsla_returns |>
  within({
    mreturn_square = daily.returns ** 2
  }) |>
  ACF(mreturn_square, lag_max = 50) |>
  within({
    pacf = PACF(tsla_returns, daily.returns ** 2,
      lag_max = 50)$pacf
  })

return_models <- tsla_returns %>%
  mutate(sq_returns = daily.returns ** 2) %>%
  model(arima_model = ARIMA(
      sq_returns ~ 0 + pdq(2,0,0) + PDQ(3,0,0,7),
      stepwise = FALSE))

cowplot::plot_grid(
  ggplot(acf_pacf_df, aes(x=lag, y=acf)) +
    geom_point() +
    geom_hline(yintercept = c(0, -1, 1) / sqrt(nrow(tsla_returns)), 
      linetype = c(1, 2, 2), colour = c(1, "blue", "blue")) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    ylab(latex2exp::TeX("$\\rho(s, t)$")) +
    theme_light(),
  ggplot(acf_pacf_df, aes(x=lag, y=pacf)) +
    geom_point() +
    geom_hline(yintercept = c(0, -1, 1) / sqrt(nrow(tsla_returns)), 
      linetype = c(1, 2, 2), colour = c(1, "blue", "blue")) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    ylab(latex2exp::TeX("$\\rho(s, t)$")) +
    theme_light(),
  nrow=1
)
```

<div class="figure" style="text-align: center">
<img src="Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/acf-pacf-1.png" alt="ACF y PACF de los rendimientos logarítmicos."  />
<p class="caption">(\#fig:acf-pacf)ACF y PACF de los rendimientos logarítmicos.</p>
</div>

El modelo seleccionado al realizar el ajuste es el $SARIMA(2,0,0)(3,0,0)_7$, cuyos coeficientes se muestran en la <a href="#sarima-coef">tabla 2</a> junto con sus desviaciones estándar, se escribe como:

$$(1 - {0,079}_{(0,019)}B^7 - {0,09}_{(0,019)}B^{14} - {0,068}_{(0,019)}B^{21})(1 - {0,267}_{(0,019)}B - {0,173}_{(0,019)}B^2) r_t^2 = w_t$$

<a name="sarima-coef"></a>

```r
tidy(return_models) %>%
  select(-.model) %>%
  mutate(term = c("$\\phi_1$", "$\\phi_2$", "$\\Phi_1$", "$\\Phi_2$", "$\\Phi_3$")) %>%
  kbl(digits = c(NA, 3, 3, 2, 4), escape=FALSE,
    col.names = c("Termino", "Estimado", "Est. Desv.", "Estadístico", "$p$")
  )
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Termino </th>
   <th style="text-align:right;"> Estimado </th>
   <th style="text-align:right;"> Est. Desv. </th>
   <th style="text-align:right;"> Estadístico </th>
   <th style="text-align:right;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\phi_1$ </td>
   <td style="text-align:right;"> 0,267 </td>
   <td style="text-align:right;"> 0,019 </td>
   <td style="text-align:right;"> 14,02 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:right;"> 0,173 </td>
   <td style="text-align:right;"> 0,019 </td>
   <td style="text-align:right;"> 9,34 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Phi_1$ </td>
   <td style="text-align:right;"> 0,079 </td>
   <td style="text-align:right;"> 0,019 </td>
   <td style="text-align:right;"> 4,13 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Phi_2$ </td>
   <td style="text-align:right;"> 0,090 </td>
   <td style="text-align:right;"> 0,019 </td>
   <td style="text-align:right;"> 4,81 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Phi_3$ </td>
   <td style="text-align:right;"> 0,068 </td>
   <td style="text-align:right;"> 0,019 </td>
   <td style="text-align:right;"> 3,57 </td>
   <td style="text-align:right;"> 0,0004 </td>
  </tr>
</tbody>
</table>

Los diagnósticos para este modelo se muestran en la <a href="#res-diagnostics">figura 3</a>, donde se observan mejor los _cluster_ de volatilidad en la serie de residuales. El ACF y PACF tienen un comportamiento mejor, pero aun se observan correlaciones significativas no tomadas en cuenta.

<a name="res-diagnostics"></a>

```r
cowplot::plot_grid(
  autoplot(augment(return_models), .innov, colour="orange") +
    theme_light() + xlab("") + ylab("Innovaciones"),
  cowplot::plot_grid(
    ACF(augment(return_models), .innov, lag_max=50) %>%
      autoplot() + theme_light() +
      xlab("lag") + ylab("ACF"),
    PACF(augment(return_models), .innov, lag_max=50) %>%
      autoplot() + theme_light() +
      xlab("lag") + ylab("PACF"),
    nrow=2
  ),
  nrow=1
)
```

<div class="figure" style="text-align: center">
<img src="Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/res-diagnostics-1.png" alt="Gráfico temporal de residuales y ACF y PACF de los mismos, para el modelo SARIMA ajustado."  />
<p class="caption">(\#fig:res-diagnostics)Gráfico temporal de residuales y ACF y PACF de los mismos, para el modelo SARIMA ajustado.</p>
</div>

Utilizando una ventana de un mes, se calculó la varianza mensual de las innovaciones, asi como el valor medio de los retornos al cuadrado, y se verificaron las relaciones entre la varianza de las innovaciones con respecto a los retornos cuadrados mensuales, y los retrasos (para $h=1, 2, 3$). Se puede observar que existen correlaciones importantes entre la varianza y los retornos cuadrados, y también una correlación moderadamente pequeña ($0{,}34$) con respecto a los retornos cuadrados en $t-1$, y una correlación pequeña ($0{,}17$) con respecto al retraso en $t-2$.

<a name="returns-lag"></a>

```r
month_window_agg <- augment(return_models) %>%
  index_by(year_month = ~ yearmonth(.)) %>%
  summarise(
    month_var = var(.innov, na.rm = TRUE),
    mean_sq_return = mean(sq_returns, na.rm = TRUE)
  )

month_window_agg %$%
  lag2.plot(mean_sq_return,month_var, 3)
```

<div class="figure" style="text-align: center">
<img src="Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/returns-lag-1.png" alt="Gráficos de dispersión de la varianza mensual de las innovaciones con respectoa  retrasos de los retornos al cuadrado."  />
<p class="caption">(\#fig:returns-lag)Gráficos de dispersión de la varianza mensual de las innovaciones con respectoa  retrasos de los retornos al cuadrado.</p>
</div>

Además, en la <a href="#var-lags">figura 5</a> se muestra que existe una correlación pequeña entre la varianza mensual de las innovaciones con respecto al retraso de la misma en $t-1$, y que las correlaciones subsiguientes no son tan importantes.

<a name="var-lags"></a>

```r
arch_test <- augment(return_models) %$%
  FinTS::ArchTest(.innov, lags = 4, demean = FALSE)

month_window_agg %$%
  lag1.plot(month_var, 4)
```

<div class="figure" style="text-align: center">
<img src="Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/var-lags-1.png" alt="Gráficos de dispersión de la varianza mensual de las innovaciones con respecto a retrasos de la misma."  />
<p class="caption">(\#fig:var-lags)Gráficos de dispersión de la varianza mensual de las innovaciones con respecto a retrasos de la misma.</p>
</div>

Al realizar una prueba Arch por multiplicadores de Lagrange, al descomponer la varianza de la serie e identificar si sus rezagos son significativos, se obtiene que la serie tiene efectos Arch significativos ($\chi^2=$ 64,8, $p=$ 0).

Los resultados hasta el momento parecen indicar que se debe utilizar un modelo $SARIMA(2,0,0)(3,0,0)_7$ para los retornos al cuadrado, y un modelo $GARCH(2, 1)$ para la varianza condicional, sobre los residuos del modelo SARIMA. De igual forma, para evaluar los modelos en términos de la información proveída por cada uno, se ajustaran y compararan con el modelo $GARCH(2, 1)$, los modelos $GARCH(1, 0)$ y $GARCH(1, 1)$.

Los criterios de información mostrados adelante parecen indicar que los modelos con varianza condicional dependiente del retraso en $t-1$ de la misma, son preferibles al modelo $GARCH(1, 0)$, como se esperaba del análisis exploratorio anterior. 
Por otro lado, los mismo criterios de información sugieren la selección del modelo $GARCH(1, 1)$, dado los valores de estas medidas. 

<a name="inf-criteria"></a>

```r
garch_fitting <- tibble(list_pars = list(
    c(1, 0), c(1, 1), c(2, 1)
  )) %>%
  mutate(mod_spec = map(list_pars, 
    ~ugarchspec(mean.model = list(armaOrder=c(2, 0), include.mean = FALSE),
      variance.model=list(model = "sGARCH", garchOrder=.),
      distribution.model = "std")
  ))

garch_fitting %<>%
  mutate(mod_fit = map(mod_spec,
    ~ugarchfit(., tsla_returns)
  ))

garch_fitting %>%
  select(mod_fit) %>%
  flatten() %>%
  map(\(x) {
    rugarch::infocriteria(x) %>%
      as.data.frame() %>%
      tibble::rownames_to_column()
  }) %>%
  list_rbind(, names_to = "Garch") %>%
  tidyr::spread(key = "rowname", value = "V1") %>%
  mutate(
  	Garch = recode(Garch,
  	  `1` = "$GARCH(1,0)$",
      `2` = "$GARCH(1,1)$",
      `3` = "$GARCH(2,1)$")) %>%
  kbl(digits = 4, escape=FALSE)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Garch </th>
   <th style="text-align:right;"> Akaike </th>
   <th style="text-align:right;"> Bayes </th>
   <th style="text-align:right;"> Hannan-Quinn </th>
   <th style="text-align:right;"> Shibata </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $GARCH(1,0)$ </td>
   <td style="text-align:right;"> -4,2682 </td>
   <td style="text-align:right;"> -4,2580 </td>
   <td style="text-align:right;"> -4,2645 </td>
   <td style="text-align:right;"> -4,2682 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $GARCH(1,1)$ </td>
   <td style="text-align:right;"> -4,2701 </td>
   <td style="text-align:right;"> -4,2578 </td>
   <td style="text-align:right;"> -4,2657 </td>
   <td style="text-align:right;"> -4,2701 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $GARCH(2,1)$ </td>
   <td style="text-align:right;"> -4,2700 </td>
   <td style="text-align:right;"> -4,2557 </td>
   <td style="text-align:right;"> -4,2648 </td>
   <td style="text-align:right;"> -4,2700 </td>
  </tr>
</tbody>
</table>

Al observar la tabla de coeficientes, observamos que $\beta$ es significativo, como se esperaba, dada la relación de la volatilidad en $t$ con la volatilidad en $t-1$. Sin embargo, el coeficiente para $r_{t-2}$ en el modelo $GARCH(2, 1)$ no es significativo en absoluto, por lo que se puede prescindir de este termino, eligiéndose como modelo final el $GARCH(1, 1)$. 
Notese que los valores de los coeficientes del componente ARMA son significativos y similares a los ajustados para el modelo SARIMA encontrado antes.

<a name="coef-garch"></a>

```r
garch_fitting %>%
  select(mod_fit) %>%
  slice(-1) %>%
  flatten() %>%
  map(\(x) {
    x@fit$matcoef %>%
      as_tibble()
  }) %>%
  list_rbind(, names_to = "Garch") %>%
  mutate(
  	Garch = recode(Garch,
      `1` = "$GARCH(1,1)$",
      `2` = "$GARCH(2,1)$"),
  	par = c(
  	  "$\\phi_1$", "$\\phi_2$", "$\\alpha_0$", "$\\alpha_1$", "$\\beta$", "GL",
  	  "$\\phi_1$", "$\\phi_2$", "$\\alpha_0$", "$\\alpha_1$", "$\\alpha_2$", "$\\beta$", "GL"
  	), 
  	.before = 2
  ) %>%
  kbl(digits = c(NA, NA, 5, 6, 2, 4), escape=FALSE,
    col.names = c("Modelo", "Termino", "Estimado", "Est. Desv.", "Estadístico", "$p$")
  ) %>%
  collapse_rows(columns = 1:2, valign = "middle")
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> Modelo </th>
   <th style="text-align:left;"> Termino </th>
   <th style="text-align:right;"> Estimado </th>
   <th style="text-align:right;"> Est. Desv. </th>
   <th style="text-align:right;"> Estadístico </th>
   <th style="text-align:right;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="6"> $GARCH(1,1)$ </td>
   <td style="text-align:left;"> $\phi_1$ </td>
   <td style="text-align:right;"> 0,29759 </td>
   <td style="text-align:right;"> 0,021971 </td>
   <td style="text-align:right;"> 13,54 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:right;"> 0,08831 </td>
   <td style="text-align:right;"> 0,015757 </td>
   <td style="text-align:right;"> 5,60 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_0$ </td>
   <td style="text-align:right;"> 0,00060 </td>
   <td style="text-align:right;"> 0,000081 </td>
   <td style="text-align:right;"> 7,37 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_1$ </td>
   <td style="text-align:right;"> 0,90329 </td>
   <td style="text-align:right;"> 0,155593 </td>
   <td style="text-align:right;"> 5,81 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\beta$ </td>
   <td style="text-align:right;"> 0,09570 </td>
   <td style="text-align:right;"> 0,045934 </td>
   <td style="text-align:right;"> 2,08 </td>
   <td style="text-align:right;"> 0,0372 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> GL </td>
   <td style="text-align:right;"> 2,92529 </td>
   <td style="text-align:right;"> 0,185317 </td>
   <td style="text-align:right;"> 15,79 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="7"> $GARCH(2,1)$ </td>
   <td style="text-align:left;"> $\phi_1$ </td>
   <td style="text-align:right;"> 0,29546 </td>
   <td style="text-align:right;"> 0,021879 </td>
   <td style="text-align:right;"> 13,50 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:right;"> 0,08854 </td>
   <td style="text-align:right;"> 0,015908 </td>
   <td style="text-align:right;"> 5,57 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_0$ </td>
   <td style="text-align:right;"> 0,00066 </td>
   <td style="text-align:right;"> 0,000031 </td>
   <td style="text-align:right;"> 21,17 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_1$ </td>
   <td style="text-align:right;"> 0,88327 </td>
   <td style="text-align:right;"> 0,151982 </td>
   <td style="text-align:right;"> 5,81 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_2$ </td>
   <td style="text-align:right;"> 0,11571 </td>
   <td style="text-align:right;"> 0,136311 </td>
   <td style="text-align:right;"> 0,85 </td>
   <td style="text-align:right;"> 0,3960 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\beta$ </td>
   <td style="text-align:right;"> 0,00000 </td>
   <td style="text-align:right;"> 0,128861 </td>
   <td style="text-align:right;"> 0,00 </td>
   <td style="text-align:right;"> 1,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> GL </td>
   <td style="text-align:right;"> 2,93151 </td>
   <td style="text-align:right;"> 0,185669 </td>
   <td style="text-align:right;"> 15,79 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
</tbody>
</table>

Los gráficos de diagnósticos muestran que la serie ahora se comporta como una normal, con _clusters_ de varianza desigual. Por otro lado, las ACF y PACF muestran menos correlaciones significativas, y que estas no son importantes ya que son menores a $0{,}06$. 
Esto lleva a concluir que el modelo ajustado es aceptable. 


```r
residuals_garch <- residuals(garch_fitting$mod_fit[[2]]) %>%
  fortify.zoo() %>%
  as_tsibble()

cowplot::plot_grid(
  autoplot(residuals_garch, ., colour="orange") +
    theme_light() + xlab("") + ylab("Innovaciones"),
  cowplot::plot_grid(
    ACF(residuals_garch, ., lag_max=50) %>%
      autoplot() + theme_light() +
      xlab("lag") + ylab("ACF"),
    PACF(residuals_garch, ., lag_max=50) %>%
      autoplot() + theme_light() +
      xlab("lag") + ylab("PACF"),
    nrow=2
  ),
  nrow=1
)
```

<img src="Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/garch-diagnostics-1.png" style="display: block; margin: auto;" />

El único problema que veo con el modelo es que no se captura el componente estacional de la serie. De forma que no se logran estimar los coeficientes autoregresivos estacionales, los cuales explicarían parte de las autocorrelaciones significativas en el ACF y PACF anterior. 
El modelo final escogido es:

$$
\begin{aligned}
  r_t &= \phi_1 r_{t-1} + \phi_2 r_{t-2} + \sigma_t\epsilon_t \\
  \sigma_t &= \alpha_0 + \alpha_1 r_{t-1} + \beta \sigma_{t-1}
\end{aligned}
$$

A continuación se muestra el gráfico de volatilidad (linea solida azul) junto con el valor absoluto de los retornos diarios (linea solida gris) para un modelo con distribución $t$-Student. Inicialmente, el modelo capturaba una pequeña parte de la volatilidad (no se observa el gráfico para la distribución normal), debido a que, como se observa en el _QQ plot_, el modelo correcto parece ser una distribución $t$-Student. 
Al cambiar el modelo, se observa que la mayoría de la volatilidad es capturada por el modelo. 


```r
cowplot::plot_grid(
  tsla_returns %>%
    mutate(sq_returns = abs(daily.returns)) %>%
    autoplot(sq_returns, colour="200", alpha = .6) +
    autolayer(sigma(garch_fitting$mod_fit[[2]]) %>%
      fortify.zoo() %>%
      as_tsibble(), ., colour="dodgerblue", linewidth = .5) +
    theme_light() +
    ylab("Volatilidad") + xlab(""),
  residuals_garch %>% 
    ggplot(aes(sample = .)) + 
    stat_qq() + stat_qq_line(color="red") + 
    theme_light(),
  nrow = 1)
```

<img src="Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/volatilidad-plot-1.png" style="display: block; margin: auto;" />

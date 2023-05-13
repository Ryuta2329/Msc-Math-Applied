# Sesión de Laboratorio: Modelos GARCH.

_Marcelo J Molinatti S_



La sesión de laboratorio trata de analizar los retornos de las acciones de Tesla, con la finalidad de obtener un modelo que explique la volatilidad de la serie, tal como se observa en la <a href="#tsla-series">figura 1</a>, a la derecha. 

## Descripción de serie TSLA.

Las acciones de TSLA mostrada en la <a href="#tsla-series">figura 1</a>, a la izquierda, permite ver que la la serie se mantuvo bastante estable durante toda la primera mitad de la serie, hasta el último trimestre del año 2019, momento a partir del cual la serie comienza a crecer rápidamente, debido a la atención de muchos inversionistas ya que la acción rebasó los 100 $USD y en el primer trimestre de 2020, a pesar de la emergencia sanitaria provocada por el COVID-19, la acción alcanzó su máximo histórico de aproximadamente 300 $USD. Luego, hubo una caída en la primera mitad del 2021, momento después del cual las acciones de TESLA alcanzaron un nuevo máximo que superó los 400 $USD. 

Respecto a los rendimientos registrados para TESLA, se pueden observar 4 _clusters_ de volatilidad en la serie: el primero se presenta a finales de 2015 (a principios de la serie) donde se registraron rendimientos de $\pm10$%; la siguiente aglomeración es en octubre de 2018 donde TESLA alcanzó a registrar rendimientos de $\pm15$% y, su _cluster_ de volatilidad más acentuado es durante el primer trimestre de 2020 cuando TESLA comenzó a registrar incrementos considerables llegando a obtener rendimientos de $\pm20$% en un solo día. Finalmente, un _cluster_ de volatilidad persistente que se registra a desde el último trimestre del 2021, hasta el final de la serie.

<a name="tsla-series"></a>

```r
suppressPackageStartupMessages({
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
})

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
  autoplot(tsla_close, TSLA.Close, colour="orange") +
    theme_light() + xlab("") + ylab("Precios de Cierre ($USD)") +
    ggtitle("Precio de Cierre de TESLA"),
  autoplot(tsla_returns, daily.returns, colour="orange") +
    theme_light() + xlab("") + ylab("Rendimiento") +
    ggtitle("Rendimientos logarítmicos de TESLA"),
  nrow=1
)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/tsla-series-1.png" alt="A la izquierda: Precio de Cierre de TESLA (Mayo 2015 - Mayo 2023). A la derecha: Rendimientos logarítmicos de TESLA (Mayo 2015 - Mayo 2023)."  />
</div><table><caption>Figura 1. A la izquierda: Precio de Cierre de TESLA (Mayo 2015 - Mayo 2023). A la derecha: Rendimientos logarítmicos de TESLA (Mayo 2015 - Mayo 2023).</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Los resultados de las pruebas para verificar que la serie es estacionaria mostrados en la <a href="#stationary-tests">tabla 1</a>, verifican que los rendimientos es una serie con un nivel de confianza del 95%. 

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
    escape=FALSE, 
    caption="Estadísticos de las pruebas de Raíz unitaria para estacionaridad."
  )
```

<table>
<caption>Tabla 1. Estadísticos de las pruebas de Raíz unitaria para estacionaridad.</caption>
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
   <td style="text-align:right;"> -13,1313999 </td>
   <td style="text-align:right;"> 0,01 </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Phillips-Perron Unit Root Test </td>
   <td style="text-align:right;"> -2414,0570294 </td>
   <td style="text-align:right;"> 0,01 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KPSS Test for Level Stationarity </td>
   <td style="text-align:right;"> 0,1058741 </td>
   <td style="text-align:right;"> 0,10 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
</tbody>
</table>

Al revisar las ACF y PACF de los retornos se puede observar que la ACF es bastante fluctuante y con correlaciones significativas que se extienden hasta retrasos grandes, y que decae en $9k$ ($k=1,2,3,\ldots$). 
Junto con la PACF, parece que el proceso es autoregresivo estacional de, al menos, segundo orden en el componente ARIMA, y de tercer orden con frecuencia $s=9$ en el componente estacional (ya que las correlaciones significativas en la PACF ocurren en $9P$, para $P=1, 2, y 3$ y luego se corta). 

<a name="acf-pacf"></a>

```r
acf_pacf_df <- tsla_returns |>
  ACF(daily.returns, lag_max = 50) |>
  within({
    pacf = PACF(tsla_returns, daily.returns,
      lag_max = 50)$pacf
  })

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
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/acf-pacf-1.png" alt="ACF y PACF de los rendimientos logarítmicos."  />
</div><table><caption>Figura 2. ACF y PACF de los rendimientos logarítmicos.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Es por ello que se escoge realizar un ajuste de los modelos $SARIMA(2,0,0)(2,0,0)_9$ y $SARIMA(2,0,0)(3,0,0)_9$ como alternativas posible para modelar los retornos diarios.

## Ajuste del modelo SARIMA.


```r
return_models <- tsla_returns %>%
  model(
    arima_model_1 = ARIMA(
      daily.returns ~ 0 + pdq(2,0,0) + PDQ(2,0,0,9),
      stepwise = FALSE),
    arima_model_2 = ARIMA(
      daily.returns ~ 0 + pdq(2,0,0) + PDQ(3,0,0,9),
      stepwise = FALSE))

tidied_model <- tidy(return_models) %>%
  filter(.model == "arima_model_1")
```

El modelo seleccionado al realizar el ajuste es el $SARIMA(2,0,0)(2,0,0)_9$, cuyos coeficientes se muestran en la <a href="#sarima-coef">tabla 2</a> junto con sus desviaciones estándar, se escribe como:

$$(1 - {0{,}057}_{(0{,}018)}B^9 {-0{,}048}_{(0{,}018)}B^{18})(1 - {0{,}211}_{(0{,}018)}B - {0{,}098}_{(0{,}018)}B^2)r_t = w_t$$

Los coeficientes muestran que los componentes del modelo ARIMA son importantes explicando las correlaciones encontradas, y de igual forma, los coeficientes del componente estacional son significativos, dado que se capturan las correlaciones importantes mostradas en la <a href="#acf-pacf">figura 2</a>, en $h=1, 2, 9, 10, 11$ y $18$.

<a name="sarima-coef"></a>

```r
tidied_model %>%
  select(-.model) %>%
  mutate(term = c("$\\phi_1$", "$\\phi_2$", "$\\Phi_1$", "$\\Phi_2$")) %>%
  kbl(digits = c(NA, 3, 3, 2, 4), escape=FALSE,
    col.names = c("Termino", "Estimado", "Est. Desv.", "Estadístico", "$p$"),
    caption="Estimados de los coeficientes de ajuste del modelo $SARIMA(2,0,0)(2,0,0)_9$."
  )
```

<table>
<caption>Tabla 2. Estimados de los coeficientes de ajuste del modelo SARIMA.</caption>
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
   <td style="text-align:right;"> 0,211 </td>
   <td style="text-align:right;"> 0,018 </td>
   <td style="text-align:right;"> 11,46 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:right;"> 0,098 </td>
   <td style="text-align:right;"> 0,018 </td>
   <td style="text-align:right;"> 5,30 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Phi_1$ </td>
   <td style="text-align:right;"> 0,057 </td>
   <td style="text-align:right;"> 0,018 </td>
   <td style="text-align:right;"> 3,11 </td>
   <td style="text-align:right;"> 0,0019 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Phi_2$ </td>
   <td style="text-align:right;"> -0,048 </td>
   <td style="text-align:right;"> 0,018 </td>
   <td style="text-align:right;"> -2,59 </td>
   <td style="text-align:right;"> 0,0095 </td>
  </tr>
</tbody>
</table>

Los diagnósticos para este modelo se muestran en la <a href="#res-diagnostics">figura 3</a>, donde se observan aun los _cluster_ de volatilidad en la serie de residuales, aunque es mas normal. El ACF y PACF tienen un comportamiento mejor, pero aun se observan correlaciones significativas no tomadas en cuenta en $h=5,8$ y $42$ pero cuyo valor es pequeño (menor a $0{,}05$).

<a name="res-diagnostics"></a>

```r
augmented_models <- return_models %>%
  select(arima_model_1) %>%
  augment()

cowplot::plot_grid(
  autoplot(augmented_models, .innov, colour="orange") +
    theme_light() + xlab("") + ylab("Innovaciones"),
  cowplot::plot_grid(
    ACF(augmented_models, .innov, lag_max=50) %>%
      autoplot() + theme_light() +
      xlab("lag") + ylab("ACF"),
    PACF(augmented_models, .innov, lag_max=50) %>%
      autoplot() + theme_light() +
      xlab("lag") + ylab("PACF"),
    nrow=2
  ),
  nrow=1
)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/res-diagnostics-1.png" alt="Gráfico temporal de residuales y ACF y PACF de los mismos, para el modelo SARIMA ajustado."  />
</div><table><caption>Figura 3. Gráfico temporal de residuales y ACF y PACF de los mismos, para el modelo SARIMA ajustado.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

## Evaluación de la volatilidad de la serie TSLA.

Utilizando una ventana de un mes, se calculó la varianza mensual de las innovaciones, así como el valor medio de los retornos al cuadrado, y se verificaron las relaciones entre la varianza de las innovaciones con respecto a los retornos cuadrados mensuales, y los retrasos (para $h=1, 2, 3$). 

La serie correspondiente a la volatilidad mensual (<a href="#volatility-month">figura 4</a>) muestra que la volatilidad es bastante variable con máximos que son más del doble de la volatilidad obtenida mínima registrada, con fluctuaciones anuales y otros patrones menos claros. 

<a name="volatility-month"></a>

```r
month_window_agg <- augmented_models %>%
  index_by(year_month = ~ yearmonth(.)) %>%
  summarise(
    month_var = var(.innov, na.rm = TRUE),
    mean_sq_return = mean(daily.returns ** 2, na.rm = TRUE)
  )

month_window_agg %>%
  autoplot(month_var, colour = "dodgerblue4") +
    theme_light() +
    ylab("Volatilidad") +
    xlab("")
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/volatility-month-1.png" alt="Gráficos de dispersión de la varianza mensual de las innovaciones con respecto a retrasos de los retornos al cuadrado."  />
</div><table><caption>Figura 4. Gráficos de dispersión de la varianza mensual de las innovaciones con respecto a retrasos de los retornos al cuadrado.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Se puede observar en la <a href="#returns-lag">figura 5</a> que existen correlaciones importantes entre la varianza y los retornos cuadrados ($0{,}98$), y también una correlación moderadamente pequeña ($0{,}51$) con respecto a los retornos cuadrados en $t-1$, y una correlación pequeña ($0{,}30$) con respecto al retraso en $t-2$.

<a name="returns-lag"></a>

```r
month_window_agg %$%
  lag2.plot(mean_sq_return, month_var, 3)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/returns-lag-1.png" alt="Gráficos de dispersión de la varianza mensual de las innovaciones con respectoa  retrasos de los retornos al cuadrado."  />
</div><table><caption>Figura 5. Gráficos de dispersión de la varianza mensual de las innovaciones con respectoa  retrasos de los retornos al cuadrado.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Además, en la <a href="#var-lags">figura 6</a> se muestra que existe una correlación moderada entre la varianza mensual de las innovaciones con respecto al retraso de la misma en $t-1$, y que las correlaciones subsiguientes, de $h=2$ a $6$ son pequeñas (alrededor de $0{,}15$ a $0{,}33$).

<a name="var-lags"></a>

```r
arch_test <- augmented_models %$%
  FinTS::ArchTest(.innov, lags = 4, demean = FALSE)

month_window_agg %$%
  lag1.plot(month_var, 9)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/var-lags-1.png" alt="Gráficos de dispersión de la varianza mensual de las innovaciones con respecto a retrasos de la misma."  />
</div><table><caption>Figura 6. Gráficos de dispersión de la varianza mensual de las innovaciones con respecto a retrasos de la misma.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Al realizar una prueba Arch por multiplicadores de Lagrange, al descomponer la varianza de la serie e identificar si sus rezagos son significativos, se obtiene que la serie tiene efectos Arch significativos ($\chi^2=$ 167,91, $p=$ 0).

## Ajuste del modelo GARCH.

Los resultados hasta el momento parecen indicar que se debe utilizar un modelo $SARIMA(2,0,0)(2,0,0)_9$ para los retornos diario, y un modelo $GARCH(2, 1)$ para la varianza condicional, sobre los residuos del modelo SARIMA. De igual forma, para evaluar los modelos en términos de la información proveída por cada uno, se ajustaran y compararan con el modelo $GARCH(2, 1)$, los modelos $GARCH(1, 0)$ y $GARCH(1, 1)$.

Los criterios de información mostrados adelante parecen indicar que los modelos con varianza condicional dependiente del retraso en $t-1$ de la misma, son preferibles al modelo $GARCH(1, 0)$, como se esperaba del análisis exploratorio anterior. 
Por otro lado, los mismo criterios de información sugieren la selección del modelo $GARCH(1, 1)$, dado los valores de estas medidas. 

<a name="inf-criteria"></a>

```r
garch_fitting <- tibble(list_pars = list(
    c(1, 0), c(1, 1), c(2, 1)
  )) %>%
  mutate(mod_spec = map(list_pars, 
    ~ugarchspec(mean.model = list(armaOrder=c(0, 0), include.mean = FALSE),
      variance.model=list(model = "sGARCH", garchOrder=.),
      distribution.model = "norm")
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
  kbl(digits = 4, escape=FALSE,
    caption="Criterios de información para los modelos GARCH ajustados.")
```

<table>
<caption>Tabla 3. Criterios de información para los modelos GARCH ajustados.</caption>
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
   <td style="text-align:right;"> -4,0064 </td>
   <td style="text-align:right;"> -4,0024 </td>
   <td style="text-align:right;"> -4,0050 </td>
   <td style="text-align:right;"> -4,0064 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $GARCH(1,1)$ </td>
   <td style="text-align:right;"> -4,0689 </td>
   <td style="text-align:right;"> -4,0628 </td>
   <td style="text-align:right;"> -4,0667 </td>
   <td style="text-align:right;"> -4,0689 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $GARCH(2,1)$ </td>
   <td style="text-align:right;"> -4,0680 </td>
   <td style="text-align:right;"> -4,0598 </td>
   <td style="text-align:right;"> -4,0650 </td>
   <td style="text-align:right;"> -4,0680 </td>
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
  	  "$\\alpha_0$", "$\\alpha_1$", "$\\beta$",
  	  "$\\alpha_0$", "$\\alpha_1$", "$\\alpha_2$", "$\\beta$"
  	), 
  	.before = 2
  ) %>%
  kbl(digits = c(NA, NA, 5, 6, 2, 4), escape=FALSE,
    col.names = c("Modelo", "Termino", "Estimado", "Est. Desv.", "Estadístico", "$p$"),
    caption="Estimados de los coeficiente para los modelos GARCH de orden $(p, q=1)$."
  ) %>%
  collapse_rows(columns = 1:2, valign = "middle")
```

<table>
<caption>Tabla 4. Estimados de los coeficiente para los modelos GARCH de orden (p, q=1).</caption>
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
   <td style="text-align:left;vertical-align: middle !important;" rowspan="3"> $GARCH(1,1)$ </td>
   <td style="text-align:left;"> $\alpha_0$ </td>
   <td style="text-align:right;"> 0,00003 </td>
   <td style="text-align:right;"> 0,000009 </td>
   <td style="text-align:right;"> 3,17 </td>
   <td style="text-align:right;"> 0,0015 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_1$ </td>
   <td style="text-align:right;"> 0,08335 </td>
   <td style="text-align:right;"> 0,014244 </td>
   <td style="text-align:right;"> 5,85 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\beta$ </td>
   <td style="text-align:right;"> 0,89569 </td>
   <td style="text-align:right;"> 0,019635 </td>
   <td style="text-align:right;"> 45,62 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;vertical-align: middle !important;" rowspan="4"> $GARCH(2,1)$ </td>
   <td style="text-align:left;"> $\alpha_0$ </td>
   <td style="text-align:right;"> 0,00003 </td>
   <td style="text-align:right;"> 0,000075 </td>
   <td style="text-align:right;"> 0,37 </td>
   <td style="text-align:right;"> 0,7077 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_1$ </td>
   <td style="text-align:right;"> 0,08343 </td>
   <td style="text-align:right;"> 0,010765 </td>
   <td style="text-align:right;"> 7,75 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\alpha_2$ </td>
   <td style="text-align:right;"> 0,00000 </td>
   <td style="text-align:right;"> 0,177558 </td>
   <td style="text-align:right;"> 0,00 </td>
   <td style="text-align:right;"> 1,0000 </td>
  </tr>
  <tr>
   
   <td style="text-align:left;"> $\beta$ </td>
   <td style="text-align:right;"> 0,89550 </td>
   <td style="text-align:right;"> 0,203507 </td>
   <td style="text-align:right;"> 4,40 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
</tbody>
</table>

Los gráficos de diagnósticos muestran que la serie ahora se comporta como una normal (similar a la observada para el modelo SARIMA ajustado antes), con _clusters_ de varianza desigual. Por otro lado, las ACF y PACF muestran menos correlaciones significativas, principalmente, las correspondientes a las del componente autoregresivo de segundo orden, y la correlación importante en $h=9$. 


```r
residuals_garch <- residuals(garch_fitting$mod_fit[[1]]) %>%
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

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/garch-diagnostics-1.png" alt="Grafico diagnostico de residuales del modelo $GARCH(1,1)$, junto a la ACF y PACF."  />
</div><table><caption>Figura 7. Grafico diagnostico de residuales del modelo GARCH(1,1), junto a la ACF y PACF.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Esto lleva a concluir que la volatilidad cambiante parece añadir correlaciones importantes que realmente no son necesarias, simplificando el modelo a solo un modelo de segundo orden.

<a name="volatilidad-plot"></a>

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
    stat_qq() + stat_qq_line(color="dodgerblue") + 
    theme_light(),
  nrow = 1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-GARCH-2_files/figure-html/volatilidad-plot-1.png" alt="Gráfico de volatilidad en conjunto con los retornos (en valor absoluto) y gráfico _QQ_ de los residuales del modelo $GARCH(1,1)$."  />
</div><table><caption>Figura 8. Gráfico de volatilidad en conjunto con los retornos (en valor absoluto) y gráfico <em>QQ</em> de los residuales del modelo GARCH(1,1).</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Un análisis mas profundo de los residuales muestra que la distribución de estos no es normal (<a href="#volatilidad-plot">figura 7</a>), sino que tiene colas pesadas. La forma es característica de una distribución $t$-Student sesgada (dado que una cola se ve más desviada que la otra). 
Se observa que el modelo paras la volatilidad no es capaz de tomar en cuenta toda la volatilidad asociada a la serie de retornos, dada la cobertura de la serie. 

Tomando en cuenta esta información, se ajusta un nuevo modelo $GARCH(1,1)$ y $GARCH(2,1)$ con componente autoregresivo de segundo orden para la media y modelo de distribución $t$-Student sesgada.


```r
garch_fitting <- tibble(
  list_pars = list(
    c(1, 1), c(2, 1)
  )) %>%
  mutate(mod_spec = map(list_pars, 
    ~ugarchspec(mean.model = list(armaOrder=c(2, 0), include.mean = FALSE),
      variance.model=list(model = "sGARCH", garchOrder=.),
      distribution.model = "sstd")
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
      `1` = "$GARCH(1,1)$",
      `2` = "$GARCH(2,1)$")) %>%
  kbl(digits = 4, escape=FALSE,
    caption="Criterios de información para los nuevos modelos GARCH ajustados usando un componente $ARMA(2,0)$ y modelo de distribución $t$-Student.")
```

<table>
<caption>Tabla 5. Criterios de información para los nuevos modelos GARCH ajustados usando un componente ARMA(2,0) y modelo de distribución t-Student.</caption>
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
   <td style="text-align:left;"> $GARCH(1,1)$ </td>
   <td style="text-align:right;"> -4,2690 </td>
   <td style="text-align:right;"> -4,2547 </td>
   <td style="text-align:right;"> -4,2638 </td>
   <td style="text-align:right;"> -4,2690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $GARCH(2,1)$ </td>
   <td style="text-align:right;"> -4,2683 </td>
   <td style="text-align:right;"> -4,2519 </td>
   <td style="text-align:right;"> -4,2624 </td>
   <td style="text-align:right;"> -4,2683 </td>
  </tr>
</tbody>
</table>

Como se observa, los nuevos modelos escogidos son preferibles a los ajustados anteriormente, siendo preferible, nuevamente, el modelo $GARCH(1,1)$. Los valores de los coeficientes para el modelo $GARCH(1,1)$ se muestran en la <a href="#coef-garch-2">tabla 5</a>, donde se observa todos son significativos.
También ha de notarse que los coeficientes estimados para el componente autoregresivo son similares a los estimados anteriormente para el modelo SARIMA. 
La distribución subyacente parece ser una $t$-Student con 3 grados de libertad y un sesgo significativo de aproximadamente 1. 

<a name="coef-garch-2"></a>

```r
garch_fitting$mod_fit[[1]]@fit$matcoef %>%
  as_tibble() %>%
  mutate(
    par = c("$\\phi_1$", "$\\phi_2$", "$\\alpha_0$", "$\\alpha_1$", "$\\beta$", "skew", "GL"), 
    .before = 1
  ) %>%
  kbl(digits = c(NA, 5, 6, 2, 4), escape=FALSE,
    col.names = c("Termino", "Estimado", "Est. Desv.", "Estadístico", "$p$"),
    caption="Coeficientes estimados para el modelo final escogido."
  )
```

<table>
<caption>Tabla 6. Coeficientes estimados para el modelo final escogido.</caption>
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
   <td style="text-align:right;"> 0,29634 </td>
   <td style="text-align:right;"> 0,022185 </td>
   <td style="text-align:right;"> 13,36 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:right;"> 0,08741 </td>
   <td style="text-align:right;"> 0,015792 </td>
   <td style="text-align:right;"> 5,54 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_0$ </td>
   <td style="text-align:right;"> 0,00060 </td>
   <td style="text-align:right;"> 0,000082 </td>
   <td style="text-align:right;"> 7,33 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_1$ </td>
   <td style="text-align:right;"> 0,90402 </td>
   <td style="text-align:right;"> 0,156583 </td>
   <td style="text-align:right;"> 5,77 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\beta$ </td>
   <td style="text-align:right;"> 0,09495 </td>
   <td style="text-align:right;"> 0,045932 </td>
   <td style="text-align:right;"> 2,07 </td>
   <td style="text-align:right;"> 0,0387 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> skew </td>
   <td style="text-align:right;"> 0,99129 </td>
   <td style="text-align:right;"> 0,020451 </td>
   <td style="text-align:right;"> 48,47 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GL </td>
   <td style="text-align:right;"> 2,92530 </td>
   <td style="text-align:right;"> 0,186940 </td>
   <td style="text-align:right;"> 15,65 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
</tbody>
</table>

El modelo final escogido es:

$$
\begin{aligned}
  r_t &= \phi_1 r_{t-1} + \phi_2 r_{t-2} + \sigma_t\epsilon_t \\
  \sigma_t &= \alpha_0 + \alpha_1 r_{t-1} + \beta \sigma_{t-1}
\end{aligned}
$$

con errores distribuidos siguiendo una $t$-Student con parámetros como se especifican arriba. 

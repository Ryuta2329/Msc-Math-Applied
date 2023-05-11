# Sesión de Laboratorio: Modelos SARIMA.

_Marcelo J Molinatti S_


Elija una de las siguientes series temporales estacionales: ```condmilk```, ```hsales```, ```usolec```  

* ✅ ¿Es necesario transformar los datos? Si es así, encuentre un transformación adecuado.
* ✅ ¿Son estacionarios los datos? Si no, encuentre una diferenciación apropiada que produce datos estacionarios.
* ✅ Identifique un par de modelos ARIMA que podrían ser útiles en describir la serie de tiempo. ¿Cuál de sus modelos es el mejor de acuerdo con sus valores $AIC$?
* ✅ Estime los parámetros de su mejor modelo y haga un diagnóstico de los residuos. ¿Los residuos se parecen al ruido blanco? Si no es así, intente encontrar otro modelo ARIMA que le quede mejor.
* ✅ Pronostique los próximos 24 meses de datos usando su modelo preferido.
* ✅ Compare los pronósticos obtenidos, usando ```ets()```.

## Descripción de la serie.


```r
library(tsibble)
library(dplyr)
library(fable)
library(feasts)

# Manufacturer's Stocks of evaporated and sweetened condensed milk.
data(condmilk, package="fma")
condmilk <- as_tsibble(condmilk)
```

Los datos seleccionados corresponden a una serie temporal de Inventarios de Manufactura de leche condensada evaporada y endulzada, recolectados mensualmente desde 1971 a 1980, como se muestra en el gráfico de la <a href="#tseries-plot">figura 1</a>.

<a name="tseries-plot"></a>
```r
# Grafico de la serie temporal
autoplot(condmilk, colour="dodgerblue3") +
  geom_point(aes(y=value), colour="dodgerblue3") + 
  theme_light() + 
  geom_line(
    aes(y=ksmooth(time(condmilk %>% as.ts()), condmilk %>% as.ts(), "normal", bandwidth=.5)$y), 
    colour="200", lwd=1) +
  geom_smooth(aes(y=value), method=loess, se=FALSE, color="200", linetype=2) +
  xlab('Tiempo') + ylab('Numero de Unidades')
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/tseries-plot-1.png" alt="Cambio temporal en el número de unidades de leche condensada entre 1971-1980. Se muestra el patrón anual y la estacionaridad de la serie usando un suavizado de _kernel_ (línea sólida gris) y _loess_ (línea fragmentada gris), respectivamente."  />
<p class="caption">Figura 1. Cambio temporal en el número de unidades de leche condensada entre 1971-1980. Se muestra el patrón anual y la estacionaridad de la serie usando un suavizado de <em>kernel</em> (línea sólida gris) y <em>loess</em> (línea fragmentada gris), respectivamente.</p>

En el gráfico se observa claramente dos componentes estacionales: uno anual obvio, que fluctúa ligeramente, observándose una disrrupción a mediados de 1973 que rompe de alguna manera el patrón unimodal de los picos anuales; y también parece haber un componente trimestral de repeticiones de picos y valles importantes, cuya amplitud disminuye con el tiempo, indicando un amortiguamiento de las variaciones o fluctuaciones importantes en la serie. 

Se resaltan en el componente trimestral: 
_i)_ picos de mayor magnitud cada tres años, a mediados de los años 1971, 1974 y 1977, mostrando un cambio periódico en la amplitud de los ciclos (aunque no se observa pico a mediados de 1980); 
_ii)_ valles profundos cada 3 años, en los años 1973, 1976 y 1979, con una ligera fluctuación mensual, en donde el número de unidades cae por debajo de las 50 unidades y cuya amplitud, al igual que la de los picos, se va amortiguando cada vez que aparece.

También se observa una pequeña tendencia decreciente durante la primera parte de la serie, que es muy perceptible debido al comportamiento anormal durante el año 1973; pero que desaparece durante la segunda parte de la serie. 

## Transformación de la serie.

Dada la descripción anterior, se decide optar por transformar los datos usando una función logarítmica, con la finalidad de hacer mas obvias las desviaciones importantes que aparecen en la serie; y en lugar de modelar la serie original transformada, se modela la serie que resulta de la diferencia de valores adyacentes (elimina el componente con tendencia en la primer tercio de la serie original):

$$log(x_t) - log(x_{t-1}) = log(\frac{x_t}{x_{t-1}}) = log(1 + r_t)$$

donde $r_t$ es el incremento o decremento proporcional de la unidad en el año $t$ con respecto al valor en el año anterior, $t-1$. Dado que la magnitud de $r_t$ es pequeña, se puede aproximar $log(1 + r_t) \approx r_t$. La serie $r_t$ se muestra en la <a href="#transform">figura 2</a>.

<a name="transform"></a>
```r
# Transformando: se usa el log en base 10
cm_transf <- condmilk %>%
  mutate(prop_change=difference(log10(value)), .keep = "unused") %>%
  tidyr::drop_na()

# This one is for functions outside tidy packages
univariate_ts <- cm_transf %>%
  as.ts() %>%
  zoo::na.approx()

autoplot(cm_transf, colour="dodgerblue3") +
  geom_point(aes(y=prop_change), colour="dodgerblue3") + 
  theme_light() + 
  geom_line(
    aes(y=ksmooth(time(cm_transf %>% as.ts()), cm_transf %>% as.ts(), "normal", bandwidth=.5)$y), 
    colour="200", lwd=1) +
  geom_smooth(aes(y=prop_change), method=loess, se=FALSE, color="200", linetype=2) + 
  xlab('Tiempo') + ylab('Cambio proporcional')
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/transform-1.png" alt="Cambio proporcional anual en las unidades de leche condensada. Se muestra el patrón anual y la estacionaridad de la serie usando un suavizado de _kernel_ (línea sólida gris) y _loess_ (línea fragmentada gris), respectivamente."  />
</div><table><caption>Figura 2. Cambio proporcional anual en las unidades de leche condensada. Se muestra el patrón anual y la estacionaridad de la serie usando un suavizado de <em>kernel</em> (línea sólida gris) y <em>loess</em> (línea fragmentada gris), respectivamente.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Como se muestra, el patrón anual persiste en la serie, y se hacen mas visibles las desviaciones que antes estaban ocultas por el componente estacional. Dos observaciones destacan como valores atípicos en la serie, al inicio del primer pico a mediados de 1971, y en el primer valle en 1973. 

## Elección de un modelo.

Para un primer análisis de la correlación serial de la serie, verificamos las ACF y PACF (<a href="#acf-pacf">figura 3</a>).

<a name="acf-pacf"></a>
```r
adf_test <- tseries::adf.test(univariate_ts)

cm_transf %>%
  ACF(prop_change, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$")) -> acf

cm_transf %>%
  PACF(prop_change, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$"))-> pacf

cowplot::plot_grid(acf, pacf, nrow=1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/acf-pacf-1.png" alt="ACF y PACF de la serie original"  />
</div><table><caption>Figura 3. ACF y PACF de la serie original</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

* 💹 La ACF indica que seria apropiada una diferencia de orden $D=1$ para el componente estacional, y un parámetro autoregresivo de orden $P=1$. También se observa que el periodo debería ser $s=6$, dado que los picos de mayor magnitud se registran en $6k$ para $k=1,2,\ldots$. Sin embargo, como el patrón es anual, se elige un periodo de $s=12$ dado que en un intervalo de 12 meses se completa un ciclo en la ACF.
* 💹 La PACF confirma que se debería elegir $P=1$, dada la correlación significativa en el $h=6$, pero en más ningún múltiplo de 6.
* 💹 Dado que la ACF es decayente en los _lags_ mostrados, y que la PACF se corta en el $h=1$, se usa un orden $p=1$ para la parte $ARMA$.
* 💹 Se observan un par de correlaciones significativas en la PACF, donde la correlación en $h=4$ es la mas importante. La otra se encuentra en $h=22$. Estas correlaciones aparecen significativas probablemente debido al patrón anual oscilante combinado con los cambios abruptos durante los picos y valles de la serie: en estos _lags_ se encuentran espaciados casi 2 años, y coinciden con la entrada a los picos y valles de la serie original.

Los resultados de la prueba de Dickey-Fuller aumentada para comprobar estacionaridad de la serie muestran que el incremento proporcional de unidades es estacionaria ($DF_T =$ -6,95, $p =$ 0,01), por lo que no es necesario una diferencia en el componente $ARMA$. 

Se ajusta entonces un modelo $ARIMA(1,0,0)(0,1,1)_{12}$, y se compara con un modelo completo estimado por MLE minimizando la sumatoria de cuadrados condicional, encontrado por medio de una búsqueda del mejor modelo en el espacio de parámetros (mejor en el sentido de sumatoria de cuadrados mínima).


```r
full <- cm_transf %>%
  model(non_stationaty=ARIMA(prop_change, 
      stepwise=FALSE, 
      method="CSS-ML", optim.method="BFGS"))
```

Se escribe el modelo inferido de los descriptivos:

$$
\begin{aligned}
  \Delta_{12}\phi(B)r_t = \Theta(B) w_t  & \Rightarrow \\
    & \Rightarrow (1 - B^12)(1 - \phi B) r_t = (1 - \Theta B) w_t \\
\end{aligned}
$$

el cual se expande como:

<a name="eq-model"></a>
$$r_t = \phi r_{t-1} + r_{t-12} + \phi r_{t-13} + w_t + \Theta w_{t-1}$$

### Estadísticos de Bondad de Ajuste.

Los resultados de los ajustes se muestran en la <a href="#fitting-sarima">tabla 1</a>, los cuales indican que el modelo dado en la ecuación <a href="#eq-model">1</a> no difiere demasiado del modelo encontrado al tantear el espacio de parámetros, en términos de robustez y precisión, dada la varianza residual, $RMSE$ y $MAE$ similares. 

Además, el modelo completo parece ser preferible según los valores obtenidos en términos de la información proveída por el modelo ($AIC$, $AICc$ y $BIC$). Esto se debe (dada la similitud de varianzas residuales), solo al aumento en la penalización consecuencia de la mayor cantidad de parámetros estimados en el modelo completo.


```r
selected_mod <- cm_transf %>%
  model(`ARMA(1,0,0)(0,1,1)_{12}`=ARIMA(prop_change ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 1, 12), 
    stepwise=FALSE, method="CSS-ML", optim.method="BFGS")) 

all_models <- selected_mod %>%
  bind_cols(select(full, non_stationaty))

box_test <- augment(selected_mod) %>%
  features(.innov, ljung_box, dof = 4, lag = 12)

information_based <- glance(all_models) %>%
  select(-ar_roots, -ma_roots)

error_based <- accuracy(all_models)

.model <- c("$ARIMA(1,0,0)(0,1,1)_{12}$", "$ARIMA(2,0,2)(0,1,1)_{12}$")

information_based %>%
  left_join(error_based) %>%
  select(sigma2:BIC, ME:MAE) %>%
  tibble::add_column(.model=.model, .before=1) %>%
  knitr::kable(digits=c(NA, 4, 2, 1, 1, 1, 4, 4, 4), 
    align='lcccccccccc', escape=FALSE,
    col.names=c("Modelo", "$\\sigma^2$", "Func. Verosim.", "AIC", "AICc", "BIC", "ME", "RMSE", "MAE"),
    caption="Estadísticos de bondad de ajuste de los modelos SARIMA ajustados.")
```

<a name="fitting-sarima"></a>
<table>
<caption>Tabla 1. Estadísticos de bondad de ajuste de los modelos SARIMA ajustados.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Modelo </th>
   <th style="text-align:center;"> $\sigma^2$ </th>
   <th style="text-align:center;"> Func. Verosim. </th>
   <th style="text-align:center;"> AIC </th>
   <th style="text-align:center;"> AICc </th>
   <th style="text-align:center;"> BIC </th>
   <th style="text-align:center;"> ME </th>
   <th style="text-align:center;"> RMSE </th>
   <th style="text-align:center;"> MAE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $ARIMA(1,0,0)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 0,0034 </td>
   <td style="text-align:center;"> 143,79 </td>
   <td style="text-align:center;"> -281,6 </td>
   <td style="text-align:center;"> -281,4 </td>
   <td style="text-align:center;"> -273,6 </td>
   <td style="text-align:center;"> 0,0005 </td>
   <td style="text-align:center;"> 0,0547 </td>
   <td style="text-align:center;"> 0,0372 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(2,0,2)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 0,0033 </td>
   <td style="text-align:center;"> 148,93 </td>
   <td style="text-align:center;"> -285,9 </td>
   <td style="text-align:center;"> -285,0 </td>
   <td style="text-align:center;"> -269,8 </td>
   <td style="text-align:center;"> 0,0007 </td>
   <td style="text-align:center;"> 0,0531 </td>
   <td style="text-align:center;"> 0,0376 </td>
  </tr>
</tbody>
</table>

### Gráficos diagnósticos de residuales.

Al verificar el comportamiento de los residuales para el modelo candidato, $ARIMA(1,0,0)(0,1,1)_{12}$, se obtiene que los residuales no parecen estar correlacionados entre si, evaluando con respecto al retraso 12, aunque el valor de probabilidad obtenido es apenas marginal ($Q=15,289$, $p=0,0537708$). 
En el gráfico para ACF y PACF se muestra un pico de correlación significativa con respeto al _lag_ 5, lo cual indica la falta de un retraso en el modelo considerado. 

<a name="fig-diagnostics-plots"></a>
```r
res_sd <- information_based %>% 
  filter(.model != "non_stationaty") %>% 
  pull(sigma2) %>%
  sqrt()
augmented_data <- augment(selected_mod) %>%
  mutate(.std.resid=.resid / res_sd)

augmented_data %>%
  ACF(.innov, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$")) -> acf

augmented_data %>%
  PACF(.innov, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$"))-> pacf 

res_series <- augmented_data %>%
  autoplot(.std.resid, colour="orange") +
  geom_point(aes(y=.std.resid), colour="orange") +
  scale_x_yearmonth(date_labels = "%Y") +
  xlab("Tiempo") +
  ylab("Residuales Estandarizados") + 
  geom_hline(yintercept=c(0, 1.96, -1.96), 
    color=c("black", 200, 200), linetype=c(1, 2, 2)) +
  theme_light() 

res_qq_plot <- augmented_data %>% 
  ggplot(aes(sample = .resid)) + 
  stat_qq() + stat_qq_line(color="red") + 
  theme_light()

cowplot::plot_grid(acf, pacf, res_series, res_qq_plot, 
  nrow=1, align="h",
  labels=c("a)", "b)", "c)", "d)"), 
  label_size=11, 
  label_fontface="italic")
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/diagnostics-plots-1.png" alt="Gráficos diagnósticos de residuales: _a)_ ACF, _b)_ PACF, _c)_ gráficos de residuales, y _d)_ gráfico _QQ_"  />
</div><table><caption>Figura 4. Gráficos diagnósticos de residuales: <em>a)</em> ACF, <em>b)</em> PACF, <em>c)</em> gráficos de residuales, y <em>d)</em> gráfico <em>QQ</em></caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Más aun, el gráfico de residuales estandarizados y el gráfico _QQ_ muestran claramente que los residuales no son normales. 
Se observa un patrón no aleatorio de distribución de los residuales alrededor de la media (secciones donde las observaciones caen mucho por encima de la media, y secciones donde caen por debajo). Además, se pueden notar observaciones atípicas, 8 de ellas específicamente, como se muestra en la <a href="#tab-outliers">tabla 2</a>. 
Se puede notar claramente que 3 de ellas caen en 1973, donde el comportamiento de la serie es anormal comparado con los otros años, y el resto corresponden a puntos de entrada a los picos y a los picos observados luego de 1973.


```r
augmented_data %>%
  filter(
    .resid < quantile(.resid, 0.25) - 1.5 * IQR(.resid) |
    .resid > quantile(.resid, 0.75) + 1.5 * IQR(.resid)
  ) %>%
  select(-.resid, -.model) %>%
  knitr::kable(digits=c(0, 4, 4, 3, 3, 2),
    escape=FALSE, align='lcccccc',
    col.names=c("Año", "$r_t$", "Predicho", "Residuo", "Res. Estand."),
    caption="Observaciones atípicas registradas para los cambios proporcionales en el número de unidades en inventario de leche condensada.")
```
<a name="tab-outliers"></a>
<table>
<caption>Tabla 2. Observaciones atípicas registradas para los cambios proporcionales en el número de unidades en inventario de leche condensada.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Año </th>
   <th style="text-align:center;"> $r_t$ </th>
   <th style="text-align:center;"> Predicho </th>
   <th style="text-align:center;"> Residuo </th>
   <th style="text-align:center;"> Res. Estand. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1972 abr </td>
   <td style="text-align:center;"> 0,0969 </td>
   <td style="text-align:center;"> -0,0623 </td>
   <td style="text-align:center;"> 0,159 </td>
   <td style="text-align:center;"> 2,735 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1973 mar </td>
   <td style="text-align:center;"> -0,1899 </td>
   <td style="text-align:center;"> -0,0469 </td>
   <td style="text-align:center;"> -0,143 </td>
   <td style="text-align:center;"> -2,457 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1973 abr </td>
   <td style="text-align:center;"> 0,1979 </td>
   <td style="text-align:center;"> 0,0543 </td>
   <td style="text-align:center;"> 0,144 </td>
   <td style="text-align:center;"> 2,467 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1973 ago </td>
   <td style="text-align:center;"> -0,2990 </td>
   <td style="text-align:center;"> -0,0383 </td>
   <td style="text-align:center;"> -0,261 </td>
   <td style="text-align:center;"> -4,477 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1974 mar </td>
   <td style="text-align:center;"> 0,0338 </td>
   <td style="text-align:center;"> -0,0807 </td>
   <td style="text-align:center;"> 0,115 </td>
   <td style="text-align:center;"> 1,967 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1974 ago </td>
   <td style="text-align:center;"> 0,0297 </td>
   <td style="text-align:center;"> -0,0885 </td>
   <td style="text-align:center;"> 0,118 </td>
   <td style="text-align:center;"> 2,032 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1976 mar </td>
   <td style="text-align:center;"> 0,0635 </td>
   <td style="text-align:center;"> -0,0613 </td>
   <td style="text-align:center;"> 0,125 </td>
   <td style="text-align:center;"> 2,145 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1977 ago </td>
   <td style="text-align:center;"> 0,0761 </td>
   <td style="text-align:center;"> -0,0359 </td>
   <td style="text-align:center;"> 0,112 </td>
   <td style="text-align:center;"> 1,924 </td>
  </tr>
</tbody>
</table>

Todo lo mencionado, parece indicar que aun existe una estructura de dependencia dentro de la serie que no se ha especificado, y que se debe optar por un modelo distinto. Los resultados parecen indicar que se debe considerar un modelo con el quinto _lag_ como regresor.

### Análisis de correlaciones de $r_t$ y $r_{t-h}$.

Dado los resultados encontrados para el primer modelo ajustado, se decide hacer un análisis de correlación entre la serie temporal con respecto a las series temporales retrasadas (<a href="#fig-bivariate-lag">figura 5</a>).

<a name="fig-bivariate-lag"></a>
```r
tscleaned <- augmented_data %>%
  filter(
    !(.std.resid < quantile(.std.resid, 0.25) - 1.96 * IQR(.std.resid)) |
    !(.std.resid > quantile(.std.resid, 0.75) + 1.96 * IQR(.std.resid))
  )

tscleaned %>%
  gg_lag(prop_change, period=12, lags=1:12, geom="point") +
  theme_light() +
  theme(legend.position="none") +
  facet_wrap(~.lag, 
    labeller=as_labeller(setNames(paste("h =", 1:12), paste("lag", 1:12)))) +
  xlab('lag(Cambio proporcional, h)') + 
  ylab('Cambio proporcional')
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/bivariate-lag-1.png" alt="Gráficos bivariados que relacionan la serie en $t$ con su valor retrasado en $t-h$, para $h=1$ a $12$."  />
</div><table><caption>Figura 5. Gráficos bivariados que relacionan la serie en $t$ con su valor retrasado en $t-h$, para $h=1$ a $12$.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La dependencia de la serie en $t$ con respecto a los valores en $t-1$ se toma en cuenta dentro del modelo ajustado antes, al considerar un modelo autoregresivo de orden $p=1$, al igual que la correlación positiva con respecto al 12vo retraso. 
La correlación con $h=3$ no es significativa, y en $h=2$ y $h=4$ las correlaciones son pequeñas, comparadas con las correlaciones observadas en $h=5$ y $h=6$ (las cuales son negativas y más importantes), y que parecen ser no lineales para valores mayores a $0{,}05$. 

Para estas variables retrasadas, la correlación negativa puede modelarse de dos formas: _i)_ considerando un solo modelo con regresores $r_{t-5}$ y $r_{t-5}^2$, dada la curvatura y forma no lineal de la serie; o _ii)_ generando un modelo a trozos con un regresor lineal $r_{t-6}$, cuyo coeficiente asociado cambia dependiendo de si $r_t$ es mayor a $0{,}05$. 

## Regresión con variables retrasadas.

Se busca modelar, en el primer caso, un modelo de la forma:

<a name="eq-model-reg"></a>
$$r_t = \phi r_{t-1} + \phi r_{t-12} + \phi r_{t-13} + \beta_1 r_{t-5} + \beta_2 r_{t-5}^2 + w_t + \Theta w_{t-1}$$

donde $\beta_1$ y $\beta_2$ son los coeficientes de regresión. En el segundo caso, se necesita de una variable _dummy_ $D_{t-5}$ la cual es 0 si $r_{t-6} < 0{,}05$ y 1 de otra forma, generando el modelo a trozos:

<a name="eq-model-reg-trozos"></a>
$$r_t = \begin{cases}\omega + \beta_1 r_{t-5} & r_{t-5} < 0,05 \\ \omega + \beta_2 + (\beta_1 + \beta_3) r_{t-5} & r_{t-5} \ge 0,05 \end{cases}$$

donde $\omega = \phi r_{t-1} + \phi r_{t-12} + \phi r_{t-13} + w_t + \Theta w_{t-1}$ es el componente ARIMA y, de nuevo, $\beta_1$ y $\beta_2$ son los coeficientes de regresión para $r_{t-5}$ y $D_{t-5}$, y $\beta_3$ es el coeficiente de la interacción $D_{t-5}r_{t-5}$.

### Estadísticos de Bondad de Ajuste.

```r
ts_w_lagged_reg <- cm_transf[-(1:5),] %>%
  bind_cols(
    ts.intersect(as.ts(cm_transf), 
    pcL5=as.ts(cm_transf) %>% stats::lag(-5),
    dL5=ifelse(as.ts(cm_transf) < .05, 0, 1) %>% stats::lag(-5), dframe=TRUE) %>%
    select(pcL5, dL5)
  ) %>%
  mutate(pcL5sq=pcL5 ** 2)

new_models <- ts_w_lagged_reg %>%
  model(
    `Lagged with sq`=ARIMA(prop_change ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 1, 12) + pcL5 + pcL5sq, 
      stepwise=FALSE, method="CSS-ML", optim.method="BFGS"),
  `Lagged by pieces`=ARIMA(prop_change ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 1, 12) + pcL5 * dL5, 
    stepwise=FALSE, method="CSS-ML", optim.method="BFGS")) %>%
  bind_cols(all_models)

information_based_2 <- new_models %>%
  glance() %>%
  arrange(AIC) %>%
  select(-ar_roots, -ma_roots)

error_based_2 <- accuracy(new_models)

models_xreg <- c(.model, 
  "$ARIMA(1,0,0)(0,1,1)_{12} \\text{ (no lineal con }r_{t-5}^2\\text{)}$", 
  "$ARIMA(1,0,0)(0,1,1)_6 \\text{ (a trozos)}$")

information_based_2 %>%
  left_join(error_based_2) %>%
  select(sigma2:BIC, ME:MAE) %>%
  tibble::add_column(model=models_xreg, .before=1) %>%
  knitr::kable(digits=c(NA, 4, 2, 1, 1, 1, 4, 4, 4), 
    align='lcccccccccc', escape=FALSE,
    col.names=c("Modelo", "$\\sigma^2$", "Func. Verosim.", "AIC", "AICc", "BIC", "ME", "RMSE", "MAE"),
    caption="Estadísticos de bondad de ajuste de los modelos SARIMA ajustados.")
```

<a name="tab-lagged-regression"></a>
<table>
<caption>Tabla 3. Estadísticos de bondad de ajuste de los modelos SARIMA ajustados.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Modelo </th>
   <th style="text-align:center;"> $\sigma^2$ </th>
   <th style="text-align:center;"> Func. Verosim. </th>
   <th style="text-align:center;"> AIC </th>
   <th style="text-align:center;"> AICc </th>
   <th style="text-align:center;"> BIC </th>
   <th style="text-align:center;"> ME </th>
   <th style="text-align:center;"> RMSE </th>
   <th style="text-align:center;"> MAE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $ARIMA(1,0,0)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 0,0033 </td>
   <td style="text-align:center;"> 148,93 </td>
   <td style="text-align:center;"> -285,9 </td>
   <td style="text-align:center;"> -285,0 </td>
   <td style="text-align:center;"> -269,8 </td>
   <td style="text-align:center;"> 0,0007 </td>
   <td style="text-align:center;"> 0,0531 </td>
   <td style="text-align:center;"> 0,0376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(2,0,2)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 0,0026 </td>
   <td style="text-align:center;"> 147,67 </td>
   <td style="text-align:center;"> -285,3 </td>
   <td style="text-align:center;"> -284,7 </td>
   <td style="text-align:center;"> -272,2 </td>
   <td style="text-align:center;"> -0,0002 </td>
   <td style="text-align:center;"> 0,0471 </td>
   <td style="text-align:center;"> 0,0339 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(1,0,0)(0,1,1)_{12} \text{ (no lineal con }r_{t-5}^2\text{)}$ </td>
   <td style="text-align:center;"> 0,0026 </td>
   <td style="text-align:center;"> 148,38 </td>
   <td style="text-align:center;"> -284,8 </td>
   <td style="text-align:center;"> -283,9 </td>
   <td style="text-align:center;"> -269,0 </td>
   <td style="text-align:center;"> 0,0006 </td>
   <td style="text-align:center;"> 0,0468 </td>
   <td style="text-align:center;"> 0,0336 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(1,0,0)(0,1,1)_6 \text{ (a trozos)}$ </td>
   <td style="text-align:center;"> 0,0034 </td>
   <td style="text-align:center;"> 143,79 </td>
   <td style="text-align:center;"> -281,6 </td>
   <td style="text-align:center;"> -281,4 </td>
   <td style="text-align:center;"> -273,6 </td>
   <td style="text-align:center;"> 0,0005 </td>
   <td style="text-align:center;"> 0,0547 </td>
   <td style="text-align:center;"> 0,0372 </td>
  </tr>
</tbody>
</table>

Los estadístico de bondad de ajuste muestran que las varianzas residuales de los modelos mostrados en las ecuaciones <a href="#eq-model-reg">2</a> y <a href="#eq-model-reg-trozos">3</a> son menores a la varianza residual del modelo $ARIMA(1,0,0)(0,1,1)_6$ sin regresores. 
De igual forma, las demás medidas basadas en residuales RMSE, MAE y ME, no varían demasiado con respecto a los encontrados para el primer modelo ajustado, pero son, claro, menores. 
Por otro lado, los valores de AIC y BIC para el modelo de regresión a trozos y el que usa una dependencia cuadrática son menores que los del primer modelo ajustado anteriormente (sin regresores). 

### Gráficos diagnósticos de residuales.

Los gráficos de residuales se muestran en la <a href="#fig-diagnostics-plots-2">figura 6</a> para el modelo de la ecuación <a href="#eq-model-reg">2</a>, con la dependencia cuadrática (el cual se selecciona al revisar los residuales). 
Se puede observar que ya no existen correlaciones significativas, aunque se observa, principalmente en el PACF, que hay una cantidad inusualmente grande de correlaciones negativas en una sección en la primera mitad del gráfico. 

<a name="fig-diagnostics-plots-2"></a>
```r
res_sd <- information_based_2 %>% 
  filter(.model == "Lagged with sq") %>%
  pull(sigma2) %>%
  sqrt()
augmented_data_2 <- new_models %>%
  select(`Lagged with sq`) %>%
  augment() %>%
  mutate(.std.resid=.resid / res_sd)

augmented_data_2 %>%
  ACF(.innov, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$")) -> acf

augmented_data_2 %>%
  PACF(.innov, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$"))-> pacf 

res_series <- augmented_data_2 %>%
  autoplot(.std.resid, colour="orange") +
  geom_point(aes(y=.std.resid), colour="orange") +
  scale_x_yearmonth(date_labels = "%Y") +
  xlab("Tiempo") +
  ylab("Residuales Estandarizados") + 
  geom_hline(yintercept=c(0, 1.96, -1.96), 
    color=c("black", 200, 200), linetype=c(1, 2, 2)) +
  theme_light() 

res_qq_plot <- augmented_data_2 %>% 
  ggplot(aes(sample = .resid)) + 
  stat_qq() + stat_qq_line(color="red") + 
  theme_light()

cowplot::plot_grid(acf, pacf, res_series, res_qq_plot, 
  nrow=1, align="h",
  labels=c("a)", "b)", "c)", "d)"), 
  label_size=11, 
  label_fontface="italic")
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/diagnostics-plots-2-1.png" alt="Gráficos diagnósticos de residuales para el modelo regresivo con la dependencia cuadratica con $r_{t-5}$: _a)_ ACF, _b)_ PACF, _c)_ gráficos de residuales, y _d)_ gráfico _QQ_"  />
</div><table><caption>Figura 6. Gráficos diagnósticos de residuales para el modelo regresivo con la dependencia cuadratica con $r_{t-5}$: <em>a)</em> ACF, <em>b)</em> PACF, <em>c)</em> gráficos de residuales, y <em>d)</em> gráfico <em>QQ</em></caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Por otro lado, el gráfico _QQ_ muestra una mejora significativa en el comportamiento de los residuales, comparado con el modelo anterior. La distribución de estos alrededor de la media se percibe mas aleatoria, y a excepción de los atípicos, se ajustan bien a la recta teórica en el gráfico _QQ_. 
Aun es posible observar quiebres estructurales consecuencia de las observaciones atípicas: en total se observan 5 atípicos, con más de dos desviaciones estándar, y al menos 9-10 observaciones con desviaciones importantes dentro del intervalo de una desviación estándar. 


```r
augmented_data_2 %>%
  filter(
    .resid < quantile(.resid, 0.25) - 1.5 * IQR(.resid) |
    .resid > quantile(.resid, 0.75) + 1.5 * IQR(.resid)
  ) %>%
  select(-.resid, -.model) %>%
  knitr::kable(digits=c(0, 4, 4, 3, 2),
    escape=FALSE, align='lcccccc',
    col.names=c("Año", "$r_t$", "Predicho", "Residuo", "Res. Estand."),
    caption="Observaciones atípicas registradas para los cambios proporcionales en el número de unidades en inventario de leche condensada.")
```

<a name="tab-outliers-two"></a>
<table>
<caption>Tabla 4. Observaciones atípicas registradas para los cambios proporcionales en el número de unidades en inventario de leche condensada.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Año </th>
   <th style="text-align:center;"> $r_t$ </th>
   <th style="text-align:center;"> Predicho </th>
   <th style="text-align:center;"> Residuo </th>
   <th style="text-align:center;"> Res. Estand. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1973 mar </td>
   <td style="text-align:center;"> -0,1899 </td>
   <td style="text-align:center;"> -0,0679 </td>
   <td style="text-align:center;"> -0,122 </td>
   <td style="text-align:center;"> -2,40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1973 ago </td>
   <td style="text-align:center;"> -0,2990 </td>
   <td style="text-align:center;"> -0,0799 </td>
   <td style="text-align:center;"> -0,219 </td>
   <td style="text-align:center;"> -4,31 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1974 mar </td>
   <td style="text-align:center;"> 0,0338 </td>
   <td style="text-align:center;"> -0,0830 </td>
   <td style="text-align:center;"> 0,117 </td>
   <td style="text-align:center;"> 2,30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1976 mar </td>
   <td style="text-align:center;"> 0,0635 </td>
   <td style="text-align:center;"> -0,0725 </td>
   <td style="text-align:center;"> 0,136 </td>
   <td style="text-align:center;"> 2,68 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1977 ago </td>
   <td style="text-align:center;"> 0,0761 </td>
   <td style="text-align:center;"> -0,0330 </td>
   <td style="text-align:center;"> 0,109 </td>
   <td style="text-align:center;"> 2,15 </td>
  </tr>
</tbody>
</table>

Es interesante notar varias características del gráfico de residuales:

* ⌚ Durante el año 1973, donde la series es anormal en su comportamiento anual, y al inicio del cual ocurre el primer valle, de mayor magnitud, el comportamiento de la serie esta algo desviado de la aleatoriedad general de la serie, y es donde ocurren dos de los atípicos observado (uno en el valle, y uno justo en la anormalidad a mediados de año, donde hay una caída inesperada de las unidades en inventario).
* ⌚ A inicios de 1974, existe un atípico al entrar al pico de este año, y el valor en el pico, aunque no es atípico, tiene una magnitud cercana al limite de $2\sigma$. 
* ⌚ El siguiente atípico corresponde al segundo valle de 1976, el cual sigue en magnitud al del año 1973.
* ⌚ El ultimo atípico que se registra en la serie se da a mediados del año 1977, en el ultimo pico registrado en la serie, y al cual corresponde al pico de menor magnitud.

### Parámetros estimados para el modelo escogido.


```r
new_models %>% 
  select(`Lagged with sq`) %>%
  tidy() %>%
  select(-.model) %>%
  mutate(term=c("$\\phi$", "$\\Theta$", "$\\beta_1$", "$\\beta_2$")) %>%
  knitr::kable(digits=c(3, 3, 2, 4), align='lcccc', escape=FALSE,
    col.names=c("Coef.", "Estimado", "Desv. Est.", "Estadístico", "$p$"),
    caption="Parámetros estimados para el modelo de regresión con errores ARMA.")
```

<a name="tab-parametters-table"></a>
<table>
<caption>Tabla 5. Parámetros estimados para el modelo de regresión con errores ARMA.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Coef. </th>
   <th style="text-align:center;"> Estimado </th>
   <th style="text-align:center;"> Desv. Est. </th>
   <th style="text-align:center;"> Estadístico </th>
   <th style="text-align:center;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\phi$ </td>
   <td style="text-align:center;"> -0,109 </td>
   <td style="text-align:center;"> 0,10 </td>
   <td style="text-align:center;"> -1,0636 </td>
   <td style="text-align:center;"> 0,290 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Theta$ </td>
   <td style="text-align:center;"> -1,000 </td>
   <td style="text-align:center;"> 0,30 </td>
   <td style="text-align:center;"> -3,3307 </td>
   <td style="text-align:center;"> 0,001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\beta_1$ </td>
   <td style="text-align:center;"> 0,255 </td>
   <td style="text-align:center;"> 0,09 </td>
   <td style="text-align:center;"> 2,7851 </td>
   <td style="text-align:center;"> 0,006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\beta_2$ </td>
   <td style="text-align:center;"> 0,197 </td>
   <td style="text-align:center;"> 0,39 </td>
   <td style="text-align:center;"> 0,5105 </td>
   <td style="text-align:center;"> 0,611 </td>
  </tr>
</tbody>
</table>

## Predicción de los próximos 24 meses.

Las predicciones para los próximos 24 meses obtenidas a partir del modelo se muestran a continuación:

<a name="fig-forecast-plot"></a>

```r
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

forecast_24_month <- new_models %>%
  select(`Lagged with sq`) %>%
  forecast(new_data=to_forecast)

autoplot(cm_transf, colour="dodgerblue3") +
    autolayer(forecast_24_month, colour="orange") +
    scale_x_yearmonth(date_labels = "%Y") +
    theme_light() + 
    xlab('Tiempo') + ylab('Cambio proporcional') +
    theme(legend.position="none")
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/forecast-plot-1.png" alt="Predicción de los próximos 24 meses de la serie para los cambios proporcionales en las unidades de inventario, utilizando el modelo seleccionado."  />
</div><table><caption>Figura 7. Predicción de los próximos 24 meses de la serie para los cambios proporcionales en las unidades de inventario, utilizando el modelo seleccionado.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

### Predicción usando suavizado exponencial.


```r
ets_models <- cm_transf %>%
  model(`ETS model`=ETS(prop_change ~ error("A") + trend("A") + season("A", period=12), opt_crit="mse")) 
```

Ahora se ajusta un modelo usando suavizado exponencial, minimizando la sumatoria de cuadrados, usando solo componentes aditivos para la estacionalidad y la tendencia, obteniéndose un modelo con un RMSE de 0,054986 y MAE de 0,0371671, comparables a los obtenidos para el modelo ajustado inicialmente, sin regresores, mostrado en la ecuación <a href="#eq-reg-model">2</a>. 
Los valores de AIC y BIC son un orden de magnitud mayor para el modelo ajustado usando ETS. 

Los valores estimados para los parámetros de suavizados son: $\alpha= 0,0062$, $\beta= 0,0001$, y $\gamma= 0,0087$. Los valores tan pequeños para estos parámetros indican que el nivel, tendencia, y estacionalidad apenas varían con el tiempo.

Las predicciones se muestran en la <a href="#fig-ets-forecast-plot">figura 8</a>, junto con la predicción obtenida antes en la <a href="#fig-forecast-plot">figura 7</a> (linea gris a trozos), donde se observa que el modelo ETS ajustado predice de forma similar la serie durante los 24 meses siguientes.

<a name="fig-ets-forecast-plot"></a>
```r
ets_forecast <- ets_models %>% forecast(h=24) 

autoplot(cm_transf, colour="dodgerblue3") +
    autolayer(ets_forecast, colour="orange") +
    autolayer(forecast_24_month, level=NULL, colour="200", linetype=2) +
    scale_x_yearmonth(date_labels = "%Y") +
    theme_light() + 
    xlab('Tiempo') + ylab('Cambio proporcional') +
    theme(legend.position="none")
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/ets-forecast-plot-1.png" alt="Predicción de los próximos 24 meses de la serie para los cambios proporcionales en las unidades de inventario, utilizando un modelo ETS (las predicciones obtenidas usadno el modelo seleccionado se superpone (linea gris a trozos) sobre las predicciones del modelo ETS)"  />
</div><table><caption>Figura 8. Predicción de los próximos 24 meses de la serie para los cambios proporcionales en las unidades de inventario, utilizando un modelo ETS (las predicciones obtenidas usadno el modelo seleccionado se superpone (linea gris a trozos) sobre las predicciones del modelo ETS)</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>



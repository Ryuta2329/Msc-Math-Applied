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
suppressPackageStartupMessages({
  library(tsibble)
  library(fable)
  library(feasts)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(kfigr)
  library(kableExtra)
})

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
</div><table><caption>Figura 1. Cambio temporal en el número de unidades de leche condensada entre 1971-1980. Se muestra el patrón anual y la estacionaridad de la serie usando un suavizado de <em>kernel</em> (línea sólida gris) y <em>loess</em> (línea fragmentada gris), respectivamente.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

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
  drop_na()

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
* 💹 La PACF confirma que se debería elegir $P=1$, dada la correlación significativa en el $h=6$, pero en más ningún múltiplo de 6. Además, se observa que se deberia de considerar un proceso de orden $q=1$ en el componente ARMA, dada la primera correlacion significativa. 
* 💹 Dado que la ACF es decreciente en los _lags_ mostrados, y que la PACF se corta en el $h=1$, se usa un orden $p=1$ y $p=2$ para la parte $ARMA$.
* 💹 Se observan un par de correlaciones significativas en la PACF, donde la correlación en $h=4$ es la mas importante. La otra se encuentra en $h=22$. Estas correlaciones aparecen significativas probablemente debido al patrón anual oscilante combinado con los cambios abruptos durante los picos y valles de la serie: en estos _lags_ se encuentran espaciados casi 2 años, y coinciden con la entrada a los picos y valles de la serie original.

Los resultados de la prueba de Dickey-Fuller aumentada para comprobar estacionaridad de la serie muestran que el incremento proporcional de unidades es estacionaria ($DF_T =$ -6,95, $p =$ 0,01), por lo que no es necesario una diferencia en el componente $ARMA$. 


```r
full <- cm_transf %>%
  model(non_stationaty=ARIMA(prop_change, 
      stepwise=FALSE, 
      method="CSS-ML", optim.method="BFGS"))
```


Se ajusta entonces los modelos:

$$SARIMA(1,0,0)(0,1,1)_{12}\quad SARIMA(1,0,1)(0,1,1)_{12}\quad \text{y }SARIMA(2,0,1)(0,1,1)_{12}$$

y se comparan con un modelo $SARIMA(2,0,2)(0,1,1)_{12}$ estimado minimizando la sumatoria de cuadrados condicional, encontrado por medio de una búsqueda del mejor modelo en el espacio de parámetros (mejor en el sentido de sumatoria de cuadrados mínima).

<a name="fitting-sarima"></a>

```r
selected_mod <- cm_transf %>%
  model(
    `ARMA(1,0,0)(0,1,1)_{12}`=ARIMA(prop_change ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 1, 12), 
      stepwise=FALSE, method="CSS-ML", optim.method="BFGS"),
    `ARMA(1,0,1)(0,1,1)_{12}`=ARIMA(prop_change ~ 0 + pdq(1, 0, 1) + PDQ(0, 1, 1, 12), 
      stepwise=FALSE, method="CSS-ML", optim.method="BFGS"),
    `ARMA(2,0,1)(0,1,1)_{12}`=ARIMA(prop_change ~ 0 + pdq(2, 0, 1) + PDQ(0, 1, 1, 12), 
      stepwise=FALSE, method="CSS-ML", optim.method="BFGS"),
  ) %>%
  bind_cols(select(full, non_stationaty))

.model <- c("$ARIMA(1,0,0)(0,1,1)_{12}$", "$ARIMA(1,0,1)(0,1,1)_{12}$", "$ARIMA(2,0,1)(0,1,1)_{12}$", "$ARIMA(2,0,2)(0,1,1)_{12}$")

glance(selected_mod) %>%
  select(-ar_roots, -ma_roots) %>%
  select(sigma2:BIC) %>%
  tibble::add_column(.model=.model, .before=1) %>%
  kbl(digits=c(NA, 4, 2, 1, 1, 1), 
    align='lccccc', escape=FALSE,
    col.names=c("Modelo", "$\\sigma^2$", "Func. Verosim.", "AIC", "AICc", "BIC"),
    caption="Estadísticos de bondad de ajuste de los modelos SARIMA ajustados.")
```

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
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(1,0,1)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 0,0033 </td>
   <td style="text-align:center;"> 144,40 </td>
   <td style="text-align:center;"> -280,8 </td>
   <td style="text-align:center;"> -280,4 </td>
   <td style="text-align:center;"> -270,1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(2,0,1)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 0,0034 </td>
   <td style="text-align:center;"> 147,31 </td>
   <td style="text-align:center;"> -284,6 </td>
   <td style="text-align:center;"> -284,0 </td>
   <td style="text-align:center;"> -271,2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(2,0,2)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 0,0033 </td>
   <td style="text-align:center;"> 148,93 </td>
   <td style="text-align:center;"> -285,9 </td>
   <td style="text-align:center;"> -285,0 </td>
   <td style="text-align:center;"> -269,8 </td>
  </tr>
</tbody>
</table>

Los resultados de los ajustes se muestran en la <a href="#fitting-sarima">tabla 1</a>, donde se observa que en base al AIC, lo modelos que mejor se ajustan son el modelo completo con parámetros estimados vía MLE, seguido del modelo $SARIMA(2, 0, 1)(0, 1, 1)_{12}$; y este ultimo es preferible al modelo completo de acuerdo a las medidas de BIC, aunque con respecto al BIC, el mejor modelo es el modelo mas sencillo (con el menor numero de parámetros).

Sin embargo, las pruebas de Ljung-Box llevadas a cabo sobre los residuales de los modelos ajustados muestran que solo los modelos SARIMA con componente $AR$ de segundo orden tienen errores independientes. Es por ello, que en base a la simplicidad del modelo $SARIMA(2,0,1)(0,1,1)_{12}$ (tiene menor cantidad de parámetros), este es preferible al modelo completo.

<a name="ljung-box"></a>

```r
selected_mod %>%
  gather(key = "Modelo", value = "Fit") %>%
  mutate(ljung_box = purrr::map(Fit, 
    ~features(augment(.), 
      .innov, ljung_box, dof = 4, lag = 12))) %>%
  unnest(ljung_box) %>%
  mutate(Modelo = c("$ARIMA(1,0,0)(0,1,1)_{12}$", "$ARIMA(1,0,1)(0,1,1)_{12}$", "$ARIMA(2,0,1)(0,1,1)_{12}$", "$ARIMA(2,0,2)(0,1,1)_{12}$")) %>%
  select(-Fit) %>% 
  kbl(digits=c(NA, 2, 4), 
    align='lcc', escape=FALSE,
    col.names=c("Modelo", "$LJ$", "$p$"),
    caption="Resultados de la prueba de Ljung-Box sobre los modelos SARIMA ajustados.")
```

<table>
<caption>Tabla 2. Resultados de la prueba de Ljung-Box sobre los modelos SARIMA ajustados.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Modelo </th>
   <th style="text-align:center;"> $LJ$ </th>
   <th style="text-align:center;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $ARIMA(1,0,0)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 15,29 </td>
   <td style="text-align:center;"> 0,0538 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(1,0,1)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 16,93 </td>
   <td style="text-align:center;"> 0,0309 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(2,0,1)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 9,96 </td>
   <td style="text-align:center;"> 0,2680 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $ARIMA(2,0,2)(0,1,1)_{12}$ </td>
   <td style="text-align:center;"> 6,42 </td>
   <td style="text-align:center;"> 0,6005 </td>
  </tr>
</tbody>
</table>

Se escribe el modelo inferido como:

$$
\begin{aligned}
  \Delta_{12}\phi(B)r_t = \Theta(B) w_t  & \Rightarrow \\
    & \Rightarrow (1 - B^{12})(1 - \phi_1 B - \phi_2 B^2) r_t = (1 + \Theta B)(1 + \theta B) w_t \\
\end{aligned}
$$

el cual se expande como:

<a name="eq-model"></a>
$$r_t = \phi_1r_{t-1} + \phi_2r_{t-2} - r_{t-12} - \phi_1r_{t-13} - \phi_2r_{t-14} + w_t + (\theta + \Theta)w_{t-1} + \theta\Theta w_{t-2}$$

### Parámetros estimados para el modelo escogido.

Los parámetros estimados para el modelo escogido (de la <a href="#eq-model">ecuacion 1</a>) se muestran en la <a href="#parametters-table">tabla 3</a>, donde se observa que todos los coeficientes son significativos.

<a name="parametters-table"></a>

```r
tidied_mod <- selected_mod %>% 
  select(`ARMA(2,0,1)(0,1,1)_{12}`) %>%
  tidy() 

tidied_mod %>%
  select(-.model) %>%
  mutate(term=c("$\\phi_1$", "$\\phi_2$", "$\\theta$", "$\\Theta$")) %>%
  kbl(digits=c(3, 3, 2, 4), align='lcccc', escape=FALSE,
    col.names=c("Coef.", "Estimado", "Desv. Est.", "Estadístico", "$p$"),
    caption="Parámetros estimados para el modelo de regresión con errores ARMA.")
```

<table>
<caption>Tabla 3. Parámetros estimados para el modelo de regresión con errores ARMA.</caption>
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
   <td style="text-align:left;"> $\phi_1$ </td>
   <td style="text-align:center;"> -1,088 </td>
   <td style="text-align:center;"> 0,11 </td>
   <td style="text-align:center;"> -10,3499 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:center;"> -0,350 </td>
   <td style="text-align:center;"> 0,10 </td>
   <td style="text-align:center;"> -3,6391 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\theta$ </td>
   <td style="text-align:center;"> 0,890 </td>
   <td style="text-align:center;"> 0,07 </td>
   <td style="text-align:center;"> 13,4163 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Theta$ </td>
   <td style="text-align:center;"> -0,817 </td>
   <td style="text-align:center;"> 0,13 </td>
   <td style="text-align:center;"> -6,3827 </td>
   <td style="text-align:center;"> 0 </td>
  </tr>
</tbody>
</table>

Usando los estimados, el modelo de la <a href="#eq:model">ecuación 1</a> se escribe como:

$$
r_t = -1,0881_{(0,1051)} r_{t-1} -0,3501_{(0,0962)} r_{t-2} - r_{t-12} + 1,0881_{(0,1051)} r_{t-13} + 0,3501_{(0,0962)} r_{t-14} + w_t + 0,0725_{(0,1944)} w_{t-1} -0,7274_{(0,1682)} w_{t-2}
$$

### Gráficos diagnósticos de residuales.

Al verificar el comportamiento de los residuales para el modelo candidato, $ARIMA(2,0,1)(0,1,1)_{12}$, se obtiene que los residuales presentan una ACF y PACF sin correlaciones importantes, pero que de alguna forma no se observan muy aleatorios en cuanto a su dirección (especialmente durante los primeros $2/3$ del gráfico).

<a name="diagnostics-plots"></a>

```r
res_sd <- selected_mod %>% 
  select(`ARMA(2,0,1)(0,1,1)_{12}`) %>%
  glance() %>%
  pull(sigma2) %>%
  sqrt()
augmented_data <- selected_mod %>% 
  select(`ARMA(2,0,1)(0,1,1)_{12}`) %>%
  augment() %>%
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
En el gráfico _QQ_ se observa que la serie de residuales presenta cierto sesgo hacia la izquierda, con una acumulación de valores positivos, y varios atípicos a ambos lados de la distribución. 
Se observa un patrón no aleatorio de distribución de los residuales alrededor de la media (secciones donde las observaciones caen mucho por encima de la media, y secciones donde caen por debajo). Además, se pueden notar observaciones atípicas, como se muestra en la <a href="#outliers">tabla 4</a>. 
Se puede notar claramente que 3 de ellas caen en 1973, donde el comportamiento de la serie es anormal comparado con los otros años, y el resto corresponden a puntos de entrada a los picos y a los picos observados luego de 1973.

<a name="outliers"></a>

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
   <td style="text-align:left;"> 1972 abr </td>
   <td style="text-align:center;"> 0,0969 </td>
   <td style="text-align:center;"> -0,0647 </td>
   <td style="text-align:center;"> 0,162 </td>
   <td style="text-align:center;"> 2,772 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1973 mar </td>
   <td style="text-align:center;"> -0,1899 </td>
   <td style="text-align:center;"> -0,0461 </td>
   <td style="text-align:center;"> -0,144 </td>
   <td style="text-align:center;"> -2,467 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1973 abr </td>
   <td style="text-align:center;"> 0,1979 </td>
   <td style="text-align:center;"> 0,0491 </td>
   <td style="text-align:center;"> 0,149 </td>
   <td style="text-align:center;"> 2,552 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1973 ago </td>
   <td style="text-align:center;"> -0,2990 </td>
   <td style="text-align:center;"> -0,0773 </td>
   <td style="text-align:center;"> -0,222 </td>
   <td style="text-align:center;"> -3,800 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1974 mar </td>
   <td style="text-align:center;"> 0,0338 </td>
   <td style="text-align:center;"> -0,1022 </td>
   <td style="text-align:center;"> 0,136 </td>
   <td style="text-align:center;"> 2,333 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1974 ago </td>
   <td style="text-align:center;"> 0,0297 </td>
   <td style="text-align:center;"> -0,0873 </td>
   <td style="text-align:center;"> 0,117 </td>
   <td style="text-align:center;"> 2,006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1976 mar </td>
   <td style="text-align:center;"> 0,0635 </td>
   <td style="text-align:center;"> -0,0571 </td>
   <td style="text-align:center;"> 0,121 </td>
   <td style="text-align:center;"> 2,068 </td>
  </tr>
</tbody>
</table>

Todo lo mencionado, parece indicar que aun existe una estructura de dependencia dentro de la serie que no se ha especificado, y que se debe optar por un modelo distinto. 
En el gráfico temporal de residuales, es aun visible como va ocurriendo el amortiguado de las desviaciones conforme se va entrando a 1980. 

Es interesante notar varias características del gráfico de residuales:

* ⌚ Durante el año 1973, donde la series es anormal en su comportamiento anual, y al inicio del cual ocurre el primer valle, de mayor magnitud, el comportamiento de la serie esta algo desviado de la aleatoriedad general de la serie, y es donde ocurren dos de los atípicos observado (uno en el valle, y uno justo en la anormalidad a mediados de año, donde hay una caída inesperada de las unidades en inventario).
* ⌚ A inicios de 1974, existe un atípico al entrar al pico de este año, y el valor en el pico, aunque no es atípico, tiene una magnitud cercana al limite de $2\sigma$. 
* ⌚ El siguiente atípico corresponde al segundo valle de 1976, el cual sigue en magnitud al del año 1973.
* ⌚ El ultimo atípico que se registra en la serie se da a mediados del año 1977, en el ultimo pico registrado en la serie, y al cual corresponde al pico de menor magnitud.

<!---
<a name="bivariate-lag"></a>

```r
tscleaned <- augmented_data %>%
  filter(
    !(.std.resid < quantile(.std.resid, 0.25) - 1.96 * IQR(.std.resid)) |
    !(.std.resid > quantile(.std.resid, 0.75) + 1.96 * IQR(.std.resid))
  )

tscleaned %>%
  gg_lag(.innov, period=12, lags=1:12, geom="point") +
  theme_light() +
  theme(legend.position="none") +
  facet_wrap(~.lag, 
    labeller=as_labeller(setNames(paste("h =", 1:12), paste("lag", 1:12)))) +
  xlab('lag(Cambio proporcional, h)') + 
  ylab('Cambio proporcional')
```

<div class="figure" style="text-align: center">
<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/bivariate-lag-1.png" alt="Gráficos bivariados que relacionan la serie en $t$ con su valor retrasado en $t-h$, para $h=1$ a $12$."  />
<p class="caption">(\#fig:bivariate-lag)Gráficos bivariados que relacionan la serie en $t$ con su valor retrasado en $t-h$, para $h=1$ a $12$.</p>
</div><table><caption>Figura 5. Gráficos bivariados que relacionan la serie en $t$ con su valor retrasado en $t-h$, para $h=1$ a $12$.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>
--->

Un análisis mas detallado del gráfico de residuales se muestra en la <a href="#residuals-kernel">figura 6</a>, donde se superpone una curva suavizada por _kernel_ normal (`bandwidth=8`), donde se observa un patrón senoidal bianual, en el que la amplitud de las funciones senos y cosenos involucradas parece variar con el tiempo. 

<a name="residuals-kernel"></a>

```r
augmented_data %>%
  autoplot(.std.resid, colour="orange") +
  geom_point(aes(y=.std.resid), colour="orange") +
  scale_x_yearmonth(date_labels = "%Y") +
  xlab("Tiempo") +
  ylab("Residuales Estandarizados") + 
  geom_hline(yintercept=c(0, 1.96, -1.96), 
    color=c("black", 200, 200), linetype=c(1, 2, 2)) +
  theme_light() + 
  geom_line(
    aes(y=ksmooth(time(augmented_data$.std.resid %>% as.ts()), augmented_data$.std.resid %>% as.ts(), "normal", bandwidth=8)$y), 
    colour="200", lwd=1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/residuals-kernel-1.png" alt="Residuales con curva suavizada por _kernel_ superpuesta."  />
</div><table><caption>Figura 6. Residuales con curva suavizada por <em>kernel</em> superpuesta.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

En este momento no se como hacer de este comportamiento parte del modelo así que decidí no incluirlo y realizar el análisis con el modelo SARIMA ajustado, aunque no sea muy normal.

## Predicción de los próximos 24 meses.

Las predicciones para los próximos 24 meses obtenidas a partir del modelo se muestran a continuación:

<a name="forecast-plot"></a>

```r
forecast_24_month <- selected_mod %>%
  select(`ARMA(2,0,1)(0,1,1)_{12}`) %>%
  forecast(h=24)

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
  model(
    `ETS model`=ETS(prop_change ~ error("A") + trend("A") + season("A", period=12), 
    opt_crit="mse")
  ) 
```

Ahora se ajusta un modelo usando suavizado exponencial, minimizando la sumatoria de cuadrados, usando solo componentes aditivos para la estacionalidad y la tendencia, obteniéndose un modelo con un RMSE de 0,054986 y MAE de 0,0371671, comparables a los obtenidos para el modelo ajustado inicialmente, sin regresores, mostrado en la ecuación <a href="#eq:model">1</a>. 
Los valores de AIC y BIC son un orden de magnitud mayor para el modelo ajustado usando ETS. 

Los valores estimados para los parámetros de suavizados son: $\alpha= 0,0062$, $\beta= 0,0001$, y $\gamma= 0,0087$. Los valores tan pequeños para estos parámetros indican que el nivel, tendencia, y estacionalidad apenas varían con el tiempo.

Las predicciones se muestran en la <a href="#ets-forecast-plot">figura 8</a>, junto con la predicción obtenida antes en la <a href="#forecast-plot">figura 7</a>, <a href="#forecast-plot">figura 8</a> (linea gris a trozos), donde se observa que el modelo ETS ajustado predice de forma similar la serie durante los 24 meses siguientes.

<a name="ets-forecast-plot"></a>

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
<img src="/Series Temporales/output/Lab-Session-8-FPP-2_files/figure-html/ets-forecast-plot-1.png" alt="Predicción de los próximos 24 meses de la serie para los cambios proporcionales en las unidades de inventario, utilizando un modelo ETS (las predicciones obtenidas usando el modelo seleccionado se superpone (linea gris a trozos) sobre las predicciones del modelo ETS)"  />
</div><table><caption>Figura 8. Predicción de los próximos 24 meses de la serie para los cambios proporcionales en las unidades de inventario, utilizando un modelo ETS (las predicciones obtenidas usando el modelo seleccionado se superpone (linea gris a trozos) sobre las predicciones del modelo ETS)</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>



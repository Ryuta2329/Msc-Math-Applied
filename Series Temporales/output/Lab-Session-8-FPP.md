-   [0.1 Descripción de la serie.](#descripción-de-la-serie.)
-   [0.2 Transformación de la serie.](#transformación-de-la-serie.)
-   [0.3 Elección de un modelo.](#elección-de-un-modelo.)
    -   [0.3.1 Estadísticos de Bondad de
        Ajuste.](#estadísticos-de-bondad-de-ajuste.)
    -   [0.3.2 Gráficos diagnósticos de
        residuales.](#gráficos-diagnósticos-de-residuales.)
    -   [0.3.3 Análisis de correlaciones de *r*<sub>*t*</sub> y
        *r*<sub>*t* − *h*</sub>.](#análisis-de-correlaciones-de-r_t-y-r_t-h.)
-   [0.4 Regresión con variables
    retrasadas.](#regresión-con-variables-retrasadas.)
    -   [0.4.1 Estadísticos de Bondad de
        Ajuste.](#estadísticos-de-bondad-de-ajuste.-1)
    -   [0.4.2 Gráficos diagnósticos de
        residuales.](#gráficos-diagnósticos-de-residuales.-1)
    -   [0.4.3 Parámetros estimados para el modelo
        escogido.](#parámetros-estimados-para-el-modelo-escogido.)
-   [0.5 Predicción de los próximos 24
    meses.](#predicción-de-los-próximos-24-meses.)
    -   [0.5.1 Predicción usando suavizado
        exponencial.](#predicción-usando-suavizado-exponencial.)

Elija una de las siguientes series temporales estacionales: `condmilk`,
`hsales`, `usolec`  
\* ✅ ¿Es necesario transformar los datos? Si es así, encuentre un
transformación adecuado. \* ✅ ¿Son estacionarios los datos? Si no,
encuentre una diferenciación apropiada que produce datos estacionarios.
\* ✅ Identifique un par de modelos ARIMA que podrían ser útiles en
describir la serie de tiempo. ¿Cuál de sus modelos es el mejor de
acuerdo con sus valores *A**I**C*? \* ✅ Estime los parámetros de su
mejor modelo y haga un diagnóstico de los residuos. ¿Los residuos se
parecen al ruido blanco? Si no es así, intente encontrar otro modelo
ARIMA que le quede mejor. \* ✅ Pronostique los próximos 24 meses de
datos usando su modelo preferido. \* ✅ Compare los pronósticos
obtenidos, usando `ets()`.

0.1 Descripción de la serie.
----------------------------

    # Sourcing R code
    source("../R/Lab-Session-8-FPP.R")

Los datos seleccionados corresponden a una serie temporal de Inventarios
de Manufactura de leche condensada evaporada y endulzada, recolectados
mensualmente desde 1971 a 1980, como se muestra en el gráfico de la
figura <a href="#fig:tseries-plot"><strong>??</strong></a>.

    # Grafico de la serie temporal
    autoplot(condmilk, colour="dodgerblue3") +
      geom_point(aes(y=value), colour="dodgerblue3") + 
      theme_light() + 
      geom_line(
        aes(y=ksmooth(time(condmilk %>% as.ts()), condmilk %>% as.ts(), "normal", bandwidth=.5)$y), 
        colour="200", lwd=1) +
      geom_smooth(aes(y=value), method=loess, se=FALSE, color="200", linetype=2) +
      xlab('Tiempo') + ylab('Numero de Unidades')

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/tseries-plot-1.png" style="display: block; margin: auto;" />

En el gráfico se observa claramente dos componentes estacionales: uno
anual obvio, que fluctúa ligeramente, observándose una disrrupción a
mediados de 1973 que rompe de alguna manera el patrón unimodal de los
picos anuales; y también parece haber un componente trimestral de
repeticiones de picos y valles importantes, cuya amplitud disminuye con
el tiempo, indicando un amortiguamiento de las variaciones o
fluctuaciones importantes en la serie.

Se resaltan en el componente trimestral: *i)* picos de mayor magnitud
cada tres años, a mediados de los años 1971, 1974 y 1977, mostrando un
cambio periódico en la amplitud de los ciclos (aunque no se observa pico
a mediados de 1980); *ii)* valles profundos cada 3 años, en los años
1973, 1976 y 1979, con una ligera fluctuación mensual, en donde el
número de unidades cae por debajo de las 50 unidades y cuya amplitud, al
igual que la de los picos, se va amortiguando cada vez que aparece.

También se observa una pequeña tendencia decreciente durante la primera
parte de la serie, que es muy perceptible debido al comportamiento
anormal durante el año 1973; pero que desaparece durante la segunda
parte de la serie.

0.2 Transformación de la serie.
-------------------------------

Dada la descripción anterior, se decide optar por transformar los datos
usando una función logarítmica, con la finalidad de hacer mas obvias las
desviaciones importantes que aparecen en la serie; y en lugar de modelar
la serie original transformada, se modela la serie que resulta de la
diferencia de valores adyacentes (elimina el componente con tendencia en
la primer tercio de la serie original):

$$log(x\_t) - log(x\_{t-1}) = log(\\frac{x\_t}{x\_{t-1}}) = log(1 + r\_t)$$

donde *r*<sub>*t*</sub> es el incremento o decremento proporcional de la
unidad en el año *t* con respecto al valor en el año anterior, *t* − 1.
Dado que la magnitud de *r*<sub>*t*</sub> es pequeña, se puede aproximar
*l**o**g*(1 + *r*<sub>*t*</sub>) ≈ *r*<sub>*t*</sub>. La serie
*r*<sub>*t*</sub> se muestra en la figura
<a href="#fig:transform"><strong>??</strong></a>.

    autoplot(cm_transf, colour="dodgerblue3") +
      geom_point(aes(y=prop_change), colour="dodgerblue3") + 
      theme_light() + 
      geom_line(
        aes(y=ksmooth(time(cm_transf %>% as.ts()), cm_transf %>% as.ts(), "normal", bandwidth=.5)$y), 
        colour="200", lwd=1) +
      geom_smooth(aes(y=prop_change), method=loess, se=FALSE, color="200", linetype=2) + 
      xlab('Tiempo') + ylab('Cambio proporcional')

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/transform-1.png" style="display: block; margin: auto;" />

Como se muestra, el patrón anual persiste en la serie, y se hacen mas
visibles las desviaciones que antes estaban ocultas por el componente
estacional. Dos observaciones destacan como valores atípicos en la
serie, al inicio del primer pico a mediados de 1971, y en el primer
valle en 1973.

0.3 Elección de un modelo.
--------------------------

Para un primer análisis de la correlación serial de la serie,
verificamos las ACF y PACF (figura
<a href="#fig:acf-pacf"><strong>??</strong></a>).

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

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/acf-pacf-1.png" style="display: block; margin: auto;" />

-   💹 La ACF indica que seria apropiada una diferencia de orden *D* = 1
    para el componente estacional, y un parámetro autoregresivo de orden
    *P* = 1. También se observa que el periodo debería ser *s* = 6, dado
    que los picos de mayor magnitud se registran en 6*k* para
    *k* = 1, 2, …. Sin embargo, como el patrón es anual, se elige un
    periodo de *s* = 12 dado que en un intervalo de 12 meses se completa
    un ciclo en la ACF.
-   💹 La PACF confirma que se debería elegir *P* = 1, dada la
    correlación significativa en el *lag* 6, pero en más ningún múltiplo
    de 6.
-   💹 Dado que la ACF es decayente en los *lags* mostrados, y que la
    PACF se corta en el *lag* 1, se usa un orden *p* = 1 para la parte
    *A**R**M**A*.
-   Se observan un par de correlaciones significativas en la PACF, donde
    la correlación en *h* = 4 es la mas importante. La otra se encuentra
    en *h* = 22. Estas correlaciones aparecen significativas
    probablemente debido al patrón anual oscilante combinado con los
    cambios abruptos durante los picos y valles de la serie: en estos
    *lags* se encuentran espaciados casi 2 años, y coinciden con la
    entrada a los picos y valles de la serie original.

Los resultados de la prueba de Dickey-Fuller aumentada para comprobar
estacionaridad de la serie muestran que el incremento proporcional de
unidades es estacionaria (-6,95, 0,01), por lo que no es necesario una
diferencia en el componente ARMA.

Se ajusta entonces un modelo
*A**R**I**M**A*(1, 0, 0)(0, 1, 1)<sub>12</sub>, y se compara con un
modelo *A**R**I**M**A*(2, 0, 2)(0, 1, 1)<sub>12</sub> estimado
minimizando la sumatoria de cuadrados condicional, encontrado por medio
de una búsqueda del mejor modelo en el espacio de parámetros (mejor en
el sentido de sumatoria de cuadrados mínima).

Se escribe el modelo inferido de los descriptivos:

$$
\\begin{aligned}
  \\Delta\_{12}\\phi(B)r\_t = \\Theta(B) w\_t  & \\Rightarrow \\\\
    & \\Rightarrow (1 - B^12)(1 - \\phi B) r\_t = (1 - \\Theta B) w\_t \\\\
\\end{aligned}
$$

el cual se expande como:

*r*<sub>*t*</sub> = *ϕ**r*<sub>*t* − 1</sub> + *r*<sub>*t* − 12</sub> + *ϕ**r*<sub>*t* − 13</sub> + *w*<sub>*t*</sub> + *Θ**w*<sub>*t* − 1</sub>(\#*e**q* : *m**o**d**e**l*)

### 0.3.1 Estadísticos de Bondad de Ajuste.

Los resultados de los ajustes se muestran en la tabla
<a href="#tab:fitting-sarima">1</a>, los cuales indican que el modelo
dado en la ecuación (1) no difiere demasiado del modelo encontrado al
tantear el espacio de parámetros, en términos de robustez y precisión,
dada la varianza residual, RMSE y MAE similares. Además, el modelo
*A**R**I**M**A*(2, 0, 2)(0, 1, 1)<sub>12</sub> parece ser preferible
según los valores obtenidos en términos de la información proveída por
el modelo (AIC, AICc y BIC). Esto se debe (dada la similitud de
varianzas residuales), solo al aumento en la penalización consecuencia
de la mayor cantidad de parámetros estimados en el modelo
*A**R**I**M**A*(2, 0, 2)(0, 1, 1)<sub>12</sub>.

    .model <- c(
      expression("ARIMA(1,0,0)(0,1,1)"["12"]), 
      expression("ARIMA(2,0,2)(0,1,1)"["12"])
    )

    information_based %>%
      left_join(error_based) %>%
      select(.model:BIC, ME:MAE) %>%
      knitr::kable(digits=c(NA, 4, 2, 1, 1, 1, 4, 4, 4), 
        align='lcccccccccc', escape=FALSE,
        col.names=c("Modelo", "$\\sigma^2$", "Func. Verosim.", "AIC", "AICc", "BIC", "ME", "RMSE", "MAE"),
        caption="Estadísticos de bondad de ajuste de los modelos SARIMA ajustados.")

<table>
<caption>
Table 1: Estadísticos de bondad de ajuste de los modelos SARIMA
ajustados.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Modelo
</th>
<th style="text-align:center;">
*σ*<sup>2</sup>
</th>
<th style="text-align:center;">
Func. Verosim.
</th>
<th style="text-align:center;">
AIC
</th>
<th style="text-align:center;">
AICc
</th>
<th style="text-align:center;">
BIC
</th>
<th style="text-align:center;">
ME
</th>
<th style="text-align:center;">
RMSE
</th>
<th style="text-align:center;">
MAE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
0,0034
</td>
<td style="text-align:center;">
143,79
</td>
<td style="text-align:center;">
-281,6
</td>
<td style="text-align:center;">
-281,4
</td>
<td style="text-align:center;">
-273,6
</td>
<td style="text-align:center;">
0,0005
</td>
<td style="text-align:center;">
0,0547
</td>
<td style="text-align:center;">
0,0372
</td>
</tr>
<tr>
<td style="text-align:left;">
non\_stationaty
</td>
<td style="text-align:center;">
0,0033
</td>
<td style="text-align:center;">
148,93
</td>
<td style="text-align:center;">
-285,9
</td>
<td style="text-align:center;">
-285,0
</td>
<td style="text-align:center;">
-269,8
</td>
<td style="text-align:center;">
0,0007
</td>
<td style="text-align:center;">
0,0531
</td>
<td style="text-align:center;">
0,0376
</td>
</tr>
</tbody>
</table>

### 0.3.2 Gráficos diagnósticos de residuales.

Al verificar el comportamiento de los residuales para el modelo
candidato, *A**R**I**M**A*(1, 0, 0)(0, 1, 1)<sub>12</sub>, se obtiene
que los residuales no parecen estar correlacionados entre si, evaluando
con respecto al retraso 12, aunque el valor de probabilidad obtenido es
apenas marginal (15,289, 0,0537708). En el gráfico para ACF y PACF se
muestra un pico de correlación significativa con respeto al *lag* 5, lo
cual indica la falta de un retraso en el modelo considerado.

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

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/diagnostics-plots-1.png" style="display: block; margin: auto;" />

Más aun, el gráfico de residuales estandarizados y el gráfico *QQ*
muestran claramente que los residuales no son normales. Se observa un
patrón no aleatorio de distribución de los residuales alrededor de la
media (secciones donde las observaciones caen mucho por encima de la
media, y secciones donde caen por debajo). Además, se pueden notar
observaciones atípicas, 8 de ellas específicamente, como se muestra en
la tabla @ref{tab:outliers}. Se puede notar claramente que 3 de ellas
caen en 1973, donde el comportamiento de la serie es anormal comparado
con los otros años, y el resto corresponden a puntos de entrada a los
picos y a los picos observados luego de 1973.

    augmented_data %>%
      filter(
        .resid < quantile(.resid, 0.25) - 1.5 * IQR(.resid) |
        .resid > quantile(.resid, 0.75) + 1.5 * IQR(.resid)
      ) %>%
      select(-.resid) %>%
      knitr::kable(digits=c(NA, 0, 4, 4, 3, 3, 2),
        escape=FALSE, align='lcccccc',
        col.names=c("Modelo", "Año", "$r_t$", "Predicho", "Residuo", "Res. Estand."),
        caption="Observaciones atípicas registradas para los cambios proporcionales en el número de unidades en inventario de leche condensada.")

<table>
<caption>
Table 2: Observaciones atípicas registradas para los cambios
proporcionales en el número de unidades en inventario de leche
condensada.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Modelo
</th>
<th style="text-align:center;">
Año
</th>
<th style="text-align:center;">
*r*<sub>*t*</sub>
</th>
<th style="text-align:center;">
Predicho
</th>
<th style="text-align:center;">
Residuo
</th>
<th style="text-align:center;">
Res. Estand.
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1972 abr
</td>
<td style="text-align:center;">
0,0969
</td>
<td style="text-align:center;">
-0,0623
</td>
<td style="text-align:center;">
0,159
</td>
<td style="text-align:center;">
2,735
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1973 mar
</td>
<td style="text-align:center;">
-0,1899
</td>
<td style="text-align:center;">
-0,0469
</td>
<td style="text-align:center;">
-0,143
</td>
<td style="text-align:center;">
-2,457
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1973 abr
</td>
<td style="text-align:center;">
0,1979
</td>
<td style="text-align:center;">
0,0543
</td>
<td style="text-align:center;">
0,144
</td>
<td style="text-align:center;">
2,467
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1973 ago
</td>
<td style="text-align:center;">
-0,2990
</td>
<td style="text-align:center;">
-0,0383
</td>
<td style="text-align:center;">
-0,261
</td>
<td style="text-align:center;">
-4,477
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1974 mar
</td>
<td style="text-align:center;">
0,0338
</td>
<td style="text-align:center;">
-0,0807
</td>
<td style="text-align:center;">
0,115
</td>
<td style="text-align:center;">
1,967
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1974 ago
</td>
<td style="text-align:center;">
0,0297
</td>
<td style="text-align:center;">
-0,0885
</td>
<td style="text-align:center;">
0,118
</td>
<td style="text-align:center;">
2,032
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1976 mar
</td>
<td style="text-align:center;">
0,0635
</td>
<td style="text-align:center;">
-0,0613
</td>
<td style="text-align:center;">
0,125
</td>
<td style="text-align:center;">
2,145
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
1977 ago
</td>
<td style="text-align:center;">
0,0761
</td>
<td style="text-align:center;">
-0,0359
</td>
<td style="text-align:center;">
0,112
</td>
<td style="text-align:center;">
1,924
</td>
</tr>
</tbody>
</table>

Todo lo mencionado, parece indicar que aun existe una estructura de
dependencia dentro de la serie que no se ha especificado, y que se debe
optar por un modelo distinto. Los resultados parecen indicar que se debe
considerar un modelo con el quinto *lag* como regresor.

### 0.3.3 Análisis de correlaciones de *r*<sub>*t*</sub> y *r*<sub>*t* − *h*</sub>.

Dado los resultados encontrados para el primer modelo ajustado, se
decide hacer un análisis de correlación entre la serie temporal con
respecto a las series temporales retrasadas (figura
<a href="#fig:bivariate-lag"><strong>??</strong></a>).

    tscleaned %>%
      gg_lag(prop_change, period=12, lags=1:12, geom="point") +
      theme_light() +
      theme(legend.position="none") +
      facet_wrap(~.lag, 
        labeller=as_labeller(setNames(paste("h =", 1:12), paste("lag", 1:12)))) +
      xlab('lag(Cambio proporcional, h)') + 
      ylab('Cambio proporcional')

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/bivariate-lag-1.png" style="display: block; margin: auto;" />

La dependencia de la serie en *t* con respecto a los valores en *t* − 1
se toma en cuenta dentro del modelo ajustado antes, al considerar un
modelo autoregresivo de orden *p* = 1, al igual que la correlación
positiva con respecto al 12vo retraso. La correlación con *h* = 3 no es
significativa, y en *h* = 2 y *h* = 4 las correlaciones son pequeñas,
comparadas con las correlaciones observadas en *h* = 5 y *h* = 6 (las
cuales son negativas y más importantes), y que parecen ser no lineales
para valores mayores a 0, 05.

Para estas variables retrasadas, la correlación negativa puede modelarse
de dos formas: *i)* considerando un solo modelo con regresores
*r*<sub>*t* − 5</sub> y *r*<sub>*t* − 5</sub><sup>2</sup>, dada la
curvatura y forma no lineal de la serie; o *ii)* generando un modelo a
trozos con un regresor lineal *r*<sub>*t* − 6</sub>, cuyo coeficiente
asociado cambia dependiendo de si *r*<sub>*t*</sub> es mayor a 0, 05.

0.4 Regresión con variables retrasadas.
---------------------------------------

Se busca modelar, en el primer caso, un modelo de la forma:

*r*<sub>*t*</sub> = *ϕ**r*<sub>*t* − 1</sub> + *ϕ**r*<sub>*t* − 12</sub> + *ϕ**r*<sub>*t* − 13</sub> + *β*<sub>1</sub>*r*<sub>*t* − 5</sub> + *β*<sub>2</sub>*r*<sub>*t* − 5</sub><sup>2</sup> + *w*<sub>*t*</sub> + *Θ**w*<sub>*t* − 1</sub>(\#*e**q* : *m**o**d**e**l* − *r**e**g*)

donde *β*<sub>1</sub> y *β*<sub>2</sub> son los coeficientes de
regresión. En el segundo caso, se necesita de una variable *dummy*
*D*<sub>*t* − 5</sub> la cual es 0 si *r*<sub>*t* − 6</sub> &lt; 0, 05 y
1 de otra forma, generando el modelo a trozos:

$$
r\_t = \\begin{cases}
  \\omega + \\beta\_1 r\_{t-5} &  r\_{t-5} &lt; 0{,}05 \\\\
  \\omega + \\beta\_2 + (\\beta\_1 + \\beta\_3) r\_{t-5} & r\_{t-5} \\ge 0{,}05
\\end{cases}
 (\\\#eq:model-reg-trozos)
$$

donde
*ω* = *ϕ**r*<sub>*t* − 1</sub> + *ϕ**r*<sub>*t* − 12</sub> + *ϕ**r*<sub>*t* − 13</sub> + *w*<sub>*t*</sub> + *Θ**w*<sub>*t* − 1</sub>
es el componente ARIMA y, de nuevo, *β*<sub>1</sub> y *β*<sub>2</sub>
son los coeficientes de regresión para *r*<sub>*t* − 5</sub> y
*D*<sub>*t* − 5</sub>, y *β*<sub>3</sub> es el coeficiente de la
interacción *D*<sub>*t* − 5</sub>*r*<sub>*t* − 5</sub>.

### 0.4.1 Estadísticos de Bondad de Ajuste.

    models_xreg <- c(.model, 
      expression("ARIMA(1,0,0)(0,1,1)"["6"] ~ "(no lineal con r"["t-5"] ~ "^2)"), 
      expression("ARIMA(1,0,0)(0,1,1)"["6"] ~ "(a trozos)")
    )

    information_based_2 %>%
      left_join(error_based_2) %>%
      select(.model:BIC, ME:MAE) %>%
      knitr::kable(digits=c(NA, 4, 2, 1, 1, 1, 4, 4, 4), 
        align='lcccccccccc', escape=FALSE,
        col.names=c("Modelo", "$\\sigma^2$", "Func. Verosim.", "AIC", "AICc", "BIC", "ME", "RMSE", "MAE"),
        caption="Estadísticos de bondad de ajuste de los modelos SARIMA ajustados.")

<table>
<caption>
Table 3: Estadísticos de bondad de ajuste de los modelos SARIMA
ajustados.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Modelo
</th>
<th style="text-align:center;">
*σ*<sup>2</sup>
</th>
<th style="text-align:center;">
Func. Verosim.
</th>
<th style="text-align:center;">
AIC
</th>
<th style="text-align:center;">
AICc
</th>
<th style="text-align:center;">
BIC
</th>
<th style="text-align:center;">
ME
</th>
<th style="text-align:center;">
RMSE
</th>
<th style="text-align:center;">
MAE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
non\_stationaty
</td>
<td style="text-align:center;">
0,0033
</td>
<td style="text-align:center;">
148,93
</td>
<td style="text-align:center;">
-285,9
</td>
<td style="text-align:center;">
-285,0
</td>
<td style="text-align:center;">
-269,8
</td>
<td style="text-align:center;">
0,0007
</td>
<td style="text-align:center;">
0,0531
</td>
<td style="text-align:center;">
0,0376
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagged with sq
</td>
<td style="text-align:center;">
0,0026
</td>
<td style="text-align:center;">
147,67
</td>
<td style="text-align:center;">
-285,3
</td>
<td style="text-align:center;">
-284,7
</td>
<td style="text-align:center;">
-272,2
</td>
<td style="text-align:center;">
-0,0002
</td>
<td style="text-align:center;">
0,0471
</td>
<td style="text-align:center;">
0,0339
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagged by pieces
</td>
<td style="text-align:center;">
0,0026
</td>
<td style="text-align:center;">
148,38
</td>
<td style="text-align:center;">
-284,8
</td>
<td style="text-align:center;">
-283,9
</td>
<td style="text-align:center;">
-269,0
</td>
<td style="text-align:center;">
0,0006
</td>
<td style="text-align:center;">
0,0468
</td>
<td style="text-align:center;">
0,0336
</td>
</tr>
<tr>
<td style="text-align:left;">
ARMA(1,0,0)(0,1,1)\_{12}
</td>
<td style="text-align:center;">
0,0034
</td>
<td style="text-align:center;">
143,79
</td>
<td style="text-align:center;">
-281,6
</td>
<td style="text-align:center;">
-281,4
</td>
<td style="text-align:center;">
-273,6
</td>
<td style="text-align:center;">
0,0005
</td>
<td style="text-align:center;">
0,0547
</td>
<td style="text-align:center;">
0,0372
</td>
</tr>
</tbody>
</table>

Los estadístico de bondad de ajuste muestran que las varianzas
residuales de los modelos mostrados en las ecuaciones (2) y (3) son
menores a la varianza residual del modelo
*A**R**I**M**A*(1, 0, 0)(0, 1, 1)<sub>6</sub> sin regresores. De igual
forma, las demás medidas basadas en residuales RMSE, MAE y ME, no varían
demasiado con respecto a los encontrados para el primer modelo ajustado,
pero son, claro, menores. Por otro lado, los valores de AIC y BIC para
el modelo de regresión a trozos y el que usa una dependencia cuadrática
son menores que los del primer modelo ajustado anteriormente (sin
regresores).

### 0.4.2 Gráficos diagnósticos de residuales.

Los gráficos de residuales se muestran en la figura
<a href="#fig:diagnostics-plots-2"><strong>??</strong></a> para el
modelo de la ecuación (2), con la dependencia cuadrática (el cual se
selecciona al revisar los residuales). Se puede observar que ya no
existen correlaciones significativas, aunque se observa, principalmente
en el PACF, que hay una cantidad inusualmente grande de correlaciones
negativas en una sección en la primera mitad del gráfico.

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

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/diagnostics-plots-2-1.png" style="display: block; margin: auto;" />

Por otro lado, el gráfico *QQ* muestra una mejora significativa en el
comportamiento de los residuales, comparado con el modelo anterior. La
distribución de estos alrededor de la media se percibe mas aleatoria, y
a excepción de los atípicos, se ajustan bien a la recta teórica en el
gráfico *QQ*. Aun es posible observar quiebres estructurales
consecuencia de las observaciones atípicas: en total se observan 5
atípicos, con más de dos desviaciones estándar, y al menos 9-10
observaciones con desviaciones importantes dentro del intervalo de una
desviación estándar.

    augmented_data_2 %>%
      filter(
        .resid < quantile(.resid, 0.25) - 1.5 * IQR(.resid) |
        .resid > quantile(.resid, 0.75) + 1.5 * IQR(.resid)
      ) %>%
      select(-.resid) %>%
      knitr::kable(digits=c(NA, 0, 4, 4, 3, 2),
        escape=FALSE, align='lcccccc',
        col.names=c("Modelo", "Año", "$r_t$", "Predicho", "Residuo", "Res. Estand."),
        caption="Observaciones atípicas registradas para los cambios proporcionales en el número de unidades en inventario de leche condensada.")

<table>
<caption>
Table 4: Observaciones atípicas registradas para los cambios
proporcionales en el número de unidades en inventario de leche
condensada.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Modelo
</th>
<th style="text-align:center;">
Año
</th>
<th style="text-align:center;">
*r*<sub>*t*</sub>
</th>
<th style="text-align:center;">
Predicho
</th>
<th style="text-align:center;">
Residuo
</th>
<th style="text-align:center;">
Res. Estand.
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Lagged with sq
</td>
<td style="text-align:center;">
1973 mar
</td>
<td style="text-align:center;">
-0,1899
</td>
<td style="text-align:center;">
-0,0679
</td>
<td style="text-align:center;">
-0,122
</td>
<td style="text-align:center;">
-2,40
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagged with sq
</td>
<td style="text-align:center;">
1973 ago
</td>
<td style="text-align:center;">
-0,2990
</td>
<td style="text-align:center;">
-0,0799
</td>
<td style="text-align:center;">
-0,219
</td>
<td style="text-align:center;">
-4,31
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagged with sq
</td>
<td style="text-align:center;">
1974 mar
</td>
<td style="text-align:center;">
0,0338
</td>
<td style="text-align:center;">
-0,0830
</td>
<td style="text-align:center;">
0,117
</td>
<td style="text-align:center;">
2,30
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagged with sq
</td>
<td style="text-align:center;">
1976 mar
</td>
<td style="text-align:center;">
0,0635
</td>
<td style="text-align:center;">
-0,0725
</td>
<td style="text-align:center;">
0,136
</td>
<td style="text-align:center;">
2,68
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagged with sq
</td>
<td style="text-align:center;">
1977 ago
</td>
<td style="text-align:center;">
0,0761
</td>
<td style="text-align:center;">
-0,0330
</td>
<td style="text-align:center;">
0,109
</td>
<td style="text-align:center;">
2,15
</td>
</tr>
</tbody>
</table>

Es interesante notar varias características del gráfico de residuales:

-   ⌚ Durante el año 1973, donde la series es anormal en su
    comportamiento anual, y al inicio del cual ocurre el primer valle,
    de mayor magnitud, el comportamiento de la serie esta algo desviado
    de la aleatoriedad general de la serie, y es donde ocurren dos de
    los atípicos observado (uno en el valle, y uno justo en la
    anormalidad a mediados de año, donde hay una caída inesperada de las
    unidades en inventario).
-   ⌚ A inicios de 1974, existe un atípico al entrar al pico de este
    año, y el valor en el pico, aunque no es atípico, tiene una magnitud
    cercana al limite de 2*σ*.
-   ⌚ El siguiente atípico corresponde al segundo valle de 1976, el cual
    sigue en magnitud al del año 1973.
-   ⌚ El ultimo atípico que se registra en la serie se da a mediados del
    año 1977, en el ultimo pico registrado en la serie, y al cual
    corresponde al pico de menor magnitud.

### 0.4.3 Parámetros estimados para el modelo escogido.

    new_models %>% 
      select(`Lagged with sq`) %>%
      tidy() %>%
      select(-.model) %>%
      mutate(term=c("$\\phi$", "$\\Theta$", "$\\beta_1$", "$\\beta_2$")) %>%
      knitr::kable(digits=c(3, 3, 2, 4), align='lcccc', escape=FALSE,
        col.names=c("Coef.", "Estimado", "Desv. Est.", "Estadístico", "$p$"),
        caption="Parámetros estimados para el modelo de regresión con errores ARMA.")

<table>
<caption>
Table 5: Parámetros estimados para el modelo de regresión con errores
ARMA.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Coef.
</th>
<th style="text-align:center;">
Estimado
</th>
<th style="text-align:center;">
Desv. Est.
</th>
<th style="text-align:center;">
Estadístico
</th>
<th style="text-align:center;">
*p*
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
*ϕ*
</td>
<td style="text-align:center;">
-0,109
</td>
<td style="text-align:center;">
0,10
</td>
<td style="text-align:center;">
-1,0636
</td>
<td style="text-align:center;">
0,290
</td>
</tr>
<tr>
<td style="text-align:left;">
*Θ*
</td>
<td style="text-align:center;">
-1,000
</td>
<td style="text-align:center;">
0,30
</td>
<td style="text-align:center;">
-3,3307
</td>
<td style="text-align:center;">
0,001
</td>
</tr>
<tr>
<td style="text-align:left;">
*β*<sub>1</sub>
</td>
<td style="text-align:center;">
0,255
</td>
<td style="text-align:center;">
0,09
</td>
<td style="text-align:center;">
2,7851
</td>
<td style="text-align:center;">
0,006
</td>
</tr>
<tr>
<td style="text-align:left;">
*β*<sub>2</sub>
</td>
<td style="text-align:center;">
0,197
</td>
<td style="text-align:center;">
0,39
</td>
<td style="text-align:center;">
0,5105
</td>
<td style="text-align:center;">
0,611
</td>
</tr>
</tbody>
</table>

0.5 Predicción de los próximos 24 meses.
----------------------------------------

Las predicciones para los próximos 24 meses obtenidas a partir del
modelo se muestran a continuación:

    autoplot(cm_transf, colour="dodgerblue3") +
        autolayer(forecast_24_month, colour="orange") +
        scale_x_yearmonth(date_labels = "%Y") +
        theme_light() + 
        xlab('Tiempo') + ylab('Cambio proporcional') +
        theme(legend.position="none")

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/forecast-plot-1.png" style="display: block; margin: auto;" />

### 0.5.1 Predicción usando suavizado exponencial.

Ahora se ajusta un modelo usando suavizado exponencial, minimizando la
sumatoria de cuadrados, usando solo componentes aditivos para la
estacionalidad y la tendencia, obteniéndose un modelo con un RMSE de
0,054986 y MAE de 0,0371671, comparables a los obtenidos para el modelo
ajustado inicialmente, sin regresores, mostrado en la ecuación (1). Los
valores de AIC y BIC son un orden de magnitud mayor para el modelo
ajustado usando ETS.

Los valores estimados para los parámetros de suavizados son:
*α* = 0, 0062, *β* = 0, 0001, y *γ* = 0, 0087. Los valores tan pequeños
para estos parámetros indican que el nivel, tendencia, y estacionalidad
apenas varían con el tiempo.

Las predicciones se muestran en la figura
<a href="#fig:ets-forecast-plot"><strong>??</strong></a>, junto con la
predicción obtenida antes en la figura
<a href="#fig:forecast-plot"><strong>??</strong></a> (linea gris a
trozos), donde se observa que el modelo ETS ajustado predice de forma
similar la serie durante los 24 meses siguientes.

    autoplot(cm_transf, colour="dodgerblue3") +
        autolayer(ets_forecast, colour="orange") +
        autolayer(forecast_24_month, level=NULL, colour="200", linetype=2) +
        scale_x_yearmonth(date_labels = "%Y") +
        theme_light() + 
        xlab('Tiempo') + ylab('Cambio proporcional') +
        theme(legend.position="none")

<img src="/home/marcelo/MEGAsync/Msc-Math-Applied/Series Temporales/output/Lab-Session-8-FPP_files/figure-markdown_strict/ets-forecast-plot-1.png" style="display: block; margin: auto;" />

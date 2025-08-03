# Primera tarea práctica: Regresión lineal.

### Marcelo Molinatti

## Regresión lineal simple.

Considere los datos experimentales de la tabla, que se obtuvieron de $n=33$ muestras de desechos tratados químicamente en un estudio realizado en _Virginia Tech_. Se registraron los valores de _x_, la reducción porcentual de los sólidos totales, y de _y_, el porcentaje de disminución de la demanda de oxígeno químico.



Al inspeccionar dicho diagrama se observa que los puntos se acercan mucho a una línea recta, lo cual indica que la suposición de linealidad entre las dos variables parece ser razonable y el sl modelo seleccionado para el análisis.


```r
ggplot(df_RLS, aes(x=solid_reduction, y=oxigen_reduction)) +
	geom_point(size=1.8) +
	geom_smooth(method=lm, se=FALSE) + 
	xlab("Reducción de Sólidos Totales") + ylab("Reducción en la Demanda de Oxígeno") +
	theme_light()
```

<div class="figure" style="text-align: center">
<img src="Tarea-Practica-1_files/figure-html/pRLS-01-1.png" alt="Reducción de la demanda de oxigeno con respecto a la reducción de sólidos totales." width="70%" />
<p class="caption">Reducción de la demanda de oxigeno con respecto a la reducción de sólidos totales.</p>
</div>

Ajustamos un modelo de regresión lineal simple de la forma $y = \beta_0 + \beta_1 x$, para poder estimar los coeficientes del modelo de regresión $\beta_0$ y $\beta_1$


```r
fit <- lm(oxigen_reduction ~ solid_reduction, df_RLS)

as_tibble(summary(fit)$coefficients) |>
	tibble::add_column(term=c("Reducción de Sólidos", "Residuales"), .before=1) |>
	kable(digits=3, row.names=FALSE, booktabs=TRUE,
		col.names=c("", "Estimado, $\\hat{\\beta}$", "Desv. Estándar", "$\\hat{t}$", "$P(t > \\hat{t})$"), 
		caption="Resultados de la regresion lineal simple.",
		escape=FALSE, label="pRLS-01", format.args = list(big.mark=".")) %>%
	kable_classic(position = "center", latex_options = "hold_position")
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
<caption>Resultados de la regresion lineal simple.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> Estimado, $\hat{\beta}$ </th>
   <th style="text-align:right;"> Desv. Estándar </th>
   <th style="text-align:right;"> $\hat{t}$ </th>
   <th style="text-align:right;"> $P(t > \hat{t})$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Reducción de Sólidos </td>
   <td style="text-align:right;"> 3,830 </td>
   <td style="text-align:right;"> 1,768 </td>
   <td style="text-align:right;"> 2,166 </td>
   <td style="text-align:right;"> 0,038 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Residuales </td>
   <td style="text-align:right;"> 0,904 </td>
   <td style="text-align:right;"> 0,050 </td>
   <td style="text-align:right;"> 18,030 </td>
   <td style="text-align:right;"> 0,000 </td>
  </tr>
</tbody>
</table>

Como se observa, ambos estimadores (del coeficiente y de la pendiente) son significativamente distintos de cero, dado el valor de probabilidad asociado para el estadístico $\hat{t}$ que indica que una desviación tan grande solo por azar es improbable. Este estadístico busca contrastar las hipótesis $H_0: \beta_i = 0$ contra la alternativa $H_1: \beta_i \ne 0$ para cada $i=0, 1$. Como la probabilidad asociada es muy pequeña, se concluye que la probabilidad de que $\hat{\beta}_i \sim N(0, \sigma_{\beta_i})$ es muy baja y que estos deben venir de alguna otra distribución con media distinta de cero.  
Otra forma de validar el modelo de regresión es comparando el modelo planteado con un modelo nulo que no incluye pendiente (es decir, que no incluye la relación lineal con la variable independiente $x$), utilizando una prueba $F$ como se muestra en la tabla \ref{tab:pRLS-04}. 


```r
broom::tidy(aov(fit)) |>
	mutate(term=c("Reducción de Sólidos", "Residuales")) |>
	kable(digits=3, row.names=FALSE, booktabs=TRUE,
		col.names=c("", "df", "SS", "MS", "$\\hat{F}$", "$P(F > \\hat{F})$"), 
		caption="\\label{tab:pRLS-04}Tabla ANOVA para validar el modelo de regresión.",
		escape=FALSE, format.args = list(big.mark=".")) %>%
	kable_classic(position = "center", latex_options = "hold_position")
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
<caption>\label{tab:pRLS-04}Tabla ANOVA para validar el modelo de regresión.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> SS </th>
   <th style="text-align:right;"> MS </th>
   <th style="text-align:right;"> $\hat{F}$ </th>
   <th style="text-align:right;"> $P(F > \hat{F})$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Reducción de Sólidos </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3.390,551 </td>
   <td style="text-align:right;"> 3.390,551 </td>
   <td style="text-align:right;"> 325,08 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Residuales </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 323,327 </td>
   <td style="text-align:right;"> 10,430 </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
  </tr>
</tbody>
</table>

De la tabla se observa que al comparar las varianza del modelo con pendiente y el modelo nulo (solo intercepto), se obtiene un valor de $F$ mucho mas grande que el que se esperaría por azar, y es por ello que se le asocia una probabilidad aproximadamente nula (a 4 espacios decimales, el valor es cero). Esto no da la confianza de escoger el modelo de regresión como un buen modelo de trabajo que permite describir los datos observados, y realizar predicciones. De hecho, es posible calcular y obtener una medida de asociación entre las variables, $R^2$, cuyo valor es 0,9101324, el cual nos permite concluir que el modelo es capaz de explicar un 91% de la varianza observada.

Sin embargo, aun es necesario validar las suposiciones del modelo de regresión, verificando lo valores residuales. Los gráficos mostrados en la figura \ref{fig:pRLS-05} muestran que los residuales no tienen un comportamiento normal: es fácil ver del _QQ-plot_ que varias observaciones se desvían mas de los esperado, tanto por encima como por debajo. 
El gráfico de residuales a la izquierda muestra que aun se percibe cierto grado de linealidad entre los valores esperados y los residuales, y permite constatar la presencia de al menos dos residuales que se desvían mas de dos desviaciones estándar de la media de cero. 
El tercer gráfico (a la derecha) muestra la información del gráfico de residuales, pero donde cada punto se ha escalado en tamaño usando como factor la influencia que cada punto tiene sobre la estimación. Se puede observar, que las observaciones asociadas a valores grandes o pequeños de sólidos totales tienen la mayor influencia y que uno de los atípicos tiene una gran influencia en la recta estimada.


```r
augmented_fit <- broom::augment(fit)

cowplot::plot_grid(
	ggplot(augmented_fit, aes(x=.fitted, y=.std.resid)) +
		geom_point(size=1.8) +
		xlab("Predichos") + ylab("Residuales") +
		geom_hline(yintercept=c(0, -1.96, 1.96), linetype=c(1, 2, 2)) + 
		theme_light(),
	ggplot(augmented_fit, aes(sample = .resid)) + 
		stat_qq() + 
		stat_qq_line(color="blue") + 
		theme_light(),
    ggplot(mutate(augmented_fit, .too.large=ifelse(abs(.std.resid) > 1.96, 1, 0)), 
    		aes(x = .fitted, y = abs(.std.resid))) +
    	geom_point(aes(colour = .too.large, size=.hat * 100), alpha = .8) + 
    	geom_hline(yintercept = c(1.96, 3), linetype = 2, col = c("gray60", "gray50")) + 
    	xlab("Predichos") + ylab("Residuales Estandarizados") +
    	theme_bw() +
    	theme(legend.position="none") +
    	geom_text(aes(label=ifelse(.std.resid > 1.96, as.character(paste(solid_reduction, oxigen_reduction, sep=", ")), '')), 
    		hjust=0, vjust=0),
	nrow=1)
```

![\label{fig:pRLS-05}Gráficos de residuales para validación del modelo de regresión.](Tarea-Practica-1_files/figure-html/fig:pRLS-05-1.png)

Dado estos resultados, se concluye que el problema no parece ser que el modelo sea incorrecto, sino que no se esta tomando en cuenta la influencia que cada observación (particularmente los atípicos) tienen sobre la estimación de los coeficientes. Es por ello que, en lugar de una transformación de los datos, se prefirió realizar una regresión de mínimos cuadrados ponderados.

### Segundo intento: Mínimos Cuadrados Ponderados.

Se realizó una regresión de mínimos cuadrados ponderados, usando como pesos la varianza residual estimada de cada una de las observaciones:

$$w_i = \frac{1}{\hat{\sigma^2}(1 - h_{ii})}$$

donde $h_{ii}$ es la palanca (_leverage_) de la $i$-esima observación. Realizando la regresión ponderada usando estos pesos arroja los resultados mostrados en la tabla \ref{tab:pRLS-06}, donde se observa el cambio ligero en los coeficientes (los cuales aun siguen siendo significativos): aumento de la pendiente y caída del intercepto. 


```r
fit2 <- lm(oxigen_reduction ~ solid_reduction, df_RLS, 
	weights=1 / (broom::glance(fit)$sigma ** 2 * (1 - augmented_fit$.hat)))

as_tibble(summary(fit2)$coefficients) |>
	tibble::add_column(term=c("Reduccion de Solidos", "Residuales"), .before=1) |>
	kable(digits=3, row.names=TRUE, 
		col.names=c("", "Estimado, $\\hat{\\beta}$", "Desv. Estándar", "$\\hat{t}$", "$P(t > \\hat{t})$"), booktabs=TRUE,
		escape=FALSE, label="tab:pRLS-06", format.args = list(big.mark="."),
		caption="\\label{tab:pRLS-03}Resultados de la regresión lineal ponderada.") %>%
	kable_classic(position = "center", latex_options = "hold_position")
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
<caption>\label{tab:pRLS-03}Resultados de la regresión lineal ponderada.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> Estimado, $\hat{\beta}$ </th>
   <th style="text-align:right;"> Desv. Estándar </th>
   <th style="text-align:right;"> $\hat{t}$ </th>
   <th style="text-align:right;"> $P(t > \hat{t})$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Reduccion de Solidos </td>
   <td style="text-align:right;"> 3,869 </td>
   <td style="text-align:right;"> 1,672 </td>
   <td style="text-align:right;"> 2,314 </td>
   <td style="text-align:right;"> 0,027 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Residuales </td>
   <td style="text-align:right;"> 0,903 </td>
   <td style="text-align:right;"> 0,048 </td>
   <td style="text-align:right;"> 18,900 </td>
   <td style="text-align:right;"> 0,000 </td>
  </tr>
</tbody>
</table>

El modelo nuevo, sigue siendo significativo al compararlo con el modelo nulo, e incluso el error cuadrado medio es 10 veces menor en el modelo de regresión ponderado (resultados no mostrados). La superposición de la nueva recta permite darnos cuenta que la mejora no es muy grande, y que la ponderación solo resulta en un aumento de 0,744% en la varianza explicada (aunque el $R^2$ deja de ser una medida de asociación confiable en este caso).



Sin embargo, otras medidas de bondad de ajuste como el $AIC$ y $BIC$ resultaron ser mayores que los obtenidos para el modelo sin ponderación, de forma que el ajuste ponderado no es preferible al modelos de regresión lineal simple ajustado la primera vez.


```r
ggplot(df_RLS, aes(x=solid_reduction, y=oxigen_reduction)) +
	geom_point(size=1.8) +
	geom_smooth(method=lm, se=FALSE) + 
	geom_abline(slope=coef(fit2)[[2]], intercept=coef(fit2)[[1]]) + 
	xlab("Reducción de Sólidos Totales") + ylab("Reducción en la Demanda de Oxígeno") +
	theme_light()
```

![Reducción de la demanda de oxigeno con respecto a la reducción de sólidos totales.](Tarea-Practica-1_files/figure-html/fig:pRLS-08-1.png)

### Tercer intento: Transformación tentativa por polinomios.

Al revisar con mayor cuidado el gráfico de dispersión de los datos, se puede notar cierta curvatura al incrementar la reducción de sólidos totales (particularmente, los datos agrupados a magnitudes grandes de ```solid_reduction``` parecen seguir una recta distinta a la cual se accede siguiendo una curva tangente a esta y a una recta distinta que pasa por los datos agrupados para magnitudes pequeñas de ```solid_reduction```).  
Debido a esto, procedí a construir los gráficos mostrados en la figura \ref{fig:pRLS-09}, los cuales muestran la la relación entre la reducción en la demanda de oxígeno, ```oxigen_reduction```, con respecto a polinomios de la reducción de sólidos totales, ```solid_reduction```, de la forma $x^n$, para $n=1/2, 2, 3$. 


```r
cowplot::plot_grid(
	ggplot(df_RLS, aes(x=solid_reduction ** (1/2), y=oxigen_reduction)) +
		geom_point(size=1.8) +
		geom_smooth(method=lm, se=FALSE) + 
		xlab("Reducción de Sólidos Totales") + ylab("Reducción en la Demanda de Oxígeno") +
		labs(title="n=1/2") +
		theme_light(),
	ggplot(df_RLS, aes(x=solid_reduction ** 2, y=oxigen_reduction)) +
		geom_point(size=1.8) +
		geom_smooth(method=lm, se=FALSE) + 
		xlab("Reducción de Sólidos Totales") + ylab("Reducción en la Demanda de Oxígeno") +
		labs(title="n=2") +
		theme_light(),
	ggplot(df_RLS, aes(x=solid_reduction ** 3, y=oxigen_reduction)) +
		geom_point(size=1.8) +
		geom_smooth(method=lm, se=FALSE) + 
		xlab("Reducción de Sólidos Totales") + ylab("Reducción en la Demanda de Oxígeno") +
		labs(title="n=3") +
		theme_light(),
	nrow=1)
```

![\label{fig:pRLS-09}Reducción de la demanda de oxigeno con respecto a la reducción de sólidos totales.](Tarea-Practica-1_files/figure-html/pRLS-09-1.png)

Los gráficos muestran en todos los casos una curvatura: en el caso de $n=1/2$ la curvatura es convexa siguiendo algún polinomio de grado entero mayor a 1, mientras que las correspondiente a $n=2$ y $n=3$ la curvatura es cóncava y parece seguir la forma de una función con $n=1/2$. Combinando esta información, se decidió realizar una transformación de la variable independiente de la forma $p/q$ como modelo alternativo. Esta elección la hice de forma arbitraria basándome solo en los tres casos mostrados en la figura \ref{fig:pRLS-09}, pero luego determiné el coeficiente de Box-Cox el cual arrojó un valor de 1,7979798, el cual esta bastante cercano al valor de $7/4$.


```r
ggplot(df_RLS, aes(x=solid_reduction ** (7/4), y=oxigen_reduction)) +
	geom_point(size=1.8) +
	geom_smooth(method=lm, se=FALSE) + 
	xlab(expression("(Reducción de Sólidos Totales)" ** "3/2")) + 
	ylab("Reducción en la Demanda de Oxígeno") +
	theme_light()
```

![Reducción de la demanda de oxigeno con respecto a la reducción de sólidos totales transformado con $n=3/2$.](Tarea-Practica-1_files/figure-html/fig:pRLS-10-1.png)

Ajustamos un modelo de regresión lineal simple de la forma:

$$y = \beta_0 + \beta_1 x^{7/4}$$

para poder estimar los coeficientes del modelo de regresión $\beta_0$ y $\beta_1$


```r
df_RLS |> 
    mutate(solid_transformed=solid_reduction ** (7/4)) -> df_RLS

fit3 <- lm(oxigen_reduction ~ solid_transformed, df_RLS)

as_tibble(summary(fit3)$coefficients) |>
	tibble::add_column(term=c("Intercepto", "Reducción de Sólidos"), .before=1) |>
	kable(digits=3, row.names=FALSE, booktabs=TRUE,
		col.names=c("", "Estimado, $\\hat{\\beta}$", "Desv. Estándar", "$\\hat{t}$", "$P(t > \\hat{t})$"), 
		caption="Resultados de la regresion lineal simple.",
		escape=FALSE, label="pRLS-11", format.args = list(big.mark=".")) %>%
	kable_classic(position = "center", latex_options = "hold_position")
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
<caption>Resultados de la regresion lineal simple.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> Estimado, $\hat{\beta}$ </th>
   <th style="text-align:right;"> Desv. Estándar </th>
   <th style="text-align:right;"> $\hat{t}$ </th>
   <th style="text-align:right;"> $P(t > \hat{t})$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Intercepto </td>
   <td style="text-align:right;"> 12,483 </td>
   <td style="text-align:right;"> 1,475 </td>
   <td style="text-align:right;"> 8,462 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reducción de Sólidos </td>
   <td style="text-align:right;"> 0,043 </td>
   <td style="text-align:right;"> 0,003 </td>
   <td style="text-align:right;"> 16,130 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

Al igual que antes, se observa que ambos coeficientes son significativos y el modelo parece explicar la relación entre las variables. Sin embargo, se necesita saber si el nuevo modelo es significativamente distinto del modelo de regresión sin transformar, y validarlo, verificando el comportamiento de los residuales. Esta claro, al observar la tabla \ref{tab:pRLS-12} que en términos de las medidas de bondad de ajuste, el modelo sin transformar es preferible; aunque la discrepancia entre ambos es muy pequeña.


```r
broom::glance(fit) |>
	bind_rows(broom::glance(fit3)) |>
	mutate(SSE=sigma ** 2 * df.residual, MSE=sigma ** 2) |>
	select(SSE, df.residual, MSE, adj.r.squared, AIC, BIC) |>
	tibble::add_column(Model=c("Reg. Lin.", "Reg. Lin. Transf"), .before=1) |>
	kable(digits=3, row.names=FALSE, booktabs=TRUE,
		col.names=c("", "SSE", "df", "MSE", bquote("R" * "2"), "AIC", "BIC"), 
		caption="\\label{tab:pRLS-12}Tabla ANOVA para validar el modelo de regresion.",
		escape=FALSE, format.args = list(big.mark=".")) %>%
	kable_classic(position = "center", latex_options = "hold_position")
```

<table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
<caption>\label{tab:pRLS-12}Tabla ANOVA para validar el modelo de regresion.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> SSE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> MSE </th>
   <th style="text-align:right;"> "R" * "2" </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> BIC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Reg. Lin. </td>
   <td style="text-align:right;"> 323,327 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 10,430 </td>
   <td style="text-align:right;"> 0,91 </td>
   <td style="text-align:right;"> 174,961 </td>
   <td style="text-align:right;"> 179,451 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reg. Lin. Transf </td>
   <td style="text-align:right;"> 395,415 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 12,755 </td>
   <td style="text-align:right;"> 0,89 </td>
   <td style="text-align:right;"> 181,603 </td>
   <td style="text-align:right;"> 186,093 </td>
  </tr>
</tbody>
</table>

Por otro lado, al revisar los gráficos de residuales (figura \ref{fig:pRLS-13}), esta claro que el comportamiento de los mismo es mucho mejor al rededor de la media; aunque se destaca la presencia de un mayor número de datos atípicos y con desviaciones importantes. Parece entonces que la distribución subyacente de los residuales sea una distribución de colas mas pesadas que la normal. 


```r
augmented_fit3 <- broom::augment(fit3)

cowplot::plot_grid(
	ggplot(augmented_fit3, aes(x=.fitted, y=.std.resid)) +
		geom_point(size=1.8) +
		xlab("Predichos") + ylab("Residuales") +
		geom_hline(yintercept=c(0, -1.96, 1.96), linetype=c(1, 2, 2)) + 
		theme_light() +
		geom_text(aes(label=ifelse(.std.resid > 1.96, as.character(paste(round(solid_transformed, 2), oxigen_reduction, sep=", ")), '')), 
    		hjust=0, vjust=0) + 
		geom_text(aes(label=ifelse(.std.resid < -1.96, as.character(paste(round(solid_transformed, 2), oxigen_reduction, sep=", ")), '')), 
    		hjust=0, vjust=0),
	ggplot(augmented_fit3, aes(sample = .resid)) + 
		stat_qq() + 
		stat_qq_line(color="blue") + 
		theme_light(),
    ggplot(mutate(augmented_fit3, .too.large=ifelse(abs(.std.resid) > 1.96, 1, 0)), 
    		aes(x = .fitted, y = abs(.std.resid))) +
    	geom_point(aes(colour = .too.large, size=.hat * 100), alpha = .8) + 
    	geom_hline(yintercept = c(1.96, 3), linetype = 2, col = c("gray60", "gray50")) + 
    	xlab("Predichos") + ylab("Residuales Estandarizados") +
    	theme_bw() +
    	theme(legend.position="none") +
    	geom_text(aes(label=ifelse(.std.resid > 1.96, as.character(paste(round(solid_transformed, 2), oxigen_reduction, sep=", ")), '')), 
    		hjust=0, vjust=0),
	nrow=1)
```

![\label{fig:pRLS-13}Gráficos de residuales para validación del modelo de regresion.](Tarea-Practica-1_files/figure-html/fig:pRLS-13-1.png)

<!---

# Regresión múltiple con regresores variables.

Los datos de la tabla  muestran los porcentajes de las calorías totales obtenidas de los carbohidratos complejos para veinte hombres diabéticos insulina-dependientes que habían seguido una dieta alta en carbohidratos durante seis meses. Se pensó que el cumplimiento del régimen estaba relacionado con la edad (en años), el peso corporal (en relación con el peso _ideal_ para la estatura) y otros componentes de la dieta, como el porcentaje de calorías como proteína. Estas otras variables se tratan como variables explicativas.


```r
#(~Carbohydrate, ~Age, ~Weight ,~Protein
#33, 33, 100, 14,
#40, 47, 92, 15,
#37, 49, 135, 18,
#27, 35, 144, 12,
#30, 46, 140, 15,
#43, 52, 101, 15,
#34, 62, 95, 14,
#48, 23, 101, 17,
#30, 32, 98, 15,
#38, 42, 105, 14,
#50, 31, 108, 17,
#51, 61, 85, 19,
#30, 63, 130, 19,
#36, 40, 127, 20,
#41, 50, 109, 15,
#42, 64, 107, 16,
#46, 56, 117, 18,
#24, 61, 100, 13,
#35, 48, 118, 18,
#37, 28, 102, 14)
```

--->


```r
sessionInfo()
```

```
## R version 4.2.2 Patched (2022-11-10 r83330)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.6 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=es_VE.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=es_VE.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=es_VE.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=es_VE.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] tsibble_1.1.3    GGally_2.1.2     knitr_1.40       lubridate_1.8.0 
##  [5] ggfortify_0.4.15 astsa_2.0        cowplot_1.1.1    broom_1.0.1     
##  [9] dplyr_1.0.10     ggplot2_3.3.6    kableExtra_1.3.4
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.4.4         sass_0.4.2         tidyr_1.2.1        jsonlite_1.8.3    
##  [5] viridisLite_0.4.1  splines_4.2.2      bslib_0.4.1        assertthat_0.2.1  
##  [9] highr_0.9          yaml_2.3.6         progress_1.2.2     pillar_1.8.1      
## [13] backports_1.4.1    lattice_0.20-45    glue_1.6.2         digest_0.6.30     
## [17] RColorBrewer_1.1-3 rvest_1.0.3        colorspace_2.0-3   htmltools_0.5.3   
## [21] Matrix_1.5-1       plyr_1.8.7         pkgconfig_2.0.3    purrr_0.3.5       
## [25] scales_1.2.1       webshot_0.5.4      svglite_2.1.0      tibble_3.1.8      
## [29] mgcv_1.8-41        generics_0.1.3     farver_2.1.1       ellipsis_0.3.2    
## [33] cachem_1.0.6       withr_2.5.0        cli_3.4.1          magrittr_2.0.3    
## [37] crayon_1.5.2       evaluate_0.17      fansi_1.0.3        nlme_3.1-162      
## [41] MASS_7.3-58.2      anytime_0.3.9      xml2_1.3.3         prettyunits_1.1.1 
## [45] tools_4.2.2        hms_1.1.2          lifecycle_1.0.3    stringr_1.4.1     
## [49] munsell_0.5.0      compiler_4.2.2     jquerylib_0.1.4    systemfonts_1.0.4 
## [53] tinytex_0.42       rlang_1.0.6        grid_4.2.2         rstudioapi_0.14   
## [57] labeling_0.4.2     rmarkdown_2.17     gtable_0.3.1       DBI_1.1.3         
## [61] reshape_0.8.9      R6_2.5.1           gridExtra_2.3      zoo_1.8-11        
## [65] fastmap_1.1.0      utf8_1.2.2         latex2exp_0.9.5    stringi_1.7.8     
## [69] Rcpp_1.0.9         vctrs_0.5.0        tidyselect_1.2.0   xfun_0.34
```

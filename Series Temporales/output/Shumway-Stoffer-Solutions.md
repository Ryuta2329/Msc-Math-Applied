# Soluciones a problemas de _Time Series Analysis and its Applications_ de Shumway y Stoffer

### _Marcelo Molinatti_

* Ejercicios del [capitulo 1](#capitulo-1). Características de series temporales.
* Ejercicios del [capitulo 2](#capitulo-2). Análisis de datos exploratorio.
* Ejercicios del [capitulo 3](#capitulo-3). Modelos ARIMA.
* Ejercicios del [capitulo 4](#capitulo-4). Análisis espectral y filtrado.
* Ejercicios del [capitulo 5](#capitulo-5). Temas adicionales en el dominio temporal.

## Capitulo 1.


```r
suppressPackageStartupMessages({
	library(astsa)
	library(ggfortify)
})
```

**Problema 1.1** Para comparar las señales de terremotos y explosiones, represente los datos en el mismo gráfico usando diferentes colores o diferentes tipos de líneas y comente los resultados.


```r
p1 <- autoplot(cbind(EQ5, EXP6), facets = FALSE) +
  xlab("Índice de la muestra") +
  ylab("Amplitud") +
  scale_colour_manual(name = "",
    values = c("20", "200")) +
  theme_light() +
  theme(legend.position = c(0.3, 0.85))

p1
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p1-1-1.png" style="display: block; margin: auto;" />

Lo primero que sale a la vista en el gráfico es que la variabilidad de las señales de terremoto es mayor (la amplitud de la señal es mayor) y que esta variabilidad se extiende por más tiempo en el registro, mientras que la de explosiones es menor la variabilidad y la amplitud solo se extiende un intervalo de tiempo corto. 

**Problema 1.2** Considere un modelo de señal más ruido de la forma general $x_t = s_t + w_t$, donde $w_t$ es ruido blanco gaussiano con $\sigma^2_w = 1$. Simule y grafique $n = 200$ observaciones de cada uno de los siguientes dos modelos.

_a)_ $x_t = s_t + w_t$, para $t = 1, \ldots , 200$, donde:

$$s_t = \begin{cases}
    0, & t= 1, \ldots, 100\\
    10\text{exp}{-\frac{(t-100)}{20}}\text{cos}(2\pi t/4), & t=101, \ldots, 200
  \end{cases}$$

_b)_ $x_t = s_t + w_t$, para $t = 1, \ldots , 200$, donde

$$s_t = \begin{cases}
    0, & t= 1, \ldots, 100\\
    10\text{exp}{-\frac{(t-100)}{200}}\text{cos}(2\pi t/4), & t=101, \ldots, 200
  \end{cases}$$


```r
xt_a <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 20) * cos(2 * pi * 101:200 / 4)) + rnorm(200, 0, 1)
xt_b <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 200) * cos(2 * pi * 101:200 / 4)) + rnorm(200, 0, 1)
xt <- ts(cbind(xt_a, xt_b), start = 1)

p2 <- autoplot(xt, facets = TRUE) +
  xlab("Índice de la muestra") + ylab("Amplitud") + 
  theme_light()

cowplot::plot_grid(p1, p2, nrow=1)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/fig:p1-2-1.png" style="display: block; margin: auto;" />

_c)_ Compare la apariencia general de las series _a)_ y _b)_ con la serie de terremoto y la serie de explosión. Además, traza (o dibuja)
y compare los moduladores de señal _a)_ $exp\{−t/20\}$ y _b)_ $exp\{−t/200\}$, para $t = 1, 2, \ldots, 100$.

La serie en _a)_ parece describir de forma adecuada la fase P y S de las explosiones, dada la caida rapida de la amplitud al inicio de la fase S, y tambien notando que la variabilidad es bastane similar es esta serie con la serie de explosiones. Sun embargo, en la fase P, la serie del inciso _a)_ no parece realizar un buen trabajo en simular la serie de explosiones, principalemnte al inicio donde parece haber un cambio de variabilidad importante.

En el caso de la serie en el inciso _b)_, la fase S parece ser similar tambien en amplitud, aunque la serie parece ser mas regular y menos variable que la registrada en Escandinavia para la serie de terremoto. De igual forma, en la fase P la serie no tiene una variabilidad tan grande haciendo que la similitud en esta zona sea distinta (a lo mejor, un ruido aleatorio de varianza 1 no es lo suficientemente bueno para simular el proceso registrado en Escandinavia).


```r
modulators <- data.frame(mod_a=exp(-(1:100) / 20), mod_b=exp(-(1:100) / 200))

autoplot(ts(modulators, start=1), facets=FALSE) +
  xlab("t") + ylab(latex2exp::TeX("$exp\\{-t/\\tau\\}$")) + 
  scale_colour_manual(name = latex2exp::TeX("\\tau"),
  	values = c("20", "200"), labels = c("20", "200")) + 
  theme_light()
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/fig:p1-2-2-1.png" style="display: block; margin: auto;" />

Al ver los moduladores de las señales, se puede verificar que la caida exponencial es mas rapida en la señal del inciso _a)_, lo cual explica la rapida desaparicion de la señal en un intervalo corto del tiempo, mientras que la caida mas lenta del modulador de la señal del inciso _b)_ explica la persintencai de la señal en un intervalo de tiempo amplio.

**Problema 1.3** _a)_ Generar $n = 100$ observaciones a partir de la autorregresión

$$x_t = -.9x_{t-2} + w_t$$

con $\sigma_w = 1$. A continuación, aplique el filtro de promedio móvil $v_t = (x_t + x_{t−1} + x_{t−2} + x_{t−3})/4$ a $x_t$. Ahora trace $x_t$ como una línea y superponga $v_t$ como una línea discontinua. Comente sobre el comportamiento de $x_t$ y cómo la aplicación del filtro de promedio móvil cambia ese comportamiento.  
_b)_ Repite _a)_ pero con $x_t = cos(2πt/4)$.  
_c)_ Repite _b)_ pero con ruido $N(0, 1)$, $x_t = cos(2πt/4) + w_t$.  
_d)_ Compare y contraste _a)_–_c)_; i.e., como cambia el promedio móvil cada serie. 


```r
sims <- data.frame(
  # Iniciso a)
  a=stats::filter(rnorm(200, 0, 1), filter=c(0, -.9), method="recursive")[-(1:100)],
  # Iniciso b)
  b=cos(2 * pi * 1:100 / 4),
  # Iniciso c)
  c=cos(2 * pi * 1:100 / 4) + rnorm(200, 0, 1))

legend <- NULL
plot_list <- apply(sims, 2, function(ts) {
	# Aplico el filtrp sobre la serie:
	vt <- stats::filter(ts, filter=rep(1/4, 4), sides=1, method="convolution")

	# Grafico
	p1 <- autoplot(cbind(ts, vt), facets=FALSE) +
	  xlab("Índice") + ylab("") + 
	  aes(linetype = plot_group) +
	  scale_linetype_manual(name="", labels=c(expression("x"["t"]), expression("v"["t"])), values=c(1, 2)) +
  	scale_colour_manual(name="", values=c("20","1"), labels=c(expression("x"["t"]), expression("v"["t"]))) + 
	  theme_light()

	  legend <<- cowplot::get_legend(p1 + theme(legend.box.margin = margin(0, 0, 0, 1)))

	  p1
})

cowplot::plot_grid(
	cowplot::plot_grid(plotlist=lapply(plot_list, function(x) x + theme(legend.position="none")), 
		nrow=1, align = 'vh', labels = c("A", "B", "C"), hjust = -1), 
	legend, nrow = 1, rel_widths = c(3, .4))
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/fig:p1-3-1.png" width="100%" style="display: block; margin: auto;" />

Para el inciso _a)_, la serie antes del filtro muestra un cambio violento en el comportamiento de $x_t$, mientras que luego del filtro, la serie se suaviza y la variación de $v_t$ se ve menos pronunciada en amplitud (no se observan picos tan grandes).  
Para el inciso _b)_, la serie consiste de una función coseno regular, que luego de la suavización, se comporta como una constante, dado que el promediar elimina la variación dado que el periodo es de 4 unidades, y el promedio se hace con los 4 elementos inmediatamente en el pasado.  
Para el inciso _c)_, el ruido gaussiano agrega cierta variacion que quia la regularidad de la función coseno, por lo tanto la señal suavizada no se cancela en su totalidad, y esta muestra una variacion tambien, aunque su comportamiento es menos violeto. Sea como sea, aun se aprecia un póco el comportamiento de la onda coseno, tanto en $x_t$ como en $v_t$.  

Al comparar las tres series, podemos observar que el proceso de suavizar la serie cambia dependiendo de la forma de la señal subyacente. Cuando la señal varia entorno aun valor medio constante (como en el caso de la señal en A), el suavizado parece reducir la variabilidad en mayor medida que cuadno la media es variable (como en el caso de la señal en C).

**Problema 1.4** Demuestre que la función de autocovarianza se puede escribir como:

$$\gamma(s, t) = E[(x_s − \mu_s)(x_t − \mu_t)] = E(x_sx_t) − \mu_s\mu_t$$

Partiendo de la definición $E[(x_s − \mu_s)(x_t − \mu_t)]$, se distribuyen los terminos:

$$
\begin{aligned}
		\gamma(s, t) &= E[(x_s − \mu_s)(x_t − \mu_t)] \\
			&= E[x_sx_t − \mu_sx_t - x_s\mu_t + \mu_s\mu_t] \\
			&= E[x_sx_t] - \mu_sE[x_t] - \mu_tE[x_s] + \mu_s\mu_t \\
			&= E[x_sx_t] - \mu_s\mu_t - \mu_s\mu_t + \mu_s\mu_t \\
			&= E[x_sx_t] - \mu_s\mu_t
\end{aligned}
$$

**Problema 1.5** Para las dos series, $x_t$, en el Problema 1.2 _a)_ y _b)_:  
_a)_ Calcule y grafique las funciones medias $\mu_x(t)$, para $t = 1, . . . , 200$.
_b)_ Calcule las funciones de autocovarianza, $\gamma_x(s, t)$, para $s, t = 1, . . . , 200$.

Para ambos casos, la función media es:

$$
\begin{aligned}
E[x_t] &= E[s_t + w_t] \\
  &= E[s_t] + E[w_t] \\
  &= E[s_t] + 0 \\
  &= E[s_t]
\end{aligned}
$$


```r
xt_a <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 20) * cos(2 * pi * 101:200 / 4)) 
xt_b <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 200) * cos(2 * pi * 101:200 / 4)) 
xt <- ts(cbind(xt_a, xt_b), start=1)

autoplot(xt, facets=FALSE) +
	xlab("Índice de la muestra") + ylab(expression("E[x"["t"]*"]")) + 
	scale_colour_manual(name="", values=c("20","200"), labels=c("a", "b")) +
	theme_light()
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p1-5-1.png" style="display: block; margin: auto;" />

La función de autocovarianza se puede encontrar usando el resultado del problema 1.4 y el resultado anterior:

$$
\begin{aligned}
	\gamma(s, t) &= E[x_tx_s] - \mu_t\mu_s \\
		  &= E[(s_t + w_t)(s_s + w_s)] - \mu_t^2 \\
		  &= E[s_ts_s + s_tw_s + s_sw_t + w_sw_t] - \mu_t^2 \\
		  &= E[s_ts_s] + s_tE[w_s] + s_sE[w_t] + E[w_sw_t] - \mu_t^2 \\
		  &= s_ts_s + 0 + 0 + E[w_s]E[w_t] - \mu_t^2 \\
		  &= s_ts_s - \mu_t^2
\end{aligned}
$$

**Problema 1.6** Considere la serie de tiempo: $x_t = \beta_1 + \beta_2t + w_t$, donde $\beta_1$ y $\beta_2$ son constantes conocidas y $w_t$ es un proceso de ruido blanco con varianza $\sigma_w^2$.

_a)_ Determine si $x_t$ es estacionario.
_b)_ Demuestre que el proceso $y_t = x_t − x_{t−1}$ es estacionario.
_c)_ Demuestre que la media del promedio móvil:

$$v_t = \frac{1}{2q + 1}\sum_{j=-q}^q x_{t-j}$$

es $\beta_1 + \beta_2t$, y dé una expresión simplificada para la función de autocovarianza.

Para el inciso _a)_, se puede demostrar que $x_t$ no es estacionario ya que $E[x_t] = E[\beta_1 + \beta_2t + w_t] = \beta_1 + \beta_2t + E[w_t] = \beta_1 + \beta_2t$ no es independiente del tiempo. Además, la función de autocovarianza es:

$$
\begin{aligned}
	\gamma(s, t) &= E[(x_{t} - \mu_t)(x_s - \mu_s)]\\
		&= E[(\beta_1 + \beta_2t + w_t - \beta_1 - \beta_2t)(\beta_1 + \beta_2s + w_s - \beta_1 - \beta_2s)] \\
		&= E[w_tw_s] 
\end{aligned}
$$

Cuando $s=t$, entonces $\gamma(s, t) = \sigma_w^2$. Si $s\ne t$, los ruidos aleatorios son independientes y por lo tanto $\gamma(s, t) = 0$. Esta no depende de las diferencias entre $s$ y $t$.  
Por otro lado, en el inciso _b)_ se tiene el proceso $y_t = x_t − x_{t−1}$, que se escribe en términos de las variables $\beta$ como:

$$
\begin{aligned}
	y_t &= x_t − x_{t−1} \\
	  &= \beta_1 + \beta_2t + w_t - (\beta_1 + \beta_2(t-1) + w_{t-1}) \\
	  &= \beta_2 + (w_t - w_{t-1})
\end{aligned}
$$

cuya media es $E[y_t] = E[\beta_2 + (w_t - w_{t-1})] = \beta_2 + E[w_t - w_{t-1}] = \beta_2$. Su función de autocovarianza es:

$$
\begin{aligned}
	\gamma(s, t) &= E[(y_{t} - \mu_t)(y_s - \mu_s)]\\
		&= E[\beta_2 + (w_t - w_{t-1}) - \beta_2)(\beta_2 + (w_s - w_{s-1}) - \beta_2)] \\
		&= E[(w_t - w_{t-1})(w_s - w_{s-1})] 
\end{aligned}
$$

Cuando $s=t$, entonces $\gamma(s, t) = 2\sigma_w^2$. Cuando $s=t-1$, entonces $\gamma(s, t) = E[(w_t - w_{t-1})(w_{t-1} - w_{t-2})] = \sigma_w^2$; cuando $s \le t-2$, $\gamma(s, t) = 0$. Por lo que se puede escribir:

$$
\gamma_y(s, t) = \begin{cases}
		2\sigma_w^2, & \vert h\vert = 0, \\
		\sigma_w^2, & \vert h \vert = 1, \\
		0, & \vert h \vert > 1 
	\end{cases}
$$

Por lo tanto el proceso $y_t$ es estacionario.

Para el promedio móvil del inciso _c)_, se tiene una media de:

$$
\begin{aligned}
	E[v_t] &= E[\frac{1}{2q + 1}\sum_{j=-q}^q x_{t-j}] \\
		&= \frac{1}{2q + 1}E\left[\sum_{j=-q}^q x_{t-j}\right] \\
		&= \frac{1}{2q + 1}E\left[\sum_{j=-q}^q \beta_1 + \beta_2(t-j) + w_{t-j}\right] \\
		&= \frac{1}{2q + 1}E\left[ (2q + 1)\beta_1 + (2q + 1)\beta_2t + \sum_{j=-q}^q w_{t-j}\right] \\
		&= \frac{1}{2q + 1}\left((2q + 1)\beta_1 + (2q + 1)\beta_2t + E\left[\sum_{j=-q}^q w_{t-j}\right] \right)\\
		&= \beta_1 + \beta_2t + \frac{1}{2q + 1}\sum_{j=-q}^q E[w_{t-j}] \\
		&= \beta_1 + \beta_2t
\end{aligned}
$$

Para la función de autocovarianza:

$$
\begin{aligned}
	\gamma_v(s, t) &= E[(v_{t} - \mu_t)(v_s - \mu_s)] \\
		&= E\left[\frac{1}{2q + 1}\sum_{j=-q}^q x_{t-j} - \beta_1-\beta_2t)(\frac{1}{2q + 1}\sum_{j=-q}^q x_{s-j} - \beta_1- \beta_2s)\right] \\
		&= \left(\frac{1}{2q + 1}\right)^2 E\left[\left( \sum_{j=-q}^q w_{t-j} \right)\left( \sum_{j=-q}^q w_{s-j} \right)\right]
\end{aligned}
$$

Cuando $s=t$, se tiene que $\gamma_v(s, t) = \frac{\sigma_w^2}{2q + 1}$. Si $s = t-1$, se tiene:

$$
\begin{aligned}
	E\left[\left( \sum_{j=-q}^q w_{t-j} \right)\left( \sum_{j=-q}^q w_{s-j} \right)\right] &= 
			E[(w_{t+q} + w_{t+q-1} + \ldots + w_{t-q+1} + w_{t-q})(w_{t+q-1} + w_{t+q-2} + \ldots + w_{t-q} + w_{t-q-1})] \\
		&= 2q\sigma_w^2
\end{aligned}
$$

Si $s=t-2$, se tiene que solo quedan $2q - 1$ términos que no se anulan, y por lo tanto, $(2q - 1)\sigma_w^2$. De forma que se puede escribir la función de autocovarianza en términos de la diferencia $h=s-t$ como:

$$\gamma_v(s, t) = \left(\frac{2q + 1 - h}{(2q + 1)^2}\right)\sigma_w^2$$

lo cual muestra que la función de autocovarianza va decreciendo con la diferencia de tiempo $h$.

**Problema 1.7** Para un proceso de promedio móvil de la forma $x_t = w_{t−1} + 2w_t + w_{t+1}$, donde $w_t$ son independientes con medias cero y varianza $\sigma_w^2$, determine las funciones de autocovarianza y autocorrelación en función del desfase $h = s − t$ y grafique el ACF en función de $h$.

La función de autocovarianza es (dado que la media es 0, ya que se trata de la suma de la media de tres ruidos blancos): $\gamma(t,s) = E[(w_{t−1} + 2w_t + w_{t+1})(w_{s−1} + 2w_s + w_{s+1})]$. Cuando $s=t$, se tiene:

$$
\begin{aligned}
	\gamma(t,t) &= E[(w_{t−1} + 2w_t + w_{t+1})(w_{t−1} + 2w_t + w_{t+1})] \\
		&= cov(w_{t-1}, w_{t-1}) + 2 cov(w_{t}, w_{t}) + cov(w_{t+1}, w_{t+1}) \\
		&= 4\sigma_w^2
\end{aligned}
$$

Cuando la diferencia entre $s$ y $t$ es 1, solo los términos del centro y la derecha no se anulan, por lo que $\gamma(t,t\pm1) = 3\sigma_w^2$. Si la diferencia es de 2 solo un termino (el de la izquierda) no se cancela, por lo que $\gamma(t,t\pm2) = \sigma_w^2$. Para diferencias mayores que 2, la función de autocovarianza es cero. Queda entonces:

$$\gamma(t,s) = \begin{cases}
	4\sigma_w^2, & h=0 \\
	3\sigma_w^2, & h=\pm1 \\
	\sigma_w^2, & h=\pm2 \\
	0, & h=\pm3, \pm4, \ldots
\end{cases}$$

La función de autocorrelación queda:

$$\rho(t,s) = \begin{cases}
	1, & h=0 \\
	3/4, & h=\pm1 \\
	1/4, & h=\pm2 \\
	0, & h=\pm3, \pm4, \ldots
\end{cases}$$

El gráfico de esta es:


```r
df <- data.frame(lag=-5:5, rho=c(0, 0, 0, .25, .75, 1, .75, .25, 0, 0, 0))

ggplot(data = df, mapping = aes(x = lag, y = rho)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  ylab(latex2exp::TeX("$\\rho(s, t)$")) +
  theme_light()
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/fig:p1-7-1.png" style="display: block; margin: auto;" />

**Problema 1.8** Considere el modelo de paseo aleatorio con deriva $x_t = \delta + x_{t−1} + w_t$, para $t = 1, 2, \ldots$, con $x_0 = 0$, donde $w_t$ es ruido blanco con varianza $\sigma_w^2$.  
_a)_ Demuestre que el modelo se puede escribir como:

$$x_t = \delta t + \sum_{k=1}^t w_k$$

_b)_ Encuentre la función media y la función de autocovarianza de $x_t$.  
_c)_ Argumente que $x_t$ no es estacionario.  
_d)_ Muestre $\rho_x(t − 1, t) = \sqrt{\frac{t−1}{t}} \rightarrow 1$ cuando $t \rightarrow \infty$. ¿Cuál es la implicación de este resultado?  
_e)_ Sugiera una transformación para hacer que la serie sea estacionaria y demuestre que la serie transformada es estacionaria.

Para el inciso _a)_, se puede rescribir el modelo usando la definición del modelo de paseo aleatorio de forma recursiva, notando que en $t=0$ no hay una observación, por lo que definimos $x_0=0$, quedando:

$$
\begin{aligned}
	x_t &= \delta + x_{t−1} + w_t \\
		&= \delta + (\delta + x_{t-2} + w_{t-1}) + w_t = 2\delta + x_{t-2} + w_t + w_{t-1} \\
		&= 2\delta + (\delta + x_{t-3} + w_{t-2}) + w_t + w_{t-1} = 3\delta + x_{t-3} + w_t + w_{t-1} + w_{t-2} \\
		&= \ldots \\
		&= (t-1)\delta + x_1 + \sum_{j=2}^t w_j = (t-1)\delta + (\delta + x_0 + w_1) + \sum_{j=2}^t w_j \\
		&= t\delta + \sum_{j=1}^t w_j
\end{aligned}
$$

lo cual demuestra la equivalencia.  
La función media del proceso es:

$$E[x_t] = E\left[t\delta + \sum_{j=1}^t w_j\right] = t\delta + E\left[\sum_{j=1}^t w_j\right] = t\delta + \sum_{j=1}^t E[w_j] = t\delta$$

y la función de autocovarianza:

$$
\begin{aligned}
	\gamma(t,s) &= E[(x_t - \mu_t)(x_s - \mu_s)] \\
		&= E\left[\left(t\delta + \sum_{j=1}^t w_j - t\delta\right)\left(s\delta + \sum_{j=1}^s w_j - s\delta\right)\right] \\
		&= E\left[\left(\sum_{j=1}^t w_j\right)\left(\sum_{j=1}^s w_j\right)\right] \\
		&= \text{min}(s,t) \sigma_w^2
\end{aligned}
$$

Como es posible ver de las funciones de media y autocovarianza, el proceso no es estacionario dado que la media depende del valor de $t$ y crece a medida que avanza el tiempo, sin cota alguna. Mientras que la función de autocovarianza no depende de la diferencia entre $s$ y $t$, sino del mínimo valor de estos. Por lo tanto, no es posible cumplir los requisitos de estacionaridad débil, y mucho menos al estacionaridad estricta.  
Se tiene que $\rho_x(t − 1, t) = \sqrt{\frac{t−1}{t}}$. Cuando $t \rightarrow \infty$, el numerador en la ACF tiende a $t$, haciendo que $\rho_x(t-1, t) \rightarrow 1$, lo cual implica una correlación perfecta entre el valor observado en $t$ y el inmediatamente adyacente, indicando en teoría, que sería posible el predecir con certeza el valor en $t$, conociendo el valor inmediatamente anterior.  
Una transformación posible es $y_t = x_t - x_{t-1}$, de tal forma que el modelo queda como $y_t=\delta + w_t$, cuya función media es $E[y_t] = E[\delta + w_t] = \delta$, una constante; y la función de autocovarianza es $\gamma_y(t,s) = E[(\delta + w_t - \delta)(\delta + w_s - \delta)] = E[(w_t)(w_s)]$ la cual es $\sigma_w^2$ cuando $h=t-s=0$, y 0 de otra forma. 

**Problema 1.9** Una serie de tiempo con un componente periódico se puede construir a partir de $x_t = U_1 sen(2\pi w_0t) + U_2 cos(2\pi w_0t)$, donde $U_1$ y $U_2$ son variables aleatorias independientes con media cero y $E(U_1^2) = E(U_2^2) = \sigma^2$. La constante $w_0$ determina el período o tiempo que tarda el proceso en realizar un ciclo completo. Demostrar que esta serie es débilmente estacionaria con función de autocovarianza:

$$\gamma(h) = \sigma^2cos(2\pi w_0 h)$$

Se tiene que la función media es:

$$
\begin{aligned}
	E[x_t] &= E[U_1 sen(2\pi w_0t) + U_2 cos(2\pi w_0t)] \\
		&= E[U_1 sen(2\pi w_0t)] + E[U_2 cos(2\pi w_0t)] \\
		&= E[U_1] sen(2\pi w_0t) + E[U_2] cos(2\pi w_0t) \\
		&= 0 + 0 = 0
\end{aligned}
$$

lo cual muestra que la media es constante e independiente de $t$. La función de autocovarianza es:

$$
\begin{aligned}
	\gamma(h) &= E[(x_{t+h} - E[x_t])(x_{t} - E[x_{t}])] \\
		&= E[(U_1 sen(2\pi w_0(t+h)) + U_2 cos(2\pi w_0(t+h)))(U_1 sen(2\pi w_0t) + U_2 cos(2\pi w_0t))] \\
		&= E\{U_1^2 sen(2\pi w_0(t+h))sen(2\pi w_0t) + U_1U_2 [sen(2\pi w_0(t+h))cos(2\pi w_0t) + sen(2\pi w_0t)cos(2\pi w_0(t+h))] + U_2^2 cos(2\pi w_0(t+h))cos(2\pi w_0t) \} \\
		&= E[U_1^2 sen(2\pi w_0(t+h))sen(2\pi w_0t)] + E[U_2^2 cos(2\pi w_0(t+h))cos(2\pi w_0t)] \\
		&= E[U_1^2]sen(2\pi w_0(t+h))sen(2\pi w_0t) + E[U_2^2]cos(2\pi w_0(t+h))cos(2\pi w_0t) \\
		&= \sigma^2 sen(2\pi w_0(t+h))sen(2\pi w_0t) + \sigma^2 cos(2\pi w_0(t+h))cos(2\pi w_0t) \\
		&= \sigma^2 [sen(2\pi w_0(t+h))sen(2\pi w_0t) + cos(2\pi w_0(t+h))cos(2\pi w_0t)] \\
		&= \sigma^2 cos(2\pi w_0 h)
\end{aligned}
$$

donde el termino central sea nula dado que $U_1$ y $U_2$ son independientes (tal que $E[U_1U_2] = E[U_1]E[U_2] = 0$) y la identidad trigonométrica para suma de ángulos para el coseno:

$$cos(\alpha-\beta) = cos(\alpha)cos(\beta)+sen(\alpha)sen(\beta)$$

**Problema 1.10** Supongamos que nos gustaría predecir una sola serie estacionaria $x_t$ con media cero y función de autocorrelación $\rho(h)$ en algún momento en el futuro, digamos, $t + \ell$, para $l > 0$.  
_a)_ Si predecimos usando solo $x_t$ y algún multiplicador de escala $A$, demuestre que el error de predicción cuadrático medio $MSE(A) = E[(x_{t+l} - Ax_t)^2]$ es minimizado por $A = \rho(l)$.  
_b)_ Demuestre que el error de predicción cuadrático medio mínimo es $MSE(A) = \gamma(0)[1 - \rho^2(l)]$.  
_c)_ Demuestre que si $x_{t+l} = Ax_t$, entonces $\rho(l) = 1$ si $A > 0$, y $ρ(l) = −1$ si $A < 0$.

De la definición de $MSE$, se tiene por producto notable que:

$$\begin{aligned}
MSE(A) &= E[(x_{t+l} − Ax_t)^2] \\
  &= E[x_{t+l}^2 - 2Ax_tx_{t+l} + A^2x_t^2] \\
	&= E[x_{t+l}^2] - 2AE[x_tx_{t+l}] + A^2E[x_t^2]
\end{aligned}
$$

Derivando con respecto a $A$, e igualando a cero queda:

$$-2E[x_tx_{t+l}] + 2AE[x_t^2] = 0$$

Resolviendo para $A$ da:

$$A = \frac{E[x_tx_{t+l}]}{E[x_t^2]}$$

El numerador es la autocovarianza $\gamma(l)$ ya que $x_t$ tiene media cero. El denominador es la autocovarianza $\gamma(0)$, por lo que $A = \gamma(l)/\gamma(0) = \rho(l)$ y queda demostrado.

Para demostrar el inciso _b)_, solo se necesita sacar $\gamma(0)$ como factor comun de la expansión del producto notable:

$$\begin{aligned}
MSE(A) &= E[(x_{t+l} − Ax_t)^2]  \\
	&= E[x_{t+l}^2] - 2AE[x_tx_{t+l}] + A^2E[x_t^2] \\
	&= \gamma(0) - 2\frac{\gamma(l)}{\gamma(0)}\gamma(l) + \left(\frac{\gamma(l)}{\gamma(0)}\right)^2\gamma(0) \\
	&= \gamma(0)\left(1 - 2\left(\frac{\gamma(l)}{\gamma(0)}\right)^2 + \left(\frac{\gamma(l)}{\gamma(0)}\right)^2\right) \\
	&= \gamma(0)\left(1 - \left(\frac{\gamma(l)}{\gamma(0)}\right)^2\right) \\
	&= \gamma(0)\left(1 - \rho^2(l)\right)
\end{aligned}
$$

De la definición se tiene:

$$\begin{aligned}
	\rho(l) &= \frac{\gamma(l)}{\gamma(0)} \\
		&= \frac{E[x_{t+l}x_t]}{E[x_t^2]} \\
		&= \frac{AE[x_t^2]}{E[x_t^2]} \\
		&= A
\end{aligned}
$$

Si $A > 0$, entonces $\rho(l) = 1$, dado que se esta correlación seria entre $x_t$ y ella misma. Si $A < 0$, la correlación es negativa, debido al signo de $A$, pero seria la minima posible ya que la correlación seria aun entre $x_t$ y ella misma, de forma que se escribe $\rho(l) = -1$

**Problema 1.11** Considere el proceso lineal definido en (1.31), como:

$$x_t = \mu + \sum_{j=-\infty}^\infty \psi_jw_{t-j}, \qquad \sum_{j=-\infty}^\infty \vert\psi_j\vert < \infty$$

_a)_ Verifique que la función de autocovarianza del proceso está dada por (1.32):  

$$\gamma_x(h) = \sigma_w^2 \sum_{j=-\infty}^\infty \psi_{j+h}\psi_j$$

Use el resultado para verificar su respuesta al Problema 1.7. _Pista_: Para $h \ge 0$, $cov(x_{t+h}, x_t) = cov(\sum_k \psi_k w_{t+h−k}, \sum_j \psi_jw_{t−j})$. Para cada $j \in \mathbb{Z}$, el único _superviviente_ será cuando $k = h + j$.  
_b)_ Demuestre que $x_t$ existe como un límite en el cuadrado medio (vea el Apéndice A).

Para el inciso _a)_ se escribe:

$$
\begin{aligned}
    \gamma(h) &= cov(x_t,x_{t+h}) = cov\left(\mu + \sum_{j=-\infty}^\infty \psi_jw_{t-j}, \mu + \sum_{j=-\infty}^\infty \psi_jw_{t+h-j}\right) \\
        &= E\left[\left(\sum_{j=-\infty}^\infty \psi_jw_{t-j}\right)\left(\sum_{j=-\infty}^\infty \psi_jw_{t+h-j}\right)\right] \\
        &= E\left[\sum_{j=-\infty}^\infty \sum_{k=-\infty}^\infty \psi_j\psi_kw_{t-j}w_{t+h-k} \right] \\
        &= \sum_{j=-\infty}^\infty \sum_{k=-\infty}^\infty \psi_j\psi_k E[w_{t-j}w_{t+h-k}] 
\end{aligned}
$$

Para cualesquiera valor de $j$ y $k$, los ruidos blancos se cancelan por ser independientes si $t-j \ne t+h-k$. Y solo los valores para los que $k=j+h$ no se cancelan sino que $E[w_{t-j}w_{t-j}] = \sigma_w^2$, de forma que:

$$\gamma_x(h) = \sigma_w^2 \sum_{j=-\infty}^\infty \psi_{j+h}\psi_j$$





**Problema 1.12** Para dos series débilmente estacionarias $x_t$ e $y_t$, verifique (1.30): $\rho_{xy}(h) = \rho_{yx}(−h)$.

Comprobar que $\rho_{xy}(h) = \rho_{yx}(−h)$, implica verificar que $\gamma_{xy}(h) = \gamma_{yx}(−h)$. Evidentemente:

$$
\begin{aligned}
    \gamma_{xy}(h) &= E[(x_t - \mu_x)(y_{t+h} - \mu_y)] \\
        &= E[(y_{t+h} - \mu_y)(x_t - \mu_x)] \\
        &= E[(y_{s} - \mu_y)(x_{s-h} - \mu_x)] \quad \text{usando }t = s-h \\
        &= \gamma_{yx}(-h)
\end{aligned}
$$

Luego, introduciendo esto en la definición de la función de correlación cruzada demuestra la igualdad buscada $\rho_{xy}(h) = \rho_{yx}(-h)$.


**Problema 1.13** Considere las dos series $x_t = w_t$ y $y_t = w_t − \theta w_{t−1} + u_t$, donde $w_t$ y $u_t$ son series de ruido blanco independientes con varianzas $\sigma_w^2$ y $\sigma_u^2$, respectivamente, y $\theta$ es una constante no especificada.  
_a)_ Exprese el ACF, $\rho_y(h)$, para $h = 0, \pm1, \pm2, \ldots$ de la serie $y_t$ en función de $\sigma_w^2$, $\sigma_u^2$ y $\theta$.  
_b)_ Determine el CCF, $\rho_{xy}(h)$ relacionando $x_t$ y $y_t$.  
_c)_ Demuestre que $x_t$ e $y_t$ son conjuntamente estacionarios.

Para el.inciso _a)_ se tiene que $\gamma_y(0) = 2\sigma_w^2 + \sigma_u^2$, de tal forma que para $h=0$, la ACF es igual a 1. Para $h=\pm1$, solo un elemento del ruido blanco $w_t$ no se cancela, y los $u_t$ se cancelan entre si, por lo que $\rho_y(1) = \sigma_w^2 / (2\sigma_w^2 + \sigma_u^2)$. Para $h\ge2$, la ACF es cero dado que los términos de $w$ y $u$ no comparten subscritos similares. Resumiendo:

$$\rho_y(h) = \begin{cases}
    1 & h =0, \\
    \frac{\sigma_w^2}{2\sigma_w^2 + \sigma_u^2} & h =1 \\ 
    0 & h \ge 2 
\end{cases}
$$

Para el inciso _b)_, se debe calcular primero el numerador de la CCF, es decir, calcular $\gamma_{xy}(h)$, para poder encontrar una expresión simplificada para la CCF, conociendo las varianzas $x_t$ y $y_t$. Para $h=0, \pm1$, hay un termino $w_t$ en $y_{t+h}$, por lo que este no se cancela y $\gamma_{xy}(h) = \sigma_w^2$. De resto, $\gamma_{xy}(h) = 0$ y, por lo tanto, se escribe: 

$$\rho_{xy}(h) = \begin{cases}
    \frac{\sigma_w}{\sqrt{2\sigma_w^2 + \sigma_u^2}} & h =0, \pm1 \\
    0 & h\ge 2
\end{cases}
$$

Como se ve arriba, la función de covarianza cruzada $\gamma_{xy}(h)$ depende solo del retraso $h$ y, por lo tanto, las series son conjuntamente estacionarias.

**Problema 1.14** Sea $x_t$ un proceso normal estacionario con media $\mu_x$ y función de autocovarianza $\gamma(h)$. Definir la serie de tiempo no lineal $y_t = e^{x_t}$.  
_a)_ Exprese la función media $E(y_t)$ en términos de $\mu_x$ y $\gamma(0)$. La función generadora de momentos de una variable aleatoria normal $x$ con media $\mu$ y varianza $\sigma^2$ es:

$$M_x(\lambda) = E[e^{\lambda x}] = e^{\mu\lambda + \frac{1}{2}\sigma^2\lambda^2}$$

_b)_ Determine la función de autocovarianza de $y_t$. La suma de las dos variables aleatorias normales $x_{t+h} + x_t$ sigue siendo una variable aleatoria normal.

Para el inciso _a)_, se nota que $E(y_t)=E[e^{x_t}]$, la cual es la función generadora de momentos $M_x(\lambda=1)$ para $\lambda=1$. De forma que:

$$E(y_t)= e^{\mu_x + \frac{1}{2}\sigma^2}$$

La función de autocovarianza es:

$$
\begin{aligned}
    \gamma_y(h) &= E[(y_t - \mu_y)(y_{t+h} - \mu_y)] \\
        &= E\left[\left(e^{x_t} - e^{\mu + \frac{1}{2}\sigma^2}\right)\left(e^{x_{t+h}} - e^{\mu + \frac{1}{2}\sigma^2}\right)\right] \\
        &= E\left[e^{x_t + x_{t+h}} - e^{2(\mu + \frac{1}{2}\sigma^2)}\right] \\
        &= E\left[e^{x_t + x_{t+h}}\right] - e^{2\mu + \sigma^2}
\end{aligned}
$$

Como $z_t = x_t + x_{t+h}$ es otra normal, entonces $E(z_t) = 2\mu_x$ y $var(z_t)=2\sigma^2$, por lo que:

$$
\begin{aligned}
	\gamma_y(h) &= E\left[e^{x_t + x_{t+h}}\right] - e^{2\mu + \sigma^2} \\
		&= e^{2\mu + \sigma^2} - e^{2\mu + \sigma^2} \\
		&= 0
\end{aligned}
$$

**Problema 1.15** Sea $w_t$, para $t = 0, \pm1, \pm2, \ldots$ un proceso de ruido blanco normal, y considerar la serie $x_t = w_t w_{t−1}$.
Determine la media y la función de autocovarianza de $x_t$ e indique si es estacionaria.

Dado que se trata de ruido blanco $E(x_t) = E(w_tw_{t-1}) = E(w_t)E(w_{t-1}) = 0$. La función de autocovarianza viene dada por $\gamma(h) = E(x_tx_{t+h}) = E(w_tw_{t-1}w_{t+h}w_{t+h-1}) = 0$, la cual no depende de $h$, por lo que la serie no es estacionaria.

**Problema 1.16** Considere la serie $x_t = sen(2\pi U_t)$, $t = 1, 2, \ldots$, donde $U$ tiene una distribución uniforme en el intervalo $(0, 1)$.  
_a)_ Demuestre que $x_t$ es débilmente estacionario.  
_b)_ Demuestre que $x_t$ no es estrictamente estacionario.  

La serie $x_t$ esta definida dentro del intervalo 0 a $2\pi$, dado que la distribución uniforme se define en el intervalo 0 a 1. Dentro del intervalo, la función seno recorre un periodo completo, por lo que su media es cero. Formalmente:

$$E[x_t] = \int_{-1}^{1} sen(2\pi U_t) dU_t = -cos(2\pi) + cos(-2\pi) = -cos(2\pi) + cos(2\pi) = 0$$

La función de autocovarianza viene dada por:

$$\begin{aligned}
	\gamma(h) &= E\left[x_tx_{t+h}\right] \\
		&= E\left[sen(2\pi U_t)sen(2\pi U_{t+h})\right]
\end{aligned}
$$

Cuando $h=0$, se tiene que $\gamma(h) = E\left[sen^2(2\pi U_t)\right] = \int_0^1 sen^2(2\pi U_t)dU_t = 1/2$ (la varianza de la distribución uniforme en el intervalo $(0,1)$ ). Cuando $h>0$, $\gamma(h) = 0$ dado que $U_t$ y $U_{t+h}$ son independientes entre si. Como la función media es constante y la función de autocovarianza depende del retraso solamente, y como la varianza es finita, entonces se demuestra que $x_t$ es débilmente estacionaria. 





**Problema 1.17** Supongamos que tenemos el proceso lineal $x_t$ generado por $x_t = w_t − \theta w_{t−1}$, $t = 0, 1, 2, \ldots$, donde $\{w_t\}$ es independiente e idénticamente distribuida con función característica $\phi_w()$, y $\theta$ es una constante fija. [Reemplazar _función característica_ con _función generadora de momento_ si se le indica que lo haga].  
_a)_ Exprese la función característica conjunta de $x_1, x_2, \ldots, x_n$, digamos, $\phi_{x_1,x_2,\ldots,x_n}(\lambda_1, \lambda_2, \ldots , \lambda_n)$, en términos de $\phi_w()$.  
_b)_ Deducir de _a)_ que $x_t$ es estrictamente estacionario.



<!---
**Problema 1.18** Suppose that xt is a linear process of the form (1.31). Prove
∞ 
h=−∞
|γ(h)| < ∞.

**Problema 1.19** Supongamos que xt = μ + wt + θwt−1, donde wt ∼ wn(0, σw2 ).
(a) Demuestre que la función media es E(xt) = μ.
(b) Demuestre que la función de autocovarianza de xt está dada por γx(0) = σw2 (1 + θ2),
γx(±1) = σw2 θ, y γx(h) = 0 en caso contrario.
(c) Demuestre que xt es estacionario para todos los valores de θ ∈ R.
(d) Use (1.35) para calcular var(x¯) para estimar μ cuando (i) θ = 1, (ii) θ = 0 y (iii) θ = −1
(e) En las series de tiempo, el tamaño de la muestra n suele ser grande, de modo que (n − n1) ≈ 1. Teniendo esto en cuenta, comente los resultados del inciso (d); en particular, ¿cómo cambia la precisión en la estimación de la media μ para los tres casos diferentes?

**Problema 1.20** (a) Simule una serie de n = 500 observaciones de ruido blanco gaussiano como en el ejemplo 1.8 y calcule el ACF de muestra, ˆ ρ(h), con un desfase de 20. Compare el ACF de muestra que obtenga con el ACF real, ρ(h). [Recuerde el Ejemplo 1.19.]
(b) Repita la parte (a) usando solo n = 50. ¿Cómo afecta el cambio de n a los resultados?

**Problema 1.21** (a) Simule una serie de n = 500 observaciones de promedio móvil como en el ejemplo 1.9 y calcule el ACF muestral, ˆ ρ(h), con un rezago de 20. Compare el ACF muestral que obtenga con el ACF real, ρ(h). [Recuerde el Ejemplo 1.20.]
(b) Repita la parte (a) usando solo n = 50. ¿Cómo afecta el cambio de n a los resultados?

**Problema 1.22** Aunque el modelo del problema 1.2(a) no es estacionario (¿por qué?), el ACF de muestra puede ser informativo. Para los datos que generó en ese problema, calcule y trace el ACF de muestra y luego comente.

**Problema 1.23** Simule una serie de n = 500 observaciones a partir del modelo de señal más ruido presentado en el Ejemplo 1.12 con σw2 = 1. Calcule el ACF de la muestra para retrasar 100 de los datos que generó y comente.

**Problema 1.24** Para la serie de tiempo yt descrita en el ejemplo 1.26, verifique el resultado establecido de que ρy(1) = −.47 y ρy(h) = 0 para h > 1.


**Problema 1.25** Una función de valor real g(t), definida en los números enteros, es definida no negativa si y solo si

aig(ti − tj)aj ≥ 0 

para todos los enteros positivos n y para todos los vectores a = (a1, a2, . . . , an)′ y t = (t1, t2, . . . , tn)′. Para la matriz G = {g(ti − tj); yo, j = 1, 2, . . . , n}, esto implica que a′Ga ≥ 0 para todos los vectores a. Se llama definido positivo si podemos reemplazar '≥' con '>' para todo un 0, el vector cero.
(a) Demuestre que γ(h), la función de autocovarianza de un proceso estacionario, es una función definida no negativa.
(b) Verifique que la autocovarianza muestral ˆ γ(h) es una función definida no negativa.

**Problema 1.26** Considere una colección de series de tiempo x1t, x2t, . . . , xNt que están observando alguna señal común μt observada en procesos de ruido e1t, e2t, . . . , eNt, con un modelo para la j-ésima serie observada dado por
xjt = μt + ejt.
Suponga que las series de ruido tienen medias cero y no están correlacionadas para diferentes j. Las funciones de autocovarianza comunes de todas las series vienen dadas por γe(s, t). Definir la media muestral
x¯t =
1 norte
Nj=1
xjt.
(a) Demuestre que E[x¯t] = μt.
(b) Demuestre que E[(x¯t − μ)2)] = N−1γe(t, t).
(c) ¿Cómo podemos usar los resultados para estimar la señal común?

**Problema 1.27** Un concepto utilizado en geoestadística, véase Journel y Huijbregts [109] o Cressie [45], es el de variograma, definido para un proceso espacial xs, s = (s1, s2), para s1, s2 = 0, ±1, ±2, . . ., como
Vx
(h) = 1
2
E[(xs+h − xs)2],
donde h = (h1, h2), para h1, h2 = 0, ±1, ±2, . . . Muestre que, para un proceso estacionario, las funciones de variograma y autocovarianza pueden relacionarse mediante Vx(h) = γ(0) − γ(h),
donde γ(h) es la función habitual de covarianza del retraso h y 0 = (0, 0). Tenga en cuenta la fácil extensión a cualquier dimensión espacial.

**Problema 1.28** Supongamos que xt = β0 + β1t, donde β0 y β1 son constantes. Demostrar como n → ∞, ρˆx(h) → 1 para h fija, donde ˆ ρx(h) es el ACF (1.37).

**Problema 1.29** a) Suponga que xt es una serie de tiempo débilmente estacionaria con media cero y con
función de autocovarianza absolutamente sumable, γ(h), tal que
∞
h=−∞
γ(h) = 0.
Demuestre que √n x¯ →p 0, donde ¯ x es la media muestral (1.34).
(b) Dé un ejemplo de un proceso que satisfaga las condiciones de la parte (a). ¿Qué tiene de especial este proceso?

**Problema 1.30** 

**Problema 1.31** 

**Problema 1.32** 

--->

## Capitulo 2.


```r
suppressPackageStartupMessages({
	library(astsa)
	library(tsibble)
	library(fable)
	library(feasts)
	library(dplyr)
	library(ggplot2)
	library(lubridate)
	library(kableExtra)
})
```

**Problema 2.1** Un modelo estructural para los datos de Johnson y Johnson, digamos $y_t$, sea $x_t = log(y_t)$. En este problema, vamos a ajustar un tipo especial de modelo estructural, $x_t = T_t + S_t + N_t$ donde $T_t$ es un componente de tendencia, $S_t$ es un componente estacional y $N_t$ es ruido. En nuestro caso, el tiempo $t$ está en trimestres ($1960{,}00, 1960{,}25, \ldots$) por lo que una unidad de tiempo es un año.  
_a)_ Ajuste el modelo de regresión

$$x_t = \beta t + \alpha_1 Q_1(t) + \alpha_2 Q_2(t) + \alpha_3 Q_3(t) + \alpha_4 Q_4(t) + w_t$$

donde $Q_i(t) = 1$ si el tiempo $t$ corresponde al trimestre $i = 1, 2, 3, 4$ y cero en caso contrario. Las $Q_i(t)$ se denominan variables indicadoras. Supondremos por ahora que $w_t$ es una secuencia de ruido blanco gaussiana.  
_b)_ Si el modelo es correcto, ¿cuál es el incremento anual promedio estimado en las ganancias registradas por acción?  
_c)_ Si el modelo es correcto, ¿la tasa promedio de ganancias registradas aumenta o disminuye del tercer trimestre al cuarto trimestre? y ¿en qué porcentaje aumenta o disminuye?  
_d)_ ¿Qué sucede si incluye un término de intersección en el modelo en _a)_? Explique por qué hubo un problema.  
_e)_ Grafique los datos, $x_t$, y superponga los valores ajustados, digamos $\hat{x}_t$, en el gráfico. Examine los residuos, $x_t - \hat{x}_t$, y establezca sus conclusiones. ¿Parece que el modelo se ajusta bien a los datos (los residuos se ven blancos)?

La <a href="#p02-01-01">figura 1</a> muestra las ganancias trimestrales por acción de la empresa estadounidense _Johnson & Johnson_, proporcionada por el profesor Paul Griffin (comunicación personal) de la _Graduate School of Management_ de la Universidad de California, Davis. Hay 84 trimestres (21 años) medidos desde el primer trimestre de 1960 hasta el último trimestre de 1980.

<a name="p02-01-01"></a>

```r
autoplot(jj) +
  ggtitle("Pasajeros de la Clase Económica: Melbourne-Sydney") +
  xlab("Año") +
  ylab("Miles") +
  theme_light()
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-01-01-1.png" alt="Serie temporal para los Pasajeros de la Clase Económica: Melbourne-Sydney."  />
</div><table><caption>Figura 1. Serie temporal para los Pasajeros de la Clase Económica: Melbourne-Sydney.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Para transformar los datos, reorganizo los datos a un formato largo, y creo los regresores para el elemento de tendencia (usando `lubridate::year`) y para los cuartos de cada año (usando `lubridate::quarter`). Luego, para este ultimo, se crearon 4 variables distintas $Q_i$ usando una expresion condicional sobe `quarter`.

Ajustando el modelo de regresión, arroja los siguientes resultados mostrados en la <a href="#p02-01-03">tabla 1</a>. Los resultados del ajuste se interpretan tomando en cuenta el uso de variables indicadoras: `year` corresponde al elemento de tendencia $\beta$, y cada `Q1, Q2, Q3` y `Q4` corresponde a los valores de $\alpha_i$.

<a name="p02-01-03"></a>

```r
# Transformando ls datos para obtener y_t
# y generar los regresores t y Q_i
df_jj <- jj |>
	as_tsibble(key=c("Q1", "Q2", "Q3", "Q4")) |>
	mutate(log_Earnings = log(value), 
		year = year(index), 
		quarter=quarter(index),
		Q1=ifelse(quarter == 1, 1, 0),
		Q2=ifelse(quarter == 2, 1, 0),
		Q3=ifelse(quarter == 3, 1, 0),
		Q4=ifelse(quarter == 4, 1, 0))

# Ajuste del modelo
# mod <- lm(log_Earnings ~ year + quarter, df_jj)
mod <- lm(log_Earnings ~ year + Q1 + Q2 + Q3 + Q4 - 1, df_jj)

# Resultados de la estimacion.
broom::tidy(mod) |> 
	kbl(digits=4, 
		col.names=c("Regresor", "$\\beta_i$", "$\\sigma_i$", "$Z$", "$p$"),
		caption="Resultados de la regresión lineal: Estimadores para los coeficientes del modelo.", 
		escape=FALSE)
```

<table>
<caption>Tabla 1. Resultados de la regresión lineal: Estimadores para los coeficientes del modelo.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Regresor </th>
   <th style="text-align:right;"> $\beta_i$ </th>
   <th style="text-align:right;"> $\sigma_i$ </th>
   <th style="text-align:right;"> $Z$ </th>
   <th style="text-align:right;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> year </td>
   <td style="text-align:right;"> 0,1672 </td>
   <td style="text-align:right;"> 0,0023 </td>
   <td style="text-align:right;"> 73,9990 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q1 </td>
   <td style="text-align:right;"> -328,2764 </td>
   <td style="text-align:right;"> 4,4505 </td>
   <td style="text-align:right;"> -73,7611 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q2 </td>
   <td style="text-align:right;"> -328,2065 </td>
   <td style="text-align:right;"> 4,4505 </td>
   <td style="text-align:right;"> -73,7454 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q3 </td>
   <td style="text-align:right;"> -328,0946 </td>
   <td style="text-align:right;"> 4,4505 </td>
   <td style="text-align:right;"> -73,7202 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q4 </td>
   <td style="text-align:right;"> -328,3215 </td>
   <td style="text-align:right;"> 4,4505 </td>
   <td style="text-align:right;"> -73,7712 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

Los resultados muestran que todos los coeficientes son significativos, con $p$-valores mucho menores a $0{,}01$. Es de hacer notar que la diferencia entre la contribución de cada cuarto a las ganancias (en escala logarítmica) es bastante similar, variando solo en las décimas o centésimas.  
Los cambios anuales en las ganancias viene dado por el coeficiente para $t$ (`year`) de la siguiente forma:

$$x_t - x_{t-1} = \beta t - \beta(t-1) = \beta$$

donde los términos para los cuartos se cancelan en la diferencia. Usando $log(y_t) - log(y_{t-1}) = log(y_t/y_{t-1})$ permite obtener:

$$y_t = y_{t-1}e^\beta$$

Es decir, anualmente hay un incremento promedio de $e^\beta =$ 1,182 en las ganancias por acción, suponiendo que el modelo es correcto.  
Los incrementos/decrementos de cuarto a cuarto en un mismo año vienen dados por:

$$x_{t,Q_i} - x_{t-1,Q_{i-1}} = \alpha_i - \alpha_{i-1}$$

y al devolver la transformación, como antes, se obtiene:

$$y_{t, Q_i} = y_{t, Q_{i-1}}e^{\alpha_i - \alpha_{i-1}}$$

Para el incremento del cuarto trimestre al primero, se da un incremento de año de forma que el termino de tendencia no desaparece. Pero por propiedad de exponenciales, este se se puede separar del cambio de cuarto de año como: $y_{t, Q_1} = y_{t-1, Q_4}e^{\beta}e^{\alpha_4 - \alpha_1}$. Los cambios de un trimestre a otro se muestran en la <a href="#p02-01-04">tabla 2</a>.

<a name="p02-01-04"></a>

```r
# Etiquetas para cada uno de los cambios de cuarto
labels <- c(
	"$\\alpha_2-\\alpha_1$",
	"$\\alpha_3-\\alpha_2$",
	"$\\alpha_4-\\alpha_3$",
	"$\\alpha_1-\\alpha_4$")

# Incrementos de cuarto a cuarto
increment <- exp(diff(coef(mod)[c("Q1", "Q2", "Q3", "Q4", "Q1")]))

# Tabla de incrementos cuarto a cuarto
tibble(Etiqueta=labels, Incremento=increment) %>%
	mutate(Porcentaje=100 * (Incremento - 1)) %>%
	kbl(digits=4,
	  caption="Incrementos en las ganancias promedio por acción trimestre a trimestre.")
```

<table>
<caption>Tabla 2. Incrementos en las ganancias promedio por acción trimestre a trimestre.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Etiqueta </th>
   <th style="text-align:right;"> Incremento </th>
   <th style="text-align:right;"> Porcentaje </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\alpha_2-\alpha_1$ </td>
   <td style="text-align:right;"> 1,0724 </td>
   <td style="text-align:right;"> 7,2418 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_3-\alpha_2$ </td>
   <td style="text-align:right;"> 1,1184 </td>
   <td style="text-align:right;"> 11,8403 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_4-\alpha_3$ </td>
   <td style="text-align:right;"> 0,7969 </td>
   <td style="text-align:right;"> -20,3051 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_1-\alpha_4$ </td>
   <td style="text-align:right;"> 1,0462 </td>
   <td style="text-align:right;"> 4,6182 </td>
  </tr>
</tbody>
</table>

Como se observa, a excepción del paso del tercer trimestre al cuarto, siempre ocurre un incremento en las ganancias promedio por acción: de $7{,}24$% en el $Q2$, de $11{,}8$% en el $Q3$, y un incremento de $4{,}62$% al pasar al $Q1$. Los resultados muestran que la caída en las ganancias al pasar al $Q4$ es el mayor cambio en las ganancias por acción en cada año, de $20{,}3$%. 

Al intentar añadir un coeficiente, el ajuste es capaz de determinarlo, pero arroja ```NA``` para el coeficiente $\alpha_4$. 
La razón de esto es que al añadir el termino para el coeficiente, este se toma como un caso base (en este caso, $Q1$. 
El coeficiente estimado corresponde entonces a $\alpha_1$), y cada uno de los términos $Q_i$ para $i=2, 3, 4$ se determinan como cambios (cuanto por encima o por debajo del intercepto) con respecto al intercepto: 
de forma que ```Q1``` en el modelo con intercepto es en realidad la diferencia $\alpha_2 - \alpha_1$; ```Q2``` en el modelo con intercepto es en realidad la diferencia $\alpha_3 - \alpha_1$; y ```Q3``` en el modelo con intercepto es en realidad la diferencia $\alpha_4 - \alpha_1$. Pero dado que ya se sacaron todas las diferencias entre cada cuarto con respecto al caso base (primer cuarto), el ultimo coeficiente no significa nada, arrojando el valor de ```NA```.

El gráfico para el modelo se observa en la <a href="#p02-01-05">figura 2</a>, donde se observa que la serie ajustada esta ligeramente mas suavizada que la serie observada, aunque se nota que de 1962 a 1965 la series ajustada parece sobrestimar de manera sistemática la serie observada, mientras que de 1970 a 1975 el modelo parece subestimar de forma sistemática la serie observada. El resto del tiempo, la serie parece variar por encima o por debajo.

<a name="p02-01-05"></a>

```r
augmented_df <- left_join(df_jj, broom::augment(mod))

ggplot(df_jj[, c("index", "log_Earnings")], aes(x=as.Date(index), y=log_Earnings)) +
	geom_line() + geom_point() + 
	geom_line(aes(y=.fitted), 
		data=augmented_df[, c("index", ".fitted")],
		color="orange") + 
	geom_point(aes(y=.fitted), 
		data=augmented_df[, c("index", ".fitted")], 
		color="orange") +
  ggtitle("Pasajeros de la Clase Económica: Melbourne-Sydney (escala logarítmica)") +
  xlab("Año") +
  ylab("log Ganancias por Acción") + 
  theme_light()
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-01-05-1.png" alt="Serie observada superpuesta con la serie predicha por el modelo ajustado."  />
</div><table><caption>Figura 2. Serie observada superpuesta con la serie predicha por el modelo ajustado.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Al verificar los residuales, se observa en la <a href="#p02-01-06">figura 3</a> que la serie tiene un comportamiento que no se toma en cuenta en el modelo, alguna correlación entre los valores adyacentes. También se observan valores con mas de dos desviaciones estándar, y en general cambios mas violentos, al inicio y en el centro de la serie.

<a name="p02-01-06"></a>

```r
ggplot(augmented_df[, c(".fitted", ".std.resid")], 
	aes(x=.fitted, y=.std.resid)) +
	geom_line(colour="orange") +
	geom_point(colour="orange") +
	geom_hline(yintercept=0) +
	xlab("Valores Estimados") +
  ylab("Residuales Estandarizados") + 
  theme_light()
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-01-06-1.png" alt="Gráfico de residuales versus predichos"  />
</div><table><caption>Figura 3. Gráfico de residuales versus predichos</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Las gráficas de la figura <a href="#p02-01-07">figura 4</a> muestran las función de autocorrelación y autocorrelación parcial. Se observa de inmediato que la caída de la primera es bastante lenta, indicando una correlación bastante grande entre elementos adyacentes en la serie; mientras que la función de autocorrelación parcial muestra que la correlación parece ser mayor solo entre observaciones separadas a intervalo de un año.

<a name="p02-01-07"></a>

```r
# Se calculan las correlaciones y correlaciones parciales
df_values <- data.frame(lag=1:19, 
	acf_vals=acf(augmented_df$log_Earnings, plot=FALSE)$acf[,,1][1:19],
	pacf_vals=pacf(augmented_df$log_Earnings, plot=FALSE)$acf[,,1])

cowplot::plot_grid(
	ggplot(data = df_values, mapping = aes(x = lag, y = acf_vals)) +
	  geom_point() +
	  geom_hline(aes(yintercept = 0)) +
	  geom_segment(mapping = aes(xend = lag, yend = 0)) +
	  geom_hline(yintercept = c(1, -1) / sqrt(20), linetype=2, color='blue') +
	  ylab(latex2exp::TeX("$\\rho(s, t)$")) +
	  theme_light(), 
  ggplot(data = df_values, mapping = aes(x = lag, y = pacf_vals)) +
	  geom_point() +
	  geom_hline(aes(yintercept = 0)) +
	  geom_hline(yintercept = c(1/sqrt(20), -1/sqrt(20)), linetype=2, color='blue') +
	  geom_segment(mapping = aes(xend = lag, yend = 0)) +
	  ylab(latex2exp::TeX("$\\rho(s, t)$")) +
	  theme_light(), 
	nrow=1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-01-07-1.png" alt="ACF y PACF"  />
</div><table><caption>Figura 4. ACF y PACF</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La información mostrada en las funciones de autocorrelación y en la distribución de los residuales parece indicar que el modelo no es del todo correcto y que sera apropiado ajustar un modelo autoregresivo.

**Problema 2.2** Para los datos de mortalidad cardiovascular:  
_a)_ Agregue otro componente a la regresión que represente el conteo de partículas cuatro semanas antes; es decir, agregue $P_{t−4}$. Exprese su conclusión.
_b)_ Dibuje una matriz de diagrama de dispersión de $M_t$, $T_t$, $P_t$ y $P_{t−4}$ y luego calcule las correlaciones por pares entre las series. Compare la relación entre $M_t$ y $P_t$ versus $M_t$ y $P_{t−4}$.


La primera parte del análisis se encuentra en el archivo de ejemplo para [la mortalidad cardiovascular y partículas contaminantes](https://github.com/Ryuta2329/Msc-Math-Applied/blob/main/Series%20Temporales/colab-nb/Pollution-Mortality-example.ipynb). Allí, se muestra que el mejor modelo encontrado para mortalidad fue:

$$Mt = 2831,5 -1,396t  -0,472(T_t − T_{.}) + 0,023(T_t − T_{.})^2 + 0,255P_t + w_t$$

En esta parte se busca añadir la información del retraso $P_{t-4}$ para verificar si hay una mejora en el ajuste. Los resultados se muestran a continuación:


```r
df_ts <- cbind(part, tempr, cmort)

# Se crea la variable retraso.
pt_4 <- ts.intersect(df_ts, pt_4=stats::lag(df_ts[, "part"],-4), dframe=TRUE)
colnames(pt_4) <- c("Particulas", "Temperatura", "Mortalidad", "P_t-4")

# Datos para la regresión7
df_lag <- pt_4 %>%
	mutate(trend = time(cmort)[-(505:508)], 
		diff_Temp=Temperatura - mean(Temperatura),
		diff_Temp_Square=diff_Temp ** 2)

mod_lag <- lm(Mortalidad ~ trend + diff_Temp + diff_Temp_Square + Particulas + `P_t-4`, df_lag)
bind_rows(fitted_models[4,], glance(mod_lag)) %>%
	mutate(SSE=sigma ** 2 * df.residual, MSE=sigma ** 2) %>%
	select(SSE, df.residual, MSE, adj.r.squared, AIC, BIC) %>%
	mutate(AIC=AIC / nrow(df_ts_tidy) - log(2*pi), BIC=BIC / nrow(df_ts_tidy) - log(2*pi)) %>%
	tibble::add_column(Model=c(
		"$M_t = \\beta_0 + \\beta_1 t + \\beta_2(T - T_{.} ) + \\beta_3(T - T_{.} )^2 + \\beta_4 P_t + w_t$", "$M_t = \\beta_0 + \\beta_1 t + \\beta_2(T - T_{.} ) + \\beta_3(T - T_{.} )^2 + \\beta_4 P_t + P_{t-4} + w_t$"), .before=1) %>%
	kbl(digits=3,
		col.names=c("", "SSE", "df", "MSE", "$R^2$", "AIC", "BIC"),
		caption="Medidas de ajuste y de información para los modelos ajustados.", 
		escape=FALSE)
```

<table>
<caption>Tabla 3. Medidas de ajuste y de información para los modelos ajustados.</caption>
 <thead>
  <tr>
   <th style="text-align:left;">  </th>
   <th style="text-align:right;"> SSE </th>
   <th style="text-align:right;"> df </th>
   <th style="text-align:right;"> MSE </th>
   <th style="text-align:right;"> $R^2$ </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> BIC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $M_t = \beta_0 + \beta_1 t + \beta_2(T - T_{.} ) + \beta_3(T - T_{.} )^2 + \beta_4 P_t + w_t$ </td>
   <td style="text-align:right;"> 20508,44 </td>
   <td style="text-align:right;"> 503 </td>
   <td style="text-align:right;"> 40,772 </td>
   <td style="text-align:right;"> 0,592 </td>
   <td style="text-align:right;"> 4,722 </td>
   <td style="text-align:right;"> 4,772 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $M_t = \beta_0 + \beta_1 t + \beta_2(T - T_{.} ) + \beta_3(T - T_{.} )^2 + \beta_4 P_t + P_{t-4} + w_t$ </td>
   <td style="text-align:right;"> 19687,01 </td>
   <td style="text-align:right;"> 498 </td>
   <td style="text-align:right;"> 39,532 </td>
   <td style="text-align:right;"> 0,604 </td>
   <td style="text-align:right;"> 4,641 </td>
   <td style="text-align:right;"> 4,700 </td>
  </tr>
</tbody>
</table>

Los resultados muestran que hay una mejora en el ajuste al añadir a $P_{t-4}$, pero que solo resulta en un aumento de la varianza explicada de 1,2%. Podemos verificar si el modelo es significativo por medio de la prueba $F$:

$$F(5, 498) = \frac{(40.8 - 39.5) / 5}{39.5 / 498} = 1.3218\times 10^{-5}$$

Este valor de $F$ tiene una probabilidad asociada de $F(5, 498) = 1$. Esto quiere decir que no hay una mejora significativa en el modelo al añadir a $P_{t-4}$ comparado con el modelo sin este termino.

Al realizar los gráficos de dispersión con los pares de variables del modelo, y calcular el coeficiente de correlación de estas, se puede observar que la correlación entre $M_t$ y $P_t$ versus $M_t$ y $P_{t−4}$, es bastante similar, siendo mayor par el ultimo caso. Dada esta correlación, seria mas apropiado elegir un modelo en el cual solo se haga una relación entre $M_t$ y $P_{t−4}$, eliminando el termino de $P_t$.


```r
pt_4 |> GGally::ggpairs(progress = FALSE)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/fig:p02-02-03-1.png" style="display: block; margin: auto;" />

**Problema 2.3** En este problema, exploramos la diferencia entre una caminata aleatoria y un proceso estacionario de tendencia.  
_a)_ Genere cuatro series que sean paseo aleatorio con deriva, de longitud $n = 100$ con $\delta = {,}01$ y $\sigma_w = 1$. Llame a los datos $x_t$ para $t = 1, \ldots, 100$. Ajuste la regresión $x_t = \beta t + w_t$ usando mínimos cuadrados. Grafique los datos, la función media verdadera (es decir, $\mu t = {,}01 t$) y la línea ajustada, $\hat{x}_t = \hat{\beta} t$, en el mismo gráfico.   
_b)_ Genere cuatro series de longitud $n = 100$ que sean de tendencia lineal más ruido, digamos $y_t = {,}01 t + w_t$, donde $t$ y $w_t$ son como en la parte _a)_. Ajuste la regresión $y_t = \beta t + w_t$ usando mínimos cuadrados. Grafique los datos, la función media verdadera y la línea ajustada.   
_c)_ Comente (qué aprendió de esta tarea).   

En la siguiente figura se observan los graficos para los incisos _a)_ y _b)_:


```r
set.seed(24644350)
sim_series <- list(xt_1 = ts(cumsum(rnorm(100, .01))),
	xt_2 = ts(cumsum(rnorm(100, .01))),
	xt_3 = ts(cumsum(rnorm(100, .01))),
	xt_4 = ts(cumsum(rnorm(100, .01)))
  )

# Regresiones
fitted_values <- purrr::map(sim_series, ~fitted(lm(.~ time(.))))

# Joining
p1 <- bind_rows(sim_series) %>% 
  tidyr::gather(key="serie", value="sim") %>%
  bind_cols( 
  	bind_cols(fitted_values) %>% 
  		tidyr::gather(key="serie", value="fitted") %>%
  		select(-serie)) %>%
  ggplot(aes(x=rep(1:100L, 4), y=sim, colour=serie)) +
    geom_line() + geom_line(aes(y=fitted)) + 
    geom_abline(slope=.1, linetype=2) + 
    xlab("") + ylab("") +
    theme_light() +
    theme(legend.position="none")

sim_series <- list(xt_1 = ts(.1 * 1:100 + rnorm(100)),
	xt_2 = ts(.1 * 1:100 + rnorm(100)),
	xt_3 = ts(.1 * 1:100 + rnorm(100)),
	xt_4 = ts(.1 * 1:100 + rnorm(100))
  )

# Regresiones
fitted_values <- purrr::map(sim_series, ~fitted(lm(.~ time(.))))

# Joining
p2 <- bind_rows(sim_series) %>% 
  tidyr::gather(key="serie", value="sim") %>%
  bind_cols( 
  	bind_cols(fitted_values) %>% 
  		tidyr::gather(key="serie", value="fitted") %>%
  		select(-serie)) %>%
  ggplot(aes(x=rep(1:100L, 4), y=sim, colour=serie)) +
    geom_line() + geom_line(aes(y=fitted)) + 
    geom_abline(slope=.1, linetype=2) + 
    xlab("") + ylab("") +
    theme_light() +
    theme(legend.position="none")

cowplot::plot_grid(p1, p2, labels=c("a)", "b)"))
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-03-01-1.png" style="display: block; margin: auto;" />

Se puede ver que los procesos que son paseos aleatorios son completamente distintos a un proceso con tendencia lineal. En el caso de los paseos aleatorios, la serie se genera de un proceso normal con valor medio dado por el _drift_, donde cada nuevo valor es dependiente unicamente del valor anterior en el proceso. 
Por otro lado, el modelo con tendencia sigue una estructura de dependencia que solo depende del tiempo, pero no de valores anteriores de la serie. Lo cual hace que la serie solo fluctué alrededor de su valor medio.

**Problema 2.4 Información de Kullback-Leibler**. Dado el vector aleatorio $n\times1$ $y$, definimos la información para discriminar entre dos densidades en la misma familia, indexadas por un parámetro $\theta$, digamos $f(y; \theta_1)$ y $f(y; \theta_2)$, como:

$$I(\theta_1;\theta_2) = n^{-1}E_1 log\frac{f(y; \theta_1)}{f(y; \theta_2)}$$

donde $E_1$ denota expectativa con respecto a la densidad determinada por $\theta_1$. Para el modelo de regresión gaussiana, los parámetros son $\theta = (\beta^\prime, \sigma^2)^\prime$. Muestra que:

$$I(\theta_1;\theta_2) = \frac{1}{2}\left(\frac{\sigma^2_1}{\sigma^2_2} - log\frac{\sigma^2_1}{\sigma^2_2} - 1\right) + \frac{1}{2}\frac{(\beta_1 - \beta_2)^\prime Z^\prime Z(\beta_1 - \beta_2)}{n\sigma_2^2}$$

<!---
La función de densidad gaussiana es:

$$f(x; \theta) = \frac{1}{\sigma\sqrt{2\pi}}e^{-\frac{1}{2}\frac{\beta^\prime Z^\prime Z\beta}{\sigma^2}}$$

donde $\theta^\prime=(\beta^\prime, \sigma^2)^\prime$. De esta forma se tiene:

$$
\begin{aligned}
	I(\theta_1;\theta_2) &= n^{-1}\text{log }f(y; \theta_1) - \text{log }f(y; \theta_2) \\
		&= n^{-1}\left[\text{log }\left(\frac{1}{\sigma_1\sqrt{2\pi}}e^{-\frac{1}{2}\frac{\beta_1^\prime Z^\prime Z\beta_1}{\sigma_1^2}}\right) - \text{log }\left(\frac{1}{\sigma_2\sqrt{2\pi}}e^{-\frac{1}{2}\frac{\beta_2^\prime Z^\prime Z\beta_2}{\sigma_2^2}}\right)\right] \\
		&= n^{-1}\left[\text{log }\left(\frac{\sigma_1}{\sigma_2}\right) - \frac{1}{2}\frac{\beta_1^\prime Z^\prime Z\beta_1}{\sigma_1^2} + \frac{1}{2}\frac{\beta_2^\prime Z^\prime Z\beta_2}{\sigma_2^2}\right] \\
		&= n^{-1}
\end{aligned}
$$
--->

**Problema 2.5 Selección del modelo.** Ambos criterios de selección (2.15) y (2.16) se derivan de argumentos teóricos de la información, basados en los bien conocidos números de discriminación de información de Kullback-Leibler. Consideramos que la medida dada por la ecuación obtenida en el problema anterior mide la discrepancia entre las dos densidades, caracterizada por los valores de los parámetros $\theta_1^\prime = (\beta_1^\prime, \sigma_1^2)^\prime$ y $\theta_2^\prime = (\beta_2^\prime, \sigma_2^2)^\prime$. Ahora, si el verdadero valor del vector de parámetros es $\theta_1$, argumentamos que el mejor modelo sería uno que minimice la discrepancia entre el valor teórico y la muestra, digamos $I(\theta_1; \hat{\theta})$. Debido a que no se conocerá $\theta_1$, Hurvich y Tsai consideraron encontrar un estimador insesgado para $E_1[I(\beta_1, \sigma_1^2; \hat{β},\hat{σ}^2)]$, donde

$$I(\beta_1, \sigma_1^2; \hat{\beta},\hat{\sigma}^2) = \frac{1}{2}\left(\frac{\sigma^2_1}{\sigma^2_2} - log\frac{\sigma^2_1}{\sigma^2_2} - 1\right) + \frac{1}{2}\frac{(\beta_1 - \beta_2)^\prime Z^\prime Z(\beta_1 - \beta_2)}{n\sigma_2^2}$$

y $\beta$ es un vector de regresión $k \times 1$. Muestra que:

$$E_1[I(\beta_1, \sigma_1^2; \hat{\beta},\hat{\sigma}^2)] = \frac{1}{2}\left(-log\sigma_1^2 + E_1 log\hat{\sigma^2} + \frac{n+k}{n-k-2} - 1\right)$$

utilizando las propiedades distributivas de los coeficientes de regresión y la varianza del error. Un estimador insesgado para $E_1 log \hat{\sigma}^2$ es $log \hat{\sigma}^2$. Por lo tanto, hemos demostrado que la expectativa de la información de discriminación anterior es como se afirma. Como se consideran modelos con diferentes dimensiones $k$, solo variarán el segundo y el tercer término en la ecuación anterior y solo necesitamos estimadores insesgados para esos dos términos. Esto da la forma de $AICc$. Necesitarás los dos resultados distributivos

$$\frac{n\hat{\sigma}^2}{\sigma_1^2} \sim \chi_{n-k}^2 \text{ y } \frac{(\hat{\beta} - \beta_1)^\prime Z^\prime Z(\hat{\beta} - \beta_1)}{\sigma_1^2} \sim \chi_k^2$$

Las dos cantidades se distribuyen de forma independiente como distribuciones chi-cuadrado con los grados de libertad indicados. Si $x \sim \chi_n^2$, $E(1/x) = 1/(n − 2)$.

**Problema 2.6** Considere un proceso que consiste en una tendencia lineal con un término de ruido aditivo que consiste en variables aleatorias independientes $w_t$ con medias cero y varianzas $\sigma_w^2$, es decir,

$$x_t = \beta_0 + \beta_1 t + w_t$$

donde $\beta_0$, $\beta_1$ son constantes fijas.  
_a)_ Demuestre que $x_t$ no es estacionario.  
_b)_ Demuestre que la serie en primera diferencia $\nabla x_t = x_t - x_{t-1}$ es estacionaria encontrando su media y su función de autocovarianza.  
_c)_ Repita el inciso _b)_ si $w_t$ se reemplaza por un proceso estacionario general, digamos $y_t$, con función media $\mu_y$ y función de autocovarianza $\gamma_y(h)$.  

La media de $x_t$ es:

$$E(x_t) = E(\beta_0 + \beta_1 t + w_t) = \beta_0 + \beta_1 t + E(w_t) = \beta_0 + \beta_1 t$$

la cual crece indefinidamente con el tiempo, por lo que no es constante. Luego, $x_t$ no es estacionario.  
La diferencia $\nabla x_t = x_t - x_{t-1} = \beta_0 + \beta_1 t + w_t - \beta_0 - \beta_1 (t - 1) - w_{t-1} = \beta_1 + w_t - w_{t-1}$ tiene por media:

$$E(\nabla x_t) = E(\beta_1 + w_t - w_{t-1}) = \beta_1 + E(w_t) - E(w_{t-1}) = \beta_1$$

la cual es constante, por lo que $\nabla x_t$ es estacionaria. La función de autocovarianza es:

$$
\begin{aligned}
	\gamma(h) &= E(\nabla x_t - \beta_1)(\nabla x_{t + h} - \beta_1) \\
		&= E(\nabla x_t \nabla x_{t + h} - \beta_1\nabla x_{t} - \beta_1\nabla x_{t + h} + \beta_1^2) \\
		&= E(w_t w_{t+h} - w_t w_{t+h-1} - w_{t-1} w_{t+h} + w_{t-1} w_{t+h-1}) \\
		&= 0
\end{aligned}
$$

para todo retraso $h$.  
Ahora, considerando el proceso $x_t = \beta_0 + \beta_1 t + y_t$, la diferencia $\nabla x_t = \beta_1 + \nabla y_t$ tiene por media $E(\nabla x_t) = E(\beta_1 + \nabla y_t) = \beta_1 + E(y_t) - E(y_{t-1}) = \beta_1 + \mu_y - \mu_y = \beta_1$, la cual es constante. La función de autocovarianza es:

$$
\begin{aligned}
	\gamma(h) &= E(\nabla x_t - \beta_1)(\nabla x_{t + h} - \beta_1) \\
		&= E(\nabla y_t)(\nabla y_{t + h}) \\
		&= E(y_t - y_{t-1})(y_{t + h} - y_{t+h-1}) \\
		&= E(y_t y_{t+h}) - E(y_t y_{t+h-1}) - E(y_{t-1} y_{t + h}) + E(y_{t-1} y_{t+h-1}) \\
		&= 2[\gamma_y(h) + \mu_y^2] - \gamma_y(h-1) - \mu_y^2 - \gamma_y(k) - \mu_y^2 \\
		&= 2\gamma_y(h) - \gamma_y(h-1) - \gamma_y(h + 1)
\end{aligned}
$$

**Problema 2.7** Sea $\mu_t = \mu_{t-1} + \delta  + w_t$. Muestra que $x_t - x_{t-1}$ es estacionaria, sabiendo que $x_t = \mu_t + y_t$ y $y_t$ es un proceso de media $0$ y ACF $\gamma_y(h)$ 

La media del proceso es:

$$
\begin{aligned}
	E(x_t - x_{t-1}) &= E(\mu_t + y_t - \mu_{t-1} - y_{t-1}) \\
		&= E(y_t - y_{t-1} + \delta  + w_t) \\
		&= E(y_t) - E(y_{t-1}) + \delta  + E(w_t) \\
		&= \delta
\end{aligned}
$$

La función de autocovarianza es:

$$
\begin{aligned}
	\gamma(h) &= E(x_t - x_{t-1} - \delta)(x_{t+h} - x_{t+h-1} - \delta) \\
		&= E(y_t - y_{t-1} + w_t)(y_{t+h} - y_{t+h-1} + w_{t+h}) \\
		&= E(y_t - y_{t-1})(y_{t+h} - y_{t+h-1}) \\
		&= 2\gamma_y(h) - \gamma_y(h-1) - \gamma_y(h + 1)
\end{aligned}
$$

donde la ultima igualdad se obtiene del resultado en el ejercicio anterior. Luego, la serie es débilmente estacionaria.  
La serie es también estrictamente estacionaria, dado que $Pr\left[x_t - x_{t-1} \le c\right] = Pr\left[y_t - y_{t-1} + w_t \le c - \delta\right]$, y si $y_t$ y $w_t$ son estacionarios, entonces $Pr\left[y_t - y_{t-1} + w_t \le c - \delta\right] = Pr\left[y_{t+h} - y_{t+h-1} + w_{t+h} \le c - \delta\right]$. Luego, 

$$Pr\left[x_t - x_{t-1} \le c\right] = Pr\left[y_{t+h} - y_{t+h-1} + w_{t+h} \le c - \delta\right]$$ 

y $x_t-x_{t-1}$ es estrictamente estacionaria.

<a name="problema-2-8">**Problema 2.8**</a> El registro de varvas glaciales muestra cierta no estacionaridad que se puede mejorar mediante la transformación a logaritmos y alguna no estacionaridad adicional que se puede corregir diferenciando los logaritmos.  
_a)_ Argumente que la serie de varvas glaciales, digamos $x_t$, exhibe heteroscedasticidad al calcular la varianza de la muestra sobre la primera mitad y la segunda mitad de los datos. Argumente que la transformación $y_t = \text{log }x_t$ estabiliza la varianza sobre la serie. Trace los histogramas de $x_t$ e $y_t$ para ver si la aproximación a la normalidad mejora al transformar los datos.  
_b)_ Trace la serie $y_t$. ¿Existen intervalos de tiempo, del orden de 100 años, en los que se pueda observar un comportamiento comparable al observado en los registros de temperatura global?  
_c)_ Examine la ACF de $y_t$ y comente.  
_d)_ Calcule la diferencia $u_t = y_t - y_{t-1}$, examine su gráfico de tiempo y muestre el ACF, y argumente que la diferenciación de los datos de varve registrados produce una serie razonablemente estacionaria. ¿Puedes pensar en una interpretación práctica para $u_t$?

<a name="p02-08-01-setup"></a>

```r
autoplot(varve, colour="orange") +
  ggtitle("Varvas glaciales recolectadas anualmente.") +
  xlab("Año") +
  ylab("") +
  theme_light()
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-08-01-setup-1.png" style="display: block; margin: auto;" /><table><caption>Figura 5. </caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La <a href="#p02-08-01-setup">figura 5</a> muestra la serie de varvas glaciales, donde se puede verificar que durante los primeros 250-300 años la volatilidad de la serie es bastante constante, pero esta se dispara en los últimos 300 años de la serie. 
Tampoco parece ser estacionaria dado que hay un montículo durante los primeros 200 años de los últimos 300 años. 
La varianza muestral para la primera mitad de la serie es $\sigma_{1}^2=$ 133,46, mientras que para la segunda mitad de la serie la varianza es $\sigma_{2}^2=$ 594,49, aproximadamente 4,45 veces mayor que en la primera mitad.  
Al transformar la serie usando una función logarítmica, ahora la varianza en la segunda mitad de la series es solo 1,67 veces mayor que en la primera mitad. Además, se observa en la <a href="#p02-08-02-hist">figura 6</a> que la distribución ahora es mas simétrica con respecto a la media, y el sesgo hacia la derecha ha decrecido bastante como resultado de la transformación. 

<a name="p02-08-02-hist"></a>

```r
p1 <- ggplot(as_tsibble(varve), aes(x = value)) +
  geom_histogram(position = "identity", bins = 25, fill="orange") +
  theme_light() + 
  xlab("") + ylab("Frecuencia")
p2 <- ggplot(as_tsibble(log(varve)), aes(x = value)) +
  geom_histogram(position = "identity", bins = 25, fill="orange") +
  theme_light() + 
  xlab("") + ylab("Frecuencia")

cowplot::plot_grid(p1, p2, nrow=2)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-08-02-hist-1.png" style="display: block; margin: auto;" /><table><caption>Figura 6. Histogramas de la serie de varvas antes y después de la transformación logarítmica.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La nueva serie de varvas glaciales transformada se observa en la <a href="#p02-08-03-logtransf">figura 7</a>, donde es posible observar más claramente los cambios de nivel de la serie.

<a name="p02-08-03-logtransf"></a>

```r
autoplot(log(varve), colour="orange") +
  ggtitle("Varvas glaciales recolectadas anualmente.") +
  xlab("Año") +
  ylab("") +
  theme_light()
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-08-03-logtransf-1.png" style="display: block; margin: auto;" /><table><caption>Figura 7. Serie de varvas glaciales transformada.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La serie muestra comportamientos explosivos de baja amplitud, pero con un componente periódico mas sutil cada 20-30 años, que es particularmente más visible durante la segunda mitad de la serie. Antes de eso, el ruido aleatorio parece esconder cualquier patrón cíclico que pueda existir en la serie. Y se observa una desviación importante del comportamiento general de la serie desde $t=550$ a $t=600$ de la serie. 
En la primera mitad de la serie, a los 80 años de la primera observación recolectada, se muestra una caída brusca de nivel de, al menos, una unidad en escala logarítmica, momento después del cual se mantiene un mismo nivel hasta aproximadamente 170 años después. En este momento, hay una tendencia creciente que no finaliza hasta 150 años después, donde la tendencia se revierte y hay una caída que parece persistir durante el resto de la serie.   
Esto parece indicar, que durante la primera mitad de la serie, los cambios observados parecen ser consecuencia de un solo cambio de nivel en la serie, seguido del comienzo del componente de tendencia. en la segunda mitad de la serie, los componentes con tendencia parecen ser los predominantes, con un inversión de la tendencia a aproximadamente la mitad de la (segunda mitad de la) serie.

<a name="p02-08-04-acf"></a>

```r
library(fable)
library(feasts)

cowplot::plot_grid(
	ACF(as_tsibble(log(varve)), value, lag_max=100) %>%
	  autoplot() + theme_light(),
	PACF(as_tsibble(log(varve)), value, lag_max=100) %>%
	  autoplot() + theme_light(),
	ncol=1)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-08-04-acf-1.png" style="display: block; margin: auto;" /><table><caption>Figura 8. ACF y PACF de la serie transformada de varvas glaciales.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La ACF muestra un proceso de memoria larga (<a href="#p02-08-04-acf">figura 8</a>), que en conjunto con la PACF parece indicar un proceso autoregresivo, con correlaciones importantes cada 18-28 años (que captura el ciclo sutil mencionado antes). 

Para eliminar estas características de tendencia y los cambios de nivel, se realiza la diferencia de la serie, de tal forma que se obtiene:

$$\nabla y_t = log(x_t) - log(x_{t-1}) = log(\frac{x_t}{x_{t-1}})$$

el cual es el cambio proporcional en la serie de varvas glaciales original en escala logarítmica. Esta se muestra en la <a href="#p02-08-05-diff">figura 9</a>, junto con la ACF y PACF.

<a name="p02-08-05-diff"></a>

```r
diff_varve <- diff(log(varve))

cowplot::plot_grid(
	autoplot(diff_varve, colour="orange") +
	  ggtitle("Varvas glaciales recolectadas anualmente.") +
	  xlab("Año") +
	  ylab("") +
	  theme_light(),
	cowplot::plot_grid(
		ACF(as_tsibble(diff_varve), value, lag_max=100) %>%
		  autoplot() + theme_light(),
		PACF(as_tsibble(diff_varve), value, lag_max=100) %>%
		  autoplot() + theme_light(),
		ncol=1), 
	nrow=1)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-08-05-diff-1.png" style="display: block; margin: auto;" /><table><caption>Figura 9. Gráfico temporal de la serie de varvas glaciales (en escala logarítmica) diferenciada.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Se muestra que la serie ahora es bastante mas normal, aunque aun es bastante perceptible el comportamiento explosivo, con atípicos en varios puntos de la serie. La PACF sigue denotando el comportamiento autoregresivo de la serie, que parece explicar los observado en el gráfico temporal, mientras que en la ACF ya no se observa ningún patrón reconocible. 

**Problema 2.9** En este problema, exploraremos la naturaleza periódica de $S_t$.  
_a)_ Elimine la tendencia de la serie ajustando una regresión de $S_t$ en el tiempo $t$. ¿Existe una tendencia significativa en la temperatura de la superficie del mar? Comente.  
_b)_ Calcule el peridiograma para la serie sin tendencia obtenida en el inciso _a)_. Identifique las frecuencias de los dos picos principales (con uno obvio en la frecuencia de un ciclo cada 12 meses). ¿Cuál es el ciclo probable de El Niño indicado por el pico menor?

Los resultados del ajuste de la serie de $SOI$ con respecto al tiempo $t$ se muestran en la <a href="#p02-09-01-setup">tabla 3</a>. 

<a name="p02-09-01-setup"></a>

```r
# Regresion con respecto a t
mod <- lm(soi ~ time(soi))

tidy(mod) %>%
  mutate(term=c("$\\beta_0$", "$\\beta_1$")) %>%
  knitr::kable(col.names=c("Termino", "Estimado", "Desv. Estand.", "Estadistico", "$p$"),
    escape=FALSE)
```

<table>
 <caption>Tabla 4. Resultados de la regresión de SOI con respecto a la tendencia.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Termino </th>
   <th style="text-align:right;"> Estimado </th>
   <th style="text-align:right;"> Desv. Estand. </th>
   <th style="text-align:right;"> Estadístico </th>
   <th style="text-align:right;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\beta_0$ </td>
   <td style="text-align:right;"> 13,7036654 </td>
   <td style="text-align:right;"> 3,1887278 </td>
   <td style="text-align:right;"> 4,297534 </td>
   <td style="text-align:right;"> 0,0000212 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\beta_1$ </td>
   <td style="text-align:right;"> -0,0069196 </td>
   <td style="text-align:right;"> 0,0016196 </td>
   <td style="text-align:right;"> -4,272499 </td>
   <td style="text-align:right;"> 0,0000236 </td>
  </tr>
</tbody>
</table>

El modelo ajustado es:

$$SOI = 13,7036654_{3,1887278}  -0,0069196_{0,0016196} t$$

Los valores encontrado para los coeficientes de regresión son significativos según os resultados obtenidos, indicando que hay una caída del $SOI$ con respecto al tiempo. Al eliminar el componente con tendencia y realizar el análisis espectral de los residuales (que representan a la serie $SOI$ sin el componente con tendencia), se obtiene el peridiograma mostrado en la <a href="#p02-09-02-peridiogram">figura 10</a> (que solo muestra una sección del peridiograma donde se encuentran las frecuencias relevantes).

<a name="p02-09-02-peridiogram"></a>

```r
P <- Mod(2 * fft(residuals(mod)) / sqrt(length(residuals(mod)))) ** 2; 
freqs <- 0:(length(residuals(mod)) - 1) / length(residuals(mod))

plot(freqs, P, 
	type="o", xlim=c(0, .2),
	xlab="Frecuencia", ylab="Peridiograma escalado")
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-09-02-peridiogram-1.png" style="display: block; margin: auto;" /><table><caption>Figura 10. Peridiograma de la serie SOI sin tendencia.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Se muestra el pico obvio a la frecuencia de $38/456 = 0,08333 \ldots$, el cual corresponde al periodo de 12 meses. El otro pico relevante ocurre a la frecuencia $12 / 456 = 0,02632 \ldots$, el cual corresponde a un periodo de 38 meses, o de $3\frac{1}{6}$ años, el cual correspondería al ciclo más probable para el efecto del Niño.

<a name="problema-2-10"></a>
**Problema 2.10** Considere las dos series de tiempo semanales de petróleo (`oil`) y gasolina (`gas`). La serie `oil` está en dólares por barril, mientras que la serie del `gas` está en centavos por galón.  
_a)_ Trace los datos en el mismo gráfico ¿Crees que las series son estacionarias (explica tu respuesta)?  
_b)_ En economía, a menudo es el cambio porcentual en el precio (denominado tasa de crecimiento o rendimiento), más que el cambio absoluto del precio, es lo que es importante. Argumente que se podría aplicar a los datos una transformación de la forma $y_t = \nabla\text{log }x_t$, donde $x_t$ es la serie de precios del petróleo o de la gasolina.  
_c)_ Transforme los datos como se describe en la parte _b)_, trace los datos en el mismo gráfico, mire las ACF de los datos transformados y comente.  
_d)_ Grafique el CCF de los datos transformados y comente los pequeños valores, pero significativos, cuando la serie `gas` lidera a `oil` pueden considerarse como retroalimentación.  
_e)_ Mostrar diagramas de dispersión de las series de tasas de crecimiento del petróleo y la gasolina para hasta tres semanas de anticipación de los precios del petróleo; incluya un suavizador no paramétrico en cada gráfico y comente los resultados (p. ej., ¿hay valores atípicos? ¿Las relaciones son lineales?).  
_f)_ Ha habido una serie de estudios que cuestionan si los precios de la gasolina responden más rápidamente cuando los precios del petróleo están subiendo que cuando los precios del petróleo están cayendo (_asimetría_). Intentaremos explorar esta cuestión aquí con una regresión retardada simple; ignoraremos algunos problemas obvios, como valores atípicos y errores autocorrelacionados, por lo que este no será un análisis definitivo. Sean $G_t$ y $O_t$ las tasas de crecimiento de la gasolina y del petróleo.

* Ajuste la regresión (y comente los resultados) $G_t = \alpha_1 + \alpha_2 I_t + \beta_1 O_t + \beta_2 O_{t−1} + w_t$, donde $I_t = 1$ si $O_t \ge 0$ y $0$ en caso contrario (es el indicador de no crecimiento o crecimiento positivo del precio del petróleo).  
* ¿Cuál es el modelo ajustado cuando hay un crecimiento negativo en el precio del petróleo en el momento $t$? ¿Cuál es el modelo ajustado cuando no hay un crecimiento positivo en el precio del petróleo? ¿Estos resultados apoyan la hipótesis de la asimetría?  
* Analizar los residuos del ajuste y comentar.

<a name="p02-10-01-setup"></a>

```r
df_oil_gas <- as_tsibble(cbind(oil, gas))

df_oil_gas %>% 
  ggplot(aes(x=index, y=value, colour=key)) +
    geom_line() +
    scale_colour_manual(values=c(2, 200), name=NULL, labels=c("Gas", "Petróleo")) +
    theme_light() +
    xlab("") + ylab("") +
    ggtitle("Precios de petroleo y gas durante el periodo 2000-2010") +
    theme(legend.position=c(0.2, 0.7))
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-10-01-setup-1.png" style="display: block; margin: auto;" /><table><caption>Figura 11. Gráficos temporales de las series de petroleo y gasolina.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La <a href="#p02-10-01-setup">figura 11</a> muestras ambas series `oil` y `gas`, donde es posible observar que ninguna de las dos series es estacionaria, dado que se puede percibir el componente con tendencia que parece comenzar a mediados del año 2002, y que tiene un carácter creciente, con una caída repentina durante el primer trimestre del año 2009. 
Las series presentan diferentes escalas, pero es posible observar en ambas la volatilidad de la ambas, en especial, después del 2005, y de forma más pronunciada sobre la serie `gas`. 
Esto implica que sería adecuado realizar una transformación logarítmica de los datos, y luego obtener la diferencia para deshacernos de las características no estacionarias de la serie. 

<a name="p02-10-02-acf"></a>

```r
oil_transf <- diff(log(oil))
gas_transf <- diff(log(gas))
df_log_og <- as_tsibble(cbind(oil_transf, gas_transf))

cowplot::plot_grid(
	df_log_og %>% 
	  ggplot(aes(x=index, y=value, colour=key)) +
	    geom_line() +
	    scale_colour_manual(values=c(2, 200), name=NULL, labels=c("Gas", "Petróleo")) +
	    theme_light() +
	    xlab("") + ylab("") +
	    ggtitle("Rendimeintos de petroleo y gas dutante el periodo 2000-2010") +
	    theme(legend.position=c(0.2, 0.8)),
	cowplot::plot_grid(
		tibble(lag=1:51, acf=acf(oil_transf, lag=50, plot=FALSE)$acf) %>%
		  ggplot(aes(x=lag, y=acf)) + 
		  geom_hline(aes(yintercept = 0)) +
		  geom_segment(mapping = aes(xend = lag, yend = 0)) +
		  ylab(latex2exp::TeX("$\\rho(s, t)$")) + theme_light() +
		  geom_hline(yintercept = c(1, -1) / sqrt(length(oil)), linetype=2, color='blue') +
		  ggtitle("Serie gas"),
		tibble(lag=1:51, acf=acf(gas_transf,  lag=50, plot=FALSE)$acf) %>%
		  ggplot(aes(x=lag, y=acf)) + 
		  geom_hline(aes(yintercept = 0)) +
		  geom_segment(mapping = aes(xend = lag, yend = 0)) +
		  ylab(latex2exp::TeX("$\\rho(s, t)$")) + theme_light() +
		  geom_hline(yintercept = c(1, -1) / sqrt(length(gas)), linetype=2, color='blue') +
		  ggtitle("Serie oil"),
		ncol=1),
	nrow=1)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-10-02-acf-1.png" style="display: block; margin: auto;" /><table><caption>Figura 12. Rendimientos, y ACF y PACF de la misma, para las series de gasolina y petroleo.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

En la <a href="#p02-10-02-acf">figura 12</a> se muestra la serie diferenciada en escala logarítmica, donde se puede ver mas claramente la presencia de atípicos en la serie `gas` al inicio del año 2006, e irregularidades en ambas series durante el año 2009. Además, las ACF para ambas serie muestran correlaciones significativas, que se extienden hasta retrasos alejados.

La CCF de la serie de petroleo versus la de gasolina, muestra que la serie de gasolina lidera la de petroleo, con correlaciones significativas a la izquierda, en $h=-2$, $h=-4$, $h=-8$, $h=-12$ y $h=-17$, indicando un proceso de retroalimentación de la serie de petroleo por la serie de gasolina, mientras que se observa correlaciones en $h=2$, $h=4$ y $h=25$, indicando que la serie petroleo lidera la de gasolina.

<a name="p02-10-03-ccf"></a>

```r
ccf(oil_transf, gas_transf, main="CCF de oil vs gas")
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-10-03-ccf-1.png" style="display: block; margin: auto;" /><table><caption>Figura 13. CCF de la serie de petroleo versus la de gasolina.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Las correlaciones con $h \le 4$ y para $h=12$, son positivas, indicando que ambas serie crecen o decrecen, cuando la otra lo hace. Mientras que el resto de las correlaciones  mencionadas son negativas, indicando que ambas series van en direcciones opuestas. 
Los diagramas de dispersión mostrados en la <a href="#p02-10-04-scatter">figura 14</a> muestra que existe una relación lineal entre ambas series, y que la relación decrece a solo una pequeña correlación con respecto al primer retraso, y desaparece a retrasos más lejanos.

<a name="p02-10-04-scatter"></a>

```r
lag2.plot(gas_transf, oil_transf, 3)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-10-04-scatter-1.png" style="display: block; margin: auto;" /><table><caption>Figura 14. Gráficos de dispersión que muestran la dependencia de la serie de gasolina con la de petroleo y sus retrasos hasta $h=4$.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La regresión retrasada de la tasa de cambio del precio de la gasolina con respecto a el precio del petroleo arrojan dos posibles modelos, dado que el coeficiente asociado a la variable indicadora es significativo:


```r
dummy <- ifelse(oil_transf < 0, 0, 1)
oil_lag <- ts.intersect(oil_transf, oilL1=stats::lag(oil_transf,-1), dframe=TRUE)

# Metging in one data.frame
df_reg <- cbind(rate_gas=gas_transf[-1], oil_lag, Indicator=dummy[-1])

# Regression
reg_f <- lm(rate_gas ~ oil_transf + oilL1 + Indicator, data=df_reg, na.action=NULL)

# Coeficientes
coefs <- broom::tidy(reg_f)
```

$$G_t = \begin{cases}
	0,0059_{0,009} + 0,6831_{0,0584}O_t + 0,1119_{0,0386}O_{t-1} & O_t \ge 0 \\
	-0,0064_{0,0035} + 0,6831_{0,0584}O_t + 0,1119_{0,0386}O_{t-1} & O_t < 0 
\end{cases}$$

Esto implica, que el cambio en el precio de la gasolina como consecuencia del cambio en el precio del petroleo y el precio del mismo registrado justamente anterior, tienen un valor promedio mayor cuando el cambio en el precio del petroleo es mayor o igual a cero, lo cual respalda la _asimetría_ de la serie. Dicho de otro modo, el precio de la gasolina aumenta en $t$ a una tasa proporcional mayor cuando el precio del petroleo es mayor o igual cero, dado que el cambio proporcional base es positivo y no negativo.

<a name="p02-10-04-residuals"></a>

```r
augmented_data <- broom::augment(reg_f)

ggplot(tibble(lag=1:51, acf=acf(augmented_data$.resid, lag=50, plot=FALSE)$acf),
	aes(x=lag, y=acf)) +
	geom_hline(aes(yintercept = 0)) +
	geom_segment(mapping = aes(xend = lag, yend = 0)) +
	ylab(latex2exp::TeX("$\\rho(s, t)$")) + theme_light() +
	geom_hline(yintercept = c(1, -1) / sqrt(length(oil)), linetype=2, color='blue') +
    xlab("lag") -> acf

ggplot(tibble(lag=1:50, acf=pacf(augmented_data$.resid, lag=50, plot=FALSE)$acf),
	aes(x=lag, y=acf)) +
	geom_hline(aes(yintercept = 0)) +
	geom_segment(mapping = aes(xend = lag, yend = 0)) +
	ylab(latex2exp::TeX("$\\rho(s, t)$")) + theme_light() +
	geom_hline(yintercept = c(1, -1) / sqrt(length(oil)), linetype=2, color='blue') +
    xlab("lag") -> pacf 

res_series <- augmented_data %>%
  ggplot(aes(x=1:nrow(augmented_data), y=.resid), colour="orange") +
    geom_line(aes(y=.resid), colour="orange") +
    scale_x_yearmonth(date_labels = "%Y") +
    xlab("Tiempo") +
    ylab("Residuales") +
    theme_light() 

res_qq_plot <- augmented_data %>% 
  ggplot(aes(sample = .resid)) + 
  stat_qq(colour="orange", fill="white") + stat_qq_line(color="grey") + 
  theme_light()

cowplot::plot_grid(acf, pacf, res_series, res_qq_plot, 
  nrow=1, align="h",
  labels=c("a)", "b)", "c)", "d)"), 
  label_size=11, 
  label_fontface="italic")
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-10-04-residuals-1.png" alt="Gráficos diagnósticos de residuales: _a)_ ACF, _b)_ PACF, _c)_ gráficos de residuales, y _d)_ gráfico _QQ_"  />
</div><table><caption>Figura 15. Gráficos diagnósticos de residuales: <em>a)</em> ACF, <em>b)</em> PACF, <em>c)</em> gráficos de residuales, y <em>d)</em> gráfico <em>QQ</em></caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Aun así, el análisis de los residuales muestra que aun hay una gran cantidad de atípicos en los datos, y se percibe en el gráfico de residuales un comportamiento autoregresivo, así como las autocorrelaciones no tomadas en cuenta en el modelo mostradas en el ACF y PACF. 

**Problema 2.11** Utilice dos técnicas de suavizado diferentes para estimar la tendencia en la serie de temperatura global ```globtemp```. Comente.

En la <a href="#p02-11-01-setup">figura 16</a> se muestra la serie temporal para los índices de temperatura promedio de tierra-océano globales desde 1880 a 2015, con el periodo base de 1951-1980. En particular, los datos corresponden a desviaciones, medidas en grados centígrados, del promedio de 1951-1980, y son actualizaciones de Hansen _et al._. 
Solapado sobre esta serie, se muestran las curvas suavizadas utilizando el método _loess_ (curva a trozos gris) y suavizado por _kernel_ (curva sólida gris) con parámetro $b = 2.5$.

<a name="p02-11-01-setup"></a>

```r
autoplot(globtemp, colour="dodgerblue") +
     ggtitle("Desviaciones Globales de Temperatura") +
     xlab("Tiempo") + ylab("") +
     theme_light() +
     scale_x_continuous(name="Tiempo", breaks=seq(1880, 2015, by=9), labels=seq(1880, 2015, by=9)) +
     geom_line(
         aes(y=ksmooth(time(globtemp %>% as.ts()), globtemp %>% as.ts(), "normal", bandwidth=2.5)$y), 
         colour="200", lwd=1) +
     geom_smooth(aes(y=value), method=loess, se=FALSE, color="200", linetype=2) +
     theme(axis.text.x= element_text(angle=45, vjust=.5))
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p02-11-01-setup-1.png" style="display: block; margin: auto;" /><table><caption>Figura 16. Gráfico temporal de las desviaciones de temperatura global superpuestas con los suavizados obtenidos por <em>loess</em> (linea gris a trozos) y <em>kernel</em> (linea gris sólida)</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

* Se observa que el uso del suavizado _loess_ muestra la tendencia de crecimiento de la serie, donde se muestra un paso de nivel de aproximadamente $0{,}25$ a $0{,}00$, que se alcanza en el año 1950, y 10 años después, en 1960 hay una tendencia de crecimiento lineal hasta el 2015. 
* En el suavizado por _kernel_, se sigue la misma tendencia anterior, pero se hace claro el patrón repetitivo de cada $\sim5$ años, donde se alcanzan los picos en la serie, y que parecen correlacionarse con los valores anteriores durante la segunda mitad de la serie. 


## Capitulo 3


```r
suppressPackageStartupMessages({
  library(astsa)
  library(fable)
  library(feasts)
  library(dplyr)
  library(ggplot2)
  library(kableExtra)
})
```

**Problema 3.1** Para un $MA(1)$, $x_t = w_t + \theta w_{t−1}$, demuestre que $\vert\rho_x(1)\vert \le 1/2$ para cualquier número $\theta$. ¿Para qué valores de $\theta$ $\rho_x(1)$ alcanza su máximo y mínimo?

<a name="problema-3-4"></a>
**Problema 3.4** Identifique los siguientes modelos como modelos $ARMA(p, q)$ (tenga cuidado con la redundancia de parámetros) y determine si son causales y/o invertibles:  
_a)_ $x_t = .80x_{t−1} − .15x_{t−2} + w_t − .30w_{t−1}$.  
_b)_ $x_t = x_{t−1} − .50x_{t−2} + w_t − w_{t−1}$.

El modelo del inciso _a)_ se puede escribir como $(1-\phi_1 B+\phi_2 B^2)x_t = (1 + \theta B)w_t$, por lo que sigue un proceso $ARMA(2, 1)$ dado que se tienen dos retrasos con respecto a $x_t$, con coeficientes $\phi_1 = -{,}80$ y $\phi_2 = -{,}15$; y un retraso con respecto a $w_t$, con coeficiente $\theta = -{,}30$. 
Sin embargo, el polinomio $\phi(B) = 1-\phi_1 B+\phi_2 B^2$ asociado a $x_t$, se puede factorizar como $\frac{20}{3}(1 - \frac{3}{10}B)(1 - 2B)$, por lo que al simplificar, el modelo sigue la forma $(1 - 2B)\frac{20}{3}x_t = w_t$, y por tanto el modelo simplificado corresponde a un proceso $ARMA(1, 0)$ sobre la serie $y_t = \frac{20}{3}x_t$. 
El modelo entonces no es causal, dado que al resolver $\phi(z) = 1 - 2 z = 0$ cuando $z = 1/2$, la cual cae dentro del circulo unitario. 

El modelo del inciso _b)_ se escribe como $(1 - B + {,}50 B^2)x_t = (1 - B)w_t$, por lo que el modelo es $ARMA(2, 1)$. 
Las raíces de $\phi(z) = 1 - z + {,}5 z^2 = 0$ son $z = 1 \pm i$, por lo que $\vert z \vert = \sqrt{2}$, el cual cae fuera del circulo unitario. Por lo tanto, el modelo es causal. 
Por otro lado, el modelo no es invertible dado que $\theta(z) = 1-z = 0 \rightarrow z = 1$, cae dentro del circulo unitario.

<a name="problema-3-33"></a>
**Problema 3.33** Ajuste un modelo $ARIMA(p, d, q)$ a los datos de temperatura global `globtemp` realizando todos los diagnósticos necesarios. Después de decidirse por un modelo apropiado, pronostique (con límites) los próximos $10$ años. Comente.

Ya en el [ejemplo 2.6](https://github.com/Ryuta2329/Msc-Math-Applied/blob/main/Series%20Temporales/colab-nb/Global-Temperature-Example.ipynb) que la serie de temperatura global parece comportarse más como un paseo aleatorio que como una serie estacionaria de tendencia (ver el <a href="/Series%20Temporales/output/Shunway-Stoffer-Solutions.md#problema-5-3">problema 5.3</a>), y por lo tanto, en lugar de eliminar la tendencia de los datos, es más apropiado utilizar la diferenciación para forzarlos a la estacionaridad. 

Al realizar esto, se encontró una autocorrelación mínima, lo que puede implicar que la serie de temperatura global es casi un paseo aleatorio con deriva. La ACF y PACF de la serie diferenciada se muestra en la <a href="#p03-33-01-example-2-6">figura 17</a>. 
LA PACF muestra correlaciones significativas hasta el _lag_ 3, otra autocorrelación importante en $h=36$, y una autocorrelación pequeña (pero significativa) en $h=5$. La ACF muestra correlaciones importantes en $h=4$, $9$ y $27$.

<a name="p03-33-01-example-2-6"></a>

```r
df_globtemp <- as_tsibble(gtemp)

cowplot::plot_grid(
    ACF(df_globtemp, diff(value), lag_max = 48) %>%
      autoplot() + theme_light(),
    PACF(df_globtemp, diff(value), lag_max = 48) %>%
      autoplot() + theme_light(),
  nrow = 1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p03-33-01-example-2-6-1.png" alt="ACF y PACF de la serie diferenciada de Temperatura Global."  />
</div><table><caption>Figura 17. ACF y PACF de la serie diferenciada de Temperatura Global.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Dada la información de las ACF y PACF, se considera un proceso ARMA con componente autoregresivo de orden 1 y componente de media móvil de orden 3, como modelo inicial de trabajo. Escrito en forma resumida como:

$$(1 - \phi B)(1 - B)x_t = (1 + \theta_1 B + \theta_2 B^2 + \theta_3 B^3)w_t$$

que se expande como:

$$x_t = (1 + \phi) x_{t-1} - \phi x_{t-2} + w_t + \theta_1 w_{t-1} + \theta_2 w_{t-2} + \theta_3 w_{t-3}$$

Además, se considera un modelo similar pero con una constante $c = \mu(1 - \phi)$, para verificar si el componente con deriva es significativo o no. 


```r
first_model <- df_globtemp %>%
  model(
    first_arima_c = ARIMA(value ~ pdq(1, 1, 3), stepwise = FALSE),
    first_arima = ARIMA(value ~ 0 + pdq(1, 1, 3), stepwise = FALSE)
  )
```

Los resultados del ajuste se muestran en la <a href="#p03-33-02-models-criteria">tabla 4</a>, donde se observa que el modelo con la deriva es preferible al modelo sin termino constante, de acuerdo a los criterios de información. 

<a name="p03-33-02-models-criteria"></a>

```r
glance(first_model) %>%
  select(sigma2:BIC) %>%
  mutate(Model=c("ARIMA(1,1,3) con deriva", "ARIMA(1,1,3)"), .before=1) %>%
  kbl(digits = c(NA, 4, 2, 2, 2, 2), escape=FALSE,
    col.names=c("Modelo", "$\\sigma^2$", "Func. Verosim.", "AIC", "AICc", "BIC"),
    caption="Criterios de información para los modelos ARIMA(1,1,3) con y sin deriva.")
```

<table>
<caption>Tabla 5. Criterios de información para los modelos ARIMA(1,1,3) con y sin deriva.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Modelo </th>
   <th style="text-align:right;"> $\sigma^2$ </th>
   <th style="text-align:right;"> Func. Verosim. </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> AICc </th>
   <th style="text-align:right;"> BIC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ARIMA(1,1,3) con deriva </td>
   <td style="text-align:right;"> 0,0091 </td>
   <td style="text-align:right;"> 122,17 </td>
   <td style="text-align:right;"> -232,34 </td>
   <td style="text-align:right;"> -231,66 </td>
   <td style="text-align:right;"> -215,19 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARIMA(1,1,3) </td>
   <td style="text-align:right;"> 0,0094 </td>
   <td style="text-align:right;"> 119,56 </td>
   <td style="text-align:right;"> -229,11 </td>
   <td style="text-align:right;"> -228,63 </td>
   <td style="text-align:right;"> -214,82 </td>
  </tr>
</tbody>
</table>

Los coeficientes estimados para el modelo con deriva se muestran adelante, observándose que todos son significativos. 
La varianza residual del modelo ajustado es $\sigma^2 =$ 0,00911. 

<a name="p03-33-02-first-model"></a>

```r
chosen_mod <- first_model %>%
  select(first_arima_c)

tidy(chosen_mod) %>%
  select(-.model) %>%
  mutate(term = c("$\\phi$", "$\\theta_1$", "$\\theta_2$", "$\\theta_3$", "$c$")) %>%
  kbl(digits=c(NA, 3, 3, 3, 4), escape=FALSE, booktabs=TRUE,
  	caption="Resultados del auste ARIMA.",
    col.names=c("Coeficiente", "Estimado", "Desv. Est.", "Estadistico", "p"))
```

<table>
<caption>Tabla 6. Resultados del ajuste ARIMA.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Coeficiente </th>
   <th style="text-align:right;"> Estimado </th>
   <th style="text-align:right;"> Desv. Est. </th>
   <th style="text-align:right;"> Estadistico </th>
   <th style="text-align:right;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\phi$ </td>
   <td style="text-align:right;"> -0,938 </td>
   <td style="text-align:right;"> 0,095 </td>
   <td style="text-align:right;"> -9,909 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\theta_1$ </td>
   <td style="text-align:right;"> 0,485 </td>
   <td style="text-align:right;"> 0,116 </td>
   <td style="text-align:right;"> 4,161 </td>
   <td style="text-align:right;"> 0,0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\theta_2$ </td>
   <td style="text-align:right;"> -0,634 </td>
   <td style="text-align:right;"> 0,096 </td>
   <td style="text-align:right;"> -6,637 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\theta_3$ </td>
   <td style="text-align:right;"> -0,286 </td>
   <td style="text-align:right;"> 0,087 </td>
   <td style="text-align:right;"> -3,299 </td>
   <td style="text-align:right;"> 0,0013 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $c$ </td>
   <td style="text-align:right;"> 0,013 </td>
   <td style="text-align:right;"> 0,005 </td>
   <td style="text-align:right;"> 2,617 </td>
   <td style="text-align:right;"> 0,0099 </td>
  </tr>
</tbody>
</table>


Los gráficos diagnósticos se muestran a continuación, donde se puede observar que la ACF y PACF se asemejan a los esperados para ruido blanco, a excepción de las correlaciones significativas. 
En la ACF se observa una correlación significativa en $h=27$, y en la PACF se observa una correlación significativa en $h=36$. 
El gráfico _QQ_, así como el gráfico temporal de residuales, muestran que la distribución es bastante normal, pero que existen 2 datos atípicos que se desvían de la recta teórica normal, y corresponden a las observaciones de 1991 y 1999.

<a name="p03-33-03-diagnostics"></a>

```r
augmented_df <- first_model %>%
  select(first_arima_c) %>%
  augment()

augmented_df %>%
  ACF(.innov, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$")) -> acf

augmented_df %>%
  PACF(.innov, lag_max=50) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$")) -> pacf 

res_series <- augmented_df %>%
     autoplot(.resid, colour="orange") +
     geom_point(aes(y=.resid), colour="orange") +
     xlab("Tiempo") +
     ylab("Residuales") +
     theme_light() +
     geom_hline(yintercept=c(0), 
     	color=c("black"), linetype=c(1))

res_qq_plot <- augmented_df %>% 
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
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p03-33-03-diagnostics-1.png" alt="Gráficos diagnósticos de residuales: _a)_ ACF, _b)_ PACF, _c)_ gráficos de residuales, y _d)_ gráfico _QQ_"  />
</div><table><caption>Figura 18. Gráficos diagnósticos de residuales: <em>a)</em> ACF, <em>b)</em> PACF, <em>c)</em> gráficos de residuales, y <em>d)</em> gráfico <em>QQ</em></caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

El modelo ajustado se escribe:

$$x_t = 0,0125_{(0,0048)} + 0,06_{(0,095)} x_{t-1}  -0,94_{(0,095)} x_{t-2} + w_t + 0,48_{(0,116)} w_{t-1}  -0,63_{(0,096)} w_{t-2}  -0,29_{(0,087)} w_{t-3}$$

Lo pronósticos para los próximos 10 años se muestran a continuación:

<a name="p03-33-07-forecast"></a>

```r
mod_forecast <- chosen_mod %>%
  forecast(h=10)

autoplot(df_globtemp, colour="dodgerblue3") +
  autolayer(mod_forecast, colour="orange") +
    theme_light() + 
    xlab('Tiempo') + ylab('Dif. de Temperatura') +
    theme(legend.position="none")
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p03-33-07-forecast-1.png" alt="Pronósticos para los próximos 10 años"  />
</div><table><caption>Figura 19. Pronósticos para los próximos 10 años</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

donde se observa una pequeña tendencia lineal creciente y fluctuante en los próximos 10 años.

## Capitulo 4.

## Capitulo 5.


```r
suppressPackageStartupMessages({
	library(astsa)
	library(tsibble)
	library(fable)
	library(feasts)
	library(dplyr)
	library(tidyr)
	library(purrr)
	library(magrittr)
	library(ggplot2)
	library(rugarch)
	library(kableExtra)
})
```

<a name="problema-5-1"></a>
**Problema 5.1** El conjunto de datos `arf` son 1000 observaciones simuladas de un modelo $ARFIMA(1, 1, 0)$ con $\phi = .75$ y $d = .4$.  
_a)_ Grafique los datos y comente.  
_b)_ Trazar el ACF y PACF de los datos y comentar.  
_c)_ Estime los parámetros y pruebe la significación de las estimaciones $\hat{\phi}$ y $\hat{d}$.  
_d)_ Explique por qué, usando los resultados de las partes _a)_ y _b)_, parecería razonable diferenciar los datos antes del análisis. Es decir, si $x_t$ representa los datos, explique por qué podríamos elegir ajustar un modelo $ARMA$ a $\nabla x_t$.  
_e)_ Trace el ACF y el PACF de $\nabla x_t$ y comente.  
_f)_ Ajuste un modelo $ARMA$ a $\nabla x_t$ y comente.  

La serie mostrada en la <a href="#p05-01-01-setup">figura 20</a> muestra los valores simulados del modelo $ARFIMA(1,1,0)$ con $\phi = .75$ y $d = .4$. 
Se observa en la serie un patrón cíclico cuyo periodo parece variar de 50 a 150 años a lo largo de la serie, alargándose el ciclo y luego haciéndose mas corto. 

<a name="p05-01-01-tsplot"></a>

```r
arf %>%
  as_tsibble() %>%
  autoplot() +
  theme_light() +
  scale_x_continuous(name = "Tiempo",
    breaks = seq(0, 1000, by = 100),
    labels = seq(0, 1000, by = 100))
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-01-01-tsplot-1.png" alt="Serie arf."  />
</div><table><caption>Figura 20. Serie arf.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

La ACF muestra que el proceso es de memoria larga, con autocorrelaciones importantes (con magnitudes moderadas a moderadamente pequeñas) que se extienden a valores de $h>100$, y que decrecen y crecen nuevamente una y otra vez. 
El PACF muestra que el proceso puede ser autoregresivo de segundo orden, con correlaciones significativas en $h=14$ y $h=69$.

<a name="p05-01-02-acf"></a>

```r
cowplot::plot_grid(
	ACF(as_tsibble(arf), value, lag_max=100) %>%
	  autoplot() + theme_light(),
	PACF(as_tsibble(arf), value, lag_max=100) %>%
	  autoplot() + theme_light(),
	nrow=1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-01-02-acf-1.png" alt="ACF y PACF de la serie."  />
</div><table><caption>Figura 21. ACF y PACF de la serie.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

El modelo ARFIMA estimado por selección automática basada en criterios de información tiene por parámetros los mostrados en la <a href="#p05-01-03-autoarfima">tabla 6</a>. Se puede observar que los componentes de promedio móvil no son importantes. 
El valor para $\phi_2$ no es significativo, pero $\phi_1$ y $d$ si lo son, indicando que se trata de un modelo ARFIMA de primer orden en el componente autoregresivo, con diferencia fraccional. 
Los valores estimados para estos parámetros son muy similares a los usados para generar la serie, a partir de una distribución normal estándar.

<a name="p05-01-03-autoarfima"></a>

```r
arfima_estimation <- autoarfima(arf,
	method="full", arfima=NULL)

arfima_estimation$fit@fit$matcoef %>%
  as_tibble() %>%
  mutate(coef=c("$\\hat{\\phi_1}$", "$\\hat{\\phi_2}$", "$\\hat{\\theta_1}$", "$\\hat{\\theta_2}$", "$\\hat{d}$", "$\\hat{\\sigma}$"), .before = 1) %>%
  kbl(digits = c(NA, 3, 4, 2, 4),
  	escape = FALSE,
  	col.names=c("Coef.", "Estimado", "Desv. Est.", "$t$", "$p$"),
  	caption = "Coeficientes estimados por selección automática de modelo."
  )
```

<table>
<caption>Tabla 7. Coeficientes estimados por selección automática de modelo.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Coef. </th>
   <th style="text-align:right;"> Estimado </th>
   <th style="text-align:right;"> Desv. Est. </th>
   <th style="text-align:right;"> $t$ </th>
   <th style="text-align:right;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\hat{\phi_1}$ </td>
   <td style="text-align:right;"> 0,725 </td>
   <td style="text-align:right;"> 0,0967 </td>
   <td style="text-align:right;"> 7,49 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\hat{\phi_2}$ </td>
   <td style="text-align:right;"> 0,077 </td>
   <td style="text-align:right;"> 0,0516 </td>
   <td style="text-align:right;"> 1,49 </td>
   <td style="text-align:right;"> 0,1364 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\hat{\theta_1}$ </td>
   <td style="text-align:right;"> 0,000 </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\hat{\theta_2}$ </td>
   <td style="text-align:right;"> -0,086 </td>
   <td style="text-align:right;"> 0,0444 </td>
   <td style="text-align:right;"> -1,95 </td>
   <td style="text-align:right;"> 0,0516 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\hat{d}$ </td>
   <td style="text-align:right;"> 0,425 </td>
   <td style="text-align:right;"> 0,0916 </td>
   <td style="text-align:right;"> 4,64 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\hat{\sigma}$ </td>
   <td style="text-align:right;"> 0,986 </td>
   <td style="text-align:right;"> 0,0220 </td>
   <td style="text-align:right;"> 44,72 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
</tbody>
</table>

Para el inciso _d)_, se observa en el gráfico temporal (<a href="#p05-01-01-tsplot">figura 21</a>) que hay cierto componente con tendencia, que se puede eliminar por medio de una diferencia. Esto, además, diminuiría la autocorrelación serial observada en la ACF mostrada en la <a href="#p05-01-02-acf">figura 22</a>.

El ACF y PACF de la primera diferencia de la serie `arf` se muestra abajo:

<a name="p05-01-04-acf"></a>

```r
cowplot::plot_grid(
	ACF(as_tsibble(diff(arf)), value, lag_max=100) %>%
	  autoplot() + theme_light(),
	PACF(as_tsibble(diff(arf)), value, lag_max=100) %>%
	  autoplot() + theme_light(),
	nrow=1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-01-04-acf-1.png" alt="ACF y PACF de la serie diferenciada."  />
</div><table><caption>Figura 22. ACF y PACF de la serie diferenciada.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Se observa de la ACF que el componente autoregresivo de primer orden resalta, con varias correlaciones pequeñas significativas. 

Se ajustan varios modelos ARMA en el siguiente fragmento de código, y se muestran los criterios de información ordenados en forma descendente de $AIC$. 
El modelo ARMA preferible según los criterios de información es el autoregresivo de primer orden, sin componente de promedio móvil.

<a name="p05-01-04-model"></a>

```r
mod <- as_tsibble(diff(arf)) %>%
  model(
  	mod_1 = ARIMA(value ~ 0 + pdq(1, 0, 0), stepwise = FALSE),
  	mod_2 = ARIMA(value ~ 0 + pdq(1, 0, 1), stepwise = FALSE),
  	mod_3 = ARIMA(value ~ 0 + pdq(1, 0, 2), stepwise = FALSE),
  	mod_4 = ARIMA(value ~ 0 + pdq(2, 0, 0), stepwise = FALSE),
  	mod_5 = ARIMA(value ~ 0 + pdq(2, 0, 1), stepwise = FALSE),
  	mod_6 = ARIMA(value ~ 0 + pdq(2, 0, 2), stepwise = FALSE)
  )

glance(mod) %>%
  select(.model:BIC) %>%
  mutate(.model = c("ARMA(1,0)", "ARMA(1,1)", "ARMA(1,2)", "ARMA(2,0)", "ARMA(2,1)", "ARMA(2,2)")) %>%
  arrange(AIC) %>%
  kbl(digits = c(NA, 2, 0, 2, 2, 2),
  	escape = FALSE,
  	col.names=c("Modelo", "$\\sigma^2$", "Func. Verosim.", "$AIC$", "$AICc$", "$BIC$"),
  	caption = "Criterios de información para los mdoelos ARMA ajustados.")
```

<table>
<caption>Tabla 8. Criterios de información para los modelos ARMA ajustados.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Modelo </th>
   <th style="text-align:right;"> $\sigma^2$ </th>
   <th style="text-align:right;"> Func. Verosim. </th>
   <th style="text-align:right;"> $AIC$ </th>
   <th style="text-align:right;"> $AICc$ </th>
   <th style="text-align:right;"> $BIC$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ARMA(1,0) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -1417 </td>
   <td style="text-align:right;"> 2838,49 </td>
   <td style="text-align:right;"> 2838,50 </td>
   <td style="text-align:right;"> 2848,30 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(1,1) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -1417 </td>
   <td style="text-align:right;"> 2839,90 </td>
   <td style="text-align:right;"> 2839,92 </td>
   <td style="text-align:right;"> 2854,62 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(2,0) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -1417 </td>
   <td style="text-align:right;"> 2840,08 </td>
   <td style="text-align:right;"> 2840,11 </td>
   <td style="text-align:right;"> 2854,80 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(2,1) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -1417 </td>
   <td style="text-align:right;"> 2841,46 </td>
   <td style="text-align:right;"> 2841,50 </td>
   <td style="text-align:right;"> 2861,09 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(1,2) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -1417 </td>
   <td style="text-align:right;"> 2841,78 </td>
   <td style="text-align:right;"> 2841,82 </td>
   <td style="text-align:right;"> 2861,41 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(2,2) </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> -1417 </td>
   <td style="text-align:right;"> 2843,29 </td>
   <td style="text-align:right;"> 2843,35 </td>
   <td style="text-align:right;"> 2867,82 </td>
  </tr>
</tbody>
</table>

El valor estimado para el coeficiente autoregresivo es $\phi =$ 0,17 $\pm$ 0,0312, el cual es muy significativo. 
Los gráficos diagnósticos muestran que los residuales se comportan normalmente, pero se observan aun autocorrelaciones pequeñas significativas en el ACF y PACF.

<a name="p05-01-05-diagnostics"></a>

```r
augmented_df <- augment(select(mod, mod_1))

augmented_df %>%
  ACF(.innov, lag_max=100) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$")) -> acf

augmented_df %>%
  PACF(.innov, lag_max=100) %>%
  autoplot() +
    theme_light() +
    xlab("lag") + 
    ylab(latex2exp::TeX("$\\rho(s, t)$")) -> pacf 

res_series <- augmented_df %>%
     autoplot(.resid, colour="orange") +
     geom_point(aes(y=.resid), colour="orange") +
     xlab("Tiempo") +
     ylab("Residuales") +
     theme_light() +
     geom_hline(yintercept=c(0), 
     	color=c("black"), linetype=c(1))

res_qq_plot <- augmented_df %>% 
  ggplot(aes(sample = .resid)) + 
  stat_qq() + stat_qq_line(color="red") + 
  theme_light()

cowplot::plot_grid(acf, pacf, res_series, res_qq_plot, 
  nrow=1, align="h",
  labels=c("a)", "b)", "c)", "d)"), 
  label_size=11, 
  label_fontface="italic")
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-01-05-diagnostics-1.png" style="display: block; margin: auto;" />

**Problema 5.3** Grafique la serie de temperatura global, `globtemp`, y luego pruebe si hay una raíz unitaria versus la alternativa de que el proceso es estacionario usando las tres pruebas: DF, ADF y PP, discutidas en el Ejemplo 5.3. Comente.

Los detalles de la prueba DF y ADF en el paquete `tseries` indican que la regresión general que se hace sobre la serie incluye un termino constante y uno con tendencia lineal, además de los $k$ retrasos usados para considerar el proceso autoregresivo. 
Esto implica, que la hipótesis nula prueba si el proceso es uno con tendencia lineal y deriva (prueba Df), y la alternativa estipula que se trata de un paseo aleatorio con componentes de tendencia lineal y deriva.

En el <a href="/Series%20Temporales/output/Shunway-Stoffer-Solutions.md#problema-3-33">Problema 3.33</a> vimos que el modelo autoregresivo mas adecuado para la serie involucra uno de orden $p=2$, por lo que se usa $k=2$ para la prueba ADF. 


```r
list(
	  df=tseries::adf.test(globtemp, k=0),
	  adf=tseries::adf.test(globtemp, k=2),
	  pp=tseries::pp.test(globtemp)) |>
  map(broom::tidy) |>
  list_rbind() |>
  select(method, statistic:parameter) |>
  kbl(
    col.names=c("Prueba", "Estadístico", "$p$", "df"),
    escape=FALSE, 
    caption="Estadísticos de las pruebas de Raíz unitaria para estacionaridad."
  )
```

<table>
<caption>Tabla 9. Estadísticos de las pruebas de Raíz unitaria para estacionaridad.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Prueba </th>
   <th style="text-align:right;"> Estadístico </th>
   <th style="text-align:right;"> $p$ </th>
   <th style="text-align:right;"> df </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Augmented Dickey-Fuller Test </td>
   <td style="text-align:right;"> -4,044159 </td>
   <td style="text-align:right;"> 0,0100000 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Augmented Dickey-Fuller Test </td>
   <td style="text-align:right;"> -2,757126 </td>
   <td style="text-align:right;"> 0,2615008 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Phillips-Perron Unit Root Test </td>
   <td style="text-align:right;"> -25,061713 </td>
   <td style="text-align:right;"> 0,0200280 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

Los resultados de aplicar las pruebas de raíz unitaria de DF y PP muestran que la serie de temperatura global es estacionaria ($p<0{,}05$), dado que se rechaza la hipótesis nula de raíz unitaria. Esto es de esperar, dado que ambas pruebas analizan el mismo esquema de dependencia. 
Por otro lado, la ADF resulta en aceptación de la hipótesis nula, indicando que la serie sigue un paseo aleatorio con deriva.

La discrepancia entre los resultados de la prueba DF y la prueba ADF resulta de que, al considerar los retrasos hasta $h=2$, se captura la dependencia de la serie con respecto a los retrasos en $t-1$ y $t-2$, haciendo innecesario el termino extra para $x_{t-1}$. 
Al usar $k=1$ en la prueba ADF, el resultado sigue siendo mantener la hipótesis nula, (aunque el resultado es marginal), lo cual podría indicar que la serie diferenciada se trata de un proceso $AR(1)$ con una tendencia lineal y deriva ($DF_\gamma =$ -3,422, $p=$ 0,0539). 

Es interesante que al realizar las pruebas sobre la serie `globtemp` luego de realizar una primera diferencia, todas las pruebas son significativas, indicando que la serie diferenciada si es estacioanria. 
Además, la  prueba ADF se hace usando $k=1$, lo cual toma en cuenta la autocorrelación con respecto al retraso $h=2$ en la serie sin diferenciar. 


```r
list(
	  df=tseries::adf.test(diff(globtemp), k=0),
	  adf=tseries::adf.test(diff(globtemp), k=1),
	  pp=tseries::pp.test(diff(globtemp))) |>
  map(broom::tidy) |>
  list_rbind() |>
  select(method, statistic:parameter) |>
  kbl(
    col.names=c("Prueba", "Estadístico", "$p$", "df"),
    escape=FALSE, 
    caption="Estadísticos de las pruebas de Raíz unitaria para estacionaridad."
  )
```

<table>
<caption>Tabla 10. Estadísticos de las pruebas de Raíz unitaria para estacionaridad.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Prueba </th>
   <th style="text-align:right;"> Estadístico </th>
   <th style="text-align:right;"> $p$ </th>
   <th style="text-align:right;"> df </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Augmented Dickey-Fuller Test </td>
   <td style="text-align:right;"> -14,67630 </td>
   <td style="text-align:right;"> 0,01 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Augmented Dickey-Fuller Test </td>
   <td style="text-align:right;"> -11,90951 </td>
   <td style="text-align:right;"> 0,01 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Phillips-Perron Unit Root Test </td>
   <td style="text-align:right;"> -135,88521 </td>
   <td style="text-align:right;"> 0,01 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

<a name="problema-5-7"></a>
**Problema 5.7** El paquete `stats` de R contiene los precios de cierre diarios de los cuatro principales índices bursátiles europeos; escriba `help(EuStockMarkets)` para obtener más detalles. Ajuste un modelo $GARCH$ a los rendimientos de una de estas series y discuta sus hallazgos. (Nota: el conjunto de datos contiene valores reales y no retornos. Por lo tanto, los datos deben transformarse antes del ajuste del modelo).


```r
EU_SMI <- EuStockMarkets[, "SMI"] %>% 
  as_tsibble() %>%
  mutate(dates = lubridate::date(index), .before = 2) %>%
  index_by(dates) %>%
  summarise(
    smi_difflog = value
  ) %>%
  mutate(smi_difflog = difference(log(smi_difflog))) %>%
  slice(-1) %>%
  complete(dates = seq(min(dates), max(dates), by = "day")) %>%
  mutate(smi_difflog=zoo::na.approx(smi_difflog)) %>%
  as_tsibble()
```

Para el análisis se seleccionó el indice bursátil correspondiente a Suiza (SMI). Al revisar la serie se nota de inmediato que la serie contiene `NA`, introducidos de forma alternativa cada 2 a 3 días. Estos datos faltantes se imputaron usando interpolación lineal.

<a name="p05-07-02-data"></a>

```r
cowplot::plot_grid(
	EuStockMarkets[, "SMI"] %>% 
    as_tsibble() %>%
	  autoplot(, colour="dodgerblue3") +
	    theme_light() +
	    xlab("") + ylab("Precios de cierre diario") +
	    ggtitle("Indice bursátil Europeo (Suiza SMI)"),
	EU_SMI %>%
	  autoplot(smi_difflog, colour="dodgerblue3") +
	    theme_light() +
	    xlab("") + ylab("Retornos") +
	    ggtitle("Retornos logarítmicos del SMI."),
	nrow=1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-07-02-data-1.png" alt="Indice bursátil de Suiza."  />
</div><table><caption>Figura 23. Indice bursátil de Suiza.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

El indice bursátil correspondiente a Suiza (SMI), el cual se muestra en la <a href="#p05-07-02-data">figura 24</a>, junto a los retornos calculados a partir de la misma. 
La serie tiene una tendencia creciente clara que se elimina al calcular los retornos, con caídas importantes durante todo el año 1994, y a la mitad del año 1997. Es en estos momentos es que se observan los _clusters_ de volatilidad más importantes de la serie de retornos. 

La ACF y PACF muestran que los retornos parecen ser generados por un proceso autoregresivo de primer o segundo orden en el componente ARMA, y se observan múltiples correlaciones pequeñas, pero significativas, que pueden resultar de la volatilidad cambiante.  
La volatilidad se observa a la derecha en la misma figura, la cual se calcula usando una ventana de un mes, y presenta un comportamiento bastante fluctuante, con crestas particularmente grandes durante los años 1994 y 1997.

<a name="p05-07-03-acf-pacf"></a>

```r
monthly_aggregates <- EU_SMI %>%
  index_by(year_month = ~ yearmonth(.)) %>%
  summarise(
  	month_var = var(smi_difflog, na.rm = TRUE),
    month_mean_return = mean(smi_difflog ** 2, na.rm = TRUE))

cowplot::plot_grid(
	cowplot::plot_grid(
		EU_SMI %>%
		  ACF(smi_difflog, lag_max=100) %>%
		  autoplot() +
		    xlab("h") +
		    ylab("ACF") +
		    theme_light(),
		EU_SMI %>%
		  PACF(smi_difflog, lag_max=100) %>%
		  autoplot() +
		    xlab("h") +
		    ylab("PACF") +
		    theme_light(),
		ncol=1), 
	monthly_aggregates %>%
	  autoplot(month_var, colour = "dodgerblue4") +
      theme_light() +
      ylab("Volatilidad") +
      xlab(""),
  nrow=1)
```

<div class="figure" style="text-align: center">
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-07-03-acf-pacf-1.png" alt="Funciones de autocorrelación y autocorrelación parcial para los retornos (a la izquierda) y volatilidad mensual de los retornos."  />
</div><table><caption>Figura 24. Funciones de autocorrelación y autocorrelación parcial para los retornos (a la izquierda) y volatilidad mensual de los retornos.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

El siguiente fragmento de código muestra los resultados de la prueba Arch por multiplicadores de Lagrange, al descomponer la varianza de la serie e identificar si sus rezagos son significativos:


```r
arch_test <- EU_SMI %$%
  FinTS::ArchTest(smi_difflog, lags = 4, demean = FALSE)
```

de lo que se obtiene que la serie tiene efectos Arch significativos ($\chi^2=$ 280,47, $p=$ 0 ), y que existe una correlación importante entre la varianza y los retornos cuadrados. 

Se procede a ajustar modelos GARCH para la volatilidad, con componente $ARCH(1)$ y $ARCH(2)$ solamente, y con volatilidad independiente de sus retrasos, y regresada sobre retrasos $h=1$ y $2$. Además, se prueban estos modelos usando la posibilidad de un modelo ARMA para la media con componente autoregresivo de primer o segundo orden. 


```r
ARMA_mod <- rep(c("ARMA(1, 0)", "ARMA(2, 0)"), each=4)
GARCH_mod <- rep(c("GARCH(1,0)", "GARCH(1,1)", "GARCH(2,1)", "GARCH(2,2)"), 2)
arma_pars <- rep(list(fisrt_order=c(1,0), second_order=c(2,0)), each=4)
garch_mod_pars <- rep(list(c(1, 0), c(1, 1), c(2, 1), c(2, 2)), 2)

garch_fitting <- tibble(
	ARMA_mod, GARCH_mod,
	arma_mod=arma_pars,
  garch_mod=garch_mod_pars) %>%
  mutate(
  	garch_mods_spec=map2(
	    arma_mod, garch_mod,
      ~ugarchspec(
   	    mean.model = list(
   		    armaOrder=.x, 
   		    include.mean = FALSE),
        variance.model=list(model = "sGARCH", garchOrder=.y),
        distribution.model = "norm")
    ),
    garch_mods_fit=map(garch_mods_spec,
      ~ugarchfit(., EU_SMI)),
    convergence=map_dbl(garch_mods_fit, convergence)
  ) %>%
  filter(convergence == 0)
```

En total, se ajustan 8 modelos, de los cuales solo el modelo $GARCH(1,0)$ con componente $ARMA(1,0)$ para la media, fallo en converger. 
En la tabla <a href="#p05-07-06-criteria-inf">tabla 9</a> se muestran los resultados de los criterios de información para el resto de los modelos ajustados, donde se observa que los mejores modelos, de acuerdo al AIC, son los $GARCH(2,2)$, tanto para el componente $ARMA(1,0)$ como para $ARMA(2,0)$; seguido de los modelos con componente $ARMA(2,0)$ y componente $GARCH(1, 1)$ y $GARCH(2, 1)$. 
La misma tendencia se sigue del resto de los criterios de información mostrados en la tabla. 

<a name="p05-07-06-criteria-inf"></a>

```r
garch_fitting %>%
  mutate(glanced=map(garch_mods_fit, infocriteria), .after=2) %>% 
  unnest(glanced) %>% 
  mutate(criteria=rep(c("AIC", "BIC", "Shibata", "Hannan-Quinn"), 7), .after=2) %>%
  select(1:4) %>%
  spread(key="criteria", value=4) %>%
  arrange(AIC) %>%
  slice(1:5) %>%
  kbl(digits = 3,
    caption="Criterios de información para 5 de los modelos GARCH ajustados.")
```

<table>
<caption>Tabla 11. Criterios de información para 5 de los modelos GARCH ajustados.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ARMA_mod </th>
   <th style="text-align:left;"> GARCH_mod </th>
   <th style="text-align:right;"> AIC </th>
   <th style="text-align:right;"> BIC </th>
   <th style="text-align:right;"> Hannan-Quinn </th>
   <th style="text-align:right;"> Shibata </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ARMA(2, 0) </td>
   <td style="text-align:left;"> GARCH(2,2) </td>
   <td style="text-align:right;"> -7,026 </td>
   <td style="text-align:right;"> -7,010 </td>
   <td style="text-align:right;"> -7,020 </td>
   <td style="text-align:right;"> -7,026 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(1, 0) </td>
   <td style="text-align:left;"> GARCH(2,2) </td>
   <td style="text-align:right;"> -7,011 </td>
   <td style="text-align:right;"> -6,998 </td>
   <td style="text-align:right;"> -7,006 </td>
   <td style="text-align:right;"> -7,011 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(2, 0) </td>
   <td style="text-align:left;"> GARCH(1,1) </td>
   <td style="text-align:right;"> -6,999 </td>
   <td style="text-align:right;"> -6,988 </td>
   <td style="text-align:right;"> -6,995 </td>
   <td style="text-align:right;"> -6,999 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(2, 0) </td>
   <td style="text-align:left;"> GARCH(2,1) </td>
   <td style="text-align:right;"> -6,998 </td>
   <td style="text-align:right;"> -6,985 </td>
   <td style="text-align:right;"> -6,993 </td>
   <td style="text-align:right;"> -6,998 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARMA(1, 0) </td>
   <td style="text-align:left;"> GARCH(1,1) </td>
   <td style="text-align:right;"> -6,988 </td>
   <td style="text-align:right;"> -6,979 </td>
   <td style="text-align:right;"> -6,985 </td>
   <td style="text-align:right;"> -6,988 </td>
  </tr>
</tbody>
</table>

Sin embargo, al verificar los coeficientes de los modelos $GARCH(2,2)$ y $GARCH(2,1)$, se obtiene que los coeficientes $\\alpha_2$ no son significativos (resultado no mostrado). 
Es por ello que se escoge el modelo $GARCH(1,1)$ con componente $ARMA(2,0)$, dados los criterios de información mostrados antes. 
Los coeficientes de este modelo se muestran a continuación.

<a name="p05-07-07-coef-est"></a>

```r
garch_fitting %>%
  mutate(tidied=map(garch_mods_fit, 
    \(x) {
    x@fit$matcoef %>%
      as_tibble()
  }), .after=2) %>% 
  unnest(tidied) %>%
  filter(ARMA_mod == "ARMA(2, 0)" & GARCH_mod == "GARCH(1,1)") %>%
  mutate(
  	criteria=c("$\\phi_1$", "$\\phi_2$", "$\\alpha_0$", "$\\alpha_1$", "$\\beta$"),
  	.after=2) %>%
  select(criteria:`Pr(>|t|)`) %>%
  kbl(digits = c(NA, 6, 7, 2, 4),
  	escape=FALSE,
  	col.names=c("Coef.", "Estimado", "Desv. Estand.", "$t$", "$p$"),
    caption="Coeficientes estimados para el modelo ARMA(2,0) con GARCH(1,1).")
```

<table>
<caption>Tabla 12. Coeficientes estimados para el modelo ARMA(2,0) con GARCH(1,1).</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Coef. </th>
   <th style="text-align:right;"> Estimado </th>
   <th style="text-align:right;"> Desv. Estand. </th>
   <th style="text-align:right;"> $t$ </th>
   <th style="text-align:right;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\phi_1$ </td>
   <td style="text-align:right;"> 0,442653 </td>
   <td style="text-align:right;"> 0,0211138 </td>
   <td style="text-align:right;"> 20,97 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:right;"> -0,114352 </td>
   <td style="text-align:right;"> 0,0211563 </td>
   <td style="text-align:right;"> -5,41 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_0$ </td>
   <td style="text-align:right;"> 0,000007 </td>
   <td style="text-align:right;"> 0,0000002 </td>
   <td style="text-align:right;"> 27,57 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_1$ </td>
   <td style="text-align:right;"> 0,174565 </td>
   <td style="text-align:right;"> 0,0138680 </td>
   <td style="text-align:right;"> 12,59 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\beta$ </td>
   <td style="text-align:right;"> 0,721929 </td>
   <td style="text-align:right;"> 0,0141531 </td>
   <td style="text-align:right;"> 51,01 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

El modelo final escogido se escribe entonces:

$$
\begin{aligned}
  r_t &= \phi_1 r_{t-1} + \phi_2 r_{t-2} + \sigma_t\epsilon_t \\
  \sigma_t &= \alpha_0 + \alpha_1 r_{t-1} + \beta \sigma_{t-1}
\end{aligned}
$$

donde $\epsilon_t \sim N(0, 1)$. 
Los residuales, tal como se observan en la <a href="#p05-07-08-residuals">figura 26</a>, donde se observa que ya no hay correlaciones importantes en las ACF y PACF, pero los residuales no se distribuyen normalmente, sino que parecen seguir una $t$-Student simétrica. 

<a name="p05-07-08-residuals"></a>

```r
residuals_garch <- garch_fitting %>%
  filter(ARMA_mod == "ARMA(2, 0)" & GARCH_mod == "GARCH(1,1)") %$%
  residuals(garch_mods_fit[[1]]) %>%
  zoo::fortify.zoo() %>%
  as_tsibble()

cowplot::plot_grid(
  autoplot(residuals_garch, ., colour="orange") +
    theme_light() + xlab("") + ylab("Innovaciones"),
  residuals_garch %>% 
    ggplot(aes(sample = .)) + 
    stat_qq() + stat_qq_line(color="dodgerblue") + 
    theme_light(),
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
<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-07-08-residuals-1.png" alt="Gráficos diagnósticos de residuales."  />
</div><table><caption>Figura 25. Gráficos diagnósticos de residuales.</caption><colgroup><col width='100'></col></colgroup><thead><tr class='header'></tr></thead><tbody></tbody></table><p>

Al realizar un ultimo ajuste del modelo $GARCH(1,1)$ con modelo para la media $ARMA(2,0)$, y usando como modelo de distribución una $t$-Student, se obtienen criterios de información menores que los encontrados para los modelos anteriores, indicando una preferencia por el nuevo modelo. 
Los coeficientes estimados se muestran en la siguiente tabla:


```r
final_spec <- ugarchspec(
  mean.model = list(
    armaOrder=c(2, 0), 
   	include.mean = FALSE),
  variance.model=list(
  	model = "sGARCH",
  	garchOrder=c(1,1)),
  distribution.model = "std")

final_fit <- ugarchfit(final_spec, EU_SMI)
final_coefs <- final_fit@fit$matcoef

final_coefs %>%
  as_tibble() %>%
  mutate(Coef=c("$\\phi_1$", "$\\phi_2$", "$\\alpha_0$", "$\\alpha_1$", "$\\beta$", "shape"), .before=1) %>%
  kbl(digits = c(NA, 5, 6, 2, 4),
  	escape=FALSE,
  	col.names=c("Coef.", "Estimado", "Desv. Estand.", "$t$", "$p$"),
    caption="Coeficientes estimados para el modelo ARMA(2,0) con GARCH(1,1)."
  )
```

<table>
<caption>Tabla 13. Coeficientes estimados para el modelo ARMA(2,0) con GARCH(1,1).</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Coef. </th>
   <th style="text-align:right;"> Estimado </th>
   <th style="text-align:right;"> Desv. Estand. </th>
   <th style="text-align:right;"> $t$ </th>
   <th style="text-align:right;"> $p$ </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\phi_1$ </td>
   <td style="text-align:right;"> 0,53257 </td>
   <td style="text-align:right;"> 0,022174 </td>
   <td style="text-align:right;"> 24,02 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\phi_2$ </td>
   <td style="text-align:right;"> -0,11589 </td>
   <td style="text-align:right;"> 0,016532 </td>
   <td style="text-align:right;"> -7,01 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_0$ </td>
   <td style="text-align:right;"> 0,00003 </td>
   <td style="text-align:right;"> 0,000002 </td>
   <td style="text-align:right;"> 14,02 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha_1$ </td>
   <td style="text-align:right;"> 0,67608 </td>
   <td style="text-align:right;"> 0,080122 </td>
   <td style="text-align:right;"> 8,44 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\beta$ </td>
   <td style="text-align:right;"> 0,01382 </td>
   <td style="text-align:right;"> 0,017169 </td>
   <td style="text-align:right;"> 0,80 </td>
   <td style="text-align:right;"> 0,4209 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> shape </td>
   <td style="text-align:right;"> 4,97331 </td>
   <td style="text-align:right;"> 0,483095 </td>
   <td style="text-align:right;"> 10,29 </td>
   <td style="text-align:right;"> 0,0000 </td>
  </tr>
</tbody>
</table>

De los valores de probabilidad se tiene que en este caso, el modelo no necesita de términos retrasados de la volatilidad, dado que $\\beta$ no es significativamente distinto de cero, por lo que el modelo se puede escribir como uno con solo componente $Arch(1)$:

$$
\begin{aligned}
  r_t &= 0,533_{(0,022)} r_{t-1} -0,116_{(0,017)} r_{t-2} + \sigma_t\epsilon_t \\
  \sigma_t &= 0,00003_{(0,000002)} + 0,676_{(0,08)} r_{t-1}
\end{aligned}
$$

con $\epsilon \sim t(\sim 5)$. 
El gráfico de los retornos versus la volatilidad muestra que el modelo es capaz de toma en cuenta las desviaciones en la serie; y el gráfico _QQ_ muestra que al usar como modelo teórico la distribución $t(5)$, las observaciones se ajustan muy bien a la recta.


```r
residuals_garch <- residuals(final_fit) %>%
  zoo::fortify.zoo() %>%
  as_tsibble()

cowplot::plot_grid(
  EU_SMI %>%
    mutate(abs_returns = abs(smi_difflog)) %>%
    autoplot(abs_returns, colour="200", alpha = .6) +
    autolayer(sigma(final_fit) %>%
      zoo::fortify.zoo() %>%
      as_tsibble(), ., colour="dodgerblue", linewidth = .5) +
    theme_light() +
    ylab("Volatilidad") + xlab(""),
  residuals_garch %>% 
    ggplot(aes(sample = .)) + 
    stat_qq(distribution=stats::qt,
    	dparams=list(df=4.97)) + 
    stat_qq_line(distribution=stats::qt,
    	dparams=list(df=4.97),
    	color="dodgerblue") + 
    theme_light(),
    nrow=1
)
```

<img src="/Series Temporales/output/Shumway-Stoffer-Solutions_files/figure-html/p05-07-10-volatility-plot-1.png" style="display: block; margin: auto;" />

<!---
[19/4 7:15 p. m.] Marcano: https://rpubs.com/jrodriguezmam/series_temporales
[19/4 7:45 p. m.] Marcano: https://rpubs.com/Aeoiu2019/890500
[22/4 7:07 a. m.] Marcano: https://youtu.be/CIAbHnsfj4A
[22/4 7:07 a. m.] Marcano: Buen día para ayudarlos a tener el proyecto para esta semana ahora pueden usar inteligencia artificial
[22/4 7:23 a. m.] Marcano: https://youtu.be/lUb6lHZVJvU

https://www.youtube.com/watch?v=emF1iBcni0U&ab_channel=AnaMetriks
https://liclourdescuellar.ruplayers.com/
https://finance-r.netlify.app/quantmod.html
-->

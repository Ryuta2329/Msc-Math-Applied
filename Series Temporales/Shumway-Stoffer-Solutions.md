# Soluciones a problemas de _Time Series Analysis and its Applications_ de Shumway y Stoffer

### Marcelo Molinatti


```r
library(astsa)
library(ggfortify)
library(kableExtra)
```

* Ejercicios del [capitulo 1](#capitulo-1). Características de series temporales.
* Ejercicios del [capitulo 2](#capitulo-2). Análisis de datos exploratorio.
* Ejercicios del [capitulo 3](#capitulo-3). Modelos ARIMA.

## Capitulo 1.

**Problema 1.1** Para comparar las señales de terremotos y explosiones, represente los datos en el mismo gráfico usando diferentes colores o diferentes tipos de líneas y comente los resultados.


```r
p1 <- autoplot(cbind(EQ5, EXP6), facets=FALSE) +
	xlab("Índice de la muestra") + ylab("Amplitud") +
	scale_colour_manual(name="", values=c("20","200")) + 
	theme_light() +
	theme(legend.position = c(0.3, 0.85))
```

Lo primero que sale a la vista en el gráfico es que la variabilidad de las señales de terremoto es mayor (la amplitud de la señal es mayor) y que esta variabilidad se extiende por más tiempo en el registro, mientras que la de explosiones es menor la variabilidad y la amplitud solo se extiende un intervalo de tiempo corto. 

**Problema 1.2** Considere un modelo de señal más ruido de la forma general $x_t = s_t + w_t$, donde $w_t$ es ruido blanco gaussiano con $\sigma^2_w = 1$. Simule y grafique $n = 200$ observaciones de cada uno de los siguientes dos modelos.

_a)_ $x_t = s_t + w_t$, para $t = 1, \ldots , 200$, donde:

$$
s_t = \begin{cases}
    0, & t= 1, \ldots, 100\\
    10\text{exp}{-\frac{(t-100)}{20}}\text{cos}(2\pi t/4), & t=101, \ldots, 200
  \end{cases}
  \label{eq:p1-2-st_a}
$$

_b)_ $x_t = s_t + w_t$, para $t = 1, \ldots , 200$, donde

$$
s_t = \begin{cases}
    0, & t= 1, \ldots, 100\\
    10\text{exp}{-\frac{(t-100)}{200}}\text{cos}(2\pi t/4), & t=101, \ldots, 200
  \end{cases}
  \label{eq:p1-2-st_b}
$$


```r
xt_a <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 20) * cos(2 * pi * 101:200 / 4)) + rnorm(200, 0, 1)
xt_b <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 200) * cos(2 * pi * 101:200 / 4)) + rnorm(200, 0, 1)
xt <- ts(cbind(xt_a, xt_b), start=1)

p2 <- autoplot(xt, facets=TRUE) +
	xlab("Índice de la muestra") + ylab("Amplitud") + 
	theme_light()

cowplot::plot_grid(p1, p2, nrow=1)
```

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p1-2-1.png)<!-- -->

_c)_ Compare la apariencia general de las series _a)_ y _b)_ con la serie de terremoto y la serie de explosión. Además, traza (o dibuja)
y compare los moduladores de señal _a)_ $exp{−t/20}$ y _b)_ $exp{−t/200}$, para $t = 1, 2, \ldots, 100$.

La serie en _a)_ parece describir de forma adecuada la fase P y S de las explosiones, dada la caida rapida de la amplitud al inicio de la fase S, y tambien notando que la variabilidad es bastane similar es esta serie con la serie de explosiones. Sun embargo, en la fase P, la serie del inciso _a)_ no parece realizar un buen trabajo en simular la serie de explosiones, principalemnte al inicio donde parece haber un cambio de variabilidad importante.

En el caso de la serie en el inciso _b)_, la fase S parece ser similar tambien en amplitud, aunque la serie parece ser mas regular y menos variable que la registrada en Escandinavia para la serie de terremoto. De igual forma, en la fase P la serie no tiene una variabilidad tan grande haciendo que la similitud en esta zona sea distinta (a lo mejor, un ruido aleatorio de varianza 1 no es lo suficientemente bueno para simular el proceso registrado en Escandinavia).


```r
modulators <- data.frame(mod_a=exp(-(1:100) / 20), mod_b=exp(-(1:100) / 200))

autoplot(ts(modulators, start=1), facets=FALSE) +
	xlab("t") + ylab(latex2exp::TeX("$exp\\{-t/\\tau\\}$")) + 
	scale_colour_manual(name=latex2exp::TeX("\\tau"), values=c("20","200"), labels=c("20","200")) + 
	theme_light()
```

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p1-2-2-1.png)<!-- -->

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

<img src="Shumway-Stoffer-Solutions_files/figure-html/fig:p1-3-1.png" width="100%" />

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

De forma que, para el inciso _a)_ $E[x_{t,a}]$ viene dada por \ref{eq:p1-2-st_a}, y para el inciso _b)_ $E[x_{t,b}]$ viene dada por \ref{eq:p1-2-st_b}. 


```r
xt_a <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 20) * cos(2 * pi * 101:200 / 4)) 
xt_b <- c(rep(0, 100), 10 * exp(-(101:200 - 100) / 200) * cos(2 * pi * 101:200 / 4)) 
xt <- ts(cbind(xt_a, xt_b), start=1)

autoplot(xt, facets=FALSE) +
	xlab("Índice de la muestra") + ylab(expression("E[x"["t"]*"]")) + 
	scale_colour_manual(name="", values=c("20","200"), labels=c("a", "b")) +
	theme_light()
```

![](Shumway-Stoffer-Solutions_files/figure-html/p1-5-1.png)<!-- -->

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

Para el incisio _a)_, se puede demostrar que $x_t$ no es estacionario ya que $E[x_t] = E[\beta_1 + \beta_2t + w_t] = \beta_1 + \beta_2t + E[w_t] = \beta_1 + \beta_2t$ no es independiente del tiempo. Además, la función de autocovarianza es:

$$
\begin{aligned}
	\gamma(s, t) &= E[(x_{t} - \mu_t)(x_s - \mu_s)]\\
		&= E[(\beta_1 + \beta_2t + w_t - \beta_1 - \beta_2t)(\beta_1 + \beta_2s + w_s - \beta_1 - \beta_2s)] \\
		&= E[w_tw_s] 
\end{aligned}
$$

Cuando $s=t$, entonces $\gamma(s, t) = \sigma_w^2$. Si $s\ne t$, los ruidos aleatorios son independientes y por lo tanto $\gamma(s, t) = 0$. Esta no depdnede de las diferencias entre $s$ y $t$.  
Por otro lado, en el inciso _b)_ se tiene el proceso $y_t = x_t − x_{t−1}$, que se escribe en terminos de las variables $\beta$ como:

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
gamma_y(s, t) = \begin{cases}
		2\sigma_w^2, & \vert h\vert = 0,
		\sigma_w^2, & \vert h \vert = 1,
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

Si $s=t-2$, se tiene que solo quedan $2q - 1$ terminos que no se anulan, y por lo tanto, $(2q - 1)\sigma_w^2$. De forma que se puede escribri la función de aitocovarianza en terminos de la diferencia $h=s-t$ como:

$$\gamma_v(s, t) = \left(\frac{2q + 1 - h}{(2q + 1)^2}\right)\sigma_w^2$$

lo cual muestra que la funcion de autocovarianza va decreciendo con la diferencia de tiempo $h$.

**Problema 1.7** Para un proceso de promedio móvil de la forma $x_t = w_{t−1} + 2w_t + w_{t+1}$, donde $w_t$ son independientes con medias cero y varianza $\sigma_w^2$, determine las funciones de autocovarianza y autocorrelación en función del desfase $h = s − t$ y grafique el ACF
en función de $h$.

La función de autocovarianza es (dado que la media es 0, ya que se trata de la suma de la media de tres ruidos blancos): $\gamma(t,s) = E[(w_{t−1} + 2w_t + w_{t+1})(w_{s−1} + 2w_s + w_{s+1})]$. Cuando $s=t$, se tiene:

$$
\begin{aligned}
	\gamma(t,t) &= E[(w_{t−1} + 2w_t + w_{t+1})(w_{t−1} + 2w_t + w_{t+1})] \\
		&= cov(w_{t-1}, w_{t-1}) + 2 cov(w_{t}, w_{t}) + cov(w_{t+1}, w_{t+1}) \\
		&= 4\sigma_w^2
\end{alogned}
$$

Cuando la diferencia entre $s$ y $t$ es 1, solo los terminos del centro y la derecha no se anulan, por lo que $\gamma(t,t\pm1) = 3\sigma_w^2$. Si la diferencia es de 2 solo un termino (el de la izquierda) no se cancela, por lo que $\gamma(t,t\pm2) = \sigma_w^2$. Para diferencias mayores que 2, la función de autocovarianza es cero. Queda entonces:

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

El grafico de esta es:


```r
df <- data.frame(lag=-5:5, rho=c(0, 0, 0, .25, .75, 1, .75, .25, 0, 0, 0))

ggplot(data = df, mapping = aes(x = lag, y = rho)) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  ylab(latex2exp::TeX("$\\rho(s, t)$")) +
  theme_light()
```

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p1-7-1.png)<!-- -->

**Problema 1.8** Considere el modelo de paseo aleatorio con deriva $x_t = \delta + x_{t−1} + w_t$, para $t = 1, 2, \ldots$, con $x_0 = 0$, donde $w_t$ es ruido blanco con varianza $\sigma_w^2$.  
_a)_ Demuestre que el modelo se puede escribir como $x_t = \delta t + \sum_{k=1}^t w_k$.  
_b)_ Encuentre la función media y la función de autocovarianza de $x_t$.  
_c)_ Argumente que $x_t$ no es estacionario.  
_d)_ Muestre $\rho_x(t − 1, t) = \sqrt{\frac{t−1}{t}} \rightarrow 1$ cuando $t \rigtharrow \infty$. ¿Cuál es la implicación de este resultado?  
_e)_ Sugiera una transformación para hacer que la serie sea estacionaria y demuestre que la serie transformada es estacionaria.

Para el inciso _a)_, se puede rescribir el modelo usando la definicion del modelo de paseo alaetorio de forma recursiva, notando que en $t=0$ no hay una opbaservación, por lo que definimos $x_0=0$, quedando:

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
		&= \text{min}\{s, t\} \sigma_w^2
\end{aligned}
$$

Como es posible ver de las funciones de media y autocovarianza, el proceso no es estacioanrio dado que la media depende del valor de $t$ y crece a medida que avamca el tiempo, sin cota alguna. Meintras que la función de autocvarianza no depende de la diferencia entre $s$ y $t$, sino del mínimo valor de estos. Por lo tanto, no es posible cumplir los requisitos de estacionaridad débil, y mucho menos al estacioanridad estricta.  
Se tiene que $\rho_x(t − 1, t) = \sqrt{\frac{t−1}{t}}$. Cuando $t \rightarrow \infty$, el numerador en la ACF tiende a $t$, haceindo que $\rho_x(t-1, t) \rightarrow 1$, lo cual implica una correlación perfecta entre el valor observado en $t$ y el inmediatamente adyacente, indicando en teoría, que sería posible el predecir con certeza el valor en $t$, conociendo el valor inmediatamente anterior.  
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

donde el termino central sea nula dado que $U_1$ y $U_2$ son independientes (tal que $E[U_1U_2] = E[U_1]E[U_2] = 0$) y la identidad trigonometrica para suma de angulos para el coseno ($cos(\alpha - \beta) = cos(\alpha)cos(\beta) + sen(\alpha)sen(\beta)$).

**Problema 1.10** Supongamos que nos gustaría predecir una sola serie estacionaria $x_t$ con media cero y función de autocorrelación $\rho(h)$ en algún momento en el futuro, digamos, $t + \ell$, para $l > 0$.  
_a)_ Si predecimos usando solo $x_t$ y algún multiplicador de escala $A$, demuestre que el error de predicción cuadrático medio $MSE(A) = E[(x_{t+l} - Ax_t)^2]$ es minimizado por $A = \rho(l)$.  
_b)_ Demuestre que el error de predicción cuadrático medio mínimo es $MSE(A) = \gamma(0)[1 - \rho^2(l)]$.  
_c)_ Demuestre que si $x_{t+l} = Ax_t$, entonces $\rho(l) = 1$ si $A > 0$, y $ρ(l) = −1$ si $A < 0$.

De la definicion de $MSE$, se tiene por producto notable que:

$$\begin{aligned}
MSE(A) = E[(x_{t+l} − Ax_t)^2] &= E[x_{t+l}^2 - 2Ax_tx_{t+l} + A^2x_t^2] \\
	&= E[x_{t+l}^2] - 2AE[x_tx_{t+l}] + A^2E[x_t^2]
\end{aligned}
$$

Derivando con respecto a $A$, e igualando a cero queda:

$$-2E[x_tx_{t+l}] + 2AE[x_t^2] = 0$$

Resolviendo para $A$ da:

$$A = \frac{E[x_tx_{t+l}]}{E[x_t^2]}$$

El numerador es la autocovarianza $\gamma(l)$ ya que $x_t$ tiene media cero. El denominador es la autocovarianza $\gamma(0)$, por lo que $A = \gamma(l)/\gamma(0) = \rho(l)$ y queda demostrado.

Para demostrar el inciso _b)_, solo se necesita sacar $\gamma(0)$ como factor comun de la expansion del producto notable:

$$\begin{aligned}
MSE(A) = E[(x_{t+l} − Ax_t)^2]  \\
	&= E[x_{t+l}^2] - 2AE[x_tx_{t+l}] + A^2E[x_t^2] \\
	&= \gamma(0) - 2\frac{\gamma(l)}{\gamma(0)}\gamma(l) + \left(\frac{\gamma(l)}{\gamma(0)}\right)^2\gamma(0) \\
	&= \gamma(0)\left(1 - 2\left(\frac{\gamma(l)}{\gamma(0)}\right)^2 + \left(\frac{\gamma(l)}{\gamma(0)}\right)^2\right) \\
	&= \gamma(0)\left(1 - \left(\frac{\gamma(l)}{\gamma(0)}\right)^2\right) \\
	&= \gamma(0)\left(1 - \rho^2(l)\right)
\end{aligned}
$$

De la definicion se tiene:

$$\begin{aligned}
	\rho(l) &= \frac{\gamma(l)}{\gamma(0)} \\
		&= \frac{E[x_{t+l}x_t]}{E[x_t^2]} \\
		&= \frac{AE[x_t^2]}{E[x_t^2]} \\
		&= A
\end{aligned}
$$

Si $A > 0$, entonces $\rho(l) = 1$, dado que se esta correlacion seria entre $x_t$ y ella misma. Si $A < 0$, la correlacion es negativa, debido al sogno de $A$, pero seria la minima posible ya que la correlacion seria aun entre $x_t$ y ella misma, de forma que se escribe $\rho(l) = -1$

<!---
**Problema 1.11** Considere el proceso lineal definido en (1.31), como:

$$x_t = \mu + \sum_{j=-\infty}^\infty \psi_jw_{t-j}, \qquad \sum_{j=-\infty}^\infty \vert\psi_j\vert < \infty$$

_a)_ Verifique que la función de autocovarianza del proceso está dada por (1.32):  

$$\gamma_x(h) = \sigma_w^2 \sum_{j=-\infty}^\infty \psi_{j+h}\psi_j$$

Use el resultado para verificar su respuesta al Problema 1.7. Pista: Para $h \ge 0$, $cov(x_{t+h}, x_t) = cov(\sum_k \psi_k w_{t+h−k}, \sum_j \psi_jw_{t−j})$. Para cada $j \in Z$, el único _superviviente_ será cuando $k = h + j$.
_b)_ Demuestre que $x_t$ existe como un límite en el cuadrado medio (vea el Apéndice A).

**Problema 1.12** Para dos series débilmente estacionarias $x_t$ e $y_t$, verifique (1.30): $\rho_{xy}(h) = \rho_{yx}(−h)$.

**Problema 1.13** Considere las dos series $x_t = w_t$ y $y_t = w_t − \theta w_{t−1} + u_t$, donde $w_t$ y $u_t$ son series de ruido blanco independientes con varianzas $\sigma_w^2$ y $\sigma_u^2$, respectivamente, y $\theta$ es una constante no especificada.  
_a)_ Exprese el ACF, $\rho_y(h)$, para $h = 0, \pm1, \pm2, \ldots$ de la serie $y_t$ en función de $\sigma_w^2$, $\sigma_u^2$ y $\theta$.  
_b)_ Determine el CCF, $ρ_{xy}(h)$ relacionando $x_t$ y $y_t$.
_c)_ Demuestre que $x_t$ e $y_t$ son conjuntamente estacionarios.

**Problema 1.14** Sea $x_t$ un proceso normal estacionario con media $\mu_x$ y función de autocovarianza $\gamma(h)$. Definir la serie de tiempo no lineal $y_t = exp{x_t}$. _a)_ Exprese la función media $E(y_t)$ en términos de $\mu_x$ y $\gamma(0)$. La función generadora de momentos de una variable aleatoria normal $x$ con media $\mu$ y varianza $\sigma^2$ es:

$$M_x(λ) = E[exp{λx}] = exp\muλ + 1 2σ2λ2$$

_b)_ Determine la función de autocovarianza de $y_t$. La suma de las dos variables aleatorias normales $x_{t+h} + x_t$ sigue siendo una variable aleatoria normal.

**Problema 1.15** Sea $w_t$, para $t = 0, \pm1, \pm2, \ldots$ un proceso de ruido blanco normal, y considerar la serie $x_t = peso peso−1$.
Determine la media y la función de autocovarianza de $x_t$ e indique si es estacionaria.

**Problema 1.16** Considere la serie $x_t = sin(2\pi Ut)$, $t = 1, 2, \ldots$, donde $U$ tiene una distribución uniforme en el intervalo $(0, 1)$.  
_a)_ Demuestre que $x_t$ es débilmente estacionario.
_b)_ Demuestre que $x_t$ no es estrictamente estacionario.

**Problema 1.17** Supongamos que tenemos el proceso lineal xt generado por xt = wt − θwt−1, t = 0, 1, 2, . . ., donde {wt } es independiente e idénticamente distribuida con función característica φw(·), y θ es una constante fija. [Reemplazar “función característica” con “función generadora de momento” si se le indica que lo haga.]
(a) Exprese la función característica conjunta de x1, x2, . . . , xn, digamos, φx1,x2,...,xn(λ1, λ2, . . . , λn), en términos de φw(·).
(b) Deducir de (a) que xt es estrictamente estacionario.

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

**Problema 2.1** Un modelo estructural para los datos de Johnson y Johnson, digamos $y_t$, sea $x_t = log(y_t)$. En este problema, vamos a ajustar un tipo especial de modelo estructural, $x_t = T_t + S_t + N_t$ donde $T_t$ es un componente de tendencia, $S_t$ es un componente estacional y $N_t$ es ruido. En nuestro caso, el tiempo $t$ está en trimestres ($1960{,}00, 1960{,}25, \ldots$) por lo que una unidad de tiempo es un año.   
_a)_ Ajuste el modelo de regresión

$$x_t = \beta t + \alpha_1 Q_1(t) + \alpha_2 Q_2(t) + \alpha_3 Q_3(t) + \alpha_4 Q_4(t) + w_t$$

donde $Q_i(t) = 1$ si el tiempo $t$ corresponde al trimestre $i = 1, 2, 3, 4$ y cero en caso contrario. Las $Q_i(t)$ se denominan variables indicadoras. Supondremos por ahora que $w_t$ es una secuencia de ruido blanco gaussiana.   
_b)_ Si el modelo es correcto, ¿cuál es el incremento anual promedio estimado en las ganancias registradas por acción?
_c)_ Si el modelo es correcto, ¿la tasa promedio de ganancias registradas aumenta o disminuye del tercer trimestre al cuarto trimestre? y ¿en qué porcentaje aumenta o disminuye?
_d)_ ¿Qué sucede si incluye un término de intersección en el modelo en _a)_? Explique por qué hubo un problema.
_e)_ Grafique los datos, $x_t$, y superponga los valores ajustados, digamos $\hat{x}_t$, en el gráfico. Examine los residuos, $x_t − \hat{x}_t$, y establezca sus conclusiones. ¿Parece que el modelo se ajusta bien a los datos (los residuos se ven blancos)?

La figura \ref{fig:ex01} muestra las ganancias trimestrales por acción de la empresa estadounidense _Johnson & Johnson_, proporcionada por el profesor Paul Griffin (comunicación personal) de la _Graduate School of Management_ de la Universidad de California, Davis. Hay 84 trimestres (21 años) medidos desde el primer trimestre de 1960 hasta el último trimestre de 1980.


```r
autoplot(jj) +
  ggtitle("Pasageros de la Clase Economica: Melbourne-Sydney") +
  xlab("Año") +
  ylab("Miles") +
  theme_light()
```

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p02-01-1.png)<!-- -->

Para transformar los datos, reorganizo los datos a un fromato largo, y creo los regresores para el elemento de tendencia (usando ```lubridate::year```) y para los cuartos de cada año (usando ```lubridate::quarter```). Luego, para este ultimo, se crearon 4 variables distintas $Q_i$ usando una expresion condicional sobe ```quarter```.


```r
library(lubridate)

# Transformando ls datos para obtener y_t
# y generar los regresores t y Q_i
df_jj <- jj |>
	tsibble::as_tsibble(key=c("Q1", "Q2", "Q3", "Q4")) |>
	mutate(log_Earnings = log(value), 
		year = year(index), 
		quarter=factor(quarter(index)), 
		Q1=ifelse(quarter == 1, 1, 0),
		Q2=ifelse(quarter == 2, 1, 0),
		Q3=ifelse(quarter == 3, 1, 0),
		Q4=ifelse(quarter == 4, 1, 0))
```

Ajustando el modelo de regresión, arroja los siguientes resultados mostrados en la tabla \ref{tab:p02-01}. Los resultados del ajuste se interpretan tomando en cuenta el uso de variables indicadoras: ```year``` corresponde al elemento de tendencia $\beta$, y cada ```Q1, Q2, Q3``` y ```Q4``` corresponde a los valores de $\alpha_i$.


```r
library(broom)

# Ajuste del modelo
# mod <- lm(log_Earnings ~ year + quarter, df_jj)
mod <- lm(log_Earnings ~ year + Q1 + Q2 + Q3 + Q4 - 1, df_jj)

# Resultados de la estimacion.
tidy(mod) |> 
	kable(digits=4, 
		col.names=c("Regresor", bquote(beta["i"]), bquote(sigma["i"]), "Z", "p"),
		caption="Resultados de la regresión lineal: Estimadores para los coeficientes del modelo.", 
		label="tab:p02-01")
```

\begin{table}

\caption{\label{tab:tab:p02-01}Resultados de la regresión lineal: Estimadores para los coeficientes del modelo.}
\centering
\begin{tabular}[t]{l|r|r|r|r}
\hline
Regresor & beta["i"] & sigma["i"] & Z & p\\
\hline
year & 0,1672 & 0,0023 & 73,9990 & 0\\
\hline
Q1 & -328,2764 & 4,4505 & -73,7611 & 0\\
\hline
Q2 & -328,2065 & 4,4505 & -73,7454 & 0\\
\hline
Q3 & -328,0946 & 4,4505 & -73,7202 & 0\\
\hline
Q4 & -328,3215 & 4,4505 & -73,7712 & 0\\
\hline
\end{tabular}
\end{table}

Los resultados muestran que todos los coeficientes son significativos, con $p$-valores mucho menores a $0{,}01$. Es de hacer notar que la diferencia entre la contribución de cada cuarto a las ganancias (en escala logarítmica) es bastante similar, variando solo en las décimas o centésimas.  
Los cambios anuales en las ganancias viene dado por el coeficiente para $t$ (```year```) de la siguiente forma:

$$x_t - x_{t-1} = \beta t - \beta (t-1) = \beta$$

donde los terminos para los cuartos se cancelan en la diferencia. Usando $log(y_t) - log(y_t) = log(y_t/y_{t-1]})$ permite obtener:

$$y_t = y_{t-1}e^\beta$$

Es decir, anualmente hay un incremento promedio de $e^\beta = 1,182$ en las ganancias por accion, suponiendo que el modelo es correcto.   
Los incrementos/decrementos de cuarto a cuarto en un mismo año vienen dados por:

$$x_{t,Q_i} - x_{t-1, Q_{i-1}} = \alpha_i - \alpha_{i-1}$$

y al devolver la transformacion, como antes, se obtiene:

$$y_{t, Q_i} = y_{t, Q_{i-1}}e^{\alpha_i - \alpha_{i-1}}$$

Para el incremento del cuarto cuarto al primero, se da un incremento de año de forma que el termino de tendencia no desaparece. Pero por propiedad de exponenciales, este se se puede separar del cambio de cuarto de año como: $y_{t, Q_1} = y_{t-1, Q_4}e^{\beta}e^{\alpha_4 - \alpha_1}$. Los cambios de un cuarto a otro se muestran en la tabla \ref{tab:p02-02}.


```r
# Etiquetas para cada uno de los cambios de cuarto
labels <- c(
	bquote(alpha["2"]~"-"~alpha["1"]), 
	bquote(alpha["3"]~"-"~alpha["2"]), 
	bquote(alpha["4"]~"-"~alpha["3"]), 
	bquote(alpha["1"]~"-"~alpha["4"]))
# Incrementos de cuarto a cuarto
increment <- exp(diff(coef(mod)[c("Q1", "Q2", "Q3", "Q4", "Q1")]))

# Tabla de incrementos cuarto a cuarto
tibble(Etiqueta=labels, Incremento=increment) %>%
	mutate(Porcentaje=100 * (Incremento - 1)) %>%
	kable(digits=4,
		caption="Incrementos en las ganancias promedio por accion cuarto a cuarto.", 
		label="tab:p02-02")
```

\begin{table}

\caption{\label{tab:tab:p02-02}Incrementos en las ganancias promedio por accion cuarto a cuarto.}
\centering
\begin{tabular}[t]{l|r|r}
\hline
Etiqueta & Incremento & Porcentaje\\
\hline
alpha["2"] \textasciitilde{} "-" \textasciitilde{} alpha["1"] & 1,0724 & 7,2418\\
\hline
alpha["3"] \textasciitilde{} "-" \textasciitilde{} alpha["2"] & 1,1184 & 11,8403\\
\hline
alpha["4"] \textasciitilde{} "-" \textasciitilde{} alpha["3"] & 0,7969 & -20,3051\\
\hline
alpha["1"] \textasciitilde{} "-" \textasciitilde{} alpha["4"] & 1,0462 & 4,6182\\
\hline
\end{tabular}
\end{table}

Como se observa, a excepción del paso del tercer cuarto al cuarto, siempre ocurre un incremento en las ganancias promedio por acción: de $7{,}24$% en el $Q2$, de $11{,}8$% en el $Q3$, y un incremento de $4{,}62$% al pasar al $Q1$. Los resultados muestran que la caída en las ganancias al pasar al $Q4$ es el mayor cambio en las ganancias por acción en cada año, de $20{,}3$%. 

Al intentar añadir un coeficiente, el ajuste es capaz de determinarlo, pero arroja ```NA``` para el coeficiente $\alpha_4$. 
La razon de esto es que al añadir el termino para el coeficiente, este se toma como un caso base (en este caso, $Q1$. 
El coeficiente estimado corresponde entonces a $\alpha_1$), y cada uno de los términos $Q_i$ para $i=2, 3, 4$ se determinan como cambios (cuanto por encima o por debajo del intercepto) con respecto al intercepto: 
de forma que ```Q1``` en el modelo con intercepto es en realidad la diferencia $\alpha_2 - \alpha_1$; ```Q2``` en el modelo con intercepto es en realidad la diferencia $\alpha_3 - \alpha_1$; y ```Q3``` en el modelo con intercepto es en realidad la diferencia $\alpha_4 - \alpha_1$. Pero dado que ya se sacaron todas las diferencias entre cada cuarto con respecto al caso base (primer cuarto), el ultimo coeficiente no significa nada, arrojando el valor de ```NA```.

El gráfico para el modelo se observa en la figura \@ref(fig:p02-02), donde se observa que la serie ajustada esta ligeramente mas suavizada que la serie observada, aunque se nota que de 1962 a 1965 la series ajustada parece sobrestimar de manera sistemática la serie observada, mientras que de 1970 a 1975 el modelo parece subestimar de forma sistemática la serie observada. El resto del tiempo, la serie parece variar por encima o por debajo.


```r
augmented_df <- left_join(df_jj, augment(mod))
```

```
## Joining, by = c("log_Earnings", "year", "Q1", "Q2", "Q3", "Q4")
```

```r
ggplot(df_jj[, c("index", "log_Earnings")], aes(x=as.Date(index), y=log_Earnings)) +
	geom_line() + geom_point() + 
	geom_line(aes(y=.fitted), 
		data=augmented_df[, c("index", ".fitted")],
		color="orange") + 
	geom_point(aes(y=.fitted), 
		data=augmented_df[, c("index", ".fitted")], 
		color="orange") +
  ggtitle("Pasageros de la Clase Economica: Melbourne-Sydney (escala logaritmica)") +
  xlab("Año") +
  ylab("log Ganancias por Accion") + 
  theme_light()
```

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p02-02-1.png)<!-- -->

Al verificar los residuales, se observa en la figura \@ref(fig:p02-03) que la serie tiene un comportamiento que no se toma en cuenta en el modelo, alguna correlacion entre los valores adyacentes. Tambien se observan valores con mas de dos desviaciones estandar, y en general cambios mas violentos, al inicio y en el centro de la serie.


```r
ggplot(augmented_df[, c(".fitted", ".std.resid")], 
	aes(x=.fitted, y=.std.resid)) +
	geom_line() + geom_point() +
	geom_hline(yintercept=0) +
	xlab("Valores Estimados") +
  ylab("Residuales Estandarizados") + 
  theme_light()
```

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p02-03-1.png)<!-- -->

Las gráficas de la figura \@ref(fig:p02-04) muestran las función de autocorrelación y autocorrelación parcial. Se observa de inmediato que la caída de la primera es bastante lenta, indicando una correlación bastante grande entre elementos adyacentes en la serie; mientras que la función de autocorrelación parcial muestra que la correlación parece ser mayor solo entre observaciones separadas a intervalo de un año.


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
	  geom_hline(yintercept = c(1/sqrt(20), -1/sqrt(20)), linetype=2, color='blue') +
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

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p02-04-1.png)<!-- -->

La información mostrada en las funciones de autocorrelación y en la distribución de los residuales parece indicar que el modelo no es del todo correcto y que sera apropiado ajustar un modelo autoregresivo.

**Problema 2.2** Para los datos de mortalidad cardiovascular:  
_a)_ Agregue otro componente a la regresión que represente el conteo de partículas cuatro semanas antes; es decir, agregue $P_{t−4}$. Exprese su conclusión.
_b)_ Dibuje una matriz de diagrama de dispersión de $M_t$, $T_t$, $P_t$ y $P_{t−4}$ y luego calcule las correlaciones por pares entre las series. Compare la relación entre $M_t$ y $P_t$ versus $M_t$ y $P_{t−4}$.


```
## 
## 
## processing file: Pollution-Mortality-example.Rmd
```

```
## 
  |                                                                            
  |                                                                      |   0%
  |                                                                            
  |.....                                                                 |   7%
  |                                                                            
  |..........                                                            |  14%
  |                                                                            
  |...............                                                       |  21%
  |                                                                            
  |....................                                                  |  29%
  |                                                                            
  |.........................                                             |  36%
  |                                                                            
  |..............................                                        |  43%
  |                                                                            
  |...................................                                   |  50%
  |                                                                            
  |........................................                              |  57%
  |                                                                            
  |.............................................                         |  64%
  |                                                                            
  |..................................................                    |  71%
  |                                                                            
  |.......................................................               |  79%
  |                                                                            
  |............................................................          |  86%
  |                                                                            
  |.................................................................     |  93%
  |                                                                            
  |......................................................................| 100%
```

```
## output file: /tmp/RtmpxLd2kx/file15b74f0b0762.R
```

```
## [1] "/tmp/RtmpxLd2kx/file15b74f0b0762.R"
```

```
## Warning: Expected frequency of weekly data: 365.25 / 7 (approx 52.18), not 52.
```

La primera parte del análisis se encuentra en el archivo de ejemplo para [la mortalidad cardiovascular y partículas contaminantes]. Allí, se muestra que el mejor modelo encontrado para mortalidad fue:

$$Mt &= 2831,5 + -1,396t + -0,472(T_t − T_\dot) + 0,023(T_t − T_\dot)^2 + 0,255P_t + w_t$$

En esta parte se busca añadir la información del retraso $P_{t-4}$ para verificar si hay una mejora en el ajuste. Los resultados se muestran a continuación:


```r
df_ts <- cbind(part, tempr, cmort)

# Se crea la variable retraso.
pt_4 <- ts.intersect(df_ts, pt_4=stats::lag(df_ts[, "part"],-4), dframe=TRUE)
colnames(pt_4) <- c("Particulas", "Temperatura", "Mortalidad", "P_t-4")

# Datos para la regresión
df_lag <- pt_4 %>%
	mutate(trend = time(cmort)[-(505:508)], 
		diff_Temp=Temperatura - mean(Temperatura),
		diff_Temp_Square=diff_Temp ** 2)

mod_lag <- lm(Mortalidad ~ trend + diff_Temp + diff_Temp_Square + Particulas + `P_t-4`, df_lag)
bind_rows(fitted_models[4,], glance(mod_lag)) %>%
	mutate(SSE=sigma ** 2 * df.residual, MSE=sigma ** 2) %>%
	dplyr::select(SSE, df.residual, MSE, adj.r.squared, AIC, BIC) %>%
	mutate(AIC=AIC / nrow(df_ts_tidy) - log(2*pi), BIC=BIC / nrow(df_ts_tidy) - log(2*pi)) %>%
	tibble::add_column(Model=c(
		"$M_t = \\beta_0 + \\beta_1 t + \\beta_2(T - T_\\dot) + \\beta_3(T - T_\\dot)^2 + \\beta_4 P_t + w_t$", "$M_t = \\beta_0 + \\beta_1 t + \\beta_2(T - T_\\dot) + \\beta_3(T - T_\\dot)^2 + \\beta_4 P_t + P_{t-4} + w_t$"), .before=1) %>%
	kable(digits=3,
		col.names=c("", "SSE", "df", "MSE", "R^2", "AIC", "BIC"),
		caption="Medidas de ajuste y de información para los modelos ajustados.", 
		label="tab:p02-02-01", escape=FALSE)
```

\begin{table}

\caption{\label{tab:tab:p02-02-01}Medidas de ajuste y de información para los modelos ajustados.}
\centering
\begin{tabular}[t]{l|r|r|r|r|r|r}
\hline
 & SSE & df & MSE & R^2 & AIC & BIC\\
\hline
$M_t = \beta_0 + \beta_1 t + \beta_2(T - T_\dot) + \beta_3(T - T_\dot)^2 + \beta_4 P_t + w_t$ & 20508,44 & 503 & 40,772 & 0,592 & 4,722 & 4,772\\
\hline
$M_t = \beta_0 + \beta_1 t + \beta_2(T - T_\dot) + \beta_3(T - T_\dot)^2 + \beta_4 P_t + P_{t-4} + w_t$ & 19687,01 & 498 & 39,532 & 0,604 & 4,641 & 4,700\\
\hline
\end{tabular}
\end{table}

Los resultados muestran que hay una mejora en el ajuste al añadir a $P_{t-4}$, pero que solo resulta en un aumento de la varianza explicada de 1,2%. Podemos verificar si el modelo es significativo por medio de la prueba $F$:

$$F(5, 498) = \frac{(40.8 - 39.5) / 5}{39.5 / 498} = 1.3218\times 10^{-5}$$

Este valor de $F$ tiene una probabilidad asociada de $F(5, 498) = 1$. Esto quiere decir que no hay una mejora significativa en el modelo al añadir a $P_{t-4}$ comparado con el modelo sin este termino.

Al realizar los gráficos de dispersión con los pares de variables del modelo, y calcular el coeficiente de correlación de estas, se puede observar que la correlación entre $M_t$ y $P_t$ versus $M_t$ y $P_{t−4}$, es bastante similar, siendo mayor par el ultimo caso. Dada esta correlación, seria mas apropiado elegir un modelo en el cual solo se haga una relación entre $M_t$ y $P_{t−4}$, eliminando el termino de $P_t$.


```r
if (!require("GGally"))
	install.packages("GGally")

pt_4 |> GGally::ggpairs()
```

```
## 
 plot: [1,1] [==>-----------------------------------------------]  6% est: 0s 
 plot: [1,2] [=====>--------------------------------------------] 12% est: 1s 
 plot: [1,3] [========>-----------------------------------------] 19% est: 1s 
 plot: [1,4] [===========>--------------------------------------] 25% est: 1s 
 plot: [2,1] [===============>----------------------------------] 31% est: 1s 
 plot: [2,2] [==================>-------------------------------] 38% est: 1s 
 plot: [2,3] [=====================>----------------------------] 44% est: 1s 
 plot: [2,4] [========================>-------------------------] 50% est: 1s 
 plot: [3,1] [===========================>----------------------] 56% est: 0s 
 plot: [3,2] [==============================>-------------------] 62% est: 0s 
 plot: [3,3] [=================================>----------------] 69% est: 0s 
 plot: [3,4] [=====================================>------------] 75% est: 0s 
 plot: [4,1] [========================================>---------] 81% est: 0s Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.
## 
 plot: [4,2] [===========================================>------] 88% est: 0s Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.
## 
 plot: [4,3] [==============================================>---] 94% est: 0s Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.
## 
 plot: [4,4] [==================================================]100% est: 0s 
                                                                              
Don't know how to automatically pick scale for object of type ts. Defaulting to continuous.
```

![](Shumway-Stoffer-Solutions_files/figure-html/fig:p02-02-03-1.png)<!-- -->

**Problema 2.3** En este problema, exploramos la diferencia entre una caminata aleatoria y un proceso estacionario de tendencia.  
_a)_ Genere cuatro series que sean paseo aleatorio con deriva, de longitud $n = 100$ con $\delta = {,}01$ y $\sigma_w = 1$. Llame a los datos $x_t$ para $t = 1, \ldots, 100$. Ajuste la regresión $x_t = \beta t + w_t$ usando mínimos cuadrados. Grafique los datos, la función media verdadera (es decir, $\mu t = {,}01 t$) y la línea ajustada, $\hat{x}_t = \hat{\beta} t$, en el mismo gráfico. 
_b)_ Genere cuatro series de longitud $n = 100$ que sean de tendencia lineal más ruido, digamos $y_t = {,}01 t + w_t$, donde $t$ y $w_t$ son como en la parte _a)_. Ajuste la regresión $y_t = \beta t + w_t$ usando mínimos cuadrados. Grafique los datos, la función media verdaderay la línea ajustada.
_c)_ Comente (qué aprendió de esta tarea).

## Capitulo 3.



https://www.youtube.com/playlist?list=PLiv5ffCNWCxMkGQft64hJFuo-I1EWtMqA

https://www.youtube.com/watch?v=emF1iBcni0U&ab_channel=AnaMetriks
https://liclourdescuellar.ruplayers.com/s5NplbrGgJuuoWg/c-mo-importar-datos-de-yahoo-finance-a-rstudio-finanzas-cuantitativas-con-quantmod.html
https://liclourdescuellar.ruplayers.com/
https://www.freecodecamp.org/news/download-trim-mp3-from-youtube-with-python/
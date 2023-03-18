# Tarea de Probabilidades y Estadística: Contraste de Hipotesis

### Marcelo J. Molinatti

## Ejercicios Guiados.

1. Un fabricante diseña un experimento para estimar si la tensión de ruptura media de una fibra es $20$. Para ello, observa las tensiones de ruptura, en libras, de $16$ hilos de dicha fibra seleccionados aleatoriamente.

 _a)_ Si la tensión de ruptura se distribuye según una normal de desviación estándar $0{,}45$.

 _b)_ Si la tensión de ruptura se distribuye según una normal de desviación típica desconocida.

 Las tensiones son $20{,}8$, $20{,}6$, $21{,}0$, $20{,}9$, $19{,}9$, $20{,}2$, $19{,}8$, $19{,}6$, $20{,}9$, $21{,}1$, $20{,}4$, $20{,}6$, $19{,}7$, $19{,}6$, $20{,}3$, y $20{,}7$.


```r
tension_data <- c(20.8, 20.6, 21.0, 20.9, 19.9, 20.2, 19.8, 19.6, 20.9, 21.1, 20.4, 20.6, 19.7, 19.6, 20.3, 20.7)
```

Este ejercicio se trata de comparar la media calculada a partir de una muestra aleatoria de tensiones de ruptura ($n=16$) con respecto a un valor teórico predeterminado de $20$. Es decir, en ambos casos, se busca contrastar las hipótesis siguientes:

$$
\begin{align}
    H_0\text{: }&\mu = 20; \\
    H_1\text{: }&\mu \ne 20;
\end{align}
$$

En este caso, no se especifica ninguna tendencia natural a que la tensión de ruptura sea mayor o menor a este valor por lo que se usa un contraste bilateral. 
Para el inciso _a)_, se tiene que $S = 0{,}45$ y que la distribución subyacente de los datos en normal, por lo que el contraste implica verificar si $\{X_1,\ldots,x_n\} \sim N(20, 0.2025'')$ o si proviene de una población normal con una media distinta. Se calcula el estadístico y la probabilidad asociada a este: 


```r
Z <- (mean(tension_data) - 20) / (.45 / sqrt(16))
p_Z <- 1 - pnorm(Z)

cat("Z =", round(Z, 2), ", p =", sprintf(p_Z, fmt = '%#.4f'), "\n")
```

```
## Z = 3.39 , p = 0.0004
```

```r
cat("CI95%: (", mean(tension_data) - qnorm(.95) * .45 / sqrt(16), ", ", mean(tension_data) + qnorm(.95) * .45 / sqrt(16), ")\n", sep="")
```

```
## CI95%: (20.1962, 20.5663)
```

Se tiene que el valor calculado del estadístico $Z$ es mayor a tres desviaciones estándar con una probabilidad muy pequeña, menor a $\alpha = 0{,}05$. Por lo tanto, se rechaza la hipótesis nula y se concluye que la tensión de ruptura de la fibra es diferente del valor teórico de $20$.

En el inciso _b)_, se indica que la varianza es desconocida, por lo que se puede aplicar una prueba $t$-Student (el uso de una distribución para el estadístico con colas mas pesadas permite tomar en cuenta la incertidumbre añadida debido al desconocimiento de la varianza). Los resultados de la prueba son:


```r
t_test_res <- t.test(tension_data, alternative="two.sided", mu=20)
t_test_res
```

```
## 
## 	One Sample t-test
## 
## data:  tension_data
## t = 2.9154, df = 15, p-value = 0.01066
## alternative hypothesis: true mean is not equal to 20
## 95 percent confidence interval:
##  20.10251 20.65999
## sample estimates:
## mean of x 
##  20.38125
```

Se observa que la hipótesis nula sigue siendo rechazada, dado que $p =$ 0.0106574 es menor al valor de $\alpha = 0{,}05$. Se nota además, que el intervalo de confianza para la media es mas amplio, pero al igual que antes, no incluye el $20$. 

2. En una muestra de $40$ alumnos, $25$ de ellos están conformes con las decisiones que ha tomado el profesor con respecto a las calificaciones. ¿Puede suponerse, con un nivel de significancia del 5%, que la mitad o más de los alumnos están de acuerdo con las calificaciones del profesor?

Si $p$ es la proporción de alumnos conformes con las calificaciones, se busca contrastar las siguientes hipótesis:

$$
\begin{align}
    H_0\text{: }& p \ge 0{,}5; \\
    H_1\text{: }& p < 0{,}5;
\end{align}
$$

Se utiliza una prueba de proporciones para calcular un estadístico que permita concluir con un $95$% de confianza cual de las dos hipótesis mantener. 


```r
N <- 40
n <- 25

prop.test(n, N, .5, alternative="less")
```

```
## 
## 	1-sample proportions test with continuity correction
## 
## data:  n out of N, null probability 0.5
## X-squared = 2.025, df = 1, p-value = 0.9226
## alternative hypothesis: true p is less than 0.5
## 95 percent confidence interval:
##  0.0000000 0.7501004
## sample estimates:
##     p 
## 0.625
```

Los resultados indican que la probabilidad de obtener un valor de $\chi^2$ tan grande como el calculado solo por azar es bastante cercana a $1$, por lo que se mantiene la hipótesis nula con un $95$% de confianza, y se concluye que el 50% o mas de los estudiantes están conformes con la calificación del profesor.

3. Una agencia estatal vigila la calidad del agua para la cría de peces. Esta agencia desea comparar la cantidad media de cierta sustancia tóxica en dos ríos contaminados por desperdicios industriales. Se seleccionaron $11$ muestras en un río y $8$ muestras en el otro. Los resultados de los análisis fueron:


| Rio.1| Rio.2|
|-----:|-----:|
|    10|    11|
|    10|     8|
|    12|     9|
|    13|     7|
|     9|    10|
|     8|     8|
|    12|     8|
|    12|    10|
|    10|      |
|    14|      |
|     8|      |


Si las dos poblaciones son normales e independientes, ¿puede suponerse que la cantidad media de sustancia tóxica presente en ambos ríos es la misma? Considerar un nivel de significación del $5$%.


```r
rio_data <- list(`Rio 1`=c(10, 10, 12, 13, 9, 8, 12, 12, 10, 14, 8),
    `Rio 2`=c(11, 8, 9, 7, 10, 8, 8, 10))
```

De nuevo se supone que ambas poblaciones provienen de una distribución normal, ambas independientes, donde $n_1=$ `11` y $n_2=$ `8`. En este caso se busca evaluar si la diferencia en la cantidad de la sustancia toxica presente en ambos ríos es la misma o no. Se tienen las hipótesis:

$$
\begin{align}
    H_0\text{: }&\mu_1 - \mu_2 = 0; \\
    H_1\text{: }&\mu_1 - \mu_2 \ne 0;
\end{align}
$$

El contraste es bilateral dado que no se dispone de información _a priori_ sobre el estado de los ríos. 
Se aplica una prueba $t$-Student para verificar si la diferencia de medias de $\bar{X_1} - \bar{X_2} =$ 1.8522727 es significativamente distinta de cero o no. 
Para ello, es necesario primero comprobar si las varianzas son iguales o no, utilizando la prueba $F$ para contrastar las hipótesis:

$$
\begin{align}
    H_0\text{: }&\sigma_1 = \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} = 1;\\
    H_1\text{: }&\sigma_1 \ne \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} \ne 1
\end{align}
$$

La prueba se lleva a cabo con un nivel de significancia de $0{,}1$ para poder mantener el nivel de significancia del contraste de medias en $\alpha=0{,}05$. 


```r
var.test(rio_data$`Rio 1`, rio_data$`Rio 2`, ratio=1, "two.sided", conf.level=0.90)
```

```
## 
## 	F test to compare two variances
## 
## data:  rio_data$`Rio 1` and rio_data$`Rio 2`
## F = 2.1846, num df = 10, denom df = 7, p-value = 0.3119
## alternative hypothesis: true ratio of variances is not equal to 1
## 90 percent confidence interval:
##  0.6007504 6.8498698
## sample estimates:
## ratio of variances 
##           2.184643
```

Los resultados muestran que, para un nivel de significancia de $0{,}1$, se puede mantener la hipótesis de varianzas iguales. Por lo tanto, el contraste de medias se puede realizar bajo este supuesto. Los resultados son:


```r
t.test(rio_data$`Rio 1`, rio_data$`Rio 2`, "two.sided", var.equal=TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  rio_data$`Rio 1` and rio_data$`Rio 2`
## t = 2.2564, df = 17, p-value = 0.0375
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.1203596 3.5841858
## sample estimates:
## mean of x mean of y 
##  10.72727   8.87500
```

Los resultados indican que para un $\alpha=0{,}05$, se rechaza la hipótesis nula en favor de la alternativa, y se concluye que la cantidad promedio de la sustancia toxica en el rió 1 es mayor que la encontrada en el rió 2. 

4. Una empresa farmacéutica está interesada en la investigación preliminar de un nuevo medicamento que parece tener propiedades reductoras del colesterol en la sangre. A tal fin se toma una muestra al azar de $6$ personas, y se determina el contenido en colesterol antes y después del tratamiento. Los resultados han sido los siguientes:


| Antes| Despues|
|-----:|-------:|
|   217|     209|
|   252|     241|
|   229|     230|
|   200|     208|
|   209|     206|
|   213|     211|

 Comprobar, a un nivel de significación del $4$% si la aplicación del medicamento es efectiva. Es decir, comprobar si el nivel medio de colesterol en sangre de los pacientes antes de la aplicación del medicamento es mayor o igual al nivel medio de colesterol en sangre después del tratamiento.


```r
colesterol_data <- data.frame(Antes=c(217, 252, 229, 200, 209, 213),
    Despues=c(209, 241, 230, 208, 206, 211))
```

Se trata de dos muestras relacionadas dado que las medidas se hacen sobre los mismos individuos antes y después del tratamiento. Por lo que se buscan contrastar las hipótesis:

$$
\begin{align}
    H_0\text{: }&\mu_{antes} - \mu_{despues} \le 0; \\
    H_1\text{: }&\mu_{antes} - \mu_{despues} > 0
\end{align}
$$

Se realiza una prueba $t$-Student para muestras pareadas, con un nivel de significancia $\alpha=0{,}04$. Para ello, es necesario primero comprobar si las varianzas son iguales o no, utilizando la prueba $F$ para contrastar las hipótesis:

$$
\begin{align}
    H_0\text{: }&\sigma_1 = \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} = 1;\\
    H_1\text{: }&\sigma_1 \ne \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} \ne 1
\end{align}
$$

La prueba se lleva a cabo con un nivel de significancia de $0{,}08$ para poder mantener el nivel de significancia del contraste de medias en $\alpha=0{,}05$. 


```r
var.test(colesterol_data$Antes, colesterol_data$Despues, ratio=1, "two.sided", conf.level=0.92)
```

```
## 
## 	F test to compare two variances
## 
## data:  colesterol_data$Antes and colesterol_data$Despues
## F = 1.6107, num df = 5, denom df = 5, p-value = 0.6137
## alternative hypothesis: true ratio of variances is not equal to 1
## 92 percent confidence interval:
##  0.2843936 9.1225520
## sample estimates:
## ratio of variances 
##           1.610713
```

Los resultados muestran que, para un nivel de significancia de $0{,}08$, se puede mantener la hipótesis de varianzas iguales. Por lo tanto, el contraste de medias se puede realizar bajo este supuesto. Los resultados son:


```r
 t.test(colesterol_data$Antes, colesterol_data$Despues, 
    alternative = "greater", var.equal=TRUE,
    paired = TRUE, conf.level=.96)
```

```
## 
## 	Paired t-test
## 
## data:  colesterol_data$Antes and colesterol_data$Despues
## t = 0.91186, df = 5, p-value = 0.2018
## alternative hypothesis: true mean difference is greater than 0
## 96 percent confidence interval:
##  -3.506849       Inf
## sample estimates:
## mean difference 
##             2.5
```

Como se observa, la probabilidad asociada a un estadístico como el encontrado no permite rechazar la hipótesis nula, por lo que se debe concluir, con un 96% de confianza, que el tratamiento no es efectivo en disminuir los niveles de colesterol en la sangre en los pacientes. 

5. Una determinada empresa quiere saber si su nuevo producto tendrá más aceptación en la población adulta o entre los jóvenes. Para ello, considera una muestra aleatoria de $400$ adultos y $600$ jóvenes, observando que sólo a $100$ adultos y $300$ jóvenes les había gustado su producto. Tomando un nivel de significación del $1$%, ¿puede suponerse que el producto gusta por igual en adultos y jóvenes?

Se busca saber si la proporción de jóvenes, $p_{Jovenes}$ a los cuales les gusto el producto es el mismo que la proporción de adultos, $p_{Adultos}$. Para ello, se busca contrastar las siguientes hipótesis: 

$$
\begin{align}
    H_0\text{: }& p_{Jovenes} - p_{Adultos} = 0; \\
    H_1\text{: }& p_{Jovenes} - p_{Adultos} \ne 0;
\end{align}
$$

Se utiliza una prueba de diferencia de proporciones para verificar si existe o no una diferencia significativa entre estas proporciones con un nivel de significancia $\alpha=0{,}01$.


```r
N <- c(Jovenes=600, Adultos=400)
n <- c(Jovenes=300, Adultos=100)

prop.test(n, N, alternative="two.sided", conf.level=0.99)
```

```
## 
## 	2-sample test for equality of proportions with continuity correction
## 
## data:  n out of N
## X-squared = 61.463, df = 1, p-value = 4.512e-15
## alternative hypothesis: two.sided
## 99 percent confidence interval:
##  0.1712704 0.3287296
## sample estimates:
## prop 1 prop 2 
##   0.50   0.25
```

Los resultados de la prueba muestran que para el nivel de significancia escogido, la diferencia de proporciones de $0{,}25$ es significativamente distinta de cero, lo cual permite concluir con un $99$% de confianza que la proporción de jóvenes a los que les gusto el producto ($p_{Jovenes} = 0{,}5$) es mayor que la proporción de adultos ($p_{Adultos} = 0{,}25$).

## Ejercicios Propuestos

1. Se realiza un experimento para estudiar el nivel (en minutos) que se requiere para que la temperatura del cuerpo de un lagarto del desierto alcance los $45^\circ\text{C}$ partiendo de la temperatura normal de su cuerpo mientras está en la sombra. Se supone que la varianza no es conocida. Se obtuvieron las siguientes observaciones: $10{,}1$; $12{,}5$; $12{,}2$; $10{,}2$; $12{,}8$; $12{,}1$; $11{,}2$; $11{,}4$; $10{,}7$; $14{,}9$; $13{,}9$; y $13{,}3$. Se pide:  

 _a)_ Hallar estimaciones puntuales de la media y la varianza.  

 _b)_ Supóngase que la variable $X$ es el tiempo en alcanzar los $45^\circ\text{C}$ y que se distribuye normalmente: ¿Puede concluirse que el tiempo medio requerido para alcanzar la dosis letal es de $15$ minutos?¿Puede concluirse que el tiempo medio requerido para alcanzar la dosis letal es inferior a $13$ minutos?  


```r
time_data <- c(10.1, 12.5, 12.2, 10.2, 12.8, 12.1, 11.2, 11.4, 10.7, 14.9, 13.9, 13.3)
```

Un estimador de la media es el promedio de los datos el cual se calcula como $\bar{X} = (\sum_kx_k)/n$. El mejor estimador para la varianza se calcula a partir de la sumatoria de cuadrados como $\bar{s^2}(\sum_k(x_k - \bar{X})^2)/(n-1)$. Estos valores se pueden obtener en `R` como:


```r
cat("Media:", mean(time_data), "\n")
```

```
## Media: 12.10833
```

```r
cat("Varianza:", var(time_data), "\n")
```

```
## Varianza: 2.186288
```

Para el contraste de hipótesis, sea $\mu$ el tiempo medio que tarda en alcanzarse la dosis letal. Para la primera pregunta se contrasta las hipótesis:

$$
\begin{align}
    H_0\text{: }&\mu \ge 15; \\
    H_1\text{: }&\mu < 15;
\end{align}
$$

Se elige un contraste unilateral porque sabemos que el promedio muestral es menor a 15, y a que es de interes si el tiempo que se tarda es menor al reportado dado que podría indicar un aumento en la sensibilidad de los lagartos a los cambios de temperatura. Para este contraste, como no se tiene información de la varianza, se utiliza una prueba $t$-Student para comprobar si la media es igual o no al valor teórico de 15, utilizando un $\alpha=0{,}05$ como umbral para rechazar o no la hipótesis nula.


```r
t.test(time_data, alternative="less", mu=15)
```

```
## 
## 	One Sample t-test
## 
## data:  time_data
## t = -6.7746, df = 11, p-value = 1.528e-05
## alternative hypothesis: true mean is less than 15
## 95 percent confidence interval:
##      -Inf 12.87489
## sample estimates:
## mean of x 
##  12.10833
```

Los resultados de la prueba muestran que el valor promedio cae por debajo del valor teórico (dado que el signo del estadístico $t$ es menor a cero, implica que $\mu > \bar{X}$), y que la diferencia es significativa dado que el valor de probabilidad asociado al estadístico es menor al valor de $\alpha$ especificado. Por lo que se concluye efectivamente, con un 95% de confianza, que el tiempo que se tarda en alcanzar la temperatura letal es menor de 15 minutos.  

Para el segundo contraste, se busca saber si este tiempo es menor a 13 minutos, por lo que las hipótesis se plantean como:

$$
\begin{align}
    H_0\text{: }&\mu \ge 13; \\
    H_1\text{: }&\mu < 13
\end{align}
$$

Al igual que antes, se utiliza una prueba $t$-Student, utilizando un $\alpha=0{,}05$:


```r
t.test(time_data, alternative="less", mu=13)
```

```
## 
## 	One Sample t-test
## 
## data:  time_data
## t = -2.089, df = 11, p-value = 0.03037
## alternative hypothesis: true mean is less than 13
## 95 percent confidence interval:
##      -Inf 12.87489
## sample estimates:
## mean of x 
##  12.10833
```

Los resultados muestran que la diferencia sigue siendo negativa y significativa para el nivel de significancia especificado. Lo cual permite concluir con un 95% de confianza que el tiempo que tarda en alcanzarse la temperatura letal es menor a 13 minutos.

2. Se quieren comparar dos poblaciones de ranas pipiens aisladas geográficamente. Para ello se toman dos muestras de ambas poblaciones de tamaño $12$ y $10$ y se les mide la longitud del cuerpo expresado en milímetros.


| Poblacion.1| Poblacion.2|
|-----------:|-----------:|
|        20.1|        25.3|
|        22.5|        31.2|
|        22.2|        22.4|
|        30.2|        23.1|
|        22.8|        26.4|
|        22.1|        28.2|
|        21.2|        21.3|
|        21.4|        31.1|
|        20.7|        26.2|
|        24.9|        21.4|
|        23.9|            |
|        23.3|            |
 
 Contrastar la hipótesis de igualdad de medias a un nivel de significancia del $5$%. (Suponiendo que la longitud se distribuye normalmente).


```r
population_data <- list(
    `Poblacion 1`=c(20.1, 22.5, 22.2, 30.2, 22.8, 22.1, 21.2, 21.4, 20.7, 24.9, 23.9, 23.3), 
    `Poblacion 2`=c(25.3, 31.2, 22.4, 23.1, 26.4, 28.2, 21.3, 31.1, 26.2, 21.4))
```

Se busca saber si la longitud del cuerpo promedio de las ranas de la primera población $\mu_1$ es igual o no a la longitud promedio del cuerpo de las ranas de la segunda población $\mu_2$. Por lo que las hipótesis quedan como:

$$
\begin{align}
    H_0\text{: }&\mu_1 - \mu_2 = 0; \\
    H_1\text{: }&\mu_1 - \mu_2 \ne 0;
\end{align}
$$

Para contrastar estas hipótesis se hace uso de una prueba $t$-Student para comparar las longitudes promedios de los cuerpos de las ranas en ambas poblaciones. Se especifica de igual forma a un nivel de significancia $\alpha=0{,}05$. Para ello, es necesario primero comprobar si las varianzas son iguales o no, utilizando la prueba $F$ para contrastar las hipótesis:

$$
\begin{align}
    H_0\text{: }&\sigma_1 = \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} = 1;\\
    H_1\text{: }&\sigma_1 \ne \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} \ne 1
\end{align}
$$

La prueba se lleva a cabo con un nivel de significancia de $0{,}1$ para poder mantener el nivel de significancia del contraste de medias en $\alpha=0{,}05$. 


```r
var.test(population_data$`Poblacion 1`, population_data$`Poblacion 2`, ratio=1, "two.sided")
```

```
## 
## 	F test to compare two variances
## 
## data:  population_data$`Poblacion 1` and population_data$`Poblacion 2`
## F = 0.51989, num df = 11, denom df = 9, p-value = 0.3043
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.1328934 1.8653086
## sample estimates:
## ratio of variances 
##          0.5198889
```

Los resultados muestran que, para un nivel de significancia de $0{,}1$, se puede mantener la hipótesis de varianzas iguales. Por lo tanto, el contraste de medias se puede realizar bajo este supuesto. Los resultados son:


```r
t.test(population_data$`Poblacion 1`, population_data$`Poblacion 2`, "two.sided", var.equal=TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  population_data$`Poblacion 1` and population_data$`Poblacion 2`
## t = -2.0097, df = 20, p-value = 0.05815
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -5.5398671  0.1032004
## sample estimates:
## mean of x mean of y 
##  22.94167  25.66000
```

Los resultados indican que para un $\alpha=0{,}05$, se debe mantener la hipótesis nula en favor de la alternativa (aunque se nota que el valor de probabilidad asociado es apenas marginal), y se concluye que la longitud promedio de las ranas en ambas poblaciones es igual, con un nivel de confianza del 95%.

3. Se realiza un estudio, en el que participan $10$ individuos, para investigar el efecto del ejercicio físico en el nivel de colesterol en plasma. Antes del ejercicio se tomaron muestras de sangre para determinar el nivel de colesterol de cada individuo. Después, los participantes fueron sometidos a un programa de ejercicios. Al final de los ejercicios se tomaron nuevamente muestras de sangre y se obtuvo una segunda lectura del nivel de colesterol. Los resultados se muestran a continuación.


| Antes| Despues|
|-----:|-------:|
|   182|     190|
|   230|     220|
|   160|     166|
|   200|     150|
|   160|     140|
|   240|     220|
|   260|     156|
|   480|     312|
|   263|     240|
|   240|     250|

 Se quiere saber si el ejercicio físico ha reducido el nivel de colesterol para un nivel de confianza del 95%.


```r
cholesterol_data <- data.frame(
    Antes=c(182, 230, 160, 200, 160, 240, 260, 480, 263, 240),
    Despues=c(190, 220, 166, 150, 140, 220, 156, 312, 240, 250))
```

Se trata de dos muestras relacionadas dado que las medidas se hacen sobre los mismos individuos antes y después del programa de ejercicios. Por lo que se buscan contrastar las hipótesis:

$$
\begin{align}
    H_0\text{: }&\mu_{antes} - \mu_{despues} \le 0; \\
    H_1\text{: }&\mu_{antes} - \mu_{despues} > 0
\end{align}
$$

Se realiza una prueba $t$-Student para muestras pareadas, con un nivel de significancia $\alpha=0{,}05$. Para ello, es necesario primero comprobar si las varianzas son iguales o no, utilizando la prueba $F$ para contrastar las hipótesis:

$$
\begin{align}
    H_0\text{: }&\sigma_1 = \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} = 1;\\
    H_1\text{: }&\sigma_1 \ne \sigma_2 \Rightarrow \frac{\sigma_1}{\sigma_2} \ne 1
\end{align}
$$

La prueba se lleva a cabo con un nivel de significancia de $0{,}1$ para poder mantener el nivel de significancia del contraste de medias en $\alpha=0{,}05$. 


```r
var.test(cholesterol_data$Antes, cholesterol_data$Despues, ratio=1, "two.sided", conf.level=0.90)
```

```
## 
## 	F test to compare two variances
## 
## data:  cholesterol_data$Antes and cholesterol_data$Despues
## F = 2.8773, num df = 9, denom df = 9, p-value = 0.1313
## alternative hypothesis: true ratio of variances is not equal to 1
## 90 percent confidence interval:
##  0.905127 9.146635
## sample estimates:
## ratio of variances 
##           2.877302
```

Los resultados muestran que, para un nivel de significancia de $0{,}1$, se puede mantener la hipótesis de varianzas iguales. Por lo tanto, el contraste de medias se puede realizar bajo este supuesto. Los resultados son:


```r
 t.test(cholesterol_data$Antes, cholesterol_data$Despues, 
    alternative = "greater", var.equal=TRUE,
    paired = TRUE, conf.level=.95)
```

```
## 
## 	Paired t-test
## 
## data:  cholesterol_data$Antes and cholesterol_data$Despues
## t = 2.0525, df = 9, p-value = 0.03516
## alternative hypothesis: true mean difference is greater than 0
## 95 percent confidence interval:
##  3.965698      Inf
## sample estimates:
## mean difference 
##            37.1
```

Como se observa, la probabilidad asociada a un estadístico como el encontrado permite rechazar la hipótesis nula, por lo que se debe concluir, con un 95% de confianza, que el programa de entrenamiento si disminuye los niveles de colesterol en plasma en los individuos. 

4. Se ignora la proporción de familias numerosas y con el fin de determinar dicha proporción se toma una muestra de $800$ familias siendo la proporción observada de $0{,}18$. ¿Se puede afirmar que la proporción de familias numerosas es $0{,}20$?.

Si $p$ es la proporción de familias numerosas, se busca contrastar las siguientes hipótesis:

$$
\begin{align}
    H_0\text{: }& p = 0{,}2; \\
    H_1\text{: }& p \ne 0{,}2;
\end{align}
$$

Se utiliza una prueba de proporciones para calcular un estadístico que permita concluir con un $95$% de confianza cual de las dos hipótesis mantener. 


```r
N <- 800
n <- 0.18 * 800

res <- prop.test(n, N, .2, alternative="two.sided")
res
```

```
## 
## 	1-sample proportions test with continuity correction
## 
## data:  n out of N, null probability 0.2
## X-squared = 1.877, df = 1, p-value = 0.1707
## alternative hypothesis: true p is not equal to 0.2
## 95 percent confidence interval:
##  0.1543404 0.2087896
## sample estimates:
##    p 
## 0.18
```

Los resultados indican que la probabilidad de obtener un valor de $\chi^2$ tan grande como el calculado solo por azar es 0.1707, por lo que se mantiene la hipótesis nula con un $95$% de confianza, y se concluye que es posible afirmar que la proporción de familias numerosas es $0{,}20$

5. Se sospecha que añadiendo al tratamiento habitual para la curación de una enfermedad un medicamento A, se consigue mayor número de pacientes curados. Tomamos dos grupos de enfermos de $100$ individuos cada uno. A un grupo se le suministra el medicamento $A$ y se curan $60$ enfermos y al otro no se le suministra, curándose $55$ enfermos. ¿Es efectivo el tratamiento A en la curación de la enfermedad?

Se busca saber si la proporción de individuos curados por el tratamiento A, $p_{A}$ es mayor que la proporción obtenida en el grupo control, $p_{control}$. Para ello, se busca contrastar las siguientes hipótesis: 

$$
\begin{align}
    H_0\text{: }& p_{A} - p_{control} \le 0; \\
    H_1\text{: }& p_{A} - p_{control} > 0;
\end{align}
$$

Se utiliza una prueba de diferencia de proporciones para verificar si existe o no una diferencia significativa entre estas proporciones con un nivel de significancia $\alpha=0{,}05$.


```r
N <- c(A=100, Control=100)
n <- c(A=60, Control=55)

prop.test(n, N, alternative="greater", conf.level=0.95)
```

```
## 
## 	2-sample test for equality of proportions with continuity correction
## 
## data:  n out of N
## X-squared = 0.32737, df = 1, p-value = 0.2836
## alternative hypothesis: greater
## 95 percent confidence interval:
##  -0.07484565  1.00000000
## sample estimates:
## prop 1 prop 2 
##   0.60   0.55
```

Los resultados de la prueba muestran que para el nivel de significancia escogido, la diferencia de proporciones de 0.05 no es significativamente distinta de cero, lo cual permite concluir con un 95% de confianza que la proporción de pacientes tratados con el medicamento A que se curaron no es mayor que la proporción obtenida para el tratamiento control.

6. En 5 zonas de la provincia de Granada (Ladihonda y Fazares, zonas muy secas y Cortijuela, Molinillo y Fardes, zonas húmedas) se hacen una serie de mediciones sobre las hojas de las encinas a lo largo de 3 años consecutivos: 1995, muy seco; y, 1996 y 1997, muy lluviosos. 

 El objetivo es medir la simetría fluctuante en dichas hojas como indicador de estrés en la planta. Bajo condiciones de estrés (sequía, herbivoría, limitación por nutrientes, entre otras), la hipótesis es que la asimetría aumente. Contamos con la siguiente información: 
 
 * Localización de árboles: 5 zonas, dos en zonas muy secas (Hoya Guadix-Baza, Ladihonda y Fazares) y tres en zonas con mayor precipitación (Cortijuela, Molinillo, Fardes). En esta última, Fardes, son árboles situados en la ladera de un río (presumiblemente poco afectados por años más o menos secos). 
 * Años de climatología diferente: 1995 año muy seco y años 1996 y 1997, años muy lluviosos. 
 * Situación de la hoja: _Canopy_ (copa de los árboles) y _Sprouts_ (rebrotes, hojas nuevas que salen desde la parte inferior del tronco). 

 Disponemos de un total de 2101 casos, cedidos por el Departamento de Ecología de la Universidad de Granada (España), de los que hemos seleccionado aleatoriamente una muestra de tamaño 15 que se presenta en la siguiente tabla: 
 

```r
 tree_data <- data.frame(
      Zona=c("Cortijuela", "Cortijuela", "Molinillo", "Molinillo", "Molinillo", "Fardes", "Fardes", 'Ladihonda', "Ladihonda", "Fazares", "Fazares", "Fazares", "Cortijuela", "Fazares", "Fazares"),
      Parte=c("Canopy", "Canopy", 'Canopy', "Canopy", "Canopy", "Canopy", 'Canopy', 'Canopy', 'Canopy', 'Canopy', "Canopy", "Canopy", "Sprouts", 'Sprouts', 'Sprouts'),
      Anho=c(1995, 1996, 1995, 1996, 1996, 1995, 1996, 1995, 1996, 1995, 1996, 1996, 1995, 1995, 1996),
      Longitud=c(26.51, 30.17, 34.24, 31.04, 34.99, 30.48, 25.07, 25.04, 29.16, 35.12, 25.41, 27.02, 23.04, 27.69, 34.71),
      Asimetria=c(0.028, 0.010, 0.080, 0.340, 0.087, 0.040, 0.010, 0.021, 0.135, 0.010, 0.094, 0.153, 0.156, 0.172, 0.077))

knitr::kable(tree_data)
```



|Zona       |Parte   | Anho| Longitud| Asimetria|
|:----------|:-------|----:|--------:|---------:|
|Cortijuela |Canopy  | 1995|    26.51|     0.028|
|Cortijuela |Canopy  | 1996|    30.17|     0.010|
|Molinillo  |Canopy  | 1995|    34.24|     0.080|
|Molinillo  |Canopy  | 1996|    31.04|     0.340|
|Molinillo  |Canopy  | 1996|    34.99|     0.087|
|Fardes     |Canopy  | 1995|    30.48|     0.040|
|Fardes     |Canopy  | 1996|    25.07|     0.010|
|Ladihonda  |Canopy  | 1995|    25.04|     0.021|
|Ladihonda  |Canopy  | 1996|    29.16|     0.135|
|Fazares    |Canopy  | 1995|    35.12|     0.010|
|Fazares    |Canopy  | 1996|    25.41|     0.094|
|Fazares    |Canopy  | 1996|    27.02|     0.153|
|Cortijuela |Sprouts | 1995|    23.04|     0.156|
|Fazares    |Sprouts | 1995|    27.69|     0.172|
|Fazares    |Sprouts | 1996|    34.71|     0.077|
 
 Se pide: 
 * ¿Se puede admitir que la longitud de las hojas de encina se distribuye normalmente? 
 * ¿Se puede admitir que la longitud media de las hojas es igual a 30 cm a un nivel de significancia del 5%? 
 * Suponiendo que la asimetría de las hojas sigan una distribución Normal; comprobar mediante un contraste de hipótesis si existen diferencias significativas en la asimetría de las hojas teniendo en cuenta la situación de la hoja en el árbol. 
 * A un nivel de significancia del 5%, ¿es representativo el ajuste lineal entre la longitud y la asimetría? ¿Cuál sería la expresión del modelo? ¿Cuánto explica el modelo? 

Para saber si la distribución de la longitud de las hojas sigue una distribución normal se utiliza una prueba de Shapiro-Wilks (no es posible usar la prueba de Kolmogorov-Smirnov debido a que los parámetros poblacionales son desconocidos). La prueba contrasta las hipotesis:

$$
\begin{align}
    H_0\text{: }& F(x) = \Phi(x) \\
    H_1\text{: }& F(x) \ne \Phi(x)
\end{align}
$$

El contraste se hace con un nivel de significancia de $\alpha = 0{,}05$, y los resultados son los siguientes:


```r
shapiro.test(tree_data$Longitud)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  tree_data$Longitud
## W = 0.92244, p-value = 0.2098
```

Se observa que la hipótesis nula se puede mantener con un nivel de confianza del 95% dado que la probabilidad asociada al estadístico calculado es mayor al $\alpha$ especificado. Para verificar estos resultados se puede observar un _q-q plot_:


```r
library(ggplot2, quietly=TRUE)

ggplot(tree_data, aes(sample=Longitud)) +
  stat_qq_line(colour = "deepskyblue4", size=1.5) + 
  stat_qq() + 
  theme_light(base_size=10) +
  xlab("Distribución teórica (Normal estandar)") + 
  theme(panel.grid=element_blank())
```

![](Tarea-2-Prob-Stat-2022_files/figure-html/e02-6-2-1.png)<!-- -->

El gráfico muestra que dentro de $\pm 1$ desviación estándar, la distribución se ajusta bien a una normal, pero mas allá de eso, los puntos se desvían mas de esta distribución teórica.

Ahora, suponiendo que el supuesto de normalidad es posible mantenerlo (basándonos en los resultados de la prueba), podemos contrastar las siguientes hipótesis sobre la longitud media de las hojas:

$$
\begin{align}
    H_0\text{: }&\mu = 30; \\
    H_1\text{: }&\mu \ne 30;
\end{align}
$$

En este caso, no se especifica ninguna tendencia natural a que la tensión de ruptura sea mayor o menor a este valor por lo que se usa un contraste bilateral. Dado que la varianza es desconocida, se puede aplicar una prueba $t$-Student. Los resultados de la prueba son:


```r
t_test_res <- t.test(tree_data$Longitud, alternative="two.sided", mu=30)
t_test_res
```

```
## 
## 	One Sample t-test
## 
## data:  tree_data$Longitud
## t = -0.65528, df = 14, p-value = 0.5229
## alternative hypothesis: true mean is not equal to 30
## 95 percent confidence interval:
##  27.06296 31.56238
## sample estimates:
## mean of x 
##  29.31267
```

Dado que $p =$ 0.5229116 es mayor al valor de $\alpha = 0{,}05$, por lo que se debe mantener la hipotesis nula, y se concluye con un nivel de confianza del 95%, que la longitud promedio de las hojas es 30 cm.

Ahora, se supone que ambas poblaciones de _Canopy_ y _Sprouts_ provienen de una distribución normal, ambas independientes, donde $n_{Canoopy}=$ `12` y $n_{Sprouts}=$ `3`. En este caso se busca evaluar si la diferencia en asimetría de las hojas de ambas partes de hojas presente en los arboles es la misma o no. Se tienen las hipótesis:

$$
\begin{align}
    H_0\text{: }&\mu_{Canopy} - \mu_{Sprouts} = 0; \\
    H_1\text{: }&\mu_{Canopy} - \mu_{Sprouts} \ne 0;
\end{align}
$$

El contraste es bilateral dado que no se dispone de información _a priori_ sobre la simetría de las hojas en ambas partes de los arboles. 
Se aplica una prueba $t$-Student para verificar si la diferencia de medias de $\bar{X_{Canopy}} - \bar{X_{Sprouts}} =$ -0.051 es significativamente distinta de cero o no. 
Para ello, es necesario primero comprobar si las varianzas son iguales o no, utilizando la prueba $F$ para contrastar las hipótesis:

$$
\begin{align}
    H_0\text{: }&\sigma_{Canopy} = \sigma_{Sprouts} \Rightarrow \frac{\sigma_{Canopy}}{\sigma_{Sprouts}} = 1;\\
    H_1\text{: }&\sigma_{Canopy} \ne \sigma_{Sprouts} \Rightarrow \frac{\sigma_{Canopy}}{\sigma_{Sprouts}} \ne 1
\end{align}
$$

La prueba se lleva a cabo con un nivel de significancia de $0{,}1$ para poder mantener el nivel de significancia del contraste de medias en $\alpha=0{,}05$. 


```r
var.test(
    tree_data$Asimetria[tree_data$Parte == "Canopy"], 
    tree_data$Asimetria[tree_data$Parte == "Sprouts"], 
    ratio=1, "two.sided", conf.level=0.90)
```

```
## 
## 	F test to compare two variances
## 
## data:  tree_data$Asimetria[tree_data$Parte == "Canopy"] and tree_data$Asimetria[tree_data$Parte == "Sprouts"]
## F = 3.4611, num df = 11, denom df = 2, p-value = 0.4908
## alternative hypothesis: true ratio of variances is not equal to 1
## 90 percent confidence interval:
##   0.1783607 13.7830583
## sample estimates:
## ratio of variances 
##           3.461082
```

Los resultados muestran que, para un nivel de significancia de $0{,}1$, se puede mantener la hipótesis de varianzas iguales. Por lo tanto, el contraste de medias se puede realizar bajo este supuesto. Los resultados son:


```r
t.test(
    tree_data$Asimetria[tree_data$Parte == "Canopy"], 
    tree_data$Asimetria[tree_data$Parte == "Sprouts"], 
    "two.sided", var.equal=TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  tree_data$Asimetria[tree_data$Parte == "Canopy"] and tree_data$Asimetria[tree_data$Parte == "Sprouts"]
## t = -0.88477, df = 13, p-value = 0.3924
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.1755285  0.0735285
## sample estimates:
## mean of x mean of y 
##     0.084     0.135
```

Los resultados indican que para un $\alpha=0{,}05$, se mantiene la hipótesis nula, y se concluye que la asimetría de las hojas es la misma, con un 95% de confianza. 

Ahora, para la regresión lineal entre la asimetría de las hojas y la longitud de la hoja se procede de la siguiente forma:


```r
mod <- lm(Asimetria ~ Longitud, tree_data)
summary(mod)
```

```
## 
## Call:
## lm(formula = Asimetria ~ Longitud, data = tree_data)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.087912 -0.072795 -0.009889  0.048489  0.247311 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.119847   0.178754   0.670    0.514
## Longitud    -0.000875   0.006044  -0.145    0.887
## 
## Residual standard error: 0.09187 on 13 degrees of freedom
## Multiple R-squared:  0.001609,	Adjusted R-squared:  -0.07519 
## F-statistic: 0.02096 on 1 and 13 DF,  p-value: 0.8871
```

Los resultados muestran que las variables Asimetría y Longitud no muestran una relación lineal dado que el valor de la prueba $F=0{,}01106$ para análisis de varianza arroja un valor de probabilidad de $p=0{,}9178$, el cual no es significativo para $\alpha=0{,}05$. De igual forma, los coeficiente estimados no son significativos para un valor de significancia igual al anterior, indicando que la pendiente y el intercepto no son diferentes de cero. El $R^2=0{,}00085$ muestra que apenas el $0{,}085$% de la varianza es explicada por el modelo, lo cual no representa ni el 1% de la varianza total, lo cual es un indicativo de la falta de asociación de las variables. 

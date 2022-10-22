---
title: "Tarea 1"
author: "José Eduardo Téllez"
date: "26/2/2021"
output: html_document
runtime: shiny
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=FALSE, warning=FALSE, include=FALSE}
library(readxl)
library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library("viridis")
pizza.data <- read.csv("pizza_delivery_tarea.csv")
library(DT)
attach(pizza.data)
library(shiny)
library(RColorBrewer)
library(kableExtra)
```



```{r, echo=FALSE, warning=FALSE, include=FALSE}
cols <- brewer.pal(9, "BuGn")
cols <- cols[3:9]
cols
```



# Tarea 1

## Análisis Exploratorio
\

Buscamos valores ausentes en nuestra información, para esto ocupamos
sapply para recorrer cada columna y sumar cada valor que sea na.

```{r, echo=FALSE}
kbl(t(sapply(pizza.data, function(x) sum(is.na(x))))) %>% kable_styling(bootstrap_options = c("striped", "hover"),full_width = F)
```

\
Observamos que las variables con valores faltantes son:
  
  *  day: Dado la variable day esta realcionada con la variable date podemos conocer el valor.
*  time: Escogemos el valor apartir de un muestreo aleatorio con respecto a los demás datos que tenemos de la variable time.
*  branch: Escogemos el valor apartir de un muestreo aleatorio con respecto a los demás datos que tenemos de la variable branch
*  free_wine: Como sabemos que la orden recibio Vino podemos definir para este caso el valor de 1

\ 


```{r, echo=FALSE}
pizza.data$day[10]<-"Thursday"
pizza.data$time[6]<-sample(pizza.data$time[-6],1)
pizza.data$branch[15]<-sample(pizza.data$branch[-15],1)
pizza.data$free_wine[7]<-1
```


## 1. ¿Tipo de cada una de las variables? Utiliza class() en cada variable

\



```{r clases, echo=FALSE}
clases_func <- function(){
  class.data <- c()
  for (i in 1:length(pizza.data[1,])) {
    class.data[i] <- class(pizza.data[,i])
    
  }
  data.frame.class <- data.frame(names(pizza.data),class.data)
  
}
clases_data <- clases_func()
kbl(t(clases_data)) %>%  kable_styling(bootstrap_options = c("striped", "hover"),full_width = F)
```

\

## 2. Transforma la variable date en tipo date (si no está en ese tipo). Explica cómo se hace.

De la tabla anterior vemos que la variable Date es un charater, por lo que utilizaremos la función
as.Date para convertirla, ocuparemos el formato $dd-mes-yy"
  
\
  
  
  
```{r}
pizza.data$date <- as.Date(pizza.data$date, format='%d-b%-%y')
class(pizza.data$date)
```

\


## 3. Si tienes alguna variable lógica, transfórmala a numérica y verifica que tenga sentido.
La variable got_wine es un valor logico, ya que toma valores entre 0 y 1. Tiene sentido ya 
que es solo podemos tomar dos resultados, sí o no.


\


```{r}
pizza.data$got_wine <- as.numeric(pizza.data$got_wine)
```

Una vez transformadas todas la variables revisamos que hayamos transfomrado los valores correctamente.

### Tabla de clases

\




```{r, echo=FALSE}
DT::renderDT({
  clases_func() %>%  DT::datatable(colnames = c("Variables","Clases"),
                                  rownames = FALSE, filter = "top",
                                  options = list(pageLenght = 13, autoWidth= TRUE,
                                                 lengthChange= FALSE,lengthMenu = list(c(15, -1), c("15", "All")))) %>% 
                                  formatStyle('names.pizza.data.',target = 'row',
                                              backgroundColor = styleEqual(c("date", "got_wine"), c('yellow', 'yellow')))
})
```



\

## 4. Crea una nueva variable que se llame quejas: variable indicadora de si hubo una queja o no. Se presentauna queja cuando la temperatura de la pizza en la entrega es menor a los 60ªC o si tardó más de 40 min y no se entregó la botella de vino.

\



\

```{r}
pizza.data <- mutate(pizza.data, 
                     quejas = ifelse(pizza.data$temperature < 60 | (pizza.data$time>40 & pizza.data$got_wine==0),T,F)) 
```




## 5, 6, 7  ------------------------------------------------------------

\

```{r eruptions, echo=FALSE}
shinyAppDir(
  "Boxplot_Time/", 
  options = list(
    width = "100%", height = 550
  )
)
```

### 5. ¿Existen diferencias significativas en el tiempo de entrega dependiendo del operador que tomó el pedido?
Observamos que con respecto al tiempo del operador no econtramos diferencias alguna, dado que el tiempo de entrega por Melissa y Laura esta distribuido de igual manera. 

### 6. ¿Existen diferencias significativas en la temperatura de entrega dependiendo del repartidor?

En el caso de los repartidores si encontramos diferencias: Vemos que Bruno es el que tiene el tiempo medio más grande de 35.82, mientras que Dominico tiene el tiempo medio más corto con 28.97; También se observa que Luigi es el que tiene la menor variabilidad en sus entregas. Mientras que Mario y Salvatore tienen un comportamiento muy parecido con un tiempo medio de 34.
También el comportamiento de Dominico se deba a que en mayoría sus entregas corrresponden a una orden con menor cantidad de pizzas ordenas, en cambio Bruno y Salvatore tiene más entregas con un número mayor de pizzas ordenadas.


7. ¿Existen diferencias significativas en el tiempo de entrega dependiendo de la sucursal? Justifica
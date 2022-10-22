---
  title: "Examen dos"
author: "José Eduardo Téllez"
date: "10/5/2021"
output: 
  rmdformats::readthedown:
  theme: bookdown 
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, warning=FALSE, message = FALSE}
library(readxl)
library(dplyr)
library(tidyverse)
library(MASS)
library(lmvar)
library(stats)
library(Matrix)
library(glmnet)
library(caret)
library(ggplot2)
library(lattice)
library(RColorBrewer)
library(purrr)
library(rattle)
library(gbm)
set.seed(157734)
```


Las variables que se tienen registradas son:
  
  Auto: Tipo de auto
Edad: Edad del conductor principal
Sexo: Indicador de Hombre o Mujer
Ingreso: Ingreso mensual promedio
CDMX: Indicadora de si vive en la CDMX
KMS: Número promedio de kms. recorridos diariamente
Monto: Monto total reclamado por concepto de seguro durante el año inmediato anterior a la solicitud de la cotización.
Seguro: Indicadora de si compró el seguro o no en el presente año.

Se te pide hacer lo siguiente:
  
  1. Descarga la base de datos que te tocó (Estará disponible en el horario del examen: Lunes 10 de mayo, 10:00).


```{r}
datos <- read.csv("base_datos_5.csv")
summary(datos)
```


Para poder trabajar con la variable Auto, Sexo, Seguro y CDMX debemos convertirlas en factores.



```{r}
datos <- datos %>% mutate(Auto = as.factor(Auto))
datos <- datos %>% mutate(Sexo = as.factor(Sexo))
datos <- datos %>% mutate(CDMX = as.factor(CDMX))
datos <- datos %>% mutate(Seguro = as.factor(Seguro))
datos <- datos[2:9]
head(datos)
```


Revisando las clases de nuestras variables
```{r}
lapply(datos, class)
```



Crearemos una base datos uno para poder contestar la pregunta dos, a la vez, partiremos esta base en 
entrenamiento (.9) y prueba (.1).
De igual manera crearemos una segunda base para la pregunta tres. Similar, partiremos la base en entrenamiento y 
prueba



```{r}
datos1 <- datos %>% dplyr::select(Monto, Auto, Edad, Sexo, Ingreso, CDMX, KMS)
ind <- 1:dim(datos)[1]
ind.entr <- sample(ind, dim(datos)[1]*.9)
ind.test <- sample(ind, dim(datos)[1]*.1)
data.entre <- datos1[ind.entr,]
data.test <- datos1[ind.test,]
data.entre2 <- datos[ind.entr,]
data.test2 <- datos[ind.test,]
```

# Pregunta 2

2. Se quiere saber si se puede explicar la variable Monto en términos de las variables Auto, Edad, Sexo, Ingreso, CDMX, KMS. Para ello, considera una partición de los datos en entrenamiento y prueba (90%/10%) y ajusta los siguientes modelos considerando validación cruzada con 10 particiones para estimar el error.

Modelo de regresión lineal
Modelo lineal generalizado
Modelo de árbol de regresión
Modelo de bosque aleatorio
Modelo de boosting  
Escoge una medida adecuada y determina cuál es el modelo que mejor ajusta los datos de entrenamiento y cuál es el modelo que tiene mejor poder predictivo en los datos de prueba.



## Regresión Lineal


Se hara un Modelo con todas las variables para observar su comportamiento
```{r}
# Queremos Ajustar un Modelo para la varibale Monto. 
lm1 <- lm(Monto~., datos1)
summary(lm1)
```

Vemos que las variables Autos y Sexo son significativas distintas de cero, por lo que podemos desechar el 
resto. Observando el R^2 de 0.4315 es muy bajo. Buscameros un mejor modelo para el ajuste.
Utilizaremos los dos procesos: backward y forward.

```{r}
lm2 <- lm(Monto~., datos)
lm0 <- lm(Monto~ 1, datos)
lm4 <- stepAIC(lm0,scope = list(lower = lm0, upper =lm2), direction = "forward")
lm3 <- stepAIC(lm1 , scope =list(lower =lm0, upper = lm2), directio = "backward")
lm4
```

Para los dos métodos resulta con el modelo 
$$ Monto \sim Auto + Sexo$$
  con un AIC de 1864.47.


Por lo que compararemos estos dos modelos por medio de la R^2, AIC y BIC

```{r}
### comparando los modelos
summary(lm1)$r.squared
summary(lm4)$r.squared
## el modelo 4 tiene un mejor R squared
AIC(lm1)
AIC(lm4)
# El modelo 4 de igual manera tiene un AIC menor
BIC(lm1)
BIC(lm4)
```

El primer modelo tiene una mejor R^2, pero el segundo modelo gana en medición de AIC y BIC.
Por lo que escogemos el modelo 2 de ajuste.

### Predicción
Buscando el mejor enfoque predictivo, ocuparemos la base de entrenamiento y validación cruzada con la 
funcion cv.lm. 
repetimos un proceso similar solo que con la partición de entrenamiento


```{r}
lm1 <- lm(Monto~., data.entre, x= TRUE, y= TRUE)
summary(lm1)
lm2 <- lm(Monto~., data.entre, x= TRUE, y= TRUE)
lm0 <- lm(Monto~ 1, data.entre, x= TRUE, y= TRUE)
summary(lm0)
lm4 <- stepAIC(lm0,scope = list(lower = lm0, upper =lm2), direction = "forward")
lm3 <- stepAIC(lm1 , scope =list(lower =lm0, upper = lm2), directio = "backward")
```

Ocupando la función cv.lm buscamos el modelo con el menor Root mean squared error.

```{r}
cv.lm(lm1)
cv.lm(lm4)
## se escogje el modelo 3
```
para este caso es el modelo lm4 es el que escogeremos


```{r}
lm4
```

Una vez escogido el modelo calculamos el error cuadrtatico medio usando la partición de test

```{r}
Errorlm <- mean((data.test$Monto-predict(lm4,newdata = data.test))^2)
sqrt(Errorlm)
```



## Modelo generalizado

Se ajusatara un modelo generalizado.
La variable Momnto se encuentra entre los positivos, por lo que se escogera una función Gamma.
Repetimos el proceso para esocger el mejor modelo

```{r}
glm0 <- glm(Monto~. , data.entre, family = Gamma())
glm2 <- glm(Monto~1 , data.entre, family = Gamma())
glm4 <- stepAIC(glm0,scope = list(lower = glm2, upper =glm0), direction = "backward")
glm3 <- stepAIC(glm2, scope = list(lower = glm2, upper = glm0), direction = "forward")
```

El mejor modelo con respecto al AIC es el modelo 3.
Haciendo la prueba chi, vemos que la probabilidad es muy pequeña por lo que es muy difícil encontrar 
un valor D mayor.

```{r}
anova(glm3,test = "Chisq")
pchisq(8.3315,1, lower.tail = FALSE)
```


### Predicción


Para la predicción y validación cruzada ocuparemos la función glmnet.
Ocupamos la regularización Ridge, Lasso y Elasticnet para encontrar la
lambda minima.

```{r}
x <- model.matrix(Monto~.,family =  "gaussian", data = data.entre)[,-1]
y <- data.entre$Monto
Ridge <- cv.glmnet(x,y,alpha = 0)
El25 <- cv.glmnet(x,y,alpha = .25)
El5 <- cv.glmnet(x,y,alpha = .5)
El75 <- cv.glmnet(x,y,alpha = .75)
Lasso <- cv.glmnet(x,y,alpha = 1)
```

Estas son las lambdas minimias que obtuvimos.

```{r}
Rid <- Ridge$lambda.min
E25 <- El25$lambda.min
E5 <- El5$lambda.min
E75 <- El75$lambda.min
La <- Lasso$lambda.min
Rid
E25
E5
E75
La
```
Vemos
```{r}
x1 <- model.matrix(Monto~.,family= "gaussian",data = data.test)
y1 <- as.numeric(data.test$Monto)
glm1.1 <- predict(Ridge, s = Rid, newx = x)
glm1.2<- predict(El25, s = E25, newx = x)
glm1.3 <- predict(El5, s = E5, newx = x)
glm1.4 <- predict(El75, s = E75, newx = x)
glm1.5 <- predict(Lasso, s = La, newx = x)
error1.1 <- mean((y - glm1.1)^2)
error1.2 <- mean((y - glm1.2)^2)
error1.3 <- mean((y - glm1.3)^2)
error1.4 <- mean((y - glm1.4)^2)
error1.5 <- mean((y - glm1.5)^2)
sqrt(error1.1)
sqrt(error1.2)
sqrt(error1.3)
sqrt(error1.4)
sqrt(error1.5)
```

VEmos que el modelo número 1 es el que tiene el menor error cuadratcio medio.
Escojemos el modelo 1.



## Modelo de Arbol


Crearemos un Arbol con todas la variables.
```{r}
library(rpart)
library(rpart.plot)
par(mfrow=c(1,1))
tree.ent <- rpart(Monto ~ Auto + Edad + Sexo + Ingreso + CDMX + KMS, data=data.entre,
                  control = rpart.control(minbucket = 5, maxdepth = 10, xval = 10, cp = 0.000001),
                  method = "anova")
rpart.plot(tree.ent)
```


Se observa que la Variable Auto es de duma importancia, al igual que en los modelos 
lineales y generalizados.

```{r}
tree.ent$variable.importance
```



Se encuentra el alpha óptimo

```{r}
printcp(tree.ent)
plotcp(tree.ent)
```



Se podara el arbol.

```{r}
tree.prune <- prune(tree.ent, tree.ent$cptable[which.min(tree.ent$cptable[,"xerror"]),"CP"])
tree.prune
fancyRpartPlot(tree.prune, uniform=TRUE, main = "Pruned Classification Tree")
```

Y encontramos el valor del Error cuadratico.

```{r}
predict(tree.prune,data.test)
sqrt(sum((data.test$Monto-predict(tree.prune,data.test))^2))
```



## Modelo de bosques

Para modelo  
```{r, warning=FALSE, message=FALSE}
library(ISLR)
library(randomForest)
t<-proc.time()
model.rf <- randomForest(Monto ~ Auto+ Edad + Sexo + Ingreso + CDMX + KMS , 
                         data = data.entre,
                         ntree = 1000, 
                         mtry = 3,
                         sampsize = floor(0.6 * nrow(data.entre)), 
                         nodesize = 100, 
                         importance = TRUE
)
```


```{r}
model.rf
```

```{r}
proc.time()-t
importance(model.rf)
varImpPlot(model.rf)
```

```{r}
predictions <- predict(model.rf, data.test) # obtener las predicciones
# Evaluate performance
sqrt(mean((predictions-data.test$Monto)^2))
```

## Modelo de boosting 

Para el modelo de Boosting ocuparemos la paqueteria gbm.
Ocuparemos la distirbución gaussian para la variable Monto.

```{r}
t<-proc.time()
gbm1 = gbm(Monto~., data=data.entre, shrinkage=0.1, distribution = 'gaussian', cv.folds=10, n.trees=1000, verbose=F)
proc.time()-t
```
Calcularemos el Error de predicción del modelo usando la base de entrenamiento y despues la base de prueba.

```{r}
mean((data.entre$Monto - predict(gbm1,newdata = data.entre,type ="response"))^2)
sqrt(mean((data.test$Monto - predict(gbm1,newdata = data.test,type ="response"))^2))
```

## Conclusión

Comparando los modelos usando el erro de predicción, obtenemos que el  menor modelo es
el modelo de regresión lineal para predecir la variable Monto.



# Pregunta 3




## Modelo Generalizado


Ajsutaremos un Model Generalizado con una familia Binomial ya que la variable Seguro tomas dos valores 0 y 1.


```{r}
# 3 Modelo generalizado ------------------------------------
glm0 <- glm(Seguro~., datos, family = binomial())
glm2 <- glm(Seguro~1 , datos, family = binomial())
glm4 <- stepAIC(glm0,scope = list(lower = glm2, upper =glm0), direction = "backward")
glm3 <- stepAIC(glm2, scope = list(lower = glm2, upper = glm0), direction = "forward")
```


Vemos que el mejor modelo de clasificacion para ajustar es el modelo glm4
```{r}
summary(glm4)
anova(glm3,test = "Chisq")
pchisq(8.3315,1, lower.tail = FALSE)
```


Ahora buscaremos el mejor modelo para clasificación

```{r}
## Prediccion
x <- model.matrix(Seguro~.,family= "binomial",data = data.entre2, type.measure = "class")
y <- as.numeric(data.entre2$Seguro)
Rid2glm<- cv.glmnet(x,y,alpha = 0,type.measure = "class", family = "binomial")
Elas2.25glm <- cv.glmnet(x,y,alpha = .25,type.measure = "class", family = "binomial")
Elas2.5glm <- cv.glmnet(x,y,alpha = .5,type.measure = "class", family = "binomial")
Elas2.75glm <- cv.glmnet(x,y,alpha = .75,type.measure = "class", family = "binomial")
Lasso2glm <- cv.glmnet(x,y,alpha = 1,type.measure = "class", family = "binomial")
Rid2<- Rid2glm$lambda.min
Elas2.25 <- Elas2.25glm$lambda.min
Elas2.5 <- Elas2.5glm$lambda.min
Elas2.75 <- Elas2.75glm$lambda.min
Lasso2 <- Lasso2glm$lambda.min
```


Intentando buscar el mejor módelo vemo que algo hice mal ya que todos losmodleso me dan 0.2.


```{r}
x <- model.matrix(Seguro~.,family= "binomial",data = data.test2, type.measure = "class")
y <- as.numeric(data.test2$Seguro)
valor <- 0.9
glm1.1 <- predict(Rid2glm, s = Rid2, newx = x)
glm1 <- rep(0, length(glm1.1))
glm1[glm1.1 >= valor] <- 1
TP <- sum((glm1==1)&(y==1))
TN <- sum((glm1==0)&(y==0))
(TP + TN) / length(y)
glm1.2 <- predict(Elas2.25glm, s = Elas2.25, newx = x)
glm2 <- rep(0, length(glm1.2))
glm2[glm1.2 >= valor] <- 1
TP2 <- sum((glm2==1)&(y==1))
TN2 <- sum((glm2==0)&(y==0))
(TP2 + TN2) / length(y)
glm1.3 <- predict(Elas2.5glm, s = Elas2.5, newx = x)
glm3 <- rep(0, length(glm1.3))
glm3[glm1.3 >= valor] <- 1
TP3 <- sum((glm3==1)&(y==1))
TN3<- sum((glm3==0)&(y==0))
(TP3 + TN3) / length(y)
glm1.4 <- predict(Elas2.75glm, s = Elas2.75, newx = x)
glm4 <- rep(0, length(glm1.4))
glm4[glm1.4 >= valor] <- 1
TP4 <- sum((glm4==1)&(y==1))
TN4 <- sum((glm4==0)&(y==0))
(TP4 + TN4) / length(y)
glm1.5 <- predict(Lasso2glm, s = Lasso2, newx = x)
glm5 <- rep(0, length(glm1.5))
glm5[glm1.5 >= valor] <- 1
TP5 <- sum((glm5==1)&(y==1))
TN5 <- sum((glm5==0)&(y==0))
(TP5 + TN5) / length(y)
```
Por lo que es
De esta manera el mejor modelo de clasificación es 
$$Seguro \sim Edad + Ingreso$$
  Ahora buscaremos un modelo de clasficar para predecir.


```{r}
x <- model.matrix(Seguro~.,family= "binomial",data = data.test2)
y <- as.numeric(data.test2$Seguro)
glm2.1 <- predict(Rid2glm, s = Rid2, newx = x)
glm2.2<- predict(Elas2.25glm, s = Elas2.25, newx = x)
glm2.3 <- predict(Elas2.5glm, s = Elas2.5, newx = x)
glm2.4 <- predict(Elas2.5glm, s = Elas2.75, newx = x)
glm2.5 <- predict(Lasso2glm, s = Lasso2, newx = x)
error2.1 <- mean((y - glm2.1)^2)
error2.2 <- mean((y - glm2.2)^2)
error2.3 <- mean((y - glm2.3)^2)
error2.4 <- mean((y - glm2.4)^2)
error2.5 <- mean((y - glm2.5)^2)
```

Vemos que el menor valor del error se da entre cuatro modelos

```{r}
error2.1
error2.2
error2.3
error2.4
error2.5
```

## Modelo de Arbol

```{r}
### escojemos el modelo 4
# 3 Arbol -------------------------------------------------------
tree.hitters <- rpart(Seguro ~ Edad + Sexo + Ingreso + CDMX + Auto + KMS + Monto, 
                      data = data.entre2,
                      control = rpart.control(minbucket = 5, maxdepth = 7,xval=5,cp=0.00001),
                      method = "class",parms = list(split = "gini"))
rpart.plot(tree.hitters)
tree.hitters$cptable
printcp(tree.hitters)
tree.hitters.recortado <-prune(tree.hitters,                            tree.hitters$cptable[which.min(tree.hitters$cptable[,"xerror"]),"CP"])
pred<-predict(tree.hitters.recortado,data.test2,type="class")
table(data.test2$Seguro,pred)
```

## Modelo de Bosques


```{r}
library(ISLR)
library(randomForest)
t<-proc.time()
model.rf <- randomForest(Seguro ~. , 
                         data = data.entre2,
                         ntree = 1000, 
                         mtry = 3,
                         sampsize = floor(0.6 * nrow(data.entre)), 
                         nodesize = 100, 
                         importance = TRUE
)
model.rf
proc.time()-t
importance(model.rf)
varImpPlot(model.rf)
predictions <- predict(model.rf, data.test) # obtener las predicciones
# Evaluate performance
mean((predictions-data.test$Monto)^2)
```

Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About

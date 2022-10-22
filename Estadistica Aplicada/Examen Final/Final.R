
library(KFAS)
library(tidyverse)
library(ggcorrplot)
library(dplyr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(ggfortify)
library(reshape2)
library(GGally)
library(gridExtra)
library(unikn) 
library(PerformanceAnalytics)




# Transformcion logaritmo -------------------------------------------------

data <- read.csv("femcare.csv")
attach(data)
data %>% glimpse()
data <- data %>% mutate_if(is.numeric, .funs = function(x){log(x)})
data[sapply(data, is.infinite)] <- NA

data <- data %>%  mutate(ï..fecha = as.Date(ï..fecha,format = "%d/%m/%y"))


####################################
###### Extras
####################################

labels <- names(data)
seecol(pal="pal_unikn_pair")
cols <- usecol(pal="pal_unikn_pair")





# Test_Train --------------------------------------------------------------

test_data <- data %>% filter(ï..fecha < as.Date("2019-11-01"))
train_data <- data %>% filter(ï..fecha >= as.Date("2019-11-01"))                              





# Paises ------------------------------------------------------------------

paises <- levels(data$pais)

data_paises <- list()

for (i in 1:7) {
  data_paises[[i]] <- test_data %>% filter(pais == paises[i])
  
}

names(data_paises)<- paises






# Cami --------------------------------------------------------------------

Cami <- data_paises[[2]]

# Analisis --------------------------------------------------------

## diagrama de dispersión

Cami[3:10] %>% select_if(is.numeric) %>% gather(variable,valor, -c(share_A)) %>% 
  ggplot(aes(x=valor,y=share_A))+geom_point(aes(color= variable),alpha=.7, size=2)+
  facet_wrap(~variable,scale = "free", ncol = 3)+
  ggtitle("Diagrama de Dispersión Fabrica A")+
  scale_colour_manual(values=cols)+theme_minimal()

## Share_A vs fabrica A en el diagrama de dispersión podemos ver una relación 
# positiva contra ventas e invetario, mientras que contra pp_a es una relacion negativa
# lo cual es posible porque mientras aumente el precio del producto
## provoca que la venta disminuya generando asi un descenso en el market share

Cami[c(8,11:16)] %>% select_if(is.numeric) %>% gather(variable,valor, -c(share_A)) %>% 
  ggplot(aes(x=valor,y=share_A))+geom_point(aes(color= variable),alpha=.7, size=2)+
  facet_wrap(~variable,scale = "free", ncol = 3)+
  ggtitle("Diagrama de Dispersión Fabrica B")+
  scale_colour_manual(values=cols)+theme_minimal()

# en cuanto al fabricante B es posible una relacion negativa contra el arket share _B
# lo cual es esperedado, ya qu si el fabricatne B aumenta su participacion en el mercado
# es logico pensar que el market sahre de A dismiuria.


# Correlación

#comprobaremos lo anterior con la correlación

cor_cam_A <- Cami[3:10]%>% select_if(is.numeric) %>%  
  GGally::ggpairs(title= "Correlación variables fabrica A")+
  scale_color_manual(colors)+
  theme_minimal()

## En esta grafica se observa tres posible variables relacionadas: la primera,
## precio por unidad con una correlacion de .861; la segunda, Inventarios A, con 
## correlacion de .638; por último, Ventas por Unidad, con una correlacion de .591.

cor_cam_B <- Cami[c(8,11:16)]%>% select_if(is.numeric) %>%  
  GGally::ggpairs(title= "Correlación variables fabrica B")+
  scale_color_manual(colors)+
  theme_minimal()

# Aqui observamos que no existe ninguna posible correlacion entres el fabricante B
# y el market share A, aunque es interesante obervar el efecto que tiene 
# el market share B sobre la empresa A.

# colinealidad

# Como vimos arriba tenemos que posibles candidatos ppa, invetarios, ventas y 
# share_b, de igual manera observaremos las inversiones de la empresa A
# ya que nos interesa ver si existe un comportamiento positivo con respecto esto
# Lo primero es obeservar que estas variables no esten correlacionadas para de esta 
# manera evitar una posile colinealidad.


Cami[c(5:7,9:10,16)] %>%  GGally::ggpairs(title= "Correlación variables fabrica B")+
  theme_minimal()


Cami[c(5:7,9:10,16)] %>% 
  cor %>%
  ggcorrplot(hc.order = T, 
             type = "lower",
             outline.col = "white",
             ggtheme = ggplot2::theme_minimal,
             colors = c("#6D9EC1", "white", "#E46726"))

## observando los datos....

#Time series

#Fabrica A

ts_emp_A <- Cami[3:8] %>% 
  ts(start = c(2017,04),end = c(2019,10),frequency = 12) %>%
  autoplot( ts.linetype = 'solid', fitted.colour = 'blue', scale = "free",
            ggtheme =ggplot2::theme_minimal,title= "Series de tiempo Fabrica A")


# Fabrica B

ts_emp_B <- data[11:16] %>% 
  ts(start = c(2017,04),end = c(2019,10),frequency = 12) %>%
  autoplot( ts.linetype = 'solid', fitted.colour = 'blue', scale = "free")+
  theme_gray()


# Seleccion de Modelo -----------------------------------------------------

modelo_cami <- lm(share_A ~ share_B + inventarios_A+
                    + ppu_A, data = Cami)
summary(modelo_cami)

# Posible correlación






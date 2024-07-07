library(haven)
base_o <- read_dta("base adultos/Adultos/BBDD Respuesta - Encuestas Adultos.dta")
View(BBDD_Respuesta_Encuestas_Adultos)


# Se cargan los paquetes necesarios 

pacman::p_load(haven, dplyr, ggplot2, stringr, cluster, factoextra, summarytools,vtable)

# Quitar notacion cientifica
options(scipen=999)

#Se lee la base de datos 

base_o <- read_dta("base adultos/Adultos/BBDD Respuesta - Encuestas Adultos.dta")
#Se filta la base de datos para que solo se utilicen las variables necesarias

base_o[base_o == 98 | base_o == 99] <- NA

baseoficial <- base_o %>%
  select(SEXO, P61, P13, P20, P10_1, P10_2, P10_3, P10_4, P10_5, P10_6, P10_7)


baseoficial <- na.omit(baseoficial)


basefiltradisima <- baseoficial %>%
  select(P10_1, P10_2, P10_3, P10_4, P10_5, P10_6, P10_7)



View(basefiltradisima) #visualizar la base filtrada

view(dfSummary(basefiltradisima, headings = F, method = "render"))


baseoficial <- baseoficial %>% 
  mutate(nvl_educ = case_when(P13 == 1 ~ 1, 
                              P13 >= 2 & P13 <= 6 ~ 2, 
                              P13 >= 7 & P13 <= 10 ~ 3,
                              P13 >= 11 & P13 <= 16 ~ 4,
                              TRUE ~ NA),
         nvl_educ = factor(nvl_educ, levels = 1:4 ,labels = c("no asistio", "media imcompleta", "media completa", "superior")), 
         pospol = case_when(P20 == 6 ~ 4, #sin identificacion
                            P20 <= 2 ~ 1, #derecha
                            P20 == 3 ~ 2, #centro
                            P20 %in% 4:5 ~ 3, #izquierda
                            TRUE ~ NA),
         pospol = factor (pospol, levels = 1:4, labels = c("derecha", "centro", "izquierda", "sin identificacion")),
         genero = case_when(P61 == 1 ~ 1, #masculino
                            P61 == 2 ~ 2, #femenino
                            P61 == 3 ~ 1, #masculino 
                            P61 == 4 ~ 2, #femenino
                            TRUE  ~ 3), #otros
         genero = factor(genero, levels = 1:3, labels = c("masculino","femenino","otro")))




##### ARMAR LOS CLUSTER (ACTUALIZADO) #####

set.seed(123)

model.km <- kmeans(basefiltradisima, 
                   iter.max=6,
                   centers=2, 
                   nstart=100, 
                   trace = FALSE)

model.km

basefiltradisima$cluster_kmedias <- model.km$cluster

# grafico de los clusters
fviz_cluster(model.km, data = basefiltradisima, geom = "point")

# Para comprender mejor los grupos
aggregate(baseoficial[1:4], by=list(model.km$cluster), median)

# grafico de codo
fviz_nbclust(basefiltradisima, 
             kmeans, 
             k.max = 15,
             method = "wss",
             diss = get_dist(basefiltradisima, method = "euclidean")) +
  geom_vline(xintercept = 2,linetype = "dashed", color = "black") +  #grafico codos
  theme_minimal()
 
baseoficial$cluster_kmedias <- model.km$cluster


#tabla n°1
# Reportamos con una librería de forma directa
library(vtable)
var_stat1 <- baseoficial %>% select(nvl_educ,pospol,genero,cluster_kmedias)

st(var_stat1, digits = 3, out="kable",
fixed.digits = TRUE,
simple.kable = TRUE,
group = "cluster_kmedias",
title="Estadísticos Descriptivos.",
numformat = NA) %>%
kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"),
full_width = FALSE, fixed_thead = T)


#tabla 2 - kmedias
var_stat2 <- baseoficial %>% select(P10_1, P10_2, P10_3, P10_4, P10_5, P10_6, P10_7,
                                    cluster_kmedias) %>%
  mutate(P10_1 = as.numeric(P10_1),
         P10_2 = as.numeric(P10_2),
         P10_3 = as.numeric(P10_3),
         P10_4 = as.numeric(P10_4),
         P10_5 = as.numeric(P10_5),
         P10_6 = as.numeric(P10_6),
         P10_7 = as.numeric(P10_7))

st(var_stat2, digits = 2, out="kable",
   fixed.digits = TRUE,
   simple.kable = TRUE,
   group = "cluster_kmedias",
   title="Estadísticos Descriptivos.",
   numformat = NA) %>%
kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"),
                            full_width = FALSE, fixed_thead = T)





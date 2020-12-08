
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Inicialización y carga de librerías ####
rm(list = ls())
graphics.off()

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(funModeling)
library(corrplot)
library(lubridate)
library(Metrics)
library(caret)
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Carga de datos ####
df1 <- read_csv("data/energy_dataset.csv")
df2 <- read_csv("data/weather_features.csv")

str(df1)
str(df2)

df_status(df1)
df_status(df2)
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Limpieza y calidad de datos ####
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Arreglamos datos en df1
# Eliminamos columnas vacias (100% NAs)
df1[,c(11,24)] <- NULL
# Eliminamos columnas con 99,95% nulos.
df1[,c(4,8,9,10,14,20)] <- NULL
# Eliminamos filas con mayoria NAs pero primero tomamos nota de las fechas/horas eliminadas. 
df_dates_na <- df1 %>% filter(is.na(df1[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)])) %>% .$time
df1 <- df1 %>%  filter(!is.na(df1[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)]))


# Asignación de la media a valores sueltos de NAs
df1[6][is.na(df1[6])] =  colMeans(df1[6], na.rm = TRUE)
df1[7][is.na(df1[7])] =  colMeans(df1[7], na.rm = TRUE)
df1[8][is.na(df1[8])] =  colMeans(df1[8], na.rm = TRUE)
df1[14][is.na(df1[14])] =  colMeans(df1[14], na.rm = TRUE)
df1[19][is.na(df1[19])] =  colMeans(df1[19], na.rm = TRUE)
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Arreglamos datos en df2 
# Adaptamos tipos de datos
df2[, c(2,15,16,17)] <- lapply(df2[, c(2,15,16,17)],as.factor)
levels(df2$city_name)
levels(df2$weather_main)
levels(df2$weather_description)
levels(df2$weather_icon)
# Borramos las filas eliminadas en df1
df2 <- df2[!df2$dt_iso %in% df_dates_na,]
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

summary(df1)
summary(df2)

# Análisis de outliers en df1+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Obtenemos indicadores estadísticos de las variables
prof_df1 <- profiling_num(df1)
# Analizamos los que tienen el valor del coeficiente de variación más alto y obtenemos 
# sus boxplot
df1_m <- df1 %>% select(c(7,13,16)) %>% reshape2::melt() 
plotar(df1_m,  target= "variable", input = "value", plot_type = "boxplot")
# Comprobamos de forma visual también para estas variables su distribución
par(mfrow=c(3,1))
plot(df1$`generation hydro pumped storage consumption`,col="gray50",alpha=0.3)
plot(df1$`generation solar`,col="gray50",alpha=0.3)
plot(df1$`forecast solar day ahead`,col="gray50",alpha=0.3)
# No apreciamos outliers claros así que ni eliminamos ni imputamos.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Análisis de outliers en df2+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Obtenemos indicadores estadísticos de las variables
prof_df2 <- profiling_num(df2)
# Analizamos los que tienen el valor del coeficiente de variación más alto y
# obtenemos sus boxplot, en dos grupos, al tener diferentes unidades.
df2_m_pres <- df2 %>% select(6) %>% reshape2::melt() 
df2_m_rain <- df2 %>% select(c(10,11,12)) %>% reshape2::melt() 

plotar(df2_m_pres,  target= "variable", input = "value", plot_type = "boxplot")
plotar(df2_m_rain,  target= "variable", input = "value", plot_type = "boxplot")

# De la inspección visual de las variables, de las gráficas de boxplot y del conocimiento de
# las variables concluímos que únicamente hay valores extraños en la variable presión (algunos 
# ceros y valores exageradamente altos para presión atmosférica) ya que las cifras altas 
# de precipitación y nieve están en rangos normales.
# Procedemos a quitar valores de presión por debajo de 918hPa y por encima de 1048hPa.
df2$pressure[which(df2$pressure < 918)] =  NA
df2$pressure[which(df2$pressure > 1048)] =  NA
# Les asignamos la media a los NAs
df2[6][is.na(df2[6])] =  colMeans(df2[6], na.rm = TRUE)
# Vemos como queda:
df2_m_pres <- df2 %>% select(6) %>% reshape2::melt()
plotar(df2_m_pres,  target= "variable", input = "value", plot_type = "boxplot")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Análisis Exploratorio de Datos (EDA) ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Vemos la distribución de los datos por columnas
plot_num(df1[,-1], bins = 20)
plot_num(df2[,-1], bins = 20)

# Análisis de datos de demanda de energía y precio.
# # Datos solo del 2018.
# df11 <- df1[year(df1$time)== 2018,]

# Comparativa de demanda energética +++++++++++++++++++++++++++++++++++++++++++++++++
df1 %>% ggplot(aes(x=time)) +
  #geom_line(aes(y= df1$`total load actual`), alpha = 0.3, col = "orangered2") +
  geom_smooth(aes(y= df1$`total load actual`, col = "Demanda actual"))+
  #geom_line(aes(y= df1$`total load forecast`), alpha = 0.3, col = "gray50")+
  geom_smooth(aes(y= df1$`total load forecast`, col = "Demanda predicción"))+
  scale_colour_manual("", guide = "legend",
                      values = c("Demanda actual" = "orangered2",
                                 "Demanda predicción" = "gray50"))+
  labs(x = "", y = "Energia (MW)", title = "Demanda actual vs predicción demanda TSO") +
  theme_bw() +
  theme(legend.position = "bottom")

# Lo mismo en forma de densidad  
df1 %>%  ggplot() +
  geom_density(aes(df1$`total load actual`, fill = "Demanda actual") ,alpha = 0.3) +
  geom_density(aes(df1$`total load forecast`, fill = "Demanda predicción"),alpha = 0.3)+
  scale_fill_manual("", guide = "legend",
                    values = c("Demanda actual" = "orangered2",
                               "Demanda predicción" = "gray50"))+
  labs(x = "Energia (MW)", title = "Demanda actual vs predicción demanda TSO") +
  theme_bw() +
  theme(legend.position = "bottom")

# Diferencia
df1 %>% mutate(dif = df1$`total load actual`-df1$`total load forecast`) %>% 
  ggplot()+
  geom_density(aes(dif), col = "firebrick", size = 1)+
  labs(x = "Diferencia demanda actual-predicción", title = "Diferencia demanda actual vs predicción TSO") +
  theme_bw()

# Promedio semanal de demanda y diferencia entre real y predicho
df12 <- df1 %>% select(c(1,18,19)) %>% 
  mutate(dia = wday(time, week_start = getOption("lubridate.week.start", 1))) %>% 
  group_by(dia) %>% 
  summarise(media_carga = mean(`total load actual`),
            media_pred = mean(`total load forecast`)) %>% 
  ungroup() %>% 
  mutate(dif = media_carga-media_pred)

# Comparativa promedio semanal de demanda energética
df12g <- df12 %>% select(everything()) %>% 
  gather(key="variable", value = "value", -c(dia,dif))

df12g %>% ggplot(aes(x=dia, y=value, fill =variable, col =variable)) +
  geom_line(aes(col = variable))+
  #geom_col(aes(col = variable),position = "dodge",width = 0.7) +
  scale_y_continuous(limits = c(26000,31000))+
  scale_fill_manual("",values = c("orangered2","gray50"))+
  scale_colour_manual("",values = c("orangered2", "gray50"))+
  labs(x = "Días semana", y = "Energia (MW)", title = "Demanda actual vs predicción demanda TSO - Semanal") +
  theme_bw() +
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),labels = c("L","M","Mi","J","V","S","D"))


# Se aprecia que no hay apenas diferencia entre la demanda y la predicción de TSO por lo que
# nos vamos a centrar en la predicción del precio.

 
# Comparativa de precios actual y predicho.+++++++++++++++++++++++++++++++++++++++++++
df1 %>% ggplot(aes(x=time)) +
  geom_line(aes(y= df1$`price actual`), alpha = 0.3, col = "orangered2") +
  geom_smooth(aes(y= df1$`price actual`, col = "Precio actual"))+
  geom_line(aes(y= df1$`price day ahead`), alpha = 0.3, col = "gray50")+
  geom_smooth(aes(y= df1$`price day ahead`, col = "Precio predicción"))+
  scale_colour_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "", y = "Precio", title = "Precio actual vs predicción TSO") +
  theme_bw() +
  theme(legend.position = "bottom")

# Lo mismo en forma de densidad
df1 %>%  ggplot() +
  geom_density(aes(df1$`price actual`, fill = "Precio actual") ,alpha = 0.3) +
  geom_density(aes(df1$`price day ahead`, fill = "Precio predicción"),alpha = 0.3)+
  scale_fill_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "Precio", title = "Precio actual vs predicción TSO") +
  theme_bw() +
  theme(legend.position = "bottom")

# Y su diferencia.
df1 %>% mutate(dif = df1$`price actual`-df1$`price day ahead`) %>% 
  ggplot()+
  geom_density(aes(dif), col = "steelblue", size = 1)+
  labs(x = "Diferencia precio actual-predicción", title = "Diferencia precio actual vs predicción TSO") +
  theme_bw()
# Donde apreciamos una clara desviación respecto al cero.


# Modificamos la tabla de clima obteniendo las medias de las cinco ciudades 
# con los valores numéricos para cada hora.
df21 <- df2 %>% select(1,3:13) %>% 
  group_by(time = dt_iso) %>% 
  summarise(temp = mean(temp),
            temp_min = mean(temp_min),
            temp_max = mean(temp_max),
            pressure = mean(pressure),
            humidity = mean(humidity),
            wind_speed = mean(wind_speed),
            wind_deg = mean(wind_deg),
            rain_1h = mean(rain_1h),
            rain_3h = mean(rain_3h),
            snow_3h = mean(snow_3h),
            clouds_all = mean(clouds_all))
  

# Ahora podemos juntar las 2 tablas
# df21 <- df21[year(df21$time)== 2018,]
data <- df1 %>% inner_join(df21,by="time")

# Variables más correlacionadas con las variables objetivo
correlation_table(data, "total load actual")
correlation_table(data, "price actual")


summary(data)
str(data)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Preparación para el modelo predicitivo ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Importancia de variables con Random Forest++++++++++++++++++++++++++++++++++++++++++++++++
# Ejecutamos Random Forest para ver cuáles son las variables explicativas más importantes.
library(randomForest)
set.seed(1234)
train_forest <- data %>%
  select(-time) %>% 
  filter(!is.na(`price actual`))

random_forest <- randomForest(x= train_forest[,-which(names(train_forest) == "price actual")], y=train_forest$`price actual`, ntree=100,importance=TRUE)
random_forest <- importance(random_forest)
random_forest <- data.frame(Variables = row.names(random_forest), MSE = random_forest[,1])
random_forest <- random_forest[order(random_forest$MSE, decreasing = TRUE),]

ggplot(random_forest[1:30,], aes(x=reorder(Variables, MSE), y=MSE)) + 
  geom_bar(stat = "identity") + labs(x = "Variable", y= "MSE") + 
  coord_flip() + 
  labs(title = "Importacia de variables (MSE explanation)")

ggplot(random_forest[1:30,], aes(x=reorder(Variables, -MSE), y=MSE, group=1)) + 
  geom_point() + 
  geom_line()+ 
  labs(x = "Variable", y= "MSE") + 
  labs(title = "MSE explanation evolution") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Análisis de multicolinearidad.++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Cálculo de la correlación y filtro para aquellas que sean mayor que |0.5| 
# y toma de los nombres de dichas variables
library(psych)
df_cor <- data[,-1]
correlations <- as.matrix(x = sort(cor(df_cor, use="pairwise.complete.obs")[,"price actual"], decreasing = TRUE))
# Hacemos un pequeño filtro
names <- names(which(apply(correlations,1, function(x) abs(x)>0.1))) 

# Ordenar en df para mostrar las variables seleccionadas
df_cor <- df_cor[, names]

# Crear y representar la matriz de correlación
correlations <- cor(df_cor, use="pairwise.complete.obs")
cor.plot(correlations, numbers=TRUE, xlas = 2, upper= FALSE, main="Correlaciones entre las variables", zlim=c(abs(0.65),abs(1)), colors=TRUE)
# Vemos qué variables están más correlacionadas para descartar redundancias:
# "generation biomass" con "generation other" (0,66). Cogemos "generation biomass"
# "generation fossil hard coal" con "price day ahead" (0,67) y con "generation fossil brown coal/lignite" (0,77).
# Cogemos "price day ahead"
# "total load actual" con "total load forecast" (0,99). Cogemos "total load actual".
# "generation wind onshore" con "generation wind onshore day ahead" (0,99). Cogemos "generation wind onshore".

df <- data[,-c(3,5,11,17,18)]
rmse(df$`price actual`,df$`price day ahead`)

# Pendiente:
# Las horas hay que ponerlas como dummy var ya que están como tipo date, no numérico.
# Hay que hacer partición train-test (80-20) con la variable "price actual" como
# target. Comprobar resultados con el dataset de test.

df_train <- df %>% mutate(hora = hour(time)) %>% 
  fastDummies::dummy_cols(select_columns = "hora") %>%
  select(-c("time", "hora"))

# Separamos en train y test
train <- createDataPartition(y = df_train$`price actual`, p = 0.8, list = FALSE, times = 1)
datos_train <- df_train[train, ]
datos_test  <- df_train[-train, ]
# Comprobamos la semejante distribución de la variable objetivo en los dataset de train y test.
ggplot()+
  geom_density(data = datos_train, aes(x=`price actual`, fill = "Train"), alpha =0.5)+
  geom_density(data = datos_test, aes(x = `price actual`, fill = "Test"), alpha = 0.5)+
  scale_fill_manual("", guide = "legend",
                    values = c("Train" = "orangered2",
                               "Test" = "gray50"))+
  theme_bw() +
  theme(legend.position = "bottom")

# Primera prueba de entrenamiento de un modelo de regresión lineal
modelo_lm <- train(`price actual` ~ ., method = "lm", data = datos_train)
modelo_lm$resample %>% head(10)
summary(modelo_lm)

# Eliminamos las variables no significativas para el modelo (p-valor > 0,05)
datos_train <- datos_train[,-c(6,7,9,23,24,50)]

# Segunda prueba de entrenamiento de un modelo de regresión lineal
modelo_lm <- train(`price actual` ~ ., method = "lm", data = datos_train)
modelo_lm$resample %>% head(10)
summary(modelo_lm)

# Utilizando resampling
particiones  <- 10
repeticiones <- 5

control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              returnResamp = "all", verboseIter = FALSE,
                              allowParallel = TRUE)

# Ajuste del modelo
modelo_lm <- train(`price actual` ~ ., data = datos_train,
                          method = "lm",
                          metric = "RMSE",
                          trControl = control_train)
modelo_lm
summary(modelo_lm)
modelo_lm$resample %>% head(10)
summary(modelo_lm$resample$RMSE)


predicciones_lm <- predict(modelo_lm, newdata = datos_test,
                            type = "raw")

RMSE(datos_test$`price actual`, predicciones_lm)
ggplot() +
  geom_density(aes(datos_test$`price actual`, fill = "Precio actual") ,alpha = 0.3) +
  geom_density(aes(predicciones_lm, fill = "Precio predicción"),alpha = 0.3)+
  scale_fill_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "Precio", title = "Precio actual vs predicción") +
  theme_bw() +
  theme(legend.position = "bottom")

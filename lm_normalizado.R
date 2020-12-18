# Regresion lineal normalizando las variables
library(caret)

unPreProc <- function(preProc, data){
  stopifnot(class(preProc) == "preProcess")
  stopifnot(class(data) == "data.frame")
  for(i in names(preProc$mean)){
    tmp <- data[, i] * preProc$std[[i]] + preProc$mean[[i]]
    data[, i] <- tmp
  }
  return(data)  
}

preproc1 <- preProcess(datos_train[,c(1:26)], method=c("center", "scale"))
preproc2 <- preProcess(datos_test[,c(1:26)], method=c("center", "scale"))

norm1 <- predict(preproc1, datos_train[,c(1:26)])
norm2 <- predict(preproc2, datos_test[,c(1:26)])


summary(norm1) 
summary(norm2) 


datos_train_norm <- datos_train
datos_test_norm <- datos_test

 
datos_train_norm[,1:26] <- norm1[,1:26]
datos_test_norm[,1:26] <- norm2[,1:26]


modelo_lm_norm <- train(`price actual` ~ ., method = "lm", data = datos_train_norm)
summary(modelo_lm_norm)
modelo_lm_norm$resample %>% head(10)
summary(modelo_lm_norm$resample$RMSE)



predicciones_lm_norm <- predict(modelo_lm_norm, newdata = datos_test_norm,
                           type = "raw")
predicciones_lm_norm <- as.data.frame(predicciones_lm_norm)
datos_test_pred <- datos_test_norm
datos_test_pred[,15] <- predicciones_lm_norm
predicciones_lm_unnorm <- unPreProc(preproc1,as.data.frame(datos_test_pred))

RMSE(datos_test$`price actual`, predicciones_lm_unnorm$`price actual`)
ggplot() +
  geom_density(aes(datos_test$`price actual`, fill = "Precio actual") ,alpha = 0.3) +
  geom_density(aes(predicciones_lm_unnorm$`price actual`, fill = "Precio predicción"),alpha = 0.3)+
  scale_fill_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "Precio", title = "Precio actual vs predicción") +
  theme_bw() +
  theme(legend.position = "bottom")


# Modelo simple solo con `price day ahead`

modelo_lm_simple <- train(`price actual` ~ `price day ahead`, method = "lm", data = datos_train)
summary(modelo_lm_simple)
modelo_lm_simple$resample %>% head(10)
summary(modelo_lm_simple$resample$RMSE)

predicciones_lm_simple <- predict(modelo_lm_simple, newdata = datos_test,
                           type = "raw")

rmse(datos_test$`price actual`, predicciones_lm_simple)

ggplot() +
  geom_density(aes(datos_test$`price actual`, fill = "Precio actual") ,alpha = 0.3) +
  geom_density(aes(predicciones_lm_simple, fill = "Precio predicción"),alpha = 0.3)+
  scale_fill_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "Precio", title = "Precio actual vs predicción") +
  theme_bw() +
  theme(legend.position = "bottom")

difer <- data.frame(dif=datos_test$`price actual`-predicciones_lm_simple)

difer %>% ggplot()+
  geom_histogram(aes(dif),col = "steelblue", size = 1)+
  labs(x = "Diferencia precio actual-predicción", title = "Diferencia precio actual vs predicción TSO") +
  theme_bw()


# Modelo simple polinomico solo con `price day ahead`

modelo_lm_poly <- train(`price actual` ~ poly(`price day ahead`,2), method = "lm", data = datos_train)
summary(modelo_lm_poly)
modelo_lm_poly$resample %>% head(10)
summary(modelo_lm_poly$resample$RMSE)

predicciones_lm_poly <- predict(modelo_lm_poly, newdata = datos_test,
                                  type = "raw")

RMSE(datos_test$`price actual`, predicciones_lm_poly)
ggplot() +
  geom_density(aes(datos_test$`price actual`, fill = "Precio actual") ,alpha = 0.3) +
  geom_density(aes(predicciones_lm_poly, fill = "Precio predicción"),alpha = 0.3)+
  scale_fill_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "Precio", title = "Precio actual vs predicción") +
  theme_bw() +
  theme(legend.position = "bottom")


# Bucle para calcular la predicción para los últimos n días.
datos_test_price_pred <- data.frame()
datos_test_price_actual <- data.frame()
rmse_list = c()
for (i in 10:1) {
  idx1 = nrow(df)-(i*24)
  idx2 = idx1 + 1
  
  datos_train <- df_train[1:idx1,]
  datos_test <- df_train[idx2:(idx2+23),]
  
  preproc1 <- preProcess(datos_train[,c(1:26)], method=c("center", "scale"))
  preproc2 <- preProcess(datos_test[,c(1:26)], method=c("center", "scale"))
  norm1 <- predict(preproc1, datos_train[,c(1:26)])
  norm2 <- predict(preproc2, datos_test[,c(1:26)])
  datos_train_norm <- datos_train
  datos_test_norm <- datos_test
  datos_train_norm[,1:26] <- norm1[,1:26]
  datos_test_norm[,1:26] <- norm2[,1:26]
  
 
  
  modelo_lm_simple <- train(`price actual` ~ `price day ahead`+
                              poly(wind_speed,2)+
                              poly(pressure,2)+
                              poly(`generation hydro run-of-river and poundage`,2)+
                              poly(temp_min,3)+
                              poly(humidity,3)+
                              `generation wind onshore`+
                              `generation waste`+
                              poly(`generation other renewable`,3)+
                              poly(`generation biomass`,2)+
                              poly(temp_max,2)+
                              poly(`forecast solar day ahead`,3)+
                              poly(`generation hydro water reservoir`,3)+
                              wind_deg +
                              `generation solar` +
                              poly(clouds_all,2)+
                              poly(`generation fossil oil`,2)+
                              `generation fossil gas`+
                              `generation hydro pumped storage consumption`, 
                            method = "lm", 
                            data = datos_train_norm)
  predicciones_lm_simple <- predict(modelo_lm_simple, newdata = datos_test_norm,
                                    type = "raw")
  predicciones_lm_norm <- as.data.frame(predicciones_lm_simple)
  datos_test_pred <- datos_test_norm
  datos_test_pred[,15] <- predicciones_lm_norm
  predicciones_lm_unnorm <- unPreProc(preproc2,as.data.frame(datos_test_pred))
  
  datos_test_price_pred = rbind(datos_test_price_pred,as.data.frame(predicciones_lm_unnorm$`price actual`))
  datos_test_price_actual = rbind(datos_test_price_actual,as.data.frame(datos_test$`price actual`))
  
  rmse_list[i] <- rmse(datos_test$`price actual`, predicciones_lm_unnorm$`price actual`)
}
rm(preproc1,preproc2,norm1,norm2,datos_train_norm,datos_test_norm)

colnames(datos_test_price_actual) <- "price actual"
colnames(datos_test_price_pred) <- "price day ahead"


mean(rmse_list)
summary(modelo_lm_simple)
rmse_list

# Comparativa de precios actual y predicho en forma de densidad
ggplot() +
  geom_density(aes(datos_test_price_actual$`price actual`, fill = "Precio actual") ,alpha = 0.3) +
  geom_density(aes(datos_test_price_pred$`price day ahead`, fill = "Precio predicción"),alpha = 0.3)+
  scale_fill_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "Precio", title = "Precio actual vs predicción TSO") +
  theme_bw() +
  theme(legend.position = "bottom")

# Diferencia entre precio real y predicho
datos_test_price_actual %>% mutate(dif = datos_test_price_actual$`price actual`- datos_test_price_pred$`price day ahead`) %>% 
ggplot()+
  geom_density(aes(dif), col = "steelblue", size = 1)+
  geom_density(data = df1,aes(df1$`price actual`-df1$`price day ahead`), col = "firebrick", size = 1)+
  labs(x = "Diferencia precio actual-predicción", title = "Comparativa de predicciones") +
  theme_bw()


ggplot() +
  geom_line(aes(x = which(datos_test$`price actual`!= 0), y=datos_test$`price actual`, col = "Precio actual")) +
  geom_line(aes(x = which(datos_test$`price actual`!= 0), y=predicciones_lm_unnorm$`price actual`, col = "Precio predicción"))+
  scale_colour_manual("", guide = "legend",
                    values = c("Precio actual" = "orangered2",
                               "Precio predicción" = "gray50"))+
  labs(x = "Horas", title = "Precio actual vs predicción") +
  theme_bw() +
  theme(legend.position = "bottom")

summary(datos_test$`price actual`-predicciones_lm_unnorm$`price actual`)

par(mfrow=c(3,1)) 
modelo_lm_simple$finalModel$residuals
plot( modelo_lm_simple$finalModel$residuals ~ modelo_lm_simple$finalModel$fitted.values)
hist(modelo_lm_simple$finalModel$residuals)
qqnorm(modelo_lm_simple$finalModel$residuals); qqline(modelo_lm_simple$finalModel$residuals, col = 2,lwd=2,lty=2)
shapiro.test(modelo_lm_simple$finalModel$residuals[1:5000])





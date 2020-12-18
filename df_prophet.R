library(prophet)

df_prophet <- df1[,c(1,21)]
df_prophet <- df_prophet %>% 
  mutate(ds=time,
         y =`price actual`)
m <- prophet(df_prophet)
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)

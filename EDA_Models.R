library(readxl)
library(dplyr)
library(fpp3)
library(ggplot2)
library(knitr)

#reading data
ts_data<-read.csv("C:/Users/Milad/Downloads/conestoga materials/second semester/statiscal forcasting/project 2/Data/updated_final.csv")
head(ts_data)
# Converting 'datetime' column to datetime format
ts_data$datetime <- as_datetime(ts_data$datetime)
head(ts_data)
# Changing table format to enable working as a time series
ts_data <- ts_data %>% as_tsibble(index = datetime)
head(ts_data)

# Create the training set (observations before split_date)
train_set <- ts_data[ ts_data$datetime < '2023-10-01 23:00', ]
train_set
# Create the test set (observations on and after split_date)
test_set <- ts_data[ts_data$datetime >= '2023-10-01 23:00', ]
#HOEP time  plot
autoplot(train_set)+ xlab("Datetime")+ ylab("Average Weighted Hourly Price (¢/kWh)")+ ggtitle ("Time Series of HOEP")
sample_week_data <- train_set %>% filter(datetime >= '2023-01-01 1:00
' & datetime < '2023-01-08 23:00')
autoplot(sample_week_data)+ xlab("Datetime")+ ylab("Average Weighted Hourly Price (¢/kWh)")+ ggtitle ("Time Series of HOEP")

day_data <- train_set %>%
  mutate(
    day_name=weekdays(datetime),
    date=as.Date(datetime),
    hour = hour(datetime),
    monthly = month(datetime),
    quarterly = quarter(datetime)
    
  )
day_data
#daily_mean <- as_tibble(day_data) %>%
# group_by(date) %>%
#summarise(mean_HOEP = mean(HOEP),day_name = first(day_name)) 
#daily_mean

gg_season(sample_week_data,period = 24)+xlab('Time')+ylab('Average Weighted Hourly Price (¢/kWh)')+labs(color='day')
gg_season(sample_week_data,polar = 'True',period = 24)+labs(color='day',y='Average Weighted Hourly Price (¢/kWh)')
#daily_mean_tsibble <- as_tsibble(daily_mean)


#gg_subseries(as_tsibble(daily_mean),period =daily_mean$day_name )+ggtitle("seasonal subseries plot:forest fires in Brazil")+ylab("Total")

# autocorrelation function plot 
ACF(train_set, lag_max = 300) %>%
  autoplot() +
  labs(title = "Auto Correlation Function Plot")
#ACF shows both seasonality and Trednd
# Partial autocorrelation function plot 
PACF(train_set,lag_max = 150) %>% autoplot()

#STL decomposition
STL_dec<-train_set%>%model(STL(HOEP))
STL_dec%>%components()%>% autoplot()
STL_dec%>%components()%>%tail(23)%>%kable()
#Trend and seasonality strength
ts_data %>%
  features(HOEP, feat_stl)
#kpss unitroot test
train_set %>% features(HOEP, unitroot_kpss)
#doing seasonal differencing and checking plots
train_set<-train_set%>%
  mutate(seasonal_difference=difference(HOEP,24),
         double_differenc=difference(difference(HOEP,24),1),
         standard_difference=difference(HOEP))
#checking seasonal difference
train_set%>%gg_tsdisplay(seasonal_difference,plot_type = "partial", lag=100)+
  labs(title = 'Seasonally differenced')

train_set %>% features(seasonal_difference, unitroot_kpss)

train_set%>%gg_tsdisplay(double_differenc,plot_type = "partial", lag=100)+
  labs(title = 'Double differenced')
train_set %>% features(double_differenc, unitroot_kpss)

train_set%>%gg_tsdisplay(standard_difference,plot_type = "partial", lag=100)+
  labs(title = 'Standard differenced')

train_set %>% features(standard_difference, unitroot_kpss)
#--------------model--------------------------
fit<-train_set%>% model(ARIMA(HOEP))
# Display the details of the selected model
report(fit)
#resiual analysis
gg_tsresiduals(fit)
#ljung_box test
fit%>%augment()%>%features(.innov, ljung_box,lag=36,dof=6)
#model 2
#fit_2<-train_set%>% 
  #model(ARIMA(HOEP, stepwise = FALSE,approximation =FALSE,
        #      order_constraint = p+q+P+Q<=9))
#report(fit_2)
#model 3
fit_3 <- train_set %>%
  model(
     ARIMA(HOEP ~ pdq(1,1,2) + PDQ(0,0,2)),
  )
report(fit_3)
gg_tsresiduals(fit_3)
fit_3%>%augment()%>%features(.innov, ljung_box,lag=36,dof=5)
#model 4
fit_4 <- train_set %>%
  model(
    ARIMA(HOEP ~ pdq(2,1,0) + PDQ(2,0,0)),)
report(fit_4)
gg_tsresiduals(fit_4)
fit_4%>%augment()%>%features(.innov, ljung_box,lag=36,dof=5)

fit_5 <- train_set %>%
  model(
     ARIMA(HOEP ~ pdq(0,1,3) + PDQ(0,0,3)),)
report(fit_5)
gg_tsresiduals(fit_5)
fit_5%>%augment()%>%features(.innov, ljung_box,lag=36,dof=6)
# forcasting and analyzing models performance using test set
pred_1 <- forecast(fit, h= nrow(test_set))
pred_2 <- forecast(fit_3, h=nrow(test_set))
pred_3 <- forecast(fit_4,h= nrow(test_set))
pred_4 <- forecast(fit_5, h=nrow(test_set))
pred_1 %>%accuracy(test_set)
pred_2 %>%accuracy(test_set)
pred_3 %>%accuracy(test_set)
pred_4%>%accuracy(test_set)
# fit, fit_3 and fit_5 have the same rmse but fit_1 has the best AICc so fit model is the best amont SARIMA models
# visualizing the forcasting
pred_1 %>% autoplot(test_set )
#basic models
ets_HOEP <- train_set %>% model(ETS(HOEP~error("A") +
                                      trend("N") +
                                      season("A")))
pred_ets <- forecast(ets_HOEP, h= nrow(test_set))
pred_ets %>%accuracy(test_set)

basic_fit <- train_set %>%
  model(
    Mean = MEAN(HOEP),
    Naive = NAIVE(HOEP),
    Drift = NAIVE(HOEP ~ drift())
  )
pred_basic <- forecast(basic_fit, h= nrow(test_set))
pred_basic %>%accuracy(test_set)
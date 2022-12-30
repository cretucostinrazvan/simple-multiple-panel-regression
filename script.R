# Setare fisier de lucru
rm(list = ls()) 
directory <- "D:/Facultate/Econometrie/proiect_econometrie2/"

# Instalarea pachetelor
PackageNames <- c("readxl","dplyr","tidyverse", "stargazer", "magrittr", "ggplot2", "lmtest", "DataCombine", "dplyr", "tseries", "caret", "car", "olsrr", "moments", "whitestrap", "plm", "nortest", "Metrics", "MLmetrics")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

CarPriceAssigment <- read_excel("CarPriceAssigment.xlsx")

# 4.Regresia simpla ----------------------------------------------------
# Vom selecta atributele importante 

CarPriceAssigment %<>% dplyr::select(price, horsepower)
str(CarPriceAssigment)
stargazer(CarPriceAssigment, type = "text")
head(CarPriceAssigment, 10)

# Explorarea datelor
cor(CarPriceAssigment)
CarPriceAssigment %<>% dplyr::mutate(avg_price = mean(price))

# Regresia simpla: price = beta0 + beta1*horsepower + u
model_CarPriceAssigment <- lm(formula = price ~ horsepower, data = CarPriceAssigment)
summary(model_CarPriceAssigment)
model_CarPriceAssigment$coefficients['horsepower']
# intercept (constanta) semnificativa la 99%
# horsepower semnificativ la 99%
# R^2 = 0.6514 => modelul explica 65.14%
# F statistic = 382.2, p=0.00000000000000022 < 0.00001 => model valid statistic

n <- nobs(model_CarPriceAssigment)
n

k <- model_CarPriceAssigment$rank - 1
k

# Graficul observatiilor cu dreapta estimata
plot(x = CarPriceAssigment$horsepower, y = CarPriceAssigment$price)
abline(a = model_CarPriceAssigment$coefficients['(Intercept)'], 
       b = model_CarPriceAssigment$coefficients['horsepower'],
       col = 'red')

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = CarPriceAssigment, mapping = aes(x = horsepower, y = price)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Predictie dupa regresie ------------------------------------------

# Valoarea estimata pentru variabila dependenta (pricehat)
CarPriceAssigment %<>% dplyr::mutate(pricehat = fitted(model_CarPriceAssigment))
stargazer(CarPriceAssigment, type = "text")
ggplot(data = CarPriceAssigment, mapping = aes(x = horsepower)) +
  geom_point(mapping = aes(y = price, color = 'Price - actual value')) +
  geom_point(mapping = aes(y = pricehat, color = 'Pricehat - predicted value')) 

# Reziduuri
CarPriceAssigment %<>% dplyr::mutate(uhat = residuals(model_CarPriceAssigment))
stargazer(CarPriceAssigment, type = "text")
ggplot(CarPriceAssigment, aes(x = horsepower)) +
  geom_point(aes(y = price, col = 'Price - actual value')) +
  geom_point(aes(y = uhat, col = 'Residual uhat'))

head(CarPriceAssigment, 10)

# Graficul valorilor si reziduurilor reale si previzionate
ggplot(CarPriceAssigment, aes(x = horsepower)) +
  geom_point(aes(y = price, color = 'Price - actual value')) +
  geom_point(aes(y = pricehat, color = 'Pricehat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = price, color = 'Fitted line'), 
              method = "lm", se = FALSE)
# Bonitatea modelului (R-squared) ----------------------------------
# Utilizam functia 'str' pt a afisa ceea ce este inclus in model sau rezumatul acestuia
str(model_CarPriceAssigment)
str(summary(model_CarPriceAssigment)) #este destul de greu de urmarit in aceasta maniera

# 'r.squared' din summary este R-squared
summary(model_CarPriceAssigment)$r.squared

# Folosim nobs pentru a afisa numarul de observatii pe care le contine modelul
nobs(model_CarPriceAssigment)
# Forma functionala log: log-log si log-lin -------------------------------
CarPriceAssigment2 <- read_excel("CarPriceAssigment.xlsx")
CarPriceAssigment2 %>% dplyr::select(price, lprice, horsepower, lhorsepower) %>% head(10)

# Forma liniara
model_CarPriceAssigment2 <- lm(price ~ horsepower, CarPriceAssigment2)
summary(model_CarPriceAssigment2)
ggplot(CarPriceAssigment2, aes(x = horsepower, y = price)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Cream o variabila noua pe care o logaritmam cu ajutorul functiei log
# CarPriceAssigment2 <- CarPriceAssigment2 %>% mutate(lhorsepower = log(horsepower),
#                               lprice = log(price))

# Log-log 
model_CarPriceAssigment3 <- lm(lprice ~ lhorsepower, CarPriceAssigment2)
summary(model_CarPriceAssigment3)
# Grafic
ggplot(CarPriceAssigment2, aes(x = lhorsepower, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Linear-log 
model_CarPriceAssigment3 <- lm(price ~ lhorsepower, CarPriceAssigment2)
summary(model_CarPriceAssigment3)
# Grafic
ggplot(CarPriceAssigment2, aes(x = lhorsepower, y = price)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Log-linear 
model_CarPriceAssigment4 <- lm(lprice ~ horsepower, CarPriceAssigment2)
summary(model_CarPriceAssigment4)
# Grafic
ggplot(CarPriceAssigment2, aes(x = horsepower, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)
# Heteroschedasticitate ----
model_CarPriceAssigmentHT <- lm(price ~ horsepower, CarPriceAssigment)
summary(model_CarPriceAssigmentHT)

# Graficul reziduurilor "geom_point"
ggplot(data = CarPriceAssigment, mapping = aes(x = horsepower)) +
  theme_bw() +
  geom_point(mapping = aes(y = uhat)) +
  geom_hline(yintercept = 0, col = 'red') + # adaugarea dreptei orizontale
  ylab(label = "Residuals") # nume axa y
# Analiza grafica a heteroscedasticitatii ----
# Vom testa heteroscedasticitatea pe baza primului model estimat (model_0)

# Graficul reziduurilor fata de variabila independenta horsepower
CarPriceAssigment %<>% mutate(uhat = resid(model_CarPriceAssigment)) # extragem reziduurile din model 
ggplot(data = CarPriceAssigment, mapping = aes(x = horsepower, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Pret de vanzare, horsepower')

# Graficul reziduurilor fata de valorile estimate de model
CarPriceAssigment %<>% mutate(yhat = fitted(model_CarPriceAssigment))
ggplot(data = CarPriceAssigment, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')
# O modalitate frecvent utilizata care ne ajuta sa scapam de heteroscedasticitate
# este de a schimba forma functionala a variabilelor (log ne ajuta in cele mai 
# multe cazuri)

# Regresie cu forma functionala log-log
model_3 <- lm(lprice ~ lhorsepower, CarPriceAssigment)
summary(model_3)
CarPriceAssigment %<>% mutate(uhat1 = resid(model_3))

# Graficul reziduurilor fata de variabila independenta lhorsepower
ggplot(CarPriceAssigment) + 
  theme_bw() + 
  geom_point(aes(x = lhorsepower, y = uhat1)) +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Log pret de vanzare, lhorsepower')

# Graph of residuals against fitted values
CarPriceAssigment %<>% mutate(yhat1 = fitted(model_3))
ggplot(data = CarPriceAssigment, mapping = aes(x = yhat1, y = uhat1)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')
# De aceasta data, folosind forma functionala log-log pare ca reziduurile sunt homoscedastice.
# Testam ipotezele pe reziduuri si incepem cu homoscedasticitate  ---- 
model_CarPriceAssigment <- lm(formula = price ~ horsepower, data = CarPriceAssigment)
summary(model_CarPriceAssigment)
bptest(model_CarPriceAssigment) # hetero
white_test(model_CarPriceAssigment) # hetero
coeftest(model_CarPriceAssigment, vcov. = vcovHC(model_CarPriceAssigment, type = "HC1"))# hetero
# ipoteza incalcata => corectie prin WLS (se poate si prin log-log)
model_WLS1 <- lm(formula = price ~ horsepower, 
                 data = CarPriceAssigment, weights = 1/horsepower)

CarPriceAssigment %<>% mutate(pricestar = price/sqrt(horsepower),
                              horsepowerstar = horsepower/sqrt(horsepower),
                              constantstar = 1/sqrt(horsepower)
)

model_WLS2 <- lm(pricestar ~ 0 + constantstar + horsepowerstar, 
                 CarPriceAssigment) 

bptest(model_WLS2) # hetero
white_test(model_WLS2) # hetero
coeftest(model_WLS2, vcov. = vcovHC(model_WLS2, type = "HC1"))

summary(model_WLS2)
# Autocorelarea ----
# Incarcam setul de date CarPriceAssigment
data(CarPriceAssigment)

# si variabilta independenta horsepower (cp)
model1 <- lm(price ~ horsepower, data=CarPriceAssigment)
summary(model1)
# Inspectarea autocorelarii cu ajutorul graficului ACF (autocorelare)
acf(model1$residuals)

# Testul Durbin-Watson (ordinul 1)
dwtest(model1) # p-value < 0.1 => reziduurile sunt autocorelate

# Testul Breusch-Godfrey (order superior)
bgtest(model1) # p-value < 0.1 
bgtest(model1, order = 2) # =>
bgtest(model1, order = 3)
# reziduurile sunt autocorelate si la lag superior

# Corectarea autocorelarii - adaugam lag1 ca variabila independenta in modelul original

# Cream un nou set de date 
econ_data <- data.frame(CarPriceAssigment, resid_mod1=model1$residuals)
# Cream variabila lag1 
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
econ_data_2 <- na.omit(econ_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model2 <- lm(price ~ horsepower + lag1, data=econ_data_2)

# Retestam ipoteza pe noul model
# ACF 
acf(model2$residuals) # autocorelarea a disparut
# Durbin Watson 
dwtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
# Breusch-Godfrey 
bgtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model2, order = 2)
# Testarea ipotezei de normalitate in reziduuri -----------------
CarPriceAssigment %>% 
  select(price, lprice, horsepower) %>% 
  stargazer(type = "text")
# price - pret de vanzare
# lprice - log de pret de vanzare
# horsepower - putere maxima

CarPriceAssigment %>% 
  select(price, lprice, horsepower) %>% 
  head(10)

model_0 <- lm(price ~ horsepower, CarPriceAssigment) 
summary(model_0)
CarPriceAssigment %<>% mutate(uhat = resid(model_0)) # extragem reziduurile din model 

# Pas 1 - Graficul 'Residuals vs Fitted'
plot(model_0) # scriem 1 in consola pentru afisare

# Se poate realiza un grafic asemanator folosind si libraria olsrr
ols_plot_resid_fit(model_0)

# Pasul 2 - Graficul 'Q-Q plot'
plot(model_0) # este al doilea grafic din functia plot si il putem afisa 
# tastand de doua ori o valoare in consola

ols_plot_resid_qq(model_0)

# Pasul 3 - Histograma reziduurilor
ols_plot_resid_hist(model_0)

ggplot(data = CarPriceAssigment) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

skewness(CarPriceAssigment$uhat)

kurtosis(CarPriceAssigment$uhat) # in cazul de fata distributia noastra este platicurtica

# Pasul 4 - Graficele de tip Boxplot

boxplot(model_0$residuals, main="Box Plot reziduuri")

ols_plot_resid_box(model_0)

ggplot(CarPriceAssigment, aes(x=uhat, y=price)) + 
  geom_boxplot() +
  theme_bw()+
  xlab('Reziduuri') + 
  ylab('Pret de vanzare') +
  ggtitle('Boxplot reziduuri') + 
  theme(plot.title = element_text(hjust = 0.5))

# Pasul 5 - Testarea normalitatii cu ajutorul testelor specifice acestei ipoteze.

# Testul Shapiro Wilk pentru normalitate
shapiro.test(CarPriceAssigment$uhat) 

# Testul Jarque-Bera pentru normalitate
jarque.bera.test(CarPriceAssigment$uhat)

ols_test_normality(model_0)

ols_plot_cooksd_bar(model_0) 
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(17,18,49,7,72,73,74,75,106,130,183,185,193), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(14,15,45,63,64,65,66,67,68,94,96,118,119,120), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(8,12,13,25,43,66,85,105,106,156,176,178), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(7,90,166), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(53,85,87, 133), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(5,9,84, 85, 134), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(2,7,49,81,82,150,153), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(89,144,147), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(4,55,59,90,144), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(56,70,85), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(83,113,136), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(72), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(3,4), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(39), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(38), ]
model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)
# Testele Shapiro-Wilk si Jarque Bera pentru detectarea normalitatii ----
CarPriceAssigment %>% 
  select(price, horsepower) %>% 
  head(10)

CarPriceAssigment %>% 
  select(price, horsepower) %>%
  stargazer(type = "text")

# Histograma variabilei price
ggplot(data = CarPriceAssigment) +
  theme_bw() +
  geom_histogram(mapping = aes(x = price), col = 'grey') +
  xlab('Pret vanzare') + 
  ylab('Count') +
  ggtitle('Histograma variabilei pretului de vanzare') + 
  theme(plot.title = element_text(hjust = 0.5))

# Histograma variabilei lprice
ggplot(data = CarPriceAssigment) +
  theme_bw() +
  geom_histogram(mapping = aes(x = lprice), col = 'grey')+
  xlab('LPretvanzare') + 
  ylab('Count') +
  ggtitle('Histograma variabilei LPretvanzare') + 
  theme(plot.title = element_text(hjust = 0.5))


# Testul Shapiro Wilk pentru normalitate
shapiro.test(CarPriceAssigment$price)
shapiro.test(CarPriceAssigment$lprice)

# Testul Jarque-Bera pentru normalitate
jarque.bera.test(CarPriceAssigment$price)
jarque.bera.test(CarPriceAssigment$lprice)

model_0 <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_0)

CarPriceAssigment <- CarPriceAssigment[-c(17,18,48,49,50,67,71,72,73,74,75,76,106,129,130), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(9,15,16,28,62,63,64,71,90,91,92,112,114,115,165,189,190), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(8,12,13,101,150,151,152,163,173), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(54,90,135), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(7,85,87,137), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(5,9,84,85), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(2,7,49,61,62,81,82,149,152), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(87,119,130,141), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(4,126,140), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(85), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(136), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(86), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(70), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(73), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(111,132), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(3,50,51,83,112), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(9,39,71,107), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(49,66,97), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(92,93,104), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(12,48,93), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(61,103), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(2,95,103), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(49,94,105), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(2,23), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(84), ]
model_corr <- lm(price ~ horsepower,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)



# Testul Shapiro Wilk pentru normalitate
shapiro.test(CarPriceAssigment$price)
shapiro.test(CarPriceAssigment$lprice)

# Testul Jarque-Bera pentru normalitate
jarque.bera.test(CarPriceAssigment$price)
jarque.bera.test(CarPriceAssigment$lprice)
# RMSE - Root Mean Squared Error ----
# Model de regresie de tip log-log pentru setul de antrenare
training.samples <- CarPriceAssigment$lprice %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- CarPriceAssigment[training.samples, ]
test.data <- CarPriceAssigment[-training.samples, ]
model_0 <- lm(lprice ~ lhorsepower, data = CarPriceAssigment) 
summary(model_0)
# Predictia modelului pe setul de testare
y_pred <- predict(model_0, newdata = train.data)
y_pred
RMSE(y_pred, test.data$lprice)
# MAE - Mean Absolute Error ----
# Model de regresie de tip log-log pentru setul de antrenare
training.samples <- CarPriceAssigment$lprice %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- CarPriceAssigment[training.samples, ]
test.data <- CarPriceAssigment[-training.samples, ]
model_0 <- lm(lprice ~ lhorsepower, data = CarPriceAssigment) 
summary(model_0)
# Predictia modelului pe setul de testare
y_pred <- predict(model_0, newdata = train.data)
y_pred
MAE(y_pred, test.data$lprice)
# MSE - Mean Squared Error ----
mse(y_pred, test.data$lprice)
# MAPE - Mean Absolute Percentage Error ----
MAPE(y_pred, test.data$lprice)
# 6. Imbunatatirea regresiei simple prin adaugarea unei noi variabile independente (Regresie multipla) ----
# Rulam o analiza de regresie multipla si interpretam coeficientii
# Observam diferenta dintre regresie simpla si multipla
CarPriceAssigment %>% 
  select(price, horsepower, citympg, enginesize) %>%
  head(10) 
# horsepower - exprimata in number
# citympg - exprimata in number
# enginesize - exprimata in number

CarPriceAssigment %>% 
  select(price, horsepower, citympg, enginesize) %>%
  str

CarPriceAssigment %>% 
  select(price, horsepower, citympg, enginesize) %>%
  stargazer(type = "text")

# Comparam rezultatele dintre regresia simpla si cea multipla
# Daca caii putere ai masinii cresc cu 1, cu cati dolari creste pretul de vanzare?

# Regresia simpla
model_simple <- lm(price ~ horsepower, CarPriceAssigment)
summary(model_simple)

# Daca caii putere ai masinii cresc cu 1 => pretul de vanzare creste cu 163.263

# Regresia multipla
model_multiple1 <- lm(price ~ horsepower + enginesize, CarPriceAssigment)
summary(model_multiple1)
# Daca caii putere ai masinii cresc cu 1 => pretul de vanzare creste cu 58.85 pastrand ceilalati termeni constanti

model_multiple2 <- lm(price ~ horsepower + enginesize + citympg, CarPriceAssigment)
summary(model_multiple2)
# Daca caii putere ai masinii cresc cu 1 => pretul de vanzare creste cu 43.18 pastrand ceilalati termeni constanti

# Afisarea coeficientilor
coef(model_multiple2)
model_multiple2$coefficients

# Valorile previzionate si reziduurile 
CarPriceAssigment %<>% mutate(pricehat = fitted(model_multiple2),
                              uhat = residuals(model_multiple2))

CarPriceAssigment %>% 
  select(price, pricehat, uhat) %>% 
  head(10)

CarPriceAssigment %>% 
  select(price, pricehat, uhat) %>%
  stargazer(type = "text")
# price = pricehat + uhat
# mean(uhat) = 0 and mean(price) = mean(pricehat)
# Partilizare ----
# price = beta0 + beta1*horsepower + beta2*enginesize + beta3*citympg + u
summary(model_multiple2)

# horsepower = alpha0 + alpha2*enginesize + alpha3*citympg + e
model_partial <- lm(horsepower ~ enginesize + citympg, CarPriceAssigment)
summary(model_partial)

# predict residuals ehat
CarPriceAssigment %<>% mutate(ehat = resid(model_partial))

# price = gamma0 + beta1*ehat + v
lm(price ~ ehat, CarPriceAssigment) %>% summary
# Bonitatea modelului (R-squared si R-squared ajustat) -------------------
# Bonitatea modelului este data de R-squared si R-squared ajuutate 
# care arata ce procent din variatie este explaicat de model

# Regresie simpla cu 1 regresor
summary(model_simple)

# Afisarea lui R-squared
summary(model_simple)$r.squared

# Afisarea lui R-squared ajustat
summary(model_simple)$adj.r.squared

# Regresie multipla cu 2 regresori
summary(model_multiple1)
summary(model_multiple1)$r.squared
summary(model_multiple1)$adj.r.squared

# Regresie multipla cu 3 regresori
summary(model_multiple2)
summary(model_multiple2)$r.squared
summary(model_multiple2)$adj.r.squared

# Calcularea lui R-squared
y    <- CarPriceAssigment$price
yhat <- fitted(model_multiple2)
ehat <- resid(model_multiple2)

SSE <- sum((yhat - mean(y))^2) # sum of squares estimates of errors
SSR <- sum(ehat^2) # sum of squares residual
SST <- SSE + SSR # sum of squares total

# numarul de observatii
n <- nobs(model_multiple2)
n
# gradele de libertate ale modelului = k = nr de regresori
# rangul da numarul de regresori inclusiv constanta
k <- model_multiple2$rank - 1
k

# gradele de libertate pt reziduuri = n - k - 1
df_SSR <- n - k - 1
# gradele de libertate total = n-1 = nr de observ - 1
df_SST <- n - 1

# Formula pt R-squared
R_squared <- SSE/SST
# echivalent cu R-squared = 1 - SSR/SST
R_squared

# Formula pt R-squared ajustat
adj_R_squared <- 1 - (SSR/df_SSR)/(SST/df_SST)
adj_R_squared

CarPriceAssigment %>% 
  select(price, lprice, horsepower, enginesize, lenginesize, citympg, lcitympg) %>%
  head(10)

CarPriceAssigment %>% 
  select(price, lprice, horsepower, enginesize, lenginesize, citympg, lcitympg) %>%
  stargazer(type = "text")

# Forma liniara
model_linear <- lm(price ~ horsepower + enginesize + citympg, CarPriceAssigment)
summary(model_linear)
summary(model_linear)$r.squared
summary(model_linear)$adj.r.squared

# Forma lin-log
model_linear_log <- lm(price ~ horsepower + lenginesize + lcitympg, CarPriceAssigment)
summary(model_linear_log)
summary(model_linear_log)$r.squared
summary(model_linear_log)$adj.r.squared

# Forma log-lin
model_log_linear <- lm(lprice ~ horsepower + enginesize + citympg, CarPriceAssigment)
summary(model_log_linear)
summary(model_log_linear)$r.squared
summary(model_log_linear)$adj.r.squared

# Forma log-log
model_log_log <- lm(lprice ~ horsepower + lenginesize + lcitympg, CarPriceAssigment)
summary(model_log_log)
summary(model_log_log)$r.squared
summary(model_log_log)$adj.r.squared
# Comparam R-patrat si R-patrat ajustat
# SST sunt diferite pt price si log(price)
# Modelul log-log are cel mai mare R-patrat ajustat
# Coliniaritate perfecta ----

# Coliniaritatea perfecta este o relatie liniara exacta intre variabile
# Exemplu de coliniaritate perfecta este atunci cand diesel = 1 - fueltype

# Modelul pentru pretul de vanzare cu variabila aspiration
model_no_collinearity <- lm(price ~ horsepower + enginesize + citympg + fueltype, CarPriceAssigment)
summary(model_no_collinearity)

# Diesel este o functie liniara exacta a var fueltype (Coliniaritate perfecta)
CarPriceAssigment %<>% mutate(diesel = 1 - fueltype)

# Modelul pentru pretul de vanzare cu variabila diesel
model_no_collinearity1 <- lm(price ~ horsepower + enginesize + citympg + diesel, CarPriceAssigment)
summary(model_no_collinearity1)

# Rulam regresia cu diesel si fueltype 
model_collinearity <- lm(price ~ horsepower + enginesize + citympg + fueltype + diesel, CarPriceAssigment)
summary(model_collinearity)
# Modelul nu poate fi estimat deoarece avem coliniaritate perfecta
# R va sterge una din variabile automat 

# Rulam regresia fara constanta (punem 0 pentru constanta)
model_no_constant <- lm(price ~ 0 + horsepower + enginesize + citympg + fueltype + diesel, CarPriceAssigment) 
summary(model_no_constant)
# Nr de observatii > nr variabile independente ----
nobs(model_multiple2) > (model_multiple2$rank - 1)
# Variabilitatea in x este pozitiva ----
var(CarPriceAssigment$horsepower)
var(CarPriceAssigment$enginesize)
var(CarPriceAssigment$citympg)
# toate valorile > 0 => ipoteza acceptata
# Media reziduurilor este 0 ----
mean(model_multiple2$residuals) # medie aproape de 0 => ipoteza acceptata
# Testare multicoliniaritate ----
vif(model_multiple2) # nu avem valori pt VIF > 10 => ipoteza acceptata
# Reziduurile nu sunt corelate cu variabilele independente ----
cor.test(CarPriceAssigment$horsepower, model_multiple2$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(CarPriceAssigment$enginesize, model_multiple2$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(CarPriceAssigment$citympg, model_multiple2$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza acceptata
# Testam ipotezele pe reziduuri si incepem cu homoscedasticitate  ---- 
model_multiple2 <- lm(formula = price ~ horsepower + enginesize + citympg, data = CarPriceAssigment)
summary(model_multiple2)
bptest(model_multiple2) # hetero
white_test(model_multiple2) # hetero
coeftest(model_multiple2, vcov. = vcovHC(model_multiple2, type = "HC1"))# hetero
# ipoteza incalcata => corectie prin WLS (se poate si prin log-log)
model_WLS1 <- lm(formula = price ~ horsepower + enginesize + citympg, 
                 data = CarPriceAssigment, weights = 1/horsepower)

CarPriceAssigment %<>% mutate(pricestar = price/sqrt(enginesize),
                              horsepowerstar = horsepower/sqrt(enginesize),
                              enginesizestar = enginesize/sqrt(enginesize),
                              citympgstar = citympg/sqrt(enginesize),
                              constantstar = 1/sqrt(enginesize))

model_WLS2 <- lm(pricestar ~ 0 + constantstar + horsepowerstar + enginesizestar+ citympgstar, 
                 CarPriceAssigment) 

bptest(model_WLS2) # hetero
white_test(model_WLS2) # hetero
coeftest(model_WLS2, vcov. = vcovHC(model_WLS2, type = "HC1"))# hetero

summary(model_WLS2)
# Multicoliniaritate folosind VIF ----
elemapi2 <- read_excel("CarPriceAssigment.xlsx")

elemapi2 %<>% select(price, horsepower, enginesize, citympg)
stargazer(elemapi2, type = "text")
head(elemapi2, 10)

# Tabel de corelatie
elemapi2 %>% 
  select(-price) %>% 
  na.omit %>% # stergerea valorilor NA
  cor

# Rulam regresia si calculam VIF. Daca VIF>10 atunci trebuie sa stergem var
(model_high_vif <- lm(price ~ horsepower + enginesize + citympg, elemapi2))
vif(model_high_vif)
# Autocorelarea ----
# Incarcam setul de date CarPriceAssigment
data(CarPriceAssigment)

# si variabilele independente horsepower si enginesize si citympg
model1 <- lm(price ~ horsepower + enginesize + citympg, data=CarPriceAssigment)
summary(model1)
# Inspectarea autocorelarii cu ajutorul graficului ACF (autocorelare)
acf(model1$residuals)

# Testul Durbin-Watson (ordinul 1)
dwtest(model1) # p-value < 0.1 => reziduurile sunt autocorelate

# Testul Breusch-Godfrey (order superior)

bgtest(model1) # p-value < 0.1 
bgtest(model1, order = 2) # =>
bgtest(model1, order = 3)
# reziduurile sunt autocorelate si la lag superior

# Corectarea autocorelarii - adaugam lag1 ca variabila independenta in modelul original

# Cream un nou set de date 
econ_data <- data.frame(CarPriceAssigment, resid_mod1=model1$residuals)
# Cream variabila lag1 
econ_data_1 <- slide(econ_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
econ_data_2 <- na.omit(econ_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model2 <- lm(price ~ horsepower + enginesize + citympg + lag1, data=econ_data_2)

# Retestam ipoteza pe noul model
# ACF 
acf(model2$residuals) # autocorelarea a disparut
# Durbin Watson 
dwtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
# Breusch-Godfrey 
bgtest(model2)
bgtest(model2, order = 2)
# Testele Shapiro-Wilk si Jarque Bera pentru detectarea normalitatii ----
CarPriceAssigment %>% 
  select(price, horsepower, enginesize, citympg) %>% 
  head(10)

CarPriceAssigment %>% 
  select(price, horsepower, enginesize, citympg) %>%
  stargazer(type = "text")

# Histograma variabilei price
ggplot(data = CarPriceAssigment) +
  theme_bw() +
  geom_histogram(mapping = aes(x = price), col = 'grey') +
  xlab('Pret vanzare') + 
  ylab('Count') +
  ggtitle('Histograma variabilei pretului de vanzare') + 
  theme(plot.title = element_text(hjust = 0.5))

# Histograma variabilei lprice
ggplot(data = CarPriceAssigment) +
  theme_bw() +
  geom_histogram(mapping = aes(x = lprice), col = 'grey')+
  xlab('LPretvanzare') + 
  ylab('Count') +
  ggtitle('Histograma variabilei LPretvanzare') + 
  theme(plot.title = element_text(hjust = 0.5))


# Testul Shapiro Wilk pentru normalitate
shapiro.test(CarPriceAssigment$price)
shapiro.test(CarPriceAssigment$lprice)

# Testul Jarque-Bera pentru normalitate
jarque.bera.test(CarPriceAssigment$price)
jarque.bera.test(CarPriceAssigment$lprice)

model_multiple2 <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
model_corr = model_multiple2
ols_plot_cooksd_bar(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(9,17,18,19,50,57,58,59,71,72,73,75,102,103,104,106,127,128,129), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(15,27,45,51,59,60,61,62,63,90,111,118,119,186), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(11,14,42,62,88,90,99,100,136,148,149,161,167,168,172), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(8,9,10,11,12,23,37,38,92,83,85,86,128,129,138,156,157), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(5,7,43,52,58,74,75,81,116,118,124,130), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(2,4,28,39,48,49,77,78,108,127,128), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(3,24,68,88,105,107,117), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(35,89,108), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(2,41,58), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

CarPriceAssigment <- CarPriceAssigment[-c(75), ]
model_corr <- lm(price ~ horsepower + enginesize + citympg,CarPriceAssigment)
ols_plot_cooksd_chart(model_corr)

# Testul Shapiro Wilk pentru normalitate
shapiro.test(CarPriceAssigment$price)
shapiro.test(CarPriceAssigment$lprice)

# Testul Jarque-Bera pentru normalitate
jarque.bera.test(CarPriceAssigment$price)
jarque.bera.test(CarPriceAssigment$lprice)
# RMSE - Root Mean Squared Error ----
# Model de regresie de tip log-log pentru setul de antrenare
training.samples <- CarPriceAssigment$lprice %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- CarPriceAssigment[training.samples, ]
test.data <- CarPriceAssigment[-training.samples, ]
model_0 <- lm(lprice ~ lhorsepower + lenginesize + lcitympg, data = CarPriceAssigment) 
summary(model_0)
# Predictia modelului pe setul de testare
y_pred <- predict(model_0, newdata = train.data)
y_pred
RMSE(y_pred, test.data$lprice)
# MAE - Mean Absolute Error ----
# Model de regresie de tip log-log pentru setul de antrenare
training.samples <- CarPriceAssigment$lprice %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- CarPriceAssigment[training.samples, ]
test.data <- CarPriceAssigment[-training.samples, ]
model_0 <- lm(lprice ~ lhorsepower + lenginesize + lcitympg, data = CarPriceAssigment) 
summary(model_0)
# Predictia modelului pe setul de testare
y_pred <- predict(model_0, newdata = train.data)
y_pred
MAE(y_pred, test.data$lprice)
# MSE - Mean Squared Error ----
mse(y_pred, test.data$lprice)
# MAPE - Mean Absolute Percentage Error ----
MAPE(y_pred, test.data$lprice)
# Biasul variabilelor omise -----

# Biasul variabilelor omise se petrecere atunci cand o variabila omisa
# produce bias in randul coeficientilor

HTV <- read_excel("CarPriceAssigment.xlsx")
HTV %<>% select(price, horsepower, enginesize, citympg)
stargazer(HTV, type = "text")
head(HTV)

# Modelul ce cuprinde puterea in kw a masinii si cilindreea motorului si consumul in oras
# price = beta0 + beta1*horsepower + beta2*enginesize + beta3*citympg + u
model_true <- lm(price ~ horsepower + enginesize + citympg, HTV)
summary(model_true)
beta1 <- coef(model_true)["horsepower"]
beta1
beta2 <- coef(model_true)["enginesize"]
beta2
beta3 <- coef(model_true)["citympg"]
beta3

# Modelul dintre cilindreea motorului si puterea masinii
# enginesize = delta0 + delta1*horsepower + delta2*citympg + v
model_abil <- lm(enginesize ~ horsepower + citympg, HTV)
summary(model_abil)
delta1 <- coef(model_abil)["horsepower"]
delta1
delta2 <- coef(model_abil)["citympg"]
delta2

# Modelul in care cilindreea motorului este omisa, deci coeficientul variabilei 
# ce contorizeaza nivelul de cai putere este biased
# price = (beta0+beta2*delta0) + (beta1+beta2*delta1)*horsepower +(beta2*v+u)
model_omitted <- lm(price ~ horsepower + citympg, HTV)
summary(model_omitted)
beta1_biased <- coef(model_omitted)["horsepower"]
beta1_biased

# Calcularea biasului si a coeficientilor biased
bias <- beta2*delta1
bias

beta1_biased_calculated <- beta1 + beta2*delta1
beta1_biased_calculated 


# Modelul corect specificat cu horsepower si enginesize si citympg
# model_multiple2 <- lm(price ~ horsepower + enginesize + citympg, CarPriceAssigment)
summary(model_multiple2)

# Regresia dintre cilindreea motorului si puterea maxima si consumul pe oras
model_enginesize <- lm(enginesize ~ horsepower + citympg, CarPriceAssigment)
summary(model_enginesize)

# Modelul cu enginesize omisa => coeficientul horsepower este biased
# Same as the model 'model_simple'
# model_simple <- lm(price ~ horsepower, CarPriceAssigment)
summary(model_simple)
# Varianta gresit specificata ----
# Folosim dataframeul 'HTV'

# Modelul corect cu puterea maxima
# La fel ca 'model_true'
# model_true <- lm(price ~ horsepower + enginesize + citympg, HTV)
summary(model_true)

# Modelul cu var enginesize omisa si citympg omisa
model_omitted <- lm(price ~ horsepower, HTV)
summary(model_omitted)

# 7. Imbunatatirea regresiei multiple prin adoptarea altei forme functionale si prin adaugarea unor variabile dummy ----
CarPriceAssigment %>% 
  select(price, horsepower, enginesize, citympg, fueltype) %>% 
  head(10) 
# horsepower - exprimata in number
# enginesize - exprimata in number
# citympg - exprimata in float
# fueltype - exprimata in 1 si 0 (1 -> gas ; 0 -> diesel)

CarPriceAssigment %>% 
  select(price, horsepower, enginesize, citympg, fueltype) %>%
  str

CarPriceAssigment %>% 
  select(price, horsepower, enginesize, citympg, fueltype) %>%
  stargazer(type = "text")

# Comparam rezultatele dintre regresia simpla si cea multipla cu variabila dummy
# Daca puterea maxima creste cu 1, cu cati dolari creste pretul de vanzare?

# Regresia simpla
model_simple <- lm(price ~ horsepower, CarPriceAssigment)
summary(model_simple)

# Daca puterea maxima creste cu 1 => pretul de vanzare creste cu 163.263

# Regresia multipla
model_multiple2 <- lm(price ~ horsepower + enginesize + citympg, CarPriceAssigment)
summary(model_multiple2)
# Daca puterea maxima creste cu 1 => pretul de vanzare creste cu 43.18 pastrand ceilalati termeni constanti

# Regresie multipla cu variabila dummy
model_dummy <- lm(price ~ horsepower + enginesize + + citympg + fueltype, CarPriceAssigment)
summary(model_dummy)
# Daca puterea maxima creste cu 1 => pretul de vanzare creste cu 54.05 pastrand ceilalati termeni constanti

# Afisarea coeficientilor
coef(model_dummy)
model_dummy$coefficients

# Valorile previzionate si reziduurile 
CarPriceAssigment %<>% mutate(pricehat = fitted(model_dummy),
                              uhat = residuals(model_dummy))

CarPriceAssigment %>% 
  select(price, pricehat, uhat) %>% 
  head(10)

CarPriceAssigment %>% 
  select(price, pricehat, uhat) %>%
  stargazer(type = "text")
# price = pricehat + uhat
# mean(uhat) = 0 and mean(price) = mean(pricehat)
# Partilizare ----
# price = beta0 + beta1*horsepower + beta2*enginesize + beta3*citympg + beta4*fueltype + u
summary(model_dummy)

# horsepower = alpha0 + alpha2*enginesize + alpha3*citympg + alpha4*fueltype + e
model_partial <- lm(horsepower ~ enginesize + citympg + fueltype, CarPriceAssigment)
summary(model_partial)

# predict residuals ehat
CarPriceAssigment %<>% mutate(ehat = resid(model_partial))

# price = gamma0 + beta1*ehat + v
lm(price ~ ehat, CarPriceAssigment) %>% summary
# Bonitatea modelului (R-squared si R-squared ajustat) -------------------
# Regresie simpla cu 1 regresor
summary(model_simple)

# Afisarea lui R-squared
summary(model_simple)$r.squared

# Afisarea lui R-squared ajustat
summary(model_simple)$adj.r.squared

# Regresie multipla cu 2 regresori
summary(model_multiple2)
summary(model_multiple2)$r.squared
summary(model_multiple2)$adj.r.squared

# Regresie multipla cu 3 regresori
summary(model_dummy)
summary(model_dummy)$r.squared
summary(model_dummy)$adj.r.squared

# Calcularea lui R-squared
y    <- CarPriceAssigment$price
yhat <- fitted(model_dummy)
ehat <- resid(model_dummy)

SSE <- sum((yhat - mean(y))^2) # sum of squares estimates of errors
SSR <- sum(ehat^2) # sum of squares residual
SST <- SSE + SSR # sum of squares total

# numarul de observatii
n <- nobs(model_dummy)
n
# gradele de libertate ale modelului = k = nr de regresori
# rangul da numarul de regresori inclusiv constanta
k <- model_multiple2$rank - 1
k

# gradele de libertate pt reziduuri = n - k - 1
df_SSR <- n - k - 1
# gradele de libertate total = n-1 = nr de observ - 1
df_SST <- n - 1

# Formula pt R-squared
R_squared <- SSE/SST
# echivalent cu R-squared = 1 - SSR/SST
R_squared

# Formula pt R-squared ajustat
adj_R_squared <- 1 - (SSR/df_SSR)/(SST/df_SST)
adj_R_squared

CarPriceAssigment %>% 
  select(price, lprice, horsepower, enginesize, lenginesize, citympg, lcitympg, fueltype) %>%
  head(10)

CarPriceAssigment %>% 
  select(price, lprice, horsepower, enginesize, lenginesize, citympg, lcitympg, fueltype) %>%
  stargazer(type = "text")

# Forma liniara
model_linear <- lm(price ~ horsepower + enginesize + citympg + fueltype, CarPriceAssigment)
summary(model_linear)
summary(model_linear)$r.squared
summary(model_linear)$adj.r.squared

# Forma lin-log
model_linear_log <- lm(price ~ horsepower + lenginesize + lcitympg + fueltype, CarPriceAssigment)
summary(model_linear_log)
summary(model_linear_log)$r.squared
summary(model_linear_log)$adj.r.squared

# Forma log-lin
model_log_linear <- lm(lprice ~ horsepower + enginesize + citympg + fueltype, CarPriceAssigment)
summary(model_log_linear)
summary(model_log_linear)$r.squared
summary(model_log_linear)$adj.r.squared

# Forma log-log
model_log_log <- lm(lprice ~ horsepower + lenginesize + lcitympg + fueltype, CarPriceAssigment)
summary(model_log_log)
summary(model_log_log)$r.squared
summary(model_log_log)$adj.r.squared

# Comparam R-patrat si R-patrat ajustat
# SST sunt diferite pt price si log(price)
# Modelul log-log are cel mai mare R-patrat ajustat
# Coliniaritate perfecta ----

# Coliniaritatea perfecta este o relatie liniara exacta intre variabile
# Exemplu de coliniaritate perfecta este atunci cand rear = 1 - enginelocation
# Modelul pentru pretul de vanzare cu variabila enginelocation
model_no_collinearity <- lm(price ~ horsepower + enginesize + citympg + fueltype + enginelocation, CarPriceAssigment)
summary(model_no_collinearity)

# Rear este o functie liniara exacta a var enginelocation (Coliniaritate perfecta)
CarPriceAssigment %<>% mutate(rear = 1 - enginelocation)

# Modelul pentru pretul de vanzare cu variabila rear
model_no_collinearity1 <- lm(price ~ horsepower + enginesize + citympg + rear, CarPriceAssigment)
summary(model_no_collinearity1)

# Rulam regresia cu rear si fueltype 
model_collinearity <- lm(price ~ horsepower + enginesize + citympg + fueltype + rear, CarPriceAssigment)
summary(model_collinearity)
# Modelul nu poate fi estimat deoarece avem coliniaritate perfecta
# R va sterge una din variabile automat 

# Rulam regresia fara constanta (punem 0 pentru constanta)
model_no_constant <- lm(price ~ 0 + horsepower + enginesize + citympg + fueltype + rear, CarPriceAssigment) 
summary(model_no_constant)
# Nr de observatii > nr variabile independente ----
nobs(model_dummy) > (model_dummy$rank - 1)
# Variabilitatea in x este pozitiva ----
var(CarPriceAssigment$horsepower)
var(CarPriceAssigment$enginesize)
var(CarPriceAssigment$citympg)
var(CarPriceAssigment$fueltype)
# toate valorile > 0 => ipoteza acceptata
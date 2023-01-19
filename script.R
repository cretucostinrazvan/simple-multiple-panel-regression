# Setare fisier de lucru
rm(list = ls()) 
directory <- "C:/Users/alexd/Desktop/econometrie/proiect_econometrie/"

# Instalarea pachetelor
PackageNames <- c("readxl","dplyr","tidyverse", "stargazer", "magrittr", "ggplot2", "lmtest", "DataCombine", "dplyr", "tseries", "caret", "car", "olsrr", "moments", "whitestrap", "plm", "nortest", "Metrics", "MLmetrics", "glmnet")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

CarPriceAssigment <- read_excel("CarPriceAssigment.xlsx")
#4.Regresia Simpla---------------------------------------

#model_regs_wheelbase <- lm(price ~ wheelbase,CarPriceAssigment)
#summary(model_regs_wheelbase)
# R^2: 33.39%

#model_regs_carlength <- lm(price ~ carlength,CarPriceAssigment)
#summary(model_regs_carlength)
# R^2: 46.64%

#model_regs_carwidth <- lm(price ~ carwidth,CarPriceAssigment)
#summary(model_regs_carwidth)
# R^2: 57.66%

#model_regs_carheight <- lm(price ~ carheight,CarPriceAssigment)
#summary(model_regs_carheight)
# R^2: 1.42%

#model_regs_curbweight <- lm(price ~ curbweight,CarPriceAssigment)
#summary(model_regs_curbweight)
# R^2: 69.77%

#model_regs_enginesize <- lm(price ~ enginesize, CarPriceAssigment)
#summary(model_regs_enginesize)
# R^2: 76.41%

#model_regs_boreratio <- lm(price ~ boreratio, CarPriceAssigment)
#summary(model_regs_boreratio)
# R^2: 30.6%

#model_regs_stroke <- lm(price ~ stroke, CarPriceAssigment)
#summary(model_regs_stroke)
# R^2: 0.63%

#model_regs_compressionratio <- lm(price ~ compressionratio, CarPriceAssigment)
#summary(model_regs_compressionratio)
# R^2: 0.46%

#model_regs_horsepower <- lm(price ~ horsepower,CarPriceAssigment)
#summary(model_regs_horsepower)
# R^2: 65.31%

#model_regs_peakrpm <- lm(price ~ peakrpm,CarPriceAssigment)
#summary(model_regs_peakrpm)
# R^2: 0.72%

#model_regs_citympg <- lm(price ~ citympg,CarPriceAssigment)
#summary(model_regs_citympg)
# R^2: 47.03%

#model_regs_highwaympg <- lm(price ~ highwaympg,CarPriceAssigment)
#summary(model_regs_highwaympg)
# R^2: 48.66%

CarPriceAssigment %<>% select(price, enginesize)
str(CarPriceAssigment)
stargazer(CarPriceAssigment, type = "text")
head(CarPriceAssigment, 10)

cor(CarPriceAssigment)
CarPriceAssigment %<>% mutate(avg_price = mean(price))
#price = beto0 + beta1 * enginesize + u
model_regs_enginesize <-lm(price ~ enginesize,CarPriceAssigment)
summary(model_regs_enginesize)
# Nivel semnificatie: 99%
# Bonitatea modelului: 76.41
# Valid dpdv statistic, p-value = 2.2e-16 <0.01

# Graficul observatiilor cu dreapta estimata
plot(x = CarPriceAssigment$enginesize, y = CarPriceAssigment$price)
abline(a = model_regs_enginesize$coefficients['(Intercept)'], 
       b = model_regs_enginesize$coefficients['enginesize'],
       col = 'red')

# pachetul GGPLOT2 care ne ajuta sa obtinem grafice mult mai aspectuase
ggplot(data = CarPriceAssigment, mapping = aes(x = enginesize, y = price)) +
  theme_bw() + # setarea temei graficului
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Predictie dupa regresie ------------------------------------------

# Valoarea estimata pentru variabila dependenta (Scorehat)
CarPriceAssigment %<>% mutate(pricehat = fitted(model_regs_enginesize))
stargazer(CarPriceAssigment, type = "text")
ggplot(data = CarPriceAssigment, mapping = aes(x = enginesize)) +
  geom_point(mapping = aes(y = price, color = 'price - actual value')) +
  geom_point(mapping = aes(y = pricehat, color = 'pricehat - predicted value')) + 
  xlab('enginesize')

# Reziduuri
CarPriceAssigment %<>% mutate(uhat = residuals(model_regs_enginesize))
stargazer(CarPriceAssigment, type = "text")
ggplot(CarPriceAssigment, aes(x = enginesize)) +
  geom_point(aes(y = price, col = 'price - actual value')) +
  geom_point(aes(y = uhat, col = 'Residual uhat')) +
  xlab('enginesize')


# Graficul valorilor si reziduurilor reale si previzionate
ggplot(CarPriceAssigment, aes(x = enginesize)) +
  geom_point(aes(y = price, color = 'price - actual value')) +
  geom_point(aes(y = pricehat, color = 'pricehat - predicted value')) +
  geom_point(aes(y = uhat, color = 'Residual uhat')) +
  geom_smooth(aes(y = price, color = 'Fitted line'), 
              method = "lm", se = FALSE) +
  xlab('enginesize')

#Bonitatea modelului (R-squared) ----------------------------------
summary(model_regs_enginesize)$r.squared
#Forma functionala log: log-log si log-lin -------------------------------

CarPriceAssigment %<>% mutate(lprice = log(price), lenginesize=log(enginesize))

# Forma liniara
model_regs_enginesize_lin_lin <- lm(price ~ enginesize, CarPriceAssigment)
summary(model_regs_enginesize_lin_lin)
#Grafic
ggplot(CarPriceAssigment, aes(x = enginesize, y = price)) +
  theme_bw() +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)


# Log-log 
model_regs_enginesize_log_log <- lm(lprice ~ lenginesize, CarPriceAssigment)
summary(model_regs_enginesize_log_log)
# Grafic
ggplot(CarPriceAssigment, aes(x = lenginesize, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Linear-log 
model_regs_enginesize_lin_log <- lm(price ~ lenginesize, CarPriceAssigment)
summary(model_regs_enginesize_lin_log)
# Grafic
ggplot(CarPriceAssigment, aes(x = lenginesize, y = price)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

# Log-linear 
model_regs_enginesize_log_lin <- lm(lprice ~ enginesize, CarPriceAssigment)
summary(model_regs_enginesize_log_lin)
# Grafic
ggplot(CarPriceAssigment, aes(x = enginesize, y = lprice)) +
  geom_point() +
  geom_smooth(method = lm, se = F)

rm(model_regs_enginesize_lin_lin)
rm(model_regs_enginesize_log_log)
rm(model_regs_enginesize_lin_log)
rm(model_regs_enginesize_log_lin)
#Ipoteza daca modelul liniar este in parametrii----
# Da, deoarece poate fi scris ca o functie liniara 
# price = -8005.446 + 167.698 * enginesize
#Ipoteza pentru numarul de observatii mai mare decat numarul de variabile independente----
nobs(model_regs_enginesize) > (model_regs_enginesize$rank - 1)
#Ipoteza pentru corectitudinea specificarii modelului----
# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este in cazul meu
#Ipoteza pentru variabilitate----
var(CarPriceAssigment$enginesize) # variabilitatea in x este pozitiva (1734.114 > 0) => ipoteza acceptata
#Ipoteza pentru media reziduurilor----
mean(model_regs_enginesize$residuals) # media -1.105111e-13 aproape de 0 => ipoteza acceptata
#Ipoteza pentru corelare intre reziduuri si variabile independente----
cor.test(CarPriceAssigment$enginesize, model_regs_enginesize$residuals) # p-value 1 > 0.1 => nu sunt corelate => ipoteza acceptata
#Ipoteza pentru homoscedasticitate----

# Graficul reziduurilor fata de variabila independenta enginesize
ggplot(data = CarPriceAssigment, mapping = aes(x = enginesize, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'enginesize')

# Graficul reziduurilor fata de valorile estimate de model
ggplot(data = CarPriceAssigment, mapping = aes(x = pricehat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')

bptest(model_regs_enginesize) # 3.925e-08 < 0.01 => hetero
white_test(model_regs_enginesize) # 0 < 0.01 => hetero
coeftest(model_regs_enginesize, vcov. = vcovHC(model_regs_enginesize, type = "HC1")) # hetero
# ipoteza incalcata => corectie prin WLS
model_WLS1 <- lm(formula = price ~ enginesize, data = CarPriceAssigment, weights=1/enginesize)
summary(model_WLS1)
bptest(model_WLS1) # 0.5306 > 0.01 => homo
white_test(model_WLS1) # 0 < 0.01 => hetero

CarPriceAssigment %<>% mutate(price.star = price/sqrt(enginesize),
                              enginesize.star = enginesize/sqrt(enginesize),
                              constant.star = 1/sqrt(enginesize))

model_WLS2 <- lm(price.star ~ 0 + constant.star + enginesize.star ,CarPriceAssigment)
summary(model_WLS2)
bptest(model_WLS2) # 0.09204 > 0.01 => homo
white_test(model_WLS2) # 0.018941 > 0.01 => homo

rm(model_WLS1)
rm(model_WLS2)
#Ipoteza pentru autocorelare----
acf(model_regs_enginesize$residuals)
dwtest(model_regs_enginesize) # 2.2e-16 < 0.1 => reziduuri autocorelate 
bgtest(model_regs_enginesize) # 2.2e-16 < 0.1 => reziduuri autocorelate
bgtest(model_regs_enginesize,order = 2) # 2.2e-16 < 0.1 => reziduuri autocorelate

CarPriceAssigment <- data.frame(CarPriceAssigment, resid_mod1=model_regs_enginesize$residuals)
CarPriceAssigment <- slide(CarPriceAssigment, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
CarPriceAssigment <- na.omit(CarPriceAssigment) # eliminam valorile NA
model_regs_enginesize2 <- lm(price ~ enginesize + lag1, data=CarPriceAssigment)

acf(model_regs_enginesize2$residuals)
dwtest(model_regs_enginesize2) # 0.6534 > 0.1 => reziduuri nonautocorelate 
bgtest(model_regs_enginesize2) # 0.3648 > 0.1 => reziduuri nonautocorelate
bgtest(model_regs_enginesize2,order = 2) # 0.04171 > 0.1
#Ipoteza pentru normalitate----
# Residuals vs Fitted + Q-Q plot
plot(model_regs_enginesize2)
#Histograma
ols_plot_resid_hist(model_regs_enginesize2)

ggplot(data = CarPriceAssigment) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))

#Skewness---
skewness(CarPriceAssigment$uhat) # 0.7142053

#Kurtosis---
kurtosis(CarPriceAssigment$uhat) # 4.326084

#BoxPlot
ols_plot_resid_box(model_regs_enginesize2)

jarque.bera.test(model_regs_enginesize2$residuals) # p-value 8.057e-08 reziduurile nu sunt normale distribuite
# trebuie corectie
ols_plot_cooksd_bar(model_regs_enginesize2)

CarPriceAssigment_cook <- CarPriceAssigment[-c(8,16,18,48,49,50,55,57,58,59,70,73,74,101,125,126,127,128,130,138), ]
model_regs_enginesize3 <- lm(price ~ enginesize + lag1, CarPriceAssigment_cook)
jarque.bera.test(model_regs_enginesize3$residuals) # 0.07432 > 0.05 reziduurile sunt normal distribuite
ols_plot_cooksd_bar(model_regs_enginesize3)

rm(CarPriceAssigment_cook)
#5.Prognoze----
set.seed(123)
training.samples <- CarPriceAssigment_cook$lprice %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- CarPriceAssigment_cook[training.samples, ]
test.data <- CarPriceAssigment_cook[-training.samples, ]

# Model de regresie de tip log-log pentru setul de antrenare
model_0 <- lm(lprice ~ lenginesize, data = train.data) 
summary(model_0)

# Predictia modelului pe setul de testare
y_pred <- predict(model_0, newdata = test.data)
y_pred

RMSE(y_pred, test.data$lprice) # 0.2590731 < 1 bine

MAE(y_pred, test.data$lprice)# 0.2010948 < 1 bine

MSE(y_pred, test.data$lprice)# 0.0671189 > 0 nu e bine

MAPE(y_pred, test.data$lprice)# 0.02148163 < 1 bine

out_of_sample <- data.frame(enginesize = c(70,200,300))

# Prognoza
y_pred_outsample <- predict(model_regs_enginesize, newdata = out_of_sample)
y_pred_outsample

rm(model_0)
rm(model_regs_enginesize)
rm(out_of_sample)
rm(train.data)
rm(test.data)
rm(training.samples)
rm(y_pred)
rm(y_pred_outsample)
rm(model_regs_enginesize2)
rm(model_regs_enginesize3)
rm(model_regs_enginesize4)
#6.Imbunatatirea regresiei simple prin adaugarea unor noi variabile independente(Regresie multipla)----
CarPriceAssigment %<>% select(price, wheelbase, carlength, carwidth, carheight, curbweight, enginesize, boreratio, stroke, compressionratio, horsepower, peakrpm, citympg, highwaympg)

model_regm_enginesize <-lm(price ~ enginesize,CarPriceAssigment)
summary(model_regm_enginesize)
# cand pretul creste cu o unitate, enginesize creste cu 167.698

model_ptr_det <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + boreratio + stroke + compressionratio + horsepower + peakrpm + citympg + highwaympg, CarPriceAssigment)
summary(model_ptr_det)
# de aici rezulta faptul ca sunt semnificative 99% enginesize, stroke, compressionratio si peakrpm

model_regm_enginesize2  <- lm(price ~ enginesize + stroke,CarPriceAssigment)
summary(model_regm_enginesize2)
# cand pretul creste cu o unitate, enginesize creste cu 171.687

model_regm_enginesize3  <- lm(price ~ enginesize + stroke + compressionratio,CarPriceAssigment)
summary(model_regm_enginesize3)
# cand pretul creste cu o unitate, enginesize creste cu 171.798

#model_regm_enginesize_stroke_compressionratio <- lm(price ~ enginesize + stroke + compressionratio, CarPriceAssigment)
#summary(model_regm_enginesize_stroke_compressionratio)
# R^2 = 77.8

#model_regm_enginesize_stroke_peakrpm <- lm(price ~ enginesize + stroke + peakrpm, CarPriceAssigment)
#summary(model_regm_enginesize_stroke_peakrpm)
# R^2 = 79.13

#model_regm_enginesize_compressionratio_peakrpm <- lm(price ~ enginesize + compressionratio + peakrpm, CarPriceAssigment)
#summary(model_regm_enginesize_compressionratio_peakrpm)
# R^2 = 79.45

model_regm_enginesize3 <- lm(price ~ enginesize + compressionratio + peakrpm,CarPriceAssigment)
summary(model_regm_enginesize3)
#Multicoliniaritate folosind VIF-----
vif(model_regm_enginesize3)

coef(model_regm_enginesize3)

CarPriceAssigment %<>% mutate(pricehat = fitted(model_regm_enginesize3),
                    uhat = residuals(model_regm_enginesize3))

CarPriceAssigment %>% select(price, pricehat, uhat) %>% stargazer(type = "text")

# Regresie simpla cu 1 regresor
summary(model_regm_enginesize)

# Afisarea lui R-squared
summary(model_regm_enginesize)$r.squared

# Afisarea lui R-squared ajustat
summary(model_regm_enginesize)$adj.r.squared

# Regresie multipla cu 2 regresori
summary(model_regm_enginesize2)
summary(model_regm_enginesize2)$r.squared
summary(model_regm_enginesize2)$adj.r.squared

# Regresie multipla cu 3 regresori
summary(model_regm_enginesize3)
summary(model_regm_enginesize3)$r.squared
summary(model_regm_enginesize3)$adj.r.squared

rm(model_regm_enginesize)
rm(model_regm_enginesize2)
#Coliniaritate perfecta----
# Coliniaritatea perfecta este o relatie liniara exacta intre variabile
# Exemplu de coliniaritate perfecta este atunci cand diesel = 1 - fueltype

model_no_collinearity <- lm(price ~ enginesize + compressionratio + peakrpm + fueltype, CarPriceAssigment)
summary(model_no_collinearity)

# Diesel este o functie liniara exacta a var fueltype (Coliniaritate perfecta)
CarPriceAssigment %<>% mutate(diesel = 1 - fueltype)

model_no_collinearity1 <- lm(price ~ enginesize + compressionratio + peakrpm + diesel, CarPriceAssigment)
summary(model_no_collinearity1)

# Rulam regresia cu diesel si fueltype 
model_collinearity <- lm(price ~ enginesize + compressionratio + peakrpm + fueltype + diesel, CarPriceAssigment)
summary(model_collinearity)
# Modelul nu poate fi estimat deoarece avem coliniaritate perfecta
# R va sterge una din variabile automat 

# Rulam regresia fara constanta (punem 0 pentru constanta)
model_no_constant <- lm(price ~ 0 + enginesize + compressionratio + peakrpm + fueltype + diesel, CarPriceAssigment) 
summary(model_no_constant)

rm(model_no_collinearity)
rm(model_no_collinearity1)
rm(model_collinearity)
rm(model_no_constant)
#Ipoteza daca modelul liniar este in parametrii----
# Da, deoarece poate fi scris ca o functie liniara 
# price = -2.832e+04 + 1.761e+02 * enginesize + 2.535e+02 * compressionratio + 3.253e+00 * peakrpm
#Ipoteza pentru numarul de observatii mai mare decat numarul de variabile independente----
nobs(model_regm_enginesize3) > (model_regm_enginesize3$rank - 1)
#Ipoteza pentru corectitudinea specificarii modelului----
# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este in cazul meu
#Ipoteza pentru variabilitate----
# Variabilitatea in x este pozitiva
var(CarPriceAssigment$enginesize) # variabilitatea in x este pozitiva (1734.114 > 0) => ipoteza acceptata
var(CarPriceAssigment$compressionratio) # variabilitatea in x este pozitiva (15.7771 > 0) => ipoteza acceptata
var(CarPriceAssigment$peakrpm) # variabilitatea in x este pozitiva (227515.3 > 0) => ipoteza acceptata
#Ipoteza pentru media reziduurilor----
mean(model_regm_enginesize3$residuals) # media -9.34602e-14 aproape de 0 => ipoteza acceptata
#Ipoteza pentru multicoliniaritate----
vif(model_regm_enginesize3) # nu avem valori pt VIF > 10 => ipoteza acceptata
#Ipoteza pentru corelare intre reziduuri si variabile independente----
cor.test(CarPriceAssigment$enginesize, model_regm_enginesize3$residuals) # p-value 1 > 0.1 => nu sunt corelate => ipoteza acceptata
cor.test(CarPriceAssigment$compressionratio, model_regm_enginesize3$residuals) # p-value 1 > 0.1 => nu sunt corelate => ipoteza acceptata
cor.test(CarPriceAssigment$peakrpm, model_regm_enginesize3$residuals) # p-value 1 > 0.1 => nu sunt corelate => ipoteza acceptata
#Ipoteza pentru homoscedasticitate----
bptest(model_regm_enginesize3) # 2.945e-11 < 0.01 => hetero
white_test(model_regm_enginesize3) # 0 < 0.01 => hetero
coeftest(model_regm_enginesize3, vcov. = vcovHC(model_regm_enginesize3, type = "HC1"))# hetero

CarPriceAssigment %<>% mutate(price.star = price/sqrt(compressionratio),
                    enginesize.star = enginesize/sqrt(compressionratio),
                    compressionratio.star = compressionratio/sqrt(compressionratio),
                    peakrpm.star = peakrpm/sqrt(compressionratio), 
                    constantstar = 1/sqrt(compressionratio))

model_WLS2 <- lm(price.star ~ 0 + constantstar + enginesize.star + compressionratio.star + peakrpm.star,
                 CarPriceAssigment)
bptest(model_WLS2) # 2.637e-10 < 0.01 => hetero
white_test(model_WLS2) # 0 < 0.01 => hetero

model_WLS_log_log <-lm(log(price) ~ log(enginesize) + log(compressionratio) + log(peakrpm),CarPriceAssigment)
bptest(model_WLS_log_log) # 0.02408 > 0.01 => homo
white_test(model_WLS_log_log) # 0.061302 > 0.01 => homo

rm(model_WLS2)
rm(model_WLS_log_log)
#Ipoteza pentru autocorelare----
acf(model_regm_enginesize3$residuals) # sunt autocorelate
dwtest(model_regm_enginesize3) # 2.2e-16 < 0.1 => reziduuri autocorelate
bgtest(model_regm_enginesize3) # 2.2e-16 < 0.1 => reziduuri autocorelate 
bgtest(model_regm_enginesize3, order=2)

CarPriceAssigment <- data.frame(CarPriceAssigment, resid_mod1=model_regm_enginesize3$residuals)
CarPriceAssigment <- slide(CarPriceAssigment, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
CarPriceAssigment <- na.omit(CarPriceAssigment) # eliminam valorile NA
model_regm_enginesize4 <- lm(price ~ enginesize + compressionratio + peakrpm + lag1, data = CarPriceAssigment)

acf(model_regm_enginesize4$residuals) # nu sunt autocorelate
dwtest(model_regm_enginesize4) # 0.841 > 0.1 => reziduuri nonautocorelate 
bgtest(model_regm_enginesize4) # 0.0335 > 0.1 => reziduuri nonautocorelate 
#Ipoteza pentru normalitate----
jarque.bera.test(model_regm_enginesize4$residuals) # p-value 9.67e-08 reziduurile nu sunt normale distribuite
# trebuie corectie
ols_plot_cooksd_bar(model_regm_enginesize4)
CarPriceAssigment_cook2 <- CarPriceAssigment[-c(14,16,49,50,59,68,70,73,74,101,127,128,165,192,203), ]
model_regm_enginesize5<-lm(price ~ enginesize + compressionratio + peakrpm + lag1, data=CarPriceAssigment_cook2)
jarque.bera.test(model_regm_enginesize5$residuals) # 0.0889 > 0.05 reziduurile sunt normal distribuite
ols_plot_cooksd_bar(model_regm_enginesize5)
# Regresia Ridge----
# Regresie liniara multipla
model0 <- lm(price ~ enginesize + compressionratio + peakrpm , CarPriceAssigment)
summary(model0)
prognoza <- data.frame(enginesize = c(100),
                       compressionratio = c(5),
                       peakrpm = c(5000))
y_pred_scenariu <- predict(model0, newdata = prognoza)
y_pred_scenariu

# Definim variabila raspuns
y <- CarPriceAssigment$price

# Definim predictorii
x <- data.matrix(CarPriceAssigment[, c('enginesize', 'compressionratio', 'peakrpm')])

# Estimam modelul ridge (alpha = 0)
model <- glmnet(x, y, alpha = 0)
summary(model)

# In continuare vom identifica valoarea lui lambda pt care avem MSE minimizat
# utilizand validarea incrucisata (cross validation)
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 0.7944065

# testarea valorii lamda 
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:3, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(model, s = best_lambda, newx = x)

# Progoza out-of-sample
new <- matrix(c(100, 5, 5000), nrow=1, ncol=6) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 37.23%

# Regresia LASSO----
# SSR + lambda*sum(|beta|)
model <- glmnet(x, y, alpha = 1)

# Din punct de vedere tehnic, vom seta valoarea alpha = 1 pentru 
# regresia LASSO. 
cv_model <- cv.glmnet(x, y, alpha = 1)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 21.77486

# testarea valorii lamda
plot(cv_model) 


# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 
# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:3, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample
#'enginesize', 'compressionratio', 'peakrpm'
new <- matrix(c(100, 5, 5000), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 79.44445%
# Elastic net regression----
# adauga ambele penalitati SSR + lambda*sum(beta^2) + lambda*sum(|beta|)
model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 30.01714


# testarea valorii lamda
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample
#'enginesize', 'compressionratio', 'peakrpm'
new <- matrix(c(100, 5, 5000), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 79.44065%

# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim 
# Algoritmul Boruta---- 
CarPriceAssigmentCSV <- read.csv(paste0(directory, "CarPriceAssigment.csv"))
CarPriceAssigmentCSV[,convert] <- data.frame(apply(CarPriceAssigmentCSV[convert], 2, as.factor))

# Vom converti variabilele categoricale in variabile factor 
convert <- c(5:10)

library(Boruta)
set.seed(111)
boruta.bank_train <- Boruta(price~., data = CarPriceAssigmentCSV, doTrace = 2)
print(boruta.bank_train)

# Vom selecta atributele importante 
getSelectedAttributes(boruta.bank_train, withTentative = T)

# Vom reimplementa un model de regresie cu aceste atribute
model_boruta <- lm(price ~ car_ID + symboling + CarName + fueltype + aspiration + carbody + drivewheel + drivewheel + enginelocation + wheelbase + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke + compressionratio + horsepower +  citympg + highwaympg + lprice + lenginesize + lcompressionratio + lpeakrpm, CarPriceAssigmentCSV)
summary(model_boruta) 
#7.Imbunatatirea regresiei multiple prin adoptarea altei forme functionale si prin adaugarea unei variabile dummy----
model_ptr_det2 <- lm(price ~ fueltype + enginelocation, CarPriceAssigment)
summary(model_ptr_det2)
# de aici rezulta faptul ca enginelocation semnifica 99%

model_1 <- lm(price ~ enginesize + compressionratio + peakrpm + enginelocation, data = CarPriceAssigment_cook2) 
summary(model_1)
#Ipoteza pentru variabilitate----
var(CarPriceAssigment$enginesize) # variabilitatea in x este pozitiva (1734.114 > 0) => ipoteza acceptata
var(CarPriceAssigment$compressionratio) # variabilitatea in x este pozitiva (15.7771 > 0) => ipoteza acceptata
var(CarPriceAssigment$peakrpm) # variabilitatea in x este pozitiva (227515.3 > 0) => ipoteza acceptata
var(CarPriceAssigment$enginelocation) # variabilitatea in x este pozitiva (0.01449067 > 0) => ipoteza acceptata
#Ipoteza pentru numarul de observatii mai mare decat numarul de variabile independente----
nobs(model_1) > (model_1$rank - 1)
#cand pretul creste cu o unitate, enginelocation scade cu 1.014e+04
#Ipoteza pentru homoscedasticitate----
bptest(model_1)  # 2.548e-08 < 0.1 => hetero
white_test(model_1) # 0 < 0.1 =>hetero
#Ipoteaza pentru autocorelare----
acf(model_1$residuals) 
dwtest(model_1) # 2.2e-16 < 0.1 => reziduuri autocorelate 
bgtest(model_1, order=2) # 2.2e-16 < 0.1 => reziduuri autocorelate
bgtest(model_1, order=3) # 2.2e-16 < 0.1 => reziduuri autocorelate
bgtest(model_1, order=4) # 2.2e-16 < 0.1 => reziduuri autocorelate
#Ipoteza pentru normalitate----
jarque.bera.test(model_1$residuals) # p-value 1.698e-05 reziduurile nu sunt normale distribuite
ols_test_normality(model_1) # > 0.1 1/4
ols_plot_cooksd_bar(model_1)
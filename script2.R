# Setare fisier de lucru
rm(list = ls())

# Install packages
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Exemplu: Identificarea impactul variabilelor macroeconomice majore asupra mișcării prețurilor indicelui bursier.
# la nivelul celor 9 tari in perioada 2010-2020
data <- read_excel("D:/Facultate/Econometrie/proiect_econometrie/economic_data_9_countries.xlsx")

# Statistici descriptive
summary(data)

# Declararea setului de date de tip panel
pd.df <- pdata.frame(data, index = c("country","year"), drop.index = TRUE)

# Corelatia dintre index_price/spatiu/timp
coplot(index_price ~ year|country, type="l", data=data) 

# Explorarea heterogeneitatii in sectiunea transversala
plotmeans(index_price ~ country, main = 'Heterogeneitate in randul tarilor', data = data)

# Explorarea heterogeneitatii in sectiunea temporala
plotmeans(index_price ~ year, main = 'Heterogeneitate in timp', data = data)


# Model OLS - model clasic de regresie liniara
# Nu ia in calcul heterogeneitatea intre spatiu si timp
ols <- lm(index_price ~ oil_prices + exchange_rate + percapitaincome + unemploymentrate + manufacturingoutput, data)
summary(ols) #output
yhat <- ols$fitted # valori estimate

# Model FE (cu efecte fixe) 
fe <- plm(index_price ~ oil_prices + exchange_rate + percapitaincome + unemploymentrate + manufacturingoutput, data, index = c('country','year'), model = 'within')
summary(fe)
# Unbalanced Panel: n = nr de paneluri
#                   T = ani
#                   N = nr total de observatii

# Rata de schimb este semnificativa si venitul pe cap de locuitor 99%, vom estima din nou modelul tinand
# cont de acestea
fe <- plm(index_price ~ exchange_rate + percapitaincome, data, index = c('country','year'),
          model = 'within')
summary(fe)

pFtest(fe, ols) # p-value < 0.05 => se recomanda model de panel data FE

# Model cu efecte aleatorii RE (random effects)
re <- plm(index_price ~ exchange_rate + percapitaincome, data, index = c('country','year'),
          model = 'between')
summary(re)

# Testul Hausmann =
phtest(fe,re) # p-value <0.05 => model cu efecte fixe se recomanda

# Testarea efectelor fixe in timp
fixed.time <- plm(index_price ~ exchange_rate + percapitaincome + factor(year), data=data, index=c("country",
                                                                       "year"), model="within")

pFtest(fixed.time, fe) # p-value < 0.05 => se recomanda folosirea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value >0.05  => nu este nevoie sa se 
# utilizeze efecte fixe in timp 

# Cele doua teste sunt au rezultate diferite, deci vom alege varianta in care nu avem efecte fixe in timp

# Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
pool <- plm(index_price ~ exchange_rate + percapitaincome, data=data, index=c("country", "year"), model="pooling")
summary(pool)

plmtest(pool, type=c("bp")) # p-value < 0.05 => variatiile in timp sunt diferite de 0

# Testarea dependentei transversale folosind testul Breusch-Pagan LM si testul Parasan CD
pcdtest(fe, test = 'lm') # p-value <0.05 => dependenta transversala
pcdtest(fe, test = 'cd') # p-value > 0.05 => independenta transversala

# Testarea autocorelarii - Breusch-Godfrey/Wooldridge test
pbgtest(fe) # p-value <0.05 => avem autocorelare

# Testarea heteroschedasticitatii cu testul Breusch-Pagan
bptest(index_price ~ exchange_rate + percapitaincome + factor(country), data = data, studentize=F)
# deoarece p-value <0.05 => avem heteroschedasticitate
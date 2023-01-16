# Modele cu date de tip panel

# Cuprins:
#   Declararea setului de date de tip panel
#   Explorarea heterogeneitatii
#   OLS (regresie liniara multipla)
#   Model cu efecte fixe (FE - fixed effects) 
#   Testarea ipoteze
#   Model cu efecte aleatoare (RE - Random Effects)
#   Testul Hausman pt efective fixe vs efecte aleatorii

# Fisiere: 
#   car_price_prediction.csv
#   petrol = 1, diesel = 0
#   automatic = 0, manual = 1

# Setare fisier de lucru
rm(list = ls())

# Install packages
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest", "readxl")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Exemplu: Set de masini cu data de fabricatie incadrata in perioada 2010-2020
data <- read_excel("D:/Facultate/Econometrie/proiect_econometrie2/dataset.xlsx")

# Statistici descriptive
summary(data)

# Declararea setului de date de tip panel
pd.df <- pdata.frame(data, index = c("Manufacturer","YEAR"), drop.index = TRUE)

# Corelatia dintre price/spatiu/timp
coplot(Price ~ YEAR|Manufacturer, type="l", data=data) 

# Heterogeneitatea presupune ca exista diferente intre unitatile studiate

# Explorarea heterogeneitatii in sectiunea transversala
# Graficul traseaza un interval de incredere de 95% in jurul mediilor
# Producători cu pretul foarte mare si producători cu pretul foarte mic => avem heterogeneitate transversala
plotmeans(Price ~ Manufacturer, main = 'Heterogeneitate in randul producătorilor', data = data)

# Explorarea heterogeneitatii in sectiunea temporala
# Ani cu pretul mare si ani cu pretul mic => avem heterogeneitate temporala, 
# dar mai mica decat in cazul heterogeneitatii transversale
plotmeans(Price ~ YEAR, main = 'Heterogeneitate in timp', data = data)


# Model OLS - model clasic de regresie liniara
# Nu ia in calcul heterogeneitatea intre spatiu si timp
ols <- lm(Price ~ Mileage+ Fuel_type + Engine + Gear_box_type + Airbags, data)
summary(ols) #output
yhat <- ols$fitted # valori estimate
ggplot(data, aes(x=Mileage, y=Price))+ #price~mileage
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  theme_bw()

# Model FE (cu efecte fixe) 
fe <- plm(Price ~ Mileage+ Fuel_type + Engine + Gear_box_type + Airbags, data, index = c('Manufacturer','YEAR'),
          model = 'within')
summary(fe)
# Unbalanced Panel: n = nr de paneluri
#                   T = ani
#                   N = nr total de observatii

# Sunt semnificative cilindreea motorului si kilometrajul, vom estima din nou modelul tinand
# cont de cea mai semnificativă variabilă
fe <- plm(Price ~ Engine, data, index = c('Manufacturer','YEAR'),
          model = 'within')
summary(fe)

# In cadrul modelelor panel data, din cazua complexitatii, vom compara
# p-value doar cu 0.05. Nu vom mai purta discutiile cu nivelurile de semni
# ficatie 90%/95%/99%

# Alegerea celei mai adecvate variante de model prin testarea intre regresie 
# OLS vs fixed effects panel model
# H0: FE 
# H1: OLS
pFtest(fe, ols) # p-value = 0.0005647 < 0.05 => se recomanda model de panel data FE

# Model cu efecte aleatorii RE (random effects)
re <- plm(Price ~ Engine, data, index = c('Manufacturer','YEAR'),
          model = 'between')
summary(re) 

# Testul Hausmann il utilizam pentru a decide intre FE si RE
# H0: model cu efecte random 
# H1: model cu efecte fixe
phtest(fe,re) # p-value = 0.005146 < 0.05 => se recomanda model cu efecte fixe 

# Testare ipoteze model
# In urma testului Hausmann am decis sa utilizam modelul FE

# Testarea efectelor fixe in timp
fixed.time <- plm(Price ~ Engine + factor(YEAR), data=data, index=c("Manufacturer",
                                                                    "YEAR"), model="within")
summary(fixed.time)
# H0:  nu sunt necesare efectele fixe in timp
# H1:  sunt necesare efectele fixe in timp
pFtest(fixed.time, fe) # p-value = 0.02062 < 0.05 => se recomanda folosirea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value = 0.02249 < 0.05  =>este nevoie sa se 
# utilizeze efecte fixe in timp 

# Cele doua teste arata un p-value mai mic decat 0.05, deci vom alege varianta in care avem efecte fixe in timp

# Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# Testul ne ajuta sa decidem intre RE si OLS 
pool <- plm(Price ~ Engine, data=data, index=c("Manufacturer", "YEAR"), model="pooling")
summary(pool)

# H0: variatiile in timp sunt 0
# H1: variatiile in timp sunt diferite de 0
plmtest(pool, type=c("bp")) # p-value = 0.04899 < 0.05 => acceptam ipoteza alternativa
# variatiile in timp sunt diferite de 0 => efectele aleatorii sunt adecvate =>
# exista diferente semnificative intre producatori


# Testarea dependentei transversale folosind testul Breusch-Pagan LM si 
# testul Perasan CD

# Ipoteze teste
# H0: reziduurile intre entitati nu sunt corelate
# H1: reziduurile intre entitati sunt corelate

pcdtest(fixed.time, test = 'lm') # p-value = 2.2e-16 < 0.05 => dependenta transversala
pcdtest(fixed.time, test = 'cd') # pvalue = 0.5581 > 0.05 => nu avem dependenta transversala
# Nu corectam pt ca avem panel mic. daca aveam serie de timp 40 perioade +
# trebuia sa corectam


# Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 
# Testul se aplica doar cand seriile de timp sunt lungi. In acest caz nu 
# pune probleme setul de date deoarece avem date pe 10 ani

# H0: Nu exista autocorelate
# H1: autocorelarea este prezenta
pbgtest(fixed.time) # p-value = 0.04783 < 0.05 => avem autocorelare


# Testarea heteroschedasticitatii cu testul Breusch-Pagan
# H0: homoschedasticitate
# H1: heteroschedasticitate
bptest(Price ~ Engine + factor(Manufacturer), data = data, studentize=F)# deoarece p-value = 2.2e-16 <0.05 
#=> avem heteroschedasticitate

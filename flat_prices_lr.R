#### required packages: ####
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)
install.packages("tseries")
library(tseries)
install.packages("expss")
library(expss)
library(readxl)
if(!require(rcompanion)){install.packages("rcompanion")}
library(rcompanion) # plotNormalHistogram(x), groupWiseMean(x)
if(!require(psych)){install.packages("psych")}
library(psych) #describe(), corr.test
if(!require(nortest)){install.packages("nortest")}
library(nortest) #sf.test
if(!require(lawstat)){install.packages("lawstat")}
library(lawstat) #Leven test
options(scipen=999)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)
install.packages("factoextra")
library("factoextra")
if(!require(tidyverse)){install.packages("car")}
library(car)
library("Formula")
library("plm") 
library("sandwich") 
library("zoo") 
library("lmtest")
library("aod")
install.packages("stargazer")
library(stargazer)

#### uploading dataset #### 
m_baza <- read.csv("flats_dataset.csv", header=TRUE, sep=",", dec=".")

#### SHORT ANALYSIS OF THE VARIABLES ####

### distribution of the dependent variable
plotNormalHistogram(m_baza$price, col="cadetblue3", linecol="black", main="Distribution of variable price")
plotNormalHistogram(log(m_baza$price), col="cadetblue3", linecol="black", main="Distribution of variable log(price)")
summary(m_baza$price)

# Normal Q-Q Plot
qqnorm((m_baza$price))
qqline((m_baza$price), col = 2)
qqnorm(log(m_baza$price))
qqline(log(m_baza$price), col = 2)

# visible not normal distribution of the dependent variable,
# can be improved by using the logarithmic form

# test for normality
jarque.bera.test((m_baza$price))
# p-value < 5%, indicates not normal distribution
# in this case, I decided to use logaritmic form in further analysis


### continuous independent variables

# distribution of variable squareMeters
plotNormalHistogram(m_baza$squareMeters, col="cadetblue3", linecol="black", main="Distribution of squareMeters")
plotNormalHistogram(log(m_baza$squareMeters), col="cadetblue3", linecol="black", main="Distribution of log(squareMeters)")
summary(m_baza$squareMeters)

# Normal Q-Q Plot
qqnorm((m_baza$squareMeters))
qqline((m_baza$squareMeters), col = 2) 
qqnorm(log(m_baza$squareMeters))
qqline(log(m_baza$squareMeters), col = 2) 

# visible not normal distribution of the variable

# test for normality
jarque.bera.test((m_baza$squareMeters))
# p-value < 5%, indicates not normal distribution
# -> can be improved by using the logarithmic form

# let's see, which one is a better fit, in terms of explaining dependent variable:

# scatter plot of log(price) and squareMeters

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(squareMeters)), method="lm", se=FALSE, color="red")+
  geom_point(mapping=aes(x=(squareMeters), y=log(price)))

# scatter plot of log(price) and squareMeters

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(squareMeters)), method="lm", se=FALSE, color="red")+
  geom_point(mapping=aes(x=log(squareMeters), y=log(price)))

# using both variables in log form, results in points being more evenly distributed around the red line
# -> logaritmic form might be a better fit 


# distribution of variable centreDistance
plotNormalHistogram((m_baza$centreDistance),col="cadetblue3", linecol="black", main="Distribution of centreDistance" )
plotNormalHistogram(log(m_baza$centreDistance), col="cadetblue3", linecol="black", main="Distribution of log(centreDistance")

qqnorm((m_baza$centreDistance))
qqline((m_baza$centreDistance), col = 2) 

qqnorm(log(m_baza$centreDistance))
qqline(log(m_baza$centreDistance), col = 2)
# visible not normal distribution of the variable

# Jarque-Bera normality test
jarque.bera.test((m_baza$centreDistance))
# p-value < 5%, indicates not normal distribution

# -> situation can be improved by using the logarithmic form

# scatter plot  log(price) and centreDistance
ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(centreDistance)), method="lm", se=FALSE, color="red")+
  geom_point(mapping=aes(x=(centreDistance), y=log(price)))

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(centreDistance)), method="lm", se=FALSE, color="red")+
  geom_point(mapping=aes(x=log(centreDistance), y=log(price)))


# distribution of variable schoolDistance
plotNormalHistogram((m_baza$schoolDistance), col="cadetblue3", linecol="black", main="Distribution of schoolDistance")

plotNormalHistogram(log(m_baza$schoolDistance), col="cadetblue3", linecol="black", main="Distribution of log(schoolDistance)")

qqnorm((m_baza$schoolDistance))
qqline((m_baza$schoolDistance), col = 2) 

qqnorm(log(m_baza$schoolDistance))
qqline(log(m_baza$schoolDistance), col = 2)
# visible not normal distribution of the variable
# significant change when using log form

# normality test
jarque.bera.test((m_baza$schoolDistance))
# p-value < 5%, indicates not normal distribution

# -> situation can be improved by using the logarithmic form

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(schoolDistance), method="lm", se=FALSE, color="red"))+
  geom_point(mapping=aes(x=(schoolDistance), y=log(price)))

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(schoolDistance), method="lm", se=FALSE, color="red"))+
  geom_point(mapping=aes(x=log(schoolDistance), y=log(price))) #slightly better fit


# distribution of variable pharmacyDistance
plotNormalHistogram((m_baza$pharmacyDistance),  col="cadetblue3", linecol="black", main="Distribution of pharmacyDistance")
plotNormalHistogram(log(m_baza$pharmacyDistance),  col="cadetblue3", linecol="black", main="Distribution of log(pharmacyDistance)")

qqnorm((m_baza$pharmacyDistance))
qqline((m_baza$pharmacyDistance), col = 2) 

qqnorm(log(m_baza$pharmacyDistance))
qqline(log(m_baza$pharmacyDistance), col = 2)
# visible not normal distribution of the variable

# normality test
jarque.bera.test((m_baza$pharmacyDistance))
# p-value < 5%, indicates not normal distribution

# -> situation can be improved by using the logarithmic form

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(pharmacyDistance), color="red"))+
  geom_point(mapping=aes(x=(pharmacyDistance), y=log(price)))
ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(pharmacyDistance), color="red"))+
  geom_point(mapping=aes(x=log(pharmacyDistance), y=log(price)))


# distribution of variable collegeDistance
plotNormalHistogram((m_baza$collegeDistance),  col="cadetblue3", linecol="black", main="Distribution of collegeDistance")
plotNormalHistogram(log(m_baza$collegeDistance), col="cadetblue3", linecol="black", main="Distribution of log(collegeDistance)" )

qqnorm((m_baza$collegeDistance))
qqline((m_baza$collegeDistance), col = 2) 

qqnorm(log(m_baza$collegeDistance))
qqline(log(m_baza$collegeDistance), col = 2)
# visible not normal distribution of the variable
# slight change when using log form

# normality test
jarque.bera.test((m_baza$collegeDistance)) 
# p-value < 5%, indicates not normal distribution

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(collegeDistance), color="red"))+
  geom_point(mapping=aes(x=(collegeDistance), y=log(price)))

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(collegeDistance), color="red"))+
  geom_point(mapping=aes(x=log(collegeDistance), y=log(price)))



# variable clinicDistance
plotNormalHistogram((m_baza$clinicDistance),col="cadetblue3", linecol="black", main="Distribution of clinicDistance" )
plotNormalHistogram(log(m_baza$clinicDistance),col="cadetblue3", linecol="black", main="Distribution of log(clinicDistance)")

qqnorm((m_baza$clinicDistance))
qqline((m_baza$clinicDistance), col = 2) 

qqnorm(log(m_baza$clinicDistance))
qqline(log(m_baza$clinicDistance), col = 2) # better fit 

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(clinicDistance), color="red"))+
  geom_point(mapping=aes(x=(clinicDistance), y=log(price)))
ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x= log(clinicDistance), color="red"))+
  geom_point(mapping=aes(x=log(clinicDistance), y=log(price)))

# normality test
jarque.bera.test((m_baza$clinicDistance)) 
# p-value < 5%, indicates not normal distribution


# variable kindergartenDistance
plotNormalHistogram((m_baza$kindergartenDistance),col="cadetblue3", linecol="black", main="Distribution of kindergartenDistance")
plotNormalHistogram(log(m_baza$kindergartenDistance),col="cadetblue3", linecol="black", main="Distribution of kindergartenDistance")

qqnorm((m_baza$kindergartenDistance))
qqline((m_baza$kindergartenDistance), col = 2) 

qqnorm(log(m_baza$kindergartenDistance))
qqline(log(m_baza$kindergartenDistance), col = 2)

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(kindergartenDistance), color="red"))+
  geom_point(mapping=aes(x=(kindergartenDistance), y=log(price)))
ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(kindergartenDistance), color="red"))+
  geom_point(mapping=aes(x=log(kindergartenDistance), y=log(price)))

# normality test
jarque.bera.test((m_baza$kindergartenDistance)) 
# p-value < 5%, indicates not normal distribution

# variable postOfficeDistance
plotNormalHistogram((m_baza$postOfficeDistance),col="cadetblue3", linecol="black", main="Distribution of postOfficeDistance")
plotNormalHistogram(log(m_baza$postOfficeDistance),col="cadetblue3", linecol="black", main="Distribution of log(postOfficeDistance)")

qqnorm((m_baza$postOfficeDistance))
qqline((m_baza$postOfficeDistance), col = 2) 

qqnorm(log(m_baza$postOfficeDistance))
qqline(log(m_baza$postOfficeDistance), col = 2)

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(postOfficeDistance), color="red"))+
  geom_point(mapping=aes(x=(postOfficeDistance), y=log(price)))
ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(postOfficeDistance), color="red"))+
  geom_point(mapping=aes(x=log(postOfficeDistance), y=log(price)))

# normality test
jarque.bera.test((m_baza$postOfficeDistance))
# p-value < 5%, indicates not normal distribution

# variable age
summary(m_baza$age)

plotNormalHistogram((m_baza$age), col="cadetblue3", linecol="black", main="Distribution of age")

qqnorm((m_baza$age))
qqline((m_baza$age), col = 2) 

ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(age)), method="lm", se=FALSE, color="red")+
  geom_point(mapping=aes(x=(age), y= log(price)))

# normality test
jarque.bera.test((m_baza$age)) 
# p-value < 5%, indicates not normal distribution

# will other forms fit better?
ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=log(age)), method="lm", se=FALSE, color="red")+
  geom_point(mapping=aes(x=log(age), y= log(price)))
ggplot(data=m_baza) + geom_smooth(mapping=aes(y=log(price), x=(age)^2), method="lm", se=FALSE, color="red")+
  geom_point(mapping=aes(x=(age)^2, y= log(price))) 
#not really, let's stay with the original form



### 0-1 variables ###

## dummy variables:
# rooms1, rooms2, rooms3
table(m_baza$rooms1)
table( m_baza$rooms2) #biggest group
table(m_baza$rooms3)
# building type
table(m_baza$block) #biggest group 
table(m_baza$apartmentB)
table(m_baza$tenement)

#other variables:
table(m_baza$ownership)
table(m_baza$condition)
table(m_baza$hasParkingSpace)
table(m_baza$hasBalcony)
table(m_baza$hasElevator)
table(m_baza$hasSecurity)
table(m_baza$hasStorageRoom)
table(m_baza$first_floor)
table(m_baza$top_floor)
table(m_baza$stolica)


##### LINEAR MODELS ##### 
# In this part, I test different versions of the models, so I can pick the best one. I start with a model1,
# which includes most of the variables, but without interactions. Then I try to add some new variables and interactions or 
# get rid of the unnecessary ones.

#### model1 with: log price, log squareMeters, log distances,  rooms2 - base level, block - base level #####

model1 <- lm(log(price) ~ log(squareMeters) + rooms1 + rooms3 + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(kindergartenDistance) + log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor + age +
               apartmentB + tenement, data=m_baza)
summary(model1)

# test RESET
resettest(model1, power = 2:3, type = c("fitted")) 
# p-value = 0.8622 -> form of this model is OK

# Variance Inflation Factor - is there any multicollinearity?
vif(model1) 
# each value <10 -> OK

#### model2 with: log price, log squareMeters, log distances,  rooms3 - base level, block - base level ####
# ->  should we include 0-1 variable "stolica"? 
# to find out, I use Chow test 

# Chow test -- stability test
# I split the database in two categories: living in capital city (stolica) versus living in other city (nie_stolica)

stolica <- m_baza[m_baza$stolica == 1,] 
nie_stolica <- m_baza[m_baza$stolica ==0, ]


model2 <- lm(log(price) ~ log(squareMeters) + rooms1 + rooms3 + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(kindergartenDistance) + log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor + age + apartmentB + tenement
             , data=m_baza)

# I make two models:
model_niestolica <- lm(log(price) ~ log(squareMeters) + rooms1 + rooms3 + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
                         log(kindergartenDistance) + log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
                         hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor + age + apartmentB + tenement 
                       , data=nie_stolica)

RSS_1 <- sum(resid(model_niestolica)^2)

model_stolica <- lm(log(price) ~ log(squareMeters) + rooms1 + rooms3 + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
                      log(kindergartenDistance) + log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
                      hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor + age + apartmentB + tenement
                    , data=stolica)

RSS_2 <- sum(resid(model_stolica)^2)


RSS <- sum(resid(model2)^2)
RSS_modele <- RSS_1 + RSS_2 

N <- nrow(m_baza)
K <- length(model_stolica$coefficients)

K_model2 <- length(model2$coefficients)


m <- 2
F_stat <- (RSS - RSS_modele) / (m - 1) / ((RSS_modele / (N - m * K)) / K_model2)
p_value <- round(pf(F_stat, K_model2, (N - m * K), lower.tail = FALSE), 5)
p_value = 0

# model is not stable without variable stolica, which means it plays a crucial role in explaining flat prices

model2 <- lm(log(price) ~ log(squareMeters) + rooms1 + rooms3 + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(kindergartenDistance) + log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor + age + apartmentB + tenement
             + stolica, data=m_baza)
summary(model2)

resettest(model2, power = 2:3, type = c("fitted")) 
# p-value = 0.3445 -> OK

vif(model2) # -> OK


#### model3 adding multiple interactions - my base model ####
model3 <- lm(log(price) ~ log(squareMeters) + rooms1 + rooms3 + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(kindergartenDistance) + log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor +
               age  + apartmentB + tenement  + stolica  + hasElevator*top_floor  +
               log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
             + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)
summary(model3)

resettest(model3, power = 2:3, type = c("fitted")) #OK

stargazer(model1, model2, model3, type="text") #comparison of the 3 models
# model3 with the highest Adjusted R^2

#### Model3 diagnostics ####
# before reducing irrelevant interactions and variables, I check homoscedasticity and residuals normality

# 2. Homoscedasticity

# Residuals vs fitted plot 
plot(model3, which=1) # points group in the middle of the chart (variance of the residuals in a regression model is not constant)

# Breusch-Pagan test -> H0: homoscedastic residuals
# H1: heteroscedasticity depends on fitted values of Y
# lmtest::bptest()
bptest(model3, studentize=FALSE) 
# p-value < 0.00000000000000022 -> 
# residuals are homoscedastic
# in this case, using robust variance-covariance matrix might be a solution

# 3. Normality of residuals

# q-q plot 
plot(model3, which=2)

# test Jarque-Berra
jarque.bera.test(model3$residuals)
# p-value = 0.000105 -> residuals dont't have normal distribution
# but the research sample is large (2094), so it's not a problem in this case


##### ROBUST MATRIX #####

# MacKinnon's and White's robust estimator
model3_r = coeftest(model3, vcov.=vcovHC(model3, type="HC3"))
show(model3_r)

# quality publication table
stargazer(model3, model3_r, type="text")

#### CHOOSING BEST MODEL ####

# In this step I will gradually eliminate statistically insignificant variables,
# starting with the "worst" p-value in robust matrixes,
# unless the insignificant variable is included in significant interaction (it must stay then)

#### model4 without variable  kindergartenDistance ###
model4 <- lm(log(price) ~ log(squareMeters) + rooms1 + rooms3 + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor +
               age  + apartmentB + tenement  + stolica  + hasElevator*top_floor  +
               log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
             + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)

summary(model4)

resettest(model4, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model4_r = coeftest(model4, vcov.=vcovHC(model4, type="HC3"))
show(model4_r)

### quality publication table
stargazer(model4, model4_r, type="text")

#### model 5 without variable  rooms3 ####
model5 <- lm(log(price) ~ log(squareMeters) + rooms1  + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator + hasStorageRoom + hasSecurity + first_floor + top_floor +
               age  + apartmentB + tenement  + stolica  + hasElevator*top_floor  +
               log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
             + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)

summary(model5)

resettest(model5, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model5_r = coeftest(model5, vcov.=vcovHC(model5, type="HC3"))
show(model5_r)

### quality publication table
stargazer(model5, model5_r, type="text")


#### model 6 without hasStorageRoom ####
model6 <- lm(log(price) ~ log(squareMeters) + rooms1  + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator  + hasSecurity + first_floor + top_floor +
               age  + apartmentB + tenement  + stolica  + hasElevator*top_floor  +
               log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
             + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)

summary(model6)

resettest(model6, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model6_r = coeftest(model6, vcov.=vcovHC(model6, type="HC3"))
show(model6_r)

### quality publication table
stargazer(model6, model6_r, type="text")

#### model 7 without hasSecurity ####

model7 <- lm(log(price) ~ log(squareMeters) + rooms1  + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
               age  + apartmentB + tenement  + stolica  + hasElevator*top_floor  +
               log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
             + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)

summary(model7)

resettest(model7, power = 2:3, type = c("fitted")) #OK

# MacKinnon's and White's robust estimator
model7_r = coeftest(model7, vcov.=vcovHC(model7, type="HC3"))
show(model7_r)

### quality publication table
stargazer(model7, model7_r, type="text")

#### model 8, without rooms1  ####

model8 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(restaurantDistance) + log(collegeDistance) + log(pharmacyDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
               age  + apartmentB + tenement  + stolica  + hasElevator*top_floor  +
               log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
             + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)

summary(model8)

resettest(model8, power = 2:3, type = c("fitted")) #OK

# MacKinnon's and White's robust estimator
model8_r = coeftest(model8, vcov.=vcovHC(model8, type="HC3"))
show(model8_r)

### quality publication table
stargazer(model8, model8_r, type="text")


#### model 9, without log(pharmacyDistance)  ####

model9 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
               log(restaurantDistance) + log(collegeDistance) + ownership + buildingMaterial + condition +
               hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
               age  + apartmentB + tenement  + stolica  + hasElevator*top_floor  +
               log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
             + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)

summary(model9)

resettest(model9, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model9_r = coeftest(model9, vcov.=vcovHC(model9, type="HC3"))
show(model9_r)

### quality publication table
stargazer(model9, model9_r, type="text")

#### model 10, without hasElevator*top_floor ####

model10 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + log(postOfficeDistance) + 
                log(restaurantDistance) + log(collegeDistance) + ownership + buildingMaterial + condition +
                hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
                age  + apartmentB + tenement  + stolica  +
                log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
              + ownership*tenement  + log(squareMeters)*condition + + log(centreDistance)*stolica, data=m_baza)

summary(model10)

resettest(model10, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model10_r = coeftest(model10, vcov.=vcovHC(model10, type="HC3"))
show(model10_r)

### quality publication table
stargazer(model10, model10_r, type="text")


#### model 11, without postOfficeDistance ####

model11 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + 
                log(restaurantDistance) + log(collegeDistance) + ownership + buildingMaterial + condition +
                hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
                age  + apartmentB + tenement  + stolica  +
                log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
              + ownership*tenement  + log(squareMeters)*condition + log(centreDistance)*stolica, data=m_baza)

summary(model11)

resettest(model11, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model11_r = coeftest(model11, vcov.=vcovHC(model11, type="HC3"))
show(model11_r)

### quality publication table
stargazer(model11, model11_r, type="text")

### model 12, without ownership*tenement ####

model12 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + 
                log(restaurantDistance) + log(collegeDistance) + ownership + buildingMaterial + condition +
                hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
                age  + apartmentB + tenement  + stolica  +
                log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
              + log(squareMeters)*condition + log(centreDistance)*stolica, data=m_baza)

summary(model12)

resettest(model12, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model12_r = coeftest(model12, vcov.=vcovHC(model12, type="HC3"))
show(model12_r)

### quality publication table
stargazer(model12, model12_r, type="text")


### model 13, without ownership####

model13 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + 
                log(restaurantDistance) + log(collegeDistance) + buildingMaterial + condition +
                hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
                age  + apartmentB + tenement  + stolica  +
                log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
              + log(squareMeters)*condition + log(centreDistance)*stolica, data=m_baza)

summary(model13)

resettest(model13, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model13_r = coeftest(model13, vcov.=vcovHC(model13, type="HC3"))
show(model13_r)

### quality publication table
stargazer(model13, model13_r, type="text")


### model 14, without log(squareMeters)*condition ####

model14 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + 
                log(restaurantDistance) + log(collegeDistance) + buildingMaterial + condition +
                hasParkingSpace + hasBalcony + hasElevator  + first_floor + top_floor +
                age  + apartmentB + tenement  + stolica  +
                log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
              + log(centreDistance)*stolica, data=m_baza)

summary(model14)

resettest(model14, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model14_r = coeftest(model14, vcov.=vcovHC(model14, type="HC3"))
show(model14_r)

### quality publication table
stargazer(model14, model14_r, type="text")

### model 15, without first_floor ####

model15 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + 
                log(restaurantDistance) + log(collegeDistance) + buildingMaterial + condition +
                hasParkingSpace + hasBalcony + hasElevator  + top_floor +
                age  + apartmentB + tenement  + stolica  +
                log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
              + log(centreDistance)*stolica, data=m_baza)

summary(model15)

resettest(model15, power = 2:3, type = c("fitted")) #OK

# MacKinnon's and White's robust estimator
model15_r = coeftest(model15, vcov.=vcovHC(model15, type="HC3"))
show(model15_r)

### quality publication table
stargazer(model15, model15_r, type="text")

### model 16, without hasBalcony ####

model16 <- lm(log(price) ~ log(squareMeters) + log(centreDistance) + log(schoolDistance) + log(clinicDistance) + 
                log(restaurantDistance) + log(collegeDistance) + buildingMaterial + condition +
                hasParkingSpace  + hasElevator  + top_floor +
                age  + apartmentB + tenement  + stolica  +
                log(centreDistance)*log(squareMeters) + log(centreDistance)*hasParkingSpace + log(schoolDistance)*hasParkingSpace 
              + log(centreDistance)*stolica, data=m_baza)

summary(model16)

resettest(model16, power = 2:3, type = c("fitted")) # OK

# MacKinnon's and White's robust estimator
model16_r = coeftest(model16, vcov.=vcovHC(model16, type="HC3"))
show(model16_r)

### quality publication table 
stargazer(model16, model16_r, type="text")



### FINAL MODEL: model16####
# final model includes all of the significant variables and has correct functional form
# results are discussed in the report

#### whole diagnostic for the final model

# 1. Checking the correctness of the functional form 

resettest(model16, power = 2:3, type = c("fitted")) 
# p-value = 0.4938 -> OK, form of this model is appropriate for the data

# 2. Normality of the residuals

# q-q plot 
plot(model16, which=2)

# test Jarque-Berra
jarque.bera.test(model16$residuals) 
# p-value = 0.0001409 -> residuals don't have normal distribution, but the sample is large

# 3. Homoscedasticity

# Residuals vs fitted plot
plot(model16, which=1)

# Breusch-Pagan test -> H0: homoscedastic residuals
# H1: heteroscedasticity depends on fitted valued of Y
# lmtest::bptest()
bptest(model16, studentize=FALSE) 

#p-value < 0.00000000000000022 -> residuals are homoscedastic

## ANLYSIS OF THE OUTLIER VARIABLES ####

# I calculate the values of leverage, standardized residuals
# and Cook's distance to find outliers

# leverage
m_baza$lev <- hatvalues(model16)

# residuals
m_baza$rstd <- rstandard(model16)

# Cook distance
m_baza$cookd <- cooks.distance(model16)

# For how many observations leverage > 2k/n?
# length(model$coefficients) -- # parameters, k
# nrow(m_baza) -- # obs., n

lev_threshold <- (2*(length(model16$coefficients)/nrow(m_baza))) # 0.02
length(m_baza$lev[m_baza$lev > lev_threshold])

# 87 obserwacji

# For how many observations |stand_residuals| >2?
length(m_baza$rstd[abs(m_baza$rstd)>2])
# 89 obs.

# For how many observations Cook's distance > 4/n?
cook_threshold <- 4/nrow(m_baza) # 
length(m_baza$cookd[m_baza$cookd > cook_threshold])
# 104 obs.

outliers <- m_baza[m_baza$lev > lev_threshold & (abs(m_baza$rstd)>2) & m_baza$cookd > cook_threshold,]
outliers #6 observations

plot(model16, which=5)

# Cook's distance plot
# abline -- treshold
plot(model16, which=4, cook.level= cook_threshold)
abline(h=cook_threshold, lty=2, col= "red")

# I found 6 outliers, which I discuss in the report
# observations nr: 195  337  462  557  675  852 1957

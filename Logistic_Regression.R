
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(car)
library(zoo)


dt <- read_csv("2018Boston_rain.csv")


dim(dt)    #returns the dimensions of an object. design for R, to see how many observations & attributes in this dataset. not RStudio,because this information lists in the Environment on the right top screen
str(dt)    #returns the structure of an object
sum(is.na(dt)) #returns how many observations have "na"
dt[is.na(dt)] <- '0'        #replaces "na" with 0. GLM won't run if there is NA in the data
sum(is.na(dt))
View(dt)

select (dt,-c(PGTM,SNOW,SNOW,WDF2))              #-c() means dropping these attributes


# add a Season Variable: divide 365 days of a year into 4 seasons 


yq <- as.yearqtr(as.yearmon(dt$DATE, "%m/%d/%Y") + -1/11)                 

dt$Season <- factor(format(yq, "%q"), levels = 1:4, labels = c("Spring", "Summer", "Fall","Winter"))              
view(dt)

# create a Wind Direction as factor variable

dt$NewWindDir<-dt$WDF5-23
dt$WindCat<-cut(dt$NewWindDir, c(-22,22,67,112,157,202,247,292,337))
dt$NewWindCat<-revalue(dt$WindCat, c("(-22,22]"="N","(22,67]"="NE","(67,112]"="E","(112,157]"="SE","(157,202]"="S","(202,247]"="SW","(247,292]"="W","(292,337]"="NW" ))

# create Factor variable indicating presence of Rain
dt$RainFac <- ifelse(dt$PRCP > 0, 1, 0)
rain<-factor(dt$RainFac)

# create sequential logit models

rainpredict<-glm(rain~dt$Season, data=dt, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~dt$Season + dt$AWND, data=dt, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))   

rainpredict<-glm(rain~dt$Season + dt$AWND + dt$TAVG, data=dt, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~dt$Season + dt$AWND + dt$TAVG + dt$TMAX, data=dt, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~dt$Season + dt$AWND + dt$TAVG + dt$TMAX + dt$TMIN, data=dt, family=binomial)
summary(rainpredict)                   # AIC  484.8
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~dt$Season + dt$AWND + dt$TAVG + dt$TMAX + dt$TMIN + dt$NewWindCat, data=dt, family=binomial)
summary(rainpredict)                 # AIC  485.74
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~dt$AWND + dt$TAVG + dt$TMAX + dt$TMIN + dt$NewWindCat + dt$WSF5, data=dt, family=binomial)
summary(rainpredict)             # AIC  463.2
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

#explore the smallest AIC
rainpredict<-glm(rain~dt$Season + dt$AWND + dt$TMAX + dt$TMIN + dt$NewWindCat + dt$WSF5, data=dt, family=binomial)
summary(rainpredict)          # AIC  464.52
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~dt$AWND + dt$TMAX + dt$TMIN + dt$NewWindCat + dt$WSF5, data=dt, family=binomial)      # best model
summary(rainpredict)        # AIC  461.29
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))


##### Predict rain possibility

prediction=predict(rainpredict,type="response")
prediction


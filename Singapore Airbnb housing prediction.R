library(readxl)
library(plyr)
library(tidyverse)
library(car)
library(purrr)
library(dplyr)
library(XLConnect)
library(tidyverse)
library(mi)

Dt <- read_excel("Airbnb_Singapore_clean.xlsx")
View(Dt)
Dt <- Dt %>% select(neighbourhood_group, latitude, longitude, room_type, price, minimum_nights, number_of_reviews, calculated_host_listings_count, availability_365)
# also manually delete 1 row of "price=0" in Excel
Dat <-  Dt[complete.cases(Dt), ] 
summary(Dat)


Dat$NB_group <- factor(Dat$neighbourhood_group,levels=c('Central Region', 'East Region', 'North Region', 'North-East Region', 'West Region'))

Dat$RM <- factor(Dat$room_type, levels=c('Entire home/apt', 'Private room', 'Shared room'))

Mod <-lm(price~NB_group + latitude + longitude + RM + minimum_nights + number_of_reviews + calculated_host_listings_count + availability_365, data=Dat)
summary(Mod)


Dat$min_log <- log(Dat$minimum_nights)
Dat$rw_log <- log(Dat$number_of_reviews +0.01)
Dat$hstct_log <- log(Dat$calculated_host_listings_count)
Dat$avl_log <- log(Dat$availability_365 +0.01)
Dat$prc_log <- log(Dat$price)     

Mod1 <-lm(prc_log~NB_group + latitude + longitude + RM + min_log + rw_log + hstct_log + avl_log, data=Dat)
summary(Mod1)


#VIF regression#

vif(Mod1)

Mod2 <-lm(prc_log~ latitude + longitude + RM + min_log + rw_log + hstct_log + avl_log, data=Dat)
summary(Mod2)
anova(Mod1, Mod2)          # indicate neighbourhood_group should be included

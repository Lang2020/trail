install.packages(car)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(tidyverse)              
library(haven)
library(ggthemes)
library(classInt)
library(car)
library(scatterplot3d)
library(ggpubr)
library(plyr)
library(lattice)
library(ggrepel)
library(data.table)
library(baytrends)
library(lmtest)
library(qualityTools)
library(alr3)
library(gmodels)
library(stats)
library(stringr)
library(sf)
library(corrplot)
library(ggmap)
library(ISLR)
library(glmnet)
library(car)
library(caret)
library(zoo) 
library(chron) 
library(xts)
library(rugarch)
library(writexl)
library(GGally)
library(RColorBrewer)
library(leaps)
library(corpcor)
library(mctest)
library(corrplot)
library(ppcor)
library(rlang)
install.packages("rlang")

getwd()
setwd("/Users/naljor/Desktop/Stat 760")
df<- read_excel("GDP.POP.Co2.xls")
names(df);head(df); tail(df);str(df);dim(df)

names(df)[1]<- "Country"
names(df)[3]<- "Co2"
names(df)[4] <- "GDP"
names(df)[5] <-"Population"

countries <- c("United States","India","China")
df1 <- df %>% 
  filter(Country %in% countries )

table(is.na(df1))# no missing data
df1$Co2<- as.numeric(df1$Co2)
df1$GDP<- as.numeric(df1$GDP) ####### IT'S NOT WORKING ######## ##
str(df1)
library(ggthemes)
par(mfrow=c(1,3))
ggplot(df1, aes(x=Date, y=Co2, colour=Country)) + geom_point()
ggplot(df1, aes(x=Date, y=GDP, colour=Country)) + geom_point()
ggplot(df1, aes(x=Date, y=Population, colour=Country)) + geom_point()


#### #### #### ######## ######## #### #### ####
#### ######## IDK WHY ITS NOT WORKING ######## ####
#### ######## ######## ######## ######## ######## ####
library(plotly)

dmy <- dummyVars("~.", data=df1)
new.df1<- data.frame(predict(dmy,newdata=df1))
dim(df1)
dim(new.df1)
head(new.df1);str(new.df1)
ggcorr(new.df1)+labs(title="CORRELATION OF DIFFERENT VARIABLES")
ggplotly(cat)
dev.off()

#### #### #### ######## ######## #### #### ####
#### ######## RELATION BETWEEN TWO VARIABLE ######## ####
#### ######## ######## ######## ######## ######## ####

df1 %>% 
  ggplot(aes(df1, x=Population,y=Co2))+geom_point()+theme_bw()+
  ylab("Co2 Per Capita")

df1 %>% 
  ggplot(aes(df1, x=GDP,y=Co2))+geom_point()+theme_bw()+
  ylab("Co2 Per Capita")

df1 %>% 
  ggplot(aes(df1, x=Population,y=GDP))+geom_point()+theme_bw()+
  ylab("GDP Per Capita")

# shows negative relation
df1 %>% 
  ggplot(aes(x=GDP,y=Co2, color=Country))+
  geom_line()+
  theme_bw()+
  ylab("Pop Growth Rate")+
  scale_color_manual(values=c("deeppink","forestgreen","purple"))+      
  facet_wrap(~Country)+ theme(legend.position = "none")

head(df1)
df1 %>% 
  ggplot(aes(x=Population,y=Co2, color=Country))+
  geom_line()+
  theme_bw()+
  ylab("GDP Per Capita")+
  scale_color_manual(values=c("deeppink","forestgreen","purple"))+      
  facet_wrap(~Country)+ theme(legend.position = "none")



#### #### #### ######## ######## #### #### ####
#### ######## Linear  Modelling ######## ####
#### ######## ######## ######## ######## ######## ####

model<- lm(Co2~GDP+Population, df1)
summary(model)
residual<-residuals(model)
par(mfrow=c(2,2))
### Residuals plot looks HORRIBLE???
qqnorm(residual, col="dark blue", lwd=2, xlab="", ylab="")
qqline(residual, col="red")
plot(residual, col="dark blue", lwd=1.5, xlab="",ylab="")
par(mfrow=c(2,2))
plot(model)

model$predicted <- predict(model)

#### ######## IDK IF I NEED THIS BUT IT LOOKS BAD ######## ####
#### ######## Predicted v/s real ######## ######## ####

ggplot(df1, aes(Year, Co2))+
  geom_point() +
  geom_point(aes(y = model$predicted), shape = 1) 

model %>% select(df1, predicted, residuals) %>% head()

simple_MLRM_summary = summary(simple_MLRM)
RSS1 <- c(crossprod(residual))
MSE1 <- RSS1 / length(residual)
RMSE1 <- sqrt(MSE1);RMSE1
sig2_1 <- RSS1 / residual; sig2_1
print("RSME 14 variables :"); sqrt(mean(residual^2))




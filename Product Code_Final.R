#Clear Environment.
rm(list=ls())
#Settings due to working form private repository. 
#install.packages("car")
library(tidyr)
#Sys.setenv(BINPREF = "C:/My_RStudio/rtools40/mingw64/bin/") 
#install.packages("dlookr",type = "win.binary" )

#install.packages("partykit")
library(ggplot2)
library(dlookr)
library(dplyr)
#install.packages("missRanger")

#install.packages("missRanger",type = "win.binary")
library(mice)
library(car)
library(missRanger)
#set working directory. 
#setwd('C:/Users/knighk/OneDrive - Office for National Statistics/MSc DataGov')
setwd("/Users/kyleknights/Dropbox/MSc Data Analytics/DALT7017 Dissertation/Final")


#set seed for reproducibility. 



#Code is adapted from (Van Buuren, 2018)
#load in dataset.
d=read.csv('UK_2013p_EUSILC.csv')

#selecting the income variables (Benefits,Employment, Pensions).

#=select(d,PY020G,PY021G,PY030G,PY031G,PY050G,PY080G,PY090G,PY100G,PY110G,PY120G,PY140G,PY200G)
d1=select(d,PW190,PY200G)
d1[is.na(d1)]=0
#d1=filter(d1,PY200N>0)
d1=filter(d1,PY200G>0)
d1=filter(d1,PW190>0)

#Make missing function installs 50% worth of missingness. 
make.missing <- function(data, p = 0.5){
  rx <- rbinom(nrow(data), 1, p)
  data[rx == 0, "PY200G"] <- NA
  data
}

imp<-mice(d1,m=5,method="norm.predict")
fit <- with(imp, lm(PY200G  ~PW190))
tab <- summary(pool(fit), "all", conf.int = TRUE)
as.numeric(tab["PY200G", c("estimate", "2.5 %", "97.5 %")])

test.impute <- function(data, m = 5, method = "norm", ...) {
  imp <- mice(data, method = method, m = m, print = FALSE, maxit=10, ...)
  fit <- with(imp, lm(PY200G ~PW190))
  tab <- summary(pool(fit), "all", conf.int = TRUE)
  as.numeric(tab[2, c("estimate", "2.5 %", "97.5 %")])
}

  simulate <- function(runs = 10) {
    res <- array(NA, dim = c(2, runs, 3))
    dimnames(res) <- list(c("norm.predict", "norm.nob"),
                          as.character(1:runs),
                          c("estimate", "2.5 %","97.5 %"))
    for(run in 1:runs) {
      data=d1[sample(nrow(d1),"1000"),]

      data <- make.missing(data)
      #Linear regression, predicted values
      res[1, run, ] <- test.impute(data, method = "norm.predict",
                                   m = 2)
      #Linear regression ignoring model error.
      res[2, run, ] <- test.impute(data, method = "norm.nob")
    }
    res
  }

##Run Simulations, one thousand runs chosen because of the prevelance of this  .
set.seed(101)
res <- simulate(1000) 
res50=res



#Missingness 25%.
  
  make.missing <- function(data, p = 0.25){
    rx <- rbinom(nrow(data), 1, p)
    data[rx == 0, "PY200G"] <- NA
    data
  }
  set.seed(102)
  res <- simulate(1000) 
  res25=res
  
  rm(res)
  #Missingness 10%. 
  
  make.missing <- function(data, p = 0.1){
    rx <- rbinom(nrow(data), 1, p)
    data[rx == 0, "PY200G"] <- NA
    data
  }
  set.seed(103)
  res <- simulate(1000) 
  res10=res
  


#Assessments of imputations.

# Finding true mean
true=mean(d$PW190,na.rm = TRUE)

#obtain estimate means
apply(res50, c(1, 3), mean, na.rm = TRUE)

apply(res25, c(1, 3), mean, na.rm = TRUE)

apply(res10, c(1, 3), mean, na.rm = TRUE)

#Quality Statistics.

RawBias=rowMeans(res10[,, "estimate"]) - true
PercentageBias <- 100 * abs((rowMeans(res10[,, "estimate"]) - true)/ true)
CoverageRate <- rowMeans(res10[,, "2.5 %"] < true & true < res10[,, "97.5 %"])
AverageWidth <- rowMeans(res10[,, "97.5 %"] - res10[,, "2.5 %"])
RootMeansSquarError <- sqrt(rowMeans((res10[,, "estimate"] - true)^2))

COMP10=data.frame(RawBias, PercentageBias, CoverageRate, AverageWidth, RootMeansSquarError)


RawBias=rowMeans(res25[,, "estimate"]) - true
PercentageBias <- 100 * abs((rowMeans(res25[,, "estimate"]) - true)/ true)
CoverageRate <- rowMeans(res25[,, "2.5 %"] < true & true < res25[,, "97.5 %"])
AverageWidth <- rowMeans(res25[,, "97.5 %"] - res25[,, "2.5 %"])
RootMeansSquarError <- sqrt(rowMeans((res25[,, "estimate"] - true)^2))

COMP25=data.frame(RawBias, PercentageBias, CoverageRate, AverageWidth, RootMeansSquarError)

RawBias=rowMeans(res50[,, "estimate"]) - true
PercentageBias <- 100 * abs((rowMeans(res50[,, "estimate"]) - true)/ true)
CoverageRate <- rowMeans(res50[,, "2.5 %"] < true & true < res50[,, "97.5 %"])
AverageWidth <- rowMeans(res50[,, "97.5 %"] - res50[,, "2.5 %"])
RootMeansSquarError <- sqrt(rowMeans((res50[,, "estimate"] - true)^2))

COMP50=data.frame(RawBias, PercentageBias, CoverageRate, AverageWidth, RootMeansSquarError)

#Plot visualizations of 50% missingness.
d50=as.data.frame(res50["norm.predict",,"estimate"])
d50$Sim=1:1000
d50$Model="Norm.Predict"
d50b=as.data.frame(res50["norm.nob",,"estimate"])
d50b$Sim=1:1000
d50b$Model="Norm.Nob"
colnames(d50b)=c("Estimate","Simulation","Model")
colnames(d50)=c("Estimate","Simulation","Model")

d50c=rbind(d50,d50b)

ggplot(d50c, aes(x=Simulation, y = Estimate, group = Model, colour = Model)) + 
  geom_line()+labs(title = "Simulation with 50% Missingness")+
  theme(plot.title = element_text(hjust = 0.5))
#Plot visualizations of 25% missingness.
d25=as.data.frame(res25["norm.predict",,"estimate"])
d25$Sim=1:1000
d25$Model="Norm.Predict"
d25b=as.data.frame(res25["norm.nob",,"estimate"])
d25b$Sim=1:1000
d25b$Model="Norm.Nob"
colnames(d25b)=c("Estimate","Simulation","Model")
colnames(d25)=c("Estimate","Simulation","Model")

d25c=rbind(d25,d25b)

ggplot(d25c, aes(x=Simulation, y = Estimate, group = Model, colour = Model)) + 
  geom_line()+labs(title = "Simulation with 25% Missingness")+
  theme(plot.title = element_text(hjust = 0.5))
#Plot visualizations of 25% missingness.

d10=as.data.frame(res25["norm.predict",,"estimate"])
d10$Sim=1:1000
d10$Model="Norm.Predict"
d10b=as.data.frame(res25["norm.nob",,"estimate"])
d10b$Sim=1:1000
d10b$Model="Norm.Nob"
colnames(d10b)=c("Estimate","Simulation","Model")
colnames(d10)=c("Estimate","Simulation","Model")

d10c=rbind(d10,d10b)
#Plot visualizations of 10% missingness.

ggplot(d10c, aes(x=Simulation, y = Estimate, group = Model, colour = Model)) + 
  geom_line()+labs(title = "Simulation with 10% Missingness")+
  theme(plot.title = element_text(hjust = 0.5))













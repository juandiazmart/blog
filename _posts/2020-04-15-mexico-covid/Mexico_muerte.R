library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(EnvStats)
library(tidyverse)
library(countrycode)
library(bayesplot)

setwd("~/JPD/juandiazmart.github.io/blog/_posts/2020-04-15-mexico-covid")

pop<-127091642

muertes_mx <- 
  read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_deaths_mx.csv") %>% 
  pivot_longer(-Estado, names_to = "date", values_to = "deaths") %>% 
  mutate(date = dmy(date)) %>% rename(iso3c=Estado) %>% 
  group_by(iso3c)  %>% 
  mutate(new_deaths=c(0,diff(deaths)))  %>% ungroup() %>%
  select(iso3c, date,deaths,new_deaths) 


confirmados_mx <- 
  read_csv("https://raw.githubusercontent.com/mariorz/covid19-mx-time-series/master/data/covid19_confirmed_mx.csv") %>% 
  pivot_longer(-Estado, names_to = "date", values_to = "confirmed") %>% 
  mutate(date = dmy(date)) %>% rename(iso3c=Estado) %>% 
  group_by(iso3c)  %>% 
  mutate(new_confirmed=c(0,diff(confirmed)))  %>% ungroup() %>%
  select(iso3c, date,confirmed,new_confirmed) 

estados <- confirmados_mx %>%
  full_join(muertes_mx, by = c("iso3c", "date"))

d<-estados %>% 
  group_by(date) %>%
  summarise_at(vars(-iso3c),funs(sum)) %>% mutate(iso3c="MEX") %>% bind_rows(estados)




cfr.by.country = read.csv("weighted_fatality.csv")
cfr.by.country$country = as.character(cfr.by.country[,2])

serial.interval = read.csv("serial_interval.csv")
covariates = read.csv('interventions.csv', stringsAsFactors = FALSE)
covariates <- covariates[1, c(1,2,3,4,5,6, 7, 8)]
covariates<-rename(covariates,Country=ï..Country)

p <- ncol(covariates) - 1
forecast = 0

N2 = 90 # Increase this for a further forecast

dates = list()
reported_cases = list()
stan_data = list(M=1,N=NULL,
                 covariate1=NULL,covariate2=NULL,covariate3=NULL,covariate4=NULL,covariate5=NULL,covariate6=NULL,covariate7=NULL,deaths=NULL,f=NULL,
                 N0=6,cases=NULL,SI=serial.interval$fit[1:N2],pop=pop,
                 EpidemicStart = NULL)
deaths_by_country = list()

CFR=cfr.by.country$weighted_fatality[cfr.by.country$country == "MEX"]
  
covariates1 <- covariates[covariates$Country == "MEX", 2:8]
for (i in 1:length(covariates1)) {
  covariates1[,i]<-mdy(covariates1[,i])
}

  
d1<-d %>% filter(d$iso3c=="MEX") 

d1$t = decimal_date(d1$date) 
d1=d1[order(d1$t),]
index1 = which(cumsum(d1$new_deaths)>=20)[1] # also 5
index2 = index1-20
  

d1=d1[index2:nrow(d1),]
stan_data$EpidemicStart = c(stan_data$EpidemicStart,index1+1-index2)
  
  
for (ii in 1:ncol(covariates1)) {
  covariate = names(covariates1)[ii]
  d1[covariate] <- (as.Date(d1$date, format='%d/%m/%Y') >= as.Date(covariates1[1,covariate]))*1 
}
  
dates[["MEX"]] = d1$date
  
N = length(d1$new_confirmed)

forecast = N2 - N
if(forecast < 0) {
  print(sprintf("%s: %d", Country, N))
  print("ERROR!!!! increasing N2")
  N2 = N
  forecast = N2 - N
}
  
h = rep(0,forecast+N) # discrete hazard rate from time t = 1, ..., 100

mean1 = 5.1; cv1 = 0.86; # infection to onset
mean2 = 18.8; cv2 = 0.45 # onset to death
## assume that CFR is probability of dying given infection
x1 = rgammaAlt(5e6,mean1,cv1) # infection-to-onset ----> do all people who are infected get to onset?
x2 = rgammaAlt(5e6,mean2,cv2) # onset-to-death
f = ecdf(x1+x2)
convolution = function(u) (CFR * f(u))
    
h[1] = (convolution(1.5) - convolution(0)) 

for(i in 2:length(h)) {
  h[i] = (convolution(i+.5) - convolution(i-.5)) / (1-convolution(i-.5))
}

s = rep(0,N2)
s[1] = 1 
for(i in 2:N2) {
  s[i] = s[i-1]*(1-h[i-1])
}
f = s * h
  

reported_cases[["MEX"]] = as.vector(as.numeric(d1$new_confirmed))
deaths=c(as.vector(as.numeric(d1$new_deaths)),rep(-1,forecast))
cases=c(as.vector(as.numeric(d1$new_confirmed)),rep(-1,forecast))
deaths_by_country[["MEX"]] = as.vector(as.numeric(d1$new_deaths))
covariates2 <- as.data.frame(d1[, colnames(covariates1)])
covariates2[N:(N+forecast),] <- covariates2[N,]
  
## append data
stan_data$N = c(stan_data$N,N)
# stan_data$x = cbind(stan_data$x,x)
stan_data$covariate1 = cbind(stan_data$covariate1,covariates2[,1])
stan_data$covariate2 = cbind(stan_data$covariate2,covariates2[,2])
stan_data$covariate3 = cbind(stan_data$covariate3,covariates2[,3])
stan_data$covariate4 = cbind(stan_data$covariate4,covariates2[,4])
stan_data$covariate5 = cbind(stan_data$covariate5,covariates2[,5])
stan_data$covariate6 = cbind(stan_data$covariate6,covariates2[,6])
stan_data$covariate7 = cbind(stan_data$covariate7,covariates2[,7]) 
stan_data$f = cbind(stan_data$f,f)
stan_data$deaths = cbind(stan_data$deaths,deaths)
stan_data$cases = cbind(stan_data$cases,cases)
  
stan_data$N2=N2
stan_data$x=1:N2
if(length(stan_data$N) == 1) {
  stan_data$N = as.array(stan_data$N)
}
  

stan_data$covariate2 = 0 * stan_data$covariate2 # remove travel bans
stan_data$covariate4 = 0 * stan_data$covariate4 # remove sport
  

stan_data$covariate2 = stan_data$covariate7 # self-isolating if ill
  # create the `any intervention` covariate
stan_data$covariate4 = 1*as.data.frame((stan_data$covariate1+
                                            stan_data$covariate3+
                                            stan_data$covariate5+
                                            stan_data$covariate6+
                                            stan_data$covariate7) >= 1)
stan_data$covariate5 = stan_data$covariate5
#stan_data$covariate5 = 0 * stan_data$covariate5
stan_data$covariate6 = stan_data$covariate6 # social distancing encouraged
stan_data$covariate7 = 0 # models should only take 6 covariates


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan-models/',"base",'.stan'))


fit = sampling(m,data=stan_data,iter=10000,warmup=5000,chains=3,thin=4,control = list(adapt_delta = 0.90, max_treedepth = 10))


out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deaths.cf = out$E_deaths0

save.image(paste0('results/',"base",'-',Sys.Date(),'.Rdata'))

save(fit,prediction,dates,reported_cases,deaths_by_country,estimated.deaths,estimated.deaths.cf,out,covariates,file=paste0('results/',"base",'-',Sys.Date(),'-stanfit.Rdata'))

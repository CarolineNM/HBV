rm(list=ls())
require(tidyverse)
require(rjags)
require(binom)
require(readxl)
require(MCMCvis)

# Edityearvacc_cat=readxl::read_excel("Data/Newmodelinputdat.xlsx",sheet="allinputdat")
# Edityearvacc_cat

# Input_dat=read_excel("Data/Modelinput_hepB.xlsx",sheet="Inputdata2")
# Input_dat

Input_dat=read_excel("Data/Newmodelinputdat.xlsx",sheet="allinputdat")
Input_dat



#define model
#############Age independent FOI##
#define model
###before 2007
jcode <- "model{ 

###Unvaccinated
##before 2007

for (i in 1:4) {
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-lambda*age[i]) #catalytic model
}
##2007-2009

for (i in 5:8){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-lambda_07*age[i])),
(1-exp(-lambda*(age[i]-2))*exp(-lambda_07*2)))
}

##2009-2011

for (i in 9:12){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-lambda_09*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-lambda_07*(age[i]-2))*exp(-lambda_09*2)),
(1-exp(-lambda*(age[i]-4))*exp(-lambda_07*2)*exp(-lambda_09*2))))
}

##2011-2013

for (i in 13:16){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-lambda_11*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-lambda_09*(age[i]-2))*exp(-lambda_11*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-lambda_07*(age[i]-4))*exp(-lambda_09*2)*exp(-lambda_11*2)),
(1-exp(-lambda*(age[i]-6))*exp(-lambda_07*2)*exp(-lambda_09*2)*exp(-lambda_11*2)))))
}

###2013-2015

for (i in 17:20){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-lambda_13*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-lambda_11*(age[i]-2))*exp(-lambda_13*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-lambda_09*(age[i]-4))*exp(-lambda_11*2)*exp(-lambda_13*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-lambda_07*(age[i]-6))*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2)),
(1-exp(-lambda*(age[i]-8))*exp(-lambda_07*2)*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2))))))
}

###2015-2017

for (i in 21:24){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-lambda_15*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-lambda_13*(age[i]-2))*exp(-lambda_15*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-lambda_11*(age[i]-4))*exp(-lambda_13*2)*exp(-lambda_15*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-lambda_09*(age[i]-6))*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)),ifelse(age[i]>=8 && age[i]< 10,
(1-exp(-lambda_07*(age[i]-8))*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)),
(1-exp(-lambda*(age[i]-10))*exp(-lambda_07*2)*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)))))))
}

###2017-2019

for (i in 25:28){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-lambda_17*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-lambda_15*(age[i]-2))*exp(-lambda_17*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-lambda_13*(age[i]-4))*exp(-lambda_15*2)*exp(-lambda_17*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-lambda_11*(age[i]-6))*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2)),ifelse(age[i]>=8 && age[i]< 10,
(1-exp(-lambda_09*(age[i]-8))*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2)),ifelse(age[i]>=10 && age[i]< 12,
(1-exp(-lambda_07*(age[i]-10))*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2)),
(1-exp(-lambda*(age[i]-12))*exp(-lambda_07*2)*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2))))))))
}

#2019-2021

for (i in 29:32){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-lambda_19*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-lambda_17*(age[i]-2))*exp(-lambda_19*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-lambda_15*(age[i]-4))*exp(-lambda_17*2)*exp(-lambda_19*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-lambda_13*(age[i]-6))*exp(-lambda_15*2)*exp(-lambda_17*2)*exp(-lambda_19*2)),ifelse(age[i]>=8 && age[i]< 10,
(1-exp(-lambda_11*(age[i]-8))*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2)*exp(-lambda_19*2)),ifelse(age[i]>=10 && age[i]< 12,
(1-exp(-lambda_09*(age[i]-10))*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2)*exp(-lambda_19*2)),ifelse(age[i]>=12 && age[i]< 14,
(1-exp(-lambda_07*(age[i]-12))*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2)*exp(-lambda_19*2)),
(1-exp(-lambda*(age[i]-14))*exp(-lambda_07*2)*exp(-lambda_09*2)*exp(-lambda_11*2)*exp(-lambda_13*2)*exp(-lambda_15*2)*exp(-lambda_17*2)*exp(-lambda_19*2)))))))))
}


############vaccinated
######before 2007

for (i in 33:36) {
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = 1-exp(-(lambda*omega)*age[i]) #catalytic model
}
##2007-2009

for (i in 37:40){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-(lambda_07*omega)*age[i])),
(1-exp(-(lambda*omega)*(age[i]-2))*exp(-(lambda_07*omega)*2)))
}

##2009-2011

for (i in 41:44){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-(lambda_09*omega)*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-(lambda_07*omega)*(age[i]-2))*exp(-(lambda_09*omega)*2)),
(1-exp(-(lambda*omega)*(age[i]-4))*exp(-(lambda_07*omega)*2)*exp(-(lambda_09*omega)*2))))
}

##2011-2013

for (i in 45:48){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-(lambda_11*omega)*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-(lambda_09*omega)*(age[i]-2))*exp(-(lambda_11*omega)*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-(lambda_07*omega)*(age[i]-4))*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)),
(1-exp(-(lambda*omega)*(age[i]-6))*exp(-(lambda_07*omega)*2)*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)))))
}

###2013-2015

for (i in 49:52){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-(lambda_13*omega)*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-(lambda_11*omega)*(age[i]-2))*exp(-(lambda_13*omega)*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-(lambda_09*omega)*(age[i]-4))*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-(lambda_07*omega)*(age[i]-6))*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)),
(1-exp(-(lambda*omega)*(age[i]-8))*exp(-(lambda_07*omega)*2)*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2))))))
}

###2015-2017

for (i in 53:56){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-(lambda_15*omega)*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-(lambda_13*omega)*(age[i]-2))*exp(-(lambda_15*omega)*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-(lambda_11*omega)*(age[i]-4))*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-(lambda_09*omega)*(age[i]-6))*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)),ifelse(age[i]>=8 && age[i]< 10,
(1-exp(-(lambda_07*omega)*(age[i]-8))*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)),
(1-exp(-(lambda*omega)*(age[i]-10))*exp(-(lambda_07*omega)*2)*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)))))))
}

###2017-2019##

for (i in 57:60){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-(lambda_17*omega)*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-(lambda_15*omega)*(age[i]-2))*exp(-(lambda_17*omega)*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-(lambda_13*omega)*(age[i]-4))*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-(lambda_11*omega)*(age[i]-6))*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)),ifelse(age[i]>=8 && age[i]< 10,
(1-exp(-(lambda_09*omega)*(age[i]-8))*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)),ifelse(age[i]>=10 && age[i]< 12,
(1-exp(-(lambda_07*omega)*(age[i]-10))*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)),
(1-exp(-(lambda*omega)*(age[i]-12))*exp(-(lambda_07*omega)*2)*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2))))))))
}

#2019-2021

for (i in 61:64){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(1 - exp(-(lambda_19*omega)*age[i])),ifelse(age[i]>=2 && age[i]< 4,
(1-exp(-(lambda_17*omega)*(age[i]-2))*exp(-(lambda_19*omega)*2)),ifelse(age[i]>=4 && age[i]< 6,
(1-exp(-(lambda_15*omega)*(age[i]-4))*exp(-(lambda_17*omega)*2)*exp(-(lambda_19*omega)*2)),ifelse(age[i]>=6 && age[i]< 8,
(1-exp(-(lambda_13*omega)*(age[i]-6))*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)*exp(-(lambda_19*omega)*2)),ifelse(age[i]>=8 && age[i]< 10,
(1-exp(-(lambda_11*omega)*(age[i]-8))*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)*exp(-(lambda_19*omega)*2)),ifelse(age[i]>=10 && age[i]< 12,
(1-exp(-(lambda_09*omega)*(age[i]-10))*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)*exp(-(lambda_19*omega)*2)),ifelse(age[i]>=12 && age[i]< 14,
(1-exp(-(lambda_07*omega)*(age[i]-12))*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)*exp(-(lambda_19*omega)*2)),
(1-exp(-(lambda*omega)*(age[i]-14))*exp(-(lambda_07*omega)*2)*exp(-(lambda_09*omega)*2)*exp(-(lambda_11*omega)*2)*exp(-(lambda_13*omega)*2)*exp(-(lambda_15*omega)*2)*exp(-(lambda_17*omega)*2)*exp(-(lambda_19*omega)*2)))))))))
}


  lambda ~ dunif(0,1)  #uninformative prior
  lambda_07 ~ dunif(0,1) #uninformative prior
  lambda_09 ~ dunif(0,1) #uninformative prior
  lambda_11 ~ dunif(0,1) #uninformative prior
  lambda_13 ~ dunif(0,1) #uninformative prior
  lambda_15 ~ dunif(0,1) #uninformative prior
  lambda_17 ~ dunif(0,1) #uninformative prior
  lambda_19 ~ dunif(0,1) #uninformative prior
  omega~ dunif(0,1)      #uninformative prior
}"
# Run model
mcmc.length=10000
jdat = list(n.pos= Input_dat$Pos,
            N=Input_dat$Total,
            age=Input_dat$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=3, n.adapt=2000)
update(jmod)
jpos = coda.samples(jmod, c("lambda","lambda_07","lambda_09","lambda_11","lambda_13","lambda_15","lambda_17","lambda_19","omega"), n.iter=mcmc.length)
# check convergence
par(mar = c(2, 2, 2, 1.5))
plot(jpos)
dev.off()
MCMCsummary(jpos, round = 3) ## Check ESS and Rhat
mcmcMatrix <- as.matrix(jpos)
dev.off()
# Plotting posterior distributions of all parameters
mcmcDF <- as_tibble(mcmcMatrix)
mcmcDF %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()

## Create point estimates for all parameters##
lambda_est <- mcmcMatrix[, "lambda"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_07_est <- mcmcMatrix[, "lambda_07"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_09_est <- mcmcMatrix[, "lambda_09"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_11_est <- mcmcMatrix[, "lambda_11"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_13_est <- mcmcMatrix[, "lambda_13"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_15_est <- mcmcMatrix[, "lambda_15"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_17_est <- mcmcMatrix[, "lambda_17"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_19_est <- mcmcMatrix[, "lambda_19"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
omega_est <- mcmcMatrix[, "omega"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()


## Create data for plots
## Samples from mcmc chains for credible intervals
##option 2

paramVector <- c("lambda",
                 "lambda_07",
                 "lambda_09",
                 "lambda_11",
                 "lambda_13",
                 "lambda_15",
                 "lambda_17",
                 "lambda_19",
                 "omega")

paramEstimates = list(lambda_est,
                      lambda_07_est,
                      lambda_09_est,
                      lambda_11_est,
                      lambda_13_est,
                      lambda_15_est,
                      lambda_17_est,
                      lambda_19_est,
                      omega_est)

## Outputting point estimates for inclusion within tables
varOutput = c()
for(i in 1:length(paramEstimates)){
  var = paramEstimates[[i]]
  varOut = paste(round(var[[1]],3)," (",round(var[[2]],3)," - ",round(var[[3]],3),")",sep = "")
  varOutput = c(varOutput,varOut)
}

correct_joint = data.frame(paramVector,varOutput)
# 
# ##export to excel
write.csv(correct_joint,file="Data/simplecatall_out.csv")
ager=0:14
ager1=0:1
ager2=2:14
ager3=2:3
ager4=4:14
ager5=4:5
ager6=6:14
ager7=6:7
ager8=8:14
ager9=8:9
ager10=10:14
ager11=10:11
ager12=12:14
ager13=12:13
ager14=14

numSamples = 1000
foiVector = paramVector

for(ii in 1:length(foiVector)){
  outDf_07 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_09 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_11 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_13 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_15 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_17 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_19 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_21 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_07 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_09 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_11 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_13 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_15 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_17 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_19 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_21 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_07 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_09 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_11 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_13 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_15 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_17 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_19 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_21 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  
  foiyoung <- foiVector[1]
  foimid <- foiVector[2]
  foimid2 <- foiVector[3]
  foimid3 <- foiVector[4]
  foimid4 <- foiVector[5]
  foimid5 <- foiVector[6]
  foimid6 <- foiVector[7]
  foiold <- foiVector[8]
  
  for (kk in 1:numSamples ) {
    
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,foiyoung]
    lambdaSample07 <- mcmcMatrix[randomNumber,foimid]
    lambdaSample09 <- mcmcMatrix[randomNumber,foimid2]
    lambdaSample11 <- mcmcMatrix[randomNumber,foimid3]
    lambdaSample13 <- mcmcMatrix[randomNumber,foimid4]
    lambdaSample15 <- mcmcMatrix[randomNumber,foimid5]
    lambdaSample17 <- mcmcMatrix[randomNumber,foimid6]
    lambdaSample19 <- mcmcMatrix[randomNumber,foiold]
    omegaSample<-mcmcMatrix[randomNumber,"omega"]
    
    
    ##before2007
    allV_07 <- (1-exp(-(lambdaSample*omegaSample)*ager))
    allU_07 <- (1-exp(-(lambdaSample*ager)))
    
    all_07=allV_07+allU_07
    allV_07=allV_07
    allU_07=allU_07
    
    
    ##2007-2009
    youngV_09 <- (1-exp(-(lambdaSample07*omegaSample*ager1)))
    youngU_09 <- (1-exp(-(lambdaSample07*ager1)))
    
    oldV_09 <-(1-exp(-(lambdaSample*omegaSample)*(ager2-2))*
                 exp(-1*lambdaSample07*omegaSample*2)) 
    oldU_09 <- (1-exp(-lambdaSample*(ager2-2))*exp(-1*lambdaSample07*2))
    
    all_09 <- c((youngV_09+youngU_09),(oldV_09+oldU_09))
    
    allV_09=c(youngV_09,oldV_09)
    allU_09=c(youngU_09,oldU_09)
    
    ##2009-2011
    youngV_11 <- (1-exp(-(lambdaSample09*omegaSample)*ager1))
    youngU_11 <- (1-exp(-lambdaSample09*ager1))
    
    
    midV_11 <- (1-exp(-(lambdaSample07*omegaSample)*(ager3-2))*
                  exp(-1*lambdaSample09*omegaSample*2))
    midU_11 <- (1-exp(-(lambdaSample07)*(ager3-2))*
                  exp(-1*lambdaSample09*2))
    
    oldV_11 <- (1-exp(-(lambdaSample*omegaSample)*(ager4-4))*
                  exp(-1*((lambdaSample07*omegaSample)*2))*
                  exp(-1*((lambdaSample09*omegaSample)*2)))
    
    oldU_11 <- (1-exp(-(lambdaSample)*(ager4-4))*
                  exp(-1*((lambdaSample07)*2))*
                  exp(-1*((lambdaSample09)*2)))
    
    all_11 <- c((youngV_11+youngU_11),(midV_11+midU_11),(oldV_11+oldU_11))
    
    allV_11<-c(youngV_11,midV_11,oldV_11)
    allU_11<-c(youngU_11,midU_11,oldU_11)          
    
    
    ##2011-2013
    youngV_13 <- (1-exp(-ager1*(lambdaSample11*omegaSample)))
    youngU_13 <- (1-exp(-ager1*(lambdaSample11)))
    
    
    midV_13 <- (1-exp(-(lambdaSample09*omegaSample)*(ager3-2))*
                exp(-1*(lambdaSample11*omegaSample*2)))
    
    midU_13 <- (1-exp(-(lambdaSample09)*(ager3-2))*
                  exp(-1*(lambdaSample11*2)))
    
    
    mid2V_13 <-  (1-exp(-(lambdaSample07*omegaSample)*(ager5-4))*
                    exp(-1*(lambdaSample09*omegaSample*2))*
                    exp(-1*(lambdaSample11*omegaSample*2))) 
    
    mid2U_13 <- (1-exp(-(lambdaSample07)*(ager5-4))*
                   exp(-1*(lambdaSample09*2))*
                   exp(-1*(lambdaSample11*2))) 
    
    
    oldV_13 <-  (1-exp(-(lambdaSample*omegaSample)*(ager6-6))*
                    exp(-1*(lambdaSample07*omegaSample*2))*
                    exp(-1*(lambdaSample09*omegaSample*2))*
                    exp(-1*(lambdaSample11*omegaSample*2))) 
    
    
    oldU_13 <-   (1-exp(-(lambdaSample)*(ager6-6))*
                    exp(-1*(lambdaSample07*2))*
                    exp(-1*(lambdaSample09*2))*
                    exp(-1*(lambdaSample11*2))) 
    
    all_13 <- c(youngV_13+youngU_13,midV_13+midU_13,mid2V_13+mid2U_13,oldV_13+oldU_13)
    
    allV_13<-c(youngV_13,midV_13,mid2V_13,oldV_13)
    allU_13<-c(youngU_13,midU_13,mid2U_13,oldU_13)
    
    
    ##2013-2015
    
    youngV_15 <- (1-exp(-ager1*(lambdaSample13*omegaSample)))
    youngU_15 <- (1-exp(-ager1*(lambdaSample13)))
    
    
    midV_15 <- (1-exp(-(lambdaSample11*omegaSample)*(ager3-2))*
                  exp(-1*(lambdaSample13*omegaSample*2)))
    
    midU_15 <- (1-exp(-(lambdaSample11)*(ager3-2))*
                  exp(-1*(lambdaSample13*2)))
    
    
    mid2V_15 <- (1-exp(-(lambdaSample09*omegaSample)*(ager5-4))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))) 
    
    mid2U_15 <- (1-exp(-(lambdaSample09)*(ager5-4))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))) 
    
    
    
    mid3V_15 <- (1-exp(-(lambdaSample07*omegaSample)*(ager7-6))*
                   exp(-1*(lambdaSample09*omegaSample*2))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))) 
    
    mid3U_15 <- (1-exp(-(lambdaSample07)*(ager7-6))*
                   exp(-1*(lambdaSample09*2))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))) 
    
    
    oldV_15 <-  (1-exp(-(lambdaSample*omegaSample)*(ager8-8))*
                   exp(-1*(lambdaSample07*omegaSample*2))*
                   exp(-1*(lambdaSample09*omegaSample*2))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))) 
    
    
    oldU_15 <-  (1-exp(-(lambdaSample)*(ager8-8))*
                   exp(-1*(lambdaSample07*2))*
                   exp(-1*(lambdaSample09*2))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))) 
    
    all_15 <- c(youngV_15+youngU_15,midV_15+midU_15,mid2V_15+mid2U_15,mid3V_15+mid3U_15,oldV_15+oldU_15)
    
    allV_15<-c(youngV_15,midV_15,mid2V_15,mid3V_15,oldV_15)
    allU_15<-c(youngU_15,midU_15,mid2U_15,mid3U_15,oldU_15)
    
  
    ##2015-2017
    youngV_17 <- (1-exp(-ager1*(lambdaSample15*omegaSample)))
    youngU_17 <- (1-exp(-ager1*(lambdaSample15)))
    
    midV_17 <- (1-exp(-(lambdaSample13*omegaSample)*(ager3-2))*
                  exp(-1*(lambdaSample15*omegaSample*2)))
    
    midU_17 <- (1-exp(-(lambdaSample13)*(ager3-2))*
                  exp(-1*(lambdaSample15*2)))
    
    mid2V_17 <- (1-exp(-(lambdaSample11*omegaSample)*(ager5-4))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))) 
    
    mid2U_17 <- (1-exp(-(lambdaSample11)*(ager5-4))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2)))
    
    mid3V_17 <- (1-exp(-(lambdaSample09*omegaSample)*(ager7-6))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))) 
    
    mid3U_17 <- (1-exp(-(lambdaSample09)*(ager7-6))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2))) 
    
    mid4V_17 <- (1-exp(-(lambdaSample07*omegaSample)*(ager9-8))*
                   exp(-1*(lambdaSample09*omegaSample*2))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))) 
    
    
    mid4U_17 <- (1-exp(-(lambdaSample07)*(ager9-8))*
                   exp(-1*(lambdaSample09*2))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2)))
    
    oldV_17 <- (1-exp(-(lambdaSample*omegaSample)*(ager10-10))*
                  exp(-1*(lambdaSample07*omegaSample*2))*
                  exp(-1*(lambdaSample09*omegaSample*2))*
                  exp(-1*(lambdaSample11*omegaSample*2))*
                  exp(-1*(lambdaSample13*omegaSample*2))*
                  exp(-1*(lambdaSample15*omegaSample*2)))
    
    oldU_17 <- (1-exp(-(lambdaSample)*(ager10-10))*
                  exp(-1*(lambdaSample07*2))*
                  exp(-1*(lambdaSample09*2))*
                  exp(-1*(lambdaSample11*2))*
                  exp(-1*(lambdaSample13*2))*
                  exp(-1*(lambdaSample15*2)))
    
    all_17 <- c(youngV_17+youngU_17,midV_17+midU_17,mid2V_17+mid2U_17,mid3V_17+mid3U_17,mid4V_17+mid4U_17,oldV_17+oldU_17)
    
    allV_17<-c(youngV_17,midV_17,mid2V_17,mid3V_17,mid4V_17,oldV_17)
    allU_17<-c(youngU_17,midU_17,mid2U_17,mid3U_17,mid4U_17,oldU_17)
    
    ##2017-2019
    
    youngV_19 <- (1-exp(-ager1*(lambdaSample17*omegaSample)))
    youngU_19 <- (1-exp(-ager1*(lambdaSample17)))
    
    midV_19 <- (1-exp(-(lambdaSample15*omegaSample)*(ager3-2))*
                  exp(-1*(lambdaSample17*omegaSample*2)))
    
    midU_19 <- (1-exp(-(lambdaSample15)*(ager3-2))*
                  exp(-1*(lambdaSample17*2)))
    
    
    mid2V_19 <- (1-exp(-(lambdaSample13*omegaSample)*(ager5-4))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2))) 
    
    mid2U_19 <- (1-exp(-(lambdaSample13)*(ager5-4))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2)))
    
    mid3V_19 <- (1-exp(-(lambdaSample11*omegaSample)*(ager7-6))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2))) 
    
    
    mid3U_19 <- (1-exp(-(lambdaSample11)*(ager7-6))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2))) 
    
    
    mid4V_19 <- (1-exp(-(lambdaSample09*omegaSample)*(ager9-8))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2)))
    
    
    mid4U_19 <- (1-exp(-(lambdaSample09)*(ager9-8))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2)))
    
    mid5V_19 <- (1-exp(-(lambdaSample07*omegaSample)*(ager11-10))*
                   exp(-1*(lambdaSample09*omegaSample*2))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2)))
    
    
    mid5U_19 <- (1-exp(-(lambdaSample07)*(ager11-10))*
                   exp(-1*(lambdaSample09*2))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2)))
    
    oldV_19 <- (1-exp(-(lambdaSample*omegaSample)*(ager12-12))*
                  exp(-1*(lambdaSample07*omegaSample*2))*
                  exp(-1*(lambdaSample09*omegaSample*2))*
                  exp(-1*(lambdaSample11*omegaSample*2))*
                  exp(-1*(lambdaSample13*omegaSample*2))*
                  exp(-1*(lambdaSample15*omegaSample*2))*
                  exp(-1*(lambdaSample17*omegaSample*2)))
    
    oldU_19 <- (1-exp(-(lambdaSample)*(ager12-12))*
                  exp(-1*(lambdaSample07*2))*
                  exp(-1*(lambdaSample09*2))*
                  exp(-1*(lambdaSample11*2))*
                  exp(-1*(lambdaSample13*2))*
                  exp(-1*(lambdaSample15*2))*
                  exp(-1*(lambdaSample17*2)))
    
    all_19 <- c(youngV_19+youngU_19,midV_19+midU_19,mid2V_19+mid2U_19,mid3V_19+mid3U_19,mid4V_19+mid4U_19,mid5V_19+mid5U_19,oldV_19+oldU_19)
    
    allV_19<-c(youngV_19,midV_19,mid2V_19,mid3V_19,mid4V_19,mid5V_19,oldV_19)
    allU_19<-c(youngU_19,midU_19,mid2U_19,mid3U_19,mid4U_19,mid5U_19,oldU_19)
    
    
    
    ##2019-2021
    
    youngV_21 <- (1-exp(-ager1*(lambdaSample19*omegaSample)))
    youngU_21 <- (1-exp(-ager1*(lambdaSample19)))
    
    midV_21 <- (1-exp(-(lambdaSample17*omegaSample)*(ager3-2))*
                  exp(-1*(lambdaSample19*omegaSample*2)))
    
    midU_21 <- (1-exp(-(lambdaSample17)*(ager3-2))*
                  exp(-1*(lambdaSample19*2)))
    
    mid2V_21 <- (1-exp(-(lambdaSample15*omegaSample)*(ager5-4))*
                   exp(-1*(lambdaSample17*omegaSample*2))*
                   exp(-1*(lambdaSample19*omegaSample*2))) 
    
    mid2U_21 <- (1-exp(-(lambdaSample15)*(ager5-4))*
                   exp(-1*(lambdaSample17*2))*
                   exp(-1*(lambdaSample19*2)))
    
    mid3V_21 <- (1-exp(-(lambdaSample13*omegaSample)*(ager7-6))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2))*
                   exp(-1*(lambdaSample19*omegaSample*2))) 
    
    
    mid3U_21 <- (1-exp(-(lambdaSample13)*(ager7-6))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2))*
                   exp(-1*(lambdaSample19*2))) 
    
    
    mid4V_21 <- (1-exp(-(lambdaSample11*omegaSample)*(ager9-8))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2))*
                   exp(-1*(lambdaSample19*omegaSample*2)))
    
    
    mid4U_21 <- (1-exp(-(lambdaSample11)*(ager9-8))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2))*
                   exp(-1*(lambdaSample19*2)))
    
    mid5V_21 <- (1-exp(-(lambdaSample09*omegaSample)*(ager11-10))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2))*
                   exp(-1*(lambdaSample19*omegaSample*2)))
    
    
    mid5U_21 <- (1-exp(-(lambdaSample09)*(ager11-10))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2))*
                   exp(-1*(lambdaSample19*2)))
    
    
    mid6V_21 <- (1-exp(-(lambdaSample07*omegaSample)*(ager13-12))*
                   exp(-1*(lambdaSample09*omegaSample*2))*
                   exp(-1*(lambdaSample11*omegaSample*2))*
                   exp(-1*(lambdaSample13*omegaSample*2))*
                   exp(-1*(lambdaSample15*omegaSample*2))*
                   exp(-1*(lambdaSample17*omegaSample*2))*
                   exp(-1*(lambdaSample19*omegaSample*2)))
    
    mid6U_21 <- (1-exp(-(lambdaSample07)*(ager13-12))*
                   exp(-1*(lambdaSample09*2))*
                   exp(-1*(lambdaSample11*2))*
                   exp(-1*(lambdaSample13*2))*
                   exp(-1*(lambdaSample15*2))*
                   exp(-1*(lambdaSample17*2))*
                   exp(-1*(lambdaSample19*2)))
    
    
    oldV_21 <- (1-exp(-(lambdaSample*omegaSample)*(ager14-14))*
                  exp(-1*(lambdaSample07*omegaSample*2))*
                  exp(-1*(lambdaSample09*omegaSample*2))*
                  exp(-1*(lambdaSample11*omegaSample*2))*
                  exp(-1*(lambdaSample13*omegaSample*2))*
                  exp(-1*(lambdaSample15*omegaSample*2))*
                  exp(-1*(lambdaSample17*omegaSample*2))*
                  exp(-1*(lambdaSample19*omegaSample*2)))
    
    oldU_21 <- (1-exp(-(lambdaSample)*(ager14-14))*
                  exp(-1*(lambdaSample07*2))*
                  exp(-1*(lambdaSample09*2))*
                  exp(-1*(lambdaSample11*2))*
                  exp(-1*(lambdaSample13*2))*
                  exp(-1*(lambdaSample15*2))*
                  exp(-1*(lambdaSample17*2))*
                  exp(-1*(lambdaSample19*2)))
    
    all_21 <- c(youngV_21+youngU_21,midV_21+midU_21,mid2V_21+mid2U_21,mid3V_21+mid3U_21,mid4V_21+mid4U_21,mid5V_21+mid5U_21,mid6V_21+mid6U_21,oldV_21+oldU_21)
    
    allV_21<-c(youngV_21,midV_21,mid2V_21,mid3V_21,mid4V_21,mid5V_21,mid6V_21,oldV_21)
    allU_21<-c(youngU_21,midU_21,mid2U_21,mid3U_21,mid4U_21,mid5U_21,mid6U_21,oldU_21)   
    
    
    outDf_07[kk,] <-all_07
    outDf_09[kk,] <-all_09
    outDf_11[kk,]<-all_11
    outDf_13[kk,]<-all_13
    outDf_15[kk,]<-all_15
    outDf_17[kk,]<-all_17
    outDf_19[kk,]<-all_19
    outDf_21[kk,] <- all_21
    
    outDfV_07[kk,] <-allV_07
    outDfV_09[kk,] <-allV_09
    outDfV_11[kk,]<-allV_11
    outDfV_13[kk,]<-allV_13
    outDfV_15[kk,]<-allV_15
    outDfV_17[kk,]<-allV_17
    outDfV_19[kk,]<-allV_19
    outDfV_21[kk,] <- allV_21
    
    outDfU_07[kk,] <-allU_07
    outDfU_09[kk,] <-allU_09
    outDfU_11[kk,]<-allU_11
    outDfU_13[kk,]<-allU_13
    outDfU_15[kk,]<-allU_15
    outDfU_17[kk,]<-allU_17
    outDfU_19[kk,]<-allU_19
    outDfU_21[kk,] <- allU_21
    
    
  }
  
  ###2007
  
  quantileMatrix_07<- matrix(NA,nrow=ncol(outDf_07), ncol = 3)
  for(jj in 1:ncol(outDf_07)){
    quantiles <- outDf_07[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_07[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJ_2007 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_07[,1],
    upper = quantileMatrix_07[,3],
    lower = quantileMatrix_07[,2],
    year=rep(2007,15)
  )   
  
  quantileMatrixV_07 <- matrix(NA,nrow=ncol(outDfV_07), ncol = 3)
  for(jj in 1:ncol(outDfV_07)){
    quantiles <- outDfV_07[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_07[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJV_2007 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_07[,1],
    upper = quantileMatrixV_07[,3],
    lower = quantileMatrixV_07[,2],
    year=rep(2007,15)
  )    
  
  quantileMatrixU_07 <- matrix(NA,nrow=ncol(outDfU_07), ncol = 3)
  for(jj in 1:ncol(outDfU_07)){
    quantiles <- outDfU_07[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_07[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJU_2007 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_07[,1],
    upper = quantileMatrixU_07[,3],
    lower = quantileMatrixU_07[,2],
    year=rep(2007,15)
  )      
  
  
  ##2009
  quantileMatrix_09 <- matrix(NA,nrow=ncol(outDf_09), ncol = 3)
  for(jj in 1:ncol(outDf_09)){
    quantiles <- outDf_09[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_09[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJ_2009 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_09[,1],
    upper = quantileMatrix_09[,3],
    lower = quantileMatrix_09[,2],
    year=rep(2009,15)
  )   
  
  quantileMatrixV_09 <- matrix(NA,nrow=ncol(outDfV_09), ncol = 3)
  for(jj in 1:ncol(outDfV_09)){
    quantiles <- outDfV_09[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_09[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJV_2009 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_09[,1],
    upper = quantileMatrixV_09[,3],
    lower = quantileMatrixV_09[,2],
    year=rep(2009,15)
  )    
  
  quantileMatrixU_09 <- matrix(NA,nrow=ncol(outDfU_09), ncol = 3)
  for(jj in 1:ncol(outDfU_09)){
    quantiles <- outDfU_09[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_09[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJU_2009 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_09[,1],
    upper = quantileMatrixU_09[,3],
    lower = quantileMatrixU_09[,2],
    year=rep(2009,15)
  )      
  
  
  ##2011 
  quantileMatrix_11 <- matrix(NA,nrow=ncol(outDf_11), ncol = 3)
  for(jj in 1:ncol(outDf_11)){
    quantiles <- outDf_11[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_11[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJ_2011 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_11[,1],
    upper = quantileMatrix_11[,3],
    lower = quantileMatrix_11[,2],
    year=rep(2011,15)
  )
  
  quantileMatrixV_11 <- matrix(NA,nrow=ncol(outDfV_11), ncol = 3)
  for(jj in 1:ncol(outDfV_11)){
    quantiles <- outDfV_11[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_11[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJV_2011 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_11[,1],
    upper = quantileMatrixV_11[,3],
    lower = quantileMatrixV_11[,2],
    year=rep(2011,15)
  )  
  
  quantileMatrixU_11 <- matrix(NA,nrow=ncol(outDfU_11), ncol = 3)
  for(jj in 1:ncol(outDfU_11)){
    quantiles <- outDfU_11[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_11[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJU_2011 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_11[,1],
    upper = quantileMatrixU_11[,3],
    lower = quantileMatrixU_11[,2],
    year=rep(2011,15)
  )  
  
  ##2013 
  quantileMatrix_13 <- matrix(NA,nrow=ncol(outDf_13), ncol = 3)
  for(jj in 1:ncol(outDf_13)){
    quantiles <- outDf_13[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_13[jj,] <- quantiles
  }  
  # Create a dataframe for plotting
  NdfJ_2013 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_13[,1],
    upper = quantileMatrix_13[,3],
    lower = quantileMatrix_13[,2],
    year=rep(2013,15)
  )    
  
  quantileMatrixV_13 <- matrix(NA,nrow=ncol(outDfV_13), ncol = 3)
  for(jj in 1:ncol(outDfV_13)){
    quantiles <- outDfV_13[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_13[jj,] <- quantiles
  }  
  # Create a dataframe for plotting
  NdfJV_2013 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_13[,1],
    upper = quantileMatrixV_13[,3],
    lower = quantileMatrixV_13[,2],
    year=rep(2013,15)
  )      
  
  quantileMatrixU_13 <- matrix(NA,nrow=ncol(outDfU_13), ncol = 3)
  for(jj in 1:ncol(outDfU_13)){
    quantiles <- outDfU_13[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_13[jj,] <- quantiles
  }  
  # Create a dataframe for plotting
  NdfJU_2013 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_13[,1],
    upper = quantileMatrixU_13[,3],
    lower = quantileMatrixU_13[,2],
    year=rep(2013,15)
  )      
  
  ##2015   
  quantileMatrix_15 <- matrix(NA,nrow=ncol(outDf_15), ncol = 3)
  for(jj in 1:ncol(outDf_15)){
    quantiles <- outDf_15[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_15[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJ_2015 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_15[,1],
    upper = quantileMatrix_15[,3],
    lower = quantileMatrix_15[,2],
    year=rep(2015,15)
  ) 
  
  quantileMatrixV_15 <- matrix(NA,nrow=ncol(outDfV_15), ncol = 3)
  for(jj in 1:ncol(outDfV_15)){
    quantiles <- outDfV_15[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_15[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJV_2015 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_15[,1],
    upper = quantileMatrixV_15[,3],
    lower = quantileMatrixV_15[,2],
    year=rep(2015,15)
  ) 
  
  quantileMatrixU_15 <- matrix(NA,nrow=ncol(outDfU_15), ncol = 3)
  for(jj in 1:ncol(outDfU_15)){
    quantiles <- outDfU_15[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_15[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJU_2015 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_15[,1],
    upper = quantileMatrixU_15[,3],
    lower = quantileMatrixU_15[,2],
    year=rep(2015,15)
  ) 
  
  ##2017 
  quantileMatrix_17 <- matrix(NA,nrow=ncol(outDf_17), ncol = 3)
  for(jj in 1:ncol(outDf_17)){
    quantiles <- outDf_17[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_17[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJ_2017 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_17[,1],
    upper = quantileMatrix_17[,3],
    lower = quantileMatrix_17[,2],
    year=rep(2017,15)
  )   
  
  quantileMatrixV_17 <- matrix(NA,nrow=ncol(outDfV_17), ncol = 3)
  for(jj in 1:ncol(outDfV_17)){
    quantiles <- outDfV_17[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_17[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJV_2017 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_17[,1],
    upper = quantileMatrixV_17[,3],
    lower = quantileMatrixV_17[,2],
    year=rep(2017,15)
  )      
  
  quantileMatrixU_17 <- matrix(NA,nrow=ncol(outDfU_17), ncol = 3)
  for(jj in 1:ncol(outDfU_17)){
    quantiles <- outDfU_17[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_17[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJU_2017 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_17[,1],
    upper = quantileMatrixU_17[,3],
    lower = quantileMatrixU_17[,2],
    year=rep(2017,15)
  )       
  
  
  ##2019   
  quantileMatrix_19 <- matrix(NA,nrow=ncol(outDf_19), ncol = 3)
  for(jj in 1:ncol(outDf_19)){
    quantiles <- outDf_19[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_19[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJ_2019 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_19[,1],
    upper = quantileMatrix_19[,3],
    lower = quantileMatrix_19[,2],
    year=rep(2019,15)
  )       
  
  quantileMatrixV_19 <- matrix(NA,nrow=ncol(outDfV_19), ncol = 3)
  for(jj in 1:ncol(outDfV_19)){
    quantiles <- outDfV_19[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_19[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJV_2019 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_19[,1],
    upper = quantileMatrixV_19[,3],
    lower = quantileMatrixV_19[,2],
    year=rep(2019,15)
  )       
  
  quantileMatrixU_19 <- matrix(NA,nrow=ncol(outDfU_19), ncol = 3)
  for(jj in 1:ncol(outDfU_19)){
    quantiles <- outDfU_19[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_19[jj,] <- quantiles
  }
  # Create a dataframe for plotting
  NdfJU_2019 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_19[,1],
    upper = quantileMatrixU_19[,3],
    lower = quantileMatrixU_19[,2],
    year=rep(2019,15)
  )        
  
  
  ##2021
  quantileMatrix_21 <- matrix(NA,nrow=ncol(outDf_21), ncol = 3)
  for(jj in 1:ncol(outDf_21)){
    quantiles <- outDf_21[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrix_21[jj,] <- quantiles
  }     
  # Create a dataframe for plotting
  NdfJ_2021 = data.frame(
    midpoint = ager,
    mean=quantileMatrix_21[,1],
    upper = quantileMatrix_21[,3],
    lower = quantileMatrix_21[,2],
    year=rep(2021,15)
  )
  
  quantileMatrixV_21 <- matrix(NA,nrow=ncol(outDfV_21), ncol = 3)
  for(jj in 1:ncol(outDfV_21)){
    quantiles <- outDfV_21[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixV_21[jj,] <- quantiles
  }     
  # Create a dataframe for plotting
  NdfJV_2021 = data.frame(
    midpoint = ager,
    mean=quantileMatrixV_21[,1],
    upper = quantileMatrixV_21[,3],
    lower = quantileMatrixV_21[,2],
    year=rep(2021,15)
  )
  
  quantileMatrixU_21 <- matrix(NA,nrow=ncol(outDfU_21), ncol = 3)
  for(jj in 1:ncol(outDfU_21)){
    quantiles <- outDfU_21[,jj] %>% quantile(probs=c(.5,.025,.975))
    quantileMatrixU_21[jj,] <- quantiles
  }     
  # Create a dataframe for plotting
  NdfJU_2021 = data.frame(
    midpoint = ager,
    mean=quantileMatrixU_21[,1],
    upper = quantileMatrixU_21[,3],
    lower = quantileMatrixU_21[,2],
    year=rep(2021,15)
  )
  
}

allpred=rbind(NdfJ_2007,NdfJ_2009,NdfJ_2011,NdfJ_2013,NdfJ_2015,
              NdfJ_2017,NdfJ_2019,NdfJ_2021)

Vpred=rbind(NdfJV_2007,NdfJV_2009,NdfJV_2011,NdfJV_2013,NdfJV_2015,
            NdfJV_2017,NdfJV_2019,NdfJV_2021)

Upred=rbind(NdfJU_2007,NdfJU_2009,NdfJU_2011,NdfJU_2013,NdfJU_2015,
            NdfJU_2017,NdfJU_2019,NdfJU_2021)


#################rawdata####
raw_dat=read_excel("Data/Newmodelinputdat.xlsx",sheet="raw_dat")
#raw_dat2=read_excel("Data/Modelinput_hepB.xlsx",sheet="raw_dat2")

raw_dat[,c("mean","lower","upper")]=binom::binom.confint(raw_dat$Pos,raw_dat$Total,methods="wilson")[,c("mean","lower","upper")]
#raw_dat2[,c("mean","lower","upper")]=binom::binom.confint(raw_dat2$Pos,raw_dat2$Total,methods="wilson")[,c("mean","lower","upper")]

# raw_dat2$mean=ifelse(is.na(raw_dat2$mean),0,raw_dat2$mean)
# raw_dat2$lower=ifelse(is.na(raw_dat2$lower),0,raw_dat2$lower)
# raw_dat2$upper=ifelse(is.na(raw_dat2$upper),0,raw_dat2$upper)

raw_dat$mean=ifelse(is.na(raw_dat$mean),0,raw_dat$mean)
raw_dat$lower=ifelse(is.na(raw_dat$lower),0,raw_dat$lower)
raw_dat$upper=ifelse(is.na(raw_dat$upper),0,raw_dat$upper)

#################Output plots####
###combined plot
raw_dat2$year=factor(raw_dat2$year,levels=unique(raw_dat2$year))
allpred$year=factor(allpred$year,levels=unique(allpred$year))
plot3=ggplot(allpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=rawV_data)+
  geom_linerange(data=rawV_data)+
  xlab("Age (years)") + ylab("Proportion seropositive") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+facet_wrap(~year,ncol=2)+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))

###Vaccinated and unvaccinated plots
Vpred=Vpred %>% mutate(Finalvacc_status=rep("Vaccinated",120))
Upred=Upred %>% mutate(Finalvacc_status=rep("Unvaccinated",120))
Jointpred=rbind(Vpred,Upred)
raw_dat$year=factor(raw_dat$year,levels=unique(raw_dat$year))
raw_dat$Finalvacc_status=factor(raw_dat$Finalvacc_status,levels=unique(raw_dat$Finalvacc_status))
rawV_dat=raw_dat %>% filter(Finalvacc_status=="Vaccinated")
Jointpred$year=factor(Jointpred$year,levels=unique(Jointpred$year))
Jointpred$Finalvacc_status=factor(Jointpred$Finalvacc_status,levels=unique(Jointpred$Finalvacc_status))

tiff("Figures2/simplecatfit.tiff", units="in", width=15, height=9, res=500)
ggplot(Jointpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=raw_dat,size=1.5)+
  geom_linerange(data=raw_dat)+
  xlab("Age (years)") + ylab("Proportion seropositive") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+
  facet_grid(Finalvacc_status~year)+
  theme(axis.text.x = element_text(size=13,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=20,face="plain"),
        axis.title.x=element_text(size=20,face="plain"),
        axis.title.y=element_text(size=20,face="plain"),
        strip.text.x = element_text(size=20,face="plain"),
        strip.text.y = element_text(size=20,face="plain"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))

dev.off()


#########Vaccinated only####
rawV_dat=raw_dat %>% filter(Finalvacc_status=="Vaccinated")
#Vpred$year=factor(Vpred$year,levels=unique(Vpred$year))
Upred$year=factor(Upred$year,levels=unique(Upred$year))
tiff("Figures/simplecatVfit2.tiff", units="in", width=22, height=8, res=500)
ggplot(Upred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=rawV_dat,size=1.5)+
  geom_linerange(data=rawV_dat)+
  xlab("Age (years)") + ylab("Proportion seropositive") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+
  facet_wrap(~year,nrow=2)+
  theme(axis.text.x = element_text(size=12,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=25,face="plain"),
        axis.title.x=element_text(size=25,face="plain"),
        axis.title.y=element_text(size=25,face="plain"),
        strip.text.x = element_text(size=25,face="plain"),
        strip.text.y = element_text(size=25,face="plain"),
        legend.title=element_text(size=25),
        legend.text=element_text(size=25))

dev.off()


#########Unvaccinated only####
rawU_dat=raw_dat %>% filter(Finalvacc_status=="Unvaccinated")
Vpred$year=factor(Vpred$year,levels=unique(Vpred$year))
#Upred$year=factor(Upred$year,levels=unique(Upred$year))
tiff("Figures/simplecatUfit.tiff", units="in", width=22, height=8, res=500)
ggplot(Vpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=rawU_dat,size=1.5)+
  geom_linerange(data=rawU_dat)+
  xlab("Age (years)") + ylab("Proportion seropositive") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+
  facet_wrap(~year,nrow=2)+
  theme(axis.text.x = element_text(size=10,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=25,face="plain"),
        axis.title.x=element_text(size=25,face="plain"),
        axis.title.y=element_text(size=25,face="plain"),
        strip.text.x = element_text(size=25,face="plain"),
        strip.text.y = element_text(size=25,face="plain"),
        legend.title=element_text(size=25),
        legend.text=element_text(size=25))

dev.off()










#names(rawV_data)
rawV_data=raw_dat %>% filter(Finalvacc_status=="Vaccinated")
###plots
rawV_data$year=factor(rawV_data$year,levels=unique(rawV_data$year))
Vpred$year=factor(Vpred$year,levels=unique(Vpred$year))
plot3=ggplot(Vpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=rawV_data)+
  geom_linerange(data=rawV_data)+
  xlab("Age (years)") + ylab("Proportion seropositive") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+facet_wrap(~year,ncol=2)+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))

ggsave(plot=plot3,"figures/vaccinated.jpeg", unit="cm",width=27,height=20)

###plot 1&2 all combined(vaccinated+unvaccinated)
##predictions
Vpred=Vpred %>% mutate(status=rep("Vaccinated",120))
Upred=Upred %>% mutate(status=rep("Unvaccinated",120))
##rawdata
# yearVcat=yearV_U_cat %>% filter(Vacc_status=="vaccinated") %>%
#   mutate(Vacc_status=recode(Vacc_status,"vaccinated"="Vaccinated"))
# yearUcat=yearV_U_cat %>% filter(Vacc_status=="Unvaccinated")
# rawU_data=yearUcat %>% rename(year=year2,status=Vacc_status,midpoint=agemid,mean=mid,lower=lo,upper=hi)
# rawV_data=yearVcat %>% rename(year=year2,status=Vacc_status,midpoint=agemid,mean=mid,lower=lo,upper=hi)

#Jointraw=rbind(rawU_data,rawV_data)
Jointpred=rbind(Vpred,Upred)
Jointplot=ggplot(Jointpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=raw_dat)+
  geom_linerange(data=raw_dat)+
  xlab("Age (years)") + ylab("seropositivity")+
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+
  facet_grid(~year)+
  #facet_grid(year~status)+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        strip.text.y = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+ylab("Seropositivity")+xlab("Age in years")


ggsave(plot=Jointplot,"figures/Joint.jpeg", unit="cm",width=25,height=22)


Jointpred$year=factor(Jointpred$year,levels=unique(Jointpred$year))
plot2=Jointpred %>% ggplot( aes(x=midpoint, y=mean, ymin=lower, ymax=upper,col=year,group=year,fill=year)) +
  #geom_ribbon(alpha=0.2)+
  geom_line(aes(col=year))+
  geom_point() +
  theme_classic()+
  #geom_point(data=raw_data)+
  #geom_linerange(data=raw_data)+
  xlab("Age (years)") + ylab("seropositivity") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+facet_wrap(~status)+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+ylab("Seropositivity")+xlab("Age in years")

ggsave(plot=plot2,"figures/predictions.jpeg", unit="cm",width=27,height=20)


rawall_data=yearall_cat %>% rename(year=year2,midpoint=agemid,mean=mid,lower=lo,upper=hi)
allpred$year=factor(allpred$year,levels=unique(allpred$year))

plot1=ggplot(allpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=rawall_data)+
  geom_linerange(data=rawall_data)+
  xlab("Age (years)") + ylab("seropositivity") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+facet_wrap(~year,ncol=2)+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+ylab("Seropositivity")+xlab("Age in years")

ggsave(plot=plot1,"figures/combined.jpeg", unit="cm",width=27,height=20)



plot2=allpred %>% ggplot( aes(x=midpoint, y=mean, ymin=lower, ymax=upper,col=year,group=year,fill=year)) +
  #geom_ribbon(alpha=0.2)+
  geom_line(aes(col=year))+
  #geom_point(data=raw_data)+
  #geom_linerange(data=raw_data)+
  xlab("Age (years)") + ylab("seropositivity") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()

###plots
###plot 3$4 Vaccinated
#Input_dat
raw_dat=read_excel("Data/Currentrawchildren_PCVIS_Mala_Seeck_Ver2.xlsx",sheet="raw_dat")
raw_dat
raw_dat[,c("mean","lower","upper")]=binom::binom.confint(raw_dat$Pos,raw_dat$Total,methods="wilson")[,c("mean","lower","upper")]

raw_dat$mean=ifelse(is.na(raw_dat$mean),0,Input_dat$mean)
raw_dat$lower=ifelse(is.na(raw_dat$lower),0,Input_dat$lower)
raw_dat$upper=ifelse(is.na(raw_dat$upper),0,Input_dat$upper)
#names(rawV_data)
rawV_data=raw_dat %>% filter(Finalvacc_status=="Vaccinated")
Vpred$year=factor(Vpred$year,levels=unique(Vpred$year))
Vpred$midpoint=factor(Vpred$midpoint,levels=unique(Vpred$midpoint))
plot3=ggplot(Vpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper,col=year,group=year,fill=year)) +
  geom_ribbon(alpha=0.2)+
  #geom_line(col=year)+
  geom_point(data=rawV_data)+
  geom_linerange(data=rawV_data)+
  xlab("Age (years)") + ylab("seropositivity") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  #theme_bw()+facet_wrap(~year,ncol=2)+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+ylab("Seropositivity")+xlab("Age in years")+
  ggtitle("Fit for Vaccinated")


ggsave(plot=plot3,"figures/vaccinated.jpeg", unit="cm",width=27,height=20)


Vpred$year=factor(Vpred$year,levels=unique(Vpred$year))
plot4=Vpred %>% ggplot( aes(x=midpoint, y=mean, ymin=lower, ymax=upper,col=year,group=year,fill=year)) +
  #geom_ribbon(alpha=0.2)+
  geom_line(aes(col=year))+
  #geom_point(data=raw_data)+
  #geom_linerange(data=raw_data)+
  xlab("Age (years)") + ylab("seropositivity") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()

###plots
###plot 5$6 unvaccinated

rawU_data=yearUcat %>% rename(year=year2,midpoint=agemid,mean=mid,lower=lo,upper=hi)
plot5=ggplot(Upred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=rawU_data)+
  geom_linerange(data=rawU_data)+
  xlab("Age (years)") + ylab("Proportion seropositive") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+facet_wrap(~year,ncol=2)+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+ylab("Seropositivity")+xlab("Age in years")

ggsave(plot=plot5,"figures/unvaccinated.jpeg", unit="cm",width=27,height=20)

Upred$year=factor(Upred$year,levels=unique(Upred$year))
plot6=Upred %>% ggplot( aes(x=midpoint, y=mean, ymin=lower, ymax=upper,col=year,group=year,fill=year)) +
  #geom_ribbon(alpha=0.2)+
  geom_line(aes(col=year))+
  #geom_point(data=raw_data)+
  #geom_linerange(data=raw_data)+
  xlab("Age (years)") + ylab("seropositivity") +
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()


pred_data=read_excel("raw-data/FOI_model results.xlsx",sheet="jointVE")
pred_data
pred_data$Year=factor(pred_data$Year,levels=unique(pred_data$Year))
FOI=ggplot(pred_data,aes(x=Year, y=mid, ymin=lo, ymax=hi)) +
  #geom_ribbon(alpha=0.2)+
  geom_point()+
  geom_pointrange()+ylab("Estimated FOI")+theme_bw()+
  theme(axis.text.x = element_text(size=18,face="plain",angle=60,hjust=1),
        axis.text.y = element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+xlab("Year")

ggsave(plot=FOI,"figures/Estimated.jpeg", unit="cm",width=20,height=13)









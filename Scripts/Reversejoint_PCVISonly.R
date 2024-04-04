rm(list=ls())
require(tidyverse)
require(rjags)
require(binom)
require(readxl)
require(MCMCvis)

Input_dat=readxl::read_excel("Data/Newmodelinputdat.xlsx",sheet="PCVIS_inputdat")
Input_dat
##############Age independent FOI##
#define model
##before 2015
jcode <- "model{ 
for (i in 1:4) {
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = (lambda/(lambda + delta))*(1-exp(-(lambda+delta)*age[i])) #catalytic model
}

##2015-2017

for (i in 5:8){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i]< 2,
(lambda_15/(lambda_15 + delta))*(1-exp(-(lambda_15+delta)*age[i])),
( lambda/(lambda+delta) * (1-exp(-(age[i]-2)*(lambda+delta))) - lambda_15/(lambda_15+delta) ) *
exp(-1*(lambda_15+delta)*2) + lambda_15/(lambda_15+delta))

}	


##2017-2019


for (i in 9:12){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
(lambda_17/(lambda_17 + delta))*(1-exp(-(lambda_17+delta)*age[i])),ifelse(age[i]>=2 && age[i]<4,
( lambda_15/(lambda_15+delta) * (1-exp(-(age[i]-2)*(lambda_15+delta))) - lambda_17/(lambda_17+delta) ) *
exp(-1*(lambda_17+delta)*2) + lambda_17/(lambda_17+delta),
(( lambda/(lambda+delta) * (1-exp(-(age[i]-4)*(lambda+delta))) - lambda_15/(lambda_15+delta) ) *
exp(-1*(lambda_15+delta)*2) + lambda_15/(lambda_15+delta)-lambda_17/(lambda_17+delta) ) *
exp(-1*(lambda_17+delta)*2) + lambda_17/(lambda_17+delta)))

}


##2019-2021

for (i in 13:16){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] <2,
(lambda_19/(lambda_19 + delta))*(1-exp(-age[i]*(lambda_19+delta))),ifelse(age[i]>=2 && age[i]<4,
( lambda_17/(lambda_17+delta) * (1-exp(-(age[i]-2)*(lambda_17+delta))) - lambda_19/(lambda_19+delta) ) *
exp(-1*(lambda_19+delta)*2) + lambda_19/(lambda_19+delta),ifelse(age[i]>=4 && age[i]< 6,
(( lambda_15/(lambda_15+delta) * (1-exp(-(age[i]-4)*(lambda_15+delta))) - lambda_17/(lambda_17+delta) ) *
exp(-1*(lambda_17+delta)*2) + lambda_17/(lambda_17+delta)-lambda_19/(lambda_19+delta) ) *
exp(-1*(lambda_19+delta)*2) + lambda_19/(lambda_19+delta),
((( lambda/(lambda+delta) * (1-exp(-(age[i]-6)*(lambda+delta))) - lambda_15/(lambda_15+delta) ) *
exp(-1*(lambda_15+delta)*2) + lambda_15/(lambda_15+delta)-lambda_17/(lambda_17+delta) ) *
exp(-1*(lambda_17+delta)*2) + lambda_17/(lambda_17+delta)-lambda_19/(lambda_19+delta) ) *
exp(-1*(lambda_19+delta)*2) + lambda_19/(lambda_19+delta))))

}


##########unvaccinated
for (i in 17:20) {
    n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
    seropos_est[i] = ((lambda*omega)/((lambda*omega) + delta))*(1-exp(-((lambda*omega)+delta)*age[i])) #catalytic model
}

for (i in 21:24){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i]< 2,
((lambda_15*omega)/((lambda_15*omega) + delta))*(1-exp(-((lambda_15*omega)+delta)*age[i])),
( (lambda*omega)/((lambda*omega)+delta) * (1-exp(-(age[i]-2)*((lambda*omega)+delta))) - (lambda_15*omega)/((lambda_15*omega)+delta) ) *
exp(-1*((lambda_15*omega)+delta)*2) + (lambda_15*omega)/((lambda_15*omega)+delta))
}	
for (i in 25:28){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] < 2,
((lambda_17*omega)/((lambda_17*omega) + delta))*(1-exp(-((lambda_17*omega)+delta)*age[i])),ifelse(age[i]>=2 && age[i]<4,
( (lambda_15*omega)/((lambda_15*omega)+delta) * (1-exp(-(age[i]-2)*((lambda_15*omega)+delta))) - (lambda_17*omega)/((lambda_17*omega)+delta) ) *
exp(-1*((lambda_17*omega)+delta)*2) + (lambda_17*omega)/((lambda_17*omega)+delta),
(( (lambda*omega)/((lambda*omega)+delta) * (1-exp(-(age[i]-4)*((lambda*omega)+delta))) - (lambda_15*omega)/((lambda_15*omega)+delta) ) *
exp(-1*((lambda_15*omega)+delta)*2) + (lambda_15*omega)/((lambda_15*omega)+delta)-(lambda_17*omega)/((lambda_17*omega)+delta) ) *
exp(-1*((lambda_17*omega)+delta)*2) + (lambda_17*omega)/((lambda_17*omega)+delta)))
}

for (i in 29:32){
n.pos[i] ~ dbinom(seropos_est[i],N[i]) #fit to binomial data
seropos_est[i] <- ifelse(age[i] <2,
((lambda_19*omega)/((lambda_19*omega) + delta))*(1-exp(-age[i]*((lambda_19*omega)+delta))),ifelse(age[i]>=2 && age[i]<4,
( (lambda_17*omega)/((lambda_17*omega)+delta) * (1-exp(-(age[i]-2)*((lambda_17*omega)+delta))) - (lambda_19*omega)/((lambda_19*omega)+delta) ) *
exp(-1*((lambda_19*omega)+delta)*2) + (lambda_19*omega)/((lambda_19*omega)+delta),ifelse(age[i]>=4 && age[i]< 6,
(( (lambda_15*omega)/((lambda_15*omega)+delta) * (1-exp(-(age[i]-4)*((lambda_15*omega)+delta))) - (lambda_17*omega)/((lambda_17*omega)+delta) ) *
exp(-1*((lambda_17*omega)+delta)*2) + (lambda_17*omega)/((lambda_17*omega)+delta)-(lambda_19*omega)/((lambda_19*omega)+delta) ) *
exp(-1*((lambda_19*omega)+delta)*2) + (lambda_19*omega)/((lambda_19*omega)+delta),
((( lambda/((lambda*omega)+delta) * (1-exp(-(age[i]-6)*((lambda*omega)+delta))) - (lambda_15*omega)/((lambda_15*omega)+delta) ) *
exp(-1*((lambda_15*omega)+delta)*2) + (lambda_15*omega)/((lambda_15*omega)+delta)-(lambda_17*omega)/((lambda_17*omega)+delta) ) *
exp(-1*((lambda_17*omega)+delta)*2) + (lambda_17*omega)/((lambda_17*omega)+delta)-(lambda_19*omega)/((lambda_19*omega)+delta) ) *
exp(-1*((lambda_19*omega)+delta)*2) + (lambda_19*omega)/((lambda_19*omega)+delta))))
}


  lambda ~ dunif(0,1)  #uninformative prior
  lambda_15 ~ dunif(0,1) #uninformative prior
  lambda_17 ~ dunif(0,1) #uninformative prior
  lambda_19 ~ dunif(0,1) #uninformative prior
  delta~ dunif(0,5)      #uninformative prior
  omega~ dunif(0,1)      #uninformative prior
}"

# Run model
mcmc.length=10000
jdat = list(n.pos= Input_dat$Pos,
            N=Input_dat$Total,
            age=Input_dat$agemid)
jmod = jags.model(textConnection(jcode), data=jdat, n.chains=2, n.adapt=2000)
update(jmod)
jpos = coda.samples(jmod, c("lambda","lambda_15","lambda_17","lambda_19","delta","omega"), n.iter=mcmc.length)
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
lambda_15_est <- mcmcMatrix[, "lambda_15"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_17_est <- mcmcMatrix[, "lambda_17"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
lambda_19_est <- mcmcMatrix[, "lambda_19"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
delta_est <- mcmcMatrix[, "delta"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()
omega_est <- mcmcMatrix[, "omega"] %>% quantile(probs = c(.5,.025,.975))%>% round(3) %>% print()


## Create data for plots
## Samples from mcmc chains for credible intervals
##option 2

paramVector <- c("lambda",
                 "lambda_15",
                 "lambda_17",
                 "lambda_19",
                 "delta",
                 "omega")

paramEstimates = list(lambda_est,
                      lambda_15_est,
                      lambda_17_est,
                      lambda_19_est,
                      delta_est,
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
#write.csv(correct_joint,file="Data/sensitivityreverse_joint.csv")

ager=0:14
ager1=0:1
ager2=2:14
ager3=2:3
ager4=4:14
ager5=4:5
ager6=6:14

numSamples = 1000
foiVector = paramVector

for(ii in 1:length(foiVector)){
  outDf_15 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_17 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_19 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDf_21 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_15 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_17 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_19 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfV_21 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_15 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_17 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_19 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  outDfU_21 <- matrix(NA,nrow=numSamples, ncol = length(ager))
  
  foiyoung <- foiVector[1]
  foimid <- foiVector[2]
  foimid2 <- foiVector[3]
  foimid3 <- foiVector[4]
  
  
  for (kk in 1:numSamples ) {
    
    randomNumber <- floor(runif(1, min = 1, max = nrow(mcmcMatrix)))
    
    lambdaSample <- mcmcMatrix[randomNumber,foiyoung]
    lambdaSample15 <- mcmcMatrix[randomNumber,foimid]
    lambdaSample17 <- mcmcMatrix[randomNumber,foimid2]
    lambdaSample19 <- mcmcMatrix[randomNumber,foimid3]
    deltaSample<-mcmcMatrix[randomNumber,"delta"]
    omegaSample<-mcmcMatrix[randomNumber,"omega"]
    
    ###Vaccinated
    ##before2015
    allV_15 <- ((lambdaSample*omegaSample)/((lambdaSample*omegaSample) + deltaSample))*(1-exp(-((lambdaSample*omegaSample)+deltaSample)*ager))
    allU_15 <- (lambdaSample/(lambdaSample + deltaSample))*(1-exp(-(lambdaSample+deltaSample)*ager))
    
    all_15=allV_15+allU_15
    allV_15=allV_15
    allU_15=allU_15
    
    ##2015-2017
    youngV_17 <- ((lambdaSample15*omegaSample)/((lambdaSample15*omegaSample) + deltaSample))*(1-exp(-((lambdaSample15*omegaSample)+deltaSample)*ager1))
    youngU_17 <- (lambdaSample15/(lambdaSample15 + deltaSample))*(1-exp(-(lambdaSample15+deltaSample)*ager1))
    
    
    oldV_17 <- ((lambdaSample*omegaSample)/((lambdaSample*omegaSample) + deltaSample)*(1-exp(-((lambdaSample*omegaSample)+deltaSample)*(ager2-2)))-
                  ((lambdaSample15*omegaSample)/((lambdaSample15*omegaSample) + deltaSample)))*
      exp(-1*((lambdaSample15*omegaSample)+deltaSample)*2)+((lambdaSample15*omegaSample)/((lambdaSample15*omegaSample) + deltaSample))
    
    oldU_17 <- (lambdaSample/(lambdaSample + deltaSample)*(1-exp(-(lambdaSample+deltaSample)*(ager2-2)))-
                  (lambdaSample15/(lambdaSample15 + deltaSample)))*
      exp(-1*(lambdaSample15+deltaSample)*2)+(lambdaSample15/(lambdaSample15 + deltaSample))
    
    
    all_17 <- c((youngV_17+youngU_17),(oldV_17+oldU_17))
    
    allV_17=c(youngV_17,oldV_17)
    allU_17=c(youngU_17,oldU_17)
    
    ##2017-2019
    youngV_19 <- ((lambdaSample17*omegaSample)/((lambdaSample17*omegaSample) +  deltaSample))*(1-exp(-((lambdaSample17*omegaSample)+ deltaSample)*ager1))
    youngU_19 <- (lambdaSample17/(lambdaSample17 +  deltaSample))*(1-exp(-(lambdaSample17+ deltaSample)*ager1))
    
    
    midV_19 <- (((lambdaSample15*omegaSample)/((lambdaSample15*omegaSample) +  deltaSample))*(1-exp(-((lambdaSample15*omegaSample)+ deltaSample)*(ager3-2)))-
                  (lambdaSample17*omegaSample)/((lambdaSample17*omegaSample) +  deltaSample))*exp(-1*((lambdaSample17*omegaSample)+ deltaSample)*2)+((lambdaSample17*omegaSample)/((lambdaSample17*omegaSample) +  deltaSample))
    
    midU_19 <- ((lambdaSample15/(lambdaSample15 +  deltaSample))*(1-exp(-(lambdaSample15+ deltaSample)*(ager3-2)))-
                  lambdaSample17/(lambdaSample17 +  deltaSample))*exp(-1*(lambdaSample17+ deltaSample)*2)+(lambdaSample17/(lambdaSample17 +  deltaSample))
    
    
    oldV_19 <- ((((lambdaSample*omegaSample)/((lambdaSample*omegaSample) +  deltaSample))*(1-exp(-((lambdaSample*omegaSample)+ deltaSample)*(ager4-4)))-
                   (lambdaSample15*omegaSample)/((lambdaSample15*omegaSample) +  deltaSample))*exp(-1*((lambdaSample15*omegaSample)+ deltaSample)*2)+((lambdaSample15*omegaSample) / ((lambdaSample15*omegaSample)+ deltaSample))-
                  ((lambdaSample17*omegaSample)/((lambdaSample17*omegaSample) +  deltaSample)))*exp(-1*((lambdaSample17*omegaSample)+ deltaSample)*2)+((lambdaSample17*omegaSample)/((lambdaSample17*omegaSample) + deltaSample))
    
    oldU_19 <- (((lambdaSample/(lambdaSample +  deltaSample))*(1-exp(-(lambdaSample+ deltaSample)*(ager4-4)))-
                   lambdaSample15/(lambdaSample15 +  deltaSample))*exp(-1*(lambdaSample15+ deltaSample)*2)+(lambdaSample15 / (lambdaSample15+ deltaSample))-
                  (lambdaSample17/(lambdaSample17 +  deltaSample)))*exp(-1*(lambdaSample17+ deltaSample)*2)+(lambdaSample17/(lambdaSample17 + deltaSample))
    
    all_19 <- c((youngV_19+youngU_19),(midV_19+midU_19),(oldV_19+oldU_19))
    
    allV_19<-c(youngV_19,midV_19,oldV_19)
    allU_19<-c(youngU_19,midU_19,oldU_19)          
    
    
    ##2019-2021
    youngV_21 <- ((lambdaSample19*omegaSample)/((lambdaSample19*omegaSample) + deltaSample))*(1-exp(-ager1*((lambdaSample19*omegaSample)+deltaSample)))
    youngU_21 <- (lambdaSample19/(lambdaSample19 + deltaSample))*(1-exp(-ager1*(lambdaSample19+deltaSample)))
    
    
    midV_21 <- ((lambdaSample17*omegaSample)/((lambdaSample17*omegaSample) + deltaSample)*(1-exp(-((lambdaSample17*omegaSample)+deltaSample)*(ager3-2)))-
                  ((lambdaSample19*omegaSample)/((lambdaSample19*omegaSample) + deltaSample)))*exp(-1*((lambdaSample19*omegaSample)+deltaSample)*2)+
      ((lambdaSample19*omegaSample)/((lambdaSample19*omegaSample) + deltaSample))
    
    midU_21 <- (lambdaSample17/(lambdaSample17 + deltaSample)*(1-exp(-(lambdaSample17+deltaSample)*(ager3-2)))-
                  (lambdaSample19/(lambdaSample19 + deltaSample)))*exp(-1*(lambdaSample19+deltaSample)*2)+
      (lambdaSample19/(lambdaSample19 + deltaSample))
    
    
    mid2V_21 <- ((((lambdaSample15*omegaSample) / ((lambdaSample15*omegaSample)+deltaSample))* (1-exp(-((lambdaSample15*omegaSample) +deltaSample)*(ager5-4)))- 
                    (lambdaSample17*omegaSample) / ((lambdaSample17*omegaSample)+deltaSample)) *exp(-1*((lambdaSample17*omegaSample) + deltaSample)*2) +
                   ((lambdaSample17*omegaSample) / ((lambdaSample17*omegaSample)+deltaSample))-(lambdaSample19*omegaSample) / ((lambdaSample19*omegaSample)+deltaSample))*
      exp(-1*((lambdaSample19*omegaSample) + deltaSample)*2) + ((lambdaSample19*omegaSample) / ((lambdaSample19*omegaSample)+deltaSample))
    
    mid2U_21 <- (((lambdaSample15 / (lambdaSample15+deltaSample))* (1-exp(-(lambdaSample15 +deltaSample)*(ager5-4)))- 
                    lambdaSample17 / (lambdaSample17+deltaSample)) *exp(-1*(lambdaSample17 + deltaSample)*2) +
                   (lambdaSample17 / (lambdaSample17+deltaSample))-lambdaSample19 / (lambdaSample19+deltaSample))*
      exp(-1*(lambdaSample19 + deltaSample)*2) + (lambdaSample19 / (lambdaSample19+deltaSample))
    
    
    oldV_21 <-   (((((lambdaSample*omegaSample) / ((lambdaSample*omegaSample)+deltaSample))* (1-exp(-((lambdaSample*omegaSample) +deltaSample)*(ager6-6)))- 
                      (lambdaSample15*omegaSample) / ((lambdaSample15*omegaSample)+deltaSample)) *exp(-1*((lambdaSample15*omegaSample) + deltaSample)*2) +
                     ((lambdaSample15*omegaSample) / ((lambdaSample15*omegaSample)+deltaSample))-(lambdaSample17*omegaSample) / ((lambdaSample17*omegaSample)+deltaSample))*
                    exp(-1*((lambdaSample17*omegaSample) + deltaSample)*2) + ((lambdaSample17*omegaSample) / ((lambdaSample17*omegaSample)+deltaSample))-
                    (lambdaSample19*omegaSample) / ((lambdaSample19*omegaSample)+deltaSample)) *exp(-1*((lambdaSample19*omegaSample) + deltaSample)*2)+
      ((lambdaSample19*omegaSample) / ((lambdaSample19*omegaSample)+deltaSample))
    
    oldU_21 <-   ((((lambdaSample / (lambdaSample+deltaSample))* (1-exp(-(lambdaSample +deltaSample)*(ager6-6)))- 
                      lambdaSample15 / (lambdaSample15+deltaSample)) *exp(-1*(lambdaSample15 + deltaSample)*2) +
                     (lambdaSample15 / (lambdaSample15+deltaSample))-lambdaSample17 / (lambdaSample17+deltaSample))*
                    exp(-1*(lambdaSample17 + deltaSample)*2) + (lambdaSample17 / (lambdaSample17+deltaSample))-
                    lambdaSample19 / (lambdaSample19+deltaSample)) *exp(-1*(lambdaSample19 + deltaSample)*2)+
      (lambdaSample19 / (lambdaSample19+deltaSample))
    
    all_21 <- c(youngV_21+youngU_21,midV_21+midU_21,mid2V_21+mid2U_21,oldV_21+oldU_21)
    
    allV_21<-c(youngV_21,midV_21,mid2V_21,oldV_21)
    allU_21<-c(youngU_21,midU_21,mid2U_21,oldU_21)
    
   
    outDf_15[kk,] <-all_15
    outDf_17[kk,]<-all_17
    outDf_19[kk,]<-all_19
    outDf_21[kk,]<-all_21
    

    outDfV_15[kk,]<-allV_15
    outDfV_17[kk,]<-allV_17
    outDfV_19[kk,]<-allV_19
    outDfV_21[kk,] <- allV_21
   

    outDfU_15[kk,]<-allU_15
    outDfU_17[kk,]<-allU_17
    outDfU_19[kk,]<-allU_19
    outDfU_21[kk,] <- allU_21
    
  }
  
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
allpred=rbind(NdfJ_2015,
              NdfJ_2017,NdfJ_2019,NdfJ_2021)

Vpred=rbind(NdfJV_2015,
            NdfJV_2017,NdfJV_2019,NdfJV_2021)

Upred=rbind(NdfJU_2015,
            NdfJU_2017,NdfJU_2019,NdfJU_2021)

# data14=correctpred %>% filter(midpoint==14)
# data14
#write.csv(correctpred,file="correctpred.csv")
###plots
#################rawdata####
raw_dat=read_excel("Data/Newmodelinputdat.xlsx",sheet="PCVIS_rawdat")
raw_dat[,c("mean","lower","upper")]=binom::binom.confint(raw_dat$Pos,raw_dat$Total,methods="wilson")[,c("mean","lower","upper")]

raw_dat$mean=ifelse(is.na(raw_dat$mean),0,raw_dat$mean)
raw_dat$lower=ifelse(is.na(raw_dat$lower),0,raw_dat$lower)
raw_dat$upper=ifelse(is.na(raw_dat$upper),0,raw_dat$upper)

Vpred=Vpred %>% mutate(Finalvacc_status=rep("Vaccinated",60))
Upred=Upred %>% mutate(Finalvacc_status=rep("Unvaccinated",60))
Jointpred=rbind(Vpred,Upred)
raw_dat$year=factor(raw_dat$year,levels=unique(raw_dat$year))
raw_dat$Finalvacc_status=factor(raw_dat$Finalvacc_status,levels=unique(raw_dat$Finalvacc_status))
rawV_dat=raw_dat %>% filter(Finalvacc_status=="Vaccinated")
Jointpred$year=factor(Jointpred$year,levels=unique(Jointpred$year))
Jointpred$Finalvacc_status=factor(Jointpred$Finalvacc_status,levels=unique(Jointpred$Finalvacc_status))

tiff("Figures2/PCVISreversecatfit.tiff", units="in", width=15, height=9, res=500)
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









rawV_data=yearVcat %>% rename(year=year2,midpoint=agemid,mean=mid,lower=lo,upper=hi)
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
Vpred=Vpred %>% mutate(status=rep("Vaccinated",105))
Upred=Upred %>% mutate(status=rep("Unvaccinated",105))
##rawdata
yearVcat=yearV_U_cat %>% filter(Vacc_status=="vaccinated") %>%
  mutate(Vacc_status=recode(Vacc_status,"vaccinated"="Vaccinated"))
yearUcat=yearV_U_cat %>% filter(Vacc_status=="Unvaccinated")
rawU_data=yearUcat %>% rename(year=year2,status=Vacc_status,midpoint=agemid,mean=mid,lower=lo,upper=hi)
rawV_data=yearVcat %>% rename(year=year2,status=Vacc_status,midpoint=agemid,mean=mid,lower=lo,upper=hi)

Jointraw=rbind(rawU_data,rawV_data)
Jointpred=rbind(Vpred,Upred)

Jointplot=ggplot(Jointpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper)) +
  geom_ribbon(alpha=0.2)+
  geom_line()+
  geom_point(data=Jointraw)+
  geom_linerange(data=Jointraw)+
  xlab("Age (years)") + ylab("seropositivity")+
  scale_x_continuous(breaks=seq(0,14,by=1))+
  theme_bw()+facet_grid(year~status)+
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

rawV_data=yearVcat %>% rename(year=year2,midpoint=agemid,mean=mid,lower=lo,upper=hi)
Vpred$year=factor(Vpred$year,levels=unique(Vpred$year))
plot3=ggplot(Vpred, aes(x=midpoint, y=mean, ymin=lower, ymax=upper,col=year,group=year,fill=year)) +
  geom_ribbon(alpha=0.2)+
  geom_line(col=year)+
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









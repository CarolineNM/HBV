#rm(list=ls())
library(tidyverse) # for data manipulation and plots
library(haven) #for reading sav data
library(sjstats) #for calculating intra-class correlation (ICC)
library(ROCR) #for calculating area under the curve (AUC) statistics
library(brms) #for Bayesian (multilevel) generalised linear modelling
library(modelr) #for data manipulation
library(tidybayes) #for analysis of posterior draws of a Bayesian model

Input_dat=readxl::read_excel("Data/Newmodelinputdat.xlsx",sheet="BRMSmod2")
Input_dat$`Vaccination status`
names(Input_dat)

dat=Input_dat %>% select(Sex,Age,Agegroup,Active,Status,Algorithm) %>% 
  filter(Status=="Vaccinated"|Status=="Unvaccinated") %>% 
  filter(Algorithm!="Immune(Resolved infection)") %>% 
  filter(Algorithm!="Isolated anti-HBc")

#str(dat)
dat$Sex=factor(dat$Sex,levels=c("F","M"))
dat$Status=factor(dat$Status,levels=c("Unvaccinated","Vaccinated"))
dat$Agegroup=factor(dat$Agegroup,levels=c("<1yrs","1-4yrs","5-9yrs","10-14yrs"))

str(dat)

########fit a frequentist model###
myModel_Binary <- glm(formula = Active ~ Status +Agegroup,
                    family = binomial(link = "logit"),
                    data = dat)
summary(myModel_Binary)

########fit a bayesian model
myBayes_Model_Binary <- brm(formula = Active~Status+Sex,  
                          data=dat, 
                          family = bernoulli(link = "logit"),
                          warmup = 500, 
                          iter =4000, 
                          chains = 2, 
                          init= "0", 
                          cores=2,
                          seed = 123)


prior_summary(myBayes_Model_Binary)

print(exp(fixef(myBayes_Model_Binary)[,-2]),digits=4)

######check convergence
mcmc_plot(myBayes_Model_Binary, 
          type = "trace")
#######check autocorrelation between the predictor variables
mcmc_plot(myBayes_Model_Binary, 
          type = "acf_bar")

##########model interpretation
summary(myBayes_Model_Binary)

#############visualize the credible intervals
mcmc_plot(myBayes_Model_Binary, 
          type = "areas",
          prob = 0.95)

######exponentiate the estimates for better interpretaion
print(exp(fixef(Bayes_Model_Binary)[,-2]),digits=4)

#########model evaluation
Prob <- predict(myBayes_Model_Binary, type="response")
Prob <- Prob[,1]
Pred <- prediction(Prob, as.vector(pull(dat, Active)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC

Pred <- predict(myBayes_Model_Binary, type = "response")
Pred <- if_else(Pred[,1] > 0.5, 1, 0)
ConfusionMatrix <- table(Pred, pull(dat, Active)) #`pull` results in a vector
#correct classification rate
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)






#####plot the exponeniated densities of the parameter estimates
tiff("Figures2/Autocorrplot.tiff", units="in", width=8, height=4, res=500)
mcmc_plot(myBayes_Model_Binary, 
          type = "acf_bar") +
  geom_vline(xintercept = 1, color = "grey")+
  theme(axis.text.x = element_text(size=13,face="plain"),
        axis.text.y = element_text(size=13,face="plain"),
        axis.title.x=element_text(size=13,face="plain"),
        axis.title.y=element_text(size=13,face="plain"),
        strip.text.x = element_text(size=13,face="plain"),
        strip.text.y = element_text(size=13,face="plain"),
        legend.title=element_text(size=13),
        legend.text=element_text(size=13))

dev.off()

1-0.21##being vaccinated lowers the odds of having an active HBV infection by 79%






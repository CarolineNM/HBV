rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(ggpubr)

HBsAg_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="Newdata_format")
HBsAg_dat

HBsAg_dat=HBsAg_dat %>% filter(Year>2013)

########Generate age categories
HBsAg_dat$age_years=(HBsAg_dat$doc-HBsAg_dat$dob)/365
##generate agecat
HBsAg_dat=HBsAg_dat %>% 
  mutate(agecat=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

HBsAg_dat$agecat=factor(HBsAg_dat$agecat,levels=c("<1yr","1-4yrs",
                                                    "5-9yrs","10-14yrs"))

names(HBsAg_dat)
plot2=ggplot(HBsAg_dat, aes(x=HBsAg_CAT, y=log10(ODE))) +
  #geom_boxplot(outlier.shape = NA,fill="Grey")+
  geom_boxplot(fill="Grey")+
  #facet_wrap(~HBsAg_CAT)+
  theme_bw()+
  # stat_summary(fun = "mean", geom = "point", shape = 8,
  #              size = 1, color = "red")+
  xlab("HBsAg") + ylab("Log10(OD)") +
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),
        axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),
        legend.text=element_text(size=16),
        legend.title =element_text(size=16))
 
##########HBcAb
HBcAb_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBcAb")
HBcAb_dat

plot3=ggplot(HBcAb_dat, aes(x=HBcAb_CAT, y=log10(OD))) +
  #geom_boxplot(outlier.shape = NA,fill="Grey")+
  geom_boxplot(fill="Grey")+
  #facet_wrap(~HBsAg_CAT)+
  theme_bw()+
  # stat_summary(fun = "mean", geom = "point", shape = 8,
  #              size = 1, color = "red")+
  xlab("HBcAb") + ylab("Log10(OD)") +
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),
        axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),
        legend.text=element_text(size=16),
        legend.title =element_text(size=16))+
  geom_hline(yintercept=log10(1.1),col="red",linetype="dashed")+
  geom_hline(yintercept=log10(0.9),col="green",linetype="dashed")

##########HBsAb
HBsAb_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBsAb")
HBsAb_dat

plot4=ggplot(HBsAb_dat, aes(x=HBsAb_CAT, y=log10(HBsAbIG))) +
  #geom_boxplot(outlier.shape = NA,fill="Grey")+
  geom_boxplot(fill="Grey")+
  #geom_point()+
  #facet_wrap(~HBsAg_CAT)+
  theme_bw()+
  # stat_summary(fun = "mean", geom = "point", shape = 8,
  #              size = 1, color = "red")+
  xlab("HBsAb") + ylab("Log10(IgG)") +
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),
        axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),
        legend.text=element_text(size=16),
        legend.title =element_text(size=16))+
geom_hline(yintercept=log10(10),col="red",linetype="dashed")

##########merge the plots together###########
figure <- ggarrange(plot2,plot3,plot4,
                    #labels = c("A", "B","C","D","E","F"),
                    ncol = 3, nrow=1,
                    common.legend = TRUE,
                    legend="bottom")
figure


###########HBsAg and HBcAb
HBcsAb_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBsAgcAb")
HBcsAb_dat
names(HBcsAb_dat)
plot3=ggplot(HBcsAb_dat, aes(x=log10(Ods), y=log10(Odc),group=Test,col=Test)) +
  #geom_boxplot(outlier.shape = NA,fill="Grey")+
  #geom_boxplot(fill="Grey")+
  geom_point()+
  #facet_wrap(~HBsAg_CAT)+
  theme_bw()+
  # stat_summary(fun = "mean", geom = "point", shape = 8,
  #              size = 1, color = "red")+
  xlab("log10(HBsAg OD)") + ylab("log10(HBcAb OD)") +
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),
        axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),
        legend.text=element_text(size=16),
        legend.title =element_text(size=16))+
  geom_hline(yintercept=log10(1.1),col="red",linetype="dashed")+
  geom_hline(yintercept=log10(0.9),col="green",linetype="dashed")
###########Is it an age thing?

########Generate age categories
HBcsAb_dat$age_years=(HBcsAb_dat$doc-HBcsAb_dat$dob)/365
##generate agecat
HBcsAb_dat=HBcsAb_dat %>% 
  mutate(Age_category=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

HBcsAb_dat$Age_category=factor(HBcsAb_dat$Age_category,levels=c("<1yr","1-4yrs",
                                                  "5-9yrs","10-14yrs"))

names(HBcsAb_dat)
HBcsAb_dat2=HBcsAb_dat %>% filter(Test=="Inconclusive")

ggplot(HBcsAb_dat2, aes(x=log10(Ods), y=log10(Odc),group=Age_category,col=Age_category)) +
  #geom_boxplot(outlier.shape = NA,fill="Grey")+
  #geom_boxplot(fill="Grey")+
  geom_point()+
  #facet_wrap(~HBsAg_CAT)+
  theme_bw()+
  # stat_summary(fun = "mean", geom = "point", shape = 8,
  #              size = 1, color = "red")+
  xlab("log10(HBsAg OD)") + ylab("log10(HBcAb OD)") +
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),
        axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),
        axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),
        legend.text=element_text(size=16),
        legend.title =element_text(size=16))

################Seroprevalence figures##############
HBsAg_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBsAg")
HBsAg_dat
########Generate age categories
HBsAg_dat$age_years=(HBsAg_dat$doc-HBsAg_dat$dob)/365
##generate agecat
HBsAg_dat=HBsAg_dat %>% 
  mutate(agecat=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

HBsAg_dat$agecat=factor(HBsAg_dat$agecat,levels=c("<1yr","1-4yrs",
                                                  "5-9yrs","10-14yrs"))

names(HBsAg_dat)
########generate sero estimates###
########Calculate raw HBsAg prevalence in children
HepB_childKili=HBsAg_dat %>%
  select(Year,agecat,HBsAg_CAT) %>%
  group_by(Year,agecat,HBsAg_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBsAg_CAT,values_from = n) %>%
  mutate(across(3:4,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKili[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKili$Pos,HepB_childKili$Total,methods="wilson")[,c("mean","lower","upper")]

plota=HepB_childKili%>% 
  ggplot(aes(x=agecat,y=mid,ymin=lo,ymax=hi))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBsAg(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo, ymax = hi, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),legend.text=element_text(size=16))


#########HBcAb
HBcAb_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBcAb")
HBcAb_dat
########Generate age categories
HBcAb_dat$age_years=(HBcAb_dat$doc-HBcAb_dat$dob)/365
##generate agecat
HBcAb_dat=HBcAb_dat %>% 
  mutate(agecat=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

HBcAb_dat$agecat=factor(HBcAb_dat$agecat,levels=c("<1yr","1-4yrs",
                                                  "5-9yrs","10-14yrs"))

names(HBcAb_dat)
########generate sero estimates###
########Calculate raw HBsAg prevalence in children
HepB_childKilib=HBcAb_dat %>%
  select(Year,agecat,HBcAb_CAT) %>%
  group_by(Year,agecat,HBcAb_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBcAb_CAT,values_from = n) %>%
  mutate(across(3:4,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKilib[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKilib$Pos,HepB_childKilib$Total,methods="wilson")[,c("mean","lower","upper")]

plotb=HepB_childKilib%>% 
  ggplot(aes(x=agecat,y=mid,ymin=lo,ymax=hi))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBcAb(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo, ymax = hi, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),legend.text=element_text(size=16))


#########HBsAb
HBsAb_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBsAb")
HBsAb_dat
########Generate age categories
HBsAb_dat$age_years=(HBsAb_dat$doc-HBsAb_dat$dob)/365
##generate agecat
HBsAb_dat=HBsAb_dat %>% 
  mutate(agecat=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

HBsAb_dat$agecat=factor(HBsAb_dat$agecat,levels=c("<1yr","1-4yrs",
                                                  "5-9yrs","10-14yrs"))

names(HBsAb_dat)
########generate sero estimates###
########Calculate raw HBsAg prevalence in children
HepB_childKilic=HBsAb_dat %>%
  select(Year,agecat,Test2,HBsAb_CAT) %>%
  group_by(Year,agecat,Test2,HBsAb_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBsAb_CAT,values_from = n) %>%
  mutate(across(4:5,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat,Test2) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKilic[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKilic$Pos,HepB_childKilic$Total,methods="wilson")[,c("mean","lower","upper")]
HepB_childKilic=HepB_childKilic %>% rename(Immunological_status=Test2)

plotc=HepB_childKilic%>% 
  ggplot(aes(x=agecat,y=mid*100,ymin=lo*100,ymax=hi*100))+
  facet_grid(Immunological_status~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBsAb(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=16,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),
        strip.text.y = element_text(size=16,face="plain"),
        legend.text=element_text(size=16))


##########merge the plots together###########
figure <- ggarrange(plota,plotb,plotc,
                    #labels = c("A", "B","C","D","E","F"),
                    ncol = 1, nrow=3,
                    common.legend = TRUE,
                    legend="bottom")
figure


ggsave(plot=plotc,"Figures/HBsAb_sero.jpeg", unit="cm",width=23,height=20)

##########Seroprevalence of all markers
matched_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_matched")
matched_dat
########Generate age categories
matched_dat$age_years=(matched_dat$doc-matched_dat$dob)/365
##generate agecat
matched_dat=matched_dat %>% 
  mutate(agecat=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

matched_dat$agecat=factor(matched_dat$agecat,levels=c("<1yr","1-4yrs",
                                                  "5-9yrs","10-14yrs"))
names(matched_dat)

HepB_sero=matched_dat %>%
  select(Year,agecat,Test2) %>%
  group_by(Year,agecat,Test2) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = Test2,values_from = n) %>%
  mutate(across(3:8,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick("Susceptible","Immune(Vaccination)","Immune(Resolved Infection)","Inconclusive","Acute infection","Chronic Infection"))) %>%
  ungroup()


HepB_sero[,c("mids","los","his")]=binom::binom.confint(HepB_sero$Susceptible,HepB_sero$Total,methods="wilson")[,c("mean","lower","upper")]
HepB_sero[,c("midv","lov","hiv")]=binom::binom.confint(HepB_sero$`Immune(Vaccination)`,HepB_sero$Total,methods="wilson")[,c("mean","lower","upper")]
HepB_sero[,c("midr","lor","hir")]=binom::binom.confint(HepB_sero$`Immune(Resolved Infection)`,HepB_sero$Total,methods="wilson")[,c("mean","lower","upper")]
HepB_sero[,c("midI","loI","hiI")]=binom::binom.confint(HepB_sero$Inconclusive,HepB_sero$Total,methods="wilson")[,c("mean","lower","upper")]
HepB_sero[,c("midAIn","loAIn","hiAIn")]=binom::binom.confint(HepB_sero$`Acute infection`,HepB_sero$Total,methods="wilson")[,c("mean","lower","upper")]
HepB_sero[,c("midCIn","loCIn","hiCIn")]=binom::binom.confint(HepB_sero$`Chronic Infection`,HepB_sero$Total,methods="wilson")[,c("mean","lower","upper")]

HepB_sero2=HepB_sero %>% select(Year,agecat,mids,midv,midr,midI,midAIn,midCIn)
#HepB_sero2=HepB_sero[,c(1,2,3,9,12,15,18)]
#HepB_sero2=HepB_sero2 %>% gather(values,variables,3:6)
HepB_sero2=HepB_sero2 %>% gather(values,variables,3:8)
HepB_sero2=HepB_sero2 %>% mutate(values=recode(values,"midv"="Immune(Vaccination)",
                                               "midr"="Immune(Resolved Infection)",
                                               "mids"="Susceptible",
                                               "midAIn"="Acute Infection",
                                               "midCIn"="Chronic Infection",
                                               "midI"="Inconclusive"))
#"midIn"="Infected(Acute/chronic)"))

HepB_sero2$values=factor(HepB_sero2$values,levels=c("Immune(Vaccination)",
                                                    "Immune(Resolved Infection)",
                                                    "Acute Infection",
                                                    "Chronic Infection",
                                                    "Susceptible",
                                                    "Inconclusive"))

HepB_sero2$agecat=factor(HepB_sero2$agecat,levels=c("<1yr","1-4yrs",
                                                    "5-9yrs","10-14yrs"))
###################plot the above
plot1=HepB_sero2%>% 
  ggplot(aes(x=agecat,y=variables,fill=values,colour=values))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_fill(),width = 0.75, alpha=0.6)+
  #geom_bar(stat = "identity")+
  theme_bw()+ylab("Seroprevalence(%)")+xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  #geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
  #col = "red")+
  theme(axis.text.x=element_text(size=10,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=18,face="plain"),
        axis.title.x=element_text(size=18,face="plain"),axis.title.y=element_text(size=18,face="plain"),
        strip.text.x = element_text(size=18,face="plain"),
        strip.text.y = element_text(size=18,face="plain"),
        legend.title=element_blank(),
        legend.text=element_text(size=14))+
  scale_y_continuous(labels = scales::percent)

plot1

ggsave(plot=plot1,"Figures/paneltest-all.jpeg", unit="cm",width=25,height=16)


###########HBsAg by age,sex and vaccination status###############
################Seroprevalence figures##############
HBsAg_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBsAg")
HBsAg_dat
########Generate age categories
HBsAg_dat$age_years=(HBsAg_dat$doc-HBsAg_dat$dob)/365
##generate agecat
HBsAg_dat=HBsAg_dat %>% 
  mutate(agecat=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

HBsAg_dat$agecat=factor(HBsAg_dat$agecat,levels=c("<1yr","1-4yrs",
                                                  "5-9yrs","10-14yrs"))

names(HBsAg_dat)
########generate sero estimates###
########Calculate raw HBsAg prevalence in children
HepB_childKili=HBsAg_dat %>%
  select(Year,agecat,HBsAg_CAT) %>%
  group_by(Year,agecat,HBsAg_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBsAg_CAT,values_from = n) %>%
  mutate(across(3:4,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKili[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKili$Pos,HepB_childKili$Total,methods="wilson")[,c("mean","lower","upper")]

plota=HepB_childKili%>% 
  ggplot(aes(x=agecat,y=mid*100,ymin=lo*100,ymax=hi*100))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBsAg(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=14,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),legend.text=element_text(size=16))


#ggsave(plot=plota,"Figures/HBsAgall.jpeg", unit="cm",width=35,height=20)

HepB_childKilib=HBsAg_dat %>%
  select(Year,agecat,sex,HBsAg_CAT) %>%
  group_by(Year,agecat,sex,HBsAg_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBsAg_CAT,values_from = n) %>%
  mutate(across(4:5,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKilib[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKilib$Pos,HepB_childKilib$Total,methods="wilson")[,c("mean","lower","upper")]

plotb=HepB_childKilib%>% 
  ggplot(aes(x=agecat,y=mid*100,ymin=lo*100,ymax=hi*100))+
  facet_grid(sex~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBsAg(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=14,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),
        strip.text.y = element_text(size=16,face="plain"),
        legend.text=element_text(size=16))

HepB_childKilic=HBsAg_dat %>%
  select(Year,agecat,plotvaccine,HBsAg_CAT) %>%
  group_by(Year,agecat,plotvaccine,HBsAg_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBsAg_CAT,values_from = n) %>%
  mutate(across(4:5,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKilic[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKilic$Pos,HepB_childKilic$Total,methods="wilson")[,c("mean","lower","upper")]

HepB_childKilic=HepB_childKilic %>% rename(Vaccination_status=plotvaccine)

plotc=HepB_childKilic%>% 
  ggplot(aes(x=agecat,y=mid*100,ymin=lo*100,ymax=hi*100))+
  facet_grid(Vaccination_status~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBsAg(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=14,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=15,face="plain"),
        strip.text.y = element_text(size=13,face="plain"),
        legend.text=element_text(size=16))


figure <- ggarrange(plotb,plotc,
                    #labels = c("A", "B","C","D","E","F"),
                    ncol = 1, nrow=2,
                    common.legend = TRUE,
                    legend="bottom")
figure

ggsave(plot=figure,"Figures/HBsAgtest.jpeg", unit="cm",width=26,height=25)

############HBcAb seroprevalence#################
HBcAb_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBcAb")
HBcAb_dat
########Generate age categories
HBcAb_dat$age_years=(HBcAb_dat$doc-HBcAb_dat$dob)/365
##generate agecat
HBcAb_dat=HBcAb_dat %>% 
  mutate(agecat=
           case_when(age_years< 1 ~ "<1yr",
                     age_years< 5 ~ "1-4yrs",
                     age_years< 10 ~ "5-9yrs",
                     age_years< 15 ~ "10-14yrs",
                     age_years<= 19 ~ "15-19yrs")) %>% 
  filter(age_years< 15) 

HBcAb_dat$agecat=factor(HBcAb_dat$agecat,levels=c("<1yr","1-4yrs",
                                                  "5-9yrs","10-14yrs"))

names(HBcAb_dat)
########generate sero estimates###
########Calculate raw HBsAg prevalence in children
HepB_childKilid=HBcAb_dat %>%
  select(Year,agecat,HBcAb_CAT) %>%
  group_by(Year,agecat,HBcAb_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBcAb_CAT,values_from = n) %>%
  mutate(across(3:4,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKilid[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKilid$Pos,HepB_childKilid$Total,methods="wilson")[,c("mean","lower","upper")]

plotd=HepB_childKilid%>% 
  ggplot(aes(x=agecat,y=mid*100,ymin=lo*100,ymax=hi*100))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBcAb(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=14,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),legend.text=element_text(size=16))

figure <- ggarrange(plota,plotd,
                    #labels = c("A", "B","C","D","E","F"),
                    ncol = 1, nrow=2,
                    common.legend = TRUE,
                    legend="bottom")
figure


ggsave(plot=figure,"Figures/HBcAbtest.jpeg", unit="cm",width=23,height=20)


#########Troubleshoot HBsAg and HBcAb###########
HBcAb_dat=read_excel("Data/Finalrawchildren_PCVIS_Mala_Seeck.xlsx",sheet="All_HBsAgcAb")
HBcAb_dat
########Generate age categories
HBcAb_dat$age_years=(HBcAb_dat$doc-HBcAb_dat$dob)/365
##generate agecat
HBcAb_dat=HBcAb_dat %>% 
  mutate(agecat=
           case_when(age_years< 2 ~ "<2yr",
                     age_years< 4 ~ "2-3yrs",
                     age_years< 6 ~ "4-5yrs",
                     age_years< 8 ~ "6-7yrs",
                     age_years< 10 ~"8-9yrs",
                     age_years< 12 ~ "10-11yrs",
                     age_years< 14 ~ "12-13yrs",
                     age_years< 15 ~ "14yrs")) %>% 
  filter(age_years< 15) 

HBcAb_dat$agecat=factor(HBcAb_dat$agecat,levels=c("<2yr","2-3yrs",
                                                  "4-5yrs","6-7yrs",
                                                  "8-9yrs","10-11yrs",
                                                  "12-13yrs","14yrs"))

names(HBcAb_dat)

HepB_childKilid=HBcAb_dat %>%
  select(Year,agecat,HBcAb_CAT) %>%
  group_by(Year,agecat,HBcAb_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBcAb_CAT,values_from = n) %>%
  mutate(across(3:5,~ifelse(is.na(.),0,.)))%>%
  group_by(Year,agecat) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKilid[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKilid$Pos,HepB_childKilid$Total,methods="wilson")[,c("mean","lower","upper")]

plotd=HepB_childKilid%>% 
  ggplot(aes(x=agecat,y=mid*100,ymin=lo*100,ymax=hi*100))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBcAb(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=9,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),legend.text=element_text(size=16))



HepB_childKilie=HBcAb_dat %>%
  select(HBsAg_CAT) %>%
  group_by(HBsAg_CAT) %>% summarise(n=n())%>%
  ungroup() %>% pivot_wider(names_from = HBsAg_CAT,values_from = n) %>%
  #mutate(across(2:3,~ifelse(is.na(.),0,.)))%>%
  #group_by(Year,ageca) %>%
  mutate(Total=sum(pick(Neg,Pos))) %>%
  ungroup()

HepB_childKilie[,c("mid","lo","hi")]=binom::binom.confint(HepB_childKilie$Pos,HepB_childKilie$Total,methods="wilson")[,c("mean","lower","upper")]

plote=HepB_childKilie%>% 
  ggplot(aes(x=agecat,y=mid*100,ymin=lo*100,ymax=hi*100))+
  facet_grid(~Year)+
  geom_bar(stat = "identity",position=position_dodge(),width = 0.75, alpha=0.6)+
  theme_bw()+ylab("HBsAb(%)")+
  xlab("Age category")+
  # geom_pointrange(mapping=aes(x=newage,y=mid,ymin=min,ymax=max), position=position_dodge(0.9),colour="red",alpha=1, size=0.4)+
  geom_errorbar (aes (ymin = lo*100, ymax = hi*100, width = 0.75), 
                 col = "red")+
  theme(axis.text.x=element_text(size=9,face="plain",angle=60,hjust=1),axis.text.y=element_text(size=16,face="plain"),
        axis.title.x=element_text(size=16,face="plain"),axis.title.y=element_text(size=16,face="plain"),
        strip.text.x = element_text(size=16,face="plain"),legend.text=element_text(size=16))


figure <- ggarrange(plote,plotd,
                    #labels = c("A", "B","C","D","E","F"),
                    ncol = 1, nrow=2,
                    common.legend = TRUE,
                    legend="bottom")
figure


ggsave(plot=figure,"Figures/HBcAbHBsAg.jpeg", unit="cm",width=24,height=17)







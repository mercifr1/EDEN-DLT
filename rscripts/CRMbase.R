#' ----------------------------
#' 
#' 
#' BLRM to study dose-DLT relationship
#' in presence of IE
#' 
#' F. Mercier
#' 2022-05-17
#' 
#' 
#' ----------------------------

#' Load libraries
#' ----------------------------
library(tidyverse)
library(crmPack)
library(patchwork)


#' Replicate model from publication
#' ----------------------------
model<-LogisticLogNormal(mean=c(-1.9356, 0.2466),
                         cov=matrix(c(1.2397, 0.2854, 0.2854, 0.1084463), nrow=2),
                         refDose=2.5)

#' Replicate dose increment rules from publication
#' ----------------------------
myIncrements <- IncrementsRelative(intervals=c(0, 3), increments=c(1, 0.33))
myNextBest <- NextBestNCRM(target=c(0.2, 0.35),
                           overdose=c(0.35, 1),
                           maxOverdoseProb=0.25)

#' Set MCMC options
#' ----------------------------
options<-McmcOptions(burnin=1000, step=2, samples=2000)
set.seed(1710)


#' ----------------------------
#' Original setup (without artificial change of the data)
#' ----------------------------

#' Read data
#' Create cohort number and define dose grid
mydf<-read.csv("BLRMdata.csv", header=T, sep=";") %>%
  mutate(COH=dense_rank(DOSE)) %>%
  arrange(COH, SUBJID)
mydf
data<-Data(x=mydf$DOSE, y=mydf$DLTO,
     cohort=mydf$COH, doseGrid=seq(1, 16, by=0.75))


mygdf1<-mydf %>% select(COH, SUBJID, DOSE, DLTO) %>%
  mutate(DES="1. Original") %>% rename(DLT=DLTO)
mygdf2<-mydf %>% select(COH, SUBJID, DOSE, DLTH) %>%
  mutate(DES="2. Hypothetical") %>% rename(DLT=DLTH)
mygdf3<-mydf %>% select(COH, SUBJID, DOSE, DLTH) %>%
  filter(SUBJID!=10) %>%
  mutate(DES="3. Subgroup") %>% rename(DLT=DLTH)
mygdf<-rbind(mygdf1, mygdf2, mygdf3)

#' Graphics design
#' ================
ylab<-c(1, 2.5, 4.75)
mycols<-c("grey70", "gold")
g1<-ggplot(mygdf, aes(x=as.factor(SUBJID), y=as.factor(DOSE)))+
  geom_point(pch=21, colour="grey20", size=4, aes(fill=as.factor(DLT)))+
  scale_y_discrete("Dose (unit)", breaks=ylab, labels=ylab)+
  scale_x_discrete("Participant")+
  scale_fill_manual(values=mycols, guide="none")+
  facet_wrap(~DES)+
  theme(panel.grid.major.x = element_blank())
g1
ggsave("outputs/Figure 2.png", g1, width=12, height=8, unit="cm", dpi=300)


#' Specify max allowable dose given current data
nextMaxDose<-maxDose(myIncrements, data=data)

samples<-mcmc(data, model, options)
#' print(plot(samples, model, data))

doseRecommendation<-nextBest(myNextBest,
                               doselimit=nextMaxDose,
                               samples=samples, model=model, data=data)
doseRecommendation$value
#' [1] 3.25

print(doseRecommendation$plot)


#' ----------------------------
#' Now: Assuming patient #10 has an IE (like trt discont.)
#' 1) Subgroup analysis
#'    excluding patient #10 from the analysis
#' ----------------------------

#' Read data
#' Create cohort number and define dose grid
mydf<-read.csv("BLRMdata.csv", header=T, sep=";") %>%
  mutate(COH=dense_rank(DOSE)) %>%
  filter(SUBJID!=10) %>%
  arrange(COH, SUBJID)
data<-Data(x=mydf$DOSE, y=mydf$DLTO,
           cohort=mydf$COH, doseGrid=seq(1, 16, by=0.75))

#' Specify max allowable dose given current data
nextMaxDose<-maxDose(myIncrements, data=data)

samples<-mcmc(data, model, options)
#' print(plot(samples, model, data))

doseRecommendation<-nextBest(myNextBest,
                             doselimit=nextMaxDose,
                             samples=samples, model=model, data=data)
doseRecommendation$value
#' [1] 3.25

print(doseRecommendation$plot)


#' ----------------------------
#' Now: Assuming patient #10 has an IE (like trt discont.)
#' 2) Hypothetical scenario
#'    Impute DLT=1 to patient #10 
#'    (using DLTH variable in place of the original DLTO)
#' ----------------------------

#' Read data
#' Create cohort number and define dose grid
mydf<-read.csv("BLRMdata.csv", header=T, sep=";") %>%
  mutate(COH=dense_rank(DOSE)) %>%
  arrange(COH, SUBJID)
data<-Data(x=mydf$DOSE, y=mydf$DLTH,
           cohort=mydf$COH, doseGrid=seq(1, 16, by=0.75))

#' Specify max allowable dose given current data
nextMaxDose<-maxDose(myIncrements, data=data)

samples<-mcmc(data, model, options)
#' print(plot(samples, model, data))

doseRecommendation<-nextBest(myNextBest,
                             doselimit=nextMaxDose,
                             samples=samples, model=model, data=data)
doseRecommendation$value
#' [1] 2.5

print(doseRecommendation$plot)

#' ----------------------------
#' Now: Assuming patient #10 has an IE (like trt discont.)
#' 3) Treatment policy
#'    actually correspond to the same data analysis
#'    than the original one as it ignores the IE.
#' ----------------------------



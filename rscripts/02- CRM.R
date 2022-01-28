#' =======================
#' 
#' Project: EDEN-DLT Estimand in Onco Clin Dev
#' Example: Dose-DLT analysis
#' Authors: xxx
#' 2022-Q1
#' 
#' =======================
#' 
#' Dose-escalation
#' =======================



#' Default:
#' ----------------------
p.tox0<-c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 
          0.1, 0.12, 0.15, 0.20, 0.25, 0.30, 0.40)
dose.label<-unique(df0$DOSE)
dose.rank<-data.frame(DOSE=dose.label, RANK=1:length(dose.label))
mydata<-left_join(df0, dose.rank, by="DOSE") %>%
  mutate(patient=SUBJID, dose=RANK, tox=DLTYN) %>% 
  select(patient, dose, tox)

bcrm(data=mydata, stop=list(nmax=37), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.20)


#' Dose omission:
#' ----------------------
dose.label<-sort(unique(domi1$OMIDOSE))
p.tox0<-c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 
          0.1, 0.12, 0.15, 0.175, 0.20, 0.25, 0.30, 0.40)

dose.rank<-data.frame(DOSE=dose.label, RANK=1:length(dose.label))
domi2<-domi1 %>% select(-DOSE) %>% rename(DOSE=OMIDOSE)
mydata<-left_join(domi2, dose.rank, by="DOSE") %>%
  mutate(patient=SUBJID, dose=RANK, tox=DLTYN) %>% 
  select(patient, dose, tox)

bcrm(data=mydata, stop=list(nmax=37), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.20)


#' Dosing error:
#' ----------------------
dose.label<-sort(unique(derr1$ERRDOSE))
p.tox0<-c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 
          0.1, 0.12, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)

dose.rank<-data.frame(DOSE=dose.label, RANK=1:length(dose.label))
derr2<-derr1 %>% select(-DOSE) %>% rename(DOSE=ERRDOSE)
mydata<-left_join(derr2, dose.rank, by="DOSE") %>%
  mutate(patient=SUBJID, dose=RANK, tox=DLTYN) %>% 
  select(patient, dose, tox)

bcrm(data=mydata, stop=list(nmax=35), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.20)







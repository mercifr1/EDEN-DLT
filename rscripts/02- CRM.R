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

m001<-bcrm(data=mydata, stop=list(nmax=37), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.20)
m001.est<-data.frame(t(m001$ndose[[1]]$quantiles)) %>%
  rename(PP25=X25., PP50=X50., PP75=X75.) %>%
  select(PP25, PP50, PP75) %>%
  mutate(DOSE=m001$dose, GROUP="a) Original",
         RECO=ifelse(DOSE==5.6, 1, 0))

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

m002<-bcrm(data=mydata, stop=list(nmax=37), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.20)
m002.est<-data.frame(t(m002$ndose[[1]]$quantiles)) %>%
  rename(PP25=X25., PP50=X50., PP75=X75.) %>%
  select(PP25, PP50, PP75) %>%
  mutate(DOSE=m002$dose, GROUP="b) Omission",
         RECO=ifelse(DOSE==5.6, 1, 0))


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

m003<-bcrm(data=mydata, stop=list(nmax=35), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.20)
m003.est<-data.frame(t(m003$ndose[[1]]$quantiles)) %>%
  rename(PP25=X25., PP50=X50., PP75=X75.) %>%
  select(PP25, PP50, PP75) %>%
  mutate(DOSE=m003$dose, GROUP="c) Error",
         RECO=ifelse(DOSE==6.4, 1, 0))

#' Dose tox curve
globest<-rbind(m001.est, m002.est, m003.est)

myx<-c(0.01, 0.8, 3.2, 4, 4.8, 5.6, 6.4, 12.8)
pd<-position_dodge(0.1)
g004<-ggplot(globest, aes(DOSE, PP50))+
  geom_hline(aes(yintercept=0.2), lty=2, colour="grey50")+
  geom_errorbar(aes(ymin=PP25, ymax=PP75, group=as.factor(GROUP)), 
                colour="grey80", width=.1, position=pd)+
  geom_point(pch=21, alpha=0.8, size=1.2, stroke=0.8, 
             fill="lightblue1", color="grey80")+
  geom_point(data=globest %>% filter(RECO==1), 
             pch=21, alpha=0.8, size=4, stroke=1, fill=NA, color="grey20")+
  facet_wrap(~as.factor(GROUP), ncol=1)+
  scale_x_continuous("Dose (ug/kg)", breaks=myx)+
  scale_y_continuous("Prob(DLT)",
                     breaks=c(0.01, 0.1, 0.2, 0.3),
                     limits=c(0.01, 0.35))+
  theme_ipsum()+
  theme(legend.position="none",
        panel.grid.minor=element_blank())
g004

ggsave("./outputs/Fig02-Dose reco.jpg", g004,
       width=24, height=15, unit="cm", dpi=300)



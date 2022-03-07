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
dose.label<-sort(c(unique(df0$DOSE), 2, 3, 4))
p.tox0<-c(0.05, 0.1, 0.17, 0.3, 0.42, 0.50, 0.60)
dose.rank<-data.frame(DOSE=dose.label, RANK=1:length(dose.label))
mydata<-left_join(df0, dose.rank, by="DOSE") %>%
  mutate(patient=SUBJID, dose=RANK, tox=DLTYN) %>% 
  select(patient, dose, tox)

m001<-bcrm(data=mydata, stop=list(nmax=17), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.33)
m001
m001.est<-data.frame(t(m001$ndose[[1]]$quantiles)) %>%
  rename(PP25=X25., PP50=X50., PP75=X75.) %>%
  select(PP25, PP50, PP75) %>%
  mutate(DOSE=m001$dose, GROUP="a) Original",
         RECO=ifelse(DOSE==3.25, 1, 0))
m001.est



#' Dosing error / Actual dose
#' ----------------------
dose.label<-sort(c(unique(derr1$ERRDOSE), 3, 4))
#plot(dose.label, p.tox0)

dose.rank<-data.frame(DOSE=dose.label, RANK=1:length(dose.label))
derr2<-derr1 %>% select(-DOSE) %>% rename(DOSE=ERRDOSE)
mydata<-left_join(derr2, dose.rank, by="DOSE") %>%
  mutate(patient=SUBJID, dose=RANK, tox=DLTYN) %>% 
  select(patient, dose, tox)

m002<-bcrm(data=mydata, stop=list(nmax=17), p.tox0=p.tox0, dose=dose.label,
     ff="power", prior.alpha=c(1, 1, 1), target.tox=0.33)
m002
m002.est<-data.frame(t(m002$ndose[[1]]$quantiles)) %>%
  rename(PP25=X25., PP50=X50., PP75=X75.) %>%
  select(PP25, PP50, PP75) %>%
  mutate(DOSE=m002$dose, GROUP="b) Error",
         RECO=ifelse(DOSE==3.25, 1, 0))
m002.est


#' Dose tox curve
globest<-rbind(m001.est, m002.est)

myx<-sort(c(unique(derr1$ERRDOSE), 3, 4))
pd<-position_dodge(0.1)
g003<-ggplot(globest, aes(DOSE, PP50))+
  geom_hline(aes(yintercept=0.33), lty=2, colour="grey50")+
  geom_errorbar(aes(ymin=PP25, ymax=PP75, group=as.factor(GROUP)), 
                colour="grey80", width=.1, position=pd)+
  geom_point(pch=21, alpha=0.8, size=1.2, stroke=0.8, 
             fill="lightblue1", color="grey80")+
  geom_point(data=globest %>% filter(RECO==1), 
             pch=21, alpha=0.8, size=4, stroke=1, fill=NA, color="grey20")+
  facet_wrap(~as.factor(GROUP), ncol=1)+
  scale_x_continuous("Dose (mg)", breaks=myx)+
  scale_y_continuous("Prob(DLT)",
                     breaks=c(0.01, 0.2, 0.33, 0.5), limits=c(0, 0.7))+
  theme_ipsum()+
  theme(legend.position="none",
        panel.grid.minor=element_blank())
g003

ggsave("./outputs/Fig02-Dose reco.jpg", g003,
       width=18, height=15, unit="cm", dpi=300)



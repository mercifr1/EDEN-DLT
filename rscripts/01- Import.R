#' =======================
#' 
#' Project: EDEN-DLT Estimand in Onco Clin Dev
#' Example: Dose-DLT analysis
#' Authors: xxx
#' 2022-Q1
#' 
#' =======================
#' 
#' Import data and create scenarios
#' for DLT as Y/N
#' 
#' =======================


df0<-read.csv("./data/DLTYNexample.csv", sep=";")

#' Default DLT as Y/N:
#' ----------------------
#' Introduce dosing regimen 
#' (see Schoeffski et al. 2004 - section "Study treatment")
#' Day 1, Day 4, every week for one cycle; 
#' one cycle = 3 weeks (6 infusions)
#' ----------------------
des<-expand.grid(DAY=c(1, 4), WEEK=1:3, SUBJID=df0$SUBJID)
df1<-left_join(df0, des, by="SUBJID")

#' unique(df0$DOSE)

myfills<-c("lightblue1", "orange1")
mycols<-c("grey80", "black")
g001<-ggplot(df0, aes(SUBJID, DOSE))+
  geom_line(colour="grey80", alpha=0.8)+
  geom_point(pch=21, alpha=0.8, size=1.2, stroke=0.8, 
             aes(fill=as.factor(DLTYN),
                 color=as.factor(DLTYN)))+
  scale_fill_manual(values=myfills, guide=F)+
  scale_color_manual(values=mycols, guide=F)+
  scale_x_continuous("Subject ID", breaks=1:37)+
  scale_y_continuous("Dose level (ug/kg)",
                     breaks=c(0.01, 0.8, 3.2, 4, 4.8, 5.6, 6.4, 12.8),
                     limits=c(0.01, 12.8))+
  labs(subtitle="a) Original data")+
  theme_ipsum()+
  theme(legend.position="none",
        panel.grid.minor=element_blank())
g001

#' Dose omission:
#' ----------------------
#' Altering the default dataset, assuming
#' - pat 26, 27, 28 and 32, have omitted his/her dose 
#' on Week 1 Day 4, Week 2 Day 1 and Week 2 Day 4.
#' ----------------------
domi0<-df1 %>%
  mutate(WDOSE=ifelse(
      SUBJID %in% c(26, 27, 28, 32) & WEEK==1 & DAY==4 |
      SUBJID %in% c(26, 27, 28, 32) & WEEK==2 & DAY==1 |
      SUBJID %in% c(26, 27, 28, 32) & WEEK==2 & DAY==4, 
      0, DOSE))
domi1<-domi0 %>%
  group_by(SUBJID) %>%
    summarize(OMIDOSE=mean(WDOSE)) %>%
  ungroup() %>%
  left_join(., df0, by="SUBJID")

g002<-ggplot(domi1, aes(SUBJID, DOSE))+
  geom_line(colour="grey80", alpha=0.8)+
  geom_point(pch=21, alpha=0.8, size=1.2, stroke=0.8, 
             aes(y=OMIDOSE, 
                 fill=as.factor(DLTYN),
                 color=as.factor(DLTYN)))+
  geom_point(data=domi1 %>% filter(SUBJID %in% c(26, 27, 28, 32)), 
             pch=21, alpha=0.8, size=4, stroke=1, fill=NA, color="grey20",
             aes(y=OMIDOSE))+
  scale_fill_manual(values=myfills, guide=F)+
  scale_color_manual(values=mycols, guide=F)+
  scale_x_continuous("Subject ID", breaks=1:37)+
  scale_y_continuous("Average dose level (ug/kg)",
                     breaks=c(0.01, 0.8, 3.2, 4, 4.8, 5.6, 6.4, 12.8),
                     limits=c(0.01, 12.8))+
  labs(subtitle="b) Dose omission scenario")+
  theme_ipsum()+
  theme(legend.position="none",
        panel.grid.minor=element_blank())
g002

#' Dosing error scenario:
#' ----------------------
#' Altering the default dataset, assuming
#' - pat 34 and 35, has actually received a double dose 
#' ----------------------
derr1<-df0 %>%
  mutate(ERRDOSE=ifelse(SUBJID %in% c(34, 35), 12.8, DOSE)) %>%
  filter(SUBJID<=35)

ggplot(derr1, aes(SUBJID, DOSE))+
  geom_line()+
  geom_point(aes(y=ERRDOSE, colour=as.factor(DLTYN)))+
  theme_minimal()

g003<-ggplot(derr1, aes(SUBJID, DOSE))+
  geom_line(colour="grey80", alpha=0.8)+
  geom_point(pch=21, alpha=0.8, size=1.2, stroke=0.8, 
             aes(y=ERRDOSE, 
                 fill=as.factor(DLTYN),
                 color=as.factor(DLTYN)))+
  geom_point(data=derr1 %>% filter(SUBJID %in% c(34, 35)), 
             pch=21, alpha=0.8, size=4, stroke=1, fill=NA, color="grey20",
             aes(y=ERRDOSE))+
  scale_fill_manual(values=myfills, guide=F)+
  scale_color_manual(values=mycols, guide=F)+
  scale_x_continuous("Subject ID", breaks=1:37)+
  scale_y_continuous("Dose level (ug/kg)",
                     breaks=c(0.01, 0.8, 3.2, 4, 4.8, 5.6, 6.4, 12.8),
                     limits=c(0.01, 12.8))+
  labs(subtitle="c) Dosing error scenario")+
  theme_ipsum()+
  theme(legend.position="none",
        panel.grid.minor=element_blank())
g003

gtog<-g001/g002/g003

ggsave("./outputs/Fig01-Dose esc overview.jpg", gtog,
       width=24, height=30, unit="cm", dpi=300)




#' =======================
#' 
#' Import data and create scenarios
#' for DLT as time-to
#' 
#' =======================

td0<-read.csv("./data/DLTTIMexample.csv", sep=",")

ggplot(td0, aes(START, DOSE))+
  geom_segment(aes(xend=END, yend=DOSE, colour=as.factor(DLTYN)))+
  geom_point(aes(colour=as.factor(DLTYN)))+
  geom_hline(aes(yintercept=70), lty=2)+
  scale_y_continuous("Dose (mg)", breaks=20*0:5, limits=c(0, 100))+
  theme_minimal()





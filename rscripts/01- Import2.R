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


df0<-read.csv("./data/DLTYNexample2.csv", sep=",")

#' Default DLT as Y/N:
#' ----------------------
#' Introduce dosing regimen 
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
  geom_point(pch=21, alpha=0.8, size=3, stroke=0.8, 
             aes(fill=as.factor(DLTYN),
                 color=as.factor(DLTYN)))+
  scale_fill_manual(values=myfills, guide=F)+
  scale_color_manual(values=mycols, guide=F)+
  scale_x_continuous("Subject ID", breaks=1:37)+
  scale_y_continuous("Dose level (mg)",
                     breaks=c(1, 2.5, 3.25, 4.75, 6),
                     limits=c(1, 6))+
  labs(subtitle="a) Original data")+
  theme_ipsum()+
  theme(legend.position="none",
        panel.grid.minor=element_blank())
g001


#' Dosing error scenario:
#' ----------------------
#' Altering the default dataset, assuming
#' - pat 34 and 35, has actually received a double dose 
#' ----------------------
derr1<-df0 %>%
  mutate(ERRDOSE=ifelse(SUBJID %in% c(12, 13), 2, DOSE))

g002<-ggplot(derr1, aes(SUBJID, DOSE))+
  geom_line(colour="grey80", alpha=0.8)+
  geom_point(pch=21, alpha=0.8, size=3, stroke=0.8, 
             aes(y=ERRDOSE, 
                 fill=as.factor(DLTYN),
                 color=as.factor(DLTYN)))+
  geom_point(data=derr1 %>% filter(SUBJID %in% c(34, 35)), 
             pch=21, alpha=0.8, size=3, stroke=1, fill=NA, color="grey20",
             aes(y=ERRDOSE))+
  scale_fill_manual(values=myfills, guide=F)+
  scale_color_manual(values=mycols, guide=F)+
  scale_x_continuous("Subject ID", breaks=1:37)+
  scale_y_continuous("Dose level (mg)",
                     breaks=c(1, 2.5, 3.25, 4.75, 6),
                     limits=c(1, 6))+
  labs(subtitle="c) Dosing error scenario")+
  theme_ipsum()+
  theme(legend.position="none",
        panel.grid.minor=element_blank())
g002

gtog<-g001/g002

ggsave("./outputs/Fig01-Dose esc overview.jpg", gtog,
       width=18, height=20, unit="cm", dpi=300)



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





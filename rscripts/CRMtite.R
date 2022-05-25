#' ----------------------------
#' 
#' 
#' TiTE CRM
#' Analysis of a weighted logisitic TITE-CRM under different estimand scenarios
#' 
#' V. Homer
#' 2022-05-25
#' 
#' 
#' ----------------------------

#' Load libraries
#' ----------------------------
library(tidyverse)
library(scales)
library(dfcrm)


read.csv('data/TITEdata.csv') -> DF

cols<-hue_pal()(13)
# Global parameters & history figure ---- 
g01<-DF %>% 
  mutate(DLT = ifelse(DLT == 0, 'No', 'Yes'), 
         DOSE = paste0('Dose Level: ', DoseLabel, ' (', DOSE, 'mg)'), 
         DOSE = ordered(DOSE, 
                        c('Dose Level: 1 (0.01mg)', 'Dose Level: 2 (0.02mg)', 'Dose Level: 3 (0.04mg)',
                        'Dose Level: 4 (0.1mg)', 'Dose Level: 5 (0.2mg)', 'Dose Level: 6 (0.4mg)', 
                        'Dose Level: 7 (0.8mg)', 'Dose Level: 8 (1.6mg)', 'Dose Level: 9 (2.4mg)', 
                        'Dose Level: 10 (3.2mg)', 'Dose Level: 11 (4mg)', 'Dose Level: 12 (4.8mg)', 
                        'Dose Level: 13 (5.6mg)', 'Dose Level: 14 (6.4mg)'))) %>% 
  ggplot() + 
  geom_segment(lwd=1, aes(x = START, xend = END, y = SUBJID, yend = SUBJID, col = DOSE)) + 
  geom_point(size=3, aes(x = END, y = SUBJID, shape = DLT, col = DOSE)) + 
  scale_shape_manual(values = c(1, 16), limits = c('No', 'Yes')) +
  scale_y_continuous("Participants", breaks=1:13)+
  labs(col = 'Dose Level', 
       shape = 'Incidence of DLT', 
       x = 'Time (weeks)') + 
  scale_color_manual(values = rev(cols))+
  theme(panel.grid.minor = element_blank())
ggsave("outputs/Figure 3.png", g01, width=18, height=12, unit="cm", dpi=300)

# generate an IE in TNO 13 (as described in manuscript)
DF <- DF %>% 
  mutate(IE = ifelse(SUBJID == '13', 1, 0), 
         IETime = ifelse(IE == 1, START + 2.5, NA)) 

skeleton = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.08, 
             0.1, 0.12, 0.15, 0.20, 0.25, 0.30, 0.40)

# Analysis with no consideration for IEs (treatment policy) ---- 
titecrm(prior = skeleton, 
        target = 0.30, 
        tox = DF$DLT, 
        level = DF$DoseLabel, 
        entry = DF$START, 
        exit = DF$END, 
        obswin = 10, 
        pid = DF$SUBJID)

# Hypothetical (conservative) strategy ---- 
DF %>% 
  mutate(DLT = ifelse(IE == 1, 1, DLT), 
         END = ifelse(IE == 1, IETime, END)) -> DFScen1a_Hyp

titecrm(prior = skeleton, 
        target = 0.30, 
        tox = DFScen1a_Hyp$DLT, 
        level = DFScen1a_Hyp$DoseLabel, 
        entry = DFScen1a_Hyp$START, 
        exit = DFScen1a_Hyp$END, 
        obswin = 10, 
        pid = DFScen1a_Hyp$SUBJID)



# subgroup ---- 
DF %>% 
  filter(IE == 0) -> DFScen1a_PS

titecrm(prior = skeleton, 
        target = 0.30, 
        tox = DFScen1a_PS$DLT, 
        level = DFScen1a_PS$DoseLabel, 
        entry = DFScen1a_PS$START, 
        exit = DFScen1a_PS$END, 
        obswin = 10, 
        pid = DFScen1a_PS$SUBJID)



# While on treatment ---- 
DF %>% 
  mutate(DLT = ifelse(IE == 1 & IETime < END, 0, DLT), 
         END = ifelse(IE == 1, IETime, END)) -> DFScen1a_WOT

titecrm(prior = skeleton, 
        target = 0.30, 
        tox = DFScen1a_WOT$DLT, 
        level = DFScen1a_WOT$DoseLabel, 
        entry = DFScen1a_WOT$START, 
        exit = DFScen1a_WOT$END, 
        obswin = 10, 
        pid = DFScen1a_WOT$SUBJID)


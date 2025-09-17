### Reservoir cores analyses
### Figs. S9, S10

### Packages
library(tidypaleo)
library(tidyverse)

#### Data - reservoir pigments
dug_pigments <- read.csv(file.choose(), header=TRUE)
### pigment labels
PARAMS <- c('Allo', 'Diato', 'Lutein', 'Cantha', 'Echine', 'Myxo', 'Oke',
            'Pheo_B', 'Pheo_A', 'Chla.Pheoa')
PARAM.LABS <- c('Alloxanthin', 'Diatoxanthin', 'Lutein-zeaxanthin',
                'Canthaxanthin', 'Echinenone', 'Myxoxanthophyll', 'Okenone', 
                'Pheophytin b', 'Pheophytin a', 'Chl a:Pheo a')
### data pivot and leveling
dug.long <- 
  select(dug_pigments, Depth, Date, Site, all_of(PARAMS)) %>%
  pivot_longer(all_of(PARAMS), names_to = 'param', values_to = 'value') %>%
  mutate(param = factor(param, levels = PARAMS),
         param.lab = case_when(param == 'Diato' ~ 'Diatoxanthin',
                               param == 'Allo' ~ 'Alloxanthin',
                               param == 'Pheo_B' ~ 'Pheophytin b',
                               param == 'Lutein' ~ 'Lutein-zeaxanthin',
                               param == 'Echine' ~ 'Echinenone',
                               param == 'Cantha' ~ 'Canthaxanthin',
                               param == 'Myxo' ~ 'Myxoxanthophyll',
                               param == 'Oke' ~ 'Okenone',
                               param == 'Pheo_A' ~ 'Pheophytin a',
                               param == 'Chla.Pheoa' ~ 'Chl a:Pheo a') %>%
           factor(levels = c('Diatoxanthin', 'Alloxanthin', 'Pheophytin b', 
                             'Lutein-zeaxanthin', 'Echinenone', 'Canthaxanthin',
                             'Myxoxanthophyll', 'Okenone', 'Pheophytin a', 
                             'Chl a:Pheo a')))
### more leveling
dug.new <- dug.long %>%
  mutate(across(Site, factor, levels=c("F1", "F2", "F3", "R1", "R2", "R3")))

### plot by estimated date
coredate.C.p <-
  ggplot(dug.new, aes(value, Date)) +
  facet_geochem_gridh(vars(param.lab), grouping=vars(Site), scales = 'free')+
  #scale_y_reverse()+
  expand_limits(x=0)+
  scale_x_continuous()+
  geom_point()+
  geom_lineh(data=subset(dug.new, param.lab=="Diatoxanthin"), color="#996600", linewidth=1)+
  geom_lineh(data=subset(dug.new, param.lab=="Alloxanthin"), color="#660000", linewidth=1)+
  geom_lineh(data=subset(dug.new, param.lab=="Pheophytin b"), color="#339900", linewidth=1)+
  geom_lineh(data=subset(dug.new, param.lab=="Lutein-zeaxanthin"), color="#009966", linewidth=1)+
  geom_lineh(data=subset(dug.new, param.lab=="Echinenone"), color="#003399", linewidth=1)+
  geom_lineh(data=subset(dug.new, param.lab=="Canthaxanthin"), color="#3399FF", linewidth=1)+
  geom_lineh(data=subset(dug.new, param.lab=="Myxoxanthophyll"), color="#0066CC", linewidth=1)+
  geom_lineh(data=subset(dug.new, param.lab=="Okenone"), color="#330066", linewidth=1)+
  geom_lineh(data=subset(dug.new[!is.na(dug.new$value),], param.lab=="Pheophytin a"),
             color="#003300", linewidth=1)+
  geom_lineh(data=subset(dug.new[!is.na(dug.new$value),], param.lab=="Chl a:Pheo a"), 
             color="black", linewidth=1)+
  labs(x=expression(Pigment~concentration~nmol~g^{-1}~C), y = "Estimated Date")+ 
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(strip.placement = 'outside')
  
coredate.C.p + theme(panel.spacing.x=unit(1,"lines"))
#### Fig. S9 

##############################################################################
### Reservoir geochem data
### data
dug_irms <- read.csv(file.choose(), header=TRUE)
### lables
PARAMS <- c('d13C', 'd15N', 'X.C', 'X.N', 'C.N.ratio')
PARAM.LABS <- c('δ13C ‰', 'δ15N ‰', '%C', '%N', 'C:N ratio')
### pivot
dug.long <- 
  select(dug_irms, Depth, Date, Site, all_of(PARAMS)) %>%
  pivot_longer(all_of(PARAMS), names_to = 'param', values_to = 'value') %>%
  mutate(param = factor(param, levels = PARAMS),
         param.lab = case_when(param == 'X.N' ~ '%N',
                               param == 'X.C' ~ '%C',
                               param == 'd15N' ~ 'δ15N ‰',
                               param == 'd13C' ~ 'δ13C ‰',
                               param == 'C.N.ratio' ~ 'C:N ratio') %>%
           factor(levels = c('%N', '%C', 'δ15N ‰', 'δ13C ‰', 'C:N ratio')))
### level sites
dug.new <- dug.long %>%
  mutate(across(Site, factor, levels=c("F1", "F2", "F3", "R1", "R2", "R3")))

### plot by estimated date 
coreirms.C.p <-
  ggplot(dug.new, aes(value, Date)) +
  facet_geochem_gridh(vars(param.lab), grouping=vars(Site), scales = 'free')+
  #scale_y_reverse()+
  expand_limits(x=0)+
  geom_point()+
  geom_lineh()+
  labs(x=NULL, y = "Estimated Date")+ 
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(strip.placement = 'outside')

coreirms.C.p + theme(panel.spacing.x=unit(1,"lines"))
### Fig. S10
  






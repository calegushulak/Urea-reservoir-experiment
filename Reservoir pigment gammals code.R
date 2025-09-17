### Modelling pigment concentrations using GAMs
### Fig. 1

#Packages
library('dplyr')   # for data wrangling functions
library('tidyr')   # 
library('mgcv')    # for modelling
library('ggplot2') # for plotting
library('gratia')  # for GAM plotting and diagnostics
library('purrr')   # for map
library('cowplot') # for plot grid 
library('ggpubr')  # for p values 

# Import data - just pigments
d.pigments <-
  read.csv(file.choose (), header=TRUE) %>%
  mutate(Site = factor(Site,
                       levels=c('F1', 'F2', 'F3', 'R1', 'R2', 'R3')), ### listed in order 
         # Experimental/Reference: 
         Group = factor(Group))%>% ### 
  # remove unnecessary columns
  select(Site, Group, DOY, Chlorophylla, Fucoxanthin, Diatoxanthin, Alloxanthin, 
         Chlorophyllb, LuteinZeaxanthin, Echinenone, Canthaxanthin) %>%
  as_tibble()

# pivot to long data
pigs.long <-
  pivot_longer(d.pigments,
               cols = c(Chlorophylla, Fucoxanthin, Diatoxanthin, Alloxanthin, 
                        Chlorophyllb, LuteinZeaxanthin,
                        Echinenone, Canthaxanthin),
               names_to = 'pigment', values_to = 'conc') %>%
  select(Site, Group, DOY, conc, pigment)

### plot the basic trends
ggplot(pigs.long) +
  facet_grid(pigment ~ Group, scales = 'free_y') +
  geom_point(aes(DOY, conc, group = Site, color = Group),
            show.legend = FALSE) +
  scale_color_brewer(type = 'qual', palette = 6) +
  labs(x = date.lab, y = conc.lab)


# models ####
# fixed effect of each group to estimate exact effects
# random effect of site 
# model by group (i.e. the "box" model)
fit.m.box <- function(p) {
  d <- filter(pigs.long, pigment == p) # subset to the specific pigment
  
  cat('Fitting', p, 'box model...\n')
  gam(conc ~ Group +
        s(DOY, by = Group, k = 5) +     # "fixed effect" smooths
        s(DOY, Site, k = 7, bs = 'fs'), # "random effect" smooths
      family = Gamma(link = 'log'),
      data = d,
      method = 'REML')
}

# "box" model with a location-scale distribution
fit.m.box.gammals <- function(p) {
  d <- filter(pigs.long, pigment == p) # subset to the specific pigment
  
  cat('Fitting', p, 'gammals model...\n')
  gam(list(conc ~ Group +
             s(DOY, by = Group, k = 5) +     # "fixed effect" smooths
             s(DOY, Site, k = 7, bs = 'fs'), # "random effect" smooths
           ~ s(Site, bs = 're')),
      family = gammals(),
      data = d,
      method = 'REML')
}

#models 
models <- tibble(pigment = unique(pigs.long$pigment),
                 m.box = map(pigment, fit.m.box),
                 m.box.gammals = map(pigment, fit.m.box.gammals))
### models fit

# diagnostics ####
appraise(models$m.box[[1]]) # actually looks good
appraise(models$m.box[[2]]) # some left skew
appraise(models$m.box[[3]]) # a bit left skew too
appraise(models$m.box[[4]]) # very weird residuals 
appraise(models$m.box[[5]]) # left skew again
appraise(models$m.box[[6]]) # not great 
appraise(models$m.box[[7]]) # also weird

# gammals models - better
appraise(models$m.box.gammals[[1]]) ## very good
appraise(models$m.box.gammals[[2]]) ## even better
appraise(models$m.box.gammals[[3]]) ## also pretty good
appraise(models$m.box.gammals[[4]]) ## some left skew here
appraise(models$m.box.gammals[[5]]) # left tail is long
appraise(models$m.box.gammals[[6]]) # strange distribution
appraise(models$m.box.gammals[[7]]) ## this one is good

## draw models ####
## different trends in many models
draw(models$m.box[[1]]) 
draw(models$m.box[[2]])
draw(models$m.box[[3]])
draw(models$m.box[[4]])
draw(models$m.box[[5]])
draw(models$m.box[[6]])
draw(models$m.box[[7]])

# box gammals
draw(models$m.box.gammals[[1]], parametric = FALSE) 
draw(models$m.box.gammals[[2]], parametric = FALSE) 
draw(models$m.box.gammals[[3]], parametric = FALSE)
draw(models$m.box.gammals[[4]], parametric = FALSE)
draw(models$m.box.gammals[[5]], parametric = FALSE)
draw(models$m.box.gammals[[6]], parametric = FALSE)
draw(models$m.box.gammals[[7]], parametric = FALSE)

# extract differences in AIC from full model
models %>%
  pivot_longer(-pigment) %>%
  mutate(AIC = map_dbl(value, AIC)) %>%
  group_by(pigment) %>%
  ungroup() %>%
  select(-value) %>%
  pivot_wider(names_from = name, values_from = AIC) %>%
  mutate(m.box = m.box - m.box.gammals,
         m.box.gammals = m.box.gammals - m.box.gammals)
#### 

# predictions by lake ####
pred.fun <- function(p) {
  m <- models$m.box.gammals[[which(models$pigment == p)]]
  predict(m, newd, type = 'link', se.fit = TRUE) %>%
    as.data.frame() %>%
    bind_cols(newd) %>%
    mutate(pigment = p)
}

newd <- filter(d.pigments, !duplicated(Site)) %>%
  select(Site, Group) %>% 
  expand_grid(DOY = evenly(d.pigments$DOY, n = 400))
pred <-
  map(models$pigment, pred.fun) %>%
  bind_rows() %>%
  mutate(pigm.lab = factor(case_when(pigment == 'LuteinZeaxanthin' ~ 'LuteinZeaxanthin',
                                     pigment == 'Echinenone' ~ 'Echinenone',
                                     pigment == 'Chlorophyllb' ~ 'Chlorophyllb',
                                     pigment == 'Alloxanthin' ~ 'Alloxanthin',
                                     pigment == 'Canthaxanthin' ~ 'Canthaxanthin',
                                     pigment == 'Diatoxanthin' ~ 'Diatoxanthin',
                                     pigment == 'Fucoxanthin' ~ 'Fucoxanthin',
                                     pigment == 'Chlorophylla' ~ 'Chlorophylla'),
                           levels=c('Chlorophylla', 'Fucoxanthin', 'Diatoxanthin', 
                                    'Alloxanthin', 'Chlorophyll b','LuteinZeaxanthin', 
                                    'Echinenone', 'Canthaxanthin')),
         pig.gammals = interaction(pigment, Group) %>%
           factor(levels = c('Chlorophylla.F', 'Chlorophylla.R',
                             'Fucoxanthin.F', 'Fucoxanthin.R', 
                             'Diatoxanthin.F', 'Diatoxanthin.R', 
                             'Alloxanthin.F', 'Alloxanthin.R',
                             'Chlorophyllb.F', 'Chlorophyllb.R',
                             'LuteinZeaxanthin.F', 'LuteinZeaxanthin.R',
                             'Echinenone.F', 'Echine.R',
                             'Canthaxanthin.F', 'Canthaxanthin.R')),
         mean = exp(fit.1),
         lwr = exp(fit.1 - 1.96 * se.fit.1),
         upr = exp(fit.1 + 1.96 * se.fit.1)) %>%
  as_tibble()

###plotting
date.lab <- 'Day of Year'
conc.lab <- expression(Concentration~(nmol~L^{-1}))
chl.lab <- expression(Concentration~(mu~g~L^{-1}))
          
#Plot individually and grid 
Chla.p <- 
  ggplot(data=subset(pigs.long, pigment=="Chlorophylla"))+
  facet_grid(~Group, labeller=label_parsed)+
  geom_vline(data=subset(pred, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2)+
  geom_point(aes(DOY, conc, fill=Site), pch=21, color="black", size=1.25)+
  geom_line(data=subset(pred, pigm.lab=="Chlorophylla"), 
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigm.lab=="Chlorophylla"), 
              aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  labs(x = date.lab, y = chl.lab) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  theme(legend.position = 'top')
Chla.p

Fuco.p <-
  ggplot(data=subset(pigs.long, pigment=="Fucoxanthin"))+
  facet_grid(~Group, labeller=label_parsed)+
  geom_vline(data=subset(pred, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2)+
  geom_point(aes(DOY, conc, fill=Site), pch=21, color="black", size=1.25)+
  geom_line(data=subset(pred, pigm.lab=="Fucoxanthin"), 
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigm.lab=="Fucoxanthin"), 
              aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  labs(x = date.lab, y = conc.lab) +
  coord_cartesian(ylim=c(0,50))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  theme(legend.position = 'top')
Fuco.p
  
Diato.p <- 
  ggplot(data=subset(pigs.long, pigment=="Diatoxanthin"))+
  facet_grid(~Group, scales='free_y', labeller=label_parsed)+
  geom_vline(data=subset(pigs.long, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2) +
  geom_point(aes(DOY, conc, fill=Site), pch=21, color="black", size=1.25)+
  labs(x = date.lab, y = conc.lab) +
  geom_line(data=subset(pred, pigm.lab=="Diatoxanthin"), 
           aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigm.lab=="Diatoxanthin"), 
              aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  coord_cartesian(ylim=c(0,16))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  #theme(strip.text.x=element_blank())+
  theme(legend.position = 'top')
Diato.p

Allo.p<-
  ggplot(data=subset(pigs.long, pigment=="Alloxanthin"))+
  facet_grid(~ Group, scales='free_y', labeller=label_parsed)+
  geom_vline(data=subset(pigs.long, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2) +
  geom_point(aes(DOY, conc, fill=Site),pch=21, color="black", size=1.25)+
  labs(x = date.lab, y = conc.lab) +
  geom_line(data=subset(pred, pigm.lab=="Alloxanthin"), 
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigm.lab=="Alloxanthin"), 
             aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  coord_cartesian(ylim=c(0,80))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  #theme(strip.text.x=element_blank())+
  theme(legend.position = 'top')
Allo.p

Chlb.p<-
  ggplot(data=subset(pigs.long, pigment=="Chlorophyllb"))+
  facet_grid(~ Group, scales='free_y', labeller=label_parsed)+
  geom_vline(data=subset(pigs.long, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2) +
  geom_point(aes(DOY, conc, fill=Site), pch=21, color="black", size=1.25)+
  labs(x = date.lab, y = conc.lab) +
  geom_line(data=subset(pred, pigment=="Chlorophyllb"), 
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigment=="Chlorophyllb"), 
             aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  coord_cartesian(ylim=c(0,85))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  #theme(strip.text.x=element_blank())+
  theme(legend.position = 'top')
Chlb.p

Lut.p<-
  ggplot(data=subset(pigs.long, pigment=="LuteinZeaxanthin"))+
  facet_grid(~ Group, scales='free_y', labeller=label_parsed)+
  geom_vline(data=subset(pigs.long, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2) +
  geom_point(aes(DOY, conc, fill=Site), pch=21, color="black", size=1.25)+
  labs(x = date.lab, y = conc.lab) +
  geom_line(data=subset(pred, pigm.lab=="LuteinZeaxanthin"), 
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigm.lab=="LuteinZeaxanthin"), 
             aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  coord_cartesian(ylim=c(0,120))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  #theme(strip.text.x=element_blank())+
  theme(legend.position = 'top')
Lut.p

Ech.p<-
  ggplot(data=subset(pigs.long, pigment=="Echinenone"))+
  facet_grid(~ Group, scales='free_y', labeller=label_parsed)+
  geom_vline(data=subset(pigs.long, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2) +
  geom_point(aes(DOY, conc, fill=Site), pch=21, color="black", size=1.25)+
  labs(x = date.lab, y = conc.lab) +
  geom_line(data=subset(pred, pigm.lab=="Echinenone"), 
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigm.lab=="Echinenone"), 
             aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  coord_cartesian(ylim=c(0,3.5))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  #theme(strip.text.x=element_blank())+
  theme(legend.position = 'top')
Ech.p

Canth.p <-
  ggplot(data=subset(pigs.long, pigment=="Canthaxanthin"))+
  facet_grid(~ Group, scales='free_y', labeller=label_parsed)+
  geom_vline(data=subset(pigs.long, Group=="F"), aes(xintercept=200), color ="black", size =1, linetype=2) +
  geom_point(aes(DOY, conc, fill=Site), pch=21, color="black", size=1.25)+
  labs(x = date.lab, y = conc.lab) +
  geom_line(data=subset(pred, pigm.lab=="Canthaxanthin"), 
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, pigm.lab=="Canthaxanthin"), 
             aes(DOY, ymin = lwr, ymax = upr, fill = Site, group = Site), alpha = 0.25)+
  coord_cartesian(ylim=c(0,4))+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  #theme(strip.text.x=element_blank())+
  theme(legend.position = 'top')
Canth.p

pig.gammals.p <- 
  plot_grid(align="hv", Chla.p, Fuco.p, Diato.p, Allo.p, 
          Chlb.p, Lut.p, Ech.p, Canth.p, ncol=2, nrow=4)

pig.gammals.p
### Take into after effects for correct labels and legend placement

##############################################################################
# Run ANOVA on pigment concentrations in BACI to test significance

## read data - pigments with B-A group
ans <- read.csv(file.choose(), header=TRUE)

### Pivot and level 
ans.l <- ans %>%
  select(Site, Group, DOY, BA, Chla, Fuco, Diato, Allo, 
         Chlb, Lutein, Echine, Cantha) %>%
  pivot_longer(cols = -c('Site', 'Group', 'DOY', 'BA'),
               names_to = 'pigment', 
               values_to = 'conc') %>%
  filter(!is.na(conc)) %>%
  mutate(pigment.lab = case_when(pigment == 'Lutein' ~ 'Lutein-zeaxanthin',
                                 pigment == 'Echine' ~ 'Echinenone',
                                 pigment == 'Chlb' ~ 'Chlorophyll b',
                                 pigment == 'Allo' ~ 'Alloxanthin',
                                 pigment == 'Cantha' ~ 'Canthaxanthin',
                                 pigment == 'Diato' ~ 'Diatoxanthin',
                                 pigment == 'Fuco' ~ 'Fucoxanthin',
                                 pigment == 'Chla' ~ 'Chlorophyll a'),
         pigment.lab = factor(pigment.lab, levels=c('Chlorophyll a', 
                                                    'Fucoxanthin', 
                                                    'Diatoxanthin',
                                                    'Alloxanthin', 
                                                    'Chlorophyll b',
                                                    'Lutein-zeaxanthin',
                                                    'Echinenone', 
                                                    'Canthaxanthin')),
         BA = factor(BA, levels = c('Before', 'After')))

### plot boxes to check BACI differences
ggplot(ans.l) +
  facet_grid(pigment.lab ~ Group, scale='free_y')+
  aes(x = BA, y = conc) +
  geom_boxplot()+
  labs(y='pigment concentration', x=NULL)+
  theme_bw()+
  theme(strip.background = element_blank())+
  stat_anova_test(method="one_way", label="DFn",
                  label.x=1, label.y=2)
### add p values to previous plot in after effects
### Fig. 1

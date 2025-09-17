### Code for supplemental plots and analysis of reservoir data
### Figs. S4-S8 and S11, S12, and S14

### Packages
library('dplyr')   # for data wrangling 
library('tidyr')   # for data tidying
library('mgcv')    # for modeling GAMs
library('ggplot2') # for plotting
library('gratia')  # for GAM plotting and diagnostics
library('purrr')   # for mapping
library('cowplot') # for plot grid 
library('ggpubr') # for adding p values

#############################################################
### Waterchem, pigments, toxins data
dgs <- read.csv(file.choose(), header = TRUE)

### Waterchem labels
WATCHEM <- c('TDN', 'NH4', 'NOX', 'TDP', 'SRP', 'TDNSRP', 'DIC', 'DOC', 'pH', 'COND', 'Secchi')
WATCHEM.LABS <- c('TDN~μg~N~L^-1', 'NH[4]+~μg~N~L^-1', 'NO[x]-~μg~N~L^-1','TDP~μg~P~L^-1',
                  'SRP~μg~P~L^-1', 'TDN:SRP', 'DIC~mg~C~L^-1', 'DOC~mg~C~L^-1', 'pH', 'Conductivity~μS~cm^-1', 
                  'Secchi~depth~m')
### Pivot data in two steps
dgs.long <-
  select(dgs, Site, DOY, all_of(WATCHEM)) %>%
  pivot_longer(all_of(WATCHEM), names_to = 'watchem', values_to ='value') %>%
  mutate(watchem = factor(watchem, levels = sort(unique(watchem))),
         watchem.lab = purrr::map_chr(watchem,
                                      function(x) WATCHEM.LABS[WATCHEM == x])%>%
           factor(levels = WATCHEM.LABS))

dgs.new <- dgs.long %>%
  mutate(across(Site, factor, levels=c("F1", "F2", "F3", "R1", "R2", "R3")))
  
### Make a plot
dugwc.p <-
  ggplot(dgs.new, aes(DOY, value)) +
  facet_grid(watchem.lab ~ Site, scales = 'free', labeller = label_parsed, switch = 'y')+
  geom_vline(data=subset(dgs.new, Site=="F1"), aes(xintercept=200), color ="black", size =0.75, linetype=2)+
  geom_vline(data=subset(dgs.new, Site=="F2"), aes(xintercept=200), color ="black", size =0.75, linetype=2) +
  geom_vline(data=subset(dgs.new, Site=="F3"), aes(xintercept=200), color ="black", size =0.75, linetype=2) +
  geom_line(data=subset(dgs.new, watchem.lab=="TDN~μg~N~L^-1"), color="#0066CC", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="NH[4]+~μg~N~L^-1"), color="#0000FF", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="NO[x]-~μg~N~L^-1"), color="#3399CC", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="SRP~μg~P~L^-1"), color="#FF0000", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="TDP~μg~P~L^-1"), color="#990000", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="TDN:SRP"), color="#993399", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=='DIC~mg~C~L^-1'), color="#996600", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=='DOC~mg~C~L^-1'), color="#CC9900", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="pH"), color="#008000", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="Conductivity~μS~cm^-1"), color="#003300", size=0.75)+
  geom_line(data=subset(dgs.new, watchem.lab=="Secchi~depth~m"), color="#CC3300", size=0.75)+
  geom_point(na.rm=TRUE, size=1)+
  labs(x = 'Day of Year', y = NULL)+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(strip.placement = 'outside')
dugwc.p
### and save
ggsave("dugouts-waterchem.pdf", dugwc.p, dpi=300, scale=1)
### Figure S4

###############################################################################################
### Chlorophyll a GAMs 
theme_set(theme_bw())
# import data
dugouts <-
  read.csv(file.choose (), header=TRUE) %>%
  mutate(Site = factor(Site,
                       levels=c('F1', 'F2', 'F3', 'R1', 'R2', 'R3')), ### listed in order - 3 experiment, 3 reference
         # Experimental/Reference: 
         Group = factor(Group))%>% ### 
  # remove unnecessary columns
  select(Site, Group, DOY, INTCHL, SURFCHL, SICHL) %>% ## depth-integrated / surface Chl a levels
  as_tibble()

# plot all concentrations by group (experimental v reference) 
dugouts.long <- ## put in long format
  pivot_longer(dugouts,
               cols = c(INTCHL, SURFCHL, SICHL),
               names_to = 'chl', values_to = 'conc') %>%
  select(Site, Group, DOY, conc, chl) %>%
  mutate(chl = factor(chl, levels = c('INTCHL',
                                          'SURFCHL',
                                          'SICHL')))

ggplot(dugouts.long) + ## now plot
  facet_grid(chl ~ Group, scales = 'free_y') +
  geom_line(aes(DOY, conc, group = Site, color = Group),
            show.legend = FALSE) +
  geom_point(aes(DOY, conc),
             show.legend = FALSE)+
  scale_color_brewer(type = 'qual', palette = 6)
#labs(x = DOY, y = conc.lab)

# model by group
fit.m.box <- function(c) {
  d <- filter(dugouts.long, chl == c) # subset to the specific chlorophyll type, depth-integrated or surface
  
  cat('Fitting', c, 'box model...\n')
  gam(conc ~ Group +
        s(DOY, by = Group, k = 5) +     # "fixed effect" smooths
        s(DOY, Site, k = 7, bs = 'fs'), # "random effect" smooths
      family = Gamma(link = 'log'),
      data = d,
      method = 'REML')
}

gam.check

# "box" model with a location-scale distribution
fit.m.box.gammals <- function(c) {
  d <- filter(dugouts.long, chl == c) # subset the chl
  
  cat('Fitting', c, 'gammals model...\n')
  gam(list(conc ~ Group +
             s(DOY, by = Group, k = 5) +     # "fixed effect" smooths
             s(DOY, Site, k = 7, bs = 'fs'), # "random effect" smooths
           ~ s(Site, bs = 're')),
      family = gammals(),
      data = d,
      method = 'REML')
}

### Now fit the models
models <- tibble(chl = unique(dugouts.long$chl),
                 m.box = map(chl, fit.m.box),
                 m.box.gammals = map(chl, fit.m.box.gammals))
### all fit after modifying k=values

# diagnostics ####
#box
appraise(models$m.box[[1]]) # 
appraise(models$m.box[[2]]) # both skew to the left in residuals

# gammals models
## a few outliers, but much better fits!
appraise(models$m.box.gammals[[1]])
appraise(models$m.box.gammals[[2]]) #### This is the best one, I think

summary(models$m.box.gammals)
gam.check(models$m.box.gammals)
gam.check(models$m.box.gammals[[1]])
gam.check(models$m.box.gammals[[2]])

## draw models ####
# box
draw(models$m.box[[1]])
draw(models$m.box[[2]])

# box gammals
draw(models$m.box.gammals[[1]], parametric = FALSE) ### This one looks best, I think
draw(models$m.box.gammals[[2]], parametric = FALSE) 

models
# extract differences in AIC from full model
models %>%
  pivot_longer(-chl) %>%
  mutate(AIC = map_dbl(value, AIC)) %>%
  group_by(chl) %>%
  ungroup() %>%
  select(-value) %>%
  pivot_wider(names_from = name, values_from = AIC) %>%
  mutate(m.box = m.box - m.box.gammals,
         m.box.gammals = m.box.gammals - m.box.gammals)

# predictions by site ####
pred.fun <- function(c) {
  m <- models$m.box.gammals[[which(models$chl == c)]]
  predict(m, newd, type = 'link', se.fit = TRUE) %>%
    as.data.frame() %>%
    bind_cols(newd) %>%
    mutate(chl = c)
}

newd <- filter(dugouts, !duplicated(Site)) %>%
  select(Site, Group) %>%
  expand_grid(DOY = evenly(dugouts$DOY, n = 300))

pred <-
  map(models$chl, pred.fun) %>%
  bind_rows() %>%
  mutate(chl = factor(chl, levels = c('INTCHL',
                                      'SURFCHL',
                                      'SICHL'))) %>%
  mutate(chlor.lab = factor(case_when(chl == 'INTCHL' ~ 'Depth-integrated',
                                      chl == 'SURFCHL' ~ 'Surface',
                                      chl == 'SICHL' ~ 'Surface:Integrated Ratio'),
                            levels=c('Depth-integrated', 'Surface', 
                                     'Surface:Integrated Ratio')),
         chlor.gammals = interaction(chl, Group) %>%
           factor(levels = c('INTCHL.Fertilized', 'INTCHL.Reference',
                             'SURFCHL.Fertilized', 'SURFCHL.Reference',
                             'SICHL.Fertilized', 'SICHL.Reference')),
         mean = exp(fit.1),
         lwr = exp(fit.1 - 1.96 * se.fit.1),
         upr = exp(fit.1 + 1.96 * se.fit.1)) %>%
  as_tibble()

# plot the model predictions
# set up plot objects - ### Change from Year to DOY for Dugouts  
date.lab <- 'Day of Year'
conc.lab <- expression(Chlorophyll~a~concentration~(mu~g~L^{-1}))

### Plot models
ggplot(dugouts.long) +
  facet_grid(chl ~ Group, scales='free_y', labeller = label_parsed)+
  geom_vline(data=subset(dugouts.long, Group=="Fertilized"), 
             aes(xintercept=200), color ="black", size =1, linetype=2)+
  geom_point(aes(DOY, conc, fill=Site), color="black", pch=21, size=1.5)+
  geom_line(data=subset(pred, chlor.lab=="Depth-integrated"),
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_line(data=subset(pred, chlor.lab=="Surface"),
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_line(data=subset(pred, chlor.lab=="Surface:Integrated Ratio"),
            aes(DOY, mean, color=Site, group=Site), size=1)+
  geom_ribbon(data=subset(pred, chlor.lab=="Depth-integrated"),
              aes(DOY, ymin=lwr, ymax=upr, fill=Site, group=Site),
              alpha = 0.25)+
  geom_ribbon(data=subset(pred, chlor.lab=="Surface"),
              aes(DOY, ymin=lwr, ymax=upr, fill=Site, group=Site),
              alpha = 0.25)+
  geom_ribbon(data=subset(pred, chlor.lab=="Surface:Integrated Ratio"),
              aes(DOY, ymin=lwr, ymax=upr, fill=Site, group=Site),
              alpha = 0.25)+
  labs(x = date.lab, y = conc.lab)+
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank())+
  theme(strip.background.x=element_blank())+
  theme(legend.position = 'top')
#### Add correct labels in after effects

##### ANOVA analysis in BACI design
### data
ans <- read.csv(file.choose(), header=TRUE)

ans.l <- ans %>% ## Chl data with BACI desgin based on DOY
  select(Site, Group, DOY, BA, INTCHL, SURFCHL, SICHL) %>%
  pivot_longer(cols = -c('Site', 'Group', 'DOY', 'BA'),
               names_to = 'chl', 
               values_to = 'conc') %>%
  filter(!is.na(conc)) %>%
  mutate(chl.lab = case_when(chl == 'INTCHL' ~ 'integrated',
                             chl == 'SURFCHL' ~ 'surface',
                             chl == 'SICHL' ~ 'S:I ratio'),
  chl.lab = factor(chl.lab, levels=c('integrated', 
                                     'surface',
                                     'S:I ratio')),
  BA = factor(BA, levels = c('Before', 'After')))


### plot as box and get P values
ggplot(ans.l) +
  facet_grid(chl.lab ~ Group, scale='free_y')+
  aes(x = BA, y = conc) +
  geom_boxplot()+
  labs(y='chl concentration', x=NULL)+
  theme_bw()+
  theme(strip.background = element_blank())+
  stat_anova_test(method="one_way", label="DFn",
                  label.x=1, label.y=2, )
### 
### add ANOVA results to above plot
### To form complete Fig. S5

#################################################################################################################
### Benthic chlorophyll a

### read data
benth <- read.csv(file.choose(), header=TRUE)

### level
b.chl <- benth %>%
  mutate(Site = factor(Site, levels=c("F1", "F2", "F3", "R1", "R2", "R3")))

### Plot
benth.p <-
  ggplot(b.chl, aes(DOY, Chla)) +
  facet_grid(~ Site, scales = 'fixed')+
  geom_vline(data=subset(b.chl, Site=="F1"), aes(xintercept=200), color ="black", size =0.75, linetype=2)+
  geom_vline(data=subset(b.chl, Site=="F2"), aes(xintercept=200), color ="black", size =0.75, linetype=2) +
  geom_vline(data=subset(b.chl, Site=="F3"), aes(xintercept=200), color ="black", size =0.75, linetype=2) +
  geom_point(na.rm=TRUE, size=1, color="darkgreen")+
  ylim(0,1.8)+
  xlab('Day of Year')+
  ylab(expression(Chlorophyll~a~(µg~cm^-2)))+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(strip.placement = 'outside')
benth.p
##Fig. S6

##################################################################################################################
### Reservoir cyanobacteria toxins
### toxin data
tox <- read.csv(file.choose(), header = TRUE)
### labels
TOX <- c('MC', 'BMAA', 'ANAT')
TOX.LABS <- c('Microcystin~μg~L^-1', 'BMAA~μg~L^-1', 'Anatoxin~μg~L^-1')
## arrange data
tox.long <- 
  select(tox, Site, Group, BA, DOY, all_of(TOX)) %>% 
  pivot_longer(all_of(TOX), names_to = 'toxins', values_to ='conc') %>% 
  mutate(toxins = factor(toxins, levels = sort(unique(toxins))), 
         toxins.lab = purrr::map_chr(toxins,
                                     function(x) TOX.LABS[TOX == x])%>%
           factor(levels = TOX.LABS))

tox.new <- tox.long %>% 
  mutate(across(Site, factor, levels=c("F1", "F2", "F3", "R1", "R2", "R3")),
         across(toxins.lab, factor, levels=c('Microcystin~μg~L^-1', 'Anatoxin~μg~L^-1', 'BMAA~μg~L^-1')))
### Plot
dtox.p <-
  ggplot(tox.new[!is.na(tox.new$conc),], aes(DOY, conc)) +
  facet_grid(toxins.lab ~ Site, scales = 'free_y', labeller = label_parsed, switch = 'y') +
  geom_hline(data=subset(tox.new, toxins.lab=="Microcystin~μg~L^-1"), aes(yintercept=0.16), 
             color = "grey", size=0.75)+
  geom_hline(data=subset(tox.new, toxins.lab=="Anatoxin~μg~L^-1"), aes(yintercept=0.10), 
             color = "grey", size=0.75)+
  geom_hline(data=subset(tox.new, toxins.lab=="BMAA~μg~L^-1"), aes(yintercept=0.40), 
             color = "grey", size=0.75)+
  geom_vline(data=subset(tox.new, Site=="F1"), aes(xintercept=200), color ="black", size =0.67, linetype=2)+
  geom_vline(data=subset(tox.new, Site=="F2"), aes(xintercept=200), color ="black", size =0.67, linetype=2) +
  geom_vline(data=subset(tox.new, Site=="F3"), aes(xintercept=200), color ="black", size =0.67, linetype=2) +
  geom_point(size=1)+
  geom_line(data=subset(tox.new, toxins.lab=="Microcystin~μg~L^-1"), color="#339999", size=1)+
  geom_line(data=subset(tox.new, toxins.lab=="BMAA~μg~L^-1"), color="#009966", size=1)+
  geom_line(data=subset(tox.new, toxins.lab=="Anatoxin~μg~L^-1"), color="#339966", size=1)+
  labs(x = 'Day of Year', y = NULL)+
  theme_bw()+ 
  theme(strip.placement = 'outside')+ 
  theme(strip.background = element_blank())+ 
  theme(strip.text=element_text(size=10))
dtox.p
ggsave("dugouts-toxins.pdf", dtox.p, dpi=300, scale=1)

### now reduce data to just look at BMAAs and Anatoxin and check for differences
bmaa <- tox.new %>%
  filter(toxins.lab =="BMAA~μg~L^-1") %>%
  mutate(BA = factor(BA, levels = c('Before', 'After')))
### plot
bmaa.bx<-
  ggplot(bmaa) +
  facet_wrap(~ Group, scale='fixed')+
  aes(BA, conc) +
  geom_boxplot()+
  xlab(NULL)+
  ylab(expression(BMAA~μg~L^-1))+
  theme_bw()+
  theme(strip.background = element_blank())+
  stat_anova_test(method="one_way", label="n",
                  label.x=1, label.y=2)
bmaa.bx

### anatoxin now
anat <- tox.new %>%
  filter(toxins.lab =="Anatoxin~μg~L^-1") %>%
  filter(conc > 0.10) %>%
  mutate(BA = factor(BA, levels = c('Before', 'After')))

anat.bx<-
  ggplot(anat) +
  facet_wrap(~ Group, scale='fixed')+
  aes(BA, conc) +
  geom_boxplot()+
  xlab(NULL)+
  ylab(expression(Anatoxin~μg~L^-1))+
  theme_bw()+
  theme(strip.background = element_blank())+
  stat_anova_test(method="one_way", label="n",
                  label.x=1, label.y=0.3)
anat.bx

### and combine
tox.p <- plot_grid(align="v", axis="l", 
                   dtox.p, anat.bx, bmaa.bx, nrow=3, rel_heights = c(1,0.67,0.67))
tox.p
### Fig. S7

###################################################################################################################
#### Conductivity and Sulphate estimates
### data
salt <- read.csv(file.choose(), header=TRUE)

### select data and pivot
salt.long <- salt %>%
  select(Site, Group, DOY, BA, Cond.uScm, SO4.mgL_est) %>%
  pivot_longer(cols = -c('Site', 'Group', 'DOY', 'BA'),
               names_to = 'cond',
               values_to = 'value') %>%
  mutate(cond = factor(cond)) %>%
  mutate(cond.expr = case_when(cond == 'Cond.uScm' ~ 'Conductivity µS cm-1',
                               cond == 'SO4.mgL_est' ~ 'SO42- mg L-1'),
  cond.expr = factor(cond.expr, levels = c('Conductivity µS cm-1',
                                           'SO42- mg L-1')),
  Site = factor(Site, levels = c('F1', 'F2', 'F3',
                                 'R1', 'R2', 'R3')),
  BA = factor(BA, levels = c('Before', 'After')))

#### plot time series
cond.p <- 
  ggplot(salt.long, aes(DOY, value)) +
  facet_grid(cond.expr ~ Site, scales='free_y')+
  geom_vline(data=subset(salt.long, Site=="F3"), aes(xintercept=200), color ="black", size =0.5, linetype=2)+
  geom_vline(data=subset(salt.long, Site=="F1"), aes(xintercept=200), color ="black", size =0.5, linetype=2) +
  geom_vline(data=subset(salt.long, Site=="F2"), aes(xintercept=200), color ="black", size =0.5, linetype=2) +
  geom_point()+
  geom_line()+
  labs(x = 'Day of Year', y = NULL)+
  theme_bw()+
  theme(strip.background = element_blank())
cond.p
### Fig. S8

##################################################################################################################
### See other file for core analyses
##################################################################################################################
### SRP and TDP analyses
## data limited to P types and BACI design
p <- read.csv(file.choose(), header=TRUE)
### pivot and level
p.l <- p %>%
  select(Site, Group, BA, DOY, TDP, SRP) %>%
  pivot_longer(cols = -c('Site', 'Group', 'DOY', 'BA'),
               names_to = 'p', 
               values_to = 'conc') %>%
  filter(!is.na(conc)) %>%
  mutate(p.lab = case_when(p == 'TDP' ~ 'Total dissolved phosphorus',
                           p == 'SRP' ~ 'Soluble reactive phosphorus'),
         p.lab = factor(p.lab, levels=c('Soluble  reactive phosphorus', 
                                        'Total dissolved phosphorus')),
         BA = factor(BA, levels = c('Before', 'After')))
### plot in boxplot with p value
ggplot(p.l) +
  facet_grid(p ~ Group, scale='free_y')+
  aes(x = BA, y = conc) +
  geom_boxplot()+
  ylab(expression(Concentration~µg~P~L^-1))+
  xlab(NULL)+
  theme_bw()+
  theme(strip.background = element_blank())+
  stat_anova_test(method="one_way", label="DFn",
                  label.x=1, label.y=1500)
### Fig. S11

###########################################################################################################################
### SRP and Chla relationship
### data
dat <- read.csv(file.choose(), header = TRUE)
## select proper variables
dat.c <- dat %>%
  select(Site, Group, DOY, SRP, INTCHL)
## Plot, simply
ggscatter(dat.c, x = "SRP", y = "INTCHL") +
  facet_wrap(~Group, scale='free_x')+
  stat_cor()+
  ylab(expression(Chlorophyll~a~µg~L^-1))+
  xlab(expression(Soluble~reactive~phosphorus~µg~P~L^-1))+
  theme_bw()+
  theme(strip.background = element_blank())
### add ANOVA
stat_anova_test(method="one_way", label="p",
                label.x=1, label.y=1500)
### Fig. S12
###########################################################################################################################
### See mass balance sheet for Fig. S13
##########################################################################################################################
### Nutrient Assay plots
### nutrient limitation data 
nutlim <-
  read.csv(file.choose (), header=TRUE) %>%
  select(Site, Group, DOY, n.c.ratio, np.c.ratio, p.c.ratio) %>% ## treatment and control ratios
  as_tibble()

### pivot data
nlim.long <- nutlim %>% 
  pivot_longer(cols = -c('Site', 'Group', 'DOY'),
               names_to = 'treat', 
               values_to = 'ratio') %>%
  filter(!is.na(ratio)) %>%
  mutate(treat.lab = case_when(treat == 'n.c.ratio' ~ '+N',
                               treat == 'np.c.ratio' ~ '+NP',
                               treat == 'p.c.ratio' ~ '+P'),
         treat.lab = factor(treat.lab, levels=c('+N', 
                                                '+NP',
                                                '+P')),
         DOY = factor(DOY))
### Plot first boxplots
nlim.bxf<-
  ggplot(nlim.long) +
  facet_grid(treat.lab ~ Group, scale='free_y')+
  aes(DOY, ratio) +
  geom_vline(data=subset(nlim.long, Group=="F"), aes(xintercept= 6), 
             color ="black", size =0.67, linetype=2)+
  geom_boxplot(aes(color=Site, group=NULL))+
  labs(y='Treatment:control ratio', x='Day of Year')+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(angle=45))
nlim.bxf
### and second row
nlim.bxc<-
  ggplot(nlim.long) +
  facet_grid(treat.lab ~ Group, scale='free_y')+
  aes(DOY, ratio) +
  geom_vline(data=subset(nlim.long, Group=="F"), aes(xintercept= 6), 
             color ="black", size =0.67, linetype=2)+
  geom_boxplot(aes(color=Site, group=NULL))+
  ylim(0,11)+
  labs(y='Treatment:control ratio', x='Day of Year')+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(angle=45))
nlim.bxc
### plot together
nlimbox <- plot_grid(align='hv', nlim.bxf, nlim.bxc, nrow=2)
nlimbox
#### Fig. S14
##################################################################################################




#### Plotting N and P mass balances
### Fig. 2 and S13

#### Packages
library(ggplot2)
library(tidyr)
library(dplyr)
library (cowplot)

## Nitrogen mass data including sum of added urea
dat <- read.csv(file.choose(), header=TRUE)

### N species
Nitrogen <- c('NH4_g', 'NO3.NO2_g', 'DON_g', 'PON_g')

### Pivot data
n.long <-
  select(dat, Site, DOY, Added_urea_sum, all_of(Nitrogen)) %>%
  pivot_longer(all_of(Nitrogen), names_to = 'Nitrogen', values_to ='mass')

### Plot individual reservoirs
F1u.p <- ggplot(data=subset(n.long, Site=="F1"), aes(fill=Nitrogen, y=mass, x=DOY))+
  geom_vline(data=n.long, aes(xintercept=200), color ="black", size =0.5, linetype=2)+ 
  geom_bar(position="stack", stat="identity")+ ### measured TN pools+
  scale_fill_manual(values=c('#CC9900', '#0000CC','#660099', '#009933'))+ 
  geom_point(aes(x=DOY, y=Added_urea_sum/5), pch=21, color="black", fill="#CC0000", size=1)+
  geom_line(aes(x=DOY, y=Added_urea_sum/5), linetype=1, color="#CC0000", size=1)+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*4, name="Sum of added urea (g)"))+
  xlab("Day of Year")+
  ylab(expression(Nitrogen~mass~(g~reservoir^-1)))+
  ggtitle('F1')+
  theme_bw()
F1u.p

F2u.p <- ggplot(data=subset(n.long, Site=="F2"), aes(fill=Nitrogen, y=mass, x=DOY))+
  geom_vline(data=n.long, aes(xintercept=200), color ="black", size =0.5, linetype=2)+ 
  geom_bar(position="stack", stat="identity")+ ### measured TN pools+
  scale_fill_manual(values=c('#CC9900', '#0000CC','#660099', '#009933'))+ 
  geom_point(aes(x=DOY, y=Added_urea_sum/4), pch=21, color="black", fill="#CC0000", size=1)+
  geom_line(aes(x=DOY, y=Added_urea_sum/4), linetype=1, color="#CC0000", size=1)+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*4, name="Sum of added urea (g)"))+
  xlab("Day of Year")+
  ylab(expression(Nitrogen~mass~(g~reservoir^-1)))+
  ggtitle('F2')+
  #ylim(0,22000)+
  theme_bw()
F2u.p

F3u.p <- ggplot(data=subset(n.long, Site=="F3"), aes(fill=Nitrogen, y=mass, x=DOY))+
  geom_vline(data=n.long, aes(xintercept=200), color ="black", size =0.5, linetype=2)+ 
  geom_bar(position="stack", stat="identity")+ ### measured TN pools+
  scale_fill_manual(values=c('#CC9900', '#0000CC','#660099', '#009933'))+ 
  geom_point(aes(x=DOY, y=Added_urea_sum/5), pch=21, color="black", fill="#CC0000", size=1)+
  geom_line(aes(x=DOY, y=Added_urea_sum/5), linetype=1, color="#CC0000", size=1)+
  scale_y_continuous(sec.axis = sec_axis(transform=~.*4, name="Sum of added urea (g)"))+
  xlab("Day of Year")+
  ylab(expression(Nitrogen~mass~(g~reservoir^-1)))+
  ggtitle('F3')+
  #ylim(0,22000)+
  xlim(148,250)+
  theme_bw()
F3u.p

R1.p<- ggplot(data=subset(n.long, Site=="R1"), aes(fill=Nitrogen, y=mass, x=DOY))+
  geom_bar(position="stack", stat="identity")+ ### measured TN pools+
  scale_fill_manual(values=c('#CC9900', '#0000CC','#660099', '#009933'))+ 
  labs(title="R1", x="Day of Year", y="Mass (mg)")+
  theme_bw()
R1.p

R2.p <- ggplot(data=subset(n.long, Site=="R2"), aes(fill=Nitrogen, y=mass, x=DOY))+
  geom_bar(position="stack", stat="identity")+ ### measured TN pools+
  scale_fill_manual(values=c('#CC9900', '#0000CC','#660099', '#009933'))+ 
  labs(title="R2", x="Day of Year", y="Mass (mg)")+
  xlim(148,250)+
  theme_bw()
R2.p

R3.p <- ggplot(data=subset(n.long, Site=="R3"), aes(fill=Nitrogen, y=mass, x=DOY))+
  geom_bar(position="stack", stat="identity")+ ### measured TN pools+
  scale_fill_manual(values=c('#CC9900', '#0000CC','#660099', '#009933'))+ 
  labs(title="R3", x="Day of Year", y="Mass (mg)")+
  theme_bw()
R3.p

plot_grid(align='hv', F1u.p,  R1.p, F2u.p, R2.p, 
          F3u.p, R3.p, ncol=2, nrow=3)
#### After effects
### Fig. 2 

#######################################################################################################
### P masses. 
## p mass data
pmass <- read.csv(file.choose(), header=TRUE)
## P species
Phosphorus <- c('SRP_g', 'DOP_g', 'POP_g')
## Data manipulation
p.long <-
  select(pmass, Site, DOY, all_of(Phosphorus)) %>%
  pivot_longer(all_of(Phosphorus), names_to = 'Phosphorus', values_to ='mass')

#Plot by site as well
F1P <- ggplot(data=subset(p.long, Site=="F1"), aes(fill=Phosphorus, y=mass, x=DOY))+
  geom_vline(data=subset(p.long, Site=="F1"), aes(xintercept=200), color ="black", size =0.5, linetype=2) +
  geom_bar(position="stack", stat="identity")+ ### measured TP pools+
  scale_fill_manual(values=c('#CC9900', '#009933', '#0000CC'))+ 
  labs(title="F1", x="Day of Year", y="Phosphorus mass (g reservoir -1)")+
  theme_bw() 
F1P

F2P <- ggplot(data=subset(p.long, Site=="F2"), aes(fill=Phosphorus, y=mass, x=DOY))+
  geom_vline(data=subset(p.long, Site=="F2"), aes(xintercept=200), color ="black", size =0.5, linetype=2) +
  geom_bar(position="stack", stat="identity")+ ### measured TP pools+
  scale_fill_manual(values=c('#CC9900', '#009933', '#0000CC'))+ 
  labs(title="F2", x="Day of Year", y="Phosphorus mass (g reservoir -1)")+
  theme_bw() 
F2P

F3P <- ggplot(data=subset(p.long, Site=="F3"), aes(fill=Phosphorus, y=mass, x=DOY))+
  geom_vline(data=subset(p.long, Site=="F3"), aes(xintercept=200), color ="black", size =0.5, linetype=2) +
  geom_bar(position="stack", stat="identity")+ ### measured TP pools+
  scale_fill_manual(values=c('#CC9900', '#009933', '#0000CC'))+ 
  labs(title="F3", x="Day of Year", y="Phosphorus mass (g reservoir -1)")+
  xlim(148,250)+
  theme_bw() 
F3P

R1P <- ggplot(data=subset(p.long, Site=="R1"), aes(fill=Phosphorus, y=mass, x=DOY))+
  geom_bar(position="stack", stat="identity")+ ### measured TP pools+
  scale_fill_manual(values=c('#CC9900', '#009933', '#0000CC'))+ 
  labs(title="R1", x="Day of Year", y="Phosphorus mass (g reservoir -1)")+
  theme_bw() 
R1P

R2P <- ggplot(data=subset(p.long, Site=="R2"), aes(fill=Phosphorus, y=mass, x=DOY))+
  geom_bar(position="stack", stat="identity")+ ### measured TP pools+
  scale_fill_manual(values=c('#CC9900', '#009933', '#0000CC'))+ 
  labs(title="R2", x="Day of Year", y="Phosphorus mass (g reservoir -1)")+
  xlim(148,250)+
  theme_bw() 
R2P

R3P <- ggplot(data=subset(p.long, Site=="R3"), aes(fill=Phosphorus, y=mass, x=DOY))+
  geom_bar(position="stack", stat="identity")+ ### measured TP pools+
  scale_fill_manual(values=c('#CC9900', '#009933', '#0000CC'))+ 
  labs(title="R3", x="Day of Year", y="Phosphorus mass (g reservoir -1)")+
  theme_bw() 
R3P

plot_grid(align="hv", F1P, R1P, F2P, R2P, F3P, R3P, ncol=2, nrow=3)
### Fig. S13
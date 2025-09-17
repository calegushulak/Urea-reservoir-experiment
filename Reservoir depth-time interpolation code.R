### Depth-time interpolation of temperature and oxygen
##3 Packages
library(tidyverse)
library(lubridate)
library(cowplot)

### F1
can <- read.csv(file.choose(), header=TRUE, as.is=TRUE)

date <- as.Date(can$date)
can <- cbind(can, date)
can <- subset(can, select = -3)

### temp dots
ggplot(can, aes(x= date, y= depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=18,
    high = scales::muted("red"),
    low = scales::muted("yellow"))

### interpolate by depth
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- can %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$temp, xout = target_depth)$y}

estimate_temp_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of temps
can_temp_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(can$date)),
  # depths can now be any value
  tibble(depth = seq(0, 2.2, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date(date[1], depth))

## check first interp
ggplot(can_temp_interp_depth,aes(x=date, y=depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient(
    high = ("red"),
    low = ("yellow"))

## interpolate dates
estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- can_temp_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y}


### create raster
can_temp_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by=1)),
  tibble(depth = unique(can_temp_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(depth[1], date))


### plot
cantemp<-
  ggplot(can_temp_raster, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(
    high = ("red"),
    low = ("yellow"),
    limits=range(10.7,30.3),
    name = "Temperature (°C)")+
  coord_cartesian(expand = FALSE)+
  labs(x = NULL, y = "Depth (m)")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("F1")
cantemp

### Dissolved oxygen
ggplot(can, aes(x= date, y= depth, color=do))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5.5,
    high = scales::muted("blue"),
    low = scales::muted("red"))

### interpolate by depth
estimate_ox_by_date <- function(target_date, target_depth) {
  data_for_date <- can %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$do, xout = target_depth)$y}

estimate_ox_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of DO
can_ox_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(can$date)),
  # depths can now be any value
  tibble(depth = seq(0, 2.2, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ox = estimate_ox_by_date(date[1], depth))

## check first interp
ggplot(can_ox_interp_depth, aes(x=date, y=depth, color= ox))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=10,
    high = ("blue"),
    low = ("red"))

## interpolate dates
estimate_ox_by_depth <- function(target_depth, target_date) {
  data_for_depth <- can_ox_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$ox, xout = target_date)$y
  }

### create raster
can_oxy_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by = 1)),
  tibble(depth = unique(can_ox_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(ox = estimate_ox_by_depth(depth[1], date))


### plot
canox<-
  ggplot(can_oxy_raster, aes(date, depth, fill = ox)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"),
    limits=range(0,25.5),
    name="DO (mg/L)")+
  coord_cartesian(expand = FALSE)+
  xlab(NULL)+
  ylab("Depth (m)")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("F1")
canox

################################################################################################################### 
## R1
cow <- read.csv(file.choose(), header=TRUE, as.is=TRUE)

date <- as.Date(cow$date)
date
cow <- cbind(cow, date)
cow <- subset(cow, select = -3)

### temp dots
ggplot(cow, aes(x= date, y= depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=18,
    high = scales::muted("red"),
    low = scales::muted("yellow"))

### interpolate by depth
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- cow %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$temp, xout = target_depth)$y}

estimate_temp_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of temps
cow_temp_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(cow$date)),
  # depths can now be any value
  tibble(depth = seq(0, 2, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date(date[1], depth))

## check first interp
ggplot(cow_temp_interp_depth,aes(x=date, y=depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient(
    high = ("red"),
    low = ("yellow"))

## interpolate dates
estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- cow_temp_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y}

### create raster
cow_temp_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by = 1)),
  tibble(depth = unique(cow_temp_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(depth[1], date))

### plot
cowtemp<-
  ggplot(cow_temp_raster, aes(date, depth, fill = temp))+
  geom_raster()+ 
  scale_y_reverse() +
  scale_fill_gradient(
    high = ("red"),
    low = ("yellow"),
    limits=range(10.7,30.3),
    name = "Temperature (°C)")+
  coord_cartesian(expand = FALSE)+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("R1")
cowtemp

### dissolved oxygen
ggplot(cow, aes(x= date, y= depth, color=DO))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = scales::muted("blue"),
    low = scales::muted("red"))

### interpolate by depth
estimate_ox_by_date <- function(target_date, target_depth) {
  data_for_date <- cow %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$DO, xout = target_depth)$y}

estimate_ox_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of DO
cow_ox_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(cow$date)),
  # depths can now be any value
  tibble(depth = seq(0, 2, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ox = estimate_ox_by_date(date[1], depth))

## check first interp
ggplot(cow_ox_interp_depth, aes(x=date, y=depth, color= ox))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"))

## interpolate dates
estimate_ox_by_depth <- function(target_depth, target_date) {
  data_for_depth <- cow_ox_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$ox, xout = target_date)$y
}

### create raster
cow_oxy_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by = 1)),
  tibble(depth = unique(cow_ox_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(ox = estimate_ox_by_depth(depth[1], date))

### plot
cowox<-
  ggplot(cow_oxy_raster, aes(date, depth, fill = ox)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"),
    limits=range(0,25.5),
    name="DO (mg/L)")+
  coord_cartesian(expand = FALSE)+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("R1")
cowox

#####################################################################################################################
#F2
oat <- read.csv(file.choose(), header=TRUE, as.is=TRUE)

date <- as.Date(oat$date)
date
oat <- cbind(oat, date)
oat <- subset(oat, select = -2)

### temp dots
ggplot(oat, aes(x= date, y= depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=18,
    high = scales::muted("red"),
    low = scales::muted("yellow"))

### interpolate by depth
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- oat %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$temp, xout = target_depth)$y}

estimate_temp_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of temps
temp_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(oat$date)),
  # depths can now be any value
  tibble(depth = seq(0, 2.5, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date(date[1], depth))

## check first interp
ggplot(temp_interp_depth,aes(x=date, y=depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient(
    high = ("red"),
    low = ("yellow"))

## interpolate dates
estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y}

### create raster
temp_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by = 1)),
  tibble(depth = unique(temp_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(depth[1], date))

### plot
oattemp<-
  ggplot(temp_raster, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(
    high = ("red"),
    low = ("yellow"),
    limits=range(10.7,30.3),
    name = "Temperature (°C)")+
  coord_cartesian(expand = FALSE)+
  xlab(NULL)+
  ylab("Depth (m)")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("F2")
oattemp
cantemp
#### 

### Dissolved oxygen
ggplot(oat, aes(x= date, y= depth, color=do))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = scales::muted("blue"),
    low = scales::muted("red"))

### interpolate by depth
estimate_ox_by_date <- function(target_date, target_depth) {
  data_for_date <- oat %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$do, xout = target_depth)$y}

estimate_ox_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of DO
ox_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(oat$date)),
  # depths can now be any value
  tibble(depth = seq(0, 2.5, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ox = estimate_ox_by_date(date[1], depth))

## check first interp
ggplot(ox_interp_depth, aes(x=date, y=depth, color= ox))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"))

## interpolate dates
estimate_ox_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ox_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$ox, xout = target_date)$y
}

### create raster
oxy_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by = 1)),
  tibble(depth = unique(ox_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(ox = estimate_ox_by_depth(depth[1], date))


### plot
oatox<-
  ggplot(oxy_raster, aes(date, depth, fill = ox)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"),
    limits=range(0,25.5),
    name="DO (mg/L)")+
  coord_cartesian(expand = FALSE)+
  xlab(NULL)+
  ylab("Depth (m)")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle ("F2")
oatox

################################################################################################################## 
## R2
pri <- read.csv(file.choose(), header=TRUE, as.is=TRUE)

date <- as.Date(pri$date)
date
pri <- cbind(pri, date)
pri <- subset(pri, select = -3)

### temp dots
ggplot(pri, aes(x= date, y= depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=18,
    high = scales::muted("red"),
    low = scales::muted("yellow"))

### interpolate by depth
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- pri %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$temp, xout = target_depth)$y}

estimate_temp_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of temps
temp_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(pri$date)),
  # depths can now be any value
  tibble(depth = seq(0, 0.75, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date(date[1], depth))

## check first interp
ggplot(temp_interp_depth,aes(x=date, y=depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient(
    high = ("red"),
    low = ("yellow"))

## interpolate dates
estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y}

### create raster
temp_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-08-26"), by = 1)),
  tibble(depth = unique(temp_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(depth[1], date))

### plot
pritemp<-
  ggplot(temp_raster, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(
    high = ("red"),
    low = ("yellow"),
    limits=range(10.7,30.3),
    name = "Temperature (°C)")+
  coord_cartesian(expand = FALSE)+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  ggtitle("R2")
pritemp

### Dissolved oxygen
ggplot(pri, aes(x= date, y= depth, color=do))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = scales::muted("blue"),
    low = scales::muted("red"))

### interpolate by depth
estimate_ox_by_date <- function(target_date, target_depth) {
  data_for_date <- pri %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$do, xout = target_depth)$y}

estimate_ox_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of DO
ox_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(pri$date)),
  # depths can now be any value
  tibble(depth = seq(0, 0.75, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ox = estimate_ox_by_date(date[1], depth))

## check first interp
ggplot(ox_interp_depth, aes(x=date, y=depth, color= ox))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"))

## interpolate dates
estimate_ox_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ox_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$ox, xout = target_date)$y
}

### create raster
oxy_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-08-26"), by = 1)),
  tibble(depth = unique(ox_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(ox = estimate_ox_by_depth(depth[1], date))

### plot
priox<-
  ggplot(oxy_raster, aes(date, depth, fill = ox)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"),
    limits=range(0,25.5),
    name="DO (mg/L)")+
  coord_cartesian(expand = FALSE)+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  ggtitle("R2")
priox

############################################################################################################## 
## F3
rav <- read.csv(file.choose(), header=TRUE, as.is=TRUE)

date <- as.Date(rav$date)
date
rav <- cbind(rav, date)
rav <- subset(rav, select = -3)

### temp dots
ggplot(rav, aes(x= date, y= depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=18,
    high = scales::muted("red"),
    low = scales::muted("yellow"))

### interpolate by depth
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- rav %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$temp, xout = target_depth)$y}

estimate_temp_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of temps
temp_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(rav$date)),
  # depths can now be any value
  tibble(depth = seq(0, 1.25, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date(date[1], depth))

## check first interp
ggplot(temp_interp_depth,aes(x=date, y=depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient(
    high = ("red"),
    low = ("yellow"))

## interpolate dates
estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y}

### create raster
temp_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-08-26"), by = 1)),
  tibble(depth = unique(temp_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(depth[1], date))

### plot
ravtemp<-
  ggplot(temp_raster, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(
    high = ("red"),
    low = ("yellow"),
    limits=range(10.7,30.3),
    name = "Temperature (°C)")+
  coord_cartesian(expand = FALSE)+
  xlab("Date")+
  ylab("Depth (m)")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("F3")
ravtemp

#### Dissolved oxygen 
ggplot(rav, aes(x= date, y= depth, color=do))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = scales::muted("blue"),
    low = scales::muted("red"))

### interpolate by depth
estimate_ox_by_date <- function(target_date, target_depth) {
  data_for_date <- rav %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$do, xout = target_depth)$y}

estimate_ox_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of DO
ox_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(rav$date)),
  # depths can now be any value
  tibble(depth = seq(0, 1.25, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ox = estimate_ox_by_date(date[1], depth))

## check first interp
ggplot(ox_interp_depth, aes(x=date, y=depth, color= ox))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"))

## interpolate dates
estimate_ox_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ox_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$ox, xout = target_date)$y
}

### create raster
oxy_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-08-26"), by = 1)),
  tibble(depth = unique(ox_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(ox = estimate_ox_by_depth(depth[1], date))

### plot
ravox<-
  ggplot(oxy_raster, aes(date, depth, fill = ox)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"),
    limits=range(0,25.5),
    name="DO (mg/L)")+
  coord_cartesian(expand = FALSE)+
  xlab("Date")+
  ylab("Depth (m)")+
  theme_bw()+
  theme(legend.position = 'none')+
  ggtitle("F3")
ravox

##########################################################################################################
##F3
swim <- read.csv(file.choose(), header=TRUE, as.is=TRUE)

date <- as.Date(swim$date)
date
swim <- cbind(swim, date)
swim <- subset(swim, select = -3)

### temp dots
ggplot(swim, aes(x= date, y= depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=18,
    high = scales::muted("red"),
    low = scales::muted("yellow"))

### interpolate by depth
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- swim %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$temp, xout = target_depth)$y}

estimate_temp_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of temps
temp_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(swim$date)),
  # depths can now be any value
  tibble(depth = seq(0, 1.5, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(temp = estimate_temp_by_date(date[1], depth))

## check first interp
ggplot(temp_interp_depth,aes(x=date, y=depth, color=temp))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient(
    high = ("red"),
    low = ("yellow"))

## interpolate dates
estimate_temp_by_depth <- function(target_depth, target_date) {
  data_for_depth <- temp_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$temp, xout = target_date)$y}

### create raster
temp_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by = 1)),
  tibble(depth = unique(temp_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(temp = estimate_temp_by_depth(depth[1], date))

### plot
swimtemp<-
  ggplot(temp_raster, aes(date, depth, fill = temp)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(
    high = ("red"),
    low = ("yellow"),
    limits=range(10.7,30.3),
    name = "Temperature (°C)")+
  coord_cartesian(expand = FALSE)+
  xlab("Date")+
  ylab(NULL)+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("R3")
swimtemp

#### Dissolved oxygen 
ggplot(swim, aes(x= date, y= depth, color=do))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = scales::muted("blue"),
    low = scales::muted("red"))

### interpolate by depth
estimate_ox_by_date <- function(target_date, target_depth) {
  data_for_date <- swim %>% 
    filter(date == target_date) %>%
    arrange(depth)
  # approx() is one way to do a linear interpolation
  approx(data_for_date$depth, data_for_date$do, xout = target_depth)$y}

estimate_ox_by_date(ymd("2021-06-01"), c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.5))

## make tibble of DO
ox_interp_depth <- crossing(
  # the same dates as can
  tibble(date = unique(swim$date)),
  # depths can now be any value
  tibble(depth = seq(0, 1.5, length.out = 100))
) %>%
  group_by(date) %>%
  mutate(ox = estimate_ox_by_date(date[1], depth))

## check first interp
ggplot(ox_interp_depth, aes(x=date, y=depth, color= ox))+
  geom_point()+
  scale_y_reverse()+
  scale_color_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"))

## interpolate dates
estimate_ox_by_depth <- function(target_depth, target_date) {
  data_for_depth <- ox_interp_depth %>%
    filter(depth == target_depth) %>%
    arrange(date)
  
  approx(data_for_depth$date, data_for_depth$ox, xout = target_date)$y
}

### create raster
oxy_raster <- crossing(
  tibble(date = seq(ymd("2021-06-01"), ymd("2021-09-02"), by = 1)),
  tibble(depth = unique(ox_interp_depth$depth))) %>%
  group_by(depth) %>%
  mutate(ox = estimate_ox_by_depth(depth[1], date))

### plot
swimox<-
  ggplot(oxy_raster, aes(date, depth, fill = ox)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint=5,
    high = ("blue"),
    low = ("red"),
    limits=range(0,25.5),
    name="DO (mg/L)")+
  coord_cartesian(expand = FALSE)+
  xlab("Date")+
  ylab(NULL)+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("R3")
swimox

########################################################################################################################
###### Putting the plots together here
temp_depth <- plot_grid(align='hv', cantemp, cowtemp, oattemp, pritemp,
                        ravtemp, swimtemp, ncol=2, nrow=3)
temp_depth
### Fig. S2

ox_depth <- plot_grid(align='hv', canox, cowox, oatox, priox,
                      ravox, swimox, ncol=2, nrow=3)
ox_depth
### Fig. S3
  
setwd("/Users/meggie/Documents/23 Spring/482/capstone2023")
wf = read.csv("cleanedWildfireData.csv")
View(wf)

library(dplyr)
library(ggplot2)
library(maps)
library(fields)
library(ggmap)
require(gridExtra)

dim(wf)

# looking at different classes of fire sizes
fsize2 = wf%>%filter(fire_size_class == 2)

table(wf$fire_size_class)
hist(fsize2$fire_size)

ggplot(data = wf) + geom_histogram(aes(x = fire_size))

par(mfrow = c(3,2))
for (i in 2:6) {
  wf.filt = wf%>%filter(fire_size_class == i)
  
  title = paste("Histogram of Fire Sizes for Class", i)
  xname = paste("Fire Size (acres)")
  hist(wf.filt$fire_size, main = title, xlab = xname)
  #ggplot(data = wf.filt) + geom_histogram(aes(x = fire_size))
}

dev.off()

# creating a map by size
map_stamen = get_stamenmap(bbox=c('left'=-123, 'bottom'=24, 
                                  'right'=-66,'top'=50),
                           zoom=5,
                           maptype="terrain",
                           color="color")
ggmap(map_stamen) + geom_point(aes(x = longitude, y = latitude, color=fire_size_class), data = wf)+
  labs(title="Fire Size",x ="longitude", y = "latitude") +
  guides(color=guide_legend(title="Fire Size Class"))

# 

dev.off()


# relationship between temp and fire size

temp30 = ggplot(data = wf) + geom_point(aes(x = Temp_pre_30, y = fire_size, alpha = 0.3))
temp15 = ggplot(data = wf) + geom_point(aes(x = Temp_pre_15, y = fire_size, alpha = 0.3))
temp7 = ggplot(data = wf) + geom_point(aes(x = Temp_pre_7, y = fire_size, alpha = 0.3))
temp0 = ggplot(data = wf) + geom_point(aes(x = Temp_cont, y = fire_size, alpha = 0.3))

grid.arrange(temp30, temp15, temp7, temp0, ncol=2)

wind30 = ggplot(data = wf) + geom_point(aes(x = Wind_pre_30, y = fire_size, alpha = 0.3))+ xlim(c(0,30))
wind15 = ggplot(data = wf) + geom_point(aes(x = Wind_pre_15, y = fire_size, alpha = 0.3))+ xlim(c(0,30))
wind7 = ggplot(data = wf) + geom_point(aes(x = Wind_pre_7, y = fire_size, alpha = 0.3)) + xlim(c(0,30))
wind0 = ggplot(data = wf) + geom_point(aes(x = Wind_cont, y = fire_size, alpha = 0.3)) + xlim(c(0,30))

grid.arrange(wind30, wind15, wind7, wind0, ncol = 2)

summary(wf$Wind_pre_30)
var(wf$Wind_pre_30)
summary(wf$Wind_cont)
var(wf$Wind_cont)

# removing outliers
Wind_pre_30_NO = wf$Wind_pre_30[-which(wf$Wind_pre_30 == max(wf$Wind_pre_30))]
Wind_pre_15_NO = wf$Wind_pre_15[-which(wf$Wind_pre_15 == max(wf$Wind_pre_15))]
Wind_pre_7_NO = wf$Wind_pre_7[-which(wf$Wind_pre_7 == max(wf$Wind_pre_7))]
Wind_cont_NO = wf$Wind_cont[-which(wf$Wind_pre_7 == max(wf$Wind_pre_7))]
firesize_NO = wf$fire_size[-which(wf$Wind_pre_7 == max(wf$Wind_pre_7))]

wf_NO = data.frame(Wind_pre_30_NO, Wind_pre_15_NO, Wind_pre_7_NO, Wind_cont_NO, firesize_NO)



wind30_NO = ggplot(data = wf_NO) + geom_point(aes(x = Wind_pre_30_NO, y = firesize_NO, alpha = 0.3))+ ylab("Fire Size") + xlab("Wind_pre_30")
wind15_NO = ggplot(data = wf_NO) + geom_point(aes(x = Wind_pre_15_NO, y = firesize_NO, alpha = 0.3))+ ylab("Fire Size") + xlab("Wind_pre_15")
wind7_NO = ggplot(data = wf_NO) + geom_point(aes(x = Wind_pre_7_NO, y = firesize_NO, alpha = 0.3))+ ylab("Fire Size") + xlab("Wind_pre_7")
wind0_NO = ggplot(data = wf_NO) + geom_point(aes(x = Wind_cont_NO, y = firesize_NO, alpha = 0.3)) + ylab("Fire Size") + xlab("Wind_cont")
grid.arrange(wind30_NO, wind15_NO, wind7_NO, wind0_NO, ncol = 2)


boxplot(Wind_pre_30_NO, Wind_pre_15_NO, Wind_pre_7_NO, wf$Wind_cont)

# relationship between humidity and fire size

hum30 = ggplot(data = wf) + geom_point(aes(x = Hum_pre_30, y = fire_size, alpha = 0.3))
hum15 = ggplot(data = wf) + geom_point(aes(x = Hum_pre_15, y = fire_size, alpha = 0.3))
hum7 = ggplot(data = wf) + geom_point(aes(x = Hum_pre_7, y = fire_size, alpha = 0.3))
hum0 = ggplot(data = wf) + geom_point(aes(x = Hum_cont, y = fire_size, alpha = 0.3))

grid.arrange(hum30, hum15, hum7, hum0, ncol = 2)

# relationship between precipitation and fire size

prec30 = ggplot(data = wf) + geom_point(aes(x = Prec_pre_30, y = fire_size, alpha = 0.3))
prec15 = ggplot(data = wf) + geom_point(aes(x = Prec_pre_15, y = fire_size, alpha = 0.3))
prec7 = ggplot(data = wf) + geom_point(aes(x = Prec_pre_7, y = fire_size, alpha = 0.3))
prec0 = ggplot(data = wf) + geom_point(aes(x = Prec_cont, y = fire_size, alpha = 0.3)) 

grid.arrange(prec30, prec15, prec7, prec0, ncol = 2)



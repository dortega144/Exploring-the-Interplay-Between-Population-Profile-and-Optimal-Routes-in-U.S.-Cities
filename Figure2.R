library(ggplot2)
library(ggpubr)
library(ggtext)
######################################## load data
citiesfin <- read.csv("dat_cit.csv")

######################################## 
lSD <- log10(citiesfin$sum.dis.)  
lNC <- log10(citiesfin$pop)
label1 <- as.character(citiesfin$NAME)
sample_data <- data.frame(lSD, lNC, label1)
m=lm(lSD~lNC)
summary(m)
# add text with geom_text
td<-ggplot(sample_data, aes(x=lNC, y=lSD)) +
  geom_point(color="deepskyblue",size=3,alpha=0.5) +
  geom_smooth(method = "lm", linewidth=2,alpha=0.5,se=FALSE, color="deepskyblue4", formula = y ~ x) +
  labs(y ="log<sub>10</sub> Total travel distance (km)", x = "log<sub>10</sub> Population")+
  theme_classic(base_size=11)+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

######################################################
lSD <- log10(citiesfin$sum.teo_dis./1000)
lNC <- log10(citiesfin$pop)
label1 <- as.character(citiesfin$NAME)
sample_data <- data.frame(lSD, lNC, label1)
m=lm(lSD~lNC)
summary(m)
# add text with geom_text
ed<-ggplot(sample_data, aes(x=lNC, y=lSD)) +
  geom_point(color="olivedrab3",size=3,alpha=0.7) +
  geom_smooth(method = "lm", linewidth=2,alpha=0.5,se=FALSE, color="chartreuse4", formula = y ~ x) +
  labs(y ="log<sub>10</sub> Total Euclidean distance (km)", x = "log<sub>10</sub> Population")+
  theme_classic(base_size=11)+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )
###################################################################
lSD <- log10(citiesfin$sum.times.)
lNC <- log10(citiesfin$pop)
label1 <- as.character(citiesfin$NAME)
sample_data <- data.frame(lSD, lNC, label1)
m=lm(lSD~lNC)
summary(m)
# add text with geom_text
tt<-ggplot(sample_data, aes(x=lNC, y=lSD)) +
  geom_point(color="darkorange",alpha=0.5,size=3) +
  geom_smooth(method = "lm", linewidth=2,alpha=0.5,se=FALSE, color="darkorange4", formula = y ~ x) +
  labs(x ="log<sub>10</sub> Population", y = "log<sub>10</sub> Total travel time (h)")+
  theme_classic(base_size=11)+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

################################## letters in panel
ggarrange(ed,td,tt,labels=c("A","B","C"),ncol=3,nrow=1)





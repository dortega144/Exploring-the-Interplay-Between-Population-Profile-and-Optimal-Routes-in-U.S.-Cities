library(ggplot2)
library(ggpubr)
library(ggtext)
library(ggExtra)
# The dat_EM file data can be obtained using city_propierties.py for each city
# Then, use the EM classifier (Weka), using the total length in kilometers
sal_FIN <- read.csv("dat_EM.csv")
df<-sal_FIN
# Vector of colors
cols <- c("deepskyblue4", "chartreuse4", "darkorange")
#pinta EM
gg<-ggplot(df, aes(x = log10(STREET_LEN/1000), y = H0, color = Cluster,shape=Cluster)) +
  geom_point(size=3,alpha=0.8) +
  scale_color_manual(values = cols)+
  theme(legend.title=element_blank())+
  theme_classic(base_size=18)+
  theme(legend.position = "bottom")+
  labs(y ="Orientation entropy", x = "log<sub>10</sub> Total street length (km)")+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )
ggMarginal(gg, type = "density",groupColour = TRUE,groupFill = TRUE)



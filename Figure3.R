library(ggplot2)
library(ggpubr)
library(ggtext)
############################### load data
citiesfin <- read.csv("dat_cit.csv")
############################### subroutine zone
hipl<-function(orsdis,orsteo)
{
  dft<-orsteo
  dft <- subset(dft, select = -X)
  dft<-as.matrix(dft)
  dft<-c(dft)
  dft<-as.data.frame(dft/1000)
  dfe<-as.data.frame(orsdis$x)
  ddf<-cbind(dft,dfe,dfe/dft)  
  df<-as.data.frame(dfe/dft)
  df <- replace(df, is.na(df), 0)
  df<-df[df>0]
  mean(df)
}

################################################# A PART
# Create dataset
lSD <- log10(citiesfin$sum.dis.)
lNC <- log10(citiesfin$sum.teo_dis./1000)
label1 <- as.character(citiesfin$NAME)
sample_data <- data.frame(lSD, lNC, label1)
m=lm(lSD~lNC)
summary(m)
# add text with geom_text
dis<-ggplot(sample_data, aes(x=lNC, y=lSD)) +
  geom_point(color="pink4",alpha=0.5,size=3) +
  geom_smooth(method = "lm",linewidth=2,alpha=0.2, se=FALSE, color="purple4", formula = y ~ x) +
  labs(y ="log<sub>10</sub> Total travel distance (km)", x = "log<sub>10</sub> Total Euclidean distance (km)")+
  theme_classic(base_size=12)+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

#########HISTOGRAM MEASURES##### PART B
# CREATE A FOLDER NAMED CENSUS. ANOTHER FOR STATE. INSIDE CITY.
# DATA FROM CITY IS THE OUTPUT OF MATRICES.R
N=60
#assign dfc
NUMBER=as.data.frame(seq(1,N,1))
#asigna coordenada x,y
dfp=data.frame(NUMBER,NUMBER)
colnames(dfp)[1] <- "CITY"
colnames(dfp)[2] <- "MEAN"

orsdis <- read.csv("~/Escritorio/census/FL/HIALEAH/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/HIALEAH/teod.csv")
dfp$MEAN[1]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CO/COLORADO SPRINGS/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CO/COLORADO SPRINGS/teod.csv")
dfp$MEAN[2]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CO/DENVER/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CO/DENVER/teod.csv")
dfp$MEAN[3]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CO/FORT COLLINS/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CO/FORT COLLINS/teod.csv")
dfp$MEAN[4]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CT/Bridgeport/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CT/Bridgeport/teod.csv")
dfp$MEAN[5]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CT/Hartford/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CT/Hartford/teod.csv")
dfp$MEAN[6]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CT/New Haven/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CT/New Haven/teod.csv")
dfp$MEAN[7]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CT/Stamford/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CT/Stamford/teod.csv")
dfp$MEAN[8]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/CT/Waterbury/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/CT/Waterbury/teod.csv")
dfp$MEAN[9]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/CAPE CORAL/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/CAPE CORAL/teod.csv")
dfp$MEAN[10]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/FORT LAUDERDALE/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/FORT LAUDERDALE/teod.csv")
dfp$MEAN[11]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/HIALEAH/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/HIALEAH/teod.csv")
dfp$MEAN[12]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/HOLLYWOOD/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/HOLLYWOOD/teod.csv")
dfp$MEAN[13]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/JACKSONVILLE/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/JACKSONVILLE/teod.csv")
dfp$MEAN[14]=hipl(orsdis,orsteo)


orsdis <- read.csv("~/Escritorio/census/FL/MIAMI/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/MIAMI/teod.csv")
dfp$MEAN[15]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/ORLANDO/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/ORLANDO/teod.csv")
dfp$MEAN[16]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/PORT STLUCIE/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/PORT STLUCIE/teod.csv")
dfp$MEAN[17]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/ST PETERSBURG/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/ST PETERSBURG/teod.csv")
dfp$MEAN[18]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/GA/ATLANTA/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/GA/ATLANTA/teod.csv")
dfp$MEAN[19]=hipl(orsdis,orsteo)


orsdis <- read.csv("~/Escritorio/census/GA/COLUMBUS/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/GA/COLUMBUS/teod.csv")
dfp$MEAN[20]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/GA/MACON-BIBB/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/GA/MACON-BIBB/teod.csv")
dfp$MEAN[21]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/IN/Evansville/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/IN/Evansville/teod.csv")
dfp$MEAN[22]=hipl(orsdis,orsteo)


orsdis <- read.csv("~/Escritorio/census/IN/Fort Wayne/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/IN/Fort Wayne/teod.csv")
dfp$MEAN[23]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/IN/South Bend/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/IN/South Bend/teod.csv")
dfp$MEAN[24]=hipl(orsdis,orsteo)


orsdis <- read.csv("~/Escritorio/census/MA/BOSTON/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/MA/BOSTON/teod.csv")
dfp$MEAN[25]=hipl(orsdis,orsteo)


orsdis <- read.csv("~/Escritorio/census/MA/CAMBRIDGE/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/MA/CAMBRIDGE/teod.csv")
dfp$MEAN[26]=hipl(orsdis,orsteo)


orsdis <- read.csv("~/Escritorio/census/MA/SPRINGFIELD/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/MA/SPRINGFIELD/teod.csv")
dfp$MEAN[27]=hipl(orsdis,orsteo)


orsdis <- read.csv("~/Escritorio/census/MA/WORCESTER/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/MA/WORCESTER/teod.csv")
dfp$MEAN[28]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NY/Buffalo/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NY/Buffalo/teod.csv")
dfp$MEAN[29]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NY/New York/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NY/New York/teod.csv")
dfp$MEAN[30]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NY/Rochester/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NY/Rochester/teod.csv")
dfp$MEAN[31]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NY/Yonkers/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NY/Yonkers/teod.csv")
dfp$MEAN[32]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/WDC/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/WDC/teod.csv")
dfp$MEAN[33]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/OH/Akron/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/OH/Akron/teod.csv")
dfp$MEAN[34]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/OH/Cincinnati/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/OH/Cincinnati/teod.csv")
dfp$MEAN[35]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/OH/Cleveland/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/OH/Cleveland/teod.csv")
dfp$MEAN[36]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/OH/Columbus/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/OH/Columbus/teod.csv")
dfp$MEAN[37]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/OH/Dayton/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/OH/Dayton/teod.csv")
dfp$MEAN[38]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/OH/Toledo/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/OH/Toledo/teod.csv")
dfp$MEAN[39]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NC/cary/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NC/cary/teod.csv")
dfp$MEAN[40]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NC/Charlotte/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NC/Charlotte/teod.csv")
dfp$MEAN[41]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NC/durham/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NC/durham/teod.csv")
dfp$MEAN[42]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NC/Fayetteville/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NC/Fayetteville/teod.csv")
dfp$MEAN[43]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NC/greensboro/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NC/greensboro/teod.csv")
dfp$MEAN[44]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/NC/raleigh/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/NC/raleigh/teod.csv")
dfp$MEAN[45]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/MI/detroit/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/MI/detroit/teod.csv")
dfp$MEAN[46]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/MI/grand rapids/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/MI/grand rapids/teod.csv")
dfp$MEAN[47]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/MI/sterling heights/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/MI/sterling heights/teod.csv")
dfp$MEAN[48]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/TAMPA/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/TAMPA/teod.csv")
dfp$MEAN[49]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/FL/TALLAHASSE/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/FL/TALLAHASSE/teod.csv")
dfp$MEAN[50]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/UTAH/WEST JORDAN/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/UTAH/WEST JORDAN/teod.csv")
dfp$MEAN[51]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/UTAH/WEST VALLEY/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/UTAH/WEST VALLEY/teod.csv")
dfp$MEAN[52]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/AMARILLO/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/AMARILLO/teod.csv")
dfp$MEAN[53]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/ARLINGTON/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/ARLINGTON/teod.csv")
dfp$MEAN[54]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/AUSTIN/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/AUSTIN/teod.csv")
dfp$MEAN[55]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/DALLAS/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/DALLAS/teod.csv")
dfp$MEAN[56]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/ELPASO/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/ELPASO/teod.csv")
dfp$MEAN[57]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/houston/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/houston/teod.csv")
dfp$MEAN[58]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/LAREDO/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/LAREDO/teod.csv")
dfp$MEAN[59]=hipl(orsdis,orsteo)

orsdis <- read.csv("~/Escritorio/census/TX/SAN ANTONIO/orsdis.csv")
orsteo <- read.csv("~/Escritorio/census/TX/SAN ANTONIO/teod.csv")
dfp$MEAN[60]=hipl(orsdis,orsteo)


#Creta histogram 
m=hist(dfp$MEAN,breaks=6)
xx<-m$mids
yy<-m$density
ddf<-data.frame(xx,yy)
plot(ddf)
m <- nls(yy ~ 1/b*exp((a-xx)/b)*exp(-exp((a-xx)/b)), data = ddf, start = list(a=0.7,b=0.5))
summary(m)
dx=seq(1,2,0.001)
a=coef(m)[[1]]
b=coef(m)[[2]]
dy=1/b*exp((a-dx)/b)*exp(-exp((a-dx)/b))
dff=data.frame(dx,dy)
lines(dx,dy,col="red",lwd=2)
(RSS.p <- sum(residuals(m)^2))  # Residual sum of squares
(TSS <- sum((yy - mean(yy))^2))  # Total sum of squares
RR=1 - (RSS.p/TSS)  # R-squared measure
print(RR)

# add text with geom_text
dih<-ggplot() +
  geom_point(data=ddf,aes(x=xx, y = yy,colour="Data"),alpha=0.5,size=3) +
  geom_line(data=dff,aes(x=dx, y = dy,colour="Fit"),alpha=0.6,lwd=2) +
  geom_vline(xintercept=1.43,lwd=2,colour="darkred")+
  xlim(1.25,1.75)+
  #  theme(legend.title=element_blank())+
  labs(x ="Detour index", y = "Index density")+
  scale_color_manual(name = "", values = c("Data" = "deeppink2","Fit"="deeppink4","Mean"="darkred"))+
  theme(legend.title=element_blank())+
  theme_classic(base_size=12)+
  theme(legend.position = c(0.85, 0.8))+
  theme(legend.background = element_rect(linetype="solid"))
theme(
  axis.title.x = element_markdown(),
  axis.title.y = element_markdown()
)

ggarrange(dis,dih,labels=c("A","B","C"),ncol=2,nrow=1,align="h")


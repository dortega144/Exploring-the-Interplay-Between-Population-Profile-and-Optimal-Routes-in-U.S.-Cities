library(ggplot2)
library(viridis)
library(ggtext)
library(ggpubr)
################################# subroutine
hipl <- function(orsdis) {
  # Code
  df<-orsdis$x
  df<-df[df>0]
  dfc<-as.data.frame(df)
}
###############################################
############################################### PART A. REQUIRE MATRIX FROM MATRICES
orsdis <- read.csv("~/Escritorio/census/CT/Stamford/orsdis.csv")
stamford<-hipl(orsdis)

df3<-stamford
df3$Cities="Stamford"
dfci<-df3

orsdis <- read.csv("~/Escritorio/census/IN/Fort Wayne/orsdis.csv")
fort_wayne<-hipl(orsdis)

df3<-fort_wayne
df3$Cities="Fort Wayne"
dfci<-rbind(dfci,df3)


orsdis <- read.csv("~/Escritorio/census/MI/detroit/orsdis.csv")
detroit<-hipl(orsdis)


df3<-detroit
df3$Cities="Detroit"
dfci<-rbind(dfci,df3)


orsdis <- read.csv("~/Escritorio/census/NY/New York/orsdis.csv")
new_york<-hipl(orsdis)
df3<-new_york
df3$Cities="New York"
dfci<-rbind(dfci,df3)


# With transparency (right)

dp <- ggplot(data=dfci, aes(x=df, group=Cities, fill=Cities)) +
  geom_density(adjust=1.5, alpha=.3) +
  theme_classic(base_size=14)+
  xlim(0,50)+
  xlab('Travel distance (km)') +
  ylab('Distance density')+
  theme(legend.position = c(0.7, 0.7))+
  scale_fill_viridis_d(direction=-1,breaks=c('Stamford', 'Fort Wayne', 'Detroit','New York'))


#######################################################   EUCLIDEAN DISTANCE DISTRO
library(ggplot2)
######################################### SUBROUTINE
hipl <- function(dft) {
  # Code
  dft<-as.data.frame(dft)
  dft <- subset(dft, select = -X)
  dft<-as.matrix(dft)
  dft<-c(dft/1000)
  df<-as.data.frame(dft)
  df<-df[df>0]
  df<-df/mean(df)
  m=hist(df,breaks=25)
  dfc<-data.frame(m$mids,m$density)
}
##########################################
teod <- read.csv("~/Escritorio/census/CO/AURORA/teod.csv")
aurora<-hipl(teod)
df2<-aurora
teod <- read.csv("~/Escritorio/census/CO/COLORADO SPRINGS/teod.csv")
colorado_springs<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/CO/DENVER/teod.csv")
denver<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/CO/FORT COLLINS/teod.csv")
fort_collins<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/CT/Bridgeport/teod.csv")
bridgeport<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/CT/Hartford/teod.csv")
hartford<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/CT/New Haven/teod.csv")
new_haven<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/CT/Stamford/teod.csv")
stamford<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/CT/Waterbury/teod.csv")
waterbury<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/CAPE CORAL/teod.csv")
cape_coral<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/FORT LAUDERDALE/teod.csv")
fort_lauderdale<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/HIALEAH/teod.csv")
hialeah<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/HOLLYWOOD/teod.csv")
hollywood<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/JACKSONVILLE/teod.csv")
jacksonville<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/MIAMI/teod.csv")
miami<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/ORLANDO/teod.csv")
orlando<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/PORT STLUCIE/teod.csv")
port_stlucie<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/ST PETERSBURG/teod.csv")
st_petersburg<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/GA/ATLANTA/teod.csv")
atlanta<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/GA/COLUMBUS/teod.csv")
columbusG<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/GA/MACON-BIBB/teod.csv")
macon_bibb<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/IN/Evansville/teod.csv")
evansville<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/IN/Fort Wayne/teod.csv")
fort_wayne<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/IN/South Bend/teod.csv")
south_bend<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/MA/BOSTON/teod.csv")
boston<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/MA/CAMBRIDGE/teod.csv")
cambridge<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/MA/SPRINGFIELD/teod.csv")
springfield<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/MA/WORCESTER/teod.csv")
worcester<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NY/Buffalo/teod.csv")
buffalo<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NY/New York/teod.csv")
new_york<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NY/Rochester/teod.csv")
rochester<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NY/Yonkers/teod.csv")
yonkers<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/WDC/teod.csv")
wdc<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/OH/Akron/teod.csv")
akron<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/OH/Cincinnati/teod.csv")
cincinnati<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/OH/Cleveland/teod.csv")
cleveland<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/OH/Columbus/teod.csv")
columbusO<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/OH/Dayton/teod.csv")
dayton<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/OH/Toledo/teod.csv")
toledo<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NC/cary/teod.csv")
cary<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NC/Charlotte/teod.csv")
charlotte<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NC/durham/teod.csv")
durham<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NC/Fayetteville/teod.csv")
Fayetteville<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NC/greensboro/teod.csv")
greensboro<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/NC/raleigh/teod.csv")
raleigh<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/MI/detroit/teod.csv")
detroit<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/MI/grand rapids/teod.csv")
gran_rapids<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/MI/sterling heights/teod.csv")
sterling_heights<-hipl(teod)
df2<-rbind(df2,hipl(teod))
#-------------------------------------------------
teod <- read.csv("~/Escritorio/census/FL/TAMPA/teod.csv")
tampa<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/FL/TALLAHASSE/teod.csv")
tallahasse<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/UTAH/WEST JORDAN/teod.csv")
wjordan<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/UTAH/WEST VALLEY/teod.csv")
wvalley<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/AMARILLO/teod.csv")
amarillo<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/ARLINGTON/teod.csv")
arlington<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/AUSTIN/teod.csv")
austin<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/DALLAS/teod.csv")
dallas<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/ELPASO/teod.csv")
elpaso<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/houston/teod.csv")
houston<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/LAREDO/teod.csv")
laredo<-hipl(teod)
df2<-rbind(df2,hipl(teod))
teod <- read.csv("~/Escritorio/census/TX/SAN ANTONIO/teod.csv")
sanantonio<-hipl(teod)
df2<-rbind(df2,hipl(teod))
colnames(df2) <- c('x','y')
m <- nls(y ~ (alfa/landa)*((x/landa)^(alfa-1))*exp(-1.*(x/landa)^alfa), data = df2, start = list(alfa=2,landa=1))
summary(m)
alfa=coef(m)[[1]]
landa=coef(m)[[2]]
x2=seq(0.0,5,0.001)
y2=(alfa/landa)*((x2/landa)^(alfa-1))*exp(-1.*(x2/landa)^alfa)


ddff=data.frame(x2,y2)
#lines(ddff,col="black",lwd=3)





ed<-ggplot(aurora,aes(x=m.mids,y=m.density))+
  geom_line(color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=colorado_springs,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=denver,aes(x=m.mids,y=m.density,colour="Data"),size=1,alpha=0.2)+
  geom_line(data=fort_collins,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=bridgeport,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=hartford,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=new_haven,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=stamford,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=waterbury,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=cape_coral,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=fort_lauderdale,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=hialeah,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=hollywood,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=jacksonville,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=miami,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=orlando,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=port_stlucie,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=st_petersburg,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=atlanta,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=columbusG,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=macon_bibb,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=evansville,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=fort_wayne,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=south_bend,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=boston,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=cambridge,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=springfield,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=buffalo,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=new_york,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=yonkers,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=rochester,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=wdc,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=akron,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=cincinnati,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=cleveland,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=columbusO,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=cincinnati,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=dayton,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=toledo,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=cary,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=Fayetteville,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=durham,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=charlotte,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=greensboro,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=raleigh,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=gran_rapids,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=sterling_heights,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=tampa,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=tallahasse,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=wjordan,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=wvalley,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=detroit,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=amarillo,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=arlington,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=austin,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=dallas,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=elpaso,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=houston,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=laredo,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  geom_line(data=sanantonio,aes(x=m.mids,y=m.density),color="olivedrab3",size=1,alpha=0.2)+
  xlim(0, 3)+
  geom_line(data=ddff,aes(x=x2, y = y2,colour="Fit"),size=2) +
  labs(x ="Normalized Euclidean distance", y = "Distance density")+
  scale_color_manual(name = "", values = c("Data" = "olivedrab3","Fit"="chartreuse4"))+
  theme(legend.title=element_blank())+
  theme_classic(base_size=14)+
  theme(legend.position = c(0.85, 0.8))+
  theme(legend.background = element_rect(linetype="solid"))+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

############################################################################## time
hipl <- function(orstimes) {
  # Code
  df<-orstimes$x
  df<-df[df>0]
  df<-df/mean(df)
  m=hist(df,breaks=25)
  dfc<-data.frame(m$mids,m$density)
}

orstimes <- read.csv("~/Escritorio/census/CO/AURORA/orstimes.csv")
aurora<-hipl(orstimes)
df2<-aurora
orstimes <- read.csv("~/Escritorio/census/CO/COLORADO SPRINGS/orstimes.csv")
colorado_springs<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/CO/DENVER/orstimes.csv")
denver<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/CO/FORT COLLINS/orstimes.csv")
fort_collins<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/CT/Bridgeport/orstimes.csv")
bridgeport<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/CT/Hartford/orstimes.csv")
hartford<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/CT/New Haven/orstimes.csv")
new_haven<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/CT/Stamford/orstimes.csv")
stamford<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/CT/Waterbury/orstimes.csv")
waterbury<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/CAPE CORAL/orstimes.csv")
cape_coral<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/FORT LAUDERDALE/orstimes.csv")
fort_lauderdale<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/HIALEAH/orstimes.csv")
hialeah<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/HOLLYWOOD/orstimes.csv")
hollywood<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/JACKSONVILLE/orstimes.csv")
jacksonville<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/MIAMI/orstimes.csv")
miami<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/ORLANDO/orstimes.csv")
orlando<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/PORT STLUCIE/orstimes.csv")
port_stlucie<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/ST PETERSBURG/orstimes.csv")
st_petersburg<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/GA/ATLANTA/orstimes.csv")
atlanta<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/GA/COLUMBUS/orstimes.csv")
columbusG<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/GA/MACON-BIBB/orstimes.csv")
macon_bibb<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/IN/Evansville/orstimes.csv")
evansville<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/IN/Fort Wayne/orstimes.csv")
fort_wayne<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/IN/South Bend/orstimes.csv")
south_bend<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/MA/BOSTON/orstimes.csv")
boston<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/MA/CAMBRIDGE/orstimes.csv")
cambridge<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/MA/SPRINGFIELD/orstimes.csv")
springfield<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/MA/WORCESTER/orstimes.csv")
worcester<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NY/Buffalo/orstimes.csv")
buffalo<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NY/New York/orstimes.csv")
new_york<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NY/Rochester/orstimes.csv")
rochester<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NY/Yonkers/orstimes.csv")
yonkers<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/WDC/orstimes.csv")
wdc<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/OH/Akron/orstimes.csv")
akron<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/OH/Cincinnati/orstimes.csv")
cincinnati<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/OH/Cleveland/orstimes.csv")
cleveland<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/OH/Columbus/orstimes.csv")
columbusO<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/OH/Dayton/orstimes.csv")
dayton<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/OH/Toledo/orstimes.csv")
toledo<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NC/cary/orstimes.csv")
cary<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NC/Charlotte/orstimes.csv")
charlotte<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NC/durham/orstimes.csv")
durham<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NC/Fayetteville/orstimes.csv")
Fayetteville<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NC/greensboro/orstimes.csv")
greensboro<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/NC/raleigh/orstimes.csv")
raleigh<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/MI/detroit/orstimes.csv")
detroit<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/MI/grand rapids/orstimes.csv")
gran_rapids<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/MI/sterling heights/orstimes.csv")
sterling_heights<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
#-------------------------------------------------
orstimes <- read.csv("~/Escritorio/census/FL/TAMPA/orstimes.csv")
tampa<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/FL/TALLAHASSE/orstimes.csv")
tallahasse<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/UTAH/WEST JORDAN/orstimes.csv")
wjordan<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/UTAH/WEST VALLEY/orstimes.csv")
wvalley<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/AMARILLO/orstimes.csv")
amarillo<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/ARLINGTON/orstimes.csv")
arlington<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/AUSTIN/orstimes.csv")
austin<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/DALLAS/orstimes.csv")
dallas<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/ELPASO/orstimes.csv")
elpaso<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/houston/orstimes.csv")
houston<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/LAREDO/orstimes.csv")
laredo<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))
orstimes <- read.csv("~/Escritorio/census/TX/SAN ANTONIO/orstimes.csv")
sanantonio<-hipl(orstimes)
df2<-rbind(df2,hipl(orstimes))





colnames(df2) <- c('x','y')
m <- nls(y ~ (alfa/landa)*((x/landa)^(alfa-1))*exp(-1.*(x/landa)^alfa), data = df2, start = list(alfa=2,landa=1))


summary(m)
alfa=coef(m)[[1]]
landa=coef(m)[[2]]
x2=seq(0.0,5,0.001)
y2=(alfa/landa)*((x2/landa)^(alfa-1))*exp(-1.*(x2/landa)^alfa)

ddff=data.frame(x2,y2)
lines(ddff,col="black",lwd=3)


tt<-ggplot(aurora,aes(x=m.mids,y=m.density))+
  geom_line(color="darkorange",size=1,alpha=0.2)+
  geom_line(data=colorado_springs,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=denver,aes(x=m.mids,y=m.density,colour="Data"),size=1,alpha=0.2)+
  geom_line(data=fort_collins,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=bridgeport,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=hartford,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=new_haven,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=stamford,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=waterbury,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=cape_coral,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=fort_lauderdale,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=hialeah,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=hollywood,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=jacksonville,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=miami,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=orlando,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=port_stlucie,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=st_petersburg,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=atlanta,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=columbusG,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=macon_bibb,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=evansville,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=fort_wayne,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=south_bend,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=boston,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=cambridge,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=springfield,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=buffalo,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=new_york,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=yonkers,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=rochester,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=wdc,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=akron,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=cincinnati,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=cleveland,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=columbusO,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=cincinnati,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=dayton,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=toledo,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=cary,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=Fayetteville,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=durham,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=charlotte,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=greensboro,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=raleigh,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=gran_rapids,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=sterling_heights,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=tampa,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=tallahasse,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=wjordan,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=wvalley,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=detroit,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=amarillo,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=arlington,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=austin,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=dallas,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=elpaso,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=houston,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=laredo,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  geom_line(data=sanantonio,aes(x=m.mids,y=m.density),color="darkorange",size=1,alpha=0.2)+
  xlim(0, 3)+
  geom_line(data=ddff,aes(x=x2, y = y2,colour="Fit"),size=2) +
  labs(x ="Normalized travel time", y = "Distance density")+
  scale_color_manual(name = "", values = c("Data" = "darkorange","Fit"="darkorange4"))+
  theme(legend.title=element_blank())+
  theme_classic(base_size=14)+
  theme(legend.position = c(0.85, 0.8))+
  theme(legend.background = element_rect(linetype="solid"))+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )



###################################################################  Travel Distance

hipl <- function(orsdis) {
  # Code
  df<-orsdis$x
  df<-df[df>0]
  df<-df/mean(df)
  m=hist(df,breaks=25)
  dfc<-data.frame(m$mids,m$density)
}

orsdis <- read.csv("~/Escritorio/census/CO/AURORA/orsdis.csv")
aurora<-hipl(orsdis)
df2<-aurora
orsdis <- read.csv("~/Escritorio/census/CO/COLORADO SPRINGS/orsdis.csv")
colorado_springs<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/CO/DENVER/orsdis.csv")
denver<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/CO/FORT COLLINS/orsdis.csv")
fort_collins<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/CT/Bridgeport/orsdis.csv")
bridgeport<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/CT/Hartford/orsdis.csv")
hartford<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/CT/New Haven/orsdis.csv")
new_haven<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/CT/Stamford/orsdis.csv")
stamford<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/CT/Waterbury/orsdis.csv")
waterbury<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/CAPE CORAL/orsdis.csv")
cape_coral<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/FORT LAUDERDALE/orsdis.csv")
fort_lauderdale<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/HIALEAH/orsdis.csv")
hialeah<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/HOLLYWOOD/orsdis.csv")
hollywood<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/JACKSONVILLE/orsdis.csv")
jacksonville<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/MIAMI/orsdis.csv")
miami<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/ORLANDO/orsdis.csv")
orlando<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/PORT STLUCIE/orsdis.csv")
port_stlucie<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/ST PETERSBURG/orsdis.csv")
st_petersburg<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/GA/ATLANTA/orsdis.csv")
atlanta<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/GA/COLUMBUS/orsdis.csv")
columbusG<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/GA/MACON-BIBB/orsdis.csv")
macon_bibb<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/IN/Evansville/orsdis.csv")
evansville<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/IN/Fort Wayne/orsdis.csv")
fort_wayne<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/IN/South Bend/orsdis.csv")
south_bend<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/MA/BOSTON/orsdis.csv")
boston<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/MA/CAMBRIDGE/orsdis.csv")
cambridge<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/MA/SPRINGFIELD/orsdis.csv")
springfield<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/MA/WORCESTER/orsdis.csv")
worcester<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NY/Buffalo/orsdis.csv")
buffalo<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NY/New York/orsdis.csv")
new_york<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NY/Rochester/orsdis.csv")
rochester<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NY/Yonkers/orsdis.csv")
yonkers<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/WDC/orsdis.csv")
wdc<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/OH/Akron/orsdis.csv")
akron<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/OH/Cincinnati/orsdis.csv")
cincinnati<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/OH/Cleveland/orsdis.csv")
cleveland<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/OH/Columbus/orsdis.csv")
columbusO<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/OH/Dayton/orsdis.csv")
dayton<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/OH/Toledo/orsdis.csv")
toledo<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NC/cary/orsdis.csv")
cary<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NC/Charlotte/orsdis.csv")
charlotte<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NC/durham/orsdis.csv")
durham<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NC/Fayetteville/orsdis.csv")
Fayetteville<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NC/greensboro/orsdis.csv")
greensboro<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/NC/raleigh/orsdis.csv")
raleigh<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/MI/detroit/orsdis.csv")
detroit<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/MI/grand rapids/orsdis.csv")
gran_rapids<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/MI/sterling heights/orsdis.csv")
sterling_heights<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
#-------------------------------------------------
orsdis <- read.csv("~/Escritorio/census/FL/TAMPA/orsdis.csv")
tampa<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/FL/TALLAHASSE/orsdis.csv")
tallahasse<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/UTAH/WEST JORDAN/orsdis.csv")
wjordan<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/UTAH/WEST VALLEY/orsdis.csv")
wvalley<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/AMARILLO/orsdis.csv")
amarillo<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/ARLINGTON/orsdis.csv")
arlington<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/AUSTIN/orsdis.csv")
austin<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/DALLAS/orsdis.csv")
dallas<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/ELPASO/orsdis.csv")
elpaso<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/houston/orsdis.csv")
houston<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/LAREDO/orsdis.csv")
laredo<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))
orsdis <- read.csv("~/Escritorio/census/TX/SAN ANTONIO/orsdis.csv")
sanantonio<-hipl(orsdis)
df2<-rbind(df2,hipl(orsdis))


colnames(df2) <- c('x','y')
m <- nls(y ~ (alfa/landa)*((x/landa)^(alfa-1))*exp(-1.*(x/landa)^alfa), data = df2, start = list(alfa=2,landa=1))


summary(m)
alfa=coef(m)[[1]]
landa=coef(m)[[2]]
x2=seq(0.0,5,0.001)
y2=(alfa/landa)*((x2/landa)^(alfa-1))*exp(-1.*(x2/landa)^alfa)

ddff=data.frame(x2,y2)
lines(ddff,col="black",lwd=3)


td<-ggplot(aurora,aes(x=m.mids,y=m.density))+
  geom_line(color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=colorado_springs,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=denver,aes(x=m.mids,y=m.density,colour="Data"),size=1,alpha=0.2)+
  geom_line(data=fort_collins,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=bridgeport,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=hartford,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=new_haven,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=stamford,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=waterbury,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=cape_coral,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=fort_lauderdale,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=hialeah,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=hollywood,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=jacksonville,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=miami,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=orlando,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=port_stlucie,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=st_petersburg,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=atlanta,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=columbusG,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=macon_bibb,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=evansville,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=fort_wayne,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=south_bend,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=boston,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=cambridge,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=springfield,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=buffalo,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=new_york,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=yonkers,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=rochester,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=wdc,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=akron,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=cincinnati,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=cleveland,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=columbusO,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=cincinnati,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=dayton,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=toledo,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=cary,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=Fayetteville,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=durham,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=charlotte,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=greensboro,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=raleigh,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=gran_rapids,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=sterling_heights,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=tampa,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=tallahasse,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=wjordan,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=wvalley,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=detroit,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=amarillo,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=arlington,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=austin,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=dallas,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=elpaso,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=houston,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=laredo,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  geom_line(data=sanantonio,aes(x=m.mids,y=m.density),color="deepskyblue",size=1,alpha=0.2)+
  xlim(0, 3)+
  geom_line(data=ddff,aes(x=x2, y = y2,colour="Fit"),size=2) +
  labs(x ="Normalized travel distance", y = "Distance density")+
  scale_color_manual(name = "", values = c("Data" = "deepskyblue","Fit"="deepskyblue4"))+
  theme(legend.title=element_blank())+
  theme_classic(base_size=14)+
  theme(legend.position = c(0.85, 0.8))+
  theme(legend.background = element_rect(linetype="solid"))+
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

############################# ARRANGE

ggarrange(dp,td,tt,ed,labels=c("A","B","C","D"),ncol=2,nrow=2)







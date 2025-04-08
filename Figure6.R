library(spatstat)
library(ggplot2)
library(ggpubr)
library(ggtext)
library(deldir)
#POINTS
#Number of centroids
N=40
#assign dfc
NUMBER=as.data.frame(seq(1,N,1))
#assign x,y
dfc=data.frame(NUMBER,NUMBER,NUMBER)
colnames(dfc)[1] <- "NODE"
colnames(dfc)[2] <- "XN"
colnames(dfc)[3] <- "YN"
#1000 runs of 40 centroids (1000 simulated cities)
for (loo in 1:1000){
#exponential and degrees
     dis=0.
     rahs=0.
     k=1
     while (k<=N) {
     r=runif(1,0,1) #random radius
     azar=runif(1,0,1) #rando number 
     theta=runif(1,0,360) #random angle
     if (azar<(1-r)^2) { 
       dfc$XN[k]=r*cos(theta*pi/180.) # cartesian coordinates
       dfc$YN[k]=r*sin(theta*pi/180.)
       sum=0
       if (k>1){ #length list > 1
         sum=0
       #not in the hard sphere of any other centroid
         for (kk in 1:(k-1)) {
          x2=(dfc$XN[kk]-dfc$XN[k])^2
          y2=(dfc$YN[kk]-dfc$YN[k])^2
          dis=sqrt(x2+y2)
          if (dis>0.05) { 
            sum=sum+1 #count 1 if not in the sphere ok centroid kk
          }
         }
       if (sum==k-1) { #count=length list
         k=k+1 #increase the length list
         }
       }
       else{ # (k==1)first point no need to check distances, only azar<(1-r)^beta
         k=k+1
         } 
       } #(1-r)^beta bucle
     }

#create point pattern
Androsace<-owin(xrange=c(min(dfc$XN)-0.1,max(dfc$XN)+0.1), yrange=c(min(dfc$YN)-0.1,max(dfc$YN)+0.1))
X<-ppp(dfc$XN,dfc$YN,Androsace)
#distances distro
m=pairdist(X)
m=c(m)
m<-m[m>0]
valmax<-mean(m)
m<-m/valmax
if (loo==1) {
  dfz<-as.data.frame(c(m))
  dfr<-dfc}
else{
  dfz<-rbind(dfz,as.data.frame(c(m)))
  dfr<-rbind(dfr,dfc)}
print(loo)
}

#fit Weibull (experimental) from figure 5
alfa=1.903  #weibull parameters
landa=1.126
x2=seq(0.0,3.5,0.001)
y2=(alfa/landa)*((x2/landa)^(alfa-1))*exp(-1.*(x2/landa)^alfa)
ddff=data.frame(x2,y2)

#data from simulation
z=hist(dfz$`c(m)`,breaks=20)
plot(ddff,pch=16)
x1=z$mids
y1=z$density
ddff11=data.frame(x1,y1)
points(ddff11,pch=16,col="red")
colnames(ddff11) <- c('x','y')
m <- nls(y ~ (landa*alfa)*((landa*x)^(alfa-1))*exp(-1.*(x*landa)^alfa), data = ddff11, start = list(alfa=0.7,landa=0.5))
summary(m)
alfa=coef(m)[[1]]
landa=coef(m)[[2]]
x2=seq(0.0,3.5,0.001)
y2=(landa*alfa)*((landa*x2)^(alfa-1))*exp(-1.*(x2*landa)^alfa)
ddff3=data.frame(x2,y2)
lines(ddff,col="red",lwd=3)

#residuals
(RSS.p <- sum(residuals(m)^2))  # Residual sum of squares
(TSS <- sum((y1 - mean(y1))^2))  # Total sum of squares
RR=1 - (RSS.p/TSS)  # R-squared measure
print(RR)

K1 <- density(X,sigma = bw.scott) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, axes=FALSE,add=TRUE,expand=5,drawlabels=FALSE)
plot(X, col = "white", cex = 0.5, pch = 19, add = TRUE)
#VORONOI
tesselation <- deldir(dfc$XN,dfc$YN)
tiles <- tile.list(tesselation)
plot(tiles, pch = 19,cex=0.5,
     col.pts = "white",
     border = "white",
     fillcol = hcl.colors(50, "viridis"))


# add text with geom_text
ed<-ggplot() +
  geom_line(data=ddff,aes(x=x2, y = y2,colour="Euclidean fit"),size=2,alpha=0.5,linetype=1) +
  geom_line(data=ddff3,aes(x=x2, y = y2,colour="Simulated fit"),size=2,linetype=2) +
  geom_point(data=ddff11,aes(x=x1, y = y1,colour="Simulated"),size=3) +
  xlab('Normalized Euclidean distance') +
  ylab('Distance density')+
  scale_color_manual(name = "", values = c("Simulated fit" = "gray50","Euclidean fit"="chartreuse4","Simulated"="darkgreen"))+
#  theme(legend.title=element_blank())+
  theme_classic(base_size=12)+
  xlim(0.0,3.5)+
  theme(legend.position = c(0.85, 0.8))+
  theme(legend.background = element_rect(linetype="solid"))


##############################################################################

#radial density
radius<-as.data.frame(sqrt((dfr$XN)^2+(dfr$YN)^2))
mr<-hist(radius$`sqrt((dfr$XN)^2 + (dfr$YN)^2)`)
xrf=mr$breaks[2:length(mr$breaks)]
xri=mr$breaks[1:length(mr$breaks)-1]
yr=mr$counts
ror=yr/(3.141592*(xrf^2-xri^2))
ror=ror/max(ror)
#exponential
dfror=data.frame(mr$mids,ror)
colnames(dfror)[1] <- "xr"
mm22=nls(ror~A*exp(alfa*xr),data=dfror,start=list(A=1,alfa=-1.0))
summary(mm22)
#log
xpa=seq(0,1,0.001)
ypA=coef(mm22)[[1]]*exp(coef(mm22)[[2]]*xpa)
dfror2=data.frame(xpa,ypA)
plot(dfror,log="y")
lines(dfror2,col="red")


pd<-ggplot() +
  geom_line(data=dfror2,aes(x=xpa, y = ypA,colour="Fit Sim"),colour="gray50",size=2,alpha=0.5) +
  geom_point(data=dfror,aes(x=xr, y = ror,colour="Sim"),colour="darkgreen",size=3) +
  xlab('Radius') +
  ylab('Population density')+
  theme_classic(base_size=12)+
  theme(legend.position = c(0.85, 0.8))+
  theme(legend.background = element_rect(linetype="solid"))

ggarrange(ed,pd,labels=c("A","B"),ncol=2,nrow=1,align="h")

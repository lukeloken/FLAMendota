#Aggregates sonde casts to half meter intervals
#Averages temp, do_sat, do, ph, orp, spcond, chl_rfu, and bg_rfu according by half meter intervals
#6/16/16

rm(list=ls())
setwd("~/Dropbox/Mendota Summer 16/Sonde Casts/LAKE/Deep Hole")

#read in daily sonde cast
data<-read.csv("deephole_161025.csv")

#define ranges for half meter intervals and replace depth value
a=ifelse(data$depth>=0 & data$depth<=0.749,0.5,data$depth)
b=ifelse(a>=0.750 & a<=1.249,1.0,a)
c=ifelse(b>=1.250 & b<=1.749,1.5,b)
d=ifelse(c>=1.750 & c<=2.249,2.0,c)
e=ifelse(d>=2.250 & d<=2.749,2.5,d)
f=ifelse(e>=2.750 & e<=3.249,3.0,e)
g=ifelse(f>=3.250 & f<=3.749,3.5,f)
h=ifelse(g>=3.750 & g<=4.249,4.0,g)
i=ifelse(h>=4.250 & h<=4.749,4.5,h)
j=ifelse(i>=4.750 & i<=5.249,5.0,i)
k=ifelse(j>=5.250 & j<=5.749,5.5,j)
l=ifelse(k>=5.750 & k<=6.249,6.0,k)
m=ifelse(l>=6.250 & l<=6.749,6.5,l)
n=ifelse(m>=6.750 & m<=7.249,7.0,m)
o=ifelse(n>=7.250 & n<=7.749,7.5,n)
p=ifelse(o>=7.750 & o<=8.249,8.0,o)
q=ifelse(p>=8.250 & p<=8.749,8.5,p)
r=ifelse(q>=8.750 & q<=9.249,9.0,q)
s=ifelse(r>=9.250 & r<=9.749,9.5,r)
t=ifelse(s>=9.750 & s<=10.249,10.0,s)
u=ifelse(t>=10.250 & t<=10.749,10.5,t)
v=ifelse(u>=10.750 & u<=11.249,11.0,u)
w=ifelse(v>=11.250 & v<=11.749,11.5,v)
x=ifelse(w>=11.750 & w<=12.249,12.0,w)
y=ifelse(x>=12.250 & x<=12.749,12.5,x)
z=ifelse(y>=12.750 & y<=13.249,13.0,y)
aa=ifelse(z>=13.250 & z<=13.749,13.5,z)
bb=ifelse(aa>=13.750 & aa<=14.249,14.0,aa)
cc=ifelse(bb>=14.250 & bb<=14.749,14.5,bb)
dd=ifelse(cc>=14.750 & cc<=15.249,15.0,cc)
ee=ifelse(dd>=15.250 & dd<=15.749,15.5,dd)
ff=ifelse(ee>=15.750 & ee<=16.249,16.0,ee)
gg=ifelse(ff>=16.250 & ff<=16.749,16.5,ff)
hh=ifelse(gg>=16.750 & gg<=17.249,17.0,gg)
ii=ifelse(hh>=17.250 & hh<=17.749,17.5,hh)
jj=ifelse(ii>=17.750 & ii<=18.249,18.0,ii)
kk=ifelse(jj>=18.250 & jj<=18.749,18.5,jj)
ll=ifelse(kk>=18.750 & kk<=19.249,19.0,kk)
mm=ifelse(ll>=19.250 & ll<=19.749,19.5,ll)
nn=ifelse(mm>=19.750 & mm<=20.249,20.0,mm)
depths=ifelse(nn>=20.250 & nn<=20.749,20.5,nn)

#assign new depth values to full dataset
clean_data<-cbind(data,depths)
colnames(clean_data)

#average desired variables according to half-meter depth
cols<-c(8,10,14,16,22,23,24,26)
avg_data<-aggregate(clean_data[,cols],list(clean_data$depths),mean)
colnames(avg_data)<-c('depth','temp','spcond','chl_rfu','bg_rfu','do_sat','do','ph','orp')

#export new averaged dataset to a csv
library(xlsx)
write.csv(avg_data,'161025.csv')

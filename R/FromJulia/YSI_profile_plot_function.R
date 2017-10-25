#Function to plot Vertical Profile Data from YSI Multiparameter Sonde

#For Deep Hole Profiles
setwd("~/Dropbox/Mendota Summer 16/Sonde Casts/LAKE/Deep Hole")

#For Littoral A (Yahara) Profiles
setwd("~/Dropbox/Mendota Summer 16/Sonde Casts/LAKE/Littoral A")

#For Littoral B (Picnic Point) Profiles
setwd("~/Dropbox/Mendota Summer 16/Sonde Casts/LAKE/Littoral B")

#For Littoral C (University Bay) Profiles
setwd("~/Dropbox/Mendota Summer 16/Sonde Casts/LAKE/Littoral C")

ysi<-function(filename,var_name,units='units'){
  
  data=read.csv(filename)

  quartz()
  par(mar=c(5,5,2,1))
  ylab=expression("Depth (m)")
  xlab=expression(names(data[,var_name]))
  title=paste(var_name,' (',units,')',sep='')
  
  x = data[,var_name]
  y = data$depth
  
  plot=plot(x,y,ylab=ylab,main=title,xlab=NA,ylim=rev(range(y)),pch=16)
  return(plot)
  }

ysi(filename='deephole_161025.csv',var_name='temp',units='C')
ysi(filename='deephole_161025.csv',var_name='do',units='mg/L')
ysi(filename='deephole_161004.csv',var_name='orp',units='RFU')

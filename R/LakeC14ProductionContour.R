library(lubridate)
library(viridis)

grid.data.inhib<-read.csv('C:/Users/lloken/Downloads/C14prodvalues.csv', stringsAsFactors = F)
grid.data.inhib$x<-ymd(grid.data.inhib$x)

#Create Productivity Surfaces
#List of lake ids
lakes = unique(grid.data.inhib$n)
lakes<-lakes[1] #for testing just use the first lake

# seq of light levels for which we measure 14C uptake
span <- seq(0,3000,by=1)  


i<-lakes[1]
for(i in lakes){
temp.surface = grid.data.inhib[which(grid.data.inhib$n==i),]

dates=temp.surface$x
levels=temp.surface$y
value=temp.surface$z

#Define where to interpolate
interp_dates<-seq(ceiling_date(min(dates), "days"),floor_date(max(dates), "days"), by='days')
interp_levels<-seq(min(levels, na.rm=T), max(levels, na.rm=T), by=1)
interp_matrix<-matrix(nrow=length(interp_dates), ncol=length(interp_levels))

#Loop through light levels and interpolate through time
level=1
for (level in span){
  table<-temp.surface[temp.surface$y==level,]
  interp_values<-approx(x=table$x, y=table$z, xout=interp_dates)$y
  interp_matrix[,level+1]<-interp_values
}


interp_df<-data.frame(as.data.frame(interp_dates), interp_matrix)
names(interp_df)<-c('date', span)
# write.table, saveRDS. Depending on how you want to save this dataframe or matrix 

# #######################################
#Plot with verical lines for sample dates
# #######################################

#Create plotting objects (ticks, colors, lines)
xticks<-seq(ceiling_date(min(dates), "years"),floor_date(max(dates), "years"), by='years')
xlabels<-year(xticks)
sampledates<-unique(dates)
color.palette = colorRampPalette(c(viridis(6, begin=.2, end=.98), rev(magma(5, begin=.35, end=.98))), bias=1)


png(paste('Figures/Lake', i, 'ProductionContour.png', sep=''), width=7, height=3, units='in', res=400, bg='white')
par(pch=16)
par(ps=10)
par(mfrow=c(1,1))
par(mar = c(3,3.5,0.5,0.5),mgp=c(1.5,0.4,0),tck=-0.02)
par(lend=2)

filled.contour(x = interp_dates,y=interp_levels,z = interp_matrix, color.palette = color.palette, plot.axes = { axis(1, at=xticks, labels=xlabels); axis(2); abline(v=sampledates, lwd=1, lty=3, col='black') })

mtext("Production value", 4, -6.5)
mtext('Light level', 2, 2)

dev.off()

}

# ####################
# Noah's original code
# ####################


#Loop through lakes, (start with Trout)
for(i in 1:1){               
  temp.surface = grid.data.inhib[which(grid.data.inhib$n==lakes[i]),]
  dates<-unique(temp.surface$x)
  n.col = seq(from=min(temp.surface$x),to=max(temp.surface$x),by=1)   #seq of dates across the 14C measurements (multiple years)
  x.num=as.numeric(temp.surface$x)      #converting to numeric date so I can reference matrix rows
  temp.surface$x.num = x.num - (min(x.num)-1)     #adjusting for first measurement is column 1
  x.num=unique(x.num)       #unique days we did 14C incubations
  x.sample = x.num
  x.num=x.num - (min(x.num)-1)      #getting values for rows to populate data 
  mtx=matrix(NA,nrow=length(n.col),ncol=length(span)      #create my matrix of z values
  
    #populate matrix
    for(j in 1:length(x.num)){    
    temp.day.surface = temp.surface[which(temp.surface$x.num==x.num[j]),]
    mtx[cbind(x.num[j],(span+1))] = temp.day.surface$z
  }
  for(t in 1:ncol(mtx)){                                                      #linear interpolate 14C productivity through time for given light level
    mtx[,t] = approx(x = as.numeric(n.col),y = mtx[,t],xout = as.numeric(n.col))$y
  }
  
  
  #Create x labels
  xticks<-seq(ceiling_date(min(n.col), "years"),floor_date(max(n.col), "years"), by='years')
  xlabels<-year(xticks)
  
  filled.contour(x = n.col,y=span,z = mtx, plot.axes = { axis(1, at=xticks, labels=xlabels); axis(2); abline(v=dates, lwd=1, lty=3, col='black') }) 

  
}

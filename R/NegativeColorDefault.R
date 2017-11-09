

par_default<-par(no.readonly = TRUE)
par(bg='grey11',  fg='white', col.lab='white', col.main='white', col.sub='white', col.axis='white')
par(pch=16, family='sans' )
palette(brewer.pal(n = 8, name = "Set2"))

plot(1:10, main='main', sub='sub', col=1:8, cex=2, type='h', lwd=3)
points(1:10, main='main', sub='sub', col=1:8, cex=2, type='p', lwd=3)


par(par_default)

palette('default')
palette(c('white', palette()[-1]))
palette()

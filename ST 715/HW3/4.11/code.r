rm(list=ls())
#a)
data=as.matrix(read.table("D:\\Class\\ST 715\\HW3\\4.11\\data.txt"))
pop = data[,1]
phy = data[,2]
# hist(phy)
#b)
phyMean = mean(phy)
phySe   = sqrt(1/99*sum((phy-phyMean)^2)/100*(1-100/3141))
NphySe  = 3141*phySe
#c)
plot(pop,phy)
#d)

x = pop
y = phy

#------ use the regression method ----

xmean=mean(x)
ymean=mean(y)

B1 = sum((x-xmean)*(y-ymean))/sum((x-xmean)^2)
B0 = ymean - B1*xmean

tx=255077536
N=3141
yReg= B0+B1*tx/N
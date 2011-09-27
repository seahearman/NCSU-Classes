# ST 715 HW3 4.2
# a)

rm(list= ls())
x     = array(c(13,7,11,12,4,3,11,3,5),dim=c(1,9))
y     = array(c(10,7,13,17,8,1,15,7,4),dim=c(1,9))
N     = 9 
tx    = sum(x)
ty    = sum(y)
xMean = mean(x)
yMean = mean(y)
Sx    = sqrt(1/(N-1)*sum((x-xMean)^2))
Sy    = sqrt(1/(N-1)*sum((y-yMean)^2))
R     = sum((x-xMean)*(y-yMean))/((N-1)*Sx*Sy)
B     = ty/tx

#b)
X = x
Y = y
p = 0
NyMeans = array(0,dim=c(1,84))
tyr     = array(0,dim=c(1,84))
for (i in 1:7){
  for (j in (i+1):8){
    for (k in (j+1):9){
      p     = p+1
      x     = array(c(X[i],X[j],X[k]),dim=c(1,3))
      y     = array(c(Y[i],Y[j],Y[k]),dim=c(1,3))
      
      xMeanS= mean(x)
      yMeanS= mean(y)
      NyMeans[p]= N*yMeanS
      BEst  = yMeanS/xMeanS
      tyr[p]= yMeanS*tx/xMeanS
      
#       cat(i,",",j,",",k,xMeanS,yMeanS,N*yMeanS,BEst,tyr[p],"\n")
    
      }
  }
}

#c)
hist(NyMeans)
hist(tyr)

#d)
x=NyMeans
y=tyr
N=84
xMean = mean(x)
yMean = mean(y)
Vx= 1/N*sum((x-xMean)^2)
Vy= 1/N*sum((y-yMean)^2)
#Performing a polynomial regression using degrees d = 2, 3, 4, 5, 10, 15.
#Separating my data set into validation and training sets. Determine the validation
#setting MSE for each d value Which degree provides the best fit? and Plotting the data and polynomial regression curves in a single figure.

synthetic <- read.csv("~/Downloads/synthetic.csv")
View(synthetic)
#synt.data=read.csv("synthetic.csv")
attach(synthetic)
plot(X,Y)
range(X)
xvals=seq(0,8,length=100)
nlin=lm(Y ~ poly(X,15))
yvals=predict(nlin,data.frame(X=xvals))
lines(xvals,yvals,col="yellow",lwd=2)
# Training and Test Set
testindex=sample(1:200,100)
synt.train=synthetic[-testindex,]
synt.test=synthetic[testindex,]

#MSE Value

n=15
msevals=seq(1:n)
for(i in 1:n){
  nlin=lm(Y ~ poly(X,i),data=synt.train)
  msevals[i]=mean((predict(nlin,synt.test)-synt.test$Y)^2)}
plot(seq(1:n),msevals)
msevals

#[1] 2.8084178 1.2060485 0.8156394 0.8149637 0.8097615 0.8136266 0.8505083 0.8519667 0.8593442 0.8597655
#[11] 0.9137884 0.9120436 1.0212274 1.0198744 0.9867763

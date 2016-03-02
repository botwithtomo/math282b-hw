# hw 4 Zeyun Lu, working with Gregoire Thouvenin
# problem 1
load("04cars.version2.rda")  
library(quantreg)
dat <- dat[,c(8,10)]
names(dat)<-c("hp","mpg")
dat<-dat[complete.cases(dat),]
nrow(dat)
problem1 <- function(k,dat){
	n = nrow(dat)
	dat = dat[sample(n,n),] # do the purmutation
	m=n%/%k   # the number of how many each set contains
	if (n%%k != 0){ 
	dat=dat[-sample(n,n%%k),] #randomly delete some data in order for our 
							  # data to be equally divided
	}
	n = nrow(dat)
	errl1_total = NULL 
	errl2_total = NULL
		for (i in 1:k){
			x1 = validation = dat[c((m*(i-1)+1):(m*i)),]
			# set up validation set
			x2 = training =dat[-c((m*(i-1)+1):(m*i)),]
			# set up training set
			
			fitl1 = rq(mpg ~ hp,data=training)
			fitl2 = lm(mpg ~ hp +I(hp^2),data=training)
			newx= x1$hp
			ynewl1 = predict(fitl1, newdata = data.frame(hp=newx))
			ynewl2 = predict(fitl2,newdata = data.frame(hp=newx))
			errl1 = (1/nrow(validation))*sum((x1$mpg-ynewl1)^2)
			errl2 = (1/nrow(validation))*sum((x1$mpg-ynewl2)^2)
			errl1_total = c(errl1_total,errl1)
			errl2_total = c(errl2_total,errl2)
	}
	avg_err = list("L1"=mean(errl1_total),"L2"=mean(errl2_total))
	
}
k5 = problem1(5,dat)
k5
# Err for l1 = 19,7447 Err for l2= 16.4435
# l2 has lower Err
k10 = problem1(10,dat)
k10
# Err for l1 = 19.69  Err for l2 = 16.12385
# l2 has lower Err
kn = problem1(nrow(dat),dat)
kn
# Err for l1 = 19.58 Err for l2 = 16.08102
# l2 has lower Err
# as the K increases, the Err for both model decreases 
# we can get more accurate on model's coefficient

# problem 2
require("MASS")
str(Boston)
x<-as.matrix(Boston)
y<-x[,14]
y <- y-mean(y)  # centralize our y
x<-x[,-14]
backwardSelect<-function(x,y){
	fout = 4   # set the threshold
	index = NULL # the index that we are going to delete 
	fmin = 0   # set up fmin first 
	while (fmin < fout){
		if (!is.null(index)){
			x=x[,-index]
			# if index is null, skip this
		}
		n = ncol(x)
		fstats = NULL
		for (i in 1:n){
			fit1 = lm(y~x)  # our full model
			new_x = x[,-i]
			fit2 = lm(y~new_x)
			fstats = c(fstats,anova(fit1,fit2)[2,5])
		}
			fmin = min(fstats)
			index = which(fstats==fmin) # find the column where our fmin
										# occurs
	}
	bestfit = fit2
	
}
ans = backwardSelect(x,y)
summary(ans)



#problem 3
require("MASS")
library(quantreg)
x<-as.matrix(Boston)
y<-x[,14]
y <- y-mean(y)
x<-x[,-14]



# suppose we use least square regression
# the function returns the prediction for x.new
Fitl2 <- function(x.train,y.train,x.new){
	fit = lm(y.train ~ x.train)
	newx = x.new
	ynew = predict(fit,newdata=data.frame(x.train=newx))
}
# we can do another fit with l1
# the function returns the prediction for x.new


Fitl1 <- function(x.train,y.train,x.new){
	fit = rq(y.train ~ x.train)
	newx =x.new
	ynew = predict(fit, newdata= data.frame(x.train = newx))
}

# it will give the average err of least square regression
subsampleSelect<- function(x,y,fit ,p=0.5,B=99){
	err_total = NULL
	for (i in 1:B){
		num = nrow(x)
		subnum = as.integer(num*0.5)
		index = sample(num,subnum)
		
		
		# set up training x, validation, x and y
		x1 = training = x[index,]
		x2 = validation = x[-index,]
		ytraining = y[index]
		yvalidation = y[-index]
		
		ynew = fit(x1,ytraining,x2)
		err = (1/nrow(x2))*sum((yvalidation-ynew)^2)
		err_total = c(err_total,err)
	}
	avg_err = mean(err_total)
}
ansl1 <- subsampleSelect(x,y,Fitl1)
ansl1
# average 142.234
ansl2 <- subsampleSelect(x,y,Fitl2)
ansl2
# average 148.766

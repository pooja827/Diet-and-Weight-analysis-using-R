a<-read.csv("D:\\3 sem\\CAT\\R CAT andProject\\DietData.csv")
print(a)

#Markov Chain Monte Carlo
m<-0
s<-1
set.a<-1
sample<-rnorm(10000,m,s)
mean(sample)
summary(replicate(1000,mean(rnorm(10000,m,s))))

#CUMSUM
cummean <- function(x)
  cumsum(x) / seq_along(x)
plot(cummean(sample), type="l", xlab="Sample", ylab="Cumulative mean",
     panel.first=abline(h=0, col="red"), las=1)

#linear regression for weight loss
lmDiet.type<-lm(weight.loss~Diet.type,data = diet)
print(summary(lmDiet.type))
plot(Height~Finalweight,data = diet)

#linear regression to predict weight
lmHeight<-lm(Finalweight~Height,data = diet)
a<-data.frame(Height = 134)
result <- predict(lmHeight,a)
print(result)


# TIMESERIES
t<-read.csv("D:\\3 sem\\CAT\\R CAT andProject\\DIET AND WEIGHT TS.csv")
print(t)
mt <- c(t$Initial_weight,t$Final_weight)
mts <- ts(mt, frequency = 365.25 / 7)
print(mts)
is.ts(mts)
plot(mts)

#auto correlation
acf(mts)

#auto regression
AR <- arima(mts, order = c(1,0,0))
print(AR)
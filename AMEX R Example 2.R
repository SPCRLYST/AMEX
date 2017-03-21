#####Reading in data
library(ggplot2)
library(RColorBrewer)
library(zoo)
library(RCurl)
library(plyr)
library(reshape2)
library(lubridate)
library(scales)
library(grid)
library(gridExtra)
library(TTR)
library(neuralnet)
library(dplyr)
library(boot)

#Seed number
seed = 33

#econ data
ECONURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/NeuralProj/master/MacroF.csv")
ECONdata <- read.csv(text = ECONURL)
ECONdata <- ECONdata[c('Year','Year1REAL.GDP','Year1ConsSpending','Year1EquipInvest','Year1NonresCons',
                       'Year1ResCons','Year1CPI','Year1Unemp')] 
#coupon data
NNURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/NeuralProj/master/NeuralNetProj.csv")
NNdata <- read.csv(text = NNURL)
str(NNdata)
names(NNdata)

#merge econ and nndata
NNdata <- merge(NNdata,ECONdata,by.x = "Year", by.y = "Year")

#converting factor data to numeric data
NNdata$YearROE <- as.numeric(NNdata$YearROE) 
NNdata$Year2ROE <- as.numeric(NNdata$Year2ROE) 
NNdata$Year3ROE <- as.numeric(NNdata$Year3ROE) 
NNdata$YearDebt_EBITDA <- as.numeric(NNdata$YearDebt_EBITDA) 
NNdata$Year2Debt_EBITDA <- as.numeric(NNdata$Year2Debt_EBITDA) 
NNdata$Year3Debt_EBITDA <- as.numeric(NNdata$Year3Debt_EBITDA) 
NNdata$YearFFO_Debt <- as.numeric(NNdata$YearFFO_Debt) 
NNdata$Year2FFO_Debt <- as.numeric(NNdata$Year2FFO_Debt) 
NNdata$Year3FFO_Debt <- as.numeric(NNdata$Year3FFO_Debt)

#Fixing FFO_Debt and Debt_EBITDA
NNdata$YearROE <- NNdata$YearROE/100
NNdata$Year2ROE <- NNdata$Year2ROE/100
NNdata$Year3ROE <- NNdata$Year3ROE/100
NNdata$YearDebt_EBITDA <- NNdata$YearDebt_EBITDA/100
NNdata$Year2Debt_EBITDA <- NNdata$Year2Debt_EBITDA/100
NNdata$Year3Debt_EBITDA <- NNdata$Year3Debt_EBITDA/100
NNdata$YearFFO_Debt <- NNdata$YearFFO_Debt/100
NNdata$Year2FFO_Debt <- NNdata$Year2FFO_Debt/100
NNdata$Year3FFO_Debt <- NNdata$Year3FFO_Debt/100

#Revoming company name categoricals
NNdata[1:5] <- list(NULL)
names(NNdata)
NNdata[22:239] <- list(NULL)
names(NNdata)
#NNdata[22:28] <-  list(NULL)
#names(NNdata)

#Scaling the dataframe
maxs <- apply(NNdata, 2, max) 
mins <- apply(NNdata, 2, min)
scaled <- as.data.frame(scale(NNdata, center = mins, scale = maxs - mins))

#Checking for missing values may be necessary to replace missing values
apply(NNdata,2,function(x) sum(is.na(x)))

#Cross validation methods linear models
lm.fit <- glm(EBITDA~., data=NNdata)
cv.glm(NNdata,lm.fit,K=10)$delta[1]

#Cross validation NN
set.seed(seed)
cv.error <- NULL
k <- 10
#Progress bar for NN
pbar <- create_progress_bar('text')
pbar$init(k)

#Getting the column names for the neural net
n <- names(NNdata[1:28])
f <- as.formula(paste("EBITDA ~", paste(n[!n %in% "EBITDA"], collapse = " + ")))
#Iterating through cross validation for NN
for(i in 1:k){
  #setting the data frame as test and train
  index <- sample(1:nrow(NNdata),round(0.9*nrow(NNdata)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  #running the neural net
  nn <- neuralnet(f,data=train.cv,hidden=c(25),linear.output=F)
  #computing the neural net base of the test set
  pr.nn <- neuralnet::compute(nn,test.cv[,2:28])
  #denormalizing the EBITDA
  pr.nn <- pr.nn$net.result*(max(NNdata$EBITDA)-min(NNdata$EBITDA))+min(NNdata$EBITDA)
  #denormalizing the cv
  test.cv.r <- (test.cv$EBITDA)*(max(NNdata$EBITDA)-min(NNdata$EBITDA))+min(NNdata$EBITDA)
  #calculating the cv error
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  #showing progression through K sets
  pbar$step()
  par(mfrow=c(1,1))
  plot(test.cv.r,pr.nn,col='red',main='Real vs Predicted EBITDA NN',pch=18,cex=0.7)
  abline(0,1,lwd=2)
  legend('bottomright',legend='NN',pch=18,col='red', bty='n')
}

mean(cv.error)

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

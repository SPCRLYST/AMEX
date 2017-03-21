library(RCurl)
# load the data
charityURL <- getURL("https://raw.githubusercontent.com/SPCRLYST/AMEX/master/charity.csv")
charity <- read.csv(text = charityURL)

# predictor transformations
charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed
# set up data for analysis
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995
data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999
data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]
x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1
x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1
x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)
# data exploration
library(corrplot)
charitycorcat <- cor(data.train.std.c)
corrplot(charitycorcat, method="shade")
charitycorcon <- cor(data.train.std.y)
corrplot(charitycorcon, method="shade")
# Basic Scatterplot Matrix for donor variable
pairs(~donr + reg1 + reg2 + reg3 + reg4 + home + chld + hinc,data=data.train.std.c,
      main="Simple Scatterplot Matrix")
pairs(~donr + genf + wrat + avhv + incm + inca + plow + npro,data=data.train.std.c,
      main="Simple Scatterplot Matrix")
pairs(~donr + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.c,
      main="Simple Scatterplot Matrix")
# Basic Scatterplot Matrix for amount variable
pairs(~damt + reg1 + reg2 + reg3 + reg4 + home + chld + hinc,data=data.train.std.y,
      main="Simple Scatterplot Matrix")
pairs(~damt + genf + wrat + avhv + incm + inca + plow + npro,data=data.train.std.y,
      main="Simple Scatterplot Matrix")
pairs(~damt + tgif + lgif + rgif + tdon + tlag + agif,data=data.train.std.y,
      main="Simple Scatterplot Matrix")

##### CLASSIFICATION MODELING ######
# linear discriminant analysis
library(MASS)
set.seed(1)
model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()
post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] 
# n.valid.c post probs
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
# this is used to help guage profit with just classification variables
profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5
cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5
(675+985)/2018

# logistic regression
model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))
post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5
cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5
(709+981)/2018

# tree classification model
library(ISLR)
library(tree)
set.seed(1)
#new training data set created to deal with 0 and 1 as factors
tree.data.train.std.c <- data.train.std.c
tree.data.train.std.c$donr <-as.factor(tree.data.train.std.c$donr)
model.tree1 <- tree(donr ~ ., tree.data.train.std.c) 
summary(model.tree1)
plot(model.tree1)
text(model.tree1, pretty = 0)
#new validation data set created to deal with 0 and 1 as factors
tree.data.valid.std.c <- data.valid.std.c
tree.data.valid.std.c$donr <- as.factor(tree.data.valid.std.c$donr)
post.valid.tree1 <- predict(model.tree1, tree.data.valid.std.c)[,2] #tree predictions
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.tree1 <- cumsum(14.5*c.valid[order(post.valid.tree1, decreasing=T)]-2)
plot(profit.tree1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.tree1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree1)) # report number of mailings and maximum profit
# 1362 11413.5
cutoff.tree1 <- sort(post.valid.tree1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.tree1 <- ifelse(post.valid.tree1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.tree1, c.valid)
#               c.valid
#chat.valid.tree1  0   1
#              0 667   32
#              1 352   967
# check n.mail.valid = 352+967 = 1319
# check profit = 14.5*967-2*1319 = 11383.5
# This is slightly off from the earlier calculations, by about 30 mailings, regardless, the value is still
# less than the other methods used and the total profit is also fairly close.I think this may also be caused
# by the seed I set.
cv.post.valid.tree1 = cv.tree(model.tree1, FUN = prune.tree)
plot(cv.post.valid.tree1$size, cv.post.valid.tree1$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")
# results indicate 10 is the best number of branches
model.tree1.pruned = prune.tree(model.tree1, best = 15)
summary(model.tree1.pruned)
# No change in the pruned tree so model.tree1 will work just fine. The misclassification error rate is
# the same for the pruned and standard tree.
(667+967)/2018

# random forest classification model
library(randomForest)
set.seed(1)
rf.model = randomForest(donr ~ ., data =  tree.data.train.std.c, mtry = 2)
post.valid.rf = predict(rf.model, newdata = tree.data.valid.std.c)
summary(post.valid.rf)
plot(post.valid.rf)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2
profit.rf <- cumsum(14.5*c.valid[order(post.valid.rf, decreasing=T)]-2)
plot(profit.rf) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf)) # report number of mailings and maximum profit
# 1098 11642.5
table(post.valid.rf, tree.data.valid.std.c$donr)
#               c.valid
#post.valid.rf     0   1
#              0 850   70
#              1 169   929
# check n.mail.valid = 169+929 = 1098
# check profit = 14.5*929-2*1098 = 11642.5
(850+929)/2018

# neural net classification model
library(neuralnet)
set.seed(1)
n <- names(data.train.std.c[1:20])
f <- as.formula(paste("donr ~", paste(n[!n %in% "donr"], collapse = " + ")))
train.cv <- data.train.std.c
test.cv <- data.valid.std.c
#running the neural net
nn.model <- neuralnet(f,data=train.cv,hidden=c(20,10),linear.output=F)
#computing the neural net base of the test set
pr.nn <- neuralnet::compute(nn.model,test.cv[,1:20])
profit.nn <- cumsum(14.5*c.valid[order(pr.nn$net.result, decreasing=T)]-2)
plot(profit.nn)
n.mail.valid <- which.max(profit.nn) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.nn)) # report number of mailings and maximum profit
# 1388 11593.5
cutoff.nn <- sort(pr.nn$net.result, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.nn <- ifelse(pr.nn$net.result>cutoff.nn, 1, 0) # mail to everyone above the cutoff
table(chat.valid.nn, c.valid) 
#               c.valid
#post.valid.rf     0   1
#              0 622   8
#              1 397   991
# check n.mail.valid = 397+991 = 1388
# check profit = 14.5*991-2*1388 = 11593.5
(622+991)/2018

# Results for classification selection
# n.mail Profit  Model        Classification Error
# 1329   11624.5 model.lda1   0.823
# 1291   11642.5 model.log1   0.837
# 1319   11383.5 model.tree1  0.810 
# 1098   11642.5 rf.model     0.882
# 1388   11593.5 nn.model     0.799

# classification model selection
# select model.log1 since it has maximum profit in the validation sample
post.test <- predict(model.log1, data.test.std) # post probs for test data
# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid <- which.max(model.log1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set
cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1 
# 1772  235
# based on this model we'll mail to the 235 highest posterior probabilities

# classification model selection
# select rf.model since it also has maximum profit in the validation sample and the best 
# classification rate
post.test2 <- predict(rf.model, data.test.std) # post probs for test data
chat.test2 <- post.test2
# Oversampling adjustment for calculating number of mailings for test set
table(post.test2)
#    0    1 
# 1553  454
# based on this model we'll mail to the 454 highest results from the random forest


##### PREDICTION MODELING ######
# Least squares regression
model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)
pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
hist(pred.valid.ls1)
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1696615

# Ridge regression
library(glmnet)
xf = model.matrix(damt~.,data.train.std.y)[,-1] 
yf = data.frame(data.train.std.y)
yf = yf$damt
grid = 10^seq(10,-2, length =100) 
ridge.mod = glmnet(xf,yf,alpha = 0, lambda =grid) 
plot(ridge.mod)
set.seed(1) 
cv.out=cv.glmnet(xf,yf,alpha=0) 
plot(cv.out) 
bestlam =cv.out$lambda.min  
bestlam
xt = model.matrix(damt~.,data.valid.std.y)[,-1]
ridge.pred=predict(ridge.mod,s=bestlam,newx=xt)
hist(ridge.pred)
mean((y.valid - ridge.pred)^2) # mean prediction error
# 1.8732309
sd((y.valid - ridge.pred)^2)/sqrt(n.valid.y) # std error
# 0.1711169

# GAM multiple variable models
library(gam)  
gam.mod=gam(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
             wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + 
             tdon + tlag + agif , data=data.train.std.y)
gam.pred <- predict(gam.mod,newdata = data.valid.std.y)
hist(gam.pred)
mean((y.valid - gam.pred)^2) # mean prediction error
# 1.8675230
sd((y.valid - gam.pred)^2)/sqrt(n.valid.y) # std error
# 0.1696615

# M5P Regression Tree variable model
library(RWeka)
M5P.mod <- M5P(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + 
                  tdon + tlag + agif , data=data.train.std.y)
M5P.mod
summary(M5P.mod)
M5P.pred <- predict(M5P.mod,newdata = data.valid.std.y)
hist(M5P.pred)
mean((y.valid - M5P.pred)^2) # mean prediction error
# 1.6786359
sd((y.valid - M5P.pred)^2)/sqrt(n.valid.y) # std error
# 0.1635452

# Random Forest used to predict amount
library(randomForest)
set.seed(1)
rf2.model = randomForest(damt ~ ., data =  data.train.std.y, mtry = 7)
post.valid.rf2 = predict(rf2.model, newdata = data.valid.std.y)
hist(post.valid.rf2)
mean((y.valid - post.valid.rf2)^2) # mean prediction error
# 1.6541946
sd((y.valid - post.valid.rf2)^2)/sqrt(n.valid.y) # std error
# 0.1723719

# Results for regression
# MPE   StdE  Model
# 1.868 0.170 model.ls1
# 1.867 0.170 ridge.mod
# 1.868 0.170 gam.mod
# 1.679 0.164 M5P.mod (regression tree)
# 1.654 0.172 rf2.model

# select model.ls2 since it has minimum mean prediction error in the validation sample
yhat.test <- predict(M5P.mod, newdata = data.test.std) # test predictions

# FINAL RESULTS
# Save final results for both classification and regression
length(chat.test2) # check length = 2007
length(yhat.test) # check length = 2007
chat.test2[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt
ip <- data.frame(chat=chat.test2, yhat=yhat.test) # data frame with two variables: chat and yhat
print(ip)

library(ISLR)
library(rpart)
library(cluster)
library(fpc)
library(scales)
library(ggplot2)
library(lattice)
library(caret)
library(boot)
library(plyr)

#pulling in German Credit data set
data(GermanCredit)
set.seed(1)

#separating data to test and training data sets
ratio_sep <- 0.632
index <- sample(1:nrow(GermanCredit),round(ratio_sep*nrow(GermanCredit)))
train.gc <- GermanCredit[index,]
test.gc <- GermanCredit[-index,]

#single lm model, no bootstrapping used
#fitting lm to training data set
lm.gcfit <- lm(Amount~., data=train.gc)

#predicting amount using test data set
pred.gc <- predict(lm.gcfit, newdata = test.gc) 

#coefficients from the training data sets
gctrain_coef <- summary(lm.gcfit,data=train.gc)$coef

#r squared figures from training and test data sets
train_gcrsqr <- summary(lm.gcfit)$r.squared
hold_gcrsqr <- cor(test.gc$Amount, pred.gc)^2 

# Bootstrap 95% CI for R-Squared
# function to obtain R-Squared from the training data
rsq <- function(formula, data, indices) {
  d <- data[indices,] 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
# function to obtain R-Squared from the test data
rsq2 <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  pred <- predict(fit, data=d)
  return(cor(d$Amount, pred)^2)
}
# function to obtain coefs from the training data
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
}
# bootstrapping with 1000 replications for rsqr training and test
rsqresults1 <- boot(data=train.gc, statistic=rsq,
                R=1000, formula=Amount~.)
rsqresults2 <- boot(data=test.gc, statistic=rsq2,
                 R=1000, formula=Amount~.)
# bootstrapping with 1000 replications for coefs
results <- boot(data=train.gc, statistic=bs,
                R=1000, formula=Amount~.)

# view results for r square training data
rsqresults1
plot(rsqresults1)
# get 95% confidence interval
boot.ci(rsqresults1, type="bca")
#view results for r square test data
rsqresults2
plot(rsqresults2)
# get 95% confidence interval
boot.ci(rsqresults2, type="bca")

#difference in rsq from training and test
diff_rsq <- ((rsqresults1$t - rsqresults2$t)/rsqresults1$t)
hist(diff_rsq, breaks = 100)

# view results, also shows which values are na, used in next section for getting confidence intervals
results
#loop to plot all coefs
for(i in 1:62){
  plot(results, index=i) 
}
# loop to get 95% and 5% confidence intervals
n <- 62
mat <- matrix(ncol=2, nrow=n)
mat <- data.frame(mat)
confint <- 0.95
for(i in 1:n){
  if(i == 14 | i == 19 | i == 27 | i == 30 | i == 35 | i == 40 | i == 44 | 
     i == 45 | i == 48 | i == 52 | i == 55 | i == 58 | i == 62) next
  interv <- boot.ci(results, conf = confint, type="bca", index=i)
  mat[i,] <- interv$bca[,4:5]
}
# loop to get 75% and 25% confidence intervals
mat2 <- matrix(ncol=2, nrow=n)
mat2 <- data.frame(mat2)
confint2 <- 0.75
for(i in 1:n){
  if(i == 14 | i == 19 | i == 27 | i == 30 | i == 35 | i == 40 | i == 44 | 
     i == 45 | i == 48 | i == 52 | i == 55 | i == 58 | i == 62) next
  interv <- boot.ci(results, conf = confint2, type="bca", index=i)
  mat2[i,] <- interv$bca[,4:5]
}
#model created using all data to compare against bootsrapped values
all.gcfit <- lm(Amount~., data=GermanCredit)
allmat <- data.frame(all.gcfit$coefficients)
#creating a matrix of all confidence levels and the model given coeff(means)
matrix <- cbind(data.frame(results$t0),mat,mat2,allmat)
names(matrix)[1] <- "Coefficient Value"
names(matrix)[2] <- "95% conf interval"
names(matrix)[3] <- "5% conf interval"
names(matrix)[4] <- "75% conf interval"
names(matrix)[5] <- "25% conf interval"
names(matrix)[6] <- "All Data Model"
matrix <- round(matrix[,c(2,4,1,5,3,6)],digits = 3)
print(matrix)

#clustering the data to understand drivers better
GermanCredit$fixClass <- ""
GermanCredit$fixClass[GermanCredit$Class == 'Bad'] <- c(0)
GermanCredit$fixClass[GermanCredit$Class == 'Good'] <- c(1)
GermanCredit$Class <- NULL
#scaling large variables
GermanCredit$Duration <- scale(GermanCredit$Duration)
GermanCredit$Amount <- scale(GermanCredit$Amount)
GermanCredit$Age <- scale(GermanCredit$Age)
#building clustering for credit
GC_Clust <- kmeans(GermanCredit[,c(2,5)], centers=5, nstart = 20)
as.data.frame(GC_Clust$cluster)
as.data.frame(GC_Clust$centers)
GC_table <- table(GC_Clust$cluster, GermanCredit$fixClass)
GC_table
#percent cluster that is bad versus good in a cluster
Good_Bad <- as.data.frame(GC_table[,1]/(GC_table[,1]+GC_table[,2]))
names(Good_Bad)
names(Good_Bad)[names(Good_Bad)=="GC_table[, 1]/(GC_table[, 1] + GC_table[, 2])"] <- "Bad Account%"
Good_Bad
#plotting clusters
GC_Clust$cluster <- as.factor(GC_Clust$cluster)
ggplot(GermanCredit, aes(GermanCredit$Amount, GermanCredit$Age, color = GC_Clust$cluster)) + 
  geom_point(aes(shape=as.factor(GermanCredit$fixClass))) +
  labs(title = "Account Type: Clustering of Card Amount & Age", 
       x = "\nAmount on Card", 
       y = "Age", 
       color = "Clusters",
       shape = "Bad (0) or Good (1)") +
  theme_minimal()+
  theme(legend.position = "bottom",
        text = element_text(size = 11))



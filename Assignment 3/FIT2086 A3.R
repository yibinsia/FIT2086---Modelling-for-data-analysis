setwd("D:Downloads")
# Question 1
# Q1.1
housing.data = read.csv("housing.2023.csv") 
lm_model = lm(medv ~ ., data = housing.data)
summary(lm_model)

# Q1.2
pvalues = coefficients(summary(lm_model))[,4]
pvalues < (0.05/12)

# Q1.3
coefficients(summary(lm_model))[2,1] #crim
coefficients(summary(lm_model))[5,1] #chas

# Q1.4
step.fit.bic = step(lm_model, k = log(nrow(housing.data)), direction = "both", trace = FALSE)
summary(step.fit.bic)

# Q1.6
new_suburb_data <- data.frame(
  crim = 0.04741,
  zn = 0,
  indus = 11.93,
  chas = 0,
  nox = 0.573,
  rm = 6.03,
  age = 80.8,
  dis = 2.505,
  rad = 1,
  tax = 273,
  ptratio = 21,
  lstat = 7.88
)

predict(lm_model, newdata = new_suburb_data, interval = "confidence", type = "response", level = 0.95) 

# Q1.7
lm_interaction_model <- lm(medv ~ crim + chas + nox + rm * dis + ptratio + lstat, data = housing.data)
summary(lm_interaction_model)

# Question 2
# Q2.1
heart.train = read.csv("heart.train.2023.csv", header = TRUE, stringsAsFactors = TRUE)
library(rpart)
source("wrappers.R")
tree.heart = rpart(HD ~ ., heart.train)
tree.heart.cv = learn.tree.cv(HD ~ ., data = heart.train, nfolds = 10, m = 5000)
tree.heart.cv$best.tree

# Q2.2
plot(tree.heart.cv$best.tree)
text(tree.heart.cv$best.tree, pretty = 12)

# Q2.3
plot(tree.heart)
text(tree.heart, pretty = 12)
tree.heart

# Q2.5
lgm = glm(HD ~ ., data = heart.train, family = "binomial")
step.fit.bic = step(lgm, k = log(nrow(heart.train)), direction = "both", trace=0)
summary(step.fit.bic)

# Q2.7
heart.test = read.csv("heart.test.2023.csv", header=TRUE, stringsAsFactors= TRUE)
source("my.prediction.stats.R")
my.pred.stats(predict(tree.heart, heart.test)[,2], heart.test$HD)
my.pred.stats(predict(step.fit.bic, heart.test, type = "response"), heart.test$HD)

# Q2.8
odds_cv = predict(tree.heart.cv$best.tree, heart.test[69,])[,2]/(1-predict(tree.heart.cv$best.tree, heart.test[69,])[,2])
odds_stepwise = predict(step.fit.bic, heart.test[69,],type = "response")/(1-predict(step.fit.bic,heart.test[69,],type="response"))

# Q2.9
library(boot)
heart.train = read.csv("heart.train.2023.csv", header = TRUE, stringsAsFactors = TRUE)
heart.test = read.csv("heart.test.2023.csv", header=TRUE, stringsAsFactors= TRUE)
lgm = glm(HD ~ ., data = heart.train, family = binomial)
step.fit.bic = step(lgm, k= log(nrow(heart.train)), direction = "both", trace = FALSE)
boot.odds69 = function(formula, data, test, indices)
{
  d = data[indices,]
  fit = glm(formula, d, family = binomial)
  y_pred= predict(fit, test[69,], type = "response")
  return(y_pred)
}
bs.odds69 = boot(data=heart.train, test=heart.test, statistic=boot.odds69, 5000,formula= HD ~ .)
bs.ci69 = boot.ci(bs.odds69, 0.95, type = "bca")
bs.ci69

# Question 3
# Q3.1
library(kknn)
ms.measure = read.csv("ms.measured.2023.csv")
ms.truth = read.csv("ms.truth.2023.csv")
x = replicate(25,0)
y = (1:25)
for(i in 1:25){
  knn = train.kknn(intensity ~ MZ, ms.measure, kmax = 25 , kernel = "optimal")
  est = fitted(kknn(intensity ~ MZ, ms.measure, ms.truth, kernel = "optimal", k = i))
  x[i] = mean((est - ms.truth$intensity)**2)
}
plot(y,x, main = "Plot of MSE against various values of k", xlab = "k", ylab = "MSE")

# Q3.2
knn = train.kknn(intensity ~ MZ, ms.measure, kmax =25, kernel ="optimal")
est2 = fitted(kknn(intensity ~ MZ, ms.measure, ms.truth, kernel = "optimal", k = 2))
est5 = fitted(kknn(intensity ~ MZ, ms.measure, ms.truth, kernel = "optimal", k = 5))
est10 = fitted(kknn(intensity ~ MZ, ms.measure, ms.truth, kernel = "optimal", k = 10))
est25 = fitted(kknn(intensity ~ MZ, ms.measure, ms.truth, kernel = "optimal", k = 25))
# k=2
plot(ms.truth$MZ, est2, main = "Graph of k=2", xlab = "MZ", ylab = "Intensity")
lines(ms.measure, col= "red")
lines(ms.truth, col = "blue")
legend(x = "topright", c("Estimated Spectrum", "Training Data", "True Spectrum"), fill = c("black","red","blue"))
# k=5
plot(ms.truth$MZ, est5, main = "Graph of k=5", xlab = "MZ", ylab = "Intensity")
lines(ms.measure, col= "red")
lines(ms.truth, col = "blue")
legend(x = "topright", c("Estimated Spectrum", "Training Data", "True Spectrum"), fill = c("black","red","blue"))
# k=10
plot(ms.truth$MZ, est10, main = "Graph of k=10", xlab = "MZ", ylab = "Intensity")
lines(ms.measure, col= "red")
lines(ms.truth, col = "blue")
legend(x = "topright", c("Estimated Spectrum", "Training Data", "True Spectrum"), fill = c("black","red","blue"))
# k=25
plot(ms.truth$MZ, est25, main = "Graph of k=25", xlab = "MZ", ylab = "Intensity")
lines(ms.measure, col= "red")
lines(ms.truth, col = "blue")
legend(x = "topright", c("Estimated Spectrum", "Training Data", "True Spectrum"), fill = c("black","red","blue"))

# Q3.5
knn$best.parameters$k

# Q3.6
est = fitted(kknn(intensity ~ MZ, ms.measure, ms.truth, kernel="optimal", k=6))
err = ms.measure$intensity - est
sd(err)

# Q3.7
est = fitted(kknn(intensity ~ MZ, ms.measure, ms.truth, kernel="optimal", k=6))
n = length(est)
for(i in 1:n){
  if( est[i] == max(est)){
    index = i
    break
  }
}
ms.truth[index,]$MZ
index

# Q3.8
library(boot)
boot.est6 = function(data, truth, indices, index)
{
  #Create a bootstrapped version of the data
  d = data[indices,]
  
  return (fitted(kknn(intensity ~ MZ, d, truth[index,], kernel="optimal", k=6)))
}
bs.est = boot(data=ms.measure, truth=ms.truth, statistic=boot.est6, index=283, 5000)
bs.ci = boot.ci(bs.est, 0.95, type="bca")
bs.ci

boot.est3 = function(data, truth, indices, index)
{
  #Create a bootstrapped version of the data
  d = data[indices,]
  
  return (fitted(kknn(intensity ~ MZ, d, truth[index,], kernel="optimal", k=3)))
}
bs.est3 = boot(data=ms.measure, truth=ms.truth, statistic=boot.est3, index=283, 5000)
bs.ci3 = boot.ci(bs.est3, 0.95, type="bca")
bs.ci3

boot.est20 = function(data, truth, indices, index)
{
  #Create a bootstrapped version of the data
  d = data[indices,]
  
  return (fitted(kknn(intensity ~ MZ, d, truth[index,], kernel="optimal", k=20)))
}
bs.est20 = boot(data=ms.measure, truth=ms.truth, statistic=boot.est20, index=283, 5000)
bs.ci20 = boot.ci(bs.est20, 0.95, type="bca")
bs.ci20

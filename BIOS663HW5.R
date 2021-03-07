
library(glmnet)
dat.train = read.csv(file = "c_train.csv", header = TRUE)
dat.test = read.csv(file = "c_test.csv", header = TRUE)
test.x <- as.matrix(subset(dat.test, select = -Grad.Rate))
test.x<- apply(test.x[,-1],2,as.numeric)
test.y<- as.numeric(dat.test$Grad.Rate)
train.x <- as.matrix(subset(dat.train, select = -Grad.Rate))
train.x <- apply(train.x[,-1],2,as.numeric)
train.y <- as.numeric(dat.train$Grad.Rate)

fit = lm(Grad.Rate ~ Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad
         + P.Undergrad + Outstate + Room.Board + Books + Personal + PhD + Terminal
         + S.F.Ratio + perc.alumni + Expend, data = dat.train)
summary(fit)$r.squared

y.hat<-predict(fit, newdata = dat.train)
cor(y.hat, train.y, method = "pearson")^2
y.hat.test <- predict(fit, newdata = dat.test)
cor(y.hat.test, test.y, method = "pearson")^2

set.seed(1)
ridge.mod <- cv.glmnet(train.x, train.y, alpha = 0, nfolds=5)
y.hat.ridge <- predict(ridge.mod, newx = train.x, s = "lambda.min")
cor(y.hat.ridge, train.y, method  = "pearson")^2
y.hat.ridge.test <- predict(ridge.mod, newx = test.x, s="lambda.min")
cor(y.hat.ridge.test, test.y, method = "pearson")^2                  

set.seed(1)
lasso.mod <- cv.glmnet(train.x, train.y, alpha = 1, nfolds=5)
y.hat.lasso <- predict(lasso.mod, newx = train.x, s = "lambda.min")
cor(y.hat.lasso, train.y, method  = "pearson")^2
y.hat.lasso.test <- predict(lasso.mod, newx = test.x, s="lambda.min")
cor(y.hat.lasso.test, test.y, method = "pearson")^2       
coef(lasso.mod, s = "lambda.min")

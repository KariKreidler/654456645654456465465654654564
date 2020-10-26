nlsy <- read.csv("~/Neuer Ordner/nlsy.csv")
View(nlsy)

#Aufgabe 2 a

set.seed(748)

train_index <- sample(1:nrow(nlsy), 45/935 * nrow(nlsy))
test_index <- setdiff(1:nrow(nlsy), train_index)

X_train <- nlsy[train_index, -15]

X_test <- nlsy[test_index, -15]

#b

plot(X_train$iq, X_train$lnearn)

fit = lm(X_train$lnearn~X_train$iq)

fit_summ = summary(fit)

abline(fit)

#train_MSE

train_MSE = mean(fit_summ$residuals^2)

#test_MSE

fit2 = lm(X_test$lnearn~X_test$iq)

fit2_summ = summary(fit2)

test_MSE = mean(fit2_summ$residuals^2)

#train R^2

Train_rsq = function(x, y) fit_summ$r.squared

Train_rsq(obs, mod)

#Test R^2

Test_rsq = function(x,y) fit2_summ$r.squared

Test_rsq(obs,mod)


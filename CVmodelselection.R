############# libraries - only needed for fancy plotting ############
library(dplyr)
library(ggformula)
library(GGally)

############# Read in data and view #############
bodyfat = read.csv("bodyfat.csv")
#View(bodyfat)

dim(bodyfat)
n = dim(bodyfat)[1]
names(bodyfat)  # using BodyFatSiri as response, versus Density or BodyFatBrozek
# scatterplot matrix of response and sets of 7 predictors
pairs(bodyfat[,c(5:11,3)])  # using base-R
theme_set(theme_grey(base_size=8))
ggpairs(bodyfat[,c(5:11,3)],
        upper = list(continuous = wrap("cor", size = 2))
        ) 
ggpairs(bodyfat[,c(12:18,3)],
        upper = list(continuous = wrap("cor", size = 2))
        )  

############# Linear Model specifications #############
Model1 = (BodyFatSiri ~ Weight+BMI+Height)
Model2 = (BodyFatSiri ~ Abs+Neck+Chest)
Model3 = (BodyFatSiri ~ Weight+BMI+Height+Abs+Neck+Chest)
Model4 = (BodyFatSiri ~ Wrist+Forearm+Biceps+Abs+Neck+Chest)
Model5 = (BodyFatSiri ~ Thigh+Hip+Ankle+Knee)
Model6 = (BodyFatSiri ~ . - Case - BodyFatBrozek - Density)
#Model6 = (BodyFatSiri ~ Age+Weight+Height+BMI+Neck+Chest+Abs+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist)

############# Define labels for cross-validation groups, cvgroups #############

# LOO CV
# nfolds=n
# cvgroups = (1:n) 

# 10-fold CV
 nfolds=10
 set.seed(2)
 groups = rep(1:nfolds,length=n)  # nfolds is number of folds
 cvgroups = sample(groups,n)  

############# CV process for one of the models #############

allpredicted = rep(NA,n)   # storage for honest predictions

# loop to do cross-validation

 for (ii in 1: nfolds) {    # ii is an easier string to search for index
   groupii = (cvgroups == ii)     # logical vector for group ii
   trainset = bodyfat[!groupii,]  # all data EXCEPT for group ii
   testset = bodyfat[groupii, ]   # data in group ii
   
   modelfit = lm(Model1, data=trainset) # fit to train set
   
   predicted = predict(modelfit, newdata = testset)   # predict for test set
   allpredicted[groupii] = predicted              # store in ordered locations
 }
 

y = bodyfat$BodyFatSiri
CVvalue1 = mean((y - allpredicted)^2); CVvalue1

obs.pred = data.frame(y = y, yhat = allpredicted)
plot(allpredicted, y, title = "Compare Observed to Predicted for Model 1",
     ylab = "Observed", xlab = "Predicted") # base-R
gf_point(y ~ yhat, data = obs.pred) %>%
  gf_labs(title = "Compare Observed to Predicted for Model 1",
          y = "Observed", x = "Predicted")

############# CV process simplified, use m as index for model  #############
allModels = list(Model1,Model2,Model3,Model4,Model5,Model6)
allCVvalues = rep(NA,length(allModels))

m = 1

# revised loop to do cross-validation
allpredicted = rep(NA,n)   # storage for honest predictions

# loop to do cross-validation

for (ii in 1: nfolds) {    # ii is an easier string to search for index
  groupii = (cvgroups == ii)     # logical vector for group ii
  trainset = bodyfat[!groupii,]  # all data EXCEPT for group ii
  testset = bodyfat[groupii, ]   # data in group ii
  
  modelfit = lm(allModels[[m]], data=trainset) # fit to train set
  
  predicted = predict(modelfit, newdata = testset)   # predict for test set
  allpredicted[groupii] = predicted              # store in ordered locations
}


y = bodyfat$BodyFatSiri
allCVvalues[m] = mean((y - allpredicted)^2); allCVvalues

obs.pred = data.frame(y = y, yhat = allpredicted)
plot(allpredicted, y, main = paste("Compare Observed to Predicted for Model",m),
     ylab = "Observed", xlab = "Predicted") # base-R
gf_point(y ~ yhat, data = obs.pred) %>%
  gf_labs(title = paste("Compare Observed to Predicted for Model",m),
          y = "Observed", x = "Predicted")

############# CV process streamlined, loop through m as index for model  #############

allplots = list()

# for-loop to cycle through models

# revised loop to do cross-validation
for (m in 1:length(allModels)) {
  allpredicted = rep(NA,n)   # storage for honest predictions
  
  for (ii in 1: nfolds) {    # ii is an easier string to search for index
    groupii = (cvgroups == ii)
    trainset = bodyfat[!groupii,]  # all data EXCEPT for group ii
    testset = bodyfat[groupii, ]   # data in group ii
    modelfit = lm(allModels[[m]], data=trainset) # fit to train set
    predicted = predict(modelfit, newdata = testset)   # predict for test set
    allpredicted[groupii] = predicted              # store in ordered locations
  }
  
  y = bodyfat$BodyFatSiri
  CVvalue = mean((y - allpredicted)^2); CVvalue  
  allCVvalues[m] = CVvalue
  
  obs.pred = data.frame(y = y, yhat = allpredicted)
  allplots[[m]] <- gf_point(y ~ yhat, data = obs.pred) %>%
    gf_labs(title = paste("Compare Observed to Predicted for Model", m),
            y = "Observed", x = "Predicted")
  
}

allCVvalues  # Model 3 is "best"
allplots[[3]]


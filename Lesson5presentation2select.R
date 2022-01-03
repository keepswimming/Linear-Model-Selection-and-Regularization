############# libraries used ############
# for methods
library(glmnet)  #help(glmnet)
# for assumption-checking / error estimation
library(boot)  
# for data organization
library(dplyr)
# for visuals
library(plotmo)
library(ggplot2)

############# Read in and Review data #############
#make data frame from original data set
bodyfat.orig = read.csv("bodyfat.csv")
#View(bodyfat.orig)
names(bodyfat.orig)  # using BodyFatSiri as response, versus Density or BodyFatBrozek

# [OPTIONAL] define a new dataframe with the variables we want, 
#  response at the beginning
bodyfat <- bodyfat.orig %>%
  select(-Case,-Density,-BodyFatBrozek)
names(bodyfat)
# dimensions of full data with response and predictors
dim(bodyfat)

# fit models
x = model.matrix(BodyFatSiri~.,data=bodyfat)[,-1]
y = bodyfat$BodyFatSiri
n = dim(x)[1]
p = dim(x)[2]

#fit response on all predictors, using multiple linear regression
REGfit = lm(y~x)  
summary(REGfit)  

############# Using cv.glmnet for cross-validation  #############
# Define labels for cross-validation groups, cvgroups, with 10-fold CV
nfolds=10
set.seed(5)
groups = rep(1:nfolds,length=n)  # nfolds is number of folds
cvgroups = sample(groups,n)  

# storage for results
allalpha <- c(0,1,0.95,0.50)
bestlambda <- rep(NA,4)
minCVbestlambda <- rep(NA,4)

# fit penalized regression using all predictors for many different lambda values
#lambdalist = exp((-1200:1200)/100)  # denser array
#lambdalist = exp((-120:120)/10)  # less dense array
lambdalist = exp((-1200:100)/10)  # consider lower max value of penalty

# apply cross-validation to various RR model fits
cvRRglmnet = cv.glmnet(x, y, lambda=lambdalist, alpha = 0, 
                       nfolds=nfolds, foldid=cvgroups)
# examine cross-validation results
plot(cvRRglmnet)  # vertical lines at lambda.min and lambda.1se
cvRRglmnet$lambda.min; log(cvRRglmnet$lambda.min)
cvRRglmnet$lambda.1se; log(cvRRglmnet$lambda.1se)
min(cvRRglmnet$cvm)
plot(cvRRglmnet,ylim=c(15,25))  # vertical lines at lambda.min and lambda.1se
# store "best" lambda and CV values
bestlambda[1] = cvRRglmnet$lambda.min
minCVbestlambda[1] = min(cvRRglmnet$cvm)

# apply cross-validation to various LASSO model fits
cvLASSOglmnet = cv.glmnet(x, y, lambda=lambdalist, alpha = 1, 
                       nfolds=nfolds, foldid=cvgroups)
# examine cross-validation results
plot(cvLASSOglmnet)  # vertical lines at lambda.min and lambda.1se
# store "best" lambda and CV values
bestlambda[2] = cvLASSOglmnet$lambda.min
minCVbestlambda[2] = min(cvLASSOglmnet$cvm)

# apply cross-validation to various ENET with alpha=0.75
cvENET95glmnet = cv.glmnet(x, y, lambda=lambdalist, alpha = 0.95, 
                          nfolds=nfolds, foldid=cvgroups)
# examine cross-validation results
plot(cvENET95glmnet)  # vertical lines at lambda.min and lambda.1se
# store "best" lambda and CV values
bestlambda[3] = cvENET95glmnet$lambda.min
minCVbestlambda[3] = min(cvENET95glmnet$cvm)

# apply cross-validation to various ENET with alpha=0.5
cvENET50glmnet = cv.glmnet(x, y, lambda=lambdalist, alpha = 0.5, 
                           nfolds=nfolds, foldid=cvgroups)
# examine cross-validation results
plot(cvENET50glmnet)  # vertical lines at lambda.min and lambda.1se
# store "best" lambda and CV values
bestlambda[4] = cvENET50glmnet$lambda.min
minCVbestlambda[4] = min(cvENET50glmnet$cvm)

# compare across all models, with variety of alpha values
minCVbestlambda
whichbest = which.min(minCVbestlambda)
onebestalpha = allalpha[whichbest]
onebestlambda = bestlambda[whichbest]

# fit selected model
Bestfit = glmnet(x, y, alpha = onebestalpha,lambda=lambdalist)
coef(Bestfit,s=onebestlambda)
plot_glmnet(Bestfit,xvar="lambda"); abline(v=log(onebestlambda))


############# Estimating standard errors with bootstrap  #############
onebestalpha; onebestlambda
lambdalist = exp((-1200:100)/10) 
#lambdalist = exp((-1200:0)/10) 

# define functions that output coefficients (parameters to be estimated)
# for the best-fitting model
beta.fn.ENET = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  ENETfitboot = glmnet(xboot, yboot, alpha = onebestalpha,lambda=lambdalist)
  return(coef(ENETfitboot,s=onebestlambda)[,1])
}

# run the boot function to simulate re-samples (with replacement)
# and obtain the coefficients for each re-sample
set.seed(5)
ENETbootoutput = boot(cbind(y,x),beta.fn.ENET,R=1000)
print(ENETbootoutput)
#(ENETbootoutput$t)[,i] is all 1000 coefficient estimates for ith term, i = 1, ..., p+1
apply(ENETbootoutput$t,2,sd)

# define functions that output coefficients for multiple linear regression model
beta.fn.REGR = function(inputdata,index) {
  yboot = inputdata[index,1]
  xboot = inputdata[index,-1]
  lmfitboot = lm(yboot ~ xboot)
  return(coef(lmfitboot))
}

# run the boot function to simulate re-samples (with replacement)
# and obtain the coefficients for each re-sample
set.seed(5)
REGRbootoutput = boot(cbind(y,x),beta.fn.REGR,R=1000)
print(REGRbootoutput)
#(REGRbootoutput$t)[,i] is all 1000 coefficient estimates for ith term, i = 1, ..., p+1
apply(REGRbootoutput$t,2,sd)

# compare variability of coefs
data.frame(REGR.SEs = round(apply(REGRbootoutput$t,2,sd),4),
           ENET.SEs = round(apply(ENETbootoutput$t,2,sd),4),
           row.names=c("intercept",names(bodyfat)[2:15]))



######################################################################
##############        [Optional] BONUS practice:        ##############
############## identify best model for TreesTransformed ##############
######################################################################

#define a new dataframe with the variables we want, response at the beginning
TreesTransformed = trees[,c(3,1,2)]  
names(TreesTransformed)

# add (using mutate) additional variables to reflect physical characteristics
TreesTransformed <- TreesTransformed %>% 
  mutate(GirthHeight=trees$Girth*trees$Height,
         Girth2=trees$Girth^2,
         Girth2Height=trees$Girth^2*trees$Height)

# predictor matrix and response
x = model.matrix(Volume~.,data=TreesTransformed)[,-1]
y = TreesTransformed$Volume
n = dim(x)[1]
p = dim(x)[2]

# Define labels for cross-validation groups, cvgroups, with 10-fold CV
nfolds=10
set.seed(5)
groups = rep(1:nfolds,length=n)  # nfolds is number of folds
cvgroups = sample(groups,n)  

# storage for results
allalpha <- c(0:100)/100
bestlambda <- rep(NA,length(allalpha))
minCVbestlambda <- rep(NA,length(allalpha))

# fit penalized regression using all predictors for many different lambda values
lambdalist = exp((-100:100)/10)  

# loop through all different alpha values considered
for (whichalpha in 1:length(allalpha)) {
  alphaused = allalpha[whichalpha]
  # apply cross-validation to various ENET with alphaused
  cvENETglmnet = cv.glmnet(x, y, lambda=lambdalist, alpha = alphaused, 
                             nfolds=nfolds, foldid=cvgroups)
  plot(cvENETglmnet)  # vertical lines at lambda.min and lambda.1se
  # store "best" lambda and CV values
  bestlambda[whichalpha] = cvENETglmnet$lambda.min
  minCVbestlambda[whichalpha] = min(cvENETglmnet$cvm)
}

# compare across all models, with variety of alpha values
minCVbestlambda; plot(allalpha,minCVbestlambda)
whichbest = which.min(minCVbestlambda)
onebestalpha = allalpha[whichbest]
onebestlambda = bestlambda[whichbest]

# fit selected model
Bestfit = glmnet(x, y, alpha = onebestalpha,lambda=lambdalist)
coef(Bestfit,s=onebestlambda)
plot_glmnet(Bestfit,xvar="lambda"); abline(v=log(onebestlambda))

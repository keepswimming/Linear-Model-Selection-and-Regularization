############# libraries used ############
# for methods
library(glmnet)  #help(glmnet)
# for assumption-checking 
# for data organization
library(dplyr)
# for visuals
library(plotmo)
library(ggplot2)

############# Review data #############
#make data frame from original data set
data(trees)
#View(trees)
names(trees)

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


############# Model fitting #############
# fit multiple regression using all predictors
REGfit = lm(y~x)  
summary(REGfit)  

# fit penalized regression using all predictors for many different lambda values
lambdalist = exp((-100:100)/10)  

# fit ridge regression - use alpha = 0
RRfit = glmnet(x, y, alpha = 0,lambda=lambdalist)
plot_glmnet(RRfit)
plot_glmnet(RRfit,xvar = "lambda")  # display the x-scale for lambda in increasing order
RRlambdaused = 1; abline(v=log(RRlambdaused)) # so log(RRlambdaused) = 0
RRcoef = coef(RRfit,s=RRlambdaused); RRcoef

# fit LASSO - use alpha = 1
LASSOfit = glmnet(x, y, alpha = 1,lambda=lambdalist)
plot_glmnet(LASSOfit,xvar = "lambda")
LASSOlambdaused = 0.2; abline(v=log(LASSOlambdaused))
LASSOcoef = coef(LASSOfit,s=LASSOlambdaused); LASSOcoef

# fit ENET - use alpha between 0 to 1
ENETfit = glmnet(x, y, alpha = 0.75,lambda=lambdalist)
plot_glmnet(ENETfit,xvar = "lambda")
ENETlambdaused = 0.4; abline(v=log(ENETlambdaused))
ENETcoef = coef(ENETfit,s=ENETlambdaused); ENETcoef

# compare true (observed) y values to predicted values
RRyhat = predict(RRfit,newx=x,s=RRlambdaused)
LASSOyhat = predict(LASSOfit,newx=x,s=LASSOlambdaused)
ENETyhat = predict(ENETfit,newx=x,s=ENETlambdaused)
obs.pred = data.frame(yobs = rep(y,3), 
                      yhat = c(RRyhat,LASSOyhat,ENETyhat),
                      model = c(rep("RR",n),rep("LASSO",n),rep("ENET",n))
                      )
ggplot(obs.pred, aes(x=yhat, y=yobs, color=model,shape=model,alpha=0.8)) + 
  geom_point(size=3) 

# compare coefficients
allcoef = data.frame(RRcoef=round(RRcoef[,1],6),
                     LASSOcoef=round(LASSOcoef[,1],6),
                     ENETcoef=round(ENETcoef[,1],6))
allcoef

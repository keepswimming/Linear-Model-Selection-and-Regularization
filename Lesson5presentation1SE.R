############# libraries used ############
# for methods
#library(glmnet)  #help(glmnet)
# for assumption-checking 
# for data organization
library(dplyr)
# for visuals
library(GGally)
library(ggformula)

############# Review data #############
#make data frame from original data set
data(trees)
#View(trees)
names(trees)
theme_set(theme_grey(base_size=8))
ggpairs(trees,
        upper = list(continuous = wrap("cor", size = 2))
) 

origfit = lm(Volume ~ .,data=trees)
summary(origfit)

#define a new dataframe with the variables we want, response at the beginning
TreesTransformed = trees[,c(3,1,2)]  
names(TreesTransformed)

# add (using mutate) additional variables to reflect physical characteristics
TreesTransformed <- TreesTransformed %>% 
  mutate(GirthHeight=trees$Girth*trees$Height,
         Girth2=trees$Girth^2,
         Girth2Height=trees$Girth^2*trees$Height)

fullfit = lm(Volume ~ .,data=TreesTransformed)
summary(fullfit)

############# Calculating standard error of coefficients on #############
#############       for original (2-predictors) data        ############# 
#define matrices
x1.orig = model.matrix(Volume~.,data=trees)  # includes column of ones to fit intercept
x.orig = model.matrix(Volume~.,data=trees)[,-1]
y = TreesTransformed$Volume
#review correlations
cor(x.orig)
cor(x.orig,y)
#compute standard errors manually
s2 = sum(origfit$residuals^2)/(31-2-1); s2
round((solve(t(x1.orig)%*%x1.orig)),5)
sqrt(s2*diag(solve(t(x1.orig)%*%x1.orig)))
summary(lm(y~x.orig))$coef
s.e.beta.orig = summary(lm(y~x.orig))$coef[,2]; s.e.beta.orig

#############  considering 5 predictors (with transforms)  ############# 
#define matrices
x.full = model.matrix(Volume~.,data=TreesTransformed)[,-1]
x1.full = model.matrix(Volume~.,data=TreesTransformed) 
#review correlations
cor(x.full)
cor(x.full,y)
theme_set(theme_grey(base_size=8))
ggpairs(TreesTransformed[,c(2:6,1)],
        upper = list(continuous = wrap("cor", size = 2))
) 
#compute standard errors as output of lm function
s.e.beta.full = summary(lm(y~x.full))$coef[,2]
s.e.beta.orig; s.e.beta.full

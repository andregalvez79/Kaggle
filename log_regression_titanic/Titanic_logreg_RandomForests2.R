setwd("C:\\Users\\André\\Google Drive\\Kaggle\\log_regression_titanic")
options (scipen = 12)
install.packages("ggplot2")
library(ggplot2)
library(car)
library(psych)
library(glmnet)
install.packages("ltm")
library(ltm)
rcor.test(Kaggle_1)
install.packages("mice")
library(mice)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("tibble")
library(tibble)
install.packages("randomForest")
library(randomForest)

#load the data
Kaggle_1<-read.csv("C:/Users/André/Google Drive/Kaggle/log_regression_titanic/train.csv", sep = ',')
summary(Kaggle_1)
str(Kaggle_1)
#change stuff to factors"
Kaggle_1$Survivedf <- factor(Kaggle_1$Survived, levels=c('0','1'), labels=c('No','Yes'))
Kaggle_1$Pclassf <- factor(Kaggle_1$Pclass, levels=c('1','2','3'), labels=c('1st','2nd', '3rd'))

#looking at name we can maybe use the title to check if "important" people survived more 
Kaggle_1$Title <- gsub('(.*, )|(\\..*)', '', Kaggle_1$Name) #destring
Kaggle_1$Title
table(Kaggle_1$Sex, Kaggle_1$Title)#look at at the variable easily by sex
table(Kaggle_1$Age, Kaggle_1$Title)

#let's separate the titles to special ones
special <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
             'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
#now let's make sure there are only mrs. mr. and miss and master.
Kaggle_1$Title[which(Kaggle_1$Title == "Mr")] <- "Mr"
Kaggle_1$Title[which(Kaggle_1$Title == "Mrs")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title == "Ms")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title == "Miss")] <- "Miss"
Kaggle_1$Title[which(Kaggle_1$Title == "Mlle")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title == "Mme")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title %in% special)] <- "special"

table(Kaggle_1$Sex, Kaggle_1$Title)
#change it to factor
Kaggle_1$Title <- as.factor(Kaggle_1$Title)
summary(Kaggle_1)

#now lets lookat family composition
#here I add the family members plus the passanger
Kaggle_1$Family_num <- Kaggle_1$SibSp + Kaggle_1$Parch + 1
# Create a family variable unique for each surname
Kaggle_1$Name<- as.character(Kaggle_1$Name)
Kaggle_1$Surname <- sapply(Kaggle_1$Name,  
                              function(x) strsplit(x, split = '[,.]')[[1]][1])
Kaggle_1$Family <- paste(Kaggle_1$Surname, Kaggle_1$Family_num, sep='_')

# Discretize family size so make some boundaries to classify size of families
Kaggle_1$FsizeD[Kaggle_1$Family_num == 1] <- 'single'
Kaggle_1$FsizeD[Kaggle_1$Family_num >1 & Kaggle_1$Family_num <=4] <- 'small'
Kaggle_1$FsizeD[Kaggle_1$Family_num > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(Kaggle_1$FsizeD, Kaggle_1$Survived), main='Family Size by Survival', shade=TRUE)
#l0oking at the next variable
Kaggle_1$Embarked
#we see some empty variables
which(Kaggle_1$Embarked == "", arr.ind=TRUE)
#make sure it's a factor
Kaggle_1$Embarked <- factor(Kaggle_1$Embarked)
#we can infer from the other variables where this people embarked
Kaggle_1$Fare
Kaggle_1$Fare[62]
Kaggle_1$Fare[830]
describe(Kaggle_1$Fare, skew = TRUE, ranges = TRUE)
densityplot(Kaggle_1$Fare)
describeBy(Kaggle_1$Fare, group = Kaggle_1$Embarked, skew = TRUE, ranges = TRUE)
describeBy(Kaggle_1$Fare, group = Kaggle_1$Pclassf, skew = TRUE, ranges = TRUE)
describeBy(Kaggle_1$Pclass, group = Kaggle_1$Embarked, skew = TRUE, ranges = TRUE)
describeBy(Kaggle_1$Fare, list(Kaggle_1$Pclassf,Kaggle_1$Embarked))
#based by inference, on the most likely price and class they embarked on C
Kaggle_1$Embarked[62] <- "C"
Kaggle_1$Embarked[830] <- "C"

#the cabin varibale which could be useful if it didn't had so many NA'S
Kaggle_1$Cabin <- as.character(Kaggle_1$Cabin)
# The first character is the deck. For example:
strsplit(Kaggle_1$Cabin[2], NULL)[[1]]
# Create a Deck variable. Get passenger deck A - F:
Kaggle_1$Deck <- factor(sapply(Kaggle_1$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
#still to many NA's so useless
summary(Kaggle_1)


Kaggle_1$Age#has many Na's
#a way to deal with them is  using predictive imputation using mice package

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(Kaggle_1[, !names(Kaggle_1) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(Kaggle_1$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
# Replace Age variable from the mice model.
Kaggle_1$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(Kaggle_1$Age))

#create subset that omits cases with missing values so none in this case
Kaggle_1sub<-subset(Kaggle_1, !is.na(Age))
Kaggle_1sub$Embarked
head(Kaggle_1sub)
nrow(Kaggle_1sub)
summary(Kaggle_1sub)

#some plots
scatterplot(Kaggle_1sub$Pclass,Kaggle_1sub$Survived)

lattice::densityplot(~ Age | Pclass, group = Survivedf, auto.key = TRUE, data = Kaggle_1sub)
lattice::densityplot(~ Cabin | Pclass, group = Survivedf, auto.key = TRUE, data = Kaggle_1sub)


lattice::xyplot(Survivedf ~ Age | Pclass, data = Kaggle_1sub, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Survivedf ~ Age | Sex, data = Kaggle_1sub, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Survivedf ~ Sex | Pclass, data = Kaggle_1sub, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])
lattice::xyplot(Survivedf ~ Age | Title, data = Kaggle_1sub, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])


ggplot(data = Kaggle_1sub, aes(x = Sex, y = Age, color = Survivedf)) + geom_point()
ggplot(data = Kaggle_1sub, aes(x = Sex, y = Age, color = Survivedf)) + geom_point()
ggplot(Kaggle_1sub, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')
ggplot(Kaggle_1sub, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')
ggplot(Kaggle_1sub, aes(x = Title, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')
ggplot(Kaggle_1sub, aes(x = Family_num, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge')

lattice::xyplot(Survivedf ~ Family_num | Pclass, data = Kaggle_1sub, type = c('g', 'p', 'r'), index.cond = function(x,y) -coef(lm(y ~ x))[1])


#make sure no chr type so factor
Kaggle_1sub$Embarked <- factor(Kaggle_1sub$Embarked)
levels(Kaggle_1sub$Embarked)

#take out variable that are useless or redundant
drops <- c("Name","Survived", "Pclass", "PassengerId", "Ticket", "Cabin", "Family", "Family_num", "Deck", "Surname")
Kaggle_2sub <-  Kaggle_1sub[ , !(names(Kaggle_1sub) %in% drops)]
summary(Kaggle_2sub)
str(Kaggle_2sub)
#i thought maybe plynomial terms could help but they did not
#Kaggle_2sub$Age <- poly(Kaggle_2sub$Age, 2)[, 1]
#Kaggle_2sub$Age2 <- poly(Kaggle_2sub$Age, 2)[, 2]


#Kaggle_2sub$Fare <- poly(Kaggle_2sub$Fare, 2)[, 1]
#Kaggle_2sub$Fare2 <- poly(Kaggle_2sub$Fare, 2)[, 2]

#Kaggle_2sub$SibSp <- poly(Kaggle_2sub$SibSp, 2)[, 1]
#Kaggle_2sub$SibSp2 <- poly(Kaggle_2sub$SibSp, 2)[, 2]

#Kaggle_2sub$Parch <- poly(Kaggle_2sub$Parch, 2)[, 1]
#Kaggle_2sub$Parch2 <- poly(Kaggle_2sub$Parch, 2)[, 2]

summary(Kaggle_2sub)
str(Kaggle_2sub)
#forgot about this one
Kaggle_2sub$FsizeD <- as.factor(Kaggle_2sub$FsizeD)
#so we are going to use logistic regression with regularization or lasso logistic.
#I made first a whole set of variables and interaction but they don't help to predict better
#f <- as.formula(Survivedf ~ .*. + 0)
#the +0 is to delete the intercept because the glmnt function adds it automatically
f <- as.formula(Survivedf ~ .,Kaggle_2sub + 0)
y <- Kaggle_2sub$Survivedf
#model only accepts matrixes
x <- model.matrix(f, Kaggle_2sub)
#overload?
gc()
#model that tests automatically the best lambda for the test set
cv.out <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse")
# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="binomial"))
}
plot(cv.out)
#min value of lambda
lambda_min <- cv.out$lambda.min
#best value of lambda
lambda_1se <- cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=lambda_1se)

######################################################################################################################
######################################################################################################################
#test set
#now load test set and do the same as before, so teh varaibles match
Kaggle_test<-read.csv("C:/Users/André/Google Drive/Kaggle/log_regression_titanic/test.csv", sep = ',')
summary(Kaggle_test)
str(Kaggle_test)
Kaggle_test$Pclassf <- factor(Kaggle_test$Pclass, levels=c('1','2','3'), labels=c('1st','2nd', '3rd'))
Kaggle_test$Title <- gsub('(.*, )|(\\..*)', '', Kaggle_test$Name)
# Finally, grab surname from passenger name
Kaggle_test$Name<- as.character(Kaggle_test$Name)
Kaggle_test$Surname <- sapply(Kaggle_test$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
Kaggle_test$Title
special <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
             'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
Kaggle_test$Title[which(Kaggle_test$Title == "Mr")] <- "Mr"
Kaggle_test$Title[which(Kaggle_test$Title == "Mrs")] <- "Mrs"
Kaggle_test$Title[which(Kaggle_test$Title == "Ms")] <- "Mrs"
Kaggle_test$Title[which(Kaggle_test$Title == "Miss")] <- "Miss"
Kaggle_test$Title[which(Kaggle_test$Title == "Mlle")] <- "Mrs"
Kaggle_test$Title[which(Kaggle_test$Title == "Mme")] <- "Mrs"
Kaggle_test$Title[which(Kaggle_test$Title %in% special)] <- "special"

table(Kaggle_test$Sex, Kaggle_test$Title)
Kaggle_test$Title <- as.factor(Kaggle_test$Title)

Kaggle_test$Family_num <- Kaggle_test$SibSp + Kaggle_test$Parch + 1
# Create a family variable 
Kaggle_test$Family <- paste(Kaggle_test$Surname, Kaggle_test$Family_num, sep='_')

# Discretize family size
Kaggle_test$FsizeD[Kaggle_test$Family_num == 1] <- 'single'
Kaggle_test$FsizeD[Kaggle_test$Family_num > 1 & Kaggle_test$Family_num <=4] <- 'small'
Kaggle_test$FsizeD[Kaggle_test$Family_num > 4] <- 'large'

summary(Kaggle_test)
Kaggle_test$FsizeD<- as.factor(Kaggle_test$FsizeD)

Kaggle_test$Embarked


# Replace missing fare value with median fare for class/embarkment
which(is.na(Kaggle_test$Fare))
describeBy(Kaggle_test$Fare[153], list(Kaggle_test$Pclass[153], Kaggle_test$Embarked[153]))
describeBy(Kaggle_test$Fare, list(Kaggle_test$Pclass, Kaggle_test$Embarked))

Kaggle_test$Fare[153] <- median(Kaggle_test[Kaggle_test$Pclass == '3' & Kaggle_test$Embarked == 'S', ]$Fare, na.rm = TRUE)

Kaggle_test$Cabin <- as.character(Kaggle_test$Cabin)
# The first character is the deck. For example:
strsplit(Kaggle_test$Cabin[2], NULL)[[1]]
# Create a Deck variable. Get passenger deck A - F:
Kaggle_test$Deck <- factor(sapply(Kaggle_test$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(Kaggle_test[, !names(Kaggle_test) %in% c('PassengerId','Name','Ticket',
                                                          'Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(Kaggle_1$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
# Replace Age variable from the mice model.
Kaggle_test$Age <- mice_output$Age


#create subset that omits cases with missing values
Kaggle_testsub<-subset(Kaggle_test, !is.na(Age) & !is.na(Fare))
summary(Kaggle_testsub)
drops <- c("Name","Survived", "Pclass", "PassengerId", "Ticket", "Cabin", "Family", "Family_num", "Deck", "Surname")
Kaggle_test2sub <-  Kaggle_testsub[ , !(names(Kaggle_testsub) %in% drops)]
summary(Kaggle_test2sub)
#Kaggle_test2sub$Age <- poly(Kaggle_test2sub$Age, 2)[, 1]
#Kaggle_test2sub$Age2 <- poly(Kaggle_test2sub$Age, 2)[, 2]

#Kaggle_test2sub$Fare <- poly(Kaggle_test2sub$Fare, 2)[, 1]
#Kaggle_test2sub$Fare2 <- poly(Kaggle_test2sub$Fare, 2)[, 2]

#Kaggle_test2sub$Parch <- poly(Kaggle_test2sub$Parch, 2)[, 1]
#Kaggle_test2sub$Parch2 <- poly(Kaggle_test2sub$Parch, 2)[, 2]

#Kaggle_test2sub$SibSp <- poly(Kaggle_test2sub$SibSp, 2)[, 1]
#Kaggle_test2sub$SibSp2 <- poly(Kaggle_test2sub$SibSp, 2)[, 2]

Kaggle_test2sub$FsizeD <- as.factor(Kaggle_test2sub$FsizeD)

str(Kaggle_test2sub)
# no y becasuse we don't have one
g <- as.formula( ~ ., Kaggle_test2sub)
x_test <- model.matrix(g, Kaggle_test2sub)
#get test data
#predict class, type="class"
lasso_prob <- predict(cv.out,newx = x_test,s=lambda_1se,type="response")
#translate probabilities to predictions
lasso_predict <- rep("No",nrow(Kaggle_2sub))
lasso_predict[lasso_prob>.5] <- "Yes"
#confusion matrix
conflog <- table(pred=lasso_predict,true=Kaggle_2sub$Survivedf)
conflog
mean(lasso_predict==Kaggle_2sub$Survivedf)
#we can see we only predict 50% correctly

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=x_test, type= "response")
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=x_test)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=x_test)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=x_test)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=x_test)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=x_test)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=x_test)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=x_test)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=x_test)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=x_test)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=x_test)
y.test = mean(Kaggle_3sub$Survived)
#mse0 is the lowest mse
mse0 <- mean((y.test - yhat0)^2)
mse1 <- mean((y.test - yhat1)^2)
mse2 <- mean((y.test - yhat2)^2)
mse3 <- mean((y.test - yhat3)^2)
mse4 <- mean((y.test - yhat4)^2)
mse5 <- mean((y.test - yhat5)^2)
mse6 <- mean((y.test - yhat6)^2)
mse7 <- mean((y.test - yhat7)^2)
mse8 <- mean((y.test - yhat8)^2)
mse9 <- mean((y.test - yhat9)^2)
mse10 <- mean((y.test - yhat10)^2)

#we test predictions with alpha = 0
lasso_predict <- rep("No",nrow(Kaggle_2sub))
lasso_predict[yhat0>.5] <- "Yes"
#confusion matrix
conflog <- table(pred=lasso_predict,true=Kaggle_2sub$Survivedf)
conflog
mean(lasso_predict==Kaggle_2sub$Survivedf)
#we can see we only predict 50% correctly
#improived to 53% still shit

###########################################################################################################
##########################################################################################################
#no we will use random forests
"get the variables... I did this because for the submission I can use No and yes for the factor"
#take out variable that are useless or redundant
drops2 <- c("Name", "Pclass", "PassengerId", "Ticket", "Cabin", "Family", "Family_num", "Deck")
Kaggle_3sub <-  Kaggle_1sub[ , !(names(Kaggle_1sub) %in% drops2)]
summary(Kaggle_3sub)
str(Kaggle_3sub)
#forgot about this one
Kaggle_3sub$FsizeD <- as.factor(Kaggle_3sub$FsizeD)
# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclassf + Sex + Age + SibSp +Parch + 
                           Fare + Embarked + Title + 
                           FsizeD,
                         data = Kaggle_3sub)

# Show model error
plot(rf_model, ylim=c(0,0.36))


# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()  
  
# Predict using the test set
prediction <- predict(rf_model, Kaggle_test2sub)
#confusion matrix
conf <- rf_model$confusion
conf
conflog
#we can see that the porediciton rate is way higher than with logistic + reg 

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = Kaggle_testsub$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'Solution_Kaggle1.csv', row.names = F)

save.image(file = "X_all.RData")

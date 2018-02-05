setwd("C:\\Users\\André\\Google Drive\\Kaggle\\log_regression_titanic")
options (scipen = 12)
install.packages("ggplot2")
library(ggplot2)
library(car)
library(psych)
library(glmnet)

Kaggle_1<-read.csv("C:/Users/André/Google Drive/Kaggle/log_regression_titanic/train.csv", sep = ',')
summary(Kaggle_1)
str(Kaggle_1)
Kaggle_1$Survivedf <- factor(Kaggle_1$Survived, levels=c('0','1'), labels=c('No','Yes'))
Kaggle_1$Pclassf <- factor(Kaggle_1$Pclass, levels=c('1','2','3'), labels=c('1st','2nd', '3rd'))

Kaggle_1$Title <- gsub('(.*, )|(\\..*)', '', Kaggle_1$Name)
Kaggle_1$Title
table(Kaggle_1$Sex, Kaggle_1$Title)
table(Kaggle_1$Age, Kaggle_1$Title)

special <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
             'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

Kaggle_1$Title[which(Kaggle_1$Title == "Mr")] <- "Mr"
Kaggle_1$Title[which(Kaggle_1$Title == "Mrs")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title == "Ms")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title == "Miss")] <- "Miss"
Kaggle_1$Title[which(Kaggle_1$Title == "Mlle")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title == "Mme")] <- "Mrs"
Kaggle_1$Title[which(Kaggle_1$Title %in% special)] <- "special"

table(Kaggle_1$Sex, Kaggle_1$Title)
Kaggle_1$Title <- as.factor(Kaggle_1$Title)

summary(Kaggle_1)

Kaggle_1$Family_num <- Kaggle_1$SibSp + Kaggle_1$Parch + 1

Kaggle_1$Embarked
which(Kaggle_1$Embarked == "", arr.ind=TRUE)

Kaggle_1$Fare
Kaggle_1$Fare[62]
Kaggle_1$Fare[830]
describe(Kaggle_1$Fare, skew = TRUE, ranges = TRUE)
densityplot(Kaggle_1$Fare)
describeBy(Kaggle_1$Fare, group = Kaggle_1$Embarked, skew = TRUE, ranges = TRUE)
describeBy(Kaggle_1$Fare, group = Kaggle_1$Pclassf, skew = TRUE, ranges = TRUE)
describeBy(Kaggle_1$Pclass, group = Kaggle_1$Embarked, skew = TRUE, ranges = TRUE)
describeBy(Kaggle_1$Fare, list(Kaggle_1$Pclassf,Kaggle_1$Embarked))
Kaggle_1$Embarked[62] <- "C"
Kaggle_1$Embarked[830] <- "C"


Kaggle_1$Cabin

summary(Kaggle_1)

install.packages("ltm")
library(ltm)
rcor.test(Kaggle_1)


#create subset that omits cases with missing values
Kaggle_1sub<-subset(Kaggle_1, !is.na(Age))
Kaggle_1sub$Embarked
head(Kaggle_1sub)
nrow(Kaggle_1sub)
summary(Kaggle_1sub)

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

drops <- c("Name","Survived", "Pclass", "PassengerId")
Kaggle_2sub <-  Kaggle_1sub[ , !(names(Kaggle_1sub) %in% drops)]

f <- as.formula(Survivedf ~ .^2 + 0)
y <- Kaggle_2sub$Survivedf
x <- model.matrix(f, Kaggle_2sub)

gc()
cv.out <- cv.glmnet(x,y,alpha=1,family="binomial",type.measure = "mse" )
glmmod <- glmnet(x, y, alpha=1, family="binomial")


save.image(file = "X_all.RData")

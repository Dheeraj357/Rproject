#First read file and assign. it to wine variable 
Wine <- read.csv("Winequality-red.csv",sep = ';')
View(Wine)
head(Wine)
#getwd()
#draw barplot of  winequality
barplot(table(Wine$quality),xlab="ph",ylab="Frequency",col=1:6)
boxplot(table(Wine$quality))
#X <- Wine$quality
#Y <- LETTERS[1:6]
#pie(X,labels = Y,main = 'My chart',col = rainbow(6))
#scatter(table(Wine$quality))
View(Wine$quality)

#Divide our winequality col into bad,good,normal form
Wine$taste <- ifelse(Wine$quality < 5,'Bad','Good')
Wine$taste[Wine$quality == 5] <- 'Normal'
Wine$taste <- as.factor(Wine$taste)
table(Wine$taste)

#Let change our set into train and test
set.seed(123)
Samp <- sample(nrow(Wine), 0.5* nrow(Wine))
train <- Wine[Samp, ]
test <- Wine[Samp, ]

#use Random Forest[use for build model]
#install.packages("randomForest")
library(randomForest)
model <- randomForest(taste ~ . - quality,data = train)
model


#Now predict set quality
Pred <- predict(model ,newdata = test)
table(Pred ,test$taste)

#check accuray of data set 
(1+417+322)/nrow(test)
(0+354+269)/nrow(test)

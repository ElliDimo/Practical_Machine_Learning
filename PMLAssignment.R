
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv','data.csv')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv','test.csv')

training <- read.csv('data.csv')
testing <- read.csv('test.csv')

library(caret)
# Dividing training to sub-train and sub-test sets to do cross-validation
inTrain = createDataPartition(training$classe, p = 3/4)[[1]]
subtrain = training[ inTrain,]
subtest = training[-inTrain,]


# Removing columns, where all rows have NAs in
subtrain1 <- subtrain[ ,colSums(is.na(subtrain)) == 0]
sum(is.na(subtrain1))
# [1] 0 check

# For the below columns, 14421 out of 14718 rows had value ''
lista <- names(subtrain1[which(colSums(subtrain1=='')!=0)])
# Omitting these 33 columns from subtrain set
subtrain2<-subtrain1[which(colSums(subtrain1=='')==0)]
# check
subtrain2[which(colSums(subtrain1=='')!=0)]
subtrain2[which(colSums(subtrain1=='#DIV/0!')!=0)]

# Unsupervised Prediction with clusters
# Converting to numeric to avoid NAs introduced by coercion in kmeans call
subtrain2 <- as.data.frame(sapply(subtrain2,as.character))
subtrain2 <- as.data.frame(sapply(subtrain2,as.numeric))
kMeans1 <- kmeans(subset(subtrain2,select=-c(classe)),centers=6)
subtrain2$clusters <- as.factor(kMeans1$cluster)
# plot clusters vs classe
qplot(classe,clusters,data=subtrain2)
# Complete failure!

# Unsupervised Prediction with Support Vector Machine
mod <- svm(classe ~ ., data = subtrain2)
pred <- predict(mod,subtest)
# Error in eval(expr, envir, enclos) : object 'clusters' not found
# Screw it...

# The below commands were never ending 
# The problem seemes to be in using classe ~. instead of subtrain2[,-60],subtrain2$classe
mod1 <- train(classe ~.,method="rf",data=subtrain2,verbose=F)
mod2 <- train(classe ~.,method="gbm",data=subtrain2,verbose=F)
mod3 <- train(classe ~.,method="lda",data=subtrain2,verbose=F)

set.seed(3523)
rf.mod <- train(subtrain2[,-60],subtrain2$classe,tuneGrid=data.frame(mtry=3),trControl=trainControl(method="none"))
# rf.mod$finalModel gives :
# OOB estimate of  error rate: 0.02%
pred.rf <- predict(rf.mod,subtest)
table(subtest$classe,pred.rf)
pred.rf
     A    B    C    D    E
A 1394    1    0    0    0
B    1  948    0    0    0
C    0    0  855    0    0
D    0    0    0  804    0
E    0    0    0    2  899
# oh fucking yes!

set.seed(3523)
gbm.mod <- train(subtrain2[,-60],subtrain2$classe,method='gbm',trControl=trainControl(method="none"),verbose=F)
pred.gbm <- predict(gbm.mod,subtest)
table(subtest$classe,pred.gbm)
pred.gbm
     A    B    C    D    E
A 1395    0    0    0    0
B    0  948    1    0    0
C    0    0  855    0    0
D    0    0    0  804    0
E    0    0    0    0  901
# oh fucking yes again!

lda.mod <- train(subtrain2[,-60],subtrain2$classe,method='lda',trControl=trainControl(method="none"))
pred <- predict(lda.mod,subtest)
# Error in FUN(x, aperm(array(STATS, dims[perm]), order(perm)), ...) : 
#    non-numeric argument to binary operator

mod1 <- train(subtrain2[,-60],subtrain2$classe,method='rpart',trControl=trainControl(method="none"))
pred <- predict(mod1,subtest)
table(subtest$classe,pred)
pred
A    B    C    D    E
A 1395    0    0    0    0
B  949    0    0    0    0
C  855    0    0    0    0
D  804    0    0    0    0
E  901    0    0    0    0
# sucks...

# Trying combining predictors with rf and gbm
combinedTestData <- data.frame(pred1=pred.rf,pred2=pred.gbm,classe=subtest$classe)
comb.fit <- train(combinedTestData[,-3],combinedTestData$classe,trControl=trainControl(method="none"))
# comb.fit$finalModel returns a confusion matrix amongst others...
pred.comb <- predict(comb.fit,combinedTestData)
confusionMatrix(testing$diagnosis,pred.comb)
# Error in confusionMatrix.default(testing$diagnosis, pred.comb) : 



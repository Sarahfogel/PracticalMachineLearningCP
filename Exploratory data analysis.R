#==============================================================================
#
# Exploratory Analysis for Practical Machine Learning Course Project
#
#==============================================================================


#=======================Download and Read in Files=============================

# First Download the files, if they haven't already been downloaded

    if ("pml-training.csv" %in% list.files(".")) {
        
    } else { print("downloading")
        download.file(
            "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
            destfile="./pml-training.csv")
    }

    if ("pml-testing.csv" %in% list.files(".")) {
        
    } else { print("downloading")
             download.file(
                 "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                 destfile="./pml-testing.csv")
    }

# Now read the files in

    training<-read.csv("./pml-training.csv", header=T)
    testing<-read.csv("./pml-testing.csv", header=T)


#==========================Explore Data=========================================

# There are 160 variables and nearly 20000 observations in this dataset

    summary(training[,1:20])
    summary(training[,21:40])
    summary(training[,41:60])
    summary(training[,61:80])
    summary(training[,81:100])
    summary(training[,101:120])
    summary(training[,121:140])
    summary(training[,141:160])

# List of variables that should be removed: X - just an index, time stamps (x3) - time
#    shouldn't matter, new_window and num-window - what is that?,
# Anything that has 19216 NAs or more - these only have a 2% chance of having data:
#   columns 12:36, 50:59, 69:83, 87:101, 103:112, 125:139, 141:150


    dim(training[-c(1,3:7, 12:36, 50:59, 69:83, 87:101, 103:112,125:139, 141:150  )])

# Create new training and testing sets with the irrelevant variables removed and partition off a
#   validation set
    library(caret)
    set.seed(3450)

    trainIndex<-createDataPartition(training[,160], p=.8, list=FALSE)

    validationclean<-training[-trainIndex,-c(1,2,3:7, 12:36, 50:59, 69:83, 87:101, 103:112,125:139, 141:150  )]
    trainingclean<-training[trainIndex, -c(1,2,3:7, 12:36, 50:59, 69:83, 87:101, 103:112,125:139, 141:150  )]
    testingclean<-testing[-c(1,2,3:7, 12:36, 50:59, 69:83, 87:101, 103:112,125:139, 141:150  )]

# Also, create a vector containig the actual outcome we're interested in for this analysis
#   correct or not, not which class of incorrect

    outcomes<-factor(levels=c("Right", "Wrong"))
    for (i in 1:dim(trainingclean)[1]) {
        if (trainingclean[i,53]=="A"){
            outcomes[i]<-"Right"
        } else {
            outcomes[i]<-"Wrong"
        }
    }

    validationoutcomes<-factor(levels=c("Right", "Wrong"))
    for (i in 1:dim(validationclean)[1]) {
        if (validationclean[i,53]=="A"){
            validationoutcomes[i]<-"Right"
        } else {
            validationoutcomes[i]<-"Wrong"
        }
    }
# Continue exploring
    summary(trainingclean)

# The 53 variables we have left are; roll, pitch, and yaw of each of 4 sensors(12); 
#   total acceleration of 4 sensors(4); acceleration in 3 directions of 4 sensors (12);
#   gyroscopic forces in 3 directions on 4 sensors (12); 
#   magnetic force in 3 directions on 4 sensors (12); and class of exercise (1)

# Check if total acceleration adds anything over the 3 directions
    d<-(sqrt(accel_belt_x^2+accel_belt_y^2+accel_belt_x^2)-total_accel_belt)
    head(d)

    d2<-(sqrt(roll_belt^2+pitch_belt^2+yaw_belt^2)-total_accel_belt)
    head(d2)

    d3<-(sqrt(gyros_belt_x^2+gyros_belt_y^2+gyros_belt_x^2)-total_accel_belt)
    head(d3)

    head(total_accel_belt)

# Look for variables with near 0 variability

    nsv<- nearZeroVar(trainingclean, saveMetrics=TRUE)
    nsv

    #No variables come out small variation

# Look for highly correlated variables
    M<-abs(cor(trainingclean[,1:52]))
    diag(M)<-0
    which(M>.8, arr.ind=T)
    # Lots of high correlations

# Quick graphs of all remaining predictors to look for skewness etc.
    par(mfrow=c(2,2))
    for (i in 1:4) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 5:8) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 9:12) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 13:16) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 17:20) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 21:24) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 25:28) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 29:32) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 33:36) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 37:40) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 41:44) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 45:48) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
    for (i in 49:52) {
        hist(trainingclean[,i], main=paste(names(trainingclean)[i]))
    }
# Boxplots
    for (i in 1:4) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 5:8) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 9:12) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 13:16) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 17:20) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 21:24) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 25:28) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 29:32) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 33:36) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 37:40) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 41:44) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 45:48) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    for (i in 49:52) {
        boxplot(trainingclean[,i]~trainingclean[,53], main=paste(names(trainingclean)[i]))
    }
    par(mfrow=c(1,1))
   

# Consider pca - must apply same pcas to test set as used for training set!
# Sample code:

    preProc<-preProcess(log10(trainingclean[,1:52]+.0001), method="pca", pcaComp=2)
    pcatrain<- predict(preProc, log10(trainingclean[,1:52]+.0001))
    modelFit<- train(outcomes~., data=pcatrain, method="glm")

    pcatest<- predict(preProc, log10(testingclean[,1:52] +.0001))
    confusionMatrix(outcomes, predict(modelFit, testPC))

preProc<-preProcess(trainingclean[,1:52], method="pca", thresh=.9)
pcatrain<- predict(preProc, trainingclean[,1:52])
modelFit<- train(outcomes~., data=pcatrain, method="glm")

modelFit$result

pcavalidation<- predict(preProc,validationclean[,1:52])
confusionMatrix(validationoutcomes, predict(modelFit, pcavalidation))


# Try out a few models at this point
    library(caret)
    glm.model<-train(trainingclean[,1:52], outcomes, method="glm", na.action=na.omit)

    glm.model$results     #Accuracy of .903, not bad, but measuring the wrong thing

    confusionMatrix(outcomes, predict(glm.model, trainingclean[,1:52]))
    confusionMatrix(validationoutcomes, predict(glm.model, validationclean[,1:52]))

    rpart.model<-train(trainingclean[,1:52], trainingclean[,53], method="rpart") #began at 10:28, done by 10:38
    rpart.model$result
    confusionMatrix(validationclean[,53], predict(rpart.model, validationclean[,1:52]))
        #Accuracy of .4876, not very good

    rpart.model
    rpart.model$finalModel
    plot(rpart.model$finalModel, uniform=T, main="Classification Tree")
    text(rpart.model$finalModel, use.n=T, all=T, cex=.5)

    rf.model<-train(trainingclean[,1:52], trainingclean[,53], method="rf") #Started this at 8:45 today, done by 9:49, possibly earlier
    rf.model$result
    confusionMatrix(validationclean[,53], predict(rf.model, validationclean[,1:52]))
        #Accuracy of .9939 on validation set, excellent

    rf.model
    rf.model$finalModel
    plot(rf.model$finalModel)

#Try out some 10 fold cross validation on this model
    fitControl<-trainControl(method="repeatedcv", number=10, repeats=10)
    rf.model.cv<-train(trainingclean[,1:52], trainingclean[,53], method="rf", trControl=fitControl) #began 10:53, not done by 12:02
    system.time(rf.model.cv<-train(trainingclean[,1:52], trainingclean[,53], method="rf", trControl=fitControl)) 
    rf.model.cv
    trellis.par.set(caretTheme())
    plot(rf.model.cv)
    trellis.par.set()
    confusionMatrix(validationclean[,53], predict(rf.model.cv, validationclean[,1:52]))



    rf.pca.model<-train(pcatrain, trainingclean[,53], method="rf") #began at 9:52, done by 10:27
    rf.pca.model$result
    confusionMatrix(validationclean[,53], predict(rf.pca.model, pcavalidation))
        #Accuracy of .9748 on validation set, great but less than rf.model

# What other models should I run?

# Naive Bayes
    nb.model<-train(trainingclean[,1:52], trainingclean[,53], method="nb") 
    nb.model$result
    confusionMatrix(validationclean[,53], predict(nb.model, validationclean[,1:52]))
        #Accuracy .7487, not as good as rf.model
# Neural net deep learning?

    set.seed(4056)
    smalltraining<-trainingclean[sample(1:dim(trainingclean)[1], size=5000), ]
    nnet.model.small<-train(smalltraining[,1:52], smalltraining[,53], method="nnet") 
    nnet.model.small
    confusionMatrix(validationclean[,53], predict(nnet.model.small, validationclean[,1:52]))
        #Accuracy .3849 with 5000 observations

    gbm.model<-train(smalltraining[,1:52], smalltraining[,53], method="gbm") 
    gbm.model
    confusionMatrix(validationclean[,53], predict(gbm.model, validationclean[,1:52]))
        #Accuracy of .9467 with only 5000 variables - try this one full size


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

# Create new training and testing sets with the irrelevant variables removed

    trainingclean<-training[-c(1,3:7, 12:36, 50:59, 69:83, 87:101, 103:112,125:139, 141:150  )]
    testingclean<-testing[-c(1,3:7, 12:36, 50:59, 69:83, 87:101, 103:112,125:139, 141:150  )]

# Also, create a vector containig the actual outcome we're interested in for this analysis
#   correct or not, not which class of incorrect

    outcomes<-c()
    for (i in 1:dim(trainingclean)[1]) {
        if (trainingclean[i,54]=="A"){
            outcomes[i]<-"Right"
        } else {
            outcomes[i]<-"Wrong"
        }
    }

# Continue exploring
    summary(trainingclean)

# The 54 variables we have left are: person(1); roll, pitch, and yaw of each of 4 sensors(12); 
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

# Try out a few models at this point
    library(caret)
    t<-train(trainingclean[,2:5], outcomes, method="glm", na.action=na.omit)

summary(trainingclean[,2:53])
str(trainingclean[,2:53])

trainingclean[,5]<-as.numeric(trainingclean[,5])

    t<-train(outcomes~., data=trainingclean[,2:53], method="lm")


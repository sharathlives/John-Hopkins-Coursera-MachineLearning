#Load the data
train <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
test <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
dim(train)
dim(test)


#Cleaning the data

#Remove unwanted columns. These are not data columns.
train <- train[, -c(1:7)]
test <- test[, -c(1:7)]
dim(train)
dim(test)

#Remove columns that have more than 50% NA's. First duplicate the dataset. Then, for every column in train that hasmore than 50% NA's
#compare that column name with train2 and remove that column from train2.
train2 <- train 
for(i in 1:length(train)) { 
  if( sum( is.na( train[, i] ) ) /nrow(train) >= .5 ) { 
    for(j in 1:length(train2)) {
      if( length( grep(names(train[i]), names(train2)[j]) ) ==1)  { 
        train2 <- train2[ , -j] 
      }   
    } 
  }
}
dim(train2)
dim(train)



#Look for near zero variance variables. Tried removing some variables with low variance but does not affect the model.
#myDataNZV <- nearZeroVar(train2, saveMetrics=TRUE)
#NZVvars <- c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
#             "kurtosis_yaw_belt", "skewness_roll_belt", 
#             "skewness_roll_belt.1", "skewness_yaw_belt")
                                     
#NZVvarsLogical <- names(train) %in% NZVvars
#train <- train[!NZVvarsLogical]

#Partition the dataset to 80:20 to create validation set. I don't perform cross-validation because I get good results by keepong it simple. 
partition <- createDataPartition(y = train2$classe, p = 0.8, list = FALSE)
trainMain <- train2[partition, ]
trainTest <-  train2[-partition, ]
dim(trainMain)
dim(trainTest)

#Model building. Test for insample error on validation set. 
model1 <- randomForest(classe ~., data = trainMain)
predictMain <- predict(model1, trainTest, type = "class")
confusionMatrix(predictMain, trainTest$classe)

#Test the accuracy of the model for out of sample error
predictTest = predict(model1, test, type = "class")
#confusionMatrix(predictTest, test$classe)

#Generating the files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictTest)





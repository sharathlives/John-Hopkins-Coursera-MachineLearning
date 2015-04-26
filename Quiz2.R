training_short <- training[,c(1,58:69)]
preProc <- preProcess(training_short[,-1], method = "pca", pcaComp = 9)
trainPC <- predict(preProc, training_short[,-1])
modelFit <- train(training_short$diagnosis ~., method = "glm", data = trainPC)
confusionMatrix (training_short$diagnosis, predict(modelFit, trainPC))
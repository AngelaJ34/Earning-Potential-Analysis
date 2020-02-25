Earn_Train <- read.csv("Earnings_Train2019.csv")

Earn_Train <- Earnings_Train2019
Earn_Test <- Earnings_Test_Students
  
View(Earn_Train)
library(rpart)
library(rpart.plot)
colnames(Earn_Train)
summary(Earn_Train)

plot(Earn_Train$Earnings~Earn_Train$Number_Of_Professional_Connections)
plot(Earn_Train$Earnings~Earn_Train$GPA)# GPA is not significant
plot(Earn_Train$Earnings~Earn_Train$Height)# Height is irrelavent
plot(Earn_Train$Earnings~Earn_Train$Number_Of_Credits)#Credits do no matter
plot(Earn_Train$Earnings~Earn_Train$Number_Of_Parking_Tickets)#Parking tickets are not important
Majormean<-tapply(Earn_Train$Earnings,Earn_Train$Major, mean)
barplot(Majormean, main = "Earning Potiential Based On Major", xlab="Major", ylab = "Earnings", col=c("blue","green","orange","yellow","light green","red"))
Professmean<-tapply(Earn_Train$Earnings,Earn_Train$Number_Of_Professional_Connections, mean)
plot(Professmean, main="Earning Potiential Based On Professional Network", col=c("blue","green","orange","yellow","light green","red"))

install.packages("e1071")
library(e1071)
#Earnings & GPA
perf.svm1 = svm(Earnings~GPA, data = Earn_Train)
perf.svm1

#Earnings $ Number Of Professional Connections
perf.svm2 = svm(Earnings~Number_Of_Professional_Connections, data = Earn_Train)
perf.svm2

#Earnings & Height
perf.svm3 = svm(Earnings~Height, data = Earn_Train)
perf.svm3

#Earnings & Number of credits
perf.svm4 = svm(Earnings~Number_Of_Credits, data = Earn_Train)
perf.svm4

#Earnings & Number of parking tickets
perf.svm5 = svm(Earnings~GPA+Number_Of_Parking_Tickets, data = Earn_Train)
perf.svm5

#Earnings & GPA, Number of professional connections,
perf.svm6 = svm(Earnings~GPA+Number_Of_Professional_Connections, data = Earn_Train)
perf.svm6

#Earnings & GPA, Number of professional connections, Graduation_Year
perf.svm7 = svm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year, data = Earn_Train)
perf.svm7

#Earnings & GPA, Number of professional connections, Height, Number of Credits, Number of parking tickets
perf.svm8 = svm(Earnings~GPA+Number_Of_Professional_Connections+Height+Number_Of_Credits+Number_Of_Parking_Tickets, data = Earn_Train)
perf.svm8

#Earnings & All Variables
perf.svm9 = svm(Earnings~GPA+Number_Of_Professional_Connections+Major+Graduation_Year+Height+Number_Of_Credits+Number_Of_Parking_Tickets, data = Earn_Train)
perf.svm9

#Earnings & Major, Number of Professional Connections
perf.svm10 = svm(Earnings~Number_Of_Professional_Connections+Major, data = Earn_Train)
perf.svm10

pred1 = predict(perf.svm1, newdata = Earn_Train)
pred2 = predict(perf.svm2, newdata = Earn_Train)
pred3 = predict(perf.svm3, newdata = Earn_Train)
pred4 = predict(perf.svm4, newdata = Earn_Train)
pred5 = predict(perf.svm5, newdata = Earn_Train)
pred6 = predict(perf.svm6, newdata = Earn_Train)
pred7 = predict(perf.svm7, newdata = Earn_Train)
pred8 = predict(perf.svm8, newdata = Earn_Train)
pred9 = predict(perf.svm9, newdata = Earn_Train)
pred10 = predict(perf.svm10, newdata = Earn_Train)

mean((pred1 - Earn_Train$Earnings)^2)
mean((pred2 - Earn_Train$Earnings)^2)
mean((pred3 - Earn_Train$Earnings)^2)
mean((pred4 - Earn_Train$Earnings)^2)
mean((pred5 - Earn_Train$Earnings)^2)
mean((pred6 - Earn_Train$Earnings)^2)
mean((pred7 - Earn_Train$Earnings)^2)
mean((pred8 - Earn_Train$Earnings)^2)
mean((pred9 - Earn_Train$Earnings)^2)
mean((pred10 - Earn_Train$Earnings)^2)

pred = predict(perf.svm10, newdata=Earnings_Test_Students)
mytestprediction<-Earnings_Test_Students
mytestprediction$Earnings <- pred

submission <- mytestprediction[,c('ID','Earnings')]
write.csv(submission,file = "mytestsubmission.csv", row.names = FALSE)

library(rpart)
library(rpart.plot)
colnames(Earn_Train)
summary(Earn_Train)
data1 <- Earnings_Train2019

#All Variables
tree1 <-rpart(Earnings ~ GPA+Number_Of_Professional_Connections+Major+Graduation_Year, control = rpart.control(minsplit = 10, minbucket = 150), data = Earn_Train)
rpart.plot(tree1)
prp(tree1, type = 1, box.col = "yellow")

pred1<-predict(tree1, newdata=Earn_Train)
predictedEarning<-predict(tree1, newdata=Earn_Train)

tree2 <-rpart(Earnings ~ GPA+Number_Of_Professional_Connections+Major+Graduation_Year+Height+Number_Of_CreditsNumber_Of_Parking_Tickets, control = rpart.control(minsplit = 10, minbucket = 150), data = Earn_Train)
rpart.plot(tree2)
prp(tree2, type = 1, box.col = "yellow")

pred2<-predict(tree2, newdata=Earn_Train)
predictedEarning<-predict(tree2, newdata=Earn_Train)

tree3 <-rpart(Earnings ~ GPA+Number_Of_Professional_Connections+Major+Graduation_Year+Number_Of_Credits, control = rpart.control(minsplit = 10, minbucket = 150), data = Earn_Train)
rpart.plot(tree3)
prp(tree3, type = 1, box.col = "yellow")

pred3<-predict(tree3, newdata=Earn_Train)
predictedEarning<-predict(tree3, newdata=Earn_Train)

View(data1)
install.packages("devtools")
devtools::install_github("devanshagr/crossValidation")
library

data<-Earnings_Train2019
# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)
  
perf.model = svm(Earnings~GPA, data = train)
pred <- predict(perf.model,test)

# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))



# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)

perf.model = svm(Earnings~Number_Of_Professional_Connections, data = train)
pred <- predict(perf.model,test)

# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))


# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)
  

perf.model = svm(Earnings~Height, data = train)
pred <- predict(perf.model,test)

# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))



  # Split ratio of train-test data : 0.75(Train) and 0.25(Test)
  split_ratio <- 0.75
  # Calculate the size of train data
  sample_size <- floor(split_ratio * nrow(data))
  #Number of times you want to perform cross_validation
  iteration <- 7
  # Storing the error at each in a vector
  all_errors <- c()
  # For loop to compute the cross validation after building a model each time
  for (val in c(1:iteration)){
    #Splitting the data into training and testing
    train_ind <- sample(seq_len(nrow(data)), size = sample_size)
    # Train data
    train <- data[train_ind, ]
    # Test data
    test <- data[-train_ind, ]
    
    # NOTE : Create YOUR model on the training data
    #        Predict using the predict function
    ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
    ####### Format of the model and predict function
    # perf.model <- model_function(PERFORMANCE ~ ., data = train)
    # pred.model <- predict(perf.model, test)
  

perf.model = svm(Earnings~Number_Of_Credits, data = train)
pred <- predict(perf.model,test)

# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
  }
  # Print all the MSE's calculate
  print(all_errors)
  # Print the mean of the errors calculated
  print(mean(all_errors))


# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)

perf.model = svm(Earnings~GPA+Number_Of_Parking_Tickets, data = train)
pred <- predict(perf.model,test)


# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))



# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)


perf.model = svm(Earnings~GPA+Number_Of_Professional_Connections, data = train)
pred <- predict(perf.model,test)


# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))


# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)


perf.model = svm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year, data = train)
pred <- predict(perf.model,test)


# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))


# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)


perf.model = svm(Earnings~GPA+Number_Of_Professional_Connections+Height+Number_Of_Credits+Number_Of_Parking_Tickets, data = train)
pred <- predict(perf.model,test)

# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))



# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)

perf.model = svm(Earnings~GPA+Number_Of_Professional_Connections+Major+Graduation_Year+Height+Number_Of_Credits+Number_Of_Parking_Tickets, data = train)
pred <- predict(perf.model,test)

# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))



# Split ratio of train-test data : 0.75(Train) and 0.25(Test)
split_ratio <- 0.75
# Calculate the size of train data
sample_size <- floor(split_ratio * nrow(data))
#Number of times you want to perform cross_validation
iteration <- 7
# Storing the error at each in a vector
all_errors <- c()
# For loop to compute the cross validation after building a model each time
for (val in c(1:iteration)){
  #Splitting the data into training and testing
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  # Train data
  train <- data[train_ind, ]
  # Test data
  test <- data[-train_ind, ]
  
  # NOTE : Create YOUR model on the training data
  #        Predict using the predict function
  ####### IMPORTANT : You need to put YOUR model and the corresponding predict function here #######
  ####### Format of the model and predict function
  # perf.model <- model_function(PERFORMANCE ~ ., data = train)
  # pred.model <- predict(perf.model, test)

perf.model = svm(Earnings~Number_Of_Professional_Connections+Major, data = train)
pred <- predict(perf.model,test)

# Find the MSE on the predictions done on the test data with the same model
MSE <- mean((pred-test$Earnings)^2)
# Store all the MSE for each iteration
all_errors <- c(all_errors, MSE)
}
# Print all the MSE's calculate
print(all_errors)
# Print the mean of the errors calculated
print(mean(all_errors))


Earn_Test <- read.csv("Earnings_Test_Students.csv")
mytestprediction3 <- Earn_Test
Testpredict<-predict(tree2, newdata=Earn_Test)
Testpredict
mytestprediction3$Earnings <- Testpredict

submission <- mytestprediction3[,c('ID','Earnings')]
write.csv(submission, file = "mytestsubmission3.csv",row.names=FALSE)

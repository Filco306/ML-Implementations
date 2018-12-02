library(readxl)
# Test set from ML course. 
data = read_excel("spambase.xlsx")


knearest = function(data, K, newData) {
  #data is the training data, 
  #newData returns the predicted class probabilities
  # for newData by using K-nearest neighbour approach. 
  responsesX = as.matrix(data[,ncol(data)])
  responsesY = as.matrix(data[,ncol(newData)])
  
  X = as.matrix(data[,-c(ncol(data))])
  Y = as.matrix(newData[,-c(ncol(newData))])
  
  
  Xhat = X/sqrt(rowSums(X^2))
  Yhat = Y/sqrt(rowSums(Y^2))
  
  
  C_matrix = Xhat%*%t(Yhat)
  
  D_matrix = 1 - C_matrix
  # For the data test set, 
  # take the distance to the training data points, 
  # take the k nearest neighbours and classify them
  # as the average of the training set's labels of the k points
  
  # return the indices of the k nearest neighbours 
  # for all different new data points, generating a matrix with the k nearest neighbou
  indicesKnearest = apply(D_matrix, 2, function(col, k){
    names(col) = seq(1, length(col), 1)
    
    col = sort(col)
    
    return(strtoi(names(col[1:k])))
  }, K)
  
  classifications = apply(indicesKnearest, 2, function(row) {
    # Classify according to the mean of the training data
    avg = mean(responsesX[row,])
    return(round(avg))
  })
  
  
  return(classifications)
}

# Use same seed and partition as before. 
# Test partition according to Oleg. 
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

labelsTest = knearest(train, 30, test)
labelsTrain = knearest(train, 30, train)
table(train$Spam, labelsTrain)
table(test$Spam, labelsTest)

print("misclassification rate train")
1 - sum(diag(table(train$Spam, labelsTrain)))/sum(table(train$Spam, labelsTrain))
print("misclassification rate test")
1 - sum(diag(table(test$Spam, labelsTest)))/sum(table(test$Spam, labelsTest))

classificationRate = function(confMatrix) {
  return(sum(diag(confMatrix))/sum(confMatrix))
}

# Compare with kknn package
library("kknn")

data$Spam = factor(data$Spam)
# Use same seed and partition as before. 
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]


kkn.fit = kknn(Spam ~., train = train, test = train, k = 30)

print("misclassification rate train")
table(train$Spam, kkn.fit$fitted.values)
1 - classificationRate(table(train$Spam, kkn.fit$fitted.values))


kkn.fit = kknn(Spam ~., train = train, test = test, k = 30)

print("misclassification rate test")
table(test$Spam, kkn.fit$fitted.values)
1 - classificationRate(table(test$Spam, kkn.fit$fitted.values))

#### Density estimation

attach(mtcars)

cars = cars
detach(mtcars)
k = 6

volume_N_sphere = function(nDim, radius) {
  n = nDim
  R = radius
  return(((pi^(n/2))/gamma(n/2 + 1))*R^n)
}

kNearestNeighDensEstimation = function(data, k = 6, newData) {
  # data is the training data
  # newData is the data 
  
  N = nrow(data)
  
  nDim = ncol(data)
  X = as.matrix(data) # We do not assume responses to exist
  Y = as.matrix(newData) # We do not assume responses to exist
  
  
  
  
  D_matrix = abs(apply(Y, 1, function(row) {
    return(abs(row - X))
  }))
  # return the indices of the k+1 nearest neighbours
  # for all different new data points, generating a matrix with the k nearest neighbours
  # Since we include the point itself, we return k+1 nearest radiuses. 
  
  
  radiuses = apply(D_matrix, 2, function(col, k) {
    
    
    sortedCol = sort(col)
    
    
    return(sortedCol[k])
  }, k+1)
  
  # Making sure no radius is 0
  for (i in 1:length(radiuses)) {
    if (radiuses[i] == 0) {
      #If it is 0, do linear extrapolation
      radiuses[i] = (radiuses[i-1] + radiuses[i+1])/2
    }
  }
  
  #Density estimation
  densities = apply(as.matrix(radiuses), 1, function(R, nDim) {
    
    return(k/(N*volume_N_sphere(nDim, R)))
  }, nDim)
  
  
  return(densities)
}

plot_dens = seq(0,max(cars$speed), by = 0.01)

densities = kNearestNeighDensEstimation(data = as.matrix(cars$speed), 
                                        k=6, newData = plot_dens)

hist(cars$speed, 
     breaks = 20, 
     freq = FALSE, 
     ylim=c(0, max(densities)), 
     xlab = "Speed", 
     main = "Histogram vs k-nearest density, K= 6")
lines(x=plot_dens, y = densities, lwd = 3, col = "red")
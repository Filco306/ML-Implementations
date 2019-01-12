library(Filips.ML.package)
library(MASS)

# Trying with code and trying the LDA as instructed lda() function help example. 
for (i in 1:10) {
  set.seed(i)
  Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                     Sp = rep(c("s","c","v"), rep(50,3)))
  trainL <- sample(1:150, 75)
  
  z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = trainL)
  preds = predict(z, Iris[-trainL, ])$class
  
  
  # Now trying my own implementation
  classifs = multiclassLDA(train = Iris[trainL,-c(5)], labelsTrain = Iris[trainL,c(5)], priors = c(1,1,1)/3, validation = Iris[-trainL,-c(5)])
  print(table(Iris[-trainL,]$Sp, preds) == table(Iris[-trainL,]$Sp, classifs))
  
  
}
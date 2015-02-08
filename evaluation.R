#packages
library(tm)
library(rJava)
library(RWeka)
library(caret)

# CONTANTS
algorithms <- c("J48", "svmLinear", "knn", "bayesglm", "rpart", "mlp")
path.matrices <- "matrices/"
# FUNCTIONS

# evaluate model for dataset and algorithm
evaluate_model <- function(dataset, algorithm) {
  fit <- train(DOC.CLASS ~ .,
               data = dataset, 
               method = algorithm,
               trControl = trainControl(method = "cv"),
               tuneLength = 1)
  fit$results$Accuracy[1]
}

# PREPARE
files.matrices <- list.files(recursive=TRUE, path=path.matrices)
matrices.names <- c()
matrices.data <- list()
for (file in files.matrices) {
  matrix <- read.csv(paste(path.matrices, file, sep=''))
  matrices.names <- c(matrices.names, file)
  matrices.data[length(matrices.data)+1] <- list(matrix)
}

results <- data.frame(matrices.names)
# EVALUATE
for (algorithm in algorithms) {
  print(paste("Starting algorithm: ", algorithm))
  alg.results <- c()
  for (x in 1:length(matrices.names)) {
    print(paste("For data: ", matrices.names[x]))
    data <- data.frame(matrices.data[x])   
    tryCatch ({
      accuracy <- evaluate_model(data, algorithm)
    }, error = function(e) {
      data <- NA
    })
    alg.results <- c(alg.results, accuracy)
  }
  results[algorithm] <- alg.results
}

# compute best
best.accuracy.vector <- c()
best.algorithm.vector <- c()
for (row in 1:nrow(results)) {
  best.accuracy <- 0
  best.algorithm <- NA
  for (col in algorithms) {
    if (! is.na(results[row, col])) {
      if (results[row, col] > best.accuracy) {
        best.accuracy <- results[row, col]
        best.algorithm <- col
      }
    }
  }
  best.accuracy.vector <- c(best.accuracy.vector, best.accuracy)
  best.algorithm.vector <- c(best.algorithm.vector, best.algorithm)
}
results["best"] <- best.accuracy.vector
results["best_alg"] <- best.algorithm.vector

# csv output of results
write.csv(results, file="results.csv")

#packages
library(tm)
library(rJava)
library(RWeka)
library(caret)

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
# loading tabloids
tPath="datasets/tabloids/"
files.tabloid <- list.files(recursive=TRUE, path=tPath)
# add path to filenames
for (x in 1:length(files.tabloid)) {
  files.tabloid[x] <- paste(tPath, files.tabloid[x], sep='')
}

# loading broadsheets
bPath="datasets/broadsheets/"
files.broadsheet <- list.files(recursive=TRUE, path=bPath)
# add path to filenames
for (x in 1:length(files.broadsheet)) {
  files.broadsheet[x] <- paste(bPath, files.broadsheet[x], sep='')
}

# create vector of tabloids
documents.tabloid <- vector("character", length(files.tabloid))
for (x in 1:length(files.tabloid)) {
  file <- readLines(files.tabloid[x], encoding="UTF-8")
  
  documents.tabloid[x] <- file
}

# create vector of broadsheets
documents.broadsheet <- vector("character", length(files.broadsheet))
for (x in 1:length(files.broadsheet)) {
  file <- readLines(files.broadsheet[x], encoding="UTF-8")
  
  documents.broadsheet[x] <- file
}

# only utf8 characters
documents.tabloid <- iconv(enc2utf8(documents.tabloid), sub = "byte")
documents.broadsheet <- iconv(enc2utf8(documents.broadsheet), sub = "byte")

# create Corpus of tabloids and transform
corpus.tabloid <- VCorpus(VectorSource(documents.tabloid))
corpus.tabloid <- tm_map(corpus.tabloid, stripWhitespace)
corpus.tabloid <- tm_map(corpus.tabloid, removePunctuation)
corpus.tabloid <- tm_map(corpus.tabloid, removeNumbers)
corpus.tabloid <- tm_map(corpus.tabloid, content_transformer(tolower))
corpus.tabloid <- tm_map(corpus.tabloid, removeWords, stopwords("english"))
corpus.tabloid <- tm_map(corpus.tabloid, stemDocument, language="english")

# create Corpus of broadsheets and transform
corpus.broadsheet <- VCorpus(VectorSource(documents.broadsheet))
corpus.broadsheet <- tm_map(corpus.broadsheet, stripWhitespace)
corpus.broadsheet <- tm_map(corpus.broadsheet, removePunctuation)
corpus.broadsheet <- tm_map(corpus.broadsheet, removeNumbers)
corpus.broadsheet <- tm_map(corpus.broadsheet, content_transformer(tolower))
corpus.broadsheet <- tm_map(corpus.broadsheet, removeWords, stopwords("english"))
corpus.broadsheet <- tm_map(corpus.broadsheet, stemDocument, language="english")

# DATA FOR EVALUATION
algorithms <- c("J48", "JRip", "svmLinear", "rf", "knn", "lda", "plr", 
               "bayesglm", "rpart", "avNNet", "mlp", "ada")


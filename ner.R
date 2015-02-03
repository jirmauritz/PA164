# You need to have stanford-ner.jar package on your classpath before running this script !
#packages
library(tm)
library(stringr)

# CONSTANTS
tPath = "datasets/tabloids/"
bPath = "datasets/broadsheets/"
tNerPath = "datasets/tabloidsNER/"
bNerPath = "datasets/broadsheetsNER/"
EOFSign = "ENDOFFILE"
attrs = c('/TIME', '/LOCATION', '/ORGANIZATION', '/PERSON', '/MONEY', '/PERCENT', '/DATE')

# FUNCTIONS
appendEndOfFile <- function(files){
  for (f in files) {
    write(EOFSign, file=f,append=TRUE)
  }  
}

deleteEndOfFile <- function(files){
  for (f in files) {
    lines <- readLines(f, encoding="UTF-8")
    if (lines[length(lines)] == "ENDOFFILE") {
      newFileName = paste(f, '_', sep='')
      write(lines[1:length(lines)-1],file=newFileName,append=FALSE)
      file.rename(newFileName, f)
    }    
  }    
}

createMatricesOfCount <- function(tabloids, broadsheets) {
  for (x in 1:length(tabloids)) {
    newdoc = ''
    for (attr in attrs) {
      count <- str_count(tabloids[x], attr)
      newdoc = paste(newdoc, paste(rep(attr[2:length(attr)], count), collapse = ' '))
    }
    tabloids[x] <- newdoc
  }
  
  for (x in 1:length(broadsheets)) {
    newdoc = ''
    for (attr in attrs) {
      count <- str_count(broadsheets[x], attr)
      newdoc = paste(newdoc, paste(rep(attr[2:length(attr)], count), collapse = ' '))
    }
    broadsheets[x] <- newdoc
  }
  
  corpus.tabloid <- VCorpus(VectorSource(tabloids))
  corpus.broadsheet <- VCorpus(VectorSource(broadsheets))
  dt.Matrix.tabloid <- DocumentTermMatrix(corpus.tabloid)
  dt.Matrix.broadsheet <- DocumentTermMatrix(corpus.broadsheet)
  
  dt.Data.t <- data.frame(inspect(dt.Matrix.tabloid))
  dt.Data.t$DOC.CLASS <- rep("tabloid", 1000)
  dt.Data.b <- data.frame(inspect(dt.Matrix.broadsheet))
  dt.Data.b$DOC.CLASS <- rep("broadsheet", 1000)
  dt.Data <- rbind(dt.Data.t, dt.Data.b)
  
  tfidf.Matrix.tabloid <- DocumentTermMatrix(corpus.tabloid,
                                             control = list(weighting = 
                                                              function(x)
                                                                weightTfIdf(x, normalize=FALSE)))
  tfidf.Matrix.broadsheet <- DocumentTermMatrix(corpus.broadsheet,
                                                control = list(weighting = 
                                                                 function(x)
                                                                   weightTfIdf(x, normalize=FALSE)))
  
  tfidf.Data.t <- data.frame(inspect(tfidf.Matrix.tabloid))
  tfidf.Data.t$DOC.CLASS <- rep("tabloid", 1000)
  tfidf.Data.b <- data.frame(inspect(tfidf.Matrix.broadsheet))
  tfidf.Data.b$DOC.CLASS <- rep("broadsheet", 1000)
  tfidf.Data <- rbind(tfidf.Data.t, tfidf.Data.b)
  
  list(dt.Data, tfidf.Data)
}


# PREPARE
# loading tabloids
files.tabloid <- list.files(recursive=TRUE, path=tPath)
# add path to filenames
for (x in 1:length(files.tabloid)) {
  files.tabloid[x] <- paste(tPath, files.tabloid[x], sep='')
}

# loading broadsheets
files.broadsheet <- list.files(recursive=TRUE, path=bPath)
# add path to filenames
for (x in 1:length(files.broadsheet)) {
  files.broadsheet[x] <- paste(bPath, files.broadsheet[x], sep='')
}

# Named Entity Recognition - make couple of tagged lines from all documents
command = paste('java', '-mx600m', 'edu.stanford.nlp.ie.crf.CRFClassifier', '-loadClassifier', 'resources/classifiers/english.muc.7class.distsim.crf.ser.gz', '-textFile ')

# prepare files
appendEndOfFile(files.tabloid)
appendEndOfFile(files.broadsheet)

# tag
lines.tabloid <- system(paste(command, tPath, sep=''), intern = TRUE)
lines.broadsheet <- system(paste(command, bPath, sep=''), intern = TRUE)

# return files to the previous form
deleteEndOfFile(files.tabloid)
deleteEndOfFile(files.broadsheet)

# only utf8 characters
lines.tabloid <- iconv(enc2utf8(lines.tabloid), sub = "byte")
lines.broadsheet <- iconv(enc2utf8(lines.broadsheet), sub = "byte")


# split lines to documents by EOF sign
documents.tabloid <- vector("character", 1000)
counter <- 1
for (line in lines.tabloid) {
  if (grepl(EOFSign, line)) {
    counter <- counter + 1
  } else {
    documents.tabloid[counter] <- paste(documents.tabloid[counter], line, sep=' ')
  }
}

documents.broadsheet <- vector("character", 1000)
counter <- 1
for (line in lines.broadsheet) {
  if (grepl(EOFSign, line)) {
    counter <- counter + 1
  } else {
    documents.broadsheet[counter] <- paste(documents.broadsheet[counter], line, sep=' ')
  }
}

# PROCESS MATRICES
matricesOfCount <- createMatricesOfCount(documents.tabloid, documents.broadsheet)

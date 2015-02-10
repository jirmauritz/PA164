library("tm")

####################
# FUNKCE
# cesta -> obsah souboru v utf8 (jako character)
getCont <- function(fpath) {
  somefile <- readLines(file(fpath))
  vect <- enc2utf8(somefile) # kvuli tolower()
  return(paste(vect, collapse = "\n"))
}

# adresar -> seznam relativnich cest k souborum v adresari
relPaths <- function(dir) lapply(list.files(dir),
                               (function(f) paste0(dir,f)))

# vytvori data.frame pres DTM z korpusu, odstrani se sparse terms
makeDTM <- function(rst, ctrl=list()){
  dtm <- DocumentTermMatrix(corpus, control = ctrl)
  dtm <- removeSparseTerms(dtm, rst)
  as.data.frame(inspect(dtm))
}

####################

# vektor cest
l <- c(relPaths("datasets/tabloids/"),relPaths("datasets/broadsheets/"))

# do nl se nactou obsahy souboru v obou adresarich
nl <- list()
for(i in l) {
  nl <- c(nl, getCont(i))
  closeAllConnections()
  # protoze "lapply(l,rmHead)" hlasi:
  #     "all connections are in use" FUN(X[[126L]], ...)  
}

# vytvori se korpus
corpus <- Corpus(VectorSource(nl))
  rm(i,l,nl,relPaths,getCont)
# aplikuji se transformace
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
#corpus <- tm_map(corpus, stemDocument)



# argument:  
# A numeric for the maximal allowed sparsity in the range from 
# bigger zero to smaller one.
dtm <- makeDTM(0.90)

dtm.w <- makeDTM(0.90, ctrl=list(weighting =
                          function(x) weightTfIdf(x, normalize = T)))


doc.class <- matrix(c(rep("tabloid",1000),rep("broadsheet",1000)),
            nrow = 2000, ncol = 1, dimnames = list(c(),c("DOC.CLASS")))

dtm <- cbind.data.frame(dtm,doc.class)
dtm.w <- cbind.data.frame(dtm.w,doc.class)

dir.create("matrices", showWarnings = FALSE)
write.csv(dtm, "matrices/tf.bagow.csv")
write.csv(dtm.w, "matrices/tfidf.bagow.csv")



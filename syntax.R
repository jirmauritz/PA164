# You need to have stanford-parser.jar package on your classpath before running this script !
#libraries
library(methods)
# dependencies
source("R_parse_trees/treeclass.R")
source("R_parse_trees/treeprinter.R")
source("R_parse_trees/nlp_tree_parser.R")
source("R_parse_trees/sleuth_runner.R")

# CONSTANTS
tPath = "datasets/tabloids/"
bPath = "datasets/broadsheets/"

# FUNCTIONS
splitOutputToDocuments <- function(text) {
  res = c()
  doc <- ''
  # first line is separator, that's why [2:]
  for (line in text[3:length(text)]) {
    if (grepl('^\\(.*', line)) {
      if (doc == '') {
        doc <- line
      } else {
        doc <- paste(doc, line, sep='\n')
      }
    }
    if (grepl('Parsing file:', line)) {
      res <- c(res, doc)
      doc <- ''
    }
  }
  res <- c(res, doc)
  res
}

# PREPARE
print("Preparing data...")
# Syntax parsing - make tree out of each sentence
command = paste('java', '-mx500m', 'edu.stanford.nlp.parser.lexparser.LexicalizedParser', '-outputFormat', 'oneline', 'resources/models/englishPCFG.caseless.ser.gz', '2>&1 ')

# tag
trees.tabloid <- splitOutputToDocuments(system(paste(command, tPath, '*', sep=''), intern=TRUE))
trees.broadsheet <- splitOutputToDocuments(system(paste(command, bPath, '*', sep=''), intern=TRUE))

# FIND SUBTREES
print("Finding subtrees...")
## set these parameters !!!:
min_support <- 0.5
induced_trees <- T  # if FALSE, then embedded trees
ordered_trees <- T

## you should be OK with these parameter settings:
sleuth_directory <- if (Sys.info()['sysname'] == "Windows") {
  "R_parse_trees/sleuth_win"
} else if (Sys.info()['sysname'] == "Linux") {
  "R_parse_trees/sleuth_linux"
}

input_for_sleuth <- "input.txt"

tree_strings <- strsplit(paste(trees.tabloid, collapse='\n'), '\n')[[1]]
tree_strings <- c(tree_strings, strsplit(paste(trees.broadsheet, collapse='\n'), '\n')[[1]])
node_texts_encoding <- get_node_texts_encoding(tree_strings) # create encoding of nodes
trees <- parse_trees(tree_strings)  # parse trees


print_out_for_sleuth(trees, paste0(sleuth_directory,"/", input_for_sleuth))  # prepare data for Sleuth
sleuth_results <- sleuth(sleuth_directory, input_for_sleuth, min_support, induced = induced_trees, ordered = ordered_trees) # run Sleuth and get results
sleuth_results_parsed <- parse_sleuth_results(sleuth_results, induced = induced_trees)  # parse results

subtrees <- sapply(sleuth_results_parsed$treeCodes, parse_tree_code)  # build subtrees from parsed results
subtree_occurrences <- sleuth_results_parsed$treeOccurrences  # get list of occurrences (for each subtree, get indices of input trees; indexed from 0)

# output subtrees to console
for (i in seq_along(subtrees)) {
  cat("Subtree ", i, ":\n", sep = "")
  simple.print(subtrees[[i]], node_texts_encoding)
}

# create matrix of occurrences; rows = trees; columns = subtrees; value = TRUE (subtree is in that tree) / FALSE (it isn't)
occurrence_matrix <- create_occurrence_matrix(length(trees), subtree_occurrences)

# merge sentences into documents
  # for tabloids
tf.matrix <- data.frame()
pos <- 0
for (doc in trees.tabloid) {
  count.of.lines <- length(strsplit(doc, '\n')[[1]])
  docline = c()
  for (subtree in 1:ncol(occurrence_matrix)) {
    sum <- 0
    for (sent in 1:count.of.lines) {
      if (occurrence_matrix[pos+sent, subtree]) {
        sum <- sum + 1
      }
    }
    docline <- c(docline, sum)
    docline
  }
  tf.matrix <- rbind(tf.matrix, docline)
  pos <- pos + count.of.lines
}

  # for broadsheets
for (doc in trees.broadsheet) {
  count.of.lines <- length(strsplit(doc, '\n')[[1]])
  docline = c()
  for (subtree in 1:ncol(occurrence_matrix)) {
    sum <- 0
    for (sent in 1:count.of.lines) {
      if (occurrence_matrix[pos+sent, subtree]) {
        sum <- sum + 1
      }
    }
    docline <- c(docline, sum)
    docline
  }
  tf.matrix <- rbind(tf.matrix, docline)
  pos <- pos + count.of.lines
}

tf.matrix$DOC.CLASS <- c(rep("tabloid",length(tree.tabloid)),rep("broadsheet",length(tree.broadsheet)))

# OUTPUT MATRICES
dir.create("matrices", showWarnings = FALSE)
write.csv(tf.matrix, file="matrices/tf.matrix.subtrees.csv")


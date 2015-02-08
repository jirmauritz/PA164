
## print out in Sleuth format
print_out_for_sleuth <- function(trees, filename) {
  lines <- sapply(trees, function(x) {
    tree.as.list <- tolist(x)
    return(paste0(c(length(tree.as.list), tree.as.list), collapse=" "))
  })
  trees.for.sleuth <- data.frame(c(0:(length(lines)-1)), c(0:(length(lines)-1)), lines)
  write.table(trees.for.sleuth, file=paste0(filename), sep=" ", eol="\r", row.names=F, col.names=F, quote=F, fileEncoding = "UTF-8")
}


# support
# induced or embedded
# ordered or unordered
sleuth <- function(sleuth_dir, input_file, support = 0.8, induced = T, ordered = T) 
{
  induced <- if (induced) "-I" else ""
  ordered <- if (ordered) "-O" else ""
  oldwd <- getwd()
  setwd(paste0(oldwd, "/", sleuth_dir))
  if (Sys.info()['sysname'] == "Windows") {
    res <- system(paste("vtreeminer.exe -i",input_file,"-s", support, "-o ", induced," ", ordered," -l", 
                        sep = " "),  intern = TRUE)  
  } else if (Sys.info()['sysname'] == "Linux") {
    res <- system(paste("./vtreeminer -i",input_file,"-s", support, "-o ", induced," ", ordered," -l", 
                        sep = " "),  intern = TRUE)
  } else {
    stop("Sleuth runner script is not adapted to your OS")
  }
  
#   system(paste("vtreeminer.exe -i input.txt -s", support, "-o ", induced," ", ordered," -l", 
#                       sep = " "))
  setwd(oldwd)
  return(res)
}

## returns two elements: tree codes and occurrence places of them
parse_sleuth_results <- function(lines, induced = TRUE) {
  
  occurrences <- numeric()
  reading_occurrences <- T
  
  treetokens <- character()
  treeCode <- NULL
  
  list_of_trees <- list()
  list_of_occurences <- list()
  
  i <- 1
  done <- 0
  num_of_lines <- length(lines)
  pb <- txtProgressBar(min = 1, max = num_of_lines, style = 3)
  for (line in lines) {
    splitted <- strsplit(line, " ")[[1]]
    begins_with_number <- is.numeric(type.convert(splitted[1]))  # check whether first token can be numeric
    begins_with_tabulator <- grepl("^\t", line)
    if (begins_with_tabulator) {
      ends_with_1 <- grepl("1$", line)
      if (ends_with_1 | !induced) {  ## inducted subtrees
        occ <- as.numeric(splitted[1])
        if (!(occ %in% occurrences)) {
          occurrences <- c(occurrences, occ)
        }
      }
    } else if (begins_with_number & as.numeric(splitted[1]) >= 0) {
      reading_occurrences <- T
      if (splitted[length(splitted)-2] == "") {
        treetokens <- splitted[1:(length(splitted)-3)]
      } else {
        treetokens <- splitted[1:(length(splitted)-2)]
      }
    } else if (splitted[1] == "item") {
      reading_occurrences <- T
      treetokens <- splitted[2]
    } else {
      if (!is.null(treeCode) & length(occurrences) > 0) {
        list_of_trees[[length(list_of_trees)+1]] <- as.numeric(treeCode)
        list_of_occurences[[length(list_of_occurences)+1]] <- occurrences
        treeCode <- NULL
      }
    }
    
    if (reading_occurrences) {
      if (!is.null(treeCode) & length(occurrences) > 0) {
        list_of_trees[[length(list_of_trees)+1]] <- as.numeric(treeCode)
        list_of_occurences[[length(list_of_occurences)+1]] <- occurrences
      }
      reading_occurrences <- F
      occurrences <- numeric()
      treeCode <- treetokens
    }
    
    setTxtProgressBar(pb, i)
    i <- i+1
  }
  close(pb)
  return(list(treeCodes = list_of_trees, treeOccurrences = list_of_occurences))
}



parse_tree_code <- function(treeCode) {
  index <<- 1
  current_tree <- Tree(treeCode[index])
  index <<- index+1
  while (index <= length(treeCode)) {
    current_tree <- .parse_tree_code_rec(treeCode, current_tree)
  }
  return(current_tree)
}

.parse_tree_code_rec <- function(treeCode, parenttree) {
  tree <- Tree(treeCode[index])
  index <<- index + 1
  while (index <= length(treeCode) & treeCode[index] != -1) {
    tree <- .parse_tree_code_rec(treeCode, tree)
  }
  index <<- index + 1
  parenttree@subtrees <- c(parenttree@subtrees, tree)
  return(parenttree)
}


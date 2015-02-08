

# get all strings in the data
get_all_node_texts <- function(strings) {
  res <- strsplit(strings, "[\\(\\) ]", fixed = F)
  res <- lapply(res, function(x) x[x != ""])  # remove empty strings
  res <- unlist(res)
  return(unique(res))
}

# get table with node encoding
get_node_texts_encoding <- function(node_texts) {
  node_texts <- get_all_node_texts(tree_strings)
  return(data.frame(text = node_texts, num = 1:length(node_texts)))
}



# helper for parseTrees function
.parseSubtreeFromString <- function(chars) {
  result_trees <- NULL
  current_tree <- NULL
  current_subtrees <- NULL
  last_non_parenthesis <- -50
  while (index <= length(chars)) {
    if (chars[index] == "(") {
      index <<- index + 1
      subtree <- .parseSubtreeFromString(chars)
      if (is.null(current_tree@subtrees) || length(current_tree@subtrees) == 0) {
        current_tree@subtrees <- subtree
      } else {
        current_tree@subtrees <- c(current_tree@subtrees, subtree)
      }
    } else if (chars[index] == ")") {
      index <<- index + 1
      result_trees <- c(result_trees, current_tree)  # dont forget to add the last tree
      return(result_trees)
      
    } else {
      if (last_non_parenthesis + 1 != index) {  # if there is a leaf (neighbor of a NONTERMINAL), ignore it
        current_tree <- Tree(node_texts_encoding[node_texts_encoding$text == chars[index], "num"])
      } else {  ## it should be singleton leaf with TERMINAL
        current_tree@subtrees <- c(Tree(node_texts_encoding[node_texts_encoding$text == chars[index], "num"]))
      }
      last_non_parenthesis <- index
      index <<- index + 1
    }
  }
}

# helper for parseTrees function
.parseTreeFromString <- function(string) {
  string <- substr(string, 2, nchar(string)-1) # remove first and last parenthesis
  string <- gsub("\\(", " \\( ", string, perl = F)
  string <- gsub("\\)", " \\) ", string, perl = F)
  string <- gsub(" +", " ", string, perl = T)
  chars <- strsplit(string, " ", fixed = FALSE)[[1]]
  current_tree <- NULL
  current_subtrees <- NULL
  index <<- 1
  while (index <= length(chars)) {
    if (chars[index] == "(") {
      index <<- index + 1
      subtree <- .parseSubtreeFromString(chars)
      if (is.null(current_tree@subtrees) || length(current_tree@subtrees) == 0) {
        current_tree@subtrees <- subtree
      } else {
        current_tree@subtrees <- c(current_tree@subtrees, subtree)
      }
      
    
    } else if (chars[index] == ")") {
      
    } else {
      if (is.null(current_tree)) {
        current_tree <- Tree(node_texts_encoding[node_texts_encoding$text == chars[index], "num"])
        
      } else {
        
      }
    }
    index <<- index + 1
  }
  return(current_tree)
}

# parse trees from character vector
parse_trees <- function(tree_strings) {
  number_of_trees <- length(tree_strings)
  pb <- txtProgressBar(min = 1, max = number_of_trees, style = 3)
  trees <- mapply(function(tree, x) {
    res <- .parseTreeFromString(tree)
    setTxtProgressBar(pb, x)
    return(res)
  }, tree_strings, c(1:number_of_trees))
  close(pb)
  return(trees)
}


create_occurrence_matrix <- function(number_of_trees, subtree_occurrences) {
  result_matrix <- matrix(F, nrow = number_of_trees, ncol = length(subtree_occurrences))
  for (i in seq_along(subtree_occurrences)) {
    result_matrix[subtree_occurrences[[i]] + 1, i] <- TRUE
  }
  return(result_matrix)
}
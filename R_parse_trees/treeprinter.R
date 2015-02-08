


.getTextLinesRec <- function(tree,  ind, labelcoding=NULL, errortree = NULL, inverse = F) {
  if (is.null(labelcoding)) {
    el <- tree@elem  
  } else {
    el <- as.character(labelcoding[labelcoding$num == tree@elem, "text"])
  }
  errrors <- character(0)
  if (!is.null(errortree)) {
    errrors <- paste0("           $$E: ", paste(errortree@elem, collapse=", "))
  }
  indent <- paste(c(substr(ind, 1, nchar(ind)-5), if (nchar(ind) > 0) "o---" else ""), collapse = "")
  result <- paste(c(indent, el, errrors), collapse = "")
  subtrees <- ""
  if (length(slot(tree, "subtrees")) != 0  & class(tree) != "EmptyTree" & !is.null(slot(tree, "subtrees"))) {
    subs <- if (inverse) rev(tree@subtrees) else tree@subtrees
    if (!is.null(errortree)) {
      errsubs <- if (inverse) rev(errortree@subtrees) else errortree@subtrees
    }
    subtrees <- character(0)
    for (x in 1:length(subs)) {
      if (!is.null(errortree)) {
        errsub <- errsubs[[x]]
      } else {
        errsub <- NULL
      }
      if (x < length(subs)) {
        sub <- .getTextLinesRec(subs[[x]], paste0(ind, "|    "), labelcoding, errsub, inverse)  
      } else {
        sub <- .getTextLinesRec(subs[[x]], paste0(ind, "     "), labelcoding, errsub, inverse)
      }
      subtrees <- c(subtrees, sub)
    }
    return(c(result, paste0(ind, "|    "), subtrees, paste0(ind, "    ")))
  }
  else {
    return(result)
  }  
}
.getTextLines <- function(tree, labelcoding=NULL, errortree = NULL, inverse = F) {
  lines <- .getTextLinesRec(tree, "", labelcoding, errortree, inverse)
  if (inverse) {
    lines <- rev(lines)
  }
  return(lines)
}

simple.print <- function(tree, labelcoding=NULL, errortree = NULL, inverse = F) {
  res <- .getTextLines(tree, labelcoding, errortree, inverse)
  cat(paste(res, collapse="\n"))
  cat("\n")
}

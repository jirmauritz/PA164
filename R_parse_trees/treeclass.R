

### tree class

setClass("Tree")

setClass("EmptyTree", contains="Tree")

# setClass("Internal", contains="Tree",
#          representation=representation(elem="ANY", left="Tree", right="Tree"),
#          prototype=prototype(left=new("EmptyTree"), right=new("EmptyTree")))

setClass("Internal", contains="Tree",
         representation=representation(elem="numeric", nodeid="numeric", subtrees="ANY"),
         prototype=prototype(nodeid=0, subtrees=list()))

# setGeneric("Tree", function(elem, left, right) standardGeneric("Tree"),
#            signature="elem")
# 
# setMethod(Tree, "missing", function(elem, left, right) new("EmptyTree"))
# 
# setMethod(Tree, "ANY", function(elem, left, right) {
#   new("Internal", elem=elem, left=left, right=right)
# })
         
setGeneric("Tree", function(elem, nodeid=0, subtrees=list()) standardGeneric("Tree"),
           signature="elem")

# setMethod(Tree, "missing", function(elem, subtrees) new("EmptyTree"))

setMethod(Tree, "ANY", function(elem, nodeid, subtrees) {
  new("Internal", elem=elem, nodeid=nodeid, subtrees=subtrees)
})       
setMethod(Tree, "ANY", function(elem, nodeid=0, subtrees=list()) {
  new("Internal", elem=elem, nodeid=nodeid, subtrees=subtrees)
}) 


# setMethod(Tree, "ANY", function(elem, subtrees=list()) {
#   new("Internal", elem=elem, subtrees=subtrees)
# })       
  


# setMethod(print, "Tree", function(x, ...) {
#   if (class(x) == "EmptyTree"){ 
#     print(99)
#   } else { 
#     print(slot(x, "elem"))
#   }
#    }
# )

recursivesymbol = -1

## used in tolist.. ending -1 must be deleted later
.tolist_recursive <- function(x) {
  if (length(slot(x, "subtrees")) == 0  | class(x) == "EmptyTree" | is.null(slot(x, "subtrees"))) {
    tail <- numeric()
  } else {
    tail <- c(sapply(slot(x, "subtrees"), .tolist_recursive))
  }
  a <- unlist(c(slot(x, "elem"), tail, recursivesymbol))
  return(a)
}

setGeneric("tolist", function(x) standardGeneric("tolist"))
setMethod(tolist, "Tree", 
          function(x) {
            a <- .tolist_recursive(x)
            ## return without last element
            return(a[-length(a)])
          }
)

setGeneric("encode.for.sleuth", function(x, id) standardGeneric("encode.for.sleuth"))
setMethod(encode.for.sleuth, "Tree", function(x, id) {
  treelist <- tolist(x)
  return(paste0(id, " ", id, " ", length(treelist), " ", paste(treelist, collapse=' ')))
})

setGeneric("is.binary.tree", function(x) standardGeneric("is.binary.tree"))
setMethod(is.binary.tree, "Tree", function(x) {
  binarynode <- length(x@subtrees) == 0 | length(x@subtrees) == 2
  if (length(x@subtrees) > 0) {
    subtrees <- sapply(x@subtrees, is.binary.tree)
  } else {
    subtrees <- T
  }
  return(all(binarynode, subtrees))
})


#### TreeData class - has Tree and data.frame

setClass("TreeData",
         representation=representation(id="integer", tree="Tree", metadata="ANY"),
         prototype=prototype(id=0L, tree="NULL", metadata=data.frame()))

setGeneric("TreeData", function(id, tree=NULL, metadata=data.frame()) standardGeneric("TreeData"),
           signature="id")

setMethod(TreeData, "ANY", function(id, tree, metadata) {
  new("TreeData", id=id, tree=tree, metadata=metadata)
})       
setMethod(TreeData, "ANY", function(id, tree=NULL, metadata=data.frame()) {
  new("TreeData", id=id, tree=tree, metadata=metadata)
}) 
setMethod(is.binary.tree, "TreeData", function(x) {
  return(is.binary.tree(x@tree))
})
setMethod(tolist, "TreeData", 
          function(x) {
            return(tolist(x@tree))
          }
)
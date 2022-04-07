##' adjust the tree branch position after assigning ancestor node
##'
##' @title adjust_ally
##' @param tree ggtree object
##' @param node internal node in tree
##' @param sub logical value.
##' @importFrom utils getFromNamespace
##' @importFrom ggtree geom_tiplab
##' @importFrom ggplot2 aes_
##' @return tree
##' @export
##' @author Lang Zhou

adjust_ally <- function(tree, node, sub = FALSE) {
  
  getSubtree <- getFromNamespace("getSubtree", "ggtree")
  if(sub) {
    sub <- getSubtree(tree,node = node)
    sub_ancenstor <- sub[!sub$isTip,]
    sub_ancenstor_node <- sub_ancenstor$node[order(sub_ancenstor$y)]
    for (i in sub_ancenstor_node) {
      tree <- adjust_treey(tree = tree, node = i)
    }
  }else {
    tree <- adjust_treey(tree = tree, node = node)
  }
  
  tree$data$node_color <- "black"
  tree$data[tree$data$node == node,"node_color"] <- "red"
  tree <- tree + geom_tiplab(aes_(color = ~I(node_color)),offset = 0.002)
  
  return(tree)
}

##' extract ancestor sequence from tree data
##'
##' @title extract_seq
##' @param tree_adjust ggtree object
##' @return character
##' @export
##' @author Lang Zhou
extract_seq <- function(tree_adjust) {
  data <- tree_adjust$data
  seq <- data[data$isTip,]$mol_seq
  names(seq) <- data[data$isTip,]$label
  tidy <- tidy_msa(seq)
  return(tidy)
}


adjust_treey <- function(tree, node) {
  tree$data$isTip[tree$data$node == node] <- TRUE
  tree$data$label[tree$data$node == node] <- 
    tree$data$name[tree$data$node == node]
  
  y_ancenstor <- tree$data$y[tree$data$node == node]
  tree$data$y[tree$data$y > y_ancenstor] <- 
    tree$data$y[tree$data$y > y_ancenstor] + 1
  tree$data$y[tree$data$node == node] <- 
    tree$data$y[tree$data$node == node] %>% ceiling
  return(tree)
}







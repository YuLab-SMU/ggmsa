##' plot Tree-MSA plot
##'
##'
##' 'treeMSA_plot()' automatically re-arranges the MSA data according to 
##' the tree structure,
##' @title treeMSA_plot
##' @param p_tree tree view
##' @param tidymsa_df tidy MSA data 
##' @param ancestral_node internal node in tree. Assigning a internal node to 
##' display "ancestral sequences"
##' @param sub logical value. Displaying a subset of ancestral sequences or not.
##' @param panel panel name for plot of MSA data
##' @param font font families, possible values are 'helvetical', 'mono', and 
##' 'DroidSansMono', 'TimesNewRoman'.  Defaults is 'helvetical'. 
##' If font = NULL, only plot the background tile.
##' @param color a Color scheme. One of 'Clustal', 'Chemistry_AA', 
##' 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'LETTER', 'CN6', 'Chemistry_NT', 
##' 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Chemistry_AA'.
##' @param ... additional parameters for 'geom_msa'
##' @importFrom ggtree geom_facet
##' @return ggplot object 
##' @author Lang Zhou
treeMSA_plot <- function(p_tree, 
                         tidymsa_df, 
                         ancestral_node = NULL, 
                         sub = FALSE,
                         panel = "MSA",
                         font = NULL,
                         color = "Chemistry_AA",
                         ...) {
  if(!is.null(ancestral_node)) {
    p_tree <- adjust_ally(p_tree, node = ancestral_node, sub = sub)
    tidymsa_df <- extract_seq(p_tree)
  }
    
  p_tree + geom_facet(geom = geom_msa, 
                      data = tidymsa_df,  
                      panel = panel,
                      font = font, 
                      color = color,
                      ...) 
  
}

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
  sub_tree <- getSubtree(tree,node = node)
  sub_ancestor <- sub_tree[!sub_tree$isTip,]
  if(sub) {
    sub_ancestor_node <- sub_ancestor$node[order(sub_ancestor$y)]
    for (i in sub_ancestor_node) {
      tree <- adjust_treey(tree = tree, node = i)
    }
  }else {
    tree <- adjust_treey(tree = tree, node = node)
  }
  
  tree$data$node_color <- "black"
  if(sub) {
      tree$data[tree$data$node %in% sub_ancestor$node,"node_color"] <- "red"
  }else {
      tree$data[tree$data$node == node,"node_color"] <- "red"
  }
  
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







##' plot Tree-MSA plot
##'
##'
##' 'treeMSA_plot()' automatically re-arranges the MSA data according to 
##' the tree structure,
##' @title treeMSA_plot
##' @param p_tree tree view
##' @param tidymsa_df tidy MSA data 
##' @param ancestral_node vector, internal node in tree. Assigning a internal 
##' node to display "ancestral sequences",If ancestral_node = "none" hides 
##' all ancestral sequences, if ancestral_node = "all" shows all ancestral 
##' sequences.
##' @param sub logical value. Displaying a subset of ancestral sequences or not.
##' @param panel panel name for plot of MSA data
##' @param font font families, possible values are 'helvetical', 'mono', and 
##' 'DroidSansMono', 'TimesNewRoman'.  Defaults is 'helvetical'. 
##' If font = NULL, only plot the background tile.
##' @param color a Color scheme. One of 'Clustal', 'Chemistry_AA', 
##' 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'LETTER', 'CN6', 'Chemistry_NT', 
##' 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Chemistry_AA'.
##' @param seq_colname the colname of MSA on tree$data
##' @param ... additional parameters for 'geom_msa'
##' @export
##' @importFrom ggtree geom_facet
##' @return ggplot object 
##' @author Lang Zhou
treeMSA_plot <- function(p_tree, 
                         tidymsa_df, 
                         ancestral_node = "none", 
                         sub = FALSE,
                         panel = "MSA",
                         font = NULL,
                         color = "Chemistry_AA",
                         seq_colname = NULL,
                         ...) {
  
  if(!ancestral_node == "none" && is.null(seq_colname)) {
    stop("pls assign the colname of MSA on tree$data by arguments 'seq_colname'!")
  } 
  
  
  if(!ancestral_node == "none") {
    p_tree <- adjust_ally(p_tree, node = ancestral_node, 
                          sub = sub,
                          seq_colname = seq_colname)
    
    tidymsa_df <- extract_seq(p_tree, 
                              seq_colname = seq_colname)
  }
  
  p <- p_tree + geom_facet(geom = geom_msa, 
                      data = tidymsa_df,  
                      panel = panel,
                      font = font, 
                      color = color,
                      ...)
  
  if(ancestral_node == "none") {
    p <- p  + geom_tiplab(offset = 0.002)
  }
  print(p)
  
  
}

##' adjust the tree branch position after assigning ancestor node
##'
##' @title adjust_ally
##' @param tree ggtree object
##' @param node internal node in tree
##' @param sub logical value.
##' @param seq_colname the colname of MSA on tree$data
##' @importFrom ggtree geom_tiplab
##' @importFrom ggplot2 aes_
##' @importFrom utils getFromNamespace
##' @return tree
##' @export
##' @author Lang Zhou

adjust_ally <- function(tree, node, sub = FALSE, seq_colname = "mol_seq") {
  getSubtree <- getFromNamespace("getSubtree", "ggtree")
  
  if(node == "all"){
    d <- tree$data
    ancestor_n <- d[!d$isTip & !is.na(d[,seq_colname][[1]]),"node"][[1]]
  }else {
    
    if(sub){
      ancestor_n <- lapply(node, function(i) {
        sub_tree <- getSubtree(tree,node = i)
        sub_ancestor <- sub_tree[!sub_tree$isTip,]
        ancestor_n <- sub_ancestor$node
        return(ancestor_n)
      })%>% unlist %>% unique
    }else {
      ancestor_n <- node
    }
    
  }
  
  for (i in ancestor_n) {
    tree <- adjust_treey(tree = tree, node = i)
  }
  
  tree$data$node_color <- "black"
  tree$data[tree$data$node %in% ancestor_n,"node_color"] <- "red"
  tree <- tree + geom_tiplab(aes_(color = ~I(node_color)),offset = 0.002)
  return(tree)
}

##' extract ancestor sequence from tree data
##'
##' @title extract_seq
##' @param tree_adjust ggtree object
##' @param seq_colname the colname of MSA on tree$data
##' @return character
##' @export
##' @author Lang Zhou
extract_seq <- function(tree_adjust, seq_colname = "mol_seq") {
  data <- tree_adjust$data
  seq <- data[data$isTip,seq_colname][[1]]
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









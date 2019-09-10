#' The color scheme of Culstal
#'
#' This function is a algorithm to assign color for Multiple Sequence .
#' @param color a data frame created manually for Clutal color assignment.
#' @param alndf data frame of alignment 
#' @keywords culstal

Clustal<- function(color, alndf){
  if(color == "Clustal"){
  alndf_up <- sapply(alndf, function(x) toupper(x))
  xx <- apply(alndf_up , 2, table)
  col_convert <- lapply(xx, function(seq_column) {
  clustal <- rep("#ffffff", length(seq_column)) ##The white as the background
  names(clustal) <- names(seq_column)
  r <- seq_column/sum(seq_column)
  for (pos in seq_along(seq_column)) {
    char <- names(seq_column)[pos]
    i <- grep(char, col_df$re_position)
    for (j in i) {
      
      if (col_df$type[j] == "combined"){
        rr <- sum(r[strsplit(col_df$re_gp[j], '')[[1]]], na.rm=T)
        if (rr > col_df$thred[j]) {
          clustal[pos] <- col_df$colour[j]}
      }
      else{
        rr1<-r[strsplit(col_df$re_gp[j], ',')[[1]]]
        if (any(rr1> col_df$thred[j],na.rm = T) ) {
          clustal[pos] <- col_df$colour[j]}
      }
      break
    }
  }
  return(clustal)
    })
  col_convert<- col_convert[!names(col_convert) %in% "name"]
  }
  return(col_convert)
}
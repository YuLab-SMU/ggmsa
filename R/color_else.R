##' A color scheme.This function assigns colors for Multiple Sequence .
##' 
##' @param color A data frame created manually for color assignment.
##' @param y  A data frame, data of a tidy fasta, created by 'tidy_fasta()'.
##' @keywords color scheme

color_scheme_ <- function(color){
  if(grepl("AA", color)){
    col<- scheme_AA[[color]]
    names(col) <- scheme_AA[,1]  ##  The name of aminio acid.
  }
  else{
    col <- scheme_NT[[color]]
    names(col) <- scheme_NT[,1] ## The name of bases.
  }
  return(col)
}

color_scheme <- function(y, color) {
    scheme <- color_scheme_(color)
    y$color <- scheme[y$character]
    return(y)
}

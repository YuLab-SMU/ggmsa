#' The color scheme of Culstal
#'
#' This function is a algorithm to assign color for Multiple Sequence .
#' @param color a data frame created manually for color assignment.
#' @keywords color scheme

color_scheme_ <- function(color){
  if(grepl("AA", color)){
    col<- color_scheme_AA[[color]]
    names(col) <- color_scheme_AA[,1]  ##  The name of aminio acid.
  }
  else{
    col <- color_scheme_nucle[[color]]
    names(col) <- color_scheme_nucle[,1] ## The name of bases.
  }
  return(col)
}

color_scheme <- function(y, color) {
    scheme <- color_scheme_(color)
    y$color <- scheme[y$character]
    return(y)
}

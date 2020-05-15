## @param color A data frame created manually for color assignment.

color_scheme <- function(y, color) {
    if(grepl("AA", color)){
        y$color <- scheme_AA[y$character, color]
    }
    else{
        y$color <- scheme_NT[y$character, color]
    }
  return(y)
}





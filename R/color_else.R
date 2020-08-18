## @param color A data frame created manually for color assignment.

color_scheme <- function(y, color) {
  if(grepl("AA", color)){
    y$color <- scheme_AA[y$character, color]
  }
  else{
    if (color == "LETTER") {
      # https://en.wikipedia.org/wiki/Help:Distinguishable_colors
      colmap <- c("#FF5005", "#FFFF80", "#990000", "#740AFF", "#E0FF66",
                  "#00998F", "#5EF1F2", "#FF0010", "#426600", "#FFA8BB",
                  "#FFA405", "#003380", "#C20088", "#9DCC00", "#8F7C00",
                  "#94FFB5", "#94FFB5", "#FFCC99", "#2BCE48", "#005C31",
                  "#191919", "#4C005C", "#993F00", "#0075DC", "#F0A3FF",
                  "#000000", "#FFFFFF")
      names(colmap) <- c(LETTERS, "-")
      y$color <- colmap[y$character]
    } else if (color == "CN6") {
      colmap <- c("#0075DC", "#4A98C9", "#000000",
                  "#FC8A6A", "#F14432", "#BC191A",
                  "#FFFFFF")
      names(colmap) <- c(LETTERS[1:6], "-")
      y$color <- colmap[y$character]
    }
    else {
      y$color <- scheme_NT[y$character, color]
    }
  }
  return(y)
}





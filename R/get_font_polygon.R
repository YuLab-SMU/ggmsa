##' Extract character glyphs from a specific font as outline polygons

##' @title get_font_polygon
##' @param family The family of the font. font_families() can be used to list font families loaded by 'sysfonts'
##' @return A list
##' @importFrom fontr glyph_polygon
##' @noRd
##' @author Lang Zhou
get_font_polygon <- function(family){
    cc <- c("A","R", "N", "D", "C", "E", "Q", "G", "H", "I", "L", 
            "K", "M", "F", "P", "S", "T", "W", "Y", "V", "U", "B", "-") # 'B is a degenerate base' 
    polygon <- lapply(cc, function(n) { 
      char <- glyph_polygon(n, family = family, face = "regular", nseg = 15)
      char <- char[!is.na(char$x),]
      char$order <- seq_along(char$x)
      return(char) 
    })
    names(polygon) <- cc
  
    d <- data.frame(x = c(0.05, 0.95, 0.95, 0.05),
                    y = c(0.05, 0.05, 0.2, 0.2),
                    order = 1:4)
    polygon[["-"]] <- d
    return(polygon)
}

##Save sysdata.rda##
#load("R/sysdata.rda")
#helvetical <- get_font_polygon("helvetical")
#times <- get_font_polygon("times")
#mono <- get_font_polygon("mono")
#font_fam <- list( helvetical = helvetical, times = times, mono = mono)
#save(list =c("font_fam", "scheme_AA", "scheme_clustal", "scheme_NT"), file = "R/sysdata.rda", compress='xz')

## Add New Font Families to 'sysfonts'##
#font_families() #List Font Families Loaded by 'sysfonts'
#font_add("helvetical", "extdata/Helvetica.ttf")
#font_add("times", "extdata/times.ttf" )

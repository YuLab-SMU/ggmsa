##' @method ggplot_add seqlogo
##' @export
ggplot_add.seqlogo <- function(object, plot, object_name) {
    msaData <- plot$layers[[1]]$data
    logo_tidyData <- msa2tidy(msaData)
    logo_font <- object$font
    logo_color <- object$color
  
    ly <- geom_seqlogo1(tidyData = logo_tidyData, font = logo_font, color = logo_color)
  
    ggplot_add(ly, plot, object_name)
}

##' @method ggplot_add seed
##' @export
ggplot_add.seed <- function(object, plot, object_name) {
    msaData <- plot$layers[[1]]$data
    seed_tidyData <- msa2tidy(msaData)
    seed <- object$seed
    star <- object$star
    
    ly <- geom_seed1(seed_tidyData, seed, star)
  
    ggplot_add(ly, plot, object_name)
}



##' @method ggplot_add GCcontent
##' @export
ggplot_add.GCcontent <- function(object, plot, object_name) {
    msaData <- plot$layers[[1]]$data
    GC_tidyData <- msa2tidy(msaData) 
    
    ly <- geom_GC1(GC_tidyData)
  
    ggplot_add(ly, plot, object_name)
}

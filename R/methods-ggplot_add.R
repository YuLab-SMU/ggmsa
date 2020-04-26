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


##' @importFrom ggplot2 facet_wrap
##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add facet_msa
##' @export
ggplot_add.facet_msa <- function(object, plot, object_name){
  msaData <- plot$layers[[1]]$data #调取msaData数据
  field <- object$field
  facetData <- facet_data(msaData, field)
  #num_facet <- max(facetData$facet) + 1  ##分段数目
  plot$layers[[1]]$data <- facetData #ly_bg
  if (length(plot$layers) > 1) {
    plot$layers[[2]]$data <- facetData #ly_label
  }
  plot + facet_wrap(~facetData$facet, ncol = 1)
  #ggplot_add(msa_facet, plot, object_name)
  
}



##' The MSA would be plot in a field that you set.

##' @title segment MSA
##' @param field a numeric vector of the field size
##' @examples
##' library(ggplot2) 
##' f <- system.file("extdata/sample.fasta", package="ggmsa")
##' # 2 fields
##' ggmsa(f, end = 120, color="Chemistry_AA") + facet_msa(field = 60)
##' # 3 fields 
##' ggmsa(f, end = 120, color="Chemistry_AA") + facet_msa(field = 40)
##' @export
##' @author Lang Zhou
facet_msa <- function(field) {
    structure(list( field = field),
              class = "facet_msa"
              )
}

facet_data <- function(msaData, field) {
    msaData$facet <- msaData$position %/% field ##设置分面变量
  
    msaData[msaData$position %% field == 0,]$facet <- 
        msaData[msaData$position %% field == 0,]$facet - 1 ##临界值调整
  
    msaData$x <- msaData$x - (msaData$facet * field) #分面对齐
    msaData$position <- msaData$position - (msaData$facet * field)
    return(msaData)
    #msa_facet <- facet_wrap(msaData$facet, nrow = num_facet)
}

##' @importFrom ggplot2 facet_wrap
##' @importFrom ggplot2 ggplot_add
##' @method ggplot_add facet_msa
##' @export
ggplot_add.facet_msa <- function(object, plot, object_name){
    msaData <- plot$layers[[1]]$data #调取msaData数据
    field <- object$field
    facetData <- facet_data(msaData, field)
    num_facet <- levels(factor(facetData$facet))  ##分段数目
    plot$layers[[1]]$data <- facetData #ly_bg
    plot$layers[[2]]$data <- facetData #ly_label
    
    plot + facet_wrap(~facetData$facet, nrow = num_facet)
    #ggplot_add(msa_facet, plot, object_name)

}

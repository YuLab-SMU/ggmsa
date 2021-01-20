##' The MSA would be plot in a field that you set.

##' @title segment MSA
##' @param field a numeric vector of the field size.
##' @examples
##' library(ggplot2)
##' f <- system.file("extdata/sample.fasta", package="ggmsa")
##' # 2 fields
##' ggmsa(f, end = 120, font = NULL, color="Chemistry_AA") + facet_msa(field = 60)
##' # 3 fields
##' ggmsa(f, end = 120, font = NULL,  color="Chemistry_AA") + facet_msa(field = 40)
##' @export
##' @author Lang Zhou
facet_msa <- function(field) {
    structure(list(field = field),
              class = "facet_msa"
              )
}

facet_data <- function(msaData, field) {

    if(min(msaData$position) > 1){
        pos_reset <- msaData$position - min(msaData$position)
        pos_reset[pos_reset == 0] <- 1
    }else {
        pos_reset <- msaData$position
    }
    msaData$facet <- pos_reset %/% field


    msaData[(pos_reset %% field) == 0, "facet"] <- msaData[(pos_reset %% field) == 0, "facet"] - 1

    # if ('x' %in% colnames(msaData))
    #     msaData$x <- msaData$x - (msaData$facet * field) #ly_label translation
    #
    # msaData$position <- msaData$position - (msaData$facet * field) #ly_bg translation
    return(msaData)
    #msa_facet <- facet_wrap(msaData$facet, nrow = num_facet)
}








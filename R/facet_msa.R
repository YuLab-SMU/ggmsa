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
    msaData$facet <- msaData$position %/% field

    msaData[msaData$position %% field == 0,]$facet <-
        msaData[msaData$position %% field == 0,]$facet - 1

    # if ('x' %in% colnames(msaData))
    #     msaData$x <- msaData$x - (msaData$facet * field) #ly_label translation
    #
    # msaData$position <- msaData$position - (msaData$facet * field) #ly_bg translation
    return(msaData)
    #msa_facet <- facet_wrap(msaData$facet, nrow = num_facet)
}








##' show method
##'
##'
##' @name show
##' @docType methods
##' @rdname show-methods
##' @title show method
##' @param object SeqDiff object
##' @return message
##' @importFrom methods show
##' @exportMethod show
##' @aliases SeqDiff-class
##'   show,SeqDiff-method
##' @usage show(object)
##' @examples
##' fas <- list.files(system.file("extdata", "GVariation", package="ggmsa"),
##'                   pattern="fas", full.names=TRUE)
##' x1 <- seqdiff(fas[1], reference=1)
##' x1
setMethod("show",signature(object="SeqDiff"),
          function(object) {
              cat("sequence differences of", paste0(names(object@sequence), collapse=" and "), '\n')
              d <- object@diff$difference %>% table %>% as.data.frame
              cat(sum(d$Freq), "sites differ:\n")
              freq <- d[,2]
              names(freq) <- d[,1]
              print(freq)
          })

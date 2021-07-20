##' @export
##' @importFrom ggplot2 qplot
##' @examples
##' fas <- list.files(system.file("extdata", "GVariation", package="ggmsa"),
##'                  pattern="fas", full.names=TRUE)
##' x <- lapply(fas, seqdiff)
##' plts <- lapply(x, plot)
##' plot_grid(plotlist=plts, ncol=1, labels=LETTERS[1:3])
cowplot::plot_grid

## ##' @export
## ##' @examples
## ##' rnorm(10) %>% mean
## magrittr::`%>%`

utils::globalVariables(".")

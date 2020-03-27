##' highlight the seed in miRNA sequences(test)
##'
##' We're trying to develop this function.
##' @title geom_seed
##' @param msa a multiple sequence alignment file or object
##' @param seed character, like 'GAGGUAG' a miRNA seed sequence
##' @importFrom Biostrings vmatchPattern
##' @return a ggplot layer
##' @author Lang Zhou 
##' @examples 
##' f <- system.file("extdata/seedSample.fa", package="ggmsa")
##' ggmsa(f, color="Chemistry_NT") + geom_seed(f, seed = "GAGGUAG")
##' @export
geom_seed <- function(msa, seed){
    prepare <- prepare_msa(msa)
    seedResult <- vmatchPattern(pattern = seed, subject = prepare)
    seedPos <- seedResult[[1]]@start # start position of seed region
    seedLen <- seedResult[[1]]@width # length of seed region
    numSeq <- length(seedResult) # number of sequences
    shadingLen <- .5 #shading width 

    x <- seedPos - .5 #the x coordinate of the lower left corner
    y <- 1 -.5 - shadingLen #the y coordinate of the lower left corner
    yy <- numSeq + .5 + shadingLen # #the y coordinate of the top right corner 
    xx <- x + seedLen #the x coordinate of the top right corner 
    d <- data.frame(x = c(x, x, xx, xx), 
                    y = c(y, yy, yy, y),
                    t = c('a', 'a', 'a','a'))

    mapping <- aes_(x= ~x, y= ~y, group= ~t, fill = ~I('#bebebe'), alpha = .8)
    ly_seed <- geom_polygon(data = d, mapping = mapping)
    return(ly_seed)
 }



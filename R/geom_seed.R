##' highlight the seed in miRNA sequences(test)
##'
##' We're trying to develop this function.
##' @title geom_seed
##' @param msa a multiple sequence alignment file or object
##' @param seed character, like 'GAGGUAG' a miRNA seed sequence
##' @param star a logical value indicating whether symbol of stars should be produced
##' @importFrom Biostrings vmatchPattern
##' @return a ggplot layer
##' @author Lang Zhou 
##' @examples 
##' miRNA_sequences <- system.file("extdata/seedSample.fa", package="ggmsa")
##' ggmsa(miRNA_sequences, font = 'DroidSansMono', color = "Chemistry_NT", none_bg = TRUE) + 
##' geom_seed(miRNA_sequences, seed = "GAGGUAG") 
##' ggmsa(miRNA_sequences, font = 'DroidSansMono', color = "Chemistry_NT") + 
##' geom_seed(miRNA_sequences, seed = "GAGGUAG", star = TRUE)
##' @export
geom_seed <- function(msa, seed, star = F){
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
    
    shadingData <- data.frame(x = c(x, x, xx, xx), 
                    y = c(y, yy, yy, y),
                    t = c('a', 'a', 'a','a'))
    starData <- data.frame(star_x = seq(seedPos, length.out = nchar(seed)),
                            star_y = rep(y, times = nchar(seed)))
    
    if(star) {
        ly_star <- geom_seedStar(data = starData, aes_(x = ~star_x, y = ~star_y))
        return(ly_star)
    }
    
    mapping <- aes_(x= ~x, y= ~y, group= ~t, fill = ~I('#bebebe'), alpha = .3)
    ly_seed <- geom_polygon(data = shadingData, mapping = mapping)
    return(ly_seed)
 }





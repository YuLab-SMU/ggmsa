##' Highlighting the seed in miRNA sequences
##'
##' 
##' @title geom_seed
##' @param seed character, like 'GAGGUAG' a miRNA seed sequence
##' @param star a logical value indicating whether symbol of stars should be produced
##' @return a ggplot layer
##' @author Lang Zhou 
##' @examples 
##' miRNA_sequences <- system.file("extdata/seedSample.fa", package="ggmsa")
##' ggmsa(miRNA_sequences, font = 'DroidSansMono', color = "Chemistry_NT", none_bg = TRUE) + 
##' geom_seed(seed = "GAGGUAG") 
##' ggmsa(miRNA_sequences, font = 'DroidSansMono', color = "Chemistry_NT") + 
##' geom_seed(seed = "GAGGUAG", star = TRUE)
##' @export
geom_seed <- function(seed, star = FALSE) {
    structure(list(seed = seed,
                   star = star),
              class = "seed")
}

##' @importFrom stringr str_c
##' @importFrom stringr str_locate
geom_seed1 <- function(tidyData, seed, star) {
    #prepare <- prepare_msa(msa)
    #seedResult <- vmatchPattern(pattern = seed, subject = prepare)
    tidyData$y <- as.numeric(tidyData$name)
    seq_first <- tidyData[tidyData$y == 1,]
    char <- seq_first$character
    char <- str_c(char, collapse = "")
    locate <- str_locate(char, seed)
    df_locate <- as.data.frame(locate)
    seedPos <- df_locate$start # start position of seed region
    seedLen <- nchar(seed) # length of seed region
    numSeq <- max(tidyData$y) # number of sequences
    shadingLen <- getOption("shadingLen") #shading width 
    shading_alpha <- getOption("shading_alpha")
    
    x <- seedPos - .5 #the x coordinate of the lower left corner
    y <- 1 - .5 - shadingLen #the y coordinate of the lower left corner
    yy <- numSeq + .5 + shadingLen # #the y coordinate of the top right corner 
    xx <- x + seedLen #the x coordinate of the top right corner 
    
    shadingData <- data.frame(x = c(x, x, xx, xx), 
                    y = c(y, yy, yy, y),
                    t = c('a', 'a', 'a','a'))
    starData <- data.frame(star_x = seq(seedPos, length.out = nchar(seed)),
                            star_y = rep(y, times = nchar(seed)))
    
    if(isTRUE(star)) {
        ly_star <- geom_asterisk(data = starData, aes_(x = ~star_x, y = ~star_y))
        return(ly_star)
    }
    
    mapping <- aes_(x= ~x, y= ~y, group= ~t, fill = ~I('#bebebe'))
    ly_seed <- geom_polygon(data = shadingData, mapping = mapping, alpha = shading_alpha)
    return(ly_seed)
 }


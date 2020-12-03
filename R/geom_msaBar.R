##' Multiple sequence alignment layer for ggplot2. It plot sequence conservation bar.

##' @title geom_msaBar

##' @return A list
##' @examples
##' #plot multiple sequence alignment and conservation bar.
##' f <- system.file("extdata/sample.fasta", package="ggmsa")
##' ggmsa(f, 221, 280, font = NULL, seq_name = TRUE) + geom_msaBar()
##' @export
##' @author Lang Zhou
geom_msaBar <- function() {
    structure(list(),
              class = "msaBar")
}

##' @importFrom ggplot2 geom_col
ly_bar <- function(tidy){
    data <- bar_data(tidy)
    mapping <- aes_(x = ~pos, y = ~Freq, fill = ~Freq)
    ly_bar <- geom_col(data = data, mapping = mapping, width = 1, show.legend = F)
    return(ly_bar)
}


##' get bar data

##' @title bar_data
##' @param tidy  Multiple aligned sequence file or object for representing nucleotide sequences
##' @return A data frame
##' @noRd
##' @author Lang Zhou
bar_data <- function(tidy){
    character_position <- unique(tidy$position)
    conservation_score <- lapply(character_position, function(j) {
        cloumn_data <- tidy[tidy$position == j, ]
        character_frequency <- table(cloumn_data$character) %>% as.data.frame
        #character_frequency <- data.frame(character_frequency)
        max_frequency <- character_frequency[character_frequency[2] == max(character_frequency[2]),]
        max_frequency$Var1 <- as.character(max_frequency$Var1)
        if(nrow(max_frequency) == 1) {
            max_frequency <- max_frequency[1,]
        }else {
            max_frequency <- max_frequency[1,]
        }
}) %>% do.call("rbind", .)
    conservation_score["pos"] <- character_position
    return(conservation_score)
}

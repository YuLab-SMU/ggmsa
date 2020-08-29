##' Multiple sequence alignment layer for ggplot2. It plot points of GC content.

##' @title geom_GC

##' @examples
##' library(ggplot2) 
##' #plot GC content 
##' f <- system.file("extdata/LeaderRepeat_All.fa", package="ggmsa")
##' ggmsa(f, font = NULL, color="Chemistry_NT") + geom_GC()
##' @export
##' @author Lang Zhou
geom_GC <- function() {
    structure(list(),
              class = "GCcontent")
}


geom_GC1 <- function(tidyData){
    tidy <- tidyData
    #tidy <- tidy_msa(msa = msa, start = start, end = end)
    GC_pos <- getOption("GC_pos")
  
    GC <- content_GC(tidy)
    GC <-GC[GC$character == "GC",]
    col_num <- levels(factor(tidy$position))
    col_len <- length(col_num) + GC_pos
    ly_GC <- geom_point(data = GC, aes_(x = ~col_len, y = ~ypos, size = ~fre, color = ~fre), na.rm = T)
    return(ly_GC)
}
##' get GC content

##' @title content_GC
##' @param data  Multiple aligned sequence file or object for representing nucleotide sequences
##' @return A data frame
##' @noRd
##' @author Lang Zhou
content_GC<- function(data){
    tidy <- data
    tidy$name <- factor(tidy$name, levels = unique(tidy$name))
    tidy$ypos <- as.numeric(tidy$name)
    seq_num <- levels(factor(tidy$ypos)) # the sequence number
    lchar_num <- lapply(1:length(seq_num),function(j){
        clo <- tidy[tidy$ypos == j, ] 
        y <- prop.table(table(clo$character))
        y["GC"] <- y["G"] + y["C"]
        num <-setNames(rep(0,5), c("A", "T", "G", "C", "GC"))
        num[names(y)] <- y
        return(num)
    })
  
    char_num <- do.call(rbind,lchar_num)
    char_num <- as.data.frame(char_num)
    char_num["ypos"] =  as.numeric(seq_num) 
    char_num2 <- gather(char_num,character,fre, "A", "T", "C","G","GC")
    return(char_num2)
}

utils::globalVariables('fre')


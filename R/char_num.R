
##' get nucleotide number

##' @title nucleotide number
##' @param data  Multiple aligned sequence file or object for representing nucleotide sequences
##' @return A data frame
##' @noRd
##' @author Lang Zhou
nt_num <- function(data){
    tidy <- data
    col_num <- levels(factor(tidy$position)) # the column number
    lchar_num <- lapply(1:length(col_num),function(j){
        clo <- tidy[tidy$position == j, ] 
        y <- prop.table(table(clo$character))
        y["GC"] <- y["G"] + y["C"]
        num <-setNames(rep(0,5), c("A", "T", "G", "C", "GC"))
        num[names(y)] <- y
        return(num)
    })
    
    char_num <- do.call(rbind,lchar_num)
    char_num <- as.data.frame(char_num)
    char_num["position"] =  as.numeric(col_num) 
    char_num2 <- gather(char_num, character, fre, "A", "T", "C","G","GC")
    return(char_num2)
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
    char_num2 <- gather(char_num, character, fre, "A", "T", "C","G","GC")
    return(char_num2)
}

##' Multiple sequence alignment layer for ggplot2. It plot points of GC content.

##' @title geom_GC
##' @param msa  multiple sequence alignment file or
##' sequence object in DNAStringSet, RNAStringSet, AAStringSet, BStringSet,
##' DNAMultipleAlignment, RNAMultipleAlignment, AAMultipleAlignment, DNAbin or AAbin
##' @param start start position to extract subset of alignment
##' @param end end position to extract subset of alignemnt
##' @examples 
##' #plot GC content 
##' f <- system.file("extdata/LeaderRepeat_All.fa", package="ggmsa")
##' ggmsa(f,font = NULL,color="Chemistry_NT") + geom_GC(f)
##' @export
##' @author Lang Zhou
geom_GC <- function(msa, start=NULL, end=NULL){
    tidy <- tidy_msa(msa = msa, start = start, end = end)
    GC <- content_GC(tidy)
    GC <-GC[GC$character == "GC",]
    col_num <- levels(factor(tidy$position))
    col_len <- length(col_num) + 2
    ly_GC <- geom_point(data = GC,aes(x = col_len, ypos,size = GC$fre, color = GC$fre))
    return(ly_GC)
}








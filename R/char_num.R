
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
        num <-setNames(rep(0,4), c("A", "T", "G", "C"))
        num[names(y)] <- y
        return(num)
    })
    
    char_num <- do.call(rbind,lchar_num)
    char_num <- as.data.frame(char_num)
    char_num["position"] =  as.numeric(col_num) 
    char_num2 <- gather(char_num, character, fre, "A", "T", "C","G")
    return(char_num2)
}



##' get nucleotide number

##' @title nucleotide number
##' @param data  Multiple aligned sequence file or object for representing nucleotide sequences
##' @return A data frame
##' @noRd
##' @author Lang Zhou
nt_num <- function(data){
    tidy <- data
    col_num <- levels(factor(tidy$position)) # the column number
    lchar_num <- sapply(1:length(col_num),simplify = F,function(j){
        clo <- tidy[tidy$position == j, ] 
        num <- table(clo$character)
        if  (!"A" %in% names(num)){num["A"] <- 0}
        if  (!"C" %in% names(num)){num["C"] <- 0}
        if  (!"G" %in% names(num)){num["G"] <- 0}
        if  (!"T" %in% names(num)){num["T"] <- 0}
        return(num)
    })
    
    char_num <- do.call(dplyr::bind_rows,lchar_num)
    char_num <- as.data.frame(char_num)
    char_num["position"] =  as.numeric(col_num) 
    char_num2 <- gather(char_num, character, count, "A", "T", "C","G")
    return(char_num2)
}



##' calculate difference of two aligned sequences
##'
##'
##' @title seqdiff
##' @param fasta fasta file
##' @param reference which sequence serve as reference, 1 or 2
##' @return SeqDiff object
##' @export
##' @importFrom Biostrings readBStringSet
##' @importClassesFrom Biostrings BStringSet
##' @importFrom methods new
##' @author guangchuang yu
##' @examples
##' fas <- list.files(system.file("extdata", "GVariation", package="ggmsa"),
##'                   pattern="fas", full.names=TRUE)
##' seqdiff(fas[1], reference=1)
seqdiff <- function(fasta, reference=1) {
    sequence <- readBStringSet(fasta)
    if (length(sequence) != 2 && length(width(sequence)) != 1) {
        stop("fas should contains 2 aligned sequences...")
    }
    diff <- nucleotide_difference(sequence, reference)
    new("SeqDiff",
        file = fasta,
        sequence = sequence,
        reference = reference,
        diff = diff)
}

##' @importFrom magrittr %>%
##' @importFrom Biostrings toString
##' @importFrom Biostrings width
nucleotide_difference <- function(x, reference=1) {
    n <- width(x[1])
    nn <- seq_len(n)
    s1 <- x[1] %>% toString %>% substring(nn, nn)
    s2 <- x[2] %>% toString %>% substring(nn, nn)

    pos <- which(s1 != s2)
    if (reference == 1) {
        diff <- s2[pos]
    } else {
        diff <- s1[pos]
    }

    return(data.frame(position = pos,
                      difference = diff,
                      stringsAsFactors = FALSE))
}




##' @importFrom dplyr group_by
##' @importFrom dplyr summarize
##' @importFrom dplyr select
##' @importFrom dplyr n
nucleotide_difference_count <- function(x, width=50, keep0=FALSE) {
    n <- max(x$position)
    bin <- rep(1:ceiling(n/width), each=width)
    position <- c(seq_len(n)[!duplicated(bin)], n)
    x$bin <- bin[x$pos]
    y <- x %>% group_by(bin) %>%
        summarize(position=min(position), count = n()) %>%
        select(-bin)
    y$position <- position[findInterval(y$position, position)]
    if (keep0) {
        itv <- seq(1, n, width)
        yy <- data.frame(position = itv[!itv %in% y$position],
                         count = 0)
        y <- rbind(y, yy)
        y <- y[order(y$position, decreasing=FALSE),]
    }
    return(y)
}


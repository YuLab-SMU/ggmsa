##' Sequence similarity plot
##'
##'
##' @title simplot
##' @param file alignment fast file
##' @param query query sequence
##' @param window sliding window size (bp)
##' @param step step size to slide the window (bp)
##' @param group whether grouping sequence
##' @param id position to extract id for grouping; only works if group = TRUE
##' @param sep separator to split sequence name; only works if group = TRUE
##' @param sd whether display standard deviation of similarity among each group; only works if group=TRUE
##' @return ggplot object
##' @importFrom Biostrings readDNAStringSet
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_line
##' @importFrom ggplot2 ggtitle
##' @importFrom ggplot2 geom_ribbon
##' @importFrom magrittr %<>%
##' @importFrom dplyr group_by_
##' @importFrom dplyr summarize_
##' @export
##' @author guangchuang yu
##' @examples
##' fas <- system.file("extdata/GVariation/sample_alignment.fa", package="ggmsa")
##' simplot(fas, 'CF_YL21')
simplot <- function(file, query, window=200, step=20, group=FALSE, id, sep, sd=FALSE) {
    aln <- readDNAStringSet(file)
    nn <- names(aln)
    if (group) {
        g <- vapply(strsplit(nn, sep), function(x) x[id], character(1))
    }

    idx <- which(nn != query)
    w <- width(aln[query])
    start <- seq(1, w, by=step)
    end <- start + window - 1
    start <- start[end <= w]
    end <- end[end <= w]
    res <- lapply(idx, function(i) {
        x <- toCharacter(aln[i]) == toCharacter(aln[query])
        ## pos <- seq_along(x)
        ## data.frame(sequence=nn[i], position=pos, similarity=cumsum(x)/pos * 100)
        pos <- round((start+end)/2)
        sim <- vapply(seq_along(start), function(j) {
            mean(x[start[j]:end[j]])
        }, numeric(1))
        ## sim <- c(cummean(x[1:(pos[1]-1)]), sim, cummean(x[(pos[length(pos)]+1):w]))
        ## pos <- c(1:(pos[1]-1), pos, (pos[length(pos)]+1):w)

        y <- data.frame(sequence=nn[i], position = pos, similarity = sim)
        if(group) {
            y$group <- g[i]
        }
        return(y)
    }) %>% do.call(rbind, .)

    if (group) {
        res %<>% group_by_(~position, ~group) %>%
            summarize_(msim=~mean(similarity), sd=~sd(similarity))
    }


    if (group) {
        p <- ggplot(res, aes_(x=~position, y=~msim, group=~group))
        if (sd) p <- p + geom_ribbon(aes_(ymin=~msim-sd, ymax=~msim+sd, fill=~group), alpha=.25) #fill='grey70')
        p <- p + geom_line(aes_(color=~group))
    } else {
        p <- ggplot(res, aes_(x=~position, y=~similarity)) +
            geom_line(aes_(group=~sequence, color=~sequence))
    }

    p + xlab("Nucleotide Position") + ylab("Similarity (%)") +
        ggtitle(paste("Sequence similarities compare to", query)) +
        theme_minimal() +
        theme(legend.title=element_blank()) #, legend.position=c(.95, .15))
}


toCharacter <- function(x) {
	unlist(strsplit(toString(x),""))
}



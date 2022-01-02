##' Sequence similarity plot
##'
##'
##' @title simplot
##' @param file alignment fast file
##' @param query query sequence
##' @param window sliding window size (bp)
##' @param step step size to slide the window (bp)
##' @param group whether grouping sequence.(eg. For "A-seq1,A-seq-2,B-seq1 and 
##' B-seq2", using sep = "-" and id = 1 to divide sequences into groups A and 
##' B)
##' @param id position to extract id for grouping; only works if group = TRUE
##' @param sep separator to split sequence name; only works if group = TRUE
##' @param sd whether display standard deviation of 
##' similarity among each group; only works if group=TRUE
##' @param smooth FALSE(default)or TRUE; whether display smoothed spline.
##' @param smooth_params a list that add params for geom_smooth,
##' (default: smooth_params = list(method = "loess", se = FALSE))
##' @return ggplot object
##' @importFrom Biostrings readDNAStringSet
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_line
##' @importFrom ggplot2 ggtitle
##' @importFrom ggplot2 geom_ribbon
##' @importFrom ggplot2 geom_smooth
##' @importFrom magrittr %<>%
##' @importFrom dplyr group_by_
##' @importFrom dplyr summarize_
##' @export
##' @author guangchuang yu
##' @examples
##' fas <- system.file("extdata/GVariation/sample_alignment.fa", 
##'                     package="ggmsa")
##' simplot(fas, 'CF_YL21')
simplot <- function(file, 
                    query, 
                    window=200, 
                    step=20, 
                    group=FALSE, 
                    id, 
                    sep, 
                    sd=FALSE,
                    smooth = FALSE,
                    smooth_params = list(method = "loess", 
                                         se = FALSE)) {
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
        pos <- round((start+end)/2)
        sim <- vapply(seq_along(start), function(j) {
            mean(x[start[j]:end[j]])
        }, numeric(1))

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
        if (sd) p <- p + geom_ribbon(aes_(ymin=~msim-sd, 
                                          ymax=~msim+sd, 
                                          fill=~group), alpha=.25)
        if (smooth) {
            smooth_layer <- do.call(geom_smooth, 
                                    smooth_params)
            p <- p + smooth_layer
        } else {
            p <- p + geom_line(aes_(color=~group))
        }
        
        
    } else {
        mapping = aes_(x=~position, 
                       y=~similarity,
                       group=~sequence, 
                       color=~sequence)
        p <- ggplot(res, mapping = mapping) 
        
        if (smooth) {
            smooth_layer <- do.call(geom_smooth, 
                                    smooth_params)
            p <- p + smooth_layer
            
        } else {
            p <- p + geom_line()
        }
    }

    p + xlab("Nucleotide Position") + ylab("Similarity (%)") +
        ggtitle(paste("Sequence similarities compare to", query)) +
        theme_minimal() +
        theme(legend.title=element_blank()) 
}


toCharacter <- function(x) {
    unlist(strsplit(toString(x),""))
}



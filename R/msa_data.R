##' This function parses FASTA file and convert it to a tidy data frame. 
##' The function will also assign color to each molecule (amino acid or nucleotide) according to the selected color scheme. Sequence logo data for drawing alignment label will also be added if font != NULL. The output of msa_data() is the input of geom_msa().
##'
##' @title msa_data
##' @param fasta Aligned fasta file.
##' @param font Character font, Defaults is 'helvetica_regular'.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'.Defaults is 'Clustal'.
##' @return A data frame
##' @examples
##' fasta <- system.file("extdata/sample.fasta", package="ggmsa")
##' data <- msa_data(fasta, 20, 120, font = 'helvetica_regular', color = 'Chemistry_AA' )
## @export
##' @noRd
##' @author Guangchuang Yu
msa_data <- function(tidymsa, font = "helvetica_regular", color = "Clustal") {
    color <- match.arg(color, c("Clustal","Chemistry_AA","Shapely_AA","Zappo_AA","Taylor_AA",
                                "Chemistry_NT","Shapely_NT","Zappo_NT","Taylor_NT" ))

    y <- tidymsa

    if (color == "Clustal"){
        y <- color_Clustal(y)
    } else {
        y <- color_scheme(y, color)
    }

    if (is.null(font)) {
        return(y)
    }

    data_sp <- get_logo_data(unique(y$character), font)

    if (!'name' %in% names(y)) {
        if ('label' %in% names(y)) {
            ## y <- dplyr::rename(y, name = label)
            names(y)[names(y) == 'label'] <- "name"
        } else {
            stop("unknown sequence name...")
        }
    }

    y$name <- factor(y$name, levels = unique(y$name))
    y$ypos <- as.numeric(y$name)

    yy <- lapply(1:nrow(y), function(i) {
        d <- y[i, ]
        dd <- data_sp[[d$character]]
        dd$x <- dd$x - min(dd$x) + d$position -.45
        if (d$character == '-') {
            dd$y <- dd$y - min(dd$y) + d$ypos - 0.1
        } else {
            dd$y <- dd$y - min(dd$y) + d$ypos -.45
        }
        cn <- colnames(d)
        cn <- cn[!cn %in% c('x','y', 'ypos')]
        for (nn in cn) {
            dd[[nn]] <- d[[nn]]
        }
        ## dd$name <- d$name
        ## dd$position <- d$position
        dd$group <- paste0("V", d$position, "L", d$ypos)
        #dd$group <- paste0(d$position, d$ypos)
        ## dd$character <- d$character
        ## dd$color <- d$color
        dd <- dd[order(dd$order),]
        return(dd)
    })

    ydf <- do.call(rbind, yy)
    colnames(ydf)[colnames(ydf) == 'y'] <- 'yy'
    ydf$y <- as.numeric(ydf$name)

    ydf <- cbind(label = ydf$name, ydf)
    return(ydf)
}

##' Convert msa file/object to tidy data frame
##'
##'
##' @title tidy_msa
##' @param msa multiple sequence alignment file or
##' sequence object in DNAStringSet, RNAStringSet, AAStringSet, BStringSet,
##' DNAMultipleAlignment, RNAMultipleAlignment, AAMultipleAlignment, DNAbin or AAbin
##' @param start start position to extract subset of alignment
##' @param end end position to extract subset of alignemnt
##' @return tibble data frame
##' @export
##' @author Guangchuang Yu
tidy_msa <- function(msa, start = NULL, end = NULL) {
    aln <- prepare_msa(msa)
    alnmat <- lapply(seq_along(aln), function(i) {
        base::strsplit(as.character(aln[[i]]), '')[[1]]
    }) %>% do.call('rbind', .)
    ## for DNAbin and AAbin
    ## alnmat <- lapply(seq_along(aln), function(i) as.character(aln[[i]])) %>% do.call('rbind',. )
    alndf <- as.data.frame(alnmat)
    
    alndf$name = names(aln)
    
    cn = colnames(alndf)
    cn <- cn[!cn %in% "name"]
    df <- gather(alndf, "position", "character", cn)
    
    y <- df
    y$position = as.numeric(sub("V", "", y$position))
    y$character = toupper(y$character)
     
    y$name = factor(y$name, levels=rev(names(aln)))

    
    if (is.null(start)) start <- min(y$position)
    if (is.null(end)) end <- max(y$position)
    
    y <- y[y$position >=start & y$position <= end, ]

    return(y)
}

get_logo_data <- function(chars, font) {
    ## ggseqlogo::list_fonts()
    data_sp = lapply(chars, function(n){
        if (n == '-') {
            d <- data.frame(x = c(0.05, 0.95, 0.95, 0.05),
                            y = c(0.05, 0.05, 0.2, 0.2),
                            letter = '-',
                            position = 1,
                            order = 1:4,
                            seq_group='-')
            return(d)
        }
        d = logo_data(seqs = n,
                      font = font,
                      seq_group = n,
                      seq_type = "auto")
        
        d$x <- d$x * .9/diff(range(d$x))
        d$y <- d$y * .9/diff(range(d$y))
        return(d)
    })
    names(data_sp) = chars
    return(data_sp)
}



##' @import ggseqlogo
logo_data <- getFromNamespace("logo_data", "ggseqlogo")


##' @importFrom utils globalVariables
utils::globalVariables('.')


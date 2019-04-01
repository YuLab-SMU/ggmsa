##' plot multiple sequence alignment using ggplot2
##'
##'
##' @title ggmsa
##' @param fasta aligned fasta file
##' @param start start position to plot
##' @param end end position to plot
##' @param font character font
##' @return ggplot object
##' @importFrom tidyr gather
##' @importFrom treeio read.fasta
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 geom_polygon
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @export
##' @author guangchuang yu
ggmsa <- function(fasta, start=NULL, end=NULL, font = "helvetica_regular") {
    aln <- read.fasta(fasta)
    alnmat <- lapply(seq_along(aln), function(i) as.character(aln[[i]])) %>% do.call('rbind',. )
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



    chars <- unique(y$character)

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
        dd$name = d$name
        dd$position = d$position
        dd$group <- paste0(d$position, d$ypos)
        dd$character = d$character
        dd = dd[order(dd$order),]
        return(dd)
    })

    ydf <- do.call(rbind, yy)


    ggplot(ydf, aes_(x=~position, y=~name, fill = ~character)) + geom_tile(color='grey') +
        geom_polygon(data=ydf, aes_(x=~x, y=~y, group=~factor(group)), fill='black') +
        theme_minimal() + xlab(NULL) + ylab(NULL) +
        theme(legend.position='none')
}

##' @import ggseqlogo
logo_data <- getFromNamespace("logo_data", "ggseqlogo")

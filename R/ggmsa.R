##' plot multiple sequence alignment using ggplot2
##'
##'
##' @title ggmsa
##' @param fasta aligned fasta file
##' @param start start position to plot
##' @param end end position to plot
##' @param font character font
##' @param color 5 amino acid color schemes, 4 nucleic acid color schemes. Note: Culstal is an amino acid color scheme
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
##' @importFrom ggplot2 coord_fixed
##' @importFrom magrittr %>%
##' @export
##' @author guangchuang yu
ggmsa <- function(fasta, start=NULL, end=NULL, font = "helvetica_regular", color = "Clustal") {
    data <- msa_data(fasta, start = start, end = end,
                     color = color, font = font)

    ggplot() + geom_msa(data) + 
        theme_minimal() + xlab(NULL) + ylab(NULL) +
        theme(legend.position='none') + coord_fixed()
}

##' Plot multiple sequence alignment using ggplot2 with multiple color schemes supported. 
##'
##'
##' @title ggmsa
##' @param fasta Aligned FASTA format file for representing either nucleotide sequences or peptide sequences.
##' @param start Start position to plot, If font=NULL, only the background frame is drawn, and no character.
##' @param end End position to plot,If font=NULL, only the background frame is drawn, and no character.
##' @param font Character font, Defaults is 'helvetica_regular'.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Clustal'.
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
##' @examples 
##' #plot multiple sequence alignment
##' f <- system.file("extdata/sample.fasta", package="ggmsa")
##' ggmsa(f, 164, 213, color="Chemistry_AA")
##' @export
##' @author guangchuang yu
ggmsa <- function(fasta, start=NULL, end=NULL, font = "helvetica_regular", color = "Clustal") {
    data <- msa_data(fasta, start = start, end = end,
                     color = color, font = font)

    ggplot() + geom_msa(data) + 
        theme_minimal() + xlab(NULL) + ylab(NULL) +
        theme(legend.position='none') + coord_fixed()
}

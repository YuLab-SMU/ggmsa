##' Plot multiple sequence alignment using ggplot2 with multiple color schemes supported. 
##'
##'
##' @title ggmsa
##' @param msa Multiple aligned sequence file or object for
##' representing either nucleotide sequences or peptide sequences.
##' @param start Start position to plot, If font=NULL, only the background frame is drawn, and no character.
##' @param end End position to plot,If font=NULL, only the background frame is drawn, and no character.
##' @param font font families, possible values are 'helvetical', 'times', and 'mono'. Defaults is 'helvetical'. If you specify font = NULL, only the background box will be printed.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Clustal'.
##' @param char_width characters width. Defaults is 0.9.
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
##' @importFrom ggplot2 geom_point
##' @importFrom magrittr %>%
##' @importFrom stats setNames
##' @examples 
##' #plot multiple sequence alignment
##' f <- system.file("extdata/sample.fasta", package="ggmsa")
##' ggmsa(f, 164, 213, color="Chemistry_AA")
##' @export
##' @author guangchuang yu
ggmsa <- function(msa, start=NULL, end=NULL, font = "helvetical", color = "Clustal", char_width = 0.9) {
    data <- tidy_msa(msa, start = start, end = end)

    ggplot() + geom_msa(data, font = font, color = color, char_width = char_width) + 
        theme_minimal() + xlab(NULL) + ylab(NULL) + theme(legend.position='none') + coord_fixed()
}

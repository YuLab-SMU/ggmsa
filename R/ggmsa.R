##' Plot multiple sequence alignment using ggplot2 with multiple color schemes supported. 
##'
##'
##' @title ggmsa
##' @param msa Multiple aligned sequence file or object for
##' representing either nucleotide sequences or peptide sequences.
##' @param start a numeric.Start position to plot.
##' @param end a numeric. End position to plot.
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'.  Defaults is 'helvetical'. If you specify font = NULL, only the background box will be printed.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Clustal'.
##' @param char_width characters width. Defaults is 0.9.
##' @param none_bg a logical value indicating whether backgroud should be produced. Defaults is FALSE.
##' @param posHighligthed A numeric vector of the position that need to be highlighted.
##' @return ggplot object
##' @importFrom tidyr gather
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
##' @importFrom ggplot2 element_blank
##' @importFrom magrittr %>%
##' @importFrom stats setNames
##' @examples 
##' #plot multiple sequences by loading fasta format 
##' fasta <- system.file("extdata", "sample.fasta", package = "ggmsa")
##' ggmsa(fasta, 164, 213, color="Chemistry_AA")
##' 
##' #XMultipleAlignment objects can be used as input in the 'ggmsa'
##' AAMultipleAlignment <- Biostrings::readAAMultipleAlignment(fasta)
##' ggmsa(AAMultipleAlignment, 164, 213, color="Chemistry_AA")
##' 
##' #XStringSet objects can be used as input in the 'ggmsa'
##' AAStringSet <- Biostrings::readAAStringSet(fasta)
##' ggmsa(AAStringSet, 164, 213, color="Chemistry_AA")
##' 
##' #Xbin objects from 'seqmagick' can be used as input in the 'ggmsa'
##' AAbin <- seqmagick::fa_read(fasta)
##' ggmsa(AAbin, 164, 213, color="Chemistry_AA")
##' @export
##' @author Guangchuang Yu
ggmsa <- function(msa, start=NULL, end=NULL, font = "helvetical", color = "Clustal", char_width = 0.9, none_bg = FALSE, posHighligthed = NULL) {
    data <- tidy_msa(msa, start = start, end = end)

    ggplot() + geom_msa(data, font = font, color = color, char_width = char_width, none_bg = none_bg, posHighligthed = posHighligthed) + 
        theme_minimal() + xlab(NULL) + ylab(NULL) + theme(legend.position='none') + coord_fixed()
}

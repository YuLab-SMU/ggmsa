##' Plot multiple sequence alignment using ggplot2 with multiple color schemes supported.
##'
##'
##' @title ggmsa
##' @param msa Multiple aligned sequence files or objects representing either nucleotide sequences or AA sequences.
##' @param start a numeric vector. Start position to plot.
##' @param end a numeric vector. End position to plot.
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'.  Defaults is 'helvetical'. If font = NULL, only plot the background tile.
##' @param color a Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'LETTER', 'CN6', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Chemistry_AA'.
##' @param custom_color A data frame with two cloumn called "names" and "color".Customize the color scheme.
##' @param order vectors.Specified sequences order.
##' @param char_width a numeric vector. Specifying the character width in the range of 0 to 1. Defaults is 0.9.
##' @param by_conservation a logical value. The most conserved regions have the brightest colors.
##' @param none_bg a logical value indicating whether background should be disaplayed. Defaults is FALSE.
##' @param posHighligthed A numeric vector of the position that need to be highlighted.
##' @param seq_name a logical value indicating whether seqence names should be displayed. Defaults is 'NULL' which indicates that the sequence name is displayed when 'font = null', but 'font = char' will not be displayed. If 'seq_name = TRUE' the sequence name will be displayed in any case. If 'seq_name = FALSE' the sequence name will not be displayed under any circumstances.
##' @param border a character string. The border color.
##' @param consensus_views a logical value that opeaning consensus views.
##' @param use_dot a logical value. Displays characters as dots instead of fading their color in the consensus view.
##' @param disagreement a logical value. Displays characters that disagreememt to consensus(excludes ambiguous disagreements).
##' @param ignore_gaps a logical value. When selected TRUE, gaps in column are treated as if that row didn't exist.
##' @param ref a character string. Specifying the reference sequence which should be one of input sequences when 'consensus_views' is TRUE.
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
##' @importFrom grid unit
##' @examples
##' #plot multiple sequences by loading fasta format
##' fasta <- system.file("extdata", "sample.fasta", package = "ggmsa")
##' ggmsa(fasta, 164, 213, color="Chemistry_AA")
##'
##' #XMultipleAlignment objects can be used as input in the 'ggmsa'
##' #AAMultipleAlignment <- Biostrings::readAAMultipleAlignment(fasta)
##' #ggmsa(AAMultipleAlignment, 164, 213, color="Chemistry_AA")
##'
##' #XStringSet objects can be used as input in the 'ggmsa'
##' #AAStringSet <- Biostrings::readAAStringSet(fasta)
##' #ggmsa(AAStringSet, 164, 213, color="Chemistry_AA")
##'
##' #Xbin objects from 'seqmagick' can be used as input in the 'ggmsa'
##' #AAbin <- seqmagick::fa_read(fasta)
##' #ggmsa(AAbin, 164, 213, color="Chemistry_AA")
##' @export
##' @author Guangchuang Yu
ggmsa <- function(msa, start = NULL, end = NULL, font = "helvetical",
                  color = "Chemistry_AA", custom_color = NULL, order = NULL, char_width = 0.9, none_bg = FALSE, by_conservation = FALSE,
                  posHighligthed = NULL, seq_name = NULL, border = NULL, consensus_views = FALSE, use_dot = FALSE,
                  disagreement = TRUE, ignore_gaps = FALSE, ref = NULL) {
    data <- tidy_msa(msa, start = start, end = end)

    ggplot() + geom_msa(data, font = font, color = color, custom_color = custom_color, order = order, char_width = char_width, none_bg = none_bg,
                        by_conservation = by_conservation, posHighligthed = posHighligthed, seq_name = seq_name, border = border,
                        consensus_views = consensus_views,use_dot = use_dot, disagreement = disagreement, ignore_gaps = ignore_gaps, ref = ref) +
               theme_msa()

}







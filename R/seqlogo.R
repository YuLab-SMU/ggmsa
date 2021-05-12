##' plot sequence logo for MSA based 'ggolot2'

##' @title seqlogo
##' @param msa Multiple sequence alignment file or object for representing either nucleotide sequences or peptide sequences.
##' @param start Start position to plot.
##' @param end End position to plot.
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'.  Defaults is 'DroidSansMono'. If font=NULL, only the background tiles is drawn.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'LETTER', 'CN6','Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Chemistry_AA'.
##' @param custom_color A data frame with two cloumn called "names" and "color".Customize the color scheme.
##' @param adaptive A logical value indicating whether the overall height of seqlogo corresponds to the number of sequences. If FALSE, seqlogo overall height = 4,fixedly.
##' @param top  A logical value. If TRUE, seqlogo is aligned to the top of MSA.
##' @return ggplot object
##' @examples
##' #plot sequence motif independently
##' nt_sequence <- system.file("extdata", "LeaderRepeat_All.fa", package = "ggmsa")
##' seqlogo(nt_sequence, color = "Chemistry_NT")
##' @export
##' @author Lang Zhou
seqlogo <- function(msa, start = NULL, end = NULL, font = "DroidSansMono", color = "Chemistry_AA", adaptive = FALSE, top = FALSE, custom_color = NULL) {
    data <- tidy_msa(msa, start = start, end = end)
    ggplot() + geom_logo(data, font = font, color = color, adaptive = adaptive, top = top, custom_color = custom_color) +
        theme_minimal() + xlab(NULL) + ylab(NULL) +
        theme(legend.position = 'none') + theme(panel.grid = element_blank(), axis.text.y = element_blank()) +
        coord_fixed()
}

##' Multiple sequence alignment layer for ggplot2. It plot sequence motifs.

##' @title geom_seqlogo
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'. Defaults is 'DroidSansMono'.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'LETTER', 'CN6', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Chemistry_AA'.
##' @param custom_color A data frame with two cloumn called "names" and "color".Customize the color scheme.
##' @param adaptive A logical value indicating whether the overall height of seqlogo corresponds to the number of sequences.If is FALSE, seqlogo overall height = 4,fixedly.
##' @param top A logical value. If TRUE, seqlogo is aligned to the top of MSA.
##' @param show.legend logical. Should this layer be included in the legends?
##' @param ... additional parameter
##' @return A list
##' @examples
##' #plot multiple sequence alignment and sequence motifs
##' f <- system.file("extdata/LeaderRepeat_All.fa", package="ggmsa")
##' ggmsa(f,font = NULL,color = "Chemistry_NT") + geom_seqlogo()
##' @export
##' @author Lang Zhou
geom_seqlogo <- function(font = "DroidSansMono", color = "Chemistry_AA", adaptive = TRUE, top = TRUE, custom_color = NULL, show.legend = FALSE, ...) {
    structure(list(font = font,
                   color = color,
                   adaptive = adaptive,
                   top = top,
                   custom_color = custom_color,
                   show.legend = show.legend),
              class = "seqlogo")
}


geom_logo <- function(data, font = "DroidSansMono", color = "Chemistry_AA", adaptive = FALSE, top = TRUE, custom_color = NULL, show.legend = FALSE, ...) {
    mapping  <- aes_(x = ~logo_x, y = ~logo_y,  group = ~group, fill = ~I(color))
    logo_data <- seqlogo_data(data, font = font, color = color, adaptive = adaptive, top = top, custom_color = custom_color)

    ly_logo <- geom_polygon(mapping = mapping, data = logo_data, inherit.aes = FALSE, show.legend = show.legend)
    return(ly_logo)
}

seqlogo_data <- function(data, font = "DroidSansMono", color = "Chemistry_AA", adaptive = FALSE, top = TRUE, custom_color = NULL){
    tidy <- data

    if (color == "Clustal") {
        tidy <- color_Clustal(tidy)
    } else{
        tidy <- color_scheme(tidy, color, custom_color)
    }

    if (adaptive) {
        seq_number  <-  as.character(unique(tidy[[1]]))
        total_heigh <- length(seq_number) / 6
    } else {
        total_heigh <- 4
    }

    #total_heigh <- getOption("total_heigh")
    logo_width <- getOption("logo_width")

    col_num <- as.numeric(levels(factor(tidy$position))) # the MSA column numbers
    moti_da <- lapply(col_num, function(j){
        clo <- tidy[tidy$position == j, ] ## Calculate the char frequency in each column
        fre <- prop.table(table(clo$character))
        ywidth <- sort(total_heigh * fre ) ## total_heigh is overall hight, the height of each char is assigned.
        column_char_color <- unique(clo[c("character", "color")]) ## calling color scheme
        font_f <- font_fam[[font]]
        motif_char <- font_f[names(ywidth)]
        ds_ <- lapply(seq_along(motif_char), function(i){
            ds_ <- motif_char[[i]]
            names(ds_)[names(ds_) == "x"] <- "logo_x"
            names(ds_)[names(ds_) == "y"] <- "logo_y"
            ds_$char <- names(motif_char[i])
            ds_$logo_x <- ds_$logo_x * logo_width/diff(range(ds_$logo_x)) #width = .9
            ds_$logo_y <- ds_$logo_y * ywidth[[i]]/diff(range(ds_$logo_y)) #hight = overall hight * frequency
            ymotif <- sum(ywidth[0:(i - 1)]) # sum-hight currently
            ds_$logo_x <- ds_$logo_x - min(ds_$logo_x) - logo_width/2 + j #  moving char horizontally
            ds_$logo_y <- ds_$logo_y - min(ds_$logo_y) - ywidth[[i]]/2 + ymotif + ywidth[[i]]/2
            if (top) {
              ds_$logo_y <- ds_$logo_y + nrow(tidy[tidy$position == j, ]) + .5
            }
            ## ds_$y - min(ds_$y) - ywidth[[i]]/2, Centered at zero
            ## + ymotif, sum-hight that are below the char currently
            ## + ywidth[[i]]/2, the char height currently
            ds_$group <- paste0("P", j, '-', "Char", names(motif_char[i]))
            ds_$color <- column_char_color[column_char_color$character == unique(ds_$char), "color"]
            return(ds_)
         })
        ds <- do.call(rbind, ds_)
        return(ds)
  })
    moti_da <- do.call(rbind, moti_da)
    moti_da$name <- as.character(tidy[1,1])
    other_cn <- names(moti_da)[!names(moti_da) == 'name']
    moti_da <- moti_da[c("name", other_cn)]
    add_col <- tidy[,!names(tidy) %in% names(moti_da)]
    moti_da <- cbind(add_col[1,], moti_da, row.names = NULL)
    return(moti_da)
}



















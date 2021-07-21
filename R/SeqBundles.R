##'  plot Sequence Bundles for MSA based 'ggolot2'
##'
##'
##' @title ggSeqBundle
##' @importFrom ggalt geom_xspline
##' @param msa Multiple sequence alignment file(FASTA) or object for representing either nucleotide sequences or peptide sequences.Also receives multiple MSA files.eg:msa = c("Gram-negative_AKL.fasta", "Gram-positive_AKL.fasta").
##' @param line_widch The width of bundles at each site, default is 0.3.
##' @param line_thickness The thickness of bundles at each site, default is 0.3.
##' @param line_high The high of bundles at each site, default is 0.
##' @param spline_shape A numeric vector of values between -1 and 1, which control the shape of the spline relative to the control points.From ggalt::geom_xspline().
##' @param size A numeric vector of values between o and 1, which control the size of each lines.
##' @param alpha A numeric vector of values between o and 1, which control the alpha of each lines.
##' @param bundle_color The colors of each sequence bundles.eg: bundle_color = c("#2ba0f5","#424242").
##' @param lev_molecule Reassigning the Y-axis and displaying letter-coded amino acids/nucleotides arranged by physiochemical properties or others.eg:amino acids hydrophobicity lev_molecule = c("-","A", "V", "L", "I", "P", "F", "W", "M", "G", "S","T", "C", "Y", "N", "Q", "D", "E", "K","R", "H").
##' @return ggplot object
##' @export
##' @author Lang Zhou
ggSeqBundle <- function(msa,
                        line_widch = 0.3,
                        line_thickness = 0.3,
                        line_high = 0,
                        spline_shape = 0.3,
                        size = 0.5,
                        alpha = 0.2,
                        bundle_color = c("#2ba0f5","#424242"),
                        lev_molecule = c("-","A", "V", "L", "I", "P", "F", "W", "M", "G", "S",
                                         "T", "C", "Y", "N", "Q", "D", "E", "K","R", "H")
                        ) {
    if(length(msa) > length(bundle_color)) {
      stop("Each MSA group should be assigned a bundle color!!")
    }

    df <- lapply(seq_along(msa), function(i){
        df_aa <- tidy_msa(msa[[i]])
        df_aa$name <- as.character(df_aa$name)
        df_aa$group <- i
        df_aa
    })%>% do.call("rbind",.)

    dd <- adjustMSA(df_msa = df,
                    lev_molecule = lev_molecule,
                    line_widch = line_widch,
                    line_thickness = line_thickness,
                    line_high = line_high,
                    bundle_color = bundle_color
                    )

    mapping <- aes(x = position_adj, y = y_adj, fill = name, color = I(bundle_color))
    ggplot(data = dd, mapping = mapping) +
        geom_xspline(spline_shape = spline_shape, size = size, alpha = alpha)  +
            theme_bundles(df = df, lev_molecule = lev_molecule)

}



adjustMSA <- function(df_msa, lev_molecule, line_widch, line_thickness, bundle_color, line_high) {
    data_scale <- lapply(nrow(df_msa) %>% seq_len(), function(i) {
        d <- df_msa[i,]
        d[2,] <-  d[1,]
        d[1,"position_adj"] <- d[1,"position"] - line_widch
        d[2,"position_adj"] <- d[2,"position"] + line_widch
        d
    }) %>% do.call("rbind",.)

    data_scale$y <- factor(data_scale$character, levels = lev_molecule) %>% as.numeric()

    data_adj <- lapply(data_scale$group %>% unique, function(g) {
        data_group <- data_scale[data_scale$group == g,]
        thickness <- line_thickness / factor(data_group$name) %>% as.numeric %>% max
        dd_adj <- lapply(unique(data_group$position), function(i){
            df_pos <- data_group[data_group$position == i,]
            lapply(unique(df_pos$y), function(j){
                df_y <- df_pos[df_pos$y == j,]
                thick_lev <- df_y$name %>% factor %>% as.numeric - 1
                df_y$y_adj <- df_y$y - 0.4 + line_high + thickness * thick_lev + line_thickness * (g - 1)
                df_y
            }) %>% do.call("rbind",.)
        }) %>% do.call("rbind",.)
    dd_adj$bundle_color <- bundle_color[[g]]
    dd_adj
    }) %>% do.call("rbind",.)
    return(data_adj)
}

##' @importFrom ggplot2 element_line
theme_bundles <- function(df, lev_molecule){
    break_y <- factor(lev_molecule, levels = lev_molecule) %>% as.numeric
    minor_y <- c(break_y + 0.5, break_y - 0.5) %>% unique
    break_x <- max(df$position) %>% seq_len
    minor_x <- c(break_x + 0.5, break_x - 0.5) %>% unique

    list(
        ylab(NULL),
        xlab("Position number"),
        scale_x_continuous(breaks = break_x, labels = break_x, minor_breaks = minor_x),
        scale_y_continuous(breaks = break_y, labels = lev_molecule, minor_breaks = minor_y),
        theme(panel.grid.minor.y = element_line(color = "#e8e0e0", size = 0.4),
              axis.line.x = element_line(color = "gray60", size = 0.8),
              panel.grid.major = element_blank(),
              axis.ticks.y = element_blank(),
              panel.background = element_blank())
  )
}






##' @method ggplot_add seqlogo
##' @export
ggplot_add.seqlogo <- function(object, plot, object_name) {
    msaData <- plot$layers[[1]]$data
    logo_tidyData <- msa2tidy(msaData)
    logo_font <- object$font
    logo_color <- object[["color"]]
    adaptive <- object$adaptive
    top <- object$top
    logo_custom_color <- object[["custom_color"]]
    show.legend <- object$show.legend

    ly_logo <- geom_logo(data  = logo_tidyData, font = logo_font, color = logo_color,
                         adaptive = adaptive, top = top, custom_color = logo_custom_color, show.legend = show.legend)
    ggplot_add(ly_logo, plot, object_name)
}

##' @method ggplot_add seed
##' @export
ggplot_add.seed <- function(object, plot, object_name) {
    msaData <- plot$layers[[1]]$data
    seed_tidyData <- msa2tidy(msaData)
    seed <- object$seed
    star <- object$star

    ly <- geom_seed1(seed_tidyData, seed, star)

    ggplot_add(ly, plot, object_name)
}



##' @method ggplot_add GCcontent
##' @export
ggplot_add.GCcontent <- function(object, plot, object_name) {
    msaData <- plot$layers[[1]]$data
    show.legend <- object$show.legend
    GC_tidyData <- msa2tidy(msaData)

    ly <- geom_GC1(GC_tidyData, show.legend = show.legend )

    ggplot_add(ly, plot, object_name)
}


##' @importFrom ggplot2 facet_wrap
##' @importFrom ggplot2 ggplot_add
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 coord_cartesian
##' @importFrom ggplot2 geom_blank
##' @method ggplot_add facet_msa
##' @export
ggplot_add.facet_msa <- function(object, plot, object_name){
    msaData <- plot$layers[[1]]$data
    field <- object$field
    facetData <- facet_data(msaData, field)

    ##update data
    plot$layers[[1]]$data <- facetData #ly_bg
    if (length(plot$layers) > 1){
        plot$layers[[2]]$data <- facetData #ly_label
    }

    region <- diff(range(facetData$position))
    xl_scale <- facet_scale(facetData, field)

    if (region %% field == 0) {
        plot + facet_wrap(.~facet, ncol = 1, scales = "free_x") +
            scale_x_continuous(expand = c(0,0), breaks = xl_scale, labels = xl_scale) +
            coord_cartesian()
    }else {
        max_pos <- facetData$position %>% max
        min_pos <- facetData$position %>% min
        max_facet <- facetData$facet %>% max
        minpos_maxfacet <- facetData[facetData$facet == max_facet,"position"] %>% min
        expand_pos <-  (region %/% field + 1) * field + min_pos

        dummy <- data.frame(x = c(minpos_maxfacet, expand_pos), facet = max_facet)
        plot +
            facet_wrap(.~facet, ncol = 1, scales = "free_x") +
            geom_blank(aes_(x = ~x), dummy, inherit.aes = FALSE) +
            scale_x_continuous(expand = c(0,0), breaks = xl_scale, labels = xl_scale) +
            coord_cartesian()
    }

}

##' @method ggplot_add msaBar
##' @importFrom aplot insert_top
##' @importFrom ggplot2 coord_cartesian
##' @export
ggplot_add.msaBar <- function(object, plot, object_name){
    msaData <- plot$layers[[1]]$data
    bar_tidyData <- msa2tidy(msaData)
    ly <- ly_bar(bar_tidyData)

    p_bar <- ggplot() + ly_bar(bar_tidyData) + bar_theme(bar_tidyData)
    plot <- plot + coord_cartesian()
    p_bar %>% insert_top(plot, height = 3)
}


##' @method ggplot_add nucleotideeHelix
##' @export
ggplot_add.nucleotideeHelix <- function(object, plot, object_name){
    msa_data <- plot$layers[[1]]$data
    tidy_data <- msa2tidy(msa_data)
    seq_numbers <- levels(tidy_data$name) %>% length

    helix_data <- object$helix_data
    color_by <- object$color_by
    overlap <- object$overlap

    if(is.data.frame(helix_data)) {
        helix_tidy <- tidy_helix(helix_data, color_by = color_by)
    }else {
        helix_tidy <- tidy_list_helix(helix_data, color_by = color_by)
    }
    ly <- layer_helix(helix_data = helix_tidy, overlap = overlap, seq_numbers = seq_numbers)
    ggplot_add(ly, plot, object_name)
}

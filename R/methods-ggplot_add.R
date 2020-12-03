##' @method ggplot_add seqlogo
##' @export
ggplot_add.seqlogo <- function(object, plot, object_name) {
    msaData <- plot$layers[[1]]$data
    logo_tidyData <- msa2tidy(msaData)

     logo_font <- object$font
     logo_color <- object$color
     adaptive <- object$adaptive
     top <- object$top

     ly_logo <- geom_logo(data  = logo_tidyData, font = logo_font, color = logo_color, adaptive = adaptive, top = top)
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
    GC_tidyData <- msa2tidy(msaData)

    ly <- geom_GC1(GC_tidyData)

    ggplot_add(ly, plot, object_name)
}


##' @importFrom ggplot2 facet_wrap
##' @importFrom ggplot2 ggplot_add
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 coord_cartesian
##' @method ggplot_add facet_msa
##' @export
ggplot_add.facet_msa <- function(object, plot, object_name){
    msaData <- plot$layers[[1]]$data
    field <- object$field
    facetData <- facet_data(msaData, field)

    # facetData$x_text <- NA
    # start <- min(facetData$position)
    # end <- max(facetData$position)
    # x_label <- pretty(start:end)
    # x_label[1] <- start
    #
    # #x_label[length(x_label)] <- end
    #
    # facetData[facetData$position %in% x_label,]$x_text <-
    #     facetData[facetData$position %in% x_label,]$position
    #
    # facetData[!is.na(facetData$x_text),]$x_text <-
    #     facetData[!is.na(facetData$x_text),]$x_text + field * facetData[!is.na(facetData$x_text),]$facet

    plot$layers[[1]]$data <- facetData #ly_bg

    if (length(plot$layers) > 1)
        plot$layers[[2]]$data <- facetData #ly_label

    # plot +
    #   geom_text(aes_(x = ~position, y = ~-1, label = ~x_text), data = facetData, na.rm = T, color = "#6d6d6d", size = 3.2) +
    #   facet_wrap(~facetData$facet, ncol = 1) + scale_x_continuous(breaks = NULL)
    #ggplot_add(msa_facet, plot, object_name)
    plot + facet_wrap(~facetData$facet, ncol = 1, scales = "free") + coord_cartesian()#+ scale_x_continuous(breaks = NULL)

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

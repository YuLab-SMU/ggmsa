##' @importFrom ggplot2 theme_minimal
theme_msa <- function(){
  list(
    xlab(NULL),
    ylab(NULL),
    coord_fixed(),
    theme_minimal() +
        theme(legend.position = 'none',
            strip.text = element_blank(),
            panel.spacing.y = unit(.4, "in"),
            panel.grid = element_blank())
  )
}


##' @importFrom grDevices colorRampPalette
##' @importFrom RColorBrewer brewer.pal
##' @importFrom ggplot2 coord_cartesian
##' @importFrom ggplot2 scale_x_continuous
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom ggplot2 scale_fill_gradientn
bar_theme <- function(tidy){
    data <- bar_data(tidy)
    color_palettes <- colorRampPalette(brewer.pal(n = 9, name = "Blues")[c(4:7)])
    list(
        xlab(NULL),
        ylab("consensus"),
        scale_x_continuous(breaks = data[[3]], labels = data[[1]]),
        scale_y_continuous(breaks = NULL),
        scale_fill_gradientn(colours = color_palettes(100)),
        theme_minimal() +
            theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
        )
}

facet_scale <- function(facetData, field) {
    facet0_pos <- facetData[facetData$facet == 0,"position"]
    msa_start <- min(facet0_pos)
    facet0_xl_scale <- pretty(min(facet0_pos):max(facet0_pos)) ## x labels of facet 0
    facet0_xl_scale[1] <- msa_start ## assign the start postion to the first label
    xl_scale <- facet0_xl_scale
    #print(facet0_xl_scale)
    for(i in max(facetData$facet) %>% seq_len) {
        scale_i <- facet0_xl_scale + field * i
        if(msa_start > 1) scale_i[1] <- scale_i[1] + 1
        #print(scale_i)
        xl_scale <- xl_scale %>% c(scale_i)
    }
    return(xl_scale)
}

.onAttach <- function(libname, pkgname){
    #options(total_heigh = 4)
    options(logo_width = 0.9)
    options(asterisk_width = .03)
    options(GC_pos = 2)
    options(shadingLen = .5)
    options(shading_alpha = .3)
}

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

.onAttach <- function(libname, pkgname){
    #options(total_heigh = 4)
    options(logo_width = 0.9)
    options(asterisk_width = .03)
    options(GC_pos = 2)
    options(shadingLen = .5)
    options(shading_alpha = .3)
}

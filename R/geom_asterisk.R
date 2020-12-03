##' a ggplot2 layer of asterisk as a polygon
##'
##'
##' @title a ggplot2 layer of asterisk as a polygon
##' @param mapping aes mapping
##' @param data a data frame
##' @param stat the statistical transformation to use on the data for this layer, as a string.
##' @param position position adjustment, either as a string, or the result of a call to a position adjustment function.
##' @param na.rm a logical value
##' @param show.legend a logical value
##' @param inherit.aes a logical value
##' @param ... additional parameters
##' @importFrom ggplot2 layer
##' @return ggplot2 layer
## @export
##' @noRd
##' @author Lang Zhou
##' @examples
##' #library(ggplot2)
##' #ggplot(mtcars, aes(mpg, disp)) + geom_asterisk()
geom_asterisk <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {

  layer(geom = Geomasterisk, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

##' @importFrom grid polygonGrob
##' @importFrom grid gpar
SeedStar <- function(x = NULL , y = NULL) {

    char_width <- getOption("asterisk_width")
    char_scale_2 <- getOption("char_scale_2")

    x_width <- char_scale_2 * diff(range(star$y))
    star$x = star$x * x_width/diff(range(star$x))

    char_scale <- diff(range(star$x))/diff(range(star$y))
    star$x = star$x * (char_width * char_scale)/diff(range(star$x))
    star$y = star$y * char_width/diff(range(star$y))

    star$x = star$x - min(star$x)  - (char_width * char_scale)/2 + x
    star$y = star$y - min(star$y)  - char_width/2 + y

    polygonGrob(star$x, star$y, gp = gpar(fill = "black") )

}


##' @importFrom ggplot2 ggproto
##' @importFrom ggplot2 Geom
##' @importFrom ggplot2 draw_key_polygon
##' @importFrom ggplot2 aes
##' @importFrom grid gTree
Geomasterisk <- ggproto("Geomasterisk", Geom,
                         required_aes = c("x", "y"),
                         default_aes = aes(fill = "black"),
                         draw_key = draw_key_polygon,

                         draw_panel = function(data, panel_params, coord) {
                             data <- coord$transform(data, panel_params)
                             grobs <- lapply(1:nrow(data), function(i) {
                                          SeedStar(data$x[i], data$y[i])
                                      })
                             class(grobs) <- "gList"
                             ggplot2:::ggname("geom_asterisk", gTree(children = grobs))
                         }

)





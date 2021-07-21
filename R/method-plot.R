##' plot method for SeqDiff object
##'
##' @name plot
##' @rdname plot-methods
##' @exportMethod plot
##' @aliases plot,SeqDiff,ANY-method
##' @docType methods
##' @param x SeqDiff object
##' @param width bin width
##' @param title plot title
##' @param xlab xlab
##' @param by one of 'bar' and 'area'
##' @param fill fill color of upper part of the plot
##' @param colors color of lower part of the plot
##' @param xlim limits of x-axis
##' @return plot
##' @importFrom ggplot2 ggtitle
##' @importFrom ggplot2 xlim
##' @importFrom ggplot2 ggplot_gtable
##' @importFrom ggplot2 ggplot_build
##' @importFrom grid unit.pmax
##' @importFrom aplot plot_list
##' @author guangchuang yu
##' @examples
##' fas <- list.files(system.file("extdata", "GVariation", package="ggmsa"),
##'                   pattern="fas", full.names=TRUE)
##' x1 <- seqdiff(fas[1], reference=1)
##' plot(x1)
setMethod("plot", signature(x="SeqDiff"),
          function(x, width=50, title="auto",
                   xlab = "Nucleotide Position",
                   by="bar", fill="firebrick",
                   colors=c(A="#E495A5", C="#ABB065", G="#39BEB1", T="#ACA4E2"),
                   xlim = NULL) {
              nn <- names(x@sequence)
              if (is.null(title) || is.na(title)) {
                  title <- ""
              } else if (title == "auto") {
                  title <- paste(nn[-x@reference], "nucelotide differences relative to", nn[x@reference])
              }

              p1 <- plot_difference_count(x@diff, width, by=by, fill=fill) + ggtitle(title)
              p2 <- plot_difference(x@diff, colors=colors, xlab)

              if (!is.null(xlim)) {
                  p1 <- p1 + xlim(xlim)
                  p2 <- p2 + xlim(xlim)
              }

              # gp1<- ggplot_gtable(ggplot_build(p1))
              # gp2<- ggplot_gtable(ggplot_build(p2))
              # maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
              # gp1$widths[2:3] <- maxWidth
              # gp2$widths[2:3] <- maxWidth

              ## pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.7, 0.7), "null"))))
              ## gp2$vp = viewport(layout.pos.row = 2, layout.pos.col = 1)
              ## grid.draw(gp2)
              ## gp1$vp = viewport(layout.pos.row = 1, layout.pos.col = 1)
              ## grid.draw(gp1)
              ## plot_grid(gp1, gp2, ncol=1, rel_heights=c(.7, .4))
              plot_list(list(p1, p2), ncol=1, heights=c(.7, .4))
          }
          )



##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 scale_y_continuous
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##' @importFrom ggplot2 scale_color_manual
plot_difference <- function(x, colors, xlab="Nucleotide Position") {
    x$difference <-  x$difference %>% toupper
    yy = 4:1
    names(yy) = c("A", "C", "G", "T")
    x$y <- yy[x$difference]
    n <- sum(is.na(x$y))
    if (n > 0) {
        message(n, " sites contain deletions or ambiguous bases, which will be ignored in current implementation...")
    }
    x <- x[!is.na(x$y),]
    p <- ggplot(x, aes_(x=~position, y=~y, color=~difference))

    p + geom_segment(aes_(x=~position, xend=~position, y=~y, yend=~y+.8)) +
        xlab(xlab) + ylab(NULL) +
        scale_y_continuous(breaks=yy, labels=names(yy)) +
        theme_minimal() +
        theme(legend.position="none")+
        theme(axis.text.x=element_blank(), axis.ticks.x = element_blank()) +
        scale_color_manual(values=colors)
}

##' @importFrom ggplot2 geom_col
##' @importFrom ggplot2 geom_area
##' @importFrom ggplot2 theme_bw
plot_difference_count <- function(x, width, by = 'bar', fill='red') {
    by <- match.arg(by, c("bar", "area"))
    if (by == 'bar') {
        geom <- geom_col(fill=fill, width=width)
        keep0 <- FALSE
    } else if (by == "area") {
        geom <- geom_area(fill=fill)
        keep0 <- TRUE
    }
    d <- nucleotide_difference_count(x, width, keep0)
    p <- ggplot(d, aes_(x=~position, y=~count))
    p + geom + xlab(NULL) + ylab("Difference") + theme_bw()
}


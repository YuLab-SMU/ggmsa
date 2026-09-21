library(ggplot2)
library(ggmsa)

msa <- system.file("extdata", "seedSample.fa", package = "ggmsa")
field <- 11
n_pos <- length(unique(tidy_msa(msa)$position))
n_panels <- ceiling(n_pos / field)

panels_of <- function(p) lapply(seq_along(p), function(i) p[[i]])
bar_layer <- function(p) {
    for (s in c(list(p), if (inherits(p, "patchwork")) panels_of(p) else NULL)) {
        if (!inherits(s, "ggplot")) next
        hit <- Filter(function(ly) is.data.frame(ly$data) &&
                          "Freq" %in% names(ly$data), s$layers)
        if (length(hit)) return(hit[[1]])
    }
    NULL
}
n_layers <- function(p) {
    s <- if (inherits(p, "patchwork")) panels_of(p)[[1]] else p
    length(s$layers)
}
panels_built <- function(p) {
    vapply(panels_of(p), function(panel) {
        nrow(suppressWarnings(ggplot_build(panel))$layout$layout)
    }, integer(1))
}

test_that("geom_msaBar keeps the plot composable (#49)", {
    p <- ggmsa(msa, color = "Chemistry_NT") + geom_msaBar()
    expect_s3_class(p, "patchwork")
    expect_length(p, 2)
    ## ggplot2 4.x used to find no `+` method for the aplot composition and
    ## returned NULL silently
    expect_false(is.null(p + theme_minimal()))
    expect_s3_class(p + labs(x = "positions"), "patchwork")
    expect_s3_class(p + coord_cartesian(), "patchwork")
    expect_s3_class(p + scale_x_continuous(), "patchwork")
})

test_that("facet_msa works after geom_msaBar (#49)", {
    plain <- ggmsa(msa, color = "Chemistry_NT") + geom_msaBar()
    facetted <- plain + facet_msa(field = field)
    expect_length(facetted, 2)
    for (panel in panels_of(facetted)) {
        expect_s3_class(panel$facet, "FacetWrap")
    }
    ## the bar is split like the alignment, and every column survives the split
    expect_equal(panels_built(facetted), rep(n_panels, 2))
    expect_length(bar_layer(facetted)$data$position, n_pos)
})

test_that("facet_msa before geom_msaBar facets the bar too (#72)", {
    first <- ggmsa(msa, color = "Chemistry_NT") +
        facet_msa(field = field) + geom_msaBar()
    second <- ggmsa(msa, color = "Chemistry_NT") +
        geom_msaBar() + facet_msa(field = field)

    for (p in list(first, second)) {
        expect_length(p, 2)
        expect_equal(panels_built(p), rep(n_panels, 2))
        expect_equal(sort(bar_layer(p)$data$position), seq_len(n_pos))
        ## the bar rows are grouped exactly as the alignment rows
        expect_equal(sort(unique(bar_layer(p)$data$facet)),
                     seq(0, n_panels - 1))
    }
})

test_that("layers added after geom_msaBar land on the alignment panel", {
    plain <- ggmsa(msa, color = "Chemistry_NT")
    with_bar <- plain + geom_msaBar()
    for (layer in list(geom_seqlogo(), geom_GC())) {
        panels <- panels_of(with_bar + layer)
        expect_equal(n_layers(panels[[1]]), n_layers(plain) + 1)
        expect_length(panels[[2]]$layers, 1)
    }
})

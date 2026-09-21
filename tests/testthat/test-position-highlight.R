library(ggplot2)
library(ggmsa)

msa <- system.file("extdata", "seedSample.fa", package = "ggmsa")
highlight <- c(6, 12)
n_pos <- length(unique(tidy_msa(msa)$position))

n_positions <- function(d) length(unique(d$position))
facets_of <- function(d) sort(unique(d$facet))

layer_of <- function(p, geom) {
    Filter(function(ly) inherits(ly$geom, geom), p$layers)
}

bar_positions <- function(p) {
    panels <- if (inherits(p, "patchwork")) {
        lapply(seq_along(p), function(i) p[[i]])
    } else {
        list(p)
    }
    for (s in panels) {
        if (!inherits(s, "ggplot")) next
        for (ly in s$layers) {
            if (is.data.frame(ly$data) && "position" %in% names(ly$data) &&
                    "Freq" %in% names(ly$data)) {
                return(ly$data$position)
            }
        }
    }
    NULL
}

test_that("position_highlight leaves the alignment source intact", {
    p <- ggmsa(msa, color = "Chemistry_NT", position_highlight = highlight)
    expect_equal(n_positions(p$ggmsa_data), n_pos)
    ## highlight boxes sit behind the glyphs
    expect_s3_class(p$layers[[1]]$geom, "GeomTile")
    expect_s3_class(p$layers[[2]]$geom, "GeomPolygon")
    expect_equal(sort(unique(p$layers[[1]]$data$position)), sort(highlight))
    expect_equal(n_positions(p$layers[[2]]$data), n_pos)
})

test_that("position_highlight does not crop the axes of a font = NULL plot", {
    p <- ggmsa(msa, color = "Chemistry_NT", font = NULL,
               position_highlight = highlight)
    built <- suppressWarnings(ggplot_build(p))
    expect_equal(range(unlist(lapply(built$data, function(d) d$x))),
                 c(1, n_pos))
    ## a blank layer carries the full extent, only the requested columns paint
    expect_s3_class(p$layers[[1]]$geom, "GeomBlank")
    expect_equal(n_positions(p$layers[[1]]$data), n_pos)
    expect_equal(sort(unique(p$layers[[2]]$data$position)), sort(highlight))
})

test_that("geom_msaBar keeps every column next to position_highlight (#75)", {
    plain <- ggmsa(msa, color = "Chemistry_NT") + geom_msaBar()
    hl <- ggmsa(msa, color = "Chemistry_NT", position_highlight = highlight) +
        geom_msaBar()
    expect_equal(bar_positions(hl), bar_positions(plain))
    expect_length(bar_positions(hl), n_pos)

    hl2 <- ggmsa(msa, color = "Chemistry_NT", font = NULL,
                 position_highlight = highlight) + geom_msaBar()
    expect_length(bar_positions(hl2), n_pos)
})

test_that("facet_msa works next to position_highlight (#76)", {
    plain <- ggmsa(msa, color = "Chemistry_NT") + facet_msa(field = 11)
    hl <- ggmsa(msa, color = "Chemistry_NT",
                position_highlight = highlight) + facet_msa(field = 11)

    glyphs <- layer_of(hl, "GeomPolygon")[[1]]$data
    expect_equal(facets_of(glyphs), c(0, 1))
    expect_equal(n_positions(glyphs), n_pos)

    ## the highlight layer only holds two positions, so it must be faceted
    ## against the global origin: 6 -> panel 0, 12 -> panel 1
    boxes <- layer_of(hl, "GeomTile")[[1]]$data
    expect_equal(facets_of(boxes[boxes$position == 6, ]), 0)
    expect_equal(facets_of(boxes[boxes$position == 12, ]), 1)
    expect_equal(range(glyphs$position),
                 range(layer_of(plain, "GeomPolygon")[[1]]$data$position))
})

test_that("facet_msa still facets a font = NULL plot that is highlighted", {
    p <- ggmsa(msa, color = "Chemistry_NT", font = NULL,
               position_highlight = highlight) + facet_msa(field = 11)
    expect_equal(n_positions(p$layers[[1]]$data), n_pos)
    expect_equal(facets_of(p$layers[[1]]$data), c(0, 1))
    expect_equal(n_positions(p$layers[[2]]$data), length(highlight))
})

test_that("facet_msa no longer overwrites unrelated layers (#20, #41)", {
    ann <- data.frame(position = c(3, 9), val = c(1, 2))
    p <- ggmsa(msa, color = "Chemistry_NT", font = NULL) +
        geom_point(aes(x = position, y = val), data = ann) +
        facet_msa(field = 11)
    kept <- Filter(function(ly) is.data.frame(ly$data) &&
                       identical(ly$data$val, ann$val), p$layers)
    expect_length(kept, 1)
})

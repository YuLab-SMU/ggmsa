library(ggplot2)
library(ggmsa)

aln <- tidy_msa(system.file("extdata", "sample.fasta", package = "ggmsa"), 150, 170)
n_cells <- nrow(aln)


test_that("the glyph expansion repeats every column but `x`/`y`", {
    ## this is the invariant that makes deduplicating the tile data lossless
    expanded <- msa_data(aln)
    expect_gt(nrow(expanded), n_cells)
    expect_equal(nrow(unique(expanded[, c("name", "position", "color")])), n_cells)
})


test_that("background tiles are drawn once per cell, not once per vertex (#74)", {
    expect_equal(nrow(tile_granularity(msa_data(aln))), n_cells)

    p <- ggplot() + geom_msa(data = aln) + coord_fixed()
    bg <- p$layers[[1]]$data
    expect_equal(nrow(bg), n_cells)
    expect_false(any(duplicated(bg[, c("name", "position")])))
    expect_named(bg, names(msa_data(aln)))

    ## the glyphs must keep their vertex granularity
    glyphs <- p$layers[[2]]$data
    expect_gt(nrow(glyphs), n_cells)
    expect_true(all(c("x", "y", "group", "order") %in% names(glyphs)))
})


test_that("highlighted columns are deduplicated the same way", {
    hl <- c(155, 160)
    p <- ggplot() + geom_msa(data = aln, position_highlight = hl)
    boxes <- p$layers[[1]]$data
    expect_equal(nrow(boxes), sum(aln$position %in% hl))
    expect_setequal(unique(boxes$position), hl)
})


test_that("deduplication does not drop cells or repaint them elsewhere", {
    p <- ggplot() + geom_msa(data = aln) + coord_fixed()
    bg <- ggplot_build(p)$data[[1]]
    expect_equal(nrow(bg), n_cells)
    expect_equal(as.numeric(sort(unique(bg$x))), sort(unique(aln$position)))
    expect_equal(as.character(sort(unique(bg$fill))),
                 as.character(sort(unique(msa_data(aln)$color))))
})

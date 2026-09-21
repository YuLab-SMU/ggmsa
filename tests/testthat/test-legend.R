library(ggplot2)
library(ggmsa)

testthat::skip_if_not("get_guide_data" %in% getNamespaceExports("ggplot2"),
                      "ggplot2 is too old to inspect guide data")

msa <- system.file("extdata", "sample.fasta", package = "ggmsa")

test_that("show.legend = TRUE produces a fill legend (#68)", {
    p <- ggmsa(msa, 150, 170, font = NULL, show.legend = TRUE)
    gd <- get_guide_data(p, "fill")
    expect_false(is.null(gd))
    expect_gt(nrow(gd), 1)
    ## every key is labelled with the characters sharing that colour
    expect_true(all(nzchar(as.character(gd$.label))))
})

test_that("the legend also shows when geom_msa is added by hand", {
    p <- ggplot() +
        geom_msa(data = tidy_msa(msa, 150, 170), font = NULL,
                 show.legend = TRUE) +
        coord_fixed()
    expect_false(is.null(get_guide_data(p, "fill")))
})

test_that("show.legend = FALSE keeps the legend hidden", {
    p <- ggmsa(msa, 150, 170, font = NULL)
    expect_null(get_guide_data(p, "fill"))
})

test_that("mapping fill through the manual scale does not recolour tiles", {
    d <- msa_data(tidy_msa(msa, 150, 170), font = NULL)
    xx <- unique(d[, c("character", "color")])
    xx <- xx[!is.na(xx$color), ]
    cols <- unique(xx$color)
    names(cols) <- cols
    labels <- vapply(unique(xx$color), function(cl)
        paste(xx$character[xx$color == cl], collapse = ","), character(1))

    trained <- ggplot_build(
        ggplot() +
            geom_tile(mapping = aes(x = position, y = name, fill = color),
                      data = d, inherit.aes = FALSE, show.legend = TRUE) +
            scale_fill_manual(values = cols, breaks = cols, labels = labels)
    )$data[[1]]$fill

    expect_equal(as.character(trained), as.character(d$color))
})

test_that("position_highlight keeps a working legend", {
    p <- ggmsa(msa, font = NULL, position_highlight = c(150, 152, 155),
               show.legend = TRUE)
    expect_false(is.null(get_guide_data(p, "fill")))
})

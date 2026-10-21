library(ggplot2)
library(ggmsa)

test_that("gghelix keeps a fixed coordinate system", {
    ss <- readSSfile(system.file("extdata", "Rfam", "RF03120_SS.txt",
                                 package = "ggmsa"), type = "Vienna")
    p <- gghelix(ss)
    expect_s3_class(ggplot_build(p), "ggplot_built")
    ## ggplot2 4.x reports both coords as CoordCartesian; the fixed one
    ## carries a non-NULL ratio, so compare that instead of the class.
    expect_equal(p$coordinates$ratio, 1)
})


test_that("geom_seed works in both shading and star modes", {
    msa <- system.file("extdata", "seedSample.fa", package = "ggmsa")
    shading <- ggmsa(msa, font = "DroidSansMono", color = "Chemistry_NT") +
        geom_seed(seed = "GAGGUAG", star = FALSE)
    star <- ggmsa(msa, font = "DroidSansMono", color = "Chemistry_NT") +
        geom_seed(seed = "GAGGUAG", star = TRUE)

    expect_s3_class(ggplot_build(shading), "ggplot_built")
    expect_s3_class(ggplot_build(star), "ggplot_built")
    expect_false(
        identical(shading$layers[[length(shading$layers)]]$geom,
                  star$layers[[length(star$layers)]]$geom)
    )
})


test_that("plot.SeqDiff still builds after the mapping update", {
    fas <- list.files(system.file("extdata", "GVariation", package = "ggmsa"),
                      pattern = "fas", full.names = TRUE)
    x1 <- seqdiff(fas[1], reference = 1)
    p <- plot(x1)
    expect_s3_class(p, "patchwork")
    for (panel in seq_along(p)) {
        expect_s3_class(ggplot_build(p[[panel]]), "ggplot_built")
    }
})


test_that("ggmaf builds a MAF plot", {
    skip_if_not(requireNamespace("statebins", quietly = TRUE),
                "statebins is not installed")
    skip("the bundled MAF example needs a real .maf file; none is shipped")
})

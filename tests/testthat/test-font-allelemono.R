library(ggmsa)

test_that("AlleleMono provides the same characters as the other fonts", {
    expect_true("AlleleMono" %in% names(font_fam))
    expect_setequal(names(font_fam$AlleleMono), names(font_fam$DroidSansMono))
    for (g in font_fam$AlleleMono) {
        expect_s3_class(g, "glyph_polygon")
        expect_named(g, c("x", "y", "order"))
        expect_false(anyNA(g$x) || anyNA(g$y))
    }
})


test_that("available_fonts() returns the available font families", {
    expect_equal(available_fonts(), names(font_fam))
    expect_true("AlleleMono" %in% available_fonts())
})


test_that("available_colors() and available_msa() return their values", {
    expect_true(all(names(scheme_NT) %in% available_colors()))
    expect_true(all(c("Clustal", names(scheme_AA)) %in% available_colors()))
    expect_true(all(c(".fasta", supported_msa_class) %in% available_msa()))
})


test_that("msa_data() and ggmsa() work with `font = 'AlleleMono'`", {
    msa <- system.file("extdata", "sample.fasta", package = "ggmsa")
    msaData <- msa_data(tidy_msa(msa, 10, 20), font = "AlleleMono")
    expect_false(anyNA(msaData$x) || anyNA(msaData$yy))

    p <- ggmsa(msa, start = 10, end = 20, font = "AlleleMono")
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

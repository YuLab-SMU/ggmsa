library(ggplot2)
library(ggmsa)

msa <- system.file("extdata", "sample.fasta", package = "ggmsa")

test_that("assign_dms keeps unmatched mutations as NA instead of crashing", {
    tidy <- tidy_msa(msa, 10, 10)
    chars <- sort(unique(tidy$character))
    covered <- setdiff(chars, chars[1])
    dms <- data.frame(site_RBD = 10,
                      wildtype = "A",
                      mutation_RBD = paste0("A10", covered),
                      bind_avg = seq_along(covered),
                      stringsAsFactors = FALSE)

    out <- assign_dms(tidy, dms)
    expect_equal(nrow(out), nrow(tidy))
    expect_true(anyNA(out$bind_avg))
})


test_that("geom_msa can draw DMS data with a continuous scale", {
    tidy <- tidy_msa(msa, 10, 10)
    chars <- sort(unique(tidy$character))
    dms <- data.frame(site_RBD = 10,
                      wildtype = "A",
                      mutation_RBD = paste0("A10", chars),
                      bind_avg = seq_along(chars),
                      stringsAsFactors = FALSE)

    out <- assign_dms(tidy, dms)
    p <- ggplot() + geom_msa(out, dms = TRUE)
    expect_s3_class(ggplot_build(p), "ggplot_built")
})


test_that("consensus views reject show.legend regardless of font mode", {
    expect_error(
        ggplot_build(ggmsa(msa, 150, 170, consensus_views = TRUE,
                           font = NULL, show.legend = TRUE)),
        "consensus view"
    )
    expect_error(
        ggplot_build(ggmsa(msa, 150, 170, consensus_views = TRUE,
                           font = "helvetical", show.legend = TRUE)),
        "consensus view"
    )
})

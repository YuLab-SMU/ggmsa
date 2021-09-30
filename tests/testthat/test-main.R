library(ggmsa)
library(ggplot2)


test_that("check whether `ggmsa` create a `ggplot` object", {
    p <- ggmsa(msa = system.file("extdata", "sample.fasta", package = "ggmsa"), 
               start = 10, 
               end = 20,
               font = NULL)
    
    
    expect_true(is.ggplot(p))
})




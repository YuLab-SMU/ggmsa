

library(ggmsa)

msa <- system.file("extdata", "sample.fasta", package = "ggmsa")
tidymsa <- tidy_msa(msa, 10, 20)


test_that("check msaData integrity when using `font`", {
    msaData <- msa_data(tidymsa)
    msaFull_names <- c("label", 
                       "x",
                       "yy",
                       "order",
                       "name",
                       "position",
                       "character", 
                       "color",
                       "group", 
                       "y")
    
    expect_true(is.data.frame(msaData))
    expect_named(msaData, msaFull_names)
})


test_that("check msaData integrity when using `font = NULL`", {
    msaData <- msa_data(tidymsa, font = NULL)
    msaFull_names <- c("name", "position", "character", "color"  )
    
    expect_true(is.data.frame(msaData))
    expect_named(msaData, msaFull_names)
})

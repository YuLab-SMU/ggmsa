

library(ggmsa)
library(Biostrings)
library(phangorn)

msa <- system.file("extdata", "sample.fasta", package = "ggmsa")
tidy_names <- c("name", "position", "character")


test_that("tidy FASTA format by tidy_msa", {
  fasta_tidy <- tidy_msa(msa, 10, 20)
  expect_true(is.data.frame(fasta_tidy))
  expect_named(fasta_tidy, tidy_names)
})


test_that("tidy Biostrings objects by tidy_msa", {
    AAMultipleAlignment <- readAAMultipleAlignment(msa)
    expect_s4_class(AAMultipleAlignment, "AAMultipleAlignment")
    
    AAStringSet <- readAAStringSet(msa)
    expect_s4_class( AAStringSet, "AAStringSet")
    
    AAMultipleAlignment_tidy <- tidy_msa(AAMultipleAlignment, 10, 20)
    AAStringSet_tidy <- tidy_msa(AAStringSet, 10, 20)
    
    expect_true(is.data.frame(AAMultipleAlignment_tidy))
    expect_named(AAMultipleAlignment_tidy, tidy_names)
    
    
    expect_true(is.data.frame(AAStringSet_tidy))
    expect_named(AAStringSet_tidy, tidy_names)
})


test_that("tidy AAbin objects by tidy_msa", {
    AAbin <- read.aa(msa, "fasta")
    expect_s3_class(AAbin, "AAbin")
    
    AAbin_tidy <- tidy_msa(AAbin, 10, 20)
    
    expect_true(is.data.frame(AAbin_tidy))
    expect_named(AAbin_tidy, tidy_names)
})




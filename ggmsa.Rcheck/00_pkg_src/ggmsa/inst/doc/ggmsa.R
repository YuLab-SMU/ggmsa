## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

CRANpkg <- function(pkg) {
    cran <- "https://cran.r-project.org/package"
    fmt <- "[%s](%s=%s)"
    sprintf(fmt, pkg, cran, pkg)
}

Biocpkg <- function(pkg) {
    sprintf("[%s](http://bioconductor.org/packages/%s)", pkg, pkg)
}

library(ggmsa)
library(ggplot2)

## ----eval=FALSE----------------------------------------------------------
#  ## installing the package
#  install.packages("ggmsa")
#  ## loading the package
#  library("ggmsa")

## ----warning=FALSE-------------------------------------------------------
 available_msa()

 protein_sequences <- system.file("extdata", "sample.fasta", package = "ggmsa")
 miRNA_sequences <- system.file("extdata", "seedSample.fa", package = "ggmsa")
 nt_sequences <- system.file("extdata", "LeaderRepeat_All.fa", package = "ggmsa")
 

## ----fig.height = 2, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 265, end = 300)

## ----warning=FALSE-------------------------------------------------------
 available_colors()

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, color = "Clustal")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, color = "Chemistry_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, color = "Shapely_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, color = "Taylor_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, color = "Zappo_AA")

## ----warning=FALSE-------------------------------------------------------
 available_fonts()

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, font = "helvetical", color = "Chemistry_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, font = "TimesNewRoman", color = "Chemistry_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, font = "DroidSansMono", color = "Chemistry_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, font = NULL, color = "Chemistry_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, char_width = 0.5, color = "Chemistry_AA")

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, start = 320, end = 360, none_bg = TRUE) + theme_void()

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, 164, 213, color = "Chemistry_AA", 
      posHighligthed = c(185, 190))

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, 164, 213, color = "Chemistry_AA", seq_name = TRUE)

## ----fig.height = 3, fig.width = 10, warning=FALSE-----------------------
ggmsa(protein_sequences, 164, 213, font = NULL, color = "Chemistry_AA", seq_name = FALSE)


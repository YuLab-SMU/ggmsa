<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggmsa: Plot multiple sequence alignment using ggplot2 <img src="man/figures/logo.png" height="140" align="right" />

[![](https://www.r-pkg.org/badges/version/badger?color=blue)](https://cran.r-project.org/package=badger)
[![](https://img.shields.io/badge/devel%20version-1.0.0-blue.svg)](https://github.com/YuLab-SMU/ggmsa)
[![](http://cranlogs.r-pkg.org/badges/grand-total/badger?color=green)](https://cran.r-project.org/package=badger)
[![](http://cranlogs.r-pkg.org/badges/last-month/badger?color=green)](https://cran.r-project.org/package=badger)
[![CRAN
checks](https://cranchecks.info/badges/summary/ggmsa)](https://cran.r-project.org/web/checks/check_results_ggmsa.html)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
Artistic-2.0](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://cran.r-project.org/web/licenses/Artistic-2.0)
<!-- badges: start -->
<!-- [![CRAN_Release_Badge](https://www.r-pkg.org/badges/version-ago/ggmsa)](https://cran.r-project.org/package=ggmsa)-->
<!-- [![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/grand-total/ggmsa?color=green)](https://cran.r-project.org/package=ggmsa)-->
<!-- badges: end -->

`ggmsa` is designed for visualization and annotation of multiple
sequence alignment. It implements functions to visualize
publication-quality multiple sequence alignments (protein/DNA/RNA) in R
extremely simple and powerful.

For details, please visit <http://yulab-smu.top/ggmsa/>

## :hammer: Installation

You can install `ggmsa` from CRAN using `install.packages('ggmsa')`.
Alternatively you can grab the development version from github using
devtools:

``` r
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("YuLab-SMU/ggmsa")
```

## :bulb: Quick Example

``` r
library(ggmsa)
protein_sequences <- system.file("extdata", "sample.fasta", package = "ggmsa")
ggmsa(protein_sequences, start = 221, end = 280, char_width = 0.5, seq_name = T) + geom_seqlogo() + geom_msaBar()
```

![](man/figures/REAMED-unnamed-chunk-5-1.png)<!-- -->

## :books: Learn more

Check out the guides for learning everything there is to know about all
the different features:

  - [Getting
    Started](https://yulab-smu.github.io/ggmsa/articles/ggmsa.html)
  - [Annotations](https://yulab-smu.github.io/ggmsa/articles/guides/Annotations.html)
  - [Color Schemes and Font
    Families](https://yulab-smu.github.io/ggmsa/articles/guides/Color_schemes_And_Font_Families.html)
  - [Theme](https://yulab-smu.github.io/ggmsa/articles/guides/guides/MSA_theme.html)
  - [Other
    Modules](https://yulab-smu.github.io/ggmsa/articles/guides/guides/Other_Modules.html)
  - [View
    Modes](https://yulab-smu.github.io/ggmsa/articles/guides/guides/View_modes.html)

## :runner: Author

  - [GuangChuang Yu](https://guangchuangyu.github.io) Professor, PI

  - [Shuangbin Xu](https://github.com/xiangpin) PhD Student

  - [Lang Zhou](https://github.com/nyzhoulang) Master’s Student

**YuLab** <https://yulab-smu.top/>

**Department of Bioinformatics, School of Basic Medical Sciences,
Southern Medical University**

## :sparkling_heart: Contributing

We welcome any contributions\! By participating in this project you
agree to abide by the terms outlined in the [Contributor Code of
Conduct](http://yulab-smu.top/ggmsa/articles/CONDUCT.html).

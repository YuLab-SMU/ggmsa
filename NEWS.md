# ggmsa 1.13.2

+ replace `ggalt::geom_xspline()` with `ggfun::geom_xspline()` (2017-07-12, Sat)

# ggmsa 1.3.3

+ calling `\dontrun{}` for examples on `ggmsa()`

# ggmsa 1.3.2
+ bugfix: `geom_msaBar` conservation layer incorrectly aligned issues#34(2022-5-13, Fri)

# ggmsa 1.3.1

+ A new feature--selects ancestral sequence on Tree-MSA plot `treeMSA_plot` (2022-4-14, Thu)
+ A new feature--visualization of genome alignment `ggmaf` (2022-4-14, Thu)
+ A test feature--visualization protein-protein interactive (2022-4-14, Thu)
+ updated the way smooth is invoked on simplot(2022-01-03, Mon)

# ggmsa 1.1.4
added smoothed curve on simplot.(2021-12-17, Fri)

# ggmsa 1.1.3
fixed the typo in "posHighligthed", and changed it to 
snake_case "position_highlight" from camelCase "posHighligthed" (2021-12-13, Mon)


# ggmsa 1.1.2
fixed the assignment error on line 155 'seqlogo.R'

# ggmsa 1.1.1 
fixed error: using `||` instead of `|` on 110 lines in geom_msa.R


# ggmsa 0.99.0 or 0.99.x
(Prepare for submission to `Bioconductor`, 2021-09-22 Wed)

+ 0.99.1 update DESCRIPTION and NEWS files (2021-09-28, Tue)
+ 0.99.2 add documentation for row data in extdata/inst and clean up code (2021-09-29, Wed)
+ 0.99.3 remove some  vignettes from master (build on the gh-pages branch) (2021-10-1, Fri)
+ 0.99.4 remove 'stringr' package from 'Imports' (2021-10-11, Mon)
+ 0.99.5 make the consensus_views compatible ggtreeExtra and add package description. (2021-10-21, Thu)

# ggmsa 0.0.10 

+ update default color schemes in  lower part of the SeqDiff plot (2021-08-20, Fri)

# ggmsa 0.0.9

+ import R4RNA to fix R check (2021-08-03, Tue)

# ggmsa 0.0.8

+ bugfix: fix variable names error in color_scheme. (2021-07-29, Thu)
+ The migration of sequence recombination functionality from `seqcombo` package. (2021-07-20, Tue)


# ggmsa 0.0.7

+ added `gghelix()` and `geom_helix()`.(2021-04-1, Thu)
+ added option to show the fill legend.(2021-03-23, Tue)
+ added a error message to remind that "sequences must have unique names".(2021-03-18, Thu)
+ added `ggSeqBundle()` to plot Sequence Bundles for MSAs based `ggolot2` (2021-03-18, Thu)

# ggmsa 0.0.6

+ supports linking `ggtreeExtra`. (2021-01-21, Thu)
+ bugfix: reversed sequence in 'tree + geom_facet(font)' . (2021-01-21, Thu)
+ bugfix: partitioning error when the sequence starting point greater than 1. (2021-01-21, Thu)
+ bugfix: generates continuous x-axis labels for each panel. (2021-01-21, Thu)
+ supports customize colors `custom_color`. (2020-12-28, Mon)

# ggmsa 0.0.5

+ added a new view called `by_conservation`.(2020-12-22, Tue)
+ added a new color scheme `Hydrophobicity` and a new parameter `border`.(2020-12-21, Mon)
+ rewrite the function `facet_msa()`.(2020-12-03, Thu)
+ Debug: tree + geom_facet(geom_msa()) does not work.(2020-12-03, Thu)
+ added a new function `geom_msaBar()`.(2020-12-03, Thu)
+ added a new parameter `ignore_gaps` used in consensus views.(2020-10-09, Fri)
+ debug in consensus views (2020-10-05, Mon)
+ added consensus views (2020-9-30, Wed)
+ added new colors `LETTER` and `CN6` provided by ShixiangWang.[issues#8](https://github.com/YuLab-SMU/ggmsa/issues/8)

# ggmsa 0.0.4

+ fixed warning message in **msa_data.R** (2020-4-26, Sun)
+ added ggplot_add methods for `geom_*()` (2020-4-24, Fri)
+ added a parameter `seq_name` in `ggmsa()` (2020-4-23, Thu)
+ added a new function `facet_msa()` --> break down the MSA (2020-4-17, Fri)
+ added a parameter `posHighlighted` in `ggmsa()` (2020-4-17, Fri)
+ created a new layer `geom_asterisk()` to optimized `geom_seed()` (2020-4-11, Sta)
+ added new functions `available_colors()`, `available_fonts()` and `available_msa()` (2020-3-30, Thu)
+ added a new function `geom_seed()` --> highlight the seed region in miRNA sequences (2020-3-27, Fri)
+ added a new function `ggmotif()`--> plot sequence motifs independently (2020-3-23, Tue)
+ added a Monospaced Font `DroidSansMono` (2020-3-23, Mon)

# ggmsa 0.0.3

+ release of v=0.0.3 (2020-03-16, Mon)
+ added a new function `geom_GC()` --> plot GC content in MSA (2020-02-28, Fri)
+ added a new function `geom_seqlogo()` --> plot plot sequence motifs in MSA (2020-02-14, Fri)
+ used a proportional scaling algorithm (2020-01-08, Wed)


# ggmsa 0.0.2

+ support plot sequence logo (2019-12-25, Wed)
+ added three fonts：`helvetical`, `times_new_roman`, `mono` (2019-12-21, Sta)
+ ~~added three fonts：`serif_font`, `Montserrat_font`, `roboto_font` (2019-12-17, Tue)~~
+ added internal outline polygons (2019-12-15, Sun)
+ bug fixed of `tidy_msa`
+ import `seqmagick` for parsing fasta 
+ `tidy_msa` for converting msa file/object to tidy data frame (2019-12-09, Mon)

 
# ggmsa 0.0.1

+ initial CRAN release (2019-10-17, Thu) 
+ removed from CRAN on 2021-08-17

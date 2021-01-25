# ggmsa 0.0.6

+ supports linking `ggtreeExtra`. (2021-01-21, Thu)
+ bugfix: reversed seqence in 'tree + geom_facet(font)' . (2021-01-21, Thu)
+ bugfix: partitioning error when the sequence starting point greater than 1. (2021-01-21, Thu)
+ bugfix: generates continuous x-axis labels for each panel. (2021-01-21, Thu)
+ supports customize colors `custom_color`. (2020-12-28, Mon)

# ggmsa 0.0.5

+ added a new view called `by_conservation`.(2020-12-22, Tue)
+ added a new color scheme `Hydrophobicity` and a new paramter `border`.(2020-12-21, Mon)
+ rewrite the function `facet_msa()`.(2020-12-03, Thu)
+ Debug: tree + geom_facet(geom_msa()) does not work.(2020-12-03, Thu)
+ added a new annotational function `geom_msaBar()`.(2020-12-03, Thu)
+ added a new patameter `ignore_gaps` used in consensus views.(2020-10-09, Fri)
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

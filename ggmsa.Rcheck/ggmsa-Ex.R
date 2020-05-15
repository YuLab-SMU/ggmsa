pkgname <- "ggmsa"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ggmsa')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("available_colors")
### * available_colors

flush(stderr()); flush(stdout())

### Name: available_colors
### Title: List Color Schemes currently available
### Aliases: available_colors

### ** Examples

available_colors()



cleanEx()
nameEx("available_fonts")
### * available_fonts

flush(stderr()); flush(stdout())

### Name: available_fonts
### Title: List Font Families currently available
### Aliases: available_fonts

### ** Examples

available_fonts()



cleanEx()
nameEx("available_msa")
### * available_msa

flush(stderr()); flush(stdout())

### Name: available_msa
### Title: List MSA objects currently available
### Aliases: available_msa

### ** Examples

available_msa()



cleanEx()
nameEx("facet_msa")
### * facet_msa

flush(stderr()); flush(stdout())

### Name: facet_msa
### Title: segment MSA
### Aliases: facet_msa

### ** Examples

library(ggplot2) 
f <- system.file("extdata/sample.fasta", package="ggmsa")
# 2 fields
ggmsa(f, end = 120, font = NULL, color="Chemistry_AA") + facet_msa(field = 60)
# 3 fields 
ggmsa(f, end = 120, font = NULL,  color="Chemistry_AA") + facet_msa(field = 40)



cleanEx()
nameEx("geom_GC")
### * geom_GC

flush(stderr()); flush(stdout())

### Name: geom_GC
### Title: geom_GC
### Aliases: geom_GC

### ** Examples

library(ggplot2) 
#plot GC content 
f <- system.file("extdata/LeaderRepeat_All.fa", package="ggmsa")
ggmsa(f, font = NULL, color="Chemistry_NT") + geom_GC()



cleanEx()
nameEx("geom_asterisk")
### * geom_asterisk

flush(stderr()); flush(stdout())

### Name: geom_asterisk
### Title: a ggplot2 layer of star as a polygon
### Aliases: geom_asterisk

### ** Examples

library(ggplot2)
ggplot(mtcars, aes(mpg, disp)) + geom_asterisk()



cleanEx()
nameEx("geom_seed")
### * geom_seed

flush(stderr()); flush(stdout())

### Name: geom_seed
### Title: geom_seed
### Aliases: geom_seed

### ** Examples

miRNA_sequences <- system.file("extdata/seedSample.fa", package="ggmsa")
ggmsa(miRNA_sequences, font = 'DroidSansMono', color = "Chemistry_NT", none_bg = TRUE) + 
geom_seed(seed = "GAGGUAG") 
ggmsa(miRNA_sequences, font = 'DroidSansMono', color = "Chemistry_NT") + 
geom_seed(seed = "GAGGUAG", star = TRUE)



cleanEx()
nameEx("geom_seqlogo")
### * geom_seqlogo

flush(stderr()); flush(stdout())

### Name: geom_seqlogo
### Title: geom_seqlogo
### Aliases: geom_seqlogo

### ** Examples

#plot multiple sequence alignment and sequence motifs
f <- system.file("extdata/LeaderRepeat_All.fa", package="ggmsa")
ggmsa(f,font = NULL,color = "Chemistry_NT") + geom_seqlogo()



cleanEx()
nameEx("ggmotif")
### * ggmotif

flush(stderr()); flush(stdout())

### Name: ggmotif
### Title: ggmotif
### Aliases: ggmotif

### ** Examples

#plot sequence motif independently
nt_sequence <- system.file("extdata", "LeaderRepeat_All.fa", package = "ggmsa")
ggmotif(nt_sequence, color = "Chemistry_NT")



cleanEx()
nameEx("ggmsa")
### * ggmsa

flush(stderr()); flush(stdout())

### Name: ggmsa
### Title: ggmsa
### Aliases: ggmsa

### ** Examples

#plot multiple sequences by loading fasta format 
fasta <- system.file("extdata", "sample.fasta", package = "ggmsa")
ggmsa(fasta, 164, 213, color="Chemistry_AA")

#XMultipleAlignment objects can be used as input in the 'ggmsa'
#AAMultipleAlignment <- Biostrings::readAAMultipleAlignment(fasta)
#ggmsa(AAMultipleAlignment, 164, 213, color="Chemistry_AA")

#XStringSet objects can be used as input in the 'ggmsa'
#AAStringSet <- Biostrings::readAAStringSet(fasta)
#ggmsa(AAStringSet, 164, 213, color="Chemistry_AA")

#Xbin objects from 'seqmagick' can be used as input in the 'ggmsa'
#AAbin <- seqmagick::fa_read(fasta)
#ggmsa(AAbin, 164, 213, color="Chemistry_AA")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

##' read sequences
##'
##' This function supports both NT or AA sequences; It supports multiple input formats such as "DNAStringSet", "BStringSet", "AAStringSet", DNAbin", "AAbin" and a filepath.
##' @title read_fasta
##' @param fasta an XstringSet object frome Biostrings or a filepath
##' @return DNAbin or AAbin object
##' @export
##' @author Lang Zhou
read_fasta <- function(fasta) {
    if (missingArg(fasta)) {
      x <- NULL
    } else if (is(fasta, "DNAbin") || is(fasta, "AAbin") ) {
      x <- fasta
    } else if (is(fasta, "DNAStringSet") || is(fasta, "AAStringSet") || is(fasta, "BStringSet")){
      x <- read_biostr(fasta)
    }else if (is(fasta, "character")) {
      x <- treeio::read.fasta(fasta)
    } else {
      x <- NULL
    }
}

read_biostr <- function(fasta) {
    if (guess_biostr_type(fasta) == "NT") {
      class <- "DNAbin"
    } else {
      class <- "AAbin"
    }
    structure(lapply(fasta, .BStringSet2bin, class = class),
              class = class)
}

guess_biostr_type <- function(fasta) {
    if (class(fasta) %in% "DNAStringSet") {
      return('NT')
    }
    return('AA')
}

##' @import treeio
.BStringSet2bin <- getFromNamespace(".BStringSet2bin", "treeio")




































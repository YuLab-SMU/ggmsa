##' read sequences
##'
##' This function supports both NT or AA sequences; It supports multiple input formats such as "DNAStringSet", "BStringSet", "AAStringSet", DNAbin", "AAbin" and a filepath.
##' @title read_fasta
##' @param fasta an XstringSet object frome Biostrings or a filepath
##' @return DNAbin or AAbin object
## @export
##' @author Lang Zhou
##' @noRd
prepare_fasta <- function(fasta) {
    if (missingArg(fasta)) {
        stop("no input...")
    } else if (is(fasta, "character")) {
        return(treeio::read.fasta(fasta))
    } else if (is(fasta, "DNAbin") || is(fasta, "AAbin")) {
        return(fasta)
    } else if (!class(fasta) %in% supported_msa_class) {
        stop("multiple sequence alignment object no supported...")
    } 

    if (is(fasta, "DNAStringSet")) {
        class <- "DNAbin"
    } else if (is(fasta, "AAStringSet")) {
        class <- "AAbin"
    }

    structure(lapply(fasta, .BStringSet2bin, class = class),
              class = class)

    ## to do, BStringSet, DNAMultipleAlignment, AAMultipleAlignment
}


supported_msa_msa_class <- c("DNAStringSet",  "AAStringSet", "BStringSet", "DNAbin", "AAbin")



##' @import treeio
.BStringSet2bin <- getFromNamespace(".BStringSet2bin", "treeio")


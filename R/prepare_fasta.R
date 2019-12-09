##' read sequences
##'
##' This function supports both NT or AA sequences; It supports multiple input formats such as "DNAStringSet", "BStringSet", "AAStringSet", DNAbin", "AAbin" and a filepath.
##' @title read_fasta
##' @param fasta an XstringSet object frome Biostrings or a filepath
##' @return DNAbin or AAbin object
##' @importFrom Biostrings DNAStringSet
##' @importFrom Biostrings RNAStringSet
##' @importFrom Biostrings AAStringSet
## @export
##' @author Lang Zhou
##' @noRd
prepare_fasta <- function(fasta) {
    if (methods::missingArg(fasta)) {
        stop("no input...")
    } else if (methods::is(fasta, "character")) {
        fasta <- seqmagick::fa_read(fasta)
    } else if (!class(fasta) %in% supported_msa_class) {
        stop("multiple sequence alignment object no supported...")
    }

    res <- switch(class(fasta),
                  DNAbin = DNAbin2DNAStringSet(fasta),
                  AAbin = AAbin2AAStringSet(fasta),
                  DNAMultipleAlignment = DNAStringSet(fasta),
                  RNAMultipleAlignment = RNAStringSet(fasta),
                  ANAMultipleAlignment = AAStringSet(fasta),
                  fasta ## DNAstringSet, RNAStringSet, AAString, BStringSet
                  )
    return(res)
}
        

DNAbin2DNAStringSet <- function(fasta) {
    seqs <- vapply(seq_along(fasta),
                   function(i) paste0(as.character(fasta[[i]]), collapse=''),
                   character(1))
    names(seqs) <- names(fasta)

    switch(class(fasta),
           DNAbin = DNAStringSet(seqs),
           AAbin = AAStringSet(seqs))
}

AAbin2AAStringSet <- DNAbin2DNAStringSet



supported_msa_class <- c("DNAStringSet",  "RNAStringSet", "AAStringSet", "BStringSet",
                         "DNAMultipleAlignment", "RNAMultipleAlignment", "AAMultipleAlignment",
                         "DNAbin", "AAbin")




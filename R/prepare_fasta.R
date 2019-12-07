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
        type <- guess_sequence_type(readLines(fasta, n=2)[2])
        res <- seqmagick::fa_read(fasta, type)
    } else if (!class(fasta) %in% supported_msa_class) {
        stop("multiple sequence alignment object no supported...")
    }

    res <- switch(class(fasta),
                  DNAbin = DNAbin2DNAStringSet(fasta),
                  AAbin = AAbin2AAStringSet(fasta),
                  DNAMultipleAlignment = DNAStringSet(fasta),
                  RNAMultipleAlignment = RNAStringSet(fasta),
                  ANAMultipleAlignment = AAStringSet(fasta)
                  )
    return(res)
}
        

DNAbin2DNAstringSet <- function(fasta) {
    seqs <- vapply(seq_along(fasta),
                   function(i) paste0(as.character(fasta[[i]]), collapse=''),
                   character(1))
    names(seqs) <- names(fasta)

     switch(class(fasta),
            DNAbin = DNAStringSet(seqs),
            AAbin = AAStringSet(seqs))
}

AABin2DNAStringSet <- DNAbin2DNAStringSet


supported_msa_msa_class <- c("DNAStringSet",  "AAStringSet", "BStringSet",
                             "DNAMultipleAlignment", "RNAMultipleAlignment", "AAMultipleAlignment",
                             "DNAbin", "AAbin")



##' @import seqmagick
guess_sequence_type <- getFromNamespace("guess_sequence_type", "seqmagick")


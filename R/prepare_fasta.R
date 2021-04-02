##' preparing multiple sequence alignment
##'
##' This function supports both NT or AA sequences; It supports multiple input formats such as "DNAStringSet", "BStringSet", "AAStringSet", DNAbin", "AAbin" and a filepath.
##' @title prepare_msa
##' @param msa a multiple sequence alignment file or object
##' @return BStringSet based object
##' @importFrom Biostrings DNAStringSet
##' @importFrom Biostrings RNAStringSet
##' @importFrom Biostrings AAStringSet
## @export
##' @author Lang Zhou and Guangchuang Yu
##' @noRd
prepare_msa <- function(msa) {
    if (methods::missingArg(msa)) {
        stop("no input...")
    } else if (methods::is(msa, "character")) {
        msa <- seqmagick::fa_read(msa)
    } else if (!class(msa) %in% supported_msa_class) {
        stop("multiple sequence alignment object no supported...")
    }

    res <- switch(class(msa),
                  DNAbin = DNAbin2DNAStringSet(msa),
                  AAbin = AAbin2AAStringSet(msa),
                  DNAMultipleAlignment = DNAStringSet(msa),
                  RNAMultipleAlignment = RNAStringSet(msa),
                  AAMultipleAlignment = AAStringSet(msa),
                  msa ## DNAstringSet, RNAStringSet, AAString, BStringSet
                  )
    return(res)
}


DNAbin2DNAStringSet <- function(msa) {
    seqs <- vapply(seq_along(msa),
                   function(i) paste0(as.character(msa[i]) %>% unlist, collapse=''),
                   character(1))
    names(seqs) <- names(msa)

    switch(class(msa),
           DNAbin = DNAStringSet(seqs),
           AAbin = AAStringSet(seqs))
}

AAbin2AAStringSet <- DNAbin2DNAStringSet



supported_msa_class <- c("DNAStringSet",  "RNAStringSet", "AAStringSet", "BStringSet",
                         "DNAMultipleAlignment", "RNAMultipleAlignment", "AAMultipleAlignment",
                         "DNAbin", "AAbin")




##' This function lists font families currently available 
##' that can be used by 'ggmsa'
##'
##'
##' @title List Font Families currently available
##' @return A character vector of available font family names
##' @examples available_fonts()
##' @export
##' @author Lang Zhou
available_fonts <- function(){
    font <- names(font_fam)
    message("font families currently available:" )
    message(paste(font, collapse = ' '), "\n")
    invisible(font)
}

##' This function lists color schemes currently available that
##'  can be used by 'ggmsa'
##'
##'
##' @title List Color Schemes currently available
##' @return A character vector of available color schemes
##' @examples available_colors()
##' @export
##' @author Lang Zhou
available_colors <- function(){
    color_nt <- names(scheme_NT)
    color_aa <- c("Clustal", names(scheme_AA))

    message("1.color schemes for nucleotide sequences currently available:")
    message(paste(color_nt, collapse = ' '), "\n")

    message("2.color schemes for AA sequences currently available:")
    message(paste(color_aa, collapse = ' '), "\n")

    invisible(c(color_nt, color_aa))
}

##' This function lists MSA objects currently available that
##'  can be used by 'ggmsa'
##'
##'
##' @title List MSA objects currently available
##' @return A character vector of available objects
##' @examples available_msa()
##' @export
##' @author Lang Zhou
available_msa <- function(){
    files <- ".fasta"
    xstring <- supported_msa_class[!grepl("bin", supported_msa_class)]
    bin <- supported_msa_class[grepl("bin", supported_msa_class)]

    message("1.files currently available:")
    message(files, '\n')

    message("2.XStringSet objects from 'Biostrings' package:")
    message(paste(xstring, collapse = ' '), '\n')

    message("3.bin objects:")
    message(paste(bin, collapse = ' '), '\n')

    invisible(c(files, xstring, bin))
}


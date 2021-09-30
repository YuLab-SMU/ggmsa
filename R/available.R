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
    message("font families currently available:" )
    font <- paste(names(font_fam), collapse = ' ')
    message(font, "\n")
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
    message("1.color schemes for nucleotide sequences currently available:")
    color_nt <- paste(names(scheme_NT), collapse = ' ')
    message(color_nt, "\n")
    
    message("2.color schemes for AA sequences currently available:")
    color_aa <- paste(names(scheme_AA), collapse = ' ')
    message("Clustal", color_aa, "\n")
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
    message("1.files currently available:")
    message(".fasta",'\n')

    message("2.XStringSet objects from 'Biostrings' package:")
    mes <- paste(supported_msa_class[!grepl("bin", supported_msa_class)],
                 collapse = ' ')
    message(mes, '\n')

    message("3.bin objects:")
    mes_bin <- paste(supported_msa_class[grepl("bin", supported_msa_class)],
                     collapse = ' ')
    message(mes_bin, '\n')

}


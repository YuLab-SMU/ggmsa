
##' This function lists font families currently available that can be used by 'ggmsa'
##' 
##' 
##' @title List Font Families currently available
##' @return A character vector of available font family names
##' @examples available_fonts()
##' @export
##' @author Lang Zhou

available_fonts <- function(){
    message("font families currently available:" )
    cat(names(font_fam))
}

##' This function lists color schemes currently available that can be used by 'ggmsa'
##' 
##' 
##' @title List Color Schemes currently available
##' @return A character vector of available color schemes
##' @examples available_colors()
##' @export
##' @author Lang Zhou
available_colors <- function(){
    message("color schemes for nucleotide sequences currently available:")
    cat(names(scheme_NT), "\n")
    message("color schemes for AA sequences currently available:")
    cat("Clustal", names(scheme_AA))
}

##' This function lists MSA objects currently available that can be used by 'ggmsa'
##' 
##' 
##' @title List MSA objects currently available
##' @return A character vector of available objects
##' @examples available_msa()
##' @export
##' @author Lang Zhou
available_msa <- function(){
    message("files currently available:")
    cat(".fasta",'\n')
  
    message("XStringSet objects from 'Biostrings' package:")
    cat(supported_msa_class[!grepl("bin", supported_msa_class)],'\n')
  
    message("bin objects from 'seqmagick' package:")
    cat(supported_msa_class[grepl("bin", supported_msa_class)])
  
}


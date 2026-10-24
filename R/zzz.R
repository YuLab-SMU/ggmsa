.onLoad <- function(libname, pkgname){
    #options(total_heigh = 4)
    op <- list(logo_width = 0.9,
               asterisk_width = .03,
               GC_pos = 2,
               shadingLen = .5,
               shading_alpha = .3)
    toset <- !(names(op) %in% names(options()))
    if (any(toset)) options(op[toset])
}

#' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname){
    pkgVersion <- packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion, "  ",
                  "Document: http://yulab-smu.top/ggmsa/", "\n\n")
    citation <- paste0("If you use ", pkgname,
                       " in published research, please cite:\n",
                       "L Zhou, T Feng, S Xu, F Gao, TT Lam, Q Wang, T Wu, ",
                       "H Huang, L Zhan, L Li, Y Guan, Z Dai*, G Yu* ",
                       "ggmsa: a visual exploration tool for multiple sequence alignment and associated data. ",
                       "Briefings in Bioinformatics. DOI:10.1093/bib/bbac222")
    packageStartupMessage(paste0(msg, citation))
    
}
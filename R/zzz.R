#' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname){
    #options(total_heigh = 4)
    options(logo_width = 0.9)
    options(asterisk_width = .03)
    options(GC_pos = 2)
    options(shadingLen = .5)
    options(shading_alpha = .3)
    
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
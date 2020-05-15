##' This function parses FASTA file and convert it to a tidy data frame. 
##' The function will also assign color to each molecule (amino acid or nucleotide) according to the selected color scheme. Sequence logo data for drawing alignment label will also be added if font != NULL. The output of msa_data() is the input of geom_msa().
##'
##' @title msa_data
##' @param fasta Aligned fasta file.
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'. . Defaults is 'helvetical'. If you specify font = NULL, only the background box will be printed.
##' @param color A Color scheme. One of 'Clustal', 'Chemistry_AA', 'Shapely_AA', 'Zappo_AA', 'Taylor_AA', 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'.Defaults is 'Clustal'.
##' @param char_width characters width. Defaults is 0.9.
##' @return A data frame
##' @examples
##' fasta <- system.file("extdata/sample.fasta", package="ggmsa")
##' data <- msa_data(fasta, 20, 120, font = "helvetical", color = 'Chemistry_AA' )
## @export
##' @noRd
##' @author Guangchuang Yu
msa_data <- function(tidymsa, font = "helvetical", color = "Clustal", char_width = 0.9) {
    color <- match.arg(color, c("Clustal","Chemistry_AA","Shapely_AA","Zappo_AA","Taylor_AA",
                                "Chemistry_NT","Shapely_NT","Zappo_NT","Taylor_NT" ))

    y <- tidymsa

    if (color == "Clustal"){
        y <- color_Clustal(y)
    } else {
        y <- color_scheme(y, color)
    }

    if (is.null(font)) {
        return(y)
    }
    
    font_f <- font_fam[[font]]
    data_sp <- font_f[unique(y$character)] ## calling internal outline polygons 

    if (!'name' %in% names(y)) {
        if ('label' %in% names(y)) {
            ## y <- dplyr::rename(y, name = label)
            names(y)[names(y) == 'label'] <- "name"
        } else {
            stop("unknown sequence name...")
        }
    }

    y$name <- factor(y$name, levels = unique(y$name))
    y$ypos <- as.numeric(y$name)

    yy <- lapply(1:nrow(y), function(i) {
        d <- y[i, ]
        dd <- data_sp[[d$character]]
        char_scale <- diff(range(dd$x))/diff(range(dd$y))#equal proportion
        
        if ( diff(range(dd$x)) <= diff(range(dd$y)) ){#y-width = char_width, x-width scaled proportionally 
            dd$x <- dd$x * (char_width * char_scale)/diff(range(dd$x))
            dd$y <- dd$y * char_width/diff(range(dd$y))

            dd$x <- dd$x - min(dd$x) + d$position - (char_width * char_scale)/2
            dd$y <- dd$y - min(dd$y) + d$ypos - char_width/2
        }else{                                        #x-width = char_width, y-width scaled proportionally 
            dd$x <- dd$x * char_width/diff(range(dd$x))
            dd$y <- dd$y * char_width/(diff(range(dd$y)) * char_scale)
          
            dd$x <- dd$x - min(dd$x) + d$position - char_width/2
            dd$y <- dd$y - min(dd$y) + d$ypos - (char_width/char_scale)/2
         }
        
        cn <- colnames(d)
        cn <- cn[!cn %in% c('x','y', 'ypos')]
        for (nn in cn) {
            dd[[nn]] <- d[[nn]]
        }
        ## dd$name <- d$name
        ## dd$position <- d$position
        dd$group <- paste0("V", d$position, "L", d$ypos)
        #dd$group <- paste0(d$position, d$ypos)
        ## dd$character <- d$character
        ## dd$color <- d$color
        #dd <- dd[order(dd$order),]
        return(dd)
    })

    ydf <- do.call(rbind, yy)
    colnames(ydf)[colnames(ydf) == 'y'] <- 'yy'
    ydf$y <- as.numeric(ydf$name)

    ydf <- cbind(label = ydf$name, ydf)
    return(ydf)
}

##' Convert msa file/object to tidy data frame
##'
##'
##' @title tidy_msa
##' @param msa multiple sequence alignment file or
##' sequence object in DNAStringSet, RNAStringSet, AAStringSet, BStringSet,
##' DNAMultipleAlignment, RNAMultipleAlignment, AAMultipleAlignment, DNAbin or AAbin
##' @param start start position to extract subset of alignment
##' @param end end position to extract subset of alignemnt
##' @return tibble data frame
##' @export
##' @author Guangchuang Yu
tidy_msa <- function(msa, start = NULL, end = NULL) {
    aln <- prepare_msa(msa)
    alnmat <- lapply(seq_along(aln), function(i) {
        base::strsplit(as.character(aln[[i]]), '')[[1]]
    }) %>% do.call('rbind', .)
    ## for DNAbin and AAbin
    ## alnmat <- lapply(seq_along(aln), function(i) as.character(aln[[i]])) %>% do.call('rbind',. )
    alndf <- as.data.frame(alnmat, stringsAsFactors = F)
    
    alndf$name = names(aln)
    
    cn = colnames(alndf)
    cn <- cn[!cn %in% "name"]
    df <- gather(alndf, "position", "character", cn)
    
    y <- df
    y$position = as.numeric(sub("V", "", y$position))
    y$character = toupper(y$character)
     
    y$name = factor(y$name, levels=rev(names(aln)))

    
    if (is.null(start)) start <- min(y$position)
    if (is.null(end)) end <- max(y$position)
    
    y <- y[y$position >=start & y$position <= end, ]

    return(y)
}


##' @importFrom utils globalVariables
utils::globalVariables('.')


msa2tidy <- function(msaData) {
  if ("order" %in% names(msaData)) {
    msaData <- msaData[msaData$order == 1,]
  }
  df_tidy <- data.frame(name = msaData$name, 
                        position = msaData$position, 
                        character = msaData$character)
  return(df_tidy)
}



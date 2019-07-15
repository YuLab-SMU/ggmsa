##' plot multiple sequence alignment using ggplot2
##'
##'
##' @title ggmsa
##' @param fasta aligned fasta file
##' @param start start position to plot
##' @param end end position to plot
##' @param font character font
##' @param color color scheme. Must be one of "Clustal", "Chemistry", "Shapely", "Zappo" or "Taylor"
##' @return ggplot object
##' @importFrom tidyr gather
##' @importFrom treeio read.fasta
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_minimal
##' @importFrom ggplot2 geom_tile
##' @importFrom ggplot2 geom_polygon
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 coord_fixed
##' @importFrom magrittr %>%
##' @export
##' @author guangchuang yu


ggmsa <- function(fasta, start=NULL, end=NULL, font = "helvetica_regular", color = c("Clustal", "Chemistry", "Shapely", "Zappo", "Taylor" )) {
    aln <- read.fasta(fasta)
    alnmat <- lapply(seq_along(aln), function(i) as.character(aln[[i]])) %>% do.call('rbind',. )
    alndf <- as.data.frame(alnmat)

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



    chars <- unique(y$character)

    ## ggseqlogo::list_fonts()

    data_sp = lapply(chars, function(n){
        if (n == '-') {
            d <- data.frame(x = c(0.05, 0.95, 0.95, 0.05),
                            y = c(0.05, 0.05, 0.2, 0.2),
                            letter = '-',
                            position = 1,
                            order = 1:4,
                            seq_group='-')
            return(d)
        }
        d = logo_data(seqs = n,
                      font = font,
                      seq_group = n,
                      seq_type = "auto")

        d$x <- d$x * .9/diff(range(d$x))
        d$y <- d$y * .9/diff(range(d$y))
        return(d)
    })

    names(data_sp) = chars

    y$ypos <- as.numeric(y$name)

    ## todo: update with color scheme
    
    ##color scheme1: Clustal X 
    ##color scheme2: DNASTAR: Color by Chemistry.
    ##color scheme3: DNASTAR: Shapely - This color scheme matches the RasMol amino acid and RasMol nucleotide color schemes.
    ##color scheme4: DNASTAR: Zappo - This scheme colors residues according to their physico-chemical properties, and is also used in JalView.
    ##color scheme5: DNASTAR: Taylor
    
    
    if(!(color %in% c("Clustal", "Chemistry", "Shapely", "Zappo", "Taylor") )){
      stop("The color scheme only have Clustal and Chemistry ....")
    }
    
    col1 = c('#80a0f0', '#80a0f0','#80a0f0', '#80a0f0','#80a0f0', '#80a0f0','#80a0f0',
             '#f01505', '#f01505',
             '#c048c0', '#c048c0',
             '#15c015', '#15c015', '#15c015','#15c015',
             '#f08080',
             '#f09048',
             '#c0c000',
             '#15a4a4','#15a4a4',
             '#ffffff') 
    
    names(col1) = c("A", "I", "L", "M", "F", "W", "V",
                    "K", "R",
                    "E", "D",
                    "N", "Q", "S", "T",
                    "C",
                    "G",
                    "P",
                    "H", "Y",
                    "-")
    
    col2 = c("#ffff66", "#ffff66", "#ffff66",
             "#ff6d6d", "#ff6d6d",
             "#769dcc", "#769dcc", "#769dcc",
             "#f2be3c", "#f2be3c", "#f2be3c", "#f2be3c", "#f2be3c", "#f2be3c", "#f2be3c",
             "#74ce98", "#74ce98", "#74ce98", "#74ce98", "#74ce98",
             "#ffffff")
    names(col2) = c("F", "W","Y",
                    "D", "E",
                    "R", "H", "K",
                    "A", "G", "I", "L","M", "P","V",
                    "C", "N", "Q", "S", "T",
                    "-")
    
    col3 = c("#c8c8c8",
             "#145aff", "#145aff",
             "#00dcdc", "#00dcdc",
             "#e60a0a", "#e60a0a",
             "#e6e600", "#e6e600",
             "#ebebeb",
             "#8282d2",
             "#0f820f", "#0f820f", "#0f820f",
             "#3232aa", "#3232aa",
             "#dc9682",
             "#fa9600", "#fa9600",
             "#b45ab4",
             "#ffffff")
    names(col3)  = c("A",
                     "R", "K",
                     "N", "Q",
                     "D", "E",
                     "C", "M",
                     "G",
                     "H",
                     "I", "L", "V",
                     "F", "Y",
                     "P",
                     "S", "T",
                     "W",
                     "-")
    
    col4 = c("#ff7979", "#ff7979", "#ff7979", "#ff7979", "#ff7979",
             "#f89f56", "#f89f56", "#f89f56",
             "#cc00cc", "#cc00cc",
             "#ffff00",
             "#08c81a", "#08c81a", "#08c81a", "#08c81a",
             "#c00000", "#c00000",
             "#0070c0", "#0070c0", "#0070c0",
             "#ffffff")
    names(col4) = c("A", "I", "L", "M", "V",
                    "F", "W", "Y",
                    "G", "P",
                    "C",
                    "N", "Q", "S", "T",
                    "D", "E",
                    "R", "H", "K",
                    "-")
    
    col5 = c("#ccff00", "#0000ff", "#cc00ff", "#ff0000", "#ffff00", "#ff0066", "#ff00cc", "#ff9900", "#0066ff", "#66ff00", 
             "#66ff00", "#6600ff", "#00ff00", "#00ff66", "#ffcc00", "#ff3300", "#ff6600", "#00ccff", "#00ffcc", "#99ff00",
             "#ffffff")
    names(col5) = c("A", "R", "N", "D", "C", "E", "Q", "G", "H", "I", 
                    "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V",
                    "-")
    
    color_scheme <- list( Clustal = col1, Chemistry = col2, Shapely = col3, Zappo = col4,Taylor = col5 )
    col<- color_scheme[[color]]
    y$color <- col[y$character]

    yy <- lapply(1:nrow(y), function(i) {
        d <- y[i, ]
        dd <- data_sp[[d$character]]
        dd$x <- dd$x - min(dd$x) + d$position -.45
        if (d$character == '-') {
            dd$y <- dd$y - min(dd$y) + d$ypos - 0.1
        } else {
            dd$y <- dd$y - min(dd$y) + d$ypos -.45
        }
        dd$name <- d$name
        dd$position <- d$position
        dd$group <- paste0(d$position, d$ypos)
        dd$character <- d$character
        dd$color <- d$color
        dd <- dd[order(dd$order),]
        return(dd)
    })

    ydf <- do.call(rbind, yy)


    ggplot(ydf, aes_(x=~position, y=~name, fill = ~I(color))) + geom_tile(color='grey') +
        geom_polygon(data=ydf, aes_(x=~x, y=~y, group=~factor(group)), fill='black') +
        theme_minimal() + xlab(NULL) + ylab(NULL) +
        theme(legend.position='none') + coord_fixed()
}

##' @import ggseqlogo
logo_data <- getFromNamespace("logo_data", "ggseqlogo")


##' @importFrom utils globalVariables
utils::globalVariables('.')

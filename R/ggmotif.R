##' plot sequence motif for nucleotide sequences based 'ggolot2'

##' @title ggmotif
##' @param msa Multiple aligned sequence file or object for
##' representing either nucleotide sequences or peptide sequences.
##' @param start Start position to plot, If font=NULL, only the background frame is drawn, and no character.
##' @param end End position to plot,If font=NULL, only the background frame is drawn, and no character.
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'.  Defaults is 'helvetical'.
##' @param color A Color scheme. One of 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Chemistry_NT'.
##' @return ggplot object
##' @examples 
##' #plot sequence motif independently
##' nt_sequence <- system.file("extdata", "LeaderRepeat_All.fa", package = "ggmsa")
##' ggmotif(nt_sequence, color = "Chemistry_NT")
##' @export
##' @author Lang Zhou
ggmotif <- function(msa, start = NULL, end = NULL, font = "helvetical", color = "Chemistry_NT") {
  
    data <- tidy_msa(msa, start = start, end = end)
  
    ggplot() + geom_motif(data, font = font, color = color) + 
        theme_minimal() + xlab(NULL) + ylab(NULL) + 
        theme(legend.position='none') + theme(panel.grid = element_blank(), axis.text.y = element_blank()) + 
        coord_fixed()
}




geom_motif <- function(data, font = "helvetical", color = "Chemistry_NT") {
  
    motif_da <- seq_motif(data, font = font, color = color)
    ly_motif <- geom_polygon(aes_(x = ~x, y = ~y,  group = ~group, fill = ~I(color)),
                             data = motif_da, inherit.aes = FALSE) 
    return(ly_motif)
}



seq_motif <- function(data, font = "helvetical", color = "Chemistry_NT"){

    tidy <- data
    total_heigh <- getOption("total_heigh")
    logo_width <- getOption("logo_width")
  
    col_num <- levels(factor(tidy$position)) # the column number
    moti_da <- lapply(1:length(col_num), function(j){
        clo <- tidy[tidy$position == j, ] ## 计算每列碱基的频率
        fre <- prop.table(table(clo$character))
        ywidth <- sort(total_heigh * fre ) ## 总体高度为4，各字符高度按其频率分配
        font_f <- font_fam[[font]]
        motif_char <- font_f[names(ywidth)] 
        ds_ <- lapply(seq_along(motif_char), function(i){ ## 分配每列motif各字符高度及位置
            ds_ <- motif_char[[i]]
            ds_$x <- ds_$x * logo_width/diff(range(ds_$x)) #x固定为.9
            ds_$y <- ds_$y * ywidth[[i]]/diff(range(ds_$y)) #y根据其频率分配高度
            ymotif <- sum(ywidth[0:(i - 1)]) # 当前字符的下方所有字符所占高度
            ds_$x <- ds_$x - min(ds_$x) - logo_width/2 + j # j 为当前所在列数
            ds_$y <- ds_$y - min(ds_$y) - ywidth[[i]]/2 + ymotif + ywidth[[i]]/2 
            ## ds_$y - min(ds_$y) - ywidth[[i]]/2 以0为中心
            ## + ymotif 加上位于其下方的motif字符的高度
            ## + ywidth[du[i]]/2 再加上自身高度
            ds_$group <- paste0("P", j, "Char", names(motif_char[i]))
            ds_$color <- scheme_NT[names(motif_char[i]), color]
            return(ds_)
        })
        ds <- do.call(rbind, ds_)
        return(ds)
    })
    moti_da <- do.call(rbind, moti_da) 
    return(moti_da)
}



















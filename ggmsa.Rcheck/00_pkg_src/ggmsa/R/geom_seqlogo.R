##' Multiple sequence alignment layer for ggplot2. It plot sequence motifs.

##' @title geom_seqlogo
##' @param font font families, possible values are 'helvetical', 'mono', and 'DroidSansMono', 'TimesNewRoman'. Defaults is 'helvetical'.
##' @param color A Color scheme. One of 'Chemistry_NT', 'Shapely_NT', 'Zappo_NT', 'Taylor_NT'. Defaults is 'Chemistry_NT'.
##' @return A list
##' @examples 
##' #plot multiple sequence alignment and sequence motifs
##' f <- system.file("extdata/LeaderRepeat_All.fa", package="ggmsa")
##' ggmsa(f,font = NULL,color = "Chemistry_NT") + geom_seqlogo()
##' @export
##' @author Lang Zhou
geom_seqlogo <- function(font = "helvetical", color = "Chemistry_NT") {
    structure(list(font = font, 
                   color = color),
              class = "seqlogo")
}


geom_seqlogo1 <- function(tidyData, font = "helvetical", color = "Chemistry_NT") {
    #data <- tidy_msa(msa = msa, start = start, end = end)
    motif_da <- motif(tidyData, font = font, color = color)
    ly_logo <- geom_polygon(aes_(x = ~x, y = ~y,  group = ~group, fill = ~color ),
                            data = motif_da, inherit.aes = FALSE) 
    return(ly_logo)
}


motif <- function(tidyData, font =  "helvetical", color = "Chemistry_NT"){
    tidy <- tidyData
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
            ds_$y <- ds_$y - min(ds_$y) - ywidth[[i]]/2 + ymotif + ywidth[[i]]/2 + nrow(tidy[tidy$position == j, ]) + .5
            ## ds_$y - min(ds_$y) - ywidth[[i]]/2 以0为中心
            ## + ymotif 加上位于其下方的motif字符的高度
            ## + ywidth[du[i]]/2 再加上自身高度
            ## + nrow(tidy[tidy$position == j, ]) + .5 平移到最上方
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


.onAttach <- function(libname, pkgname){
    options(total_heigh = 4)
    options(logo_width = 0.9)
    options(asterisk_width = .03)
    options(GC_pos = 2)
    options(shadingLen = .5)
    options(shading_alpha = .3)
}



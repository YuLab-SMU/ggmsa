


geom_msa <- function(data, ...) {
    mapping <- aes_(x = ~position, y = ~name, fill = ~I(color))
    if ('y' %in% colnames(data)) {
        mapping <- modifyList(mapping, aes_(y = ~y))
    }

    ly_bg <- geom_tile(mapping = mapping, data = data, color = 'grey', inherit.aes = FALSE)


    if (!all(c("yy", "order", "group") %in% colnames(data))) {
        return(ly_bg)
    }

    data <- data[order(data$order),]

    if ('y' %in% colnames(data)) {
        data$yy = data$yy - as.numeric(data$name) + data$y
    }

    ly_label <- geom_polygon(aes_(x = ~x, y = ~yy,  group = ~group ),
                             data = data, inherit.aes = FALSE)
    list(ly_bg, ly_label)
}


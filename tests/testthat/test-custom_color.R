library(ggmsa)

msa <- system.file("extdata", "sample.fasta", package = "ggmsa")
tidymsa <- tidy_msa(msa, 10, 30)

custom <- data.frame(
    names = c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K",
              "M", "F", "P", "S", "T", "W", "Y", "V", "-", "X"),
    color = rep(c("#ff0000", "#00ff00"), 11),
    stringsAsFactors = FALSE
)

test_that("custom_color accepts the documented and historical column spellings", {
    expected <- custom$color[match(tidymsa$character, custom$names)]

    for (col in c("color", "colour", "col", "colors", "colours")) {
        cc <- custom
        names(cc)[names(cc) == "color"] <- col
        y <- msa_data(tidymsa, font = NULL, custom_color = cc)
        expect_identical(y$color, expected, info = col)
    }
})

test_that("custom_color is not confused by unrelated extra columns", {
    # `custom_color$col` used to resolve these by partial matching and
    # silently dropped the color column when the match was ambiguous.
    cc <- cbind(custom, column = seq_len(nrow(custom)))
    y <- msa_data(tidymsa, font = NULL, custom_color = cc)
    expect_false(any(is.na(y$color)))
})

test_that("custom_color with an unusable layout reports a clear error", {
    expect_error(
        msa_data(tidymsa, font = NULL, custom_color = data.frame(names = "A")),
        "`names` column and a `color` column"
    )
    expect_error(
        msa_data(tidymsa, font = NULL,
                 custom_color = data.frame(aa = "A", color = "red")),
        "`names` column and a `color` column"
    )
})

test_that("custom_color is honored under consensus_views (#54, #69)", {
    y <- msa_data(tidymsa, font = NULL, custom_color = custom,
                  consensus_views = TRUE, disagreement = TRUE)

    cons <- get_consensus(y[y$name != "Consensus", ])
    pairs <- data.frame(name = y$name, position = y$position,
                        character = y$character, color = y$color)
    pairs <- pairs[pairs$name != "Consensus", ]
    pairs$cons <- cons$character[match(pairs$position, cons$position)]

    # agreeing cells are blanked to white, disagreeing cells keep the
    # user palette rather than falling back to a built-in scheme
    expect_true(all(pairs$color[pairs$character == pairs$cons] == "#ffffff"))
    expect_true(all(pairs$color[pairs$character != pairs$cons] %in%
                        c("#ff0000", "#00ff00")))
})

test_that("consensus_views keeps working without custom_color", {
    y <- msa_data(tidymsa, font = NULL, color = "Chemistry_AA",
                  consensus_views = TRUE)
    expect_true(is.data.frame(y))
    expect_true("Consensus" %in% as.character(y$name))
})

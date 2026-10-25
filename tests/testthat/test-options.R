test_that("plotting options are set on load without overwriting user values", {
    old <- options(logo_width = NULL, GC_pos = 5)
    on.exit(options(old))
    ggmsa:::.onLoad(NULL, "ggmsa")
    expect_equal(getOption("logo_width"), 0.9)
    expect_equal(getOption("GC_pos"), 5)
})

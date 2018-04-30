```r
library(ggmsa)
f <- system.file("extdata/sample.fasta", package="ggmsa")
dev.new(width=11, height=2)
ggmsa(f, 164, 213)
```

![](inst/extdata/ggmsa.png)



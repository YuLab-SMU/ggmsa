```r
library(ggmsa)
f <- system.file("extdata/sample.fasta", package="ggmsa")
ggmsa(f, 164, 213, color="Chemistry_AA")
```

![](https://user-images.githubusercontent.com/626539/64681939-2e263080-d4b3-11e9-8409-c160cfc28e26.png)


### TODO

color schemes

+ [x] <http://www.jalview.org/help/html/colourSchemes/clustal.html> 
+ [x] <https://www.dnastar.com/manuals/MegAlignPro/15.2/en/topic/selecting-color-schemes>
+ ...

fonts

+ <https://github.com/yixuan/fontr>

ggtree support

+ re-write `ggtree::msaplot`
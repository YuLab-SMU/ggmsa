# ggmsa 本地开发说明

面向在 `~/data/source/ggtree_family/ggmsa`（upstream `YuLab-SMU/ggmsa`，分支 `devel`）上工作的 agent。
§4 是包能力与模块地图，用来省一遍全仓读；其余只记录读代码看不出来的东西：环境搭法、坑、验证手段、修复进度。

## 1. 跑起来

包本身没有装在库里，用 `pkgload` 直接加载源码：

```bash
cd ~/data/source/ggtree_family/ggmsa
Rscript -e 'pkgload::load_all("."); ...'
```

`DESCRIPTION` 里 `R4RNA` 和 `seqmagick` 是硬 Imports，缺任何一个 `load_all()` 都会直接失败：

- `R4RNA`：CRAN 有，国内用清华源装 —— `install.packages("R4RNA", repos="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")`
- `seqmagick`：用同级的本地目录装 —— `install.packages("~/data/source/ggtree_family/seqmagick", repos=NULL, type="source")`

本机基线：R 4.6.1、ggplot2 4.0.3、tidyr 1.3.2、testthat 3.3.2。`msa`、`seqinr`、`MultipleAlignment` 没装，
所以 `tests/testthat/test-tidy_msa.R` 里有 1 个 SKIP，属正常，不算回归。

## 2. 跑测试

```bash
Rscript -e 'suppressMessages(pkgload::load_all(".")); testthat::test_dir("tests/testthat", stop_on_failure=FALSE)'
```

`tests/testthat/*` 可以直接调用未导出的内部函数（`msa_data()`、`get_consensus()` 等），不用 `:::`。

复现/探查脚本统一写到仓库根的 `local-repro.R`（临时物，不要提交、不要为它写文档），
每个 issue 修完把对应断言固化进 `tests/testthat/`。

## 3. 两个必须知道的坑

**行尾混用。** 仓库以 LF 为主，但少数文件是 CRLF —— 本次改到的 `R/geom_msa.R`、`R/msa_data.R`、
`R/methods-ggplot_add.R`、`R/color_else.R` 就是 CRLF，而 `R/facet_msa.R`、`NAMESPACE`、
`tests/testthat/` 是 LF。规则是**跟随所在文件原有风格**：Edit/Write 会自动保持，
`sed`/`perl` 不会（在 CRLF 文件上 `$` 锚点匹配不到行尾，要先处理 `\r`）。
新建文件跟同目录邻居一致（`tests/testthat/` 用 LF）。

**不要跑 `roxygen2::roxygenise()`。** 仓库是 roxygen2 8.0.0 生成的，本机是 8.1.0，
重新生成会把 `NAMESPACE` 里所有 `importFrom(pkg, a, b, c)` 摊成多行，`man/` 全量重写，
产出几百行与改动无关的 diff。新增 S3method 或 export 时**手改 `NAMESPACE`**，位置按 roxygen 的字母序插入。

## 4. 包能力与模块地图

33 个 R 文件、3220 行、30 个函数导出 + `plot`/`show` 两个 S4 方法（另有 `R/sysdata.rda` 存字形与色板）。
这节只回答"有什么、在哪"；图层之间怎么传数据在 §5，改图层前先看 §5。

### 4.1 对外能力

**主图**：`ggmsa()` 是一行式入口，本质就是 `tidy_msa()` → `geom_msa()` → `theme_msa()` 的薄封装，
18 个参数全在 `R/ggmsa.R:79`（`start`/`end` 在 `tidy_msa()` 一侧裁列）。要手工搭（塞进 patchwork、
加自己的 geom）就直接 `ggplot() + geom_msa(tidy_msa(...))` —— `geom_msa()` 还多出
`dms`、`position_color`、`mapping`、`position` 四个 `ggmsa()` 没暴露的参数。

**输入**：fasta 路径、`DNA/RNA/AAStringSet`、`BStringSet`、
`MultipleAlignment` 的 `DNA/RNA/AAMultipleAlignment`、ape 的 `DNAbin/AAbin`。
白名单是 `R/prepare_fasta.R:54` 的 `supported_msa_class`（9 个 class），不在表里的直接
`stop("multiple sequence alignment object no supported...")` —— **`msa` 包的 `msa` 对象不在白名单内**。
NT / AA 共用一套代码，靠 scheme 名字里含不含 `NT` 分流（`R/color_else.R:19`）。
另一条隐式通路：`tidy_msa()` 见到**长度 > 1 的字符向量**会跳过 `prepare_msa()`，
当成已经比好的序列逐条拆开（`R/msa_data.R:206`），所以等长字符串向量可以直接喂；
但它必须有 `names()`，否则唯一名检查会报 `Sequences must have unique names`。

**染色与视图**：12 个可选 scheme —— `scheme_NT` 4 套（`Chemistry/Shapely/Taylor/Zappo_NT`）、
`scheme_AA` 7 套（同上 4 个 + `LETTER`/`CN6`/`Hydrophobicity`）、外加 `Clustal`
（走 `color_Clustal()` + `scheme_clustal`，不在 `scheme_AA` 里）。`available_colors()` 现读列名，
所以色板加列它就跟着变。另有 `custom_color` 自定义表；`by_conservation` 用保守度控制明度；
`consensus_views` 共识视图（配 `use_dot` / `disagreement` / `ignore_gaps` / `ref`）；
`dms` 把逐位结合活性（`assign_dms()` 合过来的 `bind_avg`）当连续 fill。
`font = NULL` 退化成纯 tile 图。

**字形**：`helvetical` / `mono` / `TimesNewRoman` / `DroidSansMono` 四套。轮廓是预先转好的多边形，
存在 `R/sysdata.rda` 的 `font_fam` 里，**跟系统字体无关**，也不需要装字体。

**叠加图层**：`facet_msa(field)` 横向切图、`geom_msaBar()` 底部保守度柱、
`geom_seqlogo()`（独立版 `seqlogo()`）序列 logo、`geom_seed()` miRNA seed
（`star = TRUE` 走内部 `geom_asterisk()` 画星号）、`geom_GC()` GC 含量、
`geom_helix()` / 独立版 `gghelix()` RNA 二级结构弧。

**独立图形**（另起炉灶，不经过 `msa_data()`）：`ggSeqBundle()` 序列束、`simplot()` 相似度图、
`seqdiff()` 重组检测（包内唯一 S4，配 `plot()` / `diff()` / `show()`）、
`ggmaf()` + `read_maf()` + `tidy_maf_df()` 基因组 MAF、
`treeMSA_plot()` / `adjust_ally()` / `extract_seq()` 树 + 祖先序列、
`merge_seq()` / `tidy_hdata()` / `simplify_hdata()` / `reset_pos()` 互作 gap 处理。

**内置数据**：`inst/extdata/` 10 项 —— 7 个 fasta + `TP53_genes.xlsx` + `GVariation/`、`Rfam/` 两个目录。
`R/data.R` 只是给它们写文档页（`@docType data` + `NULL`），包里并没有真正的 dataset。

**文档**：`vignettes/ggmsa.Rmd`（"ggmsa-Getting Started"）按"装包 → 导入 → 基础画法 → 标注 →
组合"的顺序覆盖了上面几乎所有能力，是公共 API 最大的隐性消费者，动签名记得同步它。
`man/` 下全是 roxygen 产物，不要本地重新生成（§3）。

### 4.2 模块地图（按调用层次排，不是字母序）

```
输入（fasta 路径 / S4·S3 对象 / 等长字符向量）
  ├─ prepare_msa() ──> BStringSet ─┐
  └─（字符向量 length > 1，绕过）──┴─> tidy_msa()  ──> 一格一行 name / position / character
                                       └─> msa_data() ──> 上色 + 字形展开成多边形顶点
                                             └─> geom_msa() ──> layer list ──> ggplot
                                                                     │
                                        ggplot_add.<class>()  叠加图层 / 分面 / 组合成面板栈
```

| 层 | 文件（行数） | 职责 | 关键内部件 |
| --- | --- | --- | --- |
| 解析 | `prepare_fasta.R` (65) | 各输入 → `BStringSet` | `prepare_msa()`、`supported_msa_class` |
| 解析 | `msa_data.R` (270，全包最大) | 规范表 / 上色 / 字形展开 / 折回 | `msa_data()`、`msa2tidy()`、`tile_granularity()` |
| 配色 | `color_else.R` (52) | scheme 分派、`custom_color` 归一 | `color_scheme()`、`normalize_custom_color()` |
| 配色 | `clustal.R` (41)、`color_by_conservation.R` (28) | Clustal 规则、保守度明度 | `color_Clustal()`、`color_visibility()` |
| 配色 | `cons.R` (112) | 共识序列、`disagreement`/`ref` | `get_consensus()`、`tidy_color()`、`order_name()` |
| 主图 | `geom_msa.R` (196) | 组 layer list，见 §5 | —— |
| 主图 | `theme_msa.R` (71) | MSA / bar / facet 三套 theme 与刻度 | `bar_theme()`、`facet_scale()` |
| 主图 | `ggmsa.R` (123) | 薄封装 | —— |
| 叠加 | `methods-ggplot_add.R` (229) | **组合协议的心脏**，7 个 `ggplot_add.*` | `msa_source_data()`、`msa_panel()`、`msa_add()`、`msa_source()` |
| 叠加 | `facet_msa.R` (46) | `position` → `PANEL` | `facet_data()` |
| 叠加 | `geom_msaBar.R` (54)、`seqlogo.R` (187)、`geom_seed.R` (76)、`geom_asterisk.R` (87)、`geom_GC.R` (66) | 各叠加图层的 `ly_*` 构造器 | `ly_bar()`、`geom_logo()`、`geom_seed1()`、`geom_GC1()` |
| 图形 | `arc.R` (248，第二长) | RNA 二级结构弧，自带栈 | `tidy_helix()`、`layer_helix()`、`theme_helix()` |
| 图形 | `ggmaf.R` (171) + `read_maf.R` (61) | 基因组 MAF，自己的 facet | `maf_plot()`、`facet_maf()`、`order_aln()` |
| 图形 | `ancestor_seq.R` (146) + `method-plot.R` (107) | 树 + 祖先序列；`seqdiff` 的 plot | `adjust_treey()`、`plot_difference()` |
| 图形 | `seqdiff.R` (77) + `AllClasses.R` (8) + `method-show.R` (29) + `methods-diff.R` (7) | 重组检测 S4 | `nucleotide_difference()` |
| 图形 | `SeqBundles.R` (136)、`simplot.R` (120)、`pp_interactive.R` (139)、`dms.R` (32) | 序列束 / 相似度 / 互作 gap / 结合活性 | `adjustMSA()`、`make_gap()` |
| 支撑 | `available.R` (59)、`data.R` (148)、`sysdata.rda`、`import-functions.R` (9)、`zzz.R` (20) | 查询、数据文档、字形与色板资源、`globalVariables()`、`.onAttach` | `font_fam`、`scheme_AA`、`scheme_NT`、`scheme_clustal`、`star` |

支撑层两处容易忘：`import-functions.R` 里的 `globalVariables()` 是消 `R CMD check` NOTE 的，
新代码用 NSE 引用裸列名要顺手在这登记；`R/sysdata.rda` **仓里没有生成脚本**（没有 `data-raw/`），
改它只能自己 `load()` 出来改完再 `use_data(..., internal = TRUE)` 写回。

### 4.3 三种扩展各自要动哪里

- **加一套配色**：往 `sysdata.rda` 的 `scheme_AA` / `scheme_NT` 加一列（行名是字符），
  再把名字加进 `R/msa_data.R:51` 的 `match.arg` 白名单 —— 漏第二步会在 `match.arg` 直接报错。
  `available_colors()` 不用改，它是现读 `names(scheme_*)`。
- **加一套字体**：`font_fam` 里加一个"字符 → 多边形顶点"的元素。取用方式是
  `font_f[[font]]` 之后 `font_f[as.character(unique(y$character))]`（`R/msa_data.R:95`），
  比对里出现表里没有的字符会静默变 NA。`available_fonts()` 自动生效。
- **加一个叠加图层**：`structure(list(...), class = "myThing")` + `ggplot_add.myThing(object, plot, ...)`
  放 `methods-ggplot_add.R`。拿对齐数据**必须**走 `msa_source_data(plot)`，
  往图上叠 geom **必须**走 `msa_add(plot, ly, ...)` —— 直接 `ggplot_add()` 到栈上，
  在 `geom_msaBar()` 之后会把图层丢到 bar 面板（见 §5）。
  三种都涉及 `NAMESPACE` 的话一律手改，理由见 §3。

## 5. 数据流架构

组合图层的关键约定，改任何一层前先看这段：

- `geom_msa()` **不返回 Layer，返回一个 list**（`ly_bg` / `ly_label` / `ly_hl` / `scale_fill_manual` / `msa_source`）。
  `ggplot() + <list>` 会逐项 add，所以 list 里的顺序就是 ggplot 的图层顺序，也就是绘制顺序：
  背景 tile 必须在字形 polygon **之前**，否则白底多边形会把高亮框盖掉。
- 辅助图层 `facet_msa` / `geom_msaBar` / `geom_seqlogo` / `geom_seed` / `geom_GC` /
  `geom_helix` 都不是普通 geom，而是 `structure(list(...), class=...)` + 对应的
  `ggplot_add.<class>` 方法，在里面现场造 geom 再叠加。
- **`ggplot_add.msaBar` 返回 patchwork，不再返回 `<aplot>`**（#49）。`<aplot>` 在 ggplot2 4.x 下
  是个"死"对象：它的 class 只有 `aplot/ggplot`，`+.gg` 要求 `is_ggplot()`（S7 判定）才认，
  于是 `p + theme()`/`p + labs()` 这类**静默返回 NULL**，`p + facet_msa()` 因为 `facet_msa` 的
  class 里没有 `gg`，直接落到 primitive `+` 报 `non-numeric argument to binary operator`。
  现在未分面走 `insert_top()` + `aplot::as.patchwork()`（与原 aplot 渲染**逐像素相同**），
  已分面走 `patchwork::wrap_plots()` —— 因为 `insert_top()` 组合多面板图时只保留第 1 个 panel。
- 于是下游图层拿到的 `plot` 可能是一个**面板栈**：`msa_panel()` 找出哪个 panel 持有对齐数据，
  `msa_add()` 把 seqlogo / seed / GC / helix 塞进**那个 panel**（直接 `ggplot_add` 到栈上会落到
  最后一个 panel，也就是 bar 面板）。`ggplot_add.facet_msa` 对栈里每个持有 `position` 列的 panel
  分别分面，所以 bar 和对齐用同一个 `field` 切（#49、#72）。
- 这些消费者需要的对齐数据来自 **`plot$ggmsa_data`**（由 `geom_msa()` 末尾的 `msa_source()` 挂上），
  不要再回到 `plot$layers[[1]]$data` —— 那份数据会被 `position_highlight` 裁剪、被 `font` 展开、
  被 `facet_msa` 改写，#75/#76 就是这么来的。`msa_source_data()` 的顺序是
  `plot$ggmsa_data` → 面板栈里那个 panel 的 `ggmsa_data` → `layers[[1]]$data` 兜底
  （兼容手工用别的 geom 搭 MSA 图，以及 bar 面板自己）。
- msaBar 的数据列叫 **`position`**（原先叫 `pos`；`facet_data()` 只认 `position`，#49 起统一）。
  `ggplot_add.facet_msa` 会把宽度存进 `plot$ggmsa_field`，"先 facet 再 msaBar"的顺序靠它对齐（#72）。
- **数据粒度**：`msa_data()` 在 `font != NULL` 时把每个字符格子展开成字形多边形顶点
  （`sample.fasta` 132 个格子 → 21249 行）。要回到"一格一行"用 `msa2tidy()`，它靠 `order == 1` 去重。
  展开时除 `x`/`y` 外的每一列都是从那一格原行**逐顶点复制**的，所以同一 `(name, position)` 内取值恒定 ——
  这是 `tile_granularity()`（`R/msa_data.R`）能安全去重的前提，背景 tile 和高亮框都走它（#74 已修）；
  字形 polygon 层仍然吃展开后的全量数据。
- **`position_highlight` 会强制 `none_bg = TRUE`**，即"只显示字形 + 高亮框"。高亮框是**独立的
  `geom_tile` 图层**，数据是裁剪过的子集；主体图层（`ly_bg` / `ly_label`）永远保持完整对齐。
  `font = NULL` 且带高亮时，第 1 层是一个撑坐标范围的 `geom_blank`（ggplot2 里 `StatNull` 的
  `compute_range` 会参与 panel range），只绘制被点名的列 —— 不要退回"裁剪 bg 层"的写法，
  那正是 #75/#76 的根因。图层顺序仍然决定遮挡：tile 必须在 `geom_polygon` 字形**之前**。

## 6. 复现素材

`inst/extdata/`：

| 文件 | 用途 | 规模 |
| --- | --- | --- |
| `seedSample.fa` | #75/#76 的 reprex 用它（`color="Chemistry_NT"`） | 22 个 position |
| `sample.fasta` | 大多数文档示例（`protein_sequences`） | 9 条序列，~500 position |
| `LeaderRepeat_All.fa` | #49 的 reprex | 较长，配合 `facet_msa(field=120)` |

`geom_msaBar()` 现在返回 patchwork：面板用 `p[[i]]` 取（`length(p)` == 2），
`ggplot_build(p)` 对栈本身拿不到 per-panel layout，要 `ggplot_build(p[[i]])` 逐个 build。
顺手记两个上游事实：`aplot::insert_top()` 的返回值 class 只有 `aplot/ggplot`（对它 `+` 任何东西
都是死的，见 §5）；`aplot::as.patchwork()` 可以把它转成 patchwork 而**不改变渲染结果**。

## 7. 验证手段

- 图例是否真的出来：`ggplot2::get_guide_data(p, "fill")`，NULL 就是没有。
  比查 gtable 里的 `guide-box-*` 可靠 —— 那些 grob 在图例为空时也存在。
  注意它需要较新的 ggplot2，测试文件里已加 `skip_if_not` 守卫。
- tile 取色有没有被改坏：`ggplot_build(p)$data[[1]]$fill`，跟改前的值做
  `identical(as.character(a), as.character(b))`（`I()` 会带 `AsIs` 类，直接 `identical` 会假阳性失败）。
- 图层是否被裁剪：遍历 `p$layers`，比较各层 `data` 的 `nrow` 和 `length(unique(position))`。
- **改动会不会让画面变样**：`ggsave()` 到 PNG 后用 `png::readPNG` 算墨水量
  （`mean(luminance < 0.97)`）。两个本该不同的配置如果墨水量一模一样，说明改动把效果抹平了
  （#75 修到一半时就出现过：高亮列和普通格子渲染完全一致）。`grDevices` 没有 `readBitmap`，别用。
- 组合场景回归清单见 `tests/testthat/test-position-highlight.R`，覆盖
  seqlogo / seed / GC / msaBar / none_bg / consensus / use_dot / seq_name / border / facet
  与 highlight 的两两组合，改数据流相关代码后至少跑一遍它。
- **导出臃肿看对象数，别看字节**：`ggsave(..., device = svg)` 再数 `<path`。
  重复 tile 在 PDF 里只是一串 `re`/`f`，压缩后体积只掉 3%，但 Illustrator 继承的是对象数量
  （#74：3600 格的比对 392277 个 path → 7260 个）。
- **A/B 新旧行为不用回滚代码**：`assignInNamespace("tile_granularity", function(x) x, "ggmsa")`
  把内部 helper 临时换成恒等函数即可。注意必须在**每种模式下各自重新构造 plot** ——
  layer data 在 `geom_msa()` 调用时就求值完了，复用同一个 `p` 两种模式量出来是一样的（踩过）。
- 去掉重复绘制后**像素不可能逐位相同**：同一格描边重复画 N 次会累积抗锯齿，格子边框会偏重一点。
  用 `mean(abs(a-b) > 0.004)` 定位差异像素，再确认它们全落在描边区域里
  （纯 ggplot2 复现：同数据画 1 次 vs 99 次，差异像素 100% 落在 stroke 区域，墨水量不变）。
- **组合对象还能不能 `+`**：`is.null(p + theme_minimal())` 一行见血。ggplot2 4.x 的 `+.gg`
  在 e1 既不是 theme 又不是 S7 ggplot 时**静默返回 NULL**（函数体没有 else），
  所以任何基于 `<aplot>` 的链式调用都是"没报错但图没了"。
- **只换容器不改画法**时，A/B 的正确标准是逐像素相同：`insert_top()` 与
  `insert_top() %>% as.patchwork()` 渲染 403200 个像素 0 差异，这种改动不需要看墨水量。
- **patchwork 1.3.2 已经没有轴对齐能力**（`align_plots()` 废弃后只剩 `align_patches()` 设尺寸，
  且对 patchwork 报 "Getting dimensions on patchworks are currently unsupported"）。
  所以拿 `wrap_plots()` 替代 `insert_top()` 必须**看图确认 bar 的列和比对格子对得上**，
  光看"没报错"不够；分面后两侧结构相同才碰巧对齐，未分面时 bar 会整体右移 ~77px。

## 8. Issue 修复进度

对照 `../ggmsa-issues.md`。已修：

- **#21/#54/#69** `R/color_else.R` + `R/msa_data.R`：`custom_color$col` 原先靠 `$` 前缀部分匹配取列，
  多一个 `column` 之类的列就静默变 NULL 并删掉 color 列；改成显式候选名匹配 + 明确报错。
  同时 `consensus_views = TRUE` 分支以前根本没把 `custom_color` 传进 `color_scheme()`，自定义配色被吞掉。
- **#68** `R/geom_msa.R`：`fill = ~I(color)` 让 `scale_fill_manual` 训练不到 level，图例整个消失。
  改成 `fill = ~color`；tile 取色逐点不变，只差 `AsIs` 属性。
- **#75/#76** `R/geom_msa.R` + `R/facet_msa.R` + `R/methods-ggplot_add.R`：高亮拆成独立图层，
  源数据不再被裁剪；`facet_msa` 改成遍历所有图层各自分面（统一 `origin`），不再按位置硬塞
  `layers[[1]]`/`layers[[2]]`。
- **#74** `R/msa_data.R`（新增内部 `tile_granularity()`）+ `R/geom_msa.R`（`bg_data` 经它取）：
  背景 tile 和高亮框按格子去重，一格只画一次。3600 格的比对：tile 行数 388617 → 3600、
  SVG `<path>` 对象 392277 → 7260（Illustrator 卡的就是这个数），PDF 字节只降 3%；
  画面差异只在描边的抗锯齿像素上，墨水量不变。
- **#49/#72** `R/methods-ggplot_add.R` + `R/geom_msaBar.R`：`ggplot_add.msaBar` 不再返回 `<aplot>`，
  未分面时 `insert_top() %>% aplot::as.patchwork()`（与原渲染逐像素相同，但 `+` 恢复了作用），
  已分面时改用 `patchwork::wrap_plots(main, bar, ncol=1, heights=c(3,1))`（`insert_top` 组合
  多面板图只会保留第 1 个 panel，这是 #72 里 "msaBar 不分面" 之外另一半真相）。
  `ggplot_add.facet_msa` 遇到面板栈时逐个 panel 分面，bar 因此和对齐用同一个 `field`；
  `ggplot_add.msaBar` 反过来读 `plot$ggmsa_field`，两种顺序结果一致。
  seqlogo / seed / GC / helix 经 `msa_add()` 落到持有对齐数据的那个 panel。
  代价：`patchwork` 进 Imports，msaBar 的产物 class 从 `aplot` 变 `patchwork`，
  bar 的数据列 `pos` 改名 `position`。
- 回归测试：`test-custom_color.R`、`test-legend.R`、`test-position-highlight.R`、
  `test-tile-granularity.R`、`test-msa-bar-composition.R`。

未修，已知还坏的：

- **#49/#72 的"别再试这几条"**（都实测过）：
  ① 给 aplot 对象补回 S7 类 → `Can't find property <aplot>@scales`；
  ② ggmsa 自己注册 `S3method("+", aplot)` → group generic 同时命中 `+.aplot`（来自 e1）和
  `+.gg`（任何 ggplot2 原生 RHS 的 class 里带 `gg`），报 `Incompatible methods` 并退回内置 `+`，
  `msaBar + scale_x_continuous()/coord_cartesian()/geom_blank()/facet_wrap()` 全炸，
  方法体里显式委派给 `+.gg` 也救不回来（冲突在进方法体前就判定了）；
  ③ 给 RHS 对象补 `"gg"` 类（`class(facet_msa(20)) <- c("facet_msa","gg")`）→ `+.gg` 的
  `is_ggplot(e1)` 判 FALSE，函数体没有 else，**静默返回 NULL**；
  ④ 未分面直接上 `patchwork::wrap_plots(bar, main)` → bar 整体右移约 77px，列对不上
  （patchwork 1.3.2 的 `align_plots()` 已废弃且对 patchwork 报错，没有轴对齐能力）。
  顺带：全包只有 `ggplot_add.msaBar` 用到 `insert_top`。
- **`dms = TRUE` 单独用会炸**（`ggplot_build()` 阶段）：`Continuous value supplied to a discrete scale`。
  `sacle_tile_cols` 是无条件挂上去的，而 dms 分支把 fill 换成了连续的 `bind_avg`；
  调用方自己再补一条连续 scale 才能用。跟 #68 同源，smoke 出来的新账，#74 前后一模一样。
- **`+ coord_fixed()` 写在 `+ facet_msa()` 后面会废**：警告/报错
  `facet_wrap() can't use free scales with coord_cartesian() with a fixed ratio argument`，
  出来的 tile 不是方形；`coord_fixed()` 写在 `facet_msa()` **前面**则一切正常。
  这大概率就是 #72 "tile 非方形" 的真实形态 —— 修 #72 前先确认这条。
- 顺手记：`geom_helix()` 必须给 `helix_data` 参数，没数据直接调会 `argument "helix_data" is missing`。
- `color = "Clustal"` 时图例 label 很怪（一个颜色对应一长串字符，且有重复 key）。以前图例根本不显示所以没暴露，
  #68 修完才看得见。属既有问题，暂未处理。
- `consensus_views = TRUE` + `font = NULL` + `show.legend = TRUE` 不会触发
  `stop("legends catn't be shown in the consensus view!")` —— 那个 guard 在 `font != NULL` 的代码路径里。
- 大面积 `aes_()` / `tidyr::gather()` 待现代化（#64 及 deprecation 噪音）。

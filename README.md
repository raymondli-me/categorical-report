# categoricalReport

GPL-3 companion to the MIT [`categorical`](https://github.com/raymondli-me/categorical)
package: **one-line, natively editable APA Word (`.docx`) tables** and
**Word/PowerPoint-editable figures**.

GPL-3 because it renders through `flextable` and `devEMF`/`rvg` (GPL). `categorical`
itself stays MIT — this is kept separate so the copyleft lives only here.

## Install

```r
# install.packages("remotes")
remotes::install_github("raymondli-me/categorical-report")
```

## APA table (one line -> editable Word table)

```r
library(categorical); library(categoricalReport)

tt <- as.data.frame(Titanic)
df <- tt[rep(seq_len(nrow(tt)), tt$Freq), c("Class","Sex","Age")]
df$Survived <- tt$Survived[rep(seq_len(nrow(tt)), tt$Freq)]
fit <- mca_run(df, active = c("Class","Sex","Age"), group = "Survived", k = 3)

apa_table(mca_top_cluster(fit),
          file   = "Table1.docx",
          number = "Table 1",
          title  = "Characteristic Categories by Cluster",
          note   = "v-test > 1.96 indicates over-representation.",
          cols   = c("cluster","category","vtest","pct_of_cluster"),   # filter columns
          headers= c(vtest = "v", pct_of_cluster = "% of cluster"),    # rename headers
          digits = 2)
```

Produces a genuine Word table (three APA rules, Times 11, right-aligned numbers),
bold **Table 1**, italic title, and an italic "*Note.*" footer — all editable in Word.

## Editable figure (Word / PowerPoint)

```r
save_editable(plot_map(fit, c(1,2), bw = TRUE, legend = FALSE), "map.emf")   # -> Word
# In Word: Insert > Picture, then right-click > Ungroup to edit every label / axis / legend.
save_editable(plot_map(fit, c(1,2)), "map.pptx", format = "pptx")            # -> PowerPoint (rvg)
```

## License

GPL-3. `flextable`, `gdtools`, `devEMF`, `rvg` are GPL; `officer` is MIT. `categorical` is MIT and separate.

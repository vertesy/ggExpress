require(ggpubr)
data("ToothGrowth")
df <- ToothGrowth

# Basic plot
# +++++++++++++++++++++++++++
# width: change box plots width
ggboxplot(df, x = "dose", y = "len", color = "dose", width = 0.8)


list_or_df = list("A"=rnorm(100),"B"=rnorm(100))
MyList = list(rnorm(100),rnorm(100))


library (plyr)
df <- ldply (MyList, data.frame)
# ------------------------------------------------------------------------------------------------
qboxplot <- function(list_or_df, ext = "pdf", xlab = F, plot = TRUE, save = TRUE, mdlink = TRUE
                     , w = 5, h = w, ...) {
  plotname <- as.character(substitute(vec))
  if (is.list(list_or_df) ) { list_or_df <- enframe(list_or_df) %>% unnest() }

  if (isFALSE(xlab)) xlab = plotname

  splitby <- colnames(list_or_df)[1]
  value <- colnames(list_or_df)[2]


  p <- ggboxplot(data = list_or_df, x = value, y = splitby)

  p <- ggdensity(data = df, x = "value" # , y = "..count.."
                 , title = plotname, xlab = xlab
                 , add = "median", rug = TRUE
                 , color = "names", fill = "names"
                 , palette = 'jco', ...
  ) +
    if (length(unique(df$"names")) == 1) theme(legend.position = "none")
  fname = kpp(plotname, "dens",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}

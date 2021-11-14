# qboxplot.qviolin.R

require(ggpubr)

data("ToothGrowth"); ToothLen.by.Dose <- ToothGrowth[ ,c('dose', 'len')]; qboxplot(ToothLen.by.Dose)

df <- ToothGrowth

# Basic plot
# +++++++++++++++++++++++++++
# width: change box plots width
ggboxplot(df, x = "dose", y = "len", color = "dose", width = 0.8)


list_or_df = list("A"=rnorm(100),"B"=rnorm(100))
MyList = list(rnorm(100),rnorm(100))


library (plyr)
df <- ldply (MyList, data.frame)

x <- ggboxplot(data = combined.obj@meta.data, "is.Stressed", 'percent.mito', fill = "is.Stressed", outlier.shape = NA, ylim = c(0, 0.06))
x + stat_compare_means(method = "wilcox.test", label.y.npc = 0, label.x = .5)



# _________________________________________________________________________________________________
#' @title qboxplot plot
#'
#' @param tbl_X_Y_Col_etc tbl_X_Y_Col_etc
#' @param suffix A suffix added to the filename. NULL by default.
#' @param title The name of the file and title of the plot.
#' @param col Color of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param stat.test Do a statistical test?
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @import ggpubr
#' @export
#' @examples data("ToothGrowth"); ToothLen.by.Dose <- ToothGrowth[ ,c('dose', 'len')]; qboxplot(ToothLen.by.Dose)

# @param logX Make X axis log10-scale.
# @param plotname The name of the file and title of the plot.


qboxplot <- function(tbl_X_Y_Col_etc
                     , suffix = NULL
                     # , plotname = qqqParsePlotname(tbl_X_Y_Col_etc, suffix)
                     , outlier.shape = NULL
                     , title = F
                     , stat.test = T
                     # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
                     , stat.method = NULL, stat.label.y.npc = "top", stat.label.x = NULL
                     # , fill = c(NULL , 3)[1]
                     , ext = "png", also.pdf = T
                     , logY = F #, logX = F
                     , hline = F, vline = F, plot = TRUE, save = TRUE, mdlink = FALSE
                     , w = 7, h = w, ...) {
  plotname <- if (isFALSE(title)) Stringendo::kpp(make.names(as.character(substitute(tbl_X_Y_Col_etc))), suffix) else title
  vars <- colnames(tbl_X_Y_Col_etc)
  df <- tbl_X_Y_Col_etc
  nrCategories.DFcol1 <- length(table(tbl_X_Y_Col_etc[,1]))
  stopif(nrCategories.DFcol1>  100)
  p <- ggpubr::ggboxplot(data = df, x = vars[1], y = vars[2], fill = vars[1]
                         # , fill = fill
                         , outlier.shape = outlier.shape
                         , title = plotname, ...) +
    ggpubr::grids(axis = 'y')
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)
  # if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)



  fname = Stringendo::kpp(plotname, suffix, "boxplot", Stringendo::flag.nameiftrue(logY), ext) # , Stringendo::flag.nameiftrue(logX)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}
# dfm.is.Stressed.percent.mito <- combined.obj@meta.data[,c("is.Stressed", "percent.mito")]
# dfm.is.Stressed.percent.mito <- combined.obj@meta.data[,c("is.Stressed", "percent.mito")]

qboxplot(dfm.is.Stressed.percent.mito, outlier.shape = NA, ylim = c(0, 0.06)
         , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5)




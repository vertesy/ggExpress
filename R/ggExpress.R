######################################################################
# ggExpress is the fastest way to create, annotate and export plots in R.
######################################################################
# try(source("~/GitHub/Packages/ggExpress/R/ggExpress.functions.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpress/main/ggExpress.functions.R"), silent = T)

# require(tidyverse)
# require(ggpubr)
# require(cowplot)
# require(MarkdownReports) # https://github.com/vertesy/MarkdownReports


######################################################################
# Auxiliary functions for ggExpress
######################################################################
# try(source("~/GitHub/Packages/ggExpress/R/ggExpress.auxiliary.functions.R"))


######################################################################
# Original functions
######################################################################

# _________________________________________________________________________________________________
#' @title Histogram
#'
#' @param vec The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param xlab X-axis label.
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param suffix A suffix added to the filename. NULL by default.
#' @param plotname The name of the file and title of the plot.
#' @param vline Draw a vertical line on the plot.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by -1 or 1. Takes effect if "*line" is defined.
#' @param palette_use GGpubr Color palette to use.
#' @param col Color of the plot.
#' @param logX Make X axis log10-scale.
#' @param logY Make Y axis log10-scale.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @export
#'
#' @examples weight <- rnorm(1000); qhistogram(vec = weight); qhistogram(vec = weight, vline = 2, filtercol = -1)


qhistogram <- function(vec, ext = "pdf", xlab = F, plot = TRUE, save = TRUE, mdlink = TRUE
                       , suffix = NULL
                       , plotname = qqqParsePlotname(vec, suffix)
                       , logX = F, logY = F
                       , vline = F, filtercol = 0
                       , palette_use = 'jco', col = as.character(1:3)[1]
                       , max.names = 50
                       , w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = max.names)

  df[["colour"]] <- if (vline & filtercol != 0) {
    if (filtercol == 1 ) (df$"value" > vline) else if (filtercol == -1 ) (df$"value" < vline)
  } else if (length(col) == length(vec)) {
    as.character(col)
  } else {
    as.character(rep(col, length(vec))[1:length(vec)])
  }

  p <- ggpubr::gghistogram(data = df, x = "value"
                           , title = plotname, xlab = xlab
                           , add = "median"
                           # , color = "names", fill = "names"
                           , color = 'colour', fill = 'colour'
                           , palette = 'jco', ...
  ) +
    if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")
  if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)
  fname = Stringendo::kpp(plotname, suffix, "hist", Stringendo::flag.nameiftrue(logX), Stringendo::flag.nameiftrue(logY), ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}




# _________________________________________________________________________________________________
#' @title Density plot
#'
#' @param vec The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param xlab X-axis label.
#' @param plot Display the plot.
#' @param suffix A suffix added to the filename. NULL by default.
#' @param plotname The name of the file and title of the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param logX Make X axis log10-scale.
#' @param logY Make Y axis log10-scale.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @export
#'
#' @examples weight <- rnorm(1000); qdensity(weight)

qdensity <- function(vec, ext = "pdf", xlab = F, plot = TRUE
                     , suffix = NULL
                     , plotname = qqqParsePlotname(vec, suffix)
                     , save = TRUE, mdlink = TRUE
                     , logX = F, logY = F
                     , max.names = 50
                     , w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = max.names)

  p <- ggpubr::ggdensity(data = df, x = "value" # , y = "..count.."
                 , title = plotname, xlab = xlab
                 , add = "median", rug = TRUE
                 , color = "names", fill = "names"
                 , palette = 'jco', ...
  ) +
    if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")
  if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  fname = Stringendo::kpp(plotname, suffix, "dens", Stringendo::flag.nameiftrue(logX), Stringendo::flag.nameiftrue(logY),  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}



# _________________________________________________________________________________________________
#' @title Barplot
#'
#' @param vec The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param plot Display the plot.
#' @param suffix A suffix added to the filename. NULL by default.
#' @param plotname The name of the file and title of the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param hline Draw a horizontal line on the plot.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by -1 or 1. Takes effect if "*line" is defined.
#' @param palette_use GGpubr Color palette to use.
#' @param col Color of the plot.
#' @param xlab.angle Rotate X-axis labels by N degree.
#' @param xlab X-axis label.
#' @param logY Make Y axis log10-scale.
#' @param label label
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize limitsize
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @export
#'
#' @examples weight3 <- runif (12);
#' qbarplot(weight3, filtercol = -1, hline = .5);
#' qbarplot(weight3, filtercol = 1, hline = .5)

qbarplot <- function(vec, ext = "pdf", plot = TRUE
                     , suffix = NULL
                     , plotname = qqqParsePlotname(vec, suffix)
                     # , title = F
                     , save = TRUE, mdlink = TRUE
                     , hline = F, filtercol = 1
                     , palette_use = 'jco', col = as.character(1:3)[1]
                     , xlab.angle = 90, xlab = F
                     , logY = F
                     , label = NULL
                     , max.names = 50
                     , limitsize = FALSE
                     , w = qqqAxisLength(vec), h = 5, ...) {

  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = max.names)

  if (length(unique(df$"names")) == 1) df$"names" <- as.character(1:length(vec))
  df[["colour"]] <- if (hline & filtercol != 0) {
    if (filtercol == 1 ) (df$"value" > hline) else if (filtercol == -1 ) (df$"value" < hline)
  } else if (length(col) == length(vec)) {
    as.character(col)
  } else {
    as.character(rep(col, length(vec))[1:length(vec)])
  }
  print(df)

  p <- ggpubr::ggbarplot(data = df, x = "names", y = "value"
                         , title = plotname, xlab = xlab
                         , color = 'colour', fill = 'colour'
                         , label = label
                         , palette = palette_use, ...
  ) + ggpubr::grids(axis = 'y') +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)
    )


  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (logY) p <- p + ggplot2::scale_y_log10()
  fname <- Stringendo::kpp(plotname, suffix, "bar", Stringendo::flag.nameiftrue(logY), ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, limitsize = limitsize)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}




# _________________________________________________________________________________________________
#' @title Pie chart
#'
#' @param vec The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param suffix A suffix added to the filename. NULL by default.
#' @param plotname The name of the file and title of the plot.
#' @param LegendSide LegendSide
#' @param LegendTitle LegendTitle
#' @param NoLegend NoLegend
#' @param pcdigits pcdigits
#' @param NamedSlices NamedSlices
#' @param custom.order custom.order
#' @param color.palette color.palette
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @export
#'
#' @examples xvec <- c("A"=12, "B"=29); qpie(vec = xvec)

qpie <- function(vec, ext = "pdf", plot = TRUE, save = TRUE, mdlink = TRUE
                 , suffix = NULL
                 , plotname = qqqParsePlotname(vec, suffix)
                 , LegendSide = T, LegendTitle = as.character(substitute(vec)), NoLegend = F
                 , pcdigits = 2, NamedSlices =F
                 , custom.order = F
                 # , custom.margin = F
                 , color.palette = 'jco'
                 , max.names = 50
                 , w = 5, h = w, ...) {

  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = max.names)
  pcX <- df$"value" / sum(df$"value")
  labs <- paste(100 * signif (pcX, pcdigits), "%", sep = "")
  if (NamedSlices) labs <- paste(df$names, "\n", labs)
  print(df)
  if (custom.order != F) df$'names' <- factor(df$'names', levels = custom.order)

  p <- ggpubr::ggpie(data = df, x = "value", label = labs
                     , fill = "names", color = "white"
                     , title = plotname
                     , palette = color.palette, ...)
  if (LegendSide) p <- ggpubr::ggpar(p, legend = "right", legend.title = LegendTitle)
  # if (custom.margin) p <- p + theme(plot.margin = unit(custom.margin, "cm"))
  # p <- if (NoLegend) p + NoLegend() else p
  p <- if (NoLegend) p + theme(legend.position = "none", validate = TRUE) else p
  fname = Stringendo::kpp(plotname, suffix, "pie",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}



# _________________________________________________________________________________________________
#' @title Scatter plot
#'
#' @param tbl_X_Y_Col_etc tbl_X_Y_Col_etc
#' @param suffix A suffix added to the filename. NULL by default.
#' @param plotname The name of the file and title of the plot.
#' @param col Color of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param logX Make X axis log10-scale.
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @import ggpubr
#' @export
#' @examples dfx <- as.data.frame(cbind("AA"=rnorm(12), "BB"=rnorm(12))); qscatter(dfx, suffix = "2D.gaussian")

qscatter <- function(tbl_X_Y_Col_etc
                     , suffix = NULL
                     , plotname = qqqParsePlotname(tbl_X_Y_Col_etc, suffix)
                     # , title = F
                     , col = c(NULL , 3)[1]
                     , ext = "png", also.pdf = T
                     , logX = F, logY = F
                     , hline = F, vline = F, plot = TRUE, save = TRUE, mdlink = TRUE
                     , w = 7, h = w, ...) {
  # plotname <- if (isFALSE(title)) Stringendo::kpp(make.names(as.character(substitute(tbl_X_Y_Col_etc))), suffix) else title
  vars <- colnames(tbl_X_Y_Col_etc)
  df <- tbl_X_Y_Col_etc
  p <- ggpubr::ggscatter(data = df, x = vars[1], y = vars[2], color = col
                 , title = plotname, ...) +
    ggpubr::grids(axis = 'xy')
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  fname = Stringendo::kpp(plotname, suffix, "scatter", Stringendo::flag.nameiftrue(logX), Stringendo::flag.nameiftrue(logY), ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}




# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
## _________________________________________________________________________________________________-



# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
# _________________________________________________________________________________________________



# ____________________________________________________________________
# ggExpress is the fastest way to create, annotate and export plots in R.  ----
# ____________________________________________________________________
# try(source("~/GitHub/Packages/ggExpress/R/ggExpress.functions.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpress/main/ggExpress.functions.R"), silent = T)

# ______________________________________________________________________________________________----
# Main plotting functions  ----
# ____________________________________________________________________



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


qhistogram <- function(vec, ext = "pdf", xlab = F, plot = TRUE, save = TRUE, mdlink = FALSE
                       , suffix = NULL
                       , plotname = sppp(substitute(vec), suffix)
                       , logX = F, logY = F
                       , vline = F, filtercol = 0
                       , add = "median"
                       , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                       , col = as.character(1:3)[1]
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
                           , add = add
                           # , color = "names", fill = "names"
                           , color = 'colour', fill = 'colour'
                           , palette = palette_use, ...
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
#' @param palette_use GGpubr Color palette to use.
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
                     , plotname = sppp(substitute(vec), suffix)
                     , save = TRUE, mdlink = FALSE
                     , logX = F, logY = F
                     , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                     , max.names = 50
                     , w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = max.names)

  p <- ggpubr::ggdensity(data = df, x = "value" # , y = "..count.."
                         , title = plotname, xlab = xlab
                         , add = "median", rug = TRUE
                         , color = "names", fill = "names"
                         , palette = palette_use, ...
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
#' @param hide.legend hide legend
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
                     , plotname = sppp(substitute(vec), suffix)
                     # , title = F
                     , save = TRUE, mdlink = FALSE
                     , hline = F, filtercol = 1
                     , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                     , col = as.character(1:3)[1]
                     , xlab.angle = 90, xlab = F
                     , logY = FALSE
                     , label = NULL
                     , hide.legend = TRUE
                     , max.names = 50
                     , limitsize = FALSE
                     , w = qqqAxisLength(vec), h = 5, ...) {


  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = max.names)
  # nrCategories.DFcol1 <- length(unique(df[,1])); stopif( nrCategories.DFcol1 >100)

  if (length(unique(df$"names")) == 1) df$"names" <- as.character(1:length(vec))

  df[["colour"]] <-
    if (length(col) == length(vec)) {
      as.character(col)
    } else if (hline & filtercol != 0) {
      if (filtercol == 1 ) (df$"value" > hline) else if (filtercol == -1 ) (df$"value" < hline)
    } else {
      as.character(rep(col, length(vec))[1:length(vec)])
    }
  print(df)

  p <- ggpubr::ggbarplot(data = df, x = "names", y = "value"
                         , title = plotname, xlab = xlab
                         , color = 'colour', fill = 'colour'
                         , label = label
                         , palette = palette_use, ...
  ) + ggpubr::grids(axis = 'y')

  if (hide.legend) { p <- p + ggplot2::theme(legend.position = "none"
    , axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1) )
  }

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
#' @param ordered Slices in the order of df. By default would ordered alphabetically in the plot.
#' @param both_pc_and_value Report both percentage AND number.
#' @param custom.order custom.order
#' @param palette_use GGpubr Color palette to use.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @export
#'
#' @examples xvec <- c("A"=12, "B"=29); qpie(vec = xvec)

qpie <- function(vec = Network.Size
                 , ext = "pdf", plot = TRUE, save = TRUE, mdlink = FALSE
                 , suffix = NULL
                 , plotname = sppp(substitute(vec), suffix)
                 , LegendSide = TRUE
                 , LegendTitle = as.character(substitute(vec)), NoLegend = F
                 , pcdigits = 2, NamedSlices = FALSE
                 , custom.order = FALSE
                 , extended.canvas = TRUE
                 , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                 , max.categories = 100
                 , max.names = 5
                 , decr.order = TRUE
                 , both_pc_and_value = TRUE
                 , w = 5, h = w, ...) {

  print(plotname)
  l.orig <- length(vec)
  sum.orig <- sum(vec)
  if (l.orig > max.categories) {
    iprint("Warning, there are more than", max.categories, "categories. Only the top", max.categories - 1, "items are show, the rest is added up.")
    sv <- sort(vec, decreasing = T)
    vec.new <- sv[1:(max.categories - 1)]
    idx.remaining <- max.categories:length(vec)
    sum.of.remaining <- sum(sv[idx.remaining])
    fr.sum <- percentage_formatter(sum.of.remaining / sum(vec))
    iprint("The remaining", length(idx.remaining), "values make up", fr.sum,"of the data.")

    vec.new[max.categories] <- sum.of.remaining
    name.of.last <- paste('Sum of rem.', length(idx.remaining))
    names(vec.new)[max.categories] <- name.of.last
    vec <- vec.new
  }

  if (is.null(names(vec))) { names(vec) <- as.character(1:length(vec)) }

  df <- qqqCovert.named.vec2tbl.v2(namedVec = vec, thr = max.names)
  if (l.orig > max.categories) df[['names']][length(df$'names')] <- name.of.last

  pcX <- df$"value" / sum(df$"value")
  labs <- paste(100 * signif(pcX, pcdigits), "%", sep = "")

  idx.named <- which(!df$'names' == "")
  if (both_pc_and_value) df$'names'[idx.named] <- paste(df$'names'[idx.named], labs[idx.named], sep = "\n")


  if (decr.order) df[['names']] <- factor(df$'names', levels = rev(make.unique(df$'names')))

  nrCategories.DFcol1 <- length(unique(df[,1])); stopif( nrCategories.DFcol1 > max.categories)
  print(nrCategories.DFcol1)

  if (NamedSlices) labs <- paste(df$names, "\n", labs)
  if (custom.order != F) df$'names' <- factor(df$'names', levels = custom.order)

  (p <- ggpubr::ggpie(data = df
                      , x = "value"
                      , label = 'names'
                      , subtitle = paste('Sum:', sum.orig)
                      # , label = labs
                      # , fill = labs
                      , fill = "names"
                      , color = "white"
                      , title = plotname
                      , palette = palette_use
                      , caption = p0('Total elements:', l.orig, '; shown:', (max.categories-1)
                                     , ' | max.names:', max.names)
                      # , ...
  ))
  if (LegendSide) p <- ggpubr::ggpar(p, legend = "right", legend.title = LegendTitle)
  if (custom.margin) p <- p + coord_polar(theta = "y", clip = "off")

  # p <- if (NoLegend) p + NoLegend() else p
  p <- if (NoLegend) p + theme(legend.position = "none", validate = TRUE) else p
  fname = Stringendo::kpp(plotname, "pie",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  # print(112121)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title qboxplot plot
#'
#' @param df_XYcol Data, as 2 column data frame, where col.1 is X axis.
#' @param suffix A suffix added to the filename. NULL by default.
#' @param title The name of the file and title of the plot.
#' @param col Color of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param outlier.shape outlier shape. NA to hide.
#' @param vline Draw a vertical line on the plot.
#' @param stat.test Do a statistical test?
#' @param stat.method stat method. NULL for default
#' @param stat.label.y.npc stat label y position
#' @param stat.label.x stat label x position
#' @param plot Display the plot.
#' @param palette_use GGpubr Color palette to use.
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


qboxplot <- function(df_XYcol
                     , suffix = NULL
                     , plotname = sppp(substitute(df_XYcol), suffix)
                     , outlier.shape = NULL
                     , title = F
                     , stat.test = T
                     # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
                     , stat.method = NULL, stat.label.y.npc = "top", stat.label.x = NULL
                     # , fill = c(NULL , 3)[1]
                     , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                     , ext = "png", also.pdf = T
                     , logY = F #, logX = F
                     , hline = F, vline = F, plot = TRUE, save = TRUE, mdlink = FALSE
                     , w = 7, h = w, ...) {
  # plotname <- if (isFALSE(title)) Stringendo::kpp(make.names(as.character(substitute(df_XYcol))), suffix) else title
  vars <- colnames(df_XYcol)
  nrCategories.DFcol1 <- length(unique(df_XYcol[,1])); stopif(nrCategories.DFcol1>  100)

  p <- ggpubr::ggboxplot(data = df_XYcol, x = vars[1], y = vars[2], fill = vars[1]
                         # , fill = fill
                         , palette = palette_use
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


# _________________________________________________________________________________________________
#' @title qviolin plot
#'
#' @param df_XYcol Data, as 2 column data frame, where col.1 is X axis.
#' @param suffix A suffix added to the filename. NULL by default.
#' @param title The name of the file and title of the plot.
#' @param col Color of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param stat.test Do a statistical test?
#' @param stat.method stat method. NULL for default
#' @param stat.label.y.npc stat label y position
#' @param stat.label.x stat label x position
#' @param plot Display the plot.
#' @param palette_use GGpubr Color palette to use.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @import ggpubr
#' @export
#' @examples data("ToothGrowth"); ToothLen.by.Dose <- ToothGrowth[ ,c('dose', 'len')]; qviolin(ToothLen.by.Dose)

# @param logX Make X axis log10-scale.
# @param plotname The name of the file and title of the plot.


qviolin <- function(df_XYcol
                    , suffix = NULL
                    , plotname = sppp(substitute(df_XYcol), suffix)
                    # , outlier.shape = NULL
                    , title = F
                    , stat.test = T
                    # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
                    , stat.method = NULL, stat.label.y.npc = "top", stat.label.x = 0.5
                    # , fill = c(NULL , 3)[1]
                    , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                    , ext = "png", also.pdf = T
                    , logY = F #, logX = F
                    , hline = F, vline = F, plot = TRUE, save = TRUE, mdlink = FALSE
                    , w = 7, h = w, ...) {
  # plotname <- if (isFALSE(title)) Stringendo::kpp(make.names(as.character(substitute(df_XYcol))), suffix) else title
  vars <- colnames(df_XYcol)
  nrCategories.DFcol1 <- length(unique(df_XYcol[,1])); stopif(nrCategories.DFcol1>  100)

  p <- ggpubr::ggviolin(data = df_XYcol, x = vars[1], y = vars[2], fill = vars[1]
                        , title = plotname
                        # , fill = fill
                        # , outlier.shape = outlier.shape
                        , palette = palette_use
                        , ...) +
    ggpubr::grids(axis = 'y')
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)
  # if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)



  fname = Stringendo::kpp(plotname, suffix, "violinplot", Stringendo::flag.nameiftrue(logY), ext) # , Stringendo::flag.nameiftrue(logX)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title Scatter plot
#'
#' @param df_XYcol Data, as 2 column data frame, where col.1 is X axis.
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
#' @param palette_use GGpubr Color palette to use.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @import ggpubr
#' @export
#' @examples dfx <- as.data.frame(cbind("AA"=rnorm(500), "BB"=rnorm(500))); qscatter(dfx, suffix = "2D.gaussian")

qscatter <- function(df_XYcol
                     , suffix = NULL
                     , plotname = sppp(substitute(df_XYcol), suffix)
                     # , title = F
                     , col = c(NULL , 3)[1]
                     , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                     , ext = "png", also.pdf = T
                     , logX = F, logY = F
                     , hline = F, vline = F, plot = TRUE, save = TRUE, mdlink = FALSE
                     , w = 7, h = w, ...) {
  # plotname <- if (isFALSE(title)) Stringendo::kpp(make.names(as.character(substitute(df_XYcol))), suffix) else title
  vars <- colnames(df_XYcol)

  p <- ggpubr::ggscatter(data = df_XYcol, x = vars[1], y = vars[2]
                         , palette = palette_use
                         , color = col
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



# ______________________________________________________________________________________________----
# Auxiliary functions ----
# ____________________________________________________________________

# _________________________________________________________________________________________________
#' @title Quick-Save ggplot objects
#'
#' @param ggobj ggobj
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param page page
#' @param title title
#' @param fname fname
#' @param suffix A suffix added to the filename. NULL by default.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @export
#'
#' @examples xplot <- ggplot2::qplot(12); qqSave(ggobj = xplot); qqSave(ggobj = xplot, ext = "pdf")
#' @importFrom cowplot save_plot

qqSave <- function(ggobj, w =4, h = w
                   , ext =c("png", "pdf")[1], also.pdf = FALSE
                   , page = c(F, "A4p", "A4l", "A5p", "A5l")[1]
                   , title = F, fname = F, suffix = NULL, ...) {
  if (isFALSE(title)) title <- as.character(substitute(ggobj))
  if (also.pdf) fname2 <- if (isFALSE(fname)) Stringendo::kpp(title, suffix, 'pdf') else Stringendo::kpp(fname, 'pdf')
  if (isFALSE(fname)) fname <- Stringendo::kpp(title, suffix, ext)

  if (!isFALSE(page)) {
    wA4 <- 8.27
    hA4 <- 11.69
    if ( page == "A4p" ) { w = wA4; h = hA4 }
    if ( page == "A4l" ) { w = hA4; h = wA4 }
    if ( page == "A5p" ) { w = wA4/2; h = hA4/2 }
    if ( page == "A5l" ) { w = hA4/2; h = wA4/2 }
  }
  print(paste0(getwd(),"/", fname))

  if (also.pdf) cowplot::save_plot(plot = ggobj, filename = fname2, base_width = w, base_height = h, ...)
  cowplot::save_plot(plot = ggobj, filename = fname, base_width = w, base_height = h, ...)
}



# _________________________________________________________________________________________________
#' @title Insert Markdown image link to .md report
#'
#' @param file_name file_name
#' @export
#'
#' @examples qMarkdownImageLink(file_name = "myplot.pdf")
#' @importFrom MarkdownHelpers llogit

qMarkdownImageLink <- function(file_name = 'myplot.pdf') {
  llogit(paste0("![", file_name, "]", "(", file_name, ")", collapse = ''))
}




# _________________________________________________________________________________________________
#' @title Define Axis Length
#'
#' @param vec The variable to plot.
#' @param minLength minLength
#' @export
#'
#' @examples qqqAxisLength()

qqqAxisLength <- function(vec = 1:20, minLength=6) {
  max(round(length(vec)*0.2), minLength)
}


# _________________________________________________________________________________________________
#' @title qqqCovert.named.vec2tbl
#' @description Covert a named vector to a table.
#' @param namedVec namedVec
#' @param verbose verbose
#' @param strip.too.many.names strip.too.many.names
#' @param thr thr
#' @export
#'
#' @examples qqqCovert.named.vec2tbl(namedVec = c("A"=2, "B"=29) )


qqqCovert.named.vec2tbl <- function(namedVec=1:14, verbose = F, strip.too.many.names = TRUE, thr = 50) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.

  # Check naming issues
  nr.uniq.names <- length(unique(names(namedVec)))
  if (nr.uniq.names > thr & verbose)  print("Vector has", thr, "+ names. Can mess up auto-color legends.")
  if (nr.uniq.names < 1 & verbose) print("Vector has no names")
  an.issue.w.names <- (nr.uniq.names > thr | nr.uniq.names < 1 )

  idx.elements.above.thr <- if (thr < length(namedVec)) thr:length(namedVec) else 1:length(namedVec)
  if (strip.too.many.names & an.issue.w.names) names(namedVec)[idx.elements.above.thr] <- rep("", length(idx.elements.above.thr))
  if (length(unique(names(namedVec))) > thr) print("Vector has", thr, "+ names. Can mess up auto-color legends.")

  df <- tibble::as_tibble(cbind("value" = namedVec))
  nm <- names(namedVec)
  df$"names" <- if (!is.null(nm)) nm else rep(".", length(namedVec))
  df
}





# _________________________________________________________________________________________________
#' @title qqqCovert.tbl2vec
#' @description Covert a table to a named vector.
#' @param tibble.input tibble.input
#' @param name.column name.column
#' @param value.column value.column
#' @export
#'
#' @examples a=1:5; x= tibble::tibble(a, a * 2); qqqCovert.tbl2vec(x)

qqqCovert.tbl2vec <- function(tibble.input, name.column = 1, value.column = 2) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
  vec <- tibble.input[[value.column]]
  names(vec) <- tibble.input[[name.column]]
  vec
}

# _________________________________________________________________________________________________
# #' @title qqqParsePlotname
# #' @description Parse Plotname from variable name.
# #' @param string string
# #' @param suffix_tag suffix_tag
# #' @export
# #'
# #' @examples qqqParsePlotname()
#
# qqqParsePlotname <- function(string = "sadsad", suffix_tag= NULL) { # parse plot name from variable name and suffix
#   nm <- make.names(as.character(substitute(string)))
#   if (!is.null(suffix_tag) & !isFALSE(suffix_tag)) nm <- Stringendo::kpp(nm, suffix_tag)
#   return(nm)
# }


# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
#' @title  q32vA4_grid_plot
#' @description Plot up to 6 panels (3-by-2) on vertically standing A4 page.
#' @param plot_list A list of ggplot objects, each of which is one panel.
#' @param plotname Plot name, Default: F
#' @param suffix A suffix added to the filename, Default: NULL
#' @param scale Scaling factor of the canvas, Default: 1
#' @param nrow number of rows for panels on the page, Default: 2
#' @param ncol number of columns for panels on the page, Default: 2
#' @param h height of the plot, Default: wA4 * scale
#' @param w width of the plot, Default: hA4 * scale
#' @param ... Pass any other parameter to the internally called functions (most of them should work).
#' @param extension file extension
#' @export
#'
#' @examples # q32vA4_grid_plot()

q32vA4_grid_plot <- function(plot_list
                             , suffix = NULL
                             , plotname = sppp(substitute(plot_list), suffix)
                             , plot =F
                             , nrow = 3, ncol = 2, extension = c('pdf', 'png')[2]
                             , h = hA4 * scale, w = wA4 * scale, scale = 1
                             , ...) { # Save 4 umaps on an A4 page.
  print("Plot panels on 3-by-2 vertical A4 page.")
  stopifnot(length(plot_list)<7)

  # if (plotname==F) plotname =  sppp(substitute(plot_list), suffix)
  fname = kpp(plotname, extension)
  p1 = cowplot::plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol, labels = LETTERS[1:length(plot_list)], ...  )
  cowplot::save_plot(plot = p1, filename = fname, base_height = h, base_width = w)
  ww.FnP_parser(fname)
}




# _________________________________________________________________________________________________
#' @title  qA4_grid_plot
#' @description Plot up to 6 panels (3-by-1) on vertically standing A4 page.
#' @param plot_list A list of ggplot objects, each of which is one panel.
#' @param plotname Plot name, Default: F
#' @param suffix A suffix added to the filename, Default: NULL
#' @param scale Scaling factor of the canvas, Default: 1
#' @param nrow number of rows for panels on the page, Default: 2
#' @param ncol number of columns for panels on the page, Default: 2
#' @param h height of the plot, Default: wA4 * scale
#' @param w width of the plot, Default: hA4 * scale
#' @param ... Pass any other parameter to the internally called functions (most of them should work).
#' @param extension file extension
#' @export
#'
#' @examples # qA4_grid_plot()

qA4_grid_plot <- function(plot_list
                          , suffix = NULL
                          , nrow = 3, ncol = 2
                          , plotname = sppp(substitute(plot_list), nrow, 'by', ncol, suffix)
                          , plot = F
                          , extension = c('pdf', 'png')[2]
                          , h = hA4 * scale, w = wA4 * scale
                          , scale = 1
                          , labels = LETTERS[1:length(plot_list)]
                          , max.list.length = 16
                          , ...) { # Save 4 umaps on an A4 page.
  iprint("Plot panels on", nrow, "by", ncol, "vertical A4 page.")
  stopifnot(length(plot_list) < max.list.length)


  # if (plotname==F) plotname =  sppp(substitute(plot_list), suffix)
  fname = kpp(plotname, extension)
  p1 = cowplot::plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol, labels = labels, ...  )
  cowplot::save_plot(plot = p1, filename = fname, base_height = h, base_width = w)
  ww.FnP_parser(fname)
}




# _________________________________________________________________________________________________
# _________________________________________________________________________________________________
# _________________________________________________________________________________________________



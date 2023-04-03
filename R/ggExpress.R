# ____________________________________________________________________
# ggExpress is the fastest way to create, annotate and export plots in R.  ----
# ____________________________________________________________________
# devtools::load_all("~/GitHub/Packages/ggExpress")
# try(source("~/GitHub/Packages/ggExpress/R/ggExpress.R"), silent = TRUE)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpress/main/ggExpress.R"), silent = TRUE)

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
#' @param add Add plot annotation lines (see ggpubr). Default: 'median'.
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
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param hide.legend hide legend
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param annotation_logticks_X
#' @param annotation_logticks_Y
#' @param grid
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples weight <- rnorm(1000); qhistogram(vec = weight); qhistogram(vec = weight, vline = 2, filtercol = -1)


qhistogram <- function(vec
                       , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'pdf')
                       , xlab = FALSE, plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                       , suffix = NULL
                       , plotname = FixPlotName(kpp(substitute(vec), suffix))
                       , logX = FALSE, logY = FALSE
                       , annotation_logticks_X = logX, annotation_logticks_Y = logY
                       , vline = FALSE, filtercol = 0
                       , add = "median"
                       , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                       , col = as.character(1:3)[1]
                       , xlab.angle = 90
                       , hide.legend = TRUE
                       , max.names = 50
                       , grid = 'y'
                       , w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)

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
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)) +
  if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (annotation_logticks_X) p <- p + annotation_logticks(sides = "b")

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none" )


  fname = Stringendo::kpp(plotname, suffix, "hist", Stringendo::flag.nameiftrue(logX), Stringendo::flag.nameiftrue(logY), ext)
  fname = Stringendo::FixPlotName(fname)
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
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param palette_use GGpubr Color palette to use.
#' @param hide.legend hide legend
#' @param logY Make Y axis log10-scale.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param grid
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples weight <- rnorm(1000); qdensity(weight)

qdensity <- function(vec
                     , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'pdf')
                     , xlab = FALSE, plot = TRUE
                     , suffix = NULL
                     , plotname = FixPlotName(kpp(substitute(vec), suffix))
                     , save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                     , logX = FALSE, logY = FALSE
                     , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                     , xlab.angle = 90
                     , hide.legend = TRUE
                     , max.names = 50
                     , grid = F
                     , w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)

  p <- ggpubr::ggdensity(data = df, x = "value" # , y = "..count.."
                         , title = plotname, xlab = xlab
                         , add = "median", rug = TRUE
                         , color = "names", fill = "names"
                         , palette = palette_use, ...
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)) +
  if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")
  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

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
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param xlab X-axis label.
#' @param logY Make Y axis log10-scale.
#' @param label label
#' @param hide.legend hide legend
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize limitsize
#' @param w width of the plot.
#' @param h height of the plot.
#' @param annotation_logticks_Y
#' @param grid
#' @param ylim ylimit values
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples weight3 <- runif (12);
#' qbarplot(weight3, filtercol = -1, hline = .5);
#' qbarplot(weight3, filtercol = 1, hline = .5)

qbarplot <- function(vec
                     , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'pdf')
                     , plot = TRUE
                     , suffix = NULL
                     , plotname = FixPlotName(kpp(substitute(vec), suffix))
                     # , title = FALSE
                     , save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                     , hline = FALSE, filtercol = 1
                     , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                     , col = as.character(1:3)[1]
                     , xlab.angle = 45, xlab = ""
                     , logY = FALSE
                     , ylim = c(0, 1.1 * max(vec, na.rm = TRUE))
                     , annotation_logticks_Y = logY
                     , label = NULL
                     , hide.legend = TRUE
                     , max.names = 50
                     , limitsize = FALSE
                     , grid = 'y'
                     , w = qqqAxisLength(vec), h = 5, ...) {


  if (isFALSE(xlab)) xlab = plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, strip.too.many.names =F)
  # nrCategories.DFcol1 <- length(unique(df[,1])); MarkdownHelpers::stopif( nrCategories.DFcol1 >100)

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
                         , ylim = ylim
                         , palette = palette_use, ...
  ) + ggpubr::grids(axis = 'y') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))
  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)

  if (length(vec) > max.names) p <- p + ggplot2::guides(x = 'none')
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none" )

  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")
  fname <- Stringendo::kpp(plotname, "bar", Stringendo::flag.nameiftrue(logY), ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, limitsize = limitsize)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}


#' @title Barplot for tibbles or dataframes
#'
#' @param df The variable to plot.
#' @param x Colname to split along X axis. Def colnames(df)[1]
#' @param y Colname to count along y axis. Def colnames(df)[3]
#' @param fill Color (split) by along Y.
#' @param color Color (split) by along Y.
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
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param xlab X-axis label.
#' @param logY Make Y axis log10-scale.
#' @param label label
#' @param hide.legend hide legend
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize limitsize
#' @param w width of the plot.
#' @param h height of the plot.
#' @param annotation_logticks_Y
#' @param grid
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples my_tibble <- tibble(Column_1 = c("A", "A", "A", "B", "C", "C"),Column_2 = c("X", "Y", "Y", "Z", "X", "Z")); freq_table <- my_tibble %>% count(Column_1, Column_2)); qbarplot.df(freq_table)


qbarplot.df <- function(df
                        , x = colnames(df)[1]
                        , y = colnames(df)[2]
                        , fill = colnames(df)[3]
                        , label = NULL
                        , color = 1
                        , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'pdf')
                        , plot = TRUE
                        , suffix = NULL
                        , plotname = FixPlotName(kpp(substitute(df), suffix))
                        # , title = FALSE
                        , save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                        , hline = FALSE, filtercol = 1
                        , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                        , col = as.character(1:3)[1]
                        , xlab.angle = 45, xlab = ""
                        , logY = FALSE
                        , annotation_logticks_Y = logY
                        , hide.legend = TRUE
                        , max.names = 50
                        , limitsize = FALSE
                        , grid = 'y'
                        , w = qqqAxisLength(df), h = 5, ...) {


  if (isFALSE(xlab)) xlab = plotname


  p <- ggpubr::ggbarplot(data = df, x = x, y = y
                         , color = color
                         , fill = fill
                         , title = plotname, xlab = xlab
                         , label = label
                         , palette = palette_use, ...
  ) + ggpubr::grids(axis = 'y') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))
  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)

  if (length(df) > max.names) p <- p + ggplot2::guides(x = 'none')
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none" )

  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")
  fname <- Stringendo::kpp(plotname, "bar", Stringendo::flag.nameiftrue(logY), ext)
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
#' @param extended.canvas Make an extended canvas, default: T
#' @param custom.margin custom plot margin, default: T
#' @param max.categories Maximum number of categories to be shown as a seprate slice
#' @param decr.order Slices in the order of df. By default would ordered alphabetically in the plot.
#' @param both_pc_and_value Report both percentage AND number.
#' @param custom.order custom.order
#' @param palette_use GGpubr Color palette to use.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @param label Slice labels. Set to NULL to remove slice names.
#'
#' @export
#'
#' @examples xvec <- c("A"=12, "B"=29); qpie(vec = xvec)


qpie <- function(vec = MyVec
                 , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'pdf')
                 , plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                 , suffix = NULL
                 , plotname = FixPlotName(kpp(substitute(vec), suffix))
                 , LegendSide = TRUE
                 , LegendTitle = FixPlotName(substitute(vec)), NoLegend = FALSE
                 , pcdigits = 2, NamedSlices = FALSE
                 , custom.order = FALSE
                 , extended.canvas = TRUE
                 , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                 , custom.margin = TRUE
                 , max.categories = 100
                 , max.names = 10
                 , decr.order = TRUE
                 , both_pc_and_value = FALSE
                 , labels = 'names' # Set to NULL to remove slice names.
                 , w = 7, h = 5, ...) {

  print(plotname)
  l.orig <- length(vec)
  sum.orig <- sum(vec)
  if (l.orig > max.categories) {
    iprint("Warning, there are more than", max.categories, "categories. Only the top", max.categories - 1, "items are show, the rest is added up.")
    sv <- sort(vec, decreasing = TRUE)
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

  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)
  if (l.orig > max.categories) df[['names']][length(df$'names')] <- name.of.last

  pcX <- df$"value" / sum(df$"value")
  labs <- paste(100 * signif(pcX, pcdigits), "%", sep = "")

  idx.named <- which(!df$'names' == "")
  df$'names'[idx.named] <- paste(df$'names'[idx.named], labs[idx.named], sep = "\n")

  # print('- -')
  if (both_pc_and_value) df$'names'[idx.named] <- paste(df$'names'[idx.named], df$'value'[idx.named], sep = "\n")

  if (decr.order) df[['names']] <- factor(df$'names', levels = rev(make.unique(df$'names')))

  nrCategories.DFcol1 <- length(unique(df[,1])); MarkdownHelpers::stopif( nrCategories.DFcol1 > max.categories)
  print(nrCategories.DFcol1)

  if (NamedSlices) labs <- paste(df$names, "\n", labs)
  if (custom.order != F) df$'names' <- factor(df$'names', levels = custom.order)
  # print('- - -')

  (p <- ggpubr::ggpie(data = df
                      , x = "value"
                      , label = labels
                      , subtitle = paste('Sum:', sum.orig)
                      , fill = "names"
                      , color = "white"
                      , title = plotname
                      , palette = palette_use
                      , caption = paste0('Total elements:', l.orig, '; shown:', (max.categories-1)
                                         , ' | max.names:', max.names)
                      # , ...
  )) + theme(legend.title = LegendTitle)
  # print('- - - -')
  if (LegendSide) p <- ggpubr::ggpar(p, legend = "right")
  if (custom.margin) p <- p + coord_polar(theta = "y", clip = "off")

  p <- if (NoLegend) p + theme(legend.position = "none", validate = TRUE) else p
  fname <- Stringendo::kpp(plotname, "pie",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)

  if (plot) p
}

# _________________________________________________________________________________________________
#' @title Boxplot
#'
#' @param df_XYcol_or_list Data, as 2 column data frame, where col.1 is X axis, alternatively a uniquely named list ov values.
#' @param suffix A suffix added to the filename. NULL by default.
#' @param title The name of the file and title of the plot.
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
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param hide.legend hide legend
#' @param palette_use GGpubr Color palette to use.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param plotname
#' @param annotation_logticks_Y
#' @param grid
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @import ggpubr
#' @export
#' @examples data("ToothGrowth"); ToothLen.by.Dose <- ToothGrowth[ ,c('dose', 'len')]; qboxplot(ToothLen.by.Dose)

qboxplot <- function(df_XYcol_or_list
                     , suffix = NULL
                     , plotname = FixPlotName(kpp(substitute(df_XYcol_or_list), suffix))
                     , outlier.shape = NULL
                     , title = FALSE
                     , stat.test = TRUE
                     # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
                     , stat.method = NULL, stat.label.y.npc = "top", stat.label.x = NULL
                     # , fill = c(NULL , 3)[1]
                     , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                     , hide.legend = FALSE
                     , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'png')
                     , also.pdf = TRUE
                     , logY = FALSE #, logX = FALSE
                     , annotation_logticks_Y = logY
                     , xlab.angle = 90
                     , hline = FALSE, vline = FALSE
                     , plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                     , grid = 'y'
                     , w = 7, h = w, ...) {

  df_XYcol <- if (CodeAndRoll2::is.list2(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list

  vars <- colnames(df_XYcol)
  nrCategories.DFcol1 <- length(unique(df_XYcol[,1])); MarkdownHelpers::stopif(nrCategories.DFcol1>  100)

  p <- ggpubr::ggboxplot(data = df_XYcol, x = vars[1], y = vars[2], fill = vars[1]
                         # , fill = fill
                         , palette = palette_use
                         , outlier.shape = outlier.shape
                         , title = plotname, ...) +
    ggpubr::grids(axis = 'y') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none" )

  fname = Stringendo::kpp(plotname, suffix, "boxplot", Stringendo::flag.nameiftrue(logY), ext) # , Stringendo::flag.nameiftrue(logX)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title qviolin plot
#'
#' @param df_XYcol_or_list Data, as 2 column data frame, where col.1 is X axis, alternatively a uniquely named list ov values.
#' @param suffix A suffix added to the filename. NULL by default.
#' @param title The name of the file and title of the plot.
#' @param plotname Name of the plot
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
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param hide.legend hide legend
#' @param palette_use GGpubr Color palette to use.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param annotation_logticks_Y
#' @param grid
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @import ggpubr
#' @export
#' @examples data("ToothGrowth"); ToothLen.by.Dose <- ToothGrowth[ ,c('dose', 'len')]; qviolin(ToothLen.by.Dose)


qviolin <- function(df_XYcol_or_list
                    , suffix = NULL
                    , plotname = FixPlotName(kpp(substitute(df_XYcol_or_list), suffix))
                    , title = FALSE
                    , stat.test = TRUE
                    , stat.method = NULL, stat.label.y.npc = "top", stat.label.x = 0.5
                    , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                    , hide.legend = FALSE
                    , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'png')
                    , also.pdf = FALSE
                    , logY = FALSE #, logX = FALSE
                    , annotation_logticks_Y = logY
                    , xlab.angle = 45
                    , hline = FALSE, vline = FALSE
                    , grid = FALSE
                    , plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                    # , outlier.shape = NULL
                    # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
                    # , fill = c(NULL , 3)[1]
                    , w = 7, h = w, ...) {

  stopifnot(is.numeric(xlab.angle))

  # plotname <- if (isFALSE(title)) Stringendo::kpp(make.names(as.character(substitute(df_XYcol_or_list))), suffix) else title
  df_XYcol <- if (CodeAndRoll2::is.list2(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list

  vars <- colnames(df_XYcol)
  nrCategories.DFcol1 <- length(unique(df_XYcol[,1])); MarkdownHelpers::stopif(nrCategories.DFcol1>  100)

  p <- ggpubr::ggviolin(data = df_XYcol, x = vars[1], y = vars[2], fill = vars[1]
                        , title = plotname
                        # , fill = fill
                        # , outlier.shape = outlier.shape
                        , palette = palette_use
                        , ...) +
    ggpubr::grids(axis = 'y') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none" )

  fname = Stringendo::kpp(plotname, suffix, "violinplot", Stringendo::flag.nameiftrue(logY), ext) # , Stringendo::flag.nameiftrue(logX)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}



# _________________________________________________________________________________________________
#' @title qstripchart plot
#'
#' @param df_XYcol_or_list Data, as 2 column data frame, where col.1 is X axis, alternatively a uniquely named list ov values.
#' @param add Add boxplot or violin chart? Default  add = c("violin", "mean_sd"), it can be "boxplot" or only "mean_sd".
#' @param suffix A suffix added to the filename. NULL by default.
#' @param title The name of the file and title of the plot.
#' @param plot Display the plot.
#' @param plotname Name of the plot
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param stat.test Do a statistical test?
#' @param stat.method stat method. NULL for default
#' @param stat.label.y.npc stat label y position
#' @param stat.label.x stat label x position
#' @param size.point Size of points
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param hide.legend hide legend
#' @param palette_use GGpubr Color palette to use.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param annotation_logticks_Y
#' @param grid
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @import ggpubr
#' @export
#' @examples data("ToothGrowth"); ToothLen.by.Dose <- ToothGrowth[ ,c('dose', 'len')]; qstripchart(ToothLen.by.Dose)


qstripchart <- function(df_XYcol_or_list
                        , add = c("violin", "mean_sd")
                        , suffix = NULL
                        , plotname = FixPlotName(kpp(substitute(df_XYcol_or_list), suffix))
                        # , outlier.shape = NULL
                        , title = FALSE
                        , size.point = .2
                        , stat.test = TRUE
                        # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
                        , stat.method = NULL, stat.label.y.npc = "top", stat.label.x = 0.75
                        # , fill = c(NULL , 3)[1]
                        , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                        , hide.legend = FALSE
                        , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'png')
                        , also.pdf = TRUE
                        , logY = FALSE #, logX = FALSE
                        , annotation_logticks_Y = logY
                        , xlab.angle = 90
                        , hline = FALSE, vline = FALSE
                        , plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                        , grid = 'y'
                        , w = 7, h = w, ...) {
  df_XYcol <- if (CodeAndRoll2::is.list2(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list

  vars <- colnames(df_XYcol)
  nrCategories.DFcol1 <- length(unique(df_XYcol[,1])); MarkdownHelpers::stopif(nrCategories.DFcol1>  100)

  p <- ggpubr::ggstripchart(data = df_XYcol, x = vars[1], y = vars[2], fill = vars[1]
                            , title = plotname
                            , add = add
                            , size = size.point
                            , palette = palette_use
                            , ...) +
    ggpubr::grids(axis = 'y') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none" )

  fix <- sppp("stripchart", sppp(add))
  fname = Stringendo::kpp(plotname, fix, suffix, "plot", Stringendo::flag.nameiftrue(logY), ext) # , Stringendo::flag.nameiftrue(logX)
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
#' @param hline Draw a horizontal line on the plot, yintercept or FALSE
#' @param vline Draw a vertical line on the plot, xintercept or FALSE.
#' @param abline Draw a sloped line on the plot. Set to FALSE, or intercept = abline[1], slope = abline[2].
#' @param add_contour_plot Add 2D contour plot. See: http://www.sthda.com/english/articles/32-r-graphics-essentials/131-plot-two-continuous-variables-scatter-graph-and-alternatives/#continuous-bivariate-distribution
#' @param correlation_r2 Add a correlation value to the plot
#' @param plot Display the plot.
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param palette_use GGpubr Color palette to use.
#' @param hide.legend hide legend
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param w width of the plot.
#' @param h height of the plot.
#' @param annotation_logticks_Y
#' @param annotation_logticks_X
#' @param grid
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @import ggpubr
#' @export
#' @examples dfx <- as.data.frame(cbind("AA"=rnorm(500), "BB"=rnorm(500))); qscatter(dfx, suffix = "2D.gaussian")

qscatter <- function(df_XYcol
                    , suffix = NULL
                    , plotname = FixPlotName(kpp(substitute(df_XYcol), suffix))
                    # , title = FALSE
                    , col = c(NULL , 3)[1]
                    , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                    , hide.legend = FALSE
                    , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'png')
                    , also.pdf = TRUE
                    , logX = FALSE, logY = FALSE
                    , annotation_logticks_Y = logY
                    , annotation_logticks_X = logX
                    , xlab.angle = 90
                    , hline = FALSE, vline = FALSE, abline = FALSE
                    , add_contour_plot = FALSE
                    , correlation_r2 = FALSE # add as  c("pearson", "spearman")
                    , plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                    , grid = 'xy'
                    , w = 7, h = w, ...) {
  print(plotname)
  stopifnot(ncol(df_XYcol) >= 2)
  if (is.matrix(df_XYcol)) df_XYcol <- as.data.frame(df_XYcol)

  vars <- colnames(df_XYcol)
  cat('Variable (column) names:', vars,'\n')
  p <- ggpubr::ggscatter(data = df_XYcol, x = vars[1], y = vars[2]
                         , palette = palette_use
                         , color = col
                         , title = plotname, ...) +
    ggpubr::grids(axis = 'xy') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))


  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (vline) p <- p + ggplot2::geom_vline(xintercept = vline)
  if (sum(abline)) p <- p + ggplot2::geom_abline(intercept = abline[1], slope = abline[2])
  if (add_contour_plot) p <- p + geom_density_2d()
  if (correlation_r2 %in% c("pearson", "spearman") ) p <- p + stat_cor(method = correlation_r2)

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (annotation_logticks_X) p <- p + annotation_logticks(sides = "b")

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")


  fname = Stringendo::kpp(plotname, suffix, "scatter", Stringendo::flag.nameiftrue(logX), Stringendo::flag.nameiftrue(logY), ext)
  if (plot) p
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(fname)
  p
}



# _________________________________________________________________________________________________
#' @title Venn Diagram
#' Using the  ggVennDiagram package.
#'
#' @param list The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param suffix A suffix added to the filename. NULL by default.
#' @param plotname The name of the file and title of the plot.
#' @param subtitle The subtitle of the plot. Default: paste (length(unique(unlist(list))), 'elements in total')
#' @param col.min Color scale minimum, default: white
#' @param col.max Color scale maximum, default: red
#' @param w width of the plot.
#' @param hide.legend hide legend
#' @param h height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples LetterSets <- list("One" = LETTERS[1:7], "Two" = LETTERS[3:12]); qvenn(LetterSets)

qvenn <- function(list
                  , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'pdf')
                  , plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified('b.mdlink', def = FALSE)
                  , suffix = NULL
                  , plotname = FixPlotName(kpp(substitute(list), suffix))
                  , subtitle = paste (length(unique(unlist(list))), 'elements in total')
                  # , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
                  # , col = as.character(1:3)[1]
                  # , xlab.angle = 90
                  , col.min = "white"
                  , col.max = "red"
                  , hide.legend = FALSE
                  , w = 8, h = 0.75 * w
                  , ...) {

  p <- ggVennDiagram::ggVennDiagram(list, ...) +
    scale_fill_gradient(low = col.min, high = col.max) +
    ggplot2::ggtitle(label = paste(' ', plotname), subtitle = paste(' ', subtitle, '\n') ) +
    theme(plot.background = element_rect(fill = 'white', colour = 'white'))

  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none" )

  fname = Stringendo::kpp(plotname, suffix, "venn", ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}






# ______________________________________________________________________________________________----
# Auxiliary functions ----
# ____________________________________________________________________

# _________________________________________________________________________________________________
#' @title Quick-Save ggplot objects
#'
#' @param ggobj Plot as ggplot object.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param page page
#' @param title title field for pdf file (saved into file metadata)
#' @param fname fname
#' @param suffix A suffix added to the filename. NULL by default.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @export
#'
#' @examples xplot <- ggplot2::qplot(12); qqSave(ggobj = xplot); qqSave(ggobj = xplot, ext = "pdf")
#' @importFrom cowplot save_plot

qqSave <- function(ggobj, w =4, h = w
                   , ext = MarkdownHelpers::unless.specified('b.def.ext', def = 'png')
                   , also.pdf = FALSE
                   , page = c(F, "A4p", "A4l", "A5p", "A5l")[1]
                   , title = FALSE, fname = FALSE, suffix = NULL, ...) {
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

  if (also.pdf) {
    cowplot::save_plot(plot = ggobj, filename = fname2, base_width = w, base_height = h
                     , title = ww.ttl_field(title, creator = "ggExpress")
                       , ...)
  }
  cowplot::save_plot(plot = ggobj, filename = fname
                     , base_width = w, base_height = h, ...)
}


# _________________________________________________________________________________________________
#' @title  q32vA4_grid_plot
#' @description Plot up to 6 panels (3-by-2) on vertically standing A4 page.
#' @param plot_list A list of ggplot objects, each of which is one panel.
#' @param plot Show the plot? Default: F
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
                             , plotname = FixPlotName(kpp(substitute(plot_list), suffix))
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
#' @param plot Show the plot? Default: F
#' @param labels Panel labels, Default: LETTERS
#' @param max.list.length Max number of panels (per page). Default: 16
#' @param plotname Plot name, Default: Autonaming
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
                          , plotname = FixPlotName(kpp(substitute(plot_list), nrow, 'by', ncol, suffix))
                          , plot = FALSE
                          , extension = c('pdf', 'png')[2]
                          , h = hA4 * scale, w = wA4 * scale
                          , scale = 1
                          , labels = LETTERS[1:length(plot_list)]
                          , max.list.length = 16
                          , ...) { # Save 4 umaps on an A4 page.
  iprint("Plot panels on", nrow, "by", ncol, "vertical A4 page.")
  stopifnot(length(plot_list) < max.list.length)

  # if (plotname==F) plotname =  sppp(substitute(plot_list), suffix)
  fname = Stringendo::kpp(plotname, suffix, extension)
  p1 = cowplot::plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol, labels = labels, ...  ) +
    theme(plot.background=element_rect(fill="white"))
  cowplot::save_plot(plot = p1, filename = fname, base_height = h, base_width = w)
  ww.FnP_parser(fname)
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

qqqAxisLength <- function(vec = 1:20, minLength=6, factor = 0.4) {
  max(round(length(vec) * factor), minLength)
}


# _________________________________________________________________________________________________
#' @title qqqNamed.Vec.2.Tbl
#' @description Covert a named vector to a table.
#' @param namedVec namedVec
#' @param verbose verbose
#' @param strip.too.many.names strip.too.many.names
#' @param thr thr
#' @export
#'
#' @examples qqqNamed.Vec.2.Tbl(namedVec = c("A"=2, "B"=29) )


qqqNamed.Vec.2.Tbl <- function(namedVec=1:14, verbose = FALSE, strip.too.many.names = TRUE, thr = 50) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.

  # Check naming issues
  nr.uniq.names <- length(unique(names(namedVec)))
  if (nr.uniq.names > thr & verbose)  iprint("Vector has", thr, "+ names. Can mess up auto-color legends.")
  if (nr.uniq.names < 1 & verbose) print("Vector has no names")
  an.issue.w.names <- (nr.uniq.names > thr | nr.uniq.names < 1 )

  idx.elements.above.thr <- if (thr < length(namedVec)) thr:length(namedVec) else 1:length(namedVec)
  if (strip.too.many.names & an.issue.w.names) names(namedVec)[idx.elements.above.thr] <- rep("", length(idx.elements.above.thr))

  if (length(unique(names(namedVec))) > thr) iprint("Vector has", thr, "+ names. Can mess up auto-color legends.")

  df <- tibble::as_tibble(cbind("value" = namedVec))
  nm <- names(namedVec)
  df$"names" <- if (!is.null(nm)) nm else rep(".", length(namedVec))
  df
}





# _________________________________________________________________________________________________
#' @title qqqTbl.2.Vec
#' @description Covert a table to a named vector.
#' @param tibble.input tibble.input
#' @param name.column name.column
#' @param value.column value.column
#' @export
#'
#' @examples a=1:5; x= tibble::tibble(a, a * 2); qqqTbl.2.Vec(x)

qqqTbl.2.Vec <- function(tibble.input, name.column = 1, value.column = 2) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
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
#' Convert a list to a tow-column data frame to plot boxplots and violin plots
#'
#' @param ls A list with all elements named
#' @export
#' @examples LetterSets <- list("One" = LETTERS[1:7], "Two" = LETTERS[3:12]); qqqList.2.DF.ggplot(LetterSets)

qqqList.2.DF.ggplot <- function(ls = LetterSets) {
  stopifnot(CodeAndRoll2::is.list2(ls))
  MarkdownHelpers::stopif(length(ls) != length(unique(names(ls))), message = "Not all list elements have a unique name! ")
  stack(ls)[, 2:1]
}




# _________________________________________________________________________________________________
# _________________________________________________________________________________________________



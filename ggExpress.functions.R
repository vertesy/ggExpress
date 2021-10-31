######################################################################
# ggExpress is the fastest way to create, annotate and export plots in R.
######################################################################
# try(source("~/GitHub/Packages/ggExpressDev/ggExpress.functions.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpressDev/main/ggExpress.functions.R"), silent = T)

require(ggpubr)
require(cowplot)
suppressWarnings(vx <- require(MarkdownReportsDev)); if (!vx) MarkdownReports # Either version is fine, preffered dev.
 # https://github.com/vertesy/MarkdownReportsDev


######################################################################
# Auxiliary functions for ggExpress
######################################################################
try(source("~/GitHub/Packages/ggExpressDev/ggExpress.auxiliary.functions.R"))


######################################################################
# Original functions
######################################################################

# ------------------------------------------------------------------------------------------------
#' Histogram
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
#' @param logX Make X axis log10-scale.
#' @param logY Make Y axis log10-scale.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples weight <- rnorm(1000); qhistogram(vec = weight); qhistogram(vec = weight, vline = 2, filtercol = -1)

qhistogram <- function(vec, ext = "pdf", xlab = F, plot = TRUE, save = TRUE, mdlink = TRUE
                       , suffix = NULL
                       , plotname = qqqParsePlotname(vec, suffix)
                       , vline = F, filtercol = NULL
                       , logX = F, logY = F
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

  p <- gghistogram(data = df, x = "value"
                   , title = plotname, xlab = xlab
                   , add = "median"
                   # , color = "names", fill = "names"
                   , color = 'colour', fill = 'colour'
                   , palette = 'jco', ...
  ) +
    if (length(unique(df$"names")) == 1) theme(legend.position = "none")
  if (logX) p <- p + scale_x_log10()
  if (logY) p <- p + scale_y_log10()
  if (vline) p <- p + geom_vline(xintercept = vline)
  fname = kpp(plotname, suffix, "hist", flag.nameiftrue(logX), flag.nameiftrue(logY), ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}




# ------------------------------------------------------------------------------------------------
#' Density plot
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
#' @param ... ...
#'
#' @return
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

  p <- ggdensity(data = df, x = "value" # , y = "..count.."
                 , title = plotname, xlab = xlab
                 , add = "median", rug = TRUE
                 , color = "names", fill = "names"
                 , palette = 'jco', ...
  ) +
    if (length(unique(df$"names")) == 1) theme(legend.position = "none")
  if (logX) p <- p + scale_x_log10()
  if (logY) p <- p + scale_y_log10()
  fname = kpp(plotname, suffix, "dens", flag.nameiftrue(logX), flag.nameiftrue(logY),  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}



# ------------------------------------------------------------------------------------------------
#' Barplot
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
#' @param palette_use palette_use
#' @param col Color of the plot.
#' @param xlab.angle Rotate X-axis labels by N degree.
#' @param xlab X-axis label.
#' @param logY Make Y axis log10-scale.
#' @param label label
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize limitsize
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples

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
  fname <- kpp(plotname, suffix, "bar", flag.nameiftrue(logY), ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, limitsize = limitsize)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}

# weight3 <- runif (12)
# qbarplot(weight3, filtercol = -1, hline = .5)
# qbarplot(weight3, filtercol = 1, hline = .5)


# ------------------------------------------------------------------------------------------------
#' Pie chart
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
#' @param ... ...
#'
#' @return
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
  if (LegendSide) p <- ggpar(p, legend = "right", legend.title = LegendTitle)
  # if (custom.margin) p <- p + theme(plot.margin = unit(custom.margin, "cm"))

  p <- if (NoLegend) p + NoLegend() else p
  fname = kpp(plotname, suffix, "pie",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}



# ------------------------------------------------------------------------------------------------
#' Scatter plot
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
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples dfx <- as.data.frame(cbind("AA"=rnorm(12), "BB"=rnorm(12))); qscatter(dfx, suffix = "2D.gaussian")

qscatter <- function(tbl_X_Y_Col_etc
                     , suffix = NULL
                     , plotname = qqqParsePlotname(vec, suffix)
                     # , title = F
                     , col = c(NULL , 3)[1]
                     , ext = "png", also.pdf = T
                     , logX = F, logY = F
                     , hline = F, vline = F, plot = TRUE, save = TRUE, mdlink = TRUE
                     , w = 7, h = w, ...) {
  # plotname <- if (isFALSE(title)) kpp(make.names(as.character(substitute(tbl_X_Y_Col_etc))), suffix) else title
  vars <- colnames(tbl_X_Y_Col_etc)
  df <- tbl_X_Y_Col_etc
  p <- ggscatter(data = df, x = vars[1], y = vars[2], color = col
                 , title = plotname, ...) +
    grids(axis = 'xy')
  if (hline) p <- p + geom_hline(yintercept = hline)
  if (vline) p <- p + geom_vline(xintercept = vline)

  if (logX) p <- p + scale_x_log10()
  if (logY) p <- p + scale_y_log10()
  fname = kpp(plotname, suffix, "scatter", flag.nameiftrue(logX), flag.nameiftrue(logY), ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(fname)
  if (plot) p
}




# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------



# MarkdownReportsDev.R ------------------------------------------------------------------------------------------------
#' Collapse vector to a string, separated by dots
#'
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples

kpp <- function(...) { stringr::str_remove(paste(..., sep = '.', collapse = '.'), "\\.+$") } # remove trailing dots

######################################################################
# Original functions
######################################################################

# ------------------------------------------------------------------------------------------------
#' Quick-Save ggplot objects
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
#' @param ... ...
#'
#' @return
#' @export
#'
#' @examples xplot <- qplot(12); qqSave(ggobj = xplot); qqSave(ggobj = xplot, ext = "pdf")

qqSave <- function(ggobj, w =4, h = w
                   , ext =c("png", "pdf")[1], also.pdf = FALSE
                   , page = c(F, "A4p", "A4l", "A5p", "A5l")[1]
                   , title = F, fname = F, suffix = NULL, ...) {
  if (isFALSE(title)) title <- as.character(substitute(ggobj))
  if (also.pdf) fname2 <- if (isFALSE(fname)) kpp(title, suffix, 'pdf') else kpp(fname, 'pdf')
  if (isFALSE(fname)) fname <- kpp(title, suffix, ext)

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



# ------------------------------------------------------------------------------------------------
#' Insert Markdown image link to .md report
#'
#' @param file_name file_name
#'
#' @return
#' @export
#'
#' @examples qMarkdownImageLink("myplot.pdf")

qMarkdownImageLink <- function(file_name = fname) {
  if (require(MarkdownReports)) llogit(kollapse("![", file_name, "]", "(", file_name, ")", print = FALSE))
}



# ------------------------------------------------------------------------------------------------
#' Define Axis Length
#'
#' @param vec The variable to plot.
#' @param minLength minLength
#'
#' @return
#' @export
#'
#' @examples qqqAxisLength()

qqqAxisLength <- function(vec = 1:20, minLength=6) {
  max(round(length(vec)*0.2), minLength)
}


# ------------------------------------------------------------------------------------------------
#' qqqCovert.named.vec2tbl
#' Covert a named vector to a table.
#' @param namedVec namedVec
#' @param verbose verbose
#' @param strip.too.many.names strip.too.many.names
#' @param thr thr
#'
#' @return
#' @export
#'
#' @examples qqqCovert.named.vec2tbl(namedVec = c("A"=2, "B"=29) )

qqqCovert.named.vec2tbl <- function(namedVec=1:14, verbose = F, strip.too.many.names = TRUE, thr = 50) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.

  # Check naming issues
  nr.uniq.names <- length(unique(names(namedVec)))
  if (nr.uniq.names > thr & verbose)  print("Vector has", thr, "+ names. Can mess up auto-color legends.")
  if (nr.uniq.names < 1 & verbose) print("Vector has no names")
  an.issue.w.names <- (nr.uniq.names > thr | nr.uniq.names < 1 )
  if (strip.too.many.names & an.issue.w.names) names(namedVec) <- rep("x", length(namedVec))
  if (length(unique(names(namedVec))) > thr) print("Vector has", thr, "+ names. Can mess up auto-color legends.")

  df <- tibble::as_tibble(cbind("value" = namedVec))
  nm <- names(namedVec)
  df$"names" <- if (!is.null(nm)) nm else rep(".", length(namedVec))
  df
}




# ------------------------------------------------------------------------------------------------
#' qqqCovert.tbl2vec
#' Covert a table to a named vector.
#' @param tibble.input tibble.input
#' @param name.column name.column
#' @param value.column value.column
#'
#' @return
#' @export
#'
#' @examples a=1:5; x= dplyr::tibble(a, a * 2); qqqCovert.tbl2vec(x)

qqqCovert.tbl2vec <- function(tibble.input = pld.rate.HQ.UVI, name.column = 1, value.column = 2) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
  vec <- tibble.input[[value.column]]
  names(vec) <- tibble.input[[name.column]]
  vec
}

# ------------------------------------------------------------------------------------------------
#' qqqParsePlotname
#' Parse Plotname from variable name.
#' @param string string
#' @param suffix_tag suffix_tag
#'
#' @return
#' @export
#'
#' @examples qqqParsePlotname()

qqqParsePlotname <- function(string = "sadsad", suffix_tag= NULL) { # parse plot name from variable name and suffix
  nm <- make.names(as.character(substitute(string)))
  if (!is.null(suffix_tag) & !isFALSE(suffix_tag)) nm <- kpp(nm, suffix_tag)
  return(nm)
}

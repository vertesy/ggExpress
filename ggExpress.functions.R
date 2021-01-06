######################################################################
# ggExpress is the fastest way to create, annotate and export plots in R.
######################################################################
# try(source("~/GitHub/Packages/ggExpressDev/ggExpress.functions.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpressDev/main/ggExpress.functions.R"), silent = T)

require(ggpubr)
require(cowplot)


######################################################################
# Explicit dependencies on MarkdownReportsDev
######################################################################

# - llogit

######################################################################
# Duplicated functions to avoid dependencies
######################################################################


# MarkdownReportsDev.R

#' unless.specified
#'
#' Return value X (TRUE by default) unless the variable is defined.
#' If defined, it returns the variable.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @param def Default return value
#' @export
#' @examples unless.specified("xsadasf32", 2); Num = 22; unless.specified("Num", 1); unless.specified("c", 333)

unless.specified <- function(NameOfaVariable, def = TRUE) {
  if (exists(NameOfaVariable))
    get(NameOfaVariable)
  else
    def
}


# MarkdownReportsDev.R ------------------------------------------------------------------------------------------------
#' ww.ttl_field
#'
#' Internal function. Creates the string written into the PDF files "Title' (metadata) field.
#' @param flname Name of the plot
#' @param creator String X in: "plotblabla by X". Default: "ggExpress".
#' @export
#' @examples ww.ttl_field("/Users/myplot.jpg")

ww.ttl_field <- function(plotname, creator = "ggExpress") {
  paste0(basename(plotname), " by "
         , unless.specified("b.scriptname", def = creator) )
}


# MarkdownReportsDev.R ------------------------------------------------------------------------------------------------
#' percentage_formatter
#'
#' Parse a string of 0-100% from a number between 0 and 1.
#' @param x A vector of numbers between 0-1.
#' @param digitz Number of digits to keep. 3 by default.
#' @export
#' @examples percentage_formatter(x = 4.2822212, digitz = 3)

percentage_formatter <- function(x, digitz = 3) {
  a = paste(100 * signif(x, digitz), "%", sep = " ")
  a[a == "NaN %"] = NaN
  a[a == "NA %"] = NA
  return(a)
}

# MarkdownReportsDev.R ------------------------------------------------------------------------------------------------
kpp <- function(...) { paste(..., sep = '.', collapse = '.') }


######################################################################
# Original functions
######################################################################

# ------------------------------------------------------------------------------------------------
qqSave <- function(ggobj, ext =c("png", "pdf")[1], w =4, h = w
                   , page = c(F, "A4p", "A4l", "A5p", "A5l")[1]
                   , title = F, fname = F, ...) {
  if (isFALSE(title)) title = substitute(ggobj)
  if (isFALSE(fname)) fname <- kpp(title, ext)
  if (!isFALSE(page)) {
    wA4 <- 8.27
    hA4 <- 11.69
    if ( page == "A4p" ) { w = wA4; h = hA4 }
    if ( page == "A4l" ) { w = hA4; h = wA4 }
    if ( page == "A5p" ) { w = wA4/2; h = hA4/2 }
    if ( page == "A5l" ) { w = hA4/2; h = wA4/2 }
  }
  print(paste0(getwd(),"/", fname))
  # ggsave(plot = ggobj, filename = fname)
  cowplot::save_plot(plot = ggobj, filename = fname, base_width = w, base_height = h, title = ww.ttl_field(title), ...)
}
# qqSave(ggobj = qplot(12))

# ------------------------------------------------------------------------------------------------
qMarkdownImageLink <- function(file_name = fname) {
  llogit(kollapse("![", file_name, "]", "(", file_name, ")", print = FALSE))
}


# ------------------------------------------------------------------------------------------------
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
# qqqCovert.named.vec2tbl(namedVec = c("A"=2, "B"=29) )


# ------------------------------------------------------------------------------------------------
qhistogram <-  function(vec, ext = "pdf", xlab = F, vline = F, plot = TRUE, save = TRUE, mdlink = TRUE
                        , plotname = as.character(substitute(vec)), w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = 50)

  p <- gghistogram(data = df, x = "value"
                , title = plotname, xlab = xlab
                , add = "median"
                , color = "names", fill = "names"
                , palette = 'jco', ...
  ) +
  if (length(unique(df$"names")) == 1) theme(legend.position = "none")
  if (vline) p <- p + geom_vline(xintercept = vline)
  fname = kpp(plotname, "hist",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (plot) p
  if (mdlink & save) qMarkdownImageLink(fname)
}
# weight <- rnorm(1000); qhistogram(weight, vline = 3)




# ------------------------------------------------------------------------------------------------
qdensity <- function(vec, ext = "pdf", xlab = F, plot = TRUE, save = TRUE, mdlink = TRUE
                     , w = 5, h = w, ...) {
  plotname <- as.character(substitute(vec))
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = 50)

  p <- ggdensity(data = df, x = "value" # , y = "..count.."
                 , title = plotname, xlab = xlab
                 , add = "median", rug = TRUE
                 , color = "names", fill = "names"
                 , palette = 'jco', ...
  ) +
    if (length(unique(df$"names")) == 1) theme(legend.position = "none")
  fname = kpp(plotname, "dens",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (plot) p
  if (mdlink & save) qMarkdownImageLink(fname)
}
# qdensity(weight)
# qdensity(weight2)


# ------------------------------------------------------------------------------------------------
qbarplot <- function(vec, ext = "pdf", plot = TRUE, save = TRUE, mdlink = TRUE
                     , hline = F, filtercol = 1
                     , xlab.angle = 90, xlab = F
                     , w = 5, h = w, ...) {
  plotname <- as.character(substitute(vec))
  if (isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = 50)

  if (length(unique(df$"names")) == 1) df$"names" <- as.character(1:length(vec))
  # df[["col"]] <- if (hline && filtercol) ifelse(df$"value" > hline, "green", "red") else "#EFC000FF"
  df[["col"]] <- if (hline && filtercol) (df$"value" > hline) else "#EFC000FF"

  p <- ggbarplot(data = df, x = "names", y = "value"
                 , title = plotname, xlab = xlab
                 , color = "col", fill = "col"
                 , palette = 'jco', ...
  ) + grids(axis ='y') +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = xlab.angle, hjust = 1)
    )

  if (hline) p <- p + geom_hline(yintercept = hline)
  fname = kpp(plotname, "bar",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (plot) p
  if (mdlink & save) qMarkdownImageLink(fname)
}

# weight3 <- runif (12)
# qbarplot(weight3)


# ------------------------------------------------------------------------------------------------
# qpie ------------------------------------------------------------------------------------------------
qpie <- function(vec, ext = "pdf", plot = TRUE, save = TRUE, mdlink = TRUE
                 , LegendSide = T, LegendTitle = as.character(substitute(vec))
                 , plotname = as.character(substitute(vec))
                 , pcdigits = 2, NamedSlices =F
                 , w = 5, h = w, ...) {
  # plotname <- as.character(substitute(vec))
  df <- qqqCovert.named.vec2tbl(namedVec = vec, thr = 50)
  pcX <- df$"value" / sum(df$"value")
  labs <- paste(100 * signif (pcX, pcdigits), "%", sep = "")
  if (NamedSlices) labs <- paste(df$names, "\n", labs)

  p <- ggpubr::ggpie(data = df, x = "value", label = labs
                     , fill = "names", color = "white"
                     , title = plotname
                     , palette = 'jco', ...)
  if (LegendSide) p <- ggpar(p, legend = "right", legend.title = LegendTitle)
  fname = kpp(plotname, "pie",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (plot) p
  if (mdlink & save) qMarkdownImageLink(fname)
}
# xvec <- c("A"=12, "B"=29); qpie(vec = xvec)


# qscatter ------------------------------------------------------------------------------------------------
qscatter <- function(tbl_X_Y_Col_etc, ext = "pdf", suffix = ""
                     , hline = F, vline = F, plot = TRUE, save = TRUE, mdlink = TRUE
                     , w = 7, h = w, ...) {

  plotname <- kpp(as.character(substitute(tbl_X_Y_Col_etc)), suffix)
  vars <- colnames(tbl_X_Y_Col_etc)

  df <- tbl_X_Y_Col_etc

  p <- ggscatter(data = df, x = vars[1], y = vars[2], color = vars[3]
                 , title = plotname, ...) +
    grids(axis ='xy')
  if (hline) p <- p + geom_hline(yintercept = hline)
  if (vline) p <- p + geom_hline(xintercept = vline)
  fname = kpp(plotname, "scatter",  ext)
  if (save) qqSave(ggobj = p, title = plotname, fname = fname, ext = ext, w = w, h = h)
  if (plot) p
  if (mdlink & save) qMarkdownImageLink(fname)
}
# qscatter(tbl_X_Y_Col_etc = Jaccard.vs.CellCount, suffix = "Star"
#          , ellipse = F, mean.point = TRUE, star.plot = TRUE)


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



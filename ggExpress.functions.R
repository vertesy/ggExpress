# try(source("~/GitHub/Packages/ggExpressDev/ggExpress.functions.R"), silent = T)

require(ggpubr)
require(cowplot)

# ------------------------------------------------------------------------------------------------
# percentage_formatter ------------------------------------------------------------------------------------------------
#' percentage_formatter
#'
#' Parse a string of 0-100% from a number between 0 and 1.
#' @param x A vector of numbers between 0-1.
#' @param digitz Number of digits to keep. 3 by default.
#' @export
#' @examples percentage_formatter (x = 4.2822212, digitz = 3)

percentage_formatter <- function(x, digitz = 3) {
  a = paste(100 * signif(x, digitz), "%", sep = " ")
  a[a == "NaN %"] = NaN
  a[a == "NA %"] = NA
  return(a)
}

# ------------------------------------------------------------------------------------------------
kpp <- function(...) { paste(..., sep = '.', collapse = '.') }

# qqSave ------------------------------------------------------------------------------------------------
qqSave <- function(ggobj, ext =c("png", "pdf")[2], w =4, h = w
                   , page = c(F, "A4p", "A4l", "A5p", "A5l")[1]
                   , title = F, fname = F, ...) {
  if(isFALSE(title)) title = substitute(ggobj)
  if(isFALSE(fname)) fname <- kpp(title, ext)
  if(!isFALSE(page)) {
    wA4 <-8.27
    hA4 <- 11.69
    if ( page == "A4p" ) { w = wA4; h = hA4}
    if ( page == "A4l" ) { w = hA4; h = wA4}
    if ( page == "A5p" ) { w = wA4/2; h = hA4/2}
    if ( page == "A5l" ) { w = hA4/2; h = wA4/2}
  }
  print(paste0(getwd(),"/", fname))
  cowplot::save_plot(plot = ggobj, filename = fname, base_width = w, base_height = h, ...)
}
# qqSave(ggobj = qplot(12))


# qqqCovert.named.vec2tbl ------------------------------------------------------------------------------------------------
qqqCovert.named.vec2tbl <- function(namedVec=1:14) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
  df <- tibble::as_tibble(cbind("value" = namedVec))
  nm <- names(namedVec)
  df$"names" <- if(!is.null(nm)) nm else rep(".", length(namedVec))
  df
}
# qqqCovert.named.vec2tbl(namedVec = c("A"=2, "B"=29) )

# ------------------------------------------------------------------------------------------------
qhistogram <-  function(vec, ext = "pdf", xlab = F, vline = F, plot = TRUE, ...) {
  plotname <- as.character(substitute(vec))
  if(isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec)
  p <- gghistogram(data = df, x = "value"
                , title = plotname, xlab = xlab
                , add = "median"
                , color = "names", fill = "names"
                , palette = 'jco', ...
  ) +
  if (length(unique(df$"names")) == 1) theme(legend.position = "none")
  if (vline) p <- p + geom_vline(xintercept = vline)
  fname = kpp(plotname, "hist",  ext)
  qqSave(ggobj = p, title = plotname, fname = fname)
  if (plot) p
}
# qhistogram(weight2, vline = 60)
# qhistogram(weight)



# ------------------------------------------------------------------------------------------------
qdensity <- function(vec, ext = "pdf", xlab = F, plot = TRUE, ...) {
  plotname <- as.character(substitute(vec))
  if(isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec)

  p <- ggdensity(data = df, x = "value" # , y = "..count.."
                 , title = plotname, xlab = xlab
                 , add = "median", rug = TRUE
                 , color = "names", fill = "names"
                 , palette = 'jco', ...
  ) +
    if (length(unique(df$"names")) == 1) theme(legend.position = "none")
  fname = kpp(plotname, "dens",  ext)
  qqSave(ggobj = p, title = plotname, fname = fname)
  if (plot) p
}
# qdensity(weight)
# qdensity(weight2)


# ------------------------------------------------------------------------------------------------
qbarplot <- function(vec, ext = "pdf", xlab = F, hline = F, plot = TRUE, ...) {
  plotname <- as.character(substitute(vec))
  if(isFALSE(xlab)) xlab = plotname
  df <- qqqCovert.named.vec2tbl(namedVec = vec)
  if (length(unique(df$"names")) == 1) df$"names" <- as.character(1:length(vec))
  p <- ggbarplot(data = df, x = "names", y = "value"
                 , title = plotname, xlab = xlab
                 , color = "#EFC000FF", fill = "#EFC000FF"
                 , palette = 'jco', ...
  ) + grids(axis ='y') +
    theme(legend.position = "none")
  if (hline) p <- p + geom_hline(yintercept = hline)
  fname = kpp(plotname, "bar",  ext)
  qqSave(ggobj = p, title = plotname, fname = fname)
  if (plot) p
}
# weight3 <- weight2[1:12]
# qbarplot(weight2)
#
# qbarplot(weight3)
# get_palette("jco", k=1)

# ------------------------------------------------------------------------------------------------
# qpie ------------------------------------------------------------------------------------------------
qpie <- function(vec, ext = "pdf", plot = TRUE, w = 5, h = w
                 , LegendSide = T, LegendTitle = as.character(substitute(vec))
                 , plotname = as.character(substitute(vec))
                 , pcdigits = 2, NamedSlices =F , ...) {
  # plotname <- as.character(substitute(vec))
  df <- qqqCovert.named.vec2tbl(namedVec = vec)
  if (length(unique(df$"names")) == 1) df$"names" <- as.character(1:length(vec))
  pcX <- df$"value" / sum(df$"value")
  labs <- paste(100 * signif(pcX, pcdigits), "%", sep = "")
  if (NamedSlices) labs <- paste(df$names, "\n", labs)

  p <- ggpubr::ggpie(data = df, x = "value", label = labs
                     , fill = "names", color = "white",
                     , title = plotname
                     , palette = 'jco', ...)
  if (LegendSide) p <- ggpar(p, legend = "right", legend.title = LegendTitle)
  fname = kpp(plotname, "pie",  ext)
  qqSave(ggobj = p, title = plotname, fname = fname, w = w, h = h)
  if (plot) p
}
# xvec <- c("A"=12, "B"=29); qpie(vec = xvec)


# qscatter ------------------------------------------------------------------------------------------------
qscatter <- function(tbl_X_Y_Col_etc, ext = "pdf", suffix = ""
                     , hline = F, vline = F, plot = TRUE, width = 7, height =  width
                     , ...) {

  plotname <- kpp(as.character(substitute(tbl_X_Y_Col_etc)), suffix)
  vars <- colnames(tbl_X_Y_Col_etc)

  df <- tbl_X_Y_Col_etc

  p <- ggscatter(data = df, x = vars[1], y = vars[2], color = vars[3]
                 , title = plotname, ...) +
    grids(axis ='xy')
  if (hline) p <- p + geom_hline(yintercept = hline)
  if (vline) p <- p + geom_hline(xintercept = vline)
  fname = kpp(plotname, "scatter",  ext)
  qqSave(ggobj = p, title = plotname, fname = fname, w = width, h = height)
  if (plot) p
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



# iprint <- function (...) {
#   argument_list <- c(...)
#   print(paste(argument_list, collapse = " "))
# }

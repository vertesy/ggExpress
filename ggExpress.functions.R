# try(source("~/GitHub/Packages/ggExpressDev/ggExpress.functions.R"), silent = T)

require(ggpubr)
require(cowplot)

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
kpp <- function(...) { paste(..., sep = '.', collapse = '.') }



# ------------------------------------------------------------------------------------------------
qqCovert.hist <- function(namedVec=1:14) {
  df <- tibble::as.tibble(cbind("value" = namedVec))
  nm <- names(namedVec)
  df$"names" <- if(!is.null(nm)) nm else rep(".", length(namedVec))
  df
}
# qqCovert.hist()

# ------------------------------------------------------------------------------------------------
shistogram <-  function(vec, ext = "pdf", xlab = F, vline = F, plot = TRUE, ...) {
  plotname <- as.character(substitute(vec))
  if(isFALSE(xlab)) xlab = plotname
  df <- qqCovert.hist(namedVec = vec)
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
# shistogram(weight2, vline = 60)
# shistogram(weight)



# ------------------------------------------------------------------------------------------------
sdensity <- function(vec, ext = "pdf", xlab = F, plot = TRUE, ...) {
  plotname <- as.character(substitute(vec))
  if(isFALSE(xlab)) xlab = plotname
  df <- qqCovert.hist(namedVec = vec)

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
# sdensity(weight)
# sdensity(weight2)


# ------------------------------------------------------------------------------------------------
sbarplot <- function(vec, ext = "pdf", xlab = F, hline = F, plot = TRUE, ...) {
  plotname <- as.character(substitute(vec))
  if(isFALSE(xlab)) xlab = plotname
  df <- qqCovert.hist(namedVec = vec)
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
# sbarplot(weight2)
#
# sbarplot(weight3)
# get_palette("jco", k=1)

# ------------------------------------------------------------------------------------------------
# sscatter ------------------------------------------------------------------------------------------------
sscatter <- function(tbl_X_Y_Col_etc, ext = "pdf", suffix = ""
                     , hline = F, vline = F, plot = TRUE
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
  qqSave(ggobj = p, title = plotname, fname = fname)
  if (plot) p
}
# sscatter(tbl_X_Y_Col_etc = Jaccard.vs.CellCount, suffix = "Star"
#          , ellipse = F, mean.point = TRUE, star.plot = TRUE)


# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
## -------------------------------------------------------------------------------------------------
qqSaveA4p <- function(ggobj, ext =c("png", "pdf")[1],
                      title = substitute(ggobj), fname = F, ...) {
  if(isFALSE(title)) title = substitute(ggobj)
  if(isFALSE(fname)) fname <- kpp(title, ext)

  save_plot(plot = ggobj, filename = kpp(title, ext), base_height = hA4, base_width = wA4, ...)
}


qqSaveA4l <- function(ggobj, ext =c("png", "pdf")[1],
                      title = substitute(ggobj), fname = F, ...) {
    if(isFALSE(title)) title = substitute(ggobj)
    if(isFALSE(fname)) fname <- kpp(title, ext)
    save_plot(plot = ggobj, filename = kpp(title, ext), base_height = wA4, base_width =hA4, ...)
}


qqSaveA5l <- function(ggobj, ext =c("png", "pdf")[1],
                      title = substitute(ggobj), fname = F, ...) {
  if(isFALSE(title)) title = substitute(ggobj)
  if(isFALSE(fname)) fname <- kpp(title, ext)
  save_plot(plot = ggobj, filename = kpp(title, ext), base_height = hA4/2, base_width = 8.27, ...)
}

# ------------------------------------------------------------------------------------------------
qqSave <- function(ggobj, ext =c("png", "pdf")[2], w =4, h = w
                   , title = substitute(ggobj), fname = F, ...) {
  if(isFALSE(title)) title = substitute(ggobj)
  if(isFALSE(fname)) fname <- kpp(title, ext)
  print(paste0(getwd(),"/", fname))
  cowplot::save_plot(plot = ggobj, filename = fname, base_height = w, base_width = h, ...)
}
# qqSave(ggobj = qplot(12))


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

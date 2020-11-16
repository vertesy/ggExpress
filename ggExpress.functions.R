require(ggpubr)
require(cowplot)
setwd("/Users/abel.vertesy/Dropbox/Abel.IMBA/Zacc/ggExpress/")

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
kpp <- function(...) { paste(..., sep = '.', collapse = '.') }


# ------------------------------------------------------------------------------------------------
qqSave <- function(ggobj, ext =c("png", "pdf")[2], w =4, h = w
                   , title = F, fname = F, ...) {
  if(isFALSE(title)) title = substitute(ggobj)
  if(isFALSE(fname)) fname <- kpp(title, ext)
  print(paste0(getwd(),"/", fname))
  save_plot(plot = ggobj, filename = fname, base_height = w, base_width = h, ...)
}
# qqSave(ggobj = qplot(12))

# ------------------------------------------------------------------------------------------------
qqCovert.hist <- function(namedVec=1:14) {
  df <- tibble::as.tibble(cbind("value" = namedVec))
  nm <- names(namedVec)
  df$"names" <- if(!is.null(nm)) nm else rep(".", length(namedVec))
  df
}
# qqCovert.hist()

# ------------------------------------------------------------------------------------------------
shistogram <-  function(vec, ext = "pdf", xlab = F, vline = F, ...) {
  plotname <- as.character(substitute(vec))
  if(isFALSE(xlab)) xlab = plotname
  df <- qqCovert.hist(namedVec = vec)
  p <- gghistogram(data = df, x = "value"
                , title = plotname, xlab = xlab
                , add = "median"
                , color = "names", fill = "names"
                , palette = 'jco', ...
  ) +
    # if (vline) geom_vline(xintercept = vline) +
    if (length(unique(df$"names")) == 1) theme(legend.position = "none")

  fname = kpp(plotname, "hist",  ext)
  qqSave(ggobj = p, title = plotname, fname = fname)
  p
}
shistogram(weight2, vline = 60)
shistogram(weight)



# ------------------------------------------------------------------------------------------------
sdensity <- function(vec, ext = "pdf", xlab = F, ...) {
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
  p
}
sdensity(weight)
sdensity(weight2)


# ------------------------------------------------------------------------------------------------
sbarplot <- function(vec, ext = "pdf", xlab = F, ...) {
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

  fname = kpp(plotname, "bar",  ext)
  qqSave(ggobj = p, title = plotname, fname = fname)
  p
}
weight3 <- weight2[1:12]
sbarplot(weight2)

sbarplot(weight3)
get_palette("jco", k=1)

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

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

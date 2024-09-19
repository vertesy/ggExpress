# ____________________________________________________________________
# ggExpress is the fastest way to create, annotate and export plots in R.  ----
# ____________________________________________________________________
# devtools::load_all("~/GitHub/Packages/ggExpress"); # devtools::document("~/GitHub/Packages/ggExpress")
# try(source("~/GitHub/Packages/ggExpress/R/ggExpress.R"), silent = TRUE)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpress/main/ggExpress.R"), silent = TRUE)
# source('~/.pack.R')

# ______________________________________________________________________________________________----
# Simple plots  ----
# ____________________________________________________________________


#' @title Quick Histogram Plotting
#'
#' @description This function generates a histogram and saves the plot for a given vector and offers several customizations.
#' @param vec A numeric vector for which the histogram is to be plotted.
#' @param ext File extension for the saved plot. Either '.pdf' or '.png'. Default is '.pdf'.
#' @param also.pdf Save plot in both png and pdf formats.
#' @param xlab Label for the X-axis. By default, it uses the plot name.
#' @param plot Logical indicating whether to display the plot. Default is TRUE.
#' @param add Character defining the type of plot annotations to add. Default is 'median'.
#' @param save Logical indicating whether to save the plot into a file. Default is TRUE.
#' @param mdlink Logical indicating whether to insert .pdf and .png image links in the markdown report, set by "path_of_report". Default is FALSE.
#' @param plotname Title of the plot and the name of the file (unless specified in `filename`). Default is parsed from `vec`.
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Optional filename for the saved plot. Default is parsed from `plotname`.
#' @param vline Numeric value at which to draw a vertical line on the plot. Default is FALSE (no line).
#' @param filtercol Numeric value indicating the direction to color bars above/below the threshold. Default is 0 (no color change).
#' @param palette_use Color palette to use from GGpubr. Default is 'jco'.
#' @param col Color of the plot. Default is '1'.
#' @param xlab.angle Angle to rotate X-axis labels. Default is 90 degrees.
#' @param hide.legend Logical indicating whether to hide the legend. Default is TRUE.
#' @param max.names Maximum number of names to show on the axis. Default is 50.
#' @param annotation_logticks_X Logical indicating whether to add annotation logticks on X-axis. Default follows the value of `logX`.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param logX Logical indicating whether to make X axis on log10 scale. Default is FALSE.
#' @param logY Logical indicating whether to make Y axis on log10 scale. Default is FALSE.
#' @param w Width of the plot. Default is 5.
#' @param h Height of the plot. Default is the same as width.
#' @param ... Additional parameters for the corresponding plotting function.
#' @return It returns a ggplot object if `plot` is TRUE.
#' @examples
#' \dontrun{
#' weight <- rnorm(1000)
#' qhistogram(vec = weight)
#' qhistogram(vec = weight, vline = 2, filtercol = -1)
#' }
#'
#' @export
qhistogram <- function(
    vec,
    also.pdf = FALSE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    xlab = FALSE, plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    plotname = FixPlotName(substitute(vec)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,
    logX = FALSE, logY = FALSE,
    annotation_logticks_X = logX, annotation_logticks_Y = logY,
    vline = FALSE, filtercol = 0,
    add = "median",
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    col = as.character(1:3)[1],
    xlab.angle = 90,
    hide.legend = TRUE,
    max.names = 50,
    grid = "y",
    w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab <- plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)

  df[["colour"]] <- if (!isFALSE(vline) & filtercol != 0) {
    if (filtercol == 1) (df$"value" > vline) else if (filtercol == -1) (df$"value" < vline)
  } else if (length(col) == length(vec)) {
    as.character(col)
  } else {
    as.character(rep(col, length(vec))[1:length(vec)])
  }

  p <- ggpubr::gghistogram(
    data = df, x = "value",
    title = plotname, xlab = xlab,
    add = add
    # , color = "names", fill = "names"
    , subtitle = subtitle,
    caption = caption,
    color = "colour", fill = "colour",
    palette = palette_use, ...
  ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)) +
    if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (annotation_logticks_X) p <- p + annotation_logticks(sides = "b")

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logX), flag.nameiftrue(logY), "hist", ext)
  }
  file_name <- FixPlotName(file_name)

  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}



# _________________________________________________________________________________________________
#' @title qdensity
#'
#' @description Draw and save a density plot.
#' @param vec The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param xlab X-axis label.
#' @param plot Display the plot.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param logX Make X axis log10-scale.
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param palette_use GGpubr Color palette to use.
#' @param hide.legend hide legend
#' @param logY Make Y axis log10-scale.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples weight <- rnorm(1000)
#' qdensity(weight)
qdensity <- function(
    vec,
    also.pdf = FALSE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    xlab = FALSE, plot = TRUE,
    plotname = FixPlotName(substitute(vec)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,
    save = TRUE, mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    logX = FALSE, logY = FALSE,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    xlab.angle = 90,
    hide.legend = TRUE,
    max.names = 50,
    grid = F,
    w = 5, h = w, ...) {
  if (isFALSE(xlab)) xlab <- plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)

  p <- ggpubr::ggdensity(
    data = df, x = "value" # , y = "..count.."
    , title = plotname, xlab = xlab,
    add = "median", rug = TRUE,
    color = "names", fill = "names",
    subtitle = subtitle,
    caption = caption,
    palette = palette_use, ...
  ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)) +
    if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")
  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logX), flag.nameiftrue(logY), "dens", ext)
  }
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title qpie
#'
#' @description Draw and save a pie chart
#' @param vec The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
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
#' @param label Slice labels. Set to NULL to remove slice names.
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples xvec <- c("A" = 12, "B" = 29)
#' qpie(vec = xvec)
qpie <- function(
    vec = MyVec,
    also.pdf = FALSE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    plot = TRUE, save = TRUE,
    mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    plotname = FixPlotName(substitute(vec)),
    filename = NULL,
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    NoLegend = FALSE,
    LegendSide = TRUE,
    LegendTitle = plotname,
    pcdigits = 2, NamedSlices = FALSE,
    custom.order = FALSE,
    extended.canvas = TRUE,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    custom.margin = TRUE,
    max.categories = 100,
    max.names = 10,
    decr.order = TRUE,
    both_pc_and_value = FALSE,
    labels = "names", # Set to NULL to remove slice names.
    w = 7, h = 5,
    ...) {
  print(plotname)
  l.orig <- length(vec)
  sum.orig <- sum(vec)

  # Plot annotation ________________________________________________
  st <- paste("Sum:", sum.orig)
  subtitle <- if (is.null(subtitle)) st else paste0(subtitle, "\n", st)

  ct <- paste0("Total elements:", l.orig, "; shown:", (max.categories - 1), " | max.names:", max.names)
  caption <- if (is.null(caption)) ct else paste0(caption, "\n", ct)

  # ________________________________________________
  if (l.orig > max.categories) {
    iprint("Warning, there are more than", max.categories, "categories. Only the top", max.categories - 1, "items are show, the rest is added up.")
    sv <- sort(vec, decreasing = TRUE)
    vec.new <- sv[1:(max.categories - 1)]
    idx.remaining <- max.categories:length(vec)
    sum.of.remaining <- sum(sv[idx.remaining])
    fr.sum <- percentage_formatter(sum.of.remaining / sum(vec))
    iprint("The remaining", length(idx.remaining), "values make up", fr.sum, "of the data.")

    vec.new[max.categories] <- sum.of.remaining
    name.of.last <- paste("Sum of rem.", length(idx.remaining))
    names(vec.new)[max.categories] <- name.of.last
    vec <- vec.new
  }

  if (is.null(names(vec))) {
    names(vec) <- as.character(1:length(vec))
  }

  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)
  if (l.orig > max.categories) df[["names"]][length(df$"names")] <- name.of.last

  pcX <- df$"value" / sum(df$"value")
  labs <- paste(100 * signif(pcX, pcdigits), "%", sep = "")

  idx.named <- which(!df$"names" == "")
  df$"names"[idx.named] <- paste(df$"names"[idx.named], labs[idx.named], sep = "\n")

  # print('- -')
  if (both_pc_and_value) df$"names"[idx.named] <- paste(df$"names"[idx.named], df$"value"[idx.named], sep = "\n")

  if (decr.order) df[["names"]] <- factor(df$"names", levels = rev(make.unique(df$"names")))

  nrCategories.DFcol1 <- length(unique(df[, 1]))
  stopif(nrCategories.DFcol1 > max.categories)
  print(nrCategories.DFcol1)

  if (NamedSlices) labs <- paste(df$names, "\n", labs)
  if (custom.order != F) df$"names" <- factor(df$"names", levels = custom.order)

  p <- ggpubr::ggpie(
    data = df,
    x = "value",
    label = labels,
    subtitle = subtitle,
    caption = caption,
    fill = "names",
    color = "white",
    title = plotname,
    palette = palette_use
    # , ...
    ) +
    guides(fill = guide_legend(LegendTitle))
    # theme(legend.title = LegendTitle)

  if (LegendSide) p <- ggpubr::ggpar(p, legend = "right")
  if (custom.margin) p <- p + coord_polar(theta = "y", clip = "off")

  p <- if (NoLegend) p + theme(legend.position = "none", validate = TRUE) else p

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, "pie", ext)
  }

  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)

  if (plot) p
}



# _________________________________________________________________________________________________
#' @title qbarplot
#'
#' @description Draw and save a barplot.
#' @param vec The variable to plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param plot Display the plot.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
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
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ylim ylimit values
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @export
#'
#' @examples weight3 <- runif(12)
#' qbarplot(weight3, filtercol = -1, hline = .5)
#' qbarplot(weight3, filtercol = 1, hline = .5)
qbarplot <- function(
    vec,
    also.pdf = FALSE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    plot = TRUE,
    plotname = FixPlotName(substitute(vec)),
    subtitle = paste("Median:", iround(median(vec))),
    suffix = NULL,
    caption = suffix,
    filename = NULL,
    save = TRUE, mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    hline = FALSE, filtercol = 1,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    col = as.character(1:3)[1],
    xlab = "", xlab.angle = 45,
    logY = FALSE,
    ylim = c(0, 1.1 * as.numeric(max(vec, na.rm = TRUE))),
    annotation_logticks_Y = logY,
    label = NULL,
    hide.legend = TRUE,
    max.names = 50,
    limitsize = FALSE,
    grid = "y",
    ylab = NULL,
    w = qqqAxisLength(vec), h = 5,
    ...) {

  stopifnot(is.numeric(vec))
  if (isFALSE(xlab)) xlab <- plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, strip.too.many.names = F)

  if (length(unique(df$"names")) == 1) df$"names" <- as.character(1:length(vec))

  df[["colour"]] <-
    if (length(col) == length(vec)) {
      as.character(col)
    } else if (hline & filtercol != 0) {
      if (filtercol == 1) (df$"value" > hline) else if (filtercol == -1) (df$"value" < hline)
    } else {
      as.character(rep(col, length(vec))[1:length(vec)])
    }
  print(df)


  p <- ggpubr::ggbarplot(
    data = df, x = "names", y = "value",
    title = plotname, xlab = xlab,
    subtitle = subtitle,
    caption = caption,
    color = "colour", fill = "colour",
    label = label,
    ylim = ylim,
    palette = palette_use,
    ...
  ) +
    ggplot2::labs(y = ylab) +
    ggpubr::grids(axis = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))


  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)

  if (length(vec) > max.names) p <- p + ggplot2::guides(x = "none")
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logY), "bar", ext)
  }

  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name, ext = ext,
      w = w, h = h, limitsize = limitsize, also.pdf = also.pdf
    )
  }
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title qbarplot.stacked.from.wide.df - Barplot for tibbles or dataframes
#'
#' @description Draw and save a stacked barplot for each row of a dataframe.
#' @param df The variable to plot.
#' @param x Colname to split along X axis. Def colnames(df)[1]
#' @param y Colname to count along y axis. Def colnames(df)[3]
#' @param fill Color (split) by along Y.
#' @param color Color (split) by along Y.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param hline Draw a horizontal line on the plot.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by -1 or 1. Takes effect if "*line" is defined.
#' @param palette_use GGpubr Color palette to use.
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param xlab X-axis label.
#' @param logY Make Y axis log10-scale.
#' @param label label
#' @param hide.legend hide legend
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize limitsize
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @examples
#' # Example of wide-format data for stacked bar plot
#' df.SingletSplit <- tibble::tibble(
#'   doublet = c(0.027886224, 0.007699141, 0.003704390, 0.003205128),
#'   singlet = c(0.9686280, 0.9872668, 0.9925912, 0.9119822),
#'   unassigned = c(0.0034857780, 0.0050340539, 0.0037043897, 0.0848126233)
#' )
#' rownames(df.SingletSplit) <- c("sc06.692", "sc06.693", "sc08.325", "sc08.327")
#'
#' # Create the stacked bar plot using the new qbarplot.df2 function
#' qbarplot.stacked.from.wide.df(df.SingletSplit)
#'
#' @export qbarplot.stacked.from.wide.df

qbarplot.stacked.from.wide.df <- function(
    df,
    x = "Samples",
    y = "Fraction",
    z = "Category",
    # fill = colnames(df)[3],
    color = 1,
    label = NULL,
    also.pdf = FALSE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    plotname = FixPlotName(substitute(df)),
    subtitle = NULL, suffix = NULL, caption = suffix,
    filename = NULL,
    scale = TRUE,
    plot = TRUE,
    save = TRUE,
    mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    hline = FALSE, filtercol = 1,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    xlab.angle = 45, xlab = x,
    logY = FALSE,
    annotation_logticks_Y = logY,
    hide.legend = FALSE,
    max.names = 50,
    limitsize = FALSE,
    grid = "y",
    max.categ = 10,
    top = NULL,
    w = qqqAxisLength(df, factor = .7), h = 5,
    ...) {

  message(plotname)
  stopifnot(is.data.frame(df),
            ncol(df) > 2)

  # if (is.null(xlab)) xlab <- if (scale) paste("%", x ) else x
  if (is.null(subtitle)) subtitle <- paste("Median:", iround(median(df[[2]])))

  # if (is.numeric(df[[fill]])) {
  #   df[[fill]] <- as.factor(df[[fill]])
  # }

  df_long <- df |>
    tibble::rownames_to_column(var = x) |>  # Convert row names to a column
    tidyr::pivot_longer(cols = -!!sym(x),      # Convert wide to long format
                        names_to = z, #"category"
                        values_to = y # "Fraction"
    )

  p <- ggpubr::ggbarplot(
    data = df_long, x = x, y = y,
    color = color,
    fill = z,   # Use the 'category' column created in long format
    subtitle = subtitle,
    title = plotname, xlab = xlab,
    caption = caption,
    label = label,
    palette = palette_use,
    position = if(scale) position_fill() else position_stack(),
    ...
  ) +
    ggpubr::grids(axis = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)

  if (length(df) > max.names) p <- p + ggplot2::guides(x = "none")
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")
  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logY), "bar", ext)
  }
  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name, ext = ext,
      w = w, h = h, limitsize = limitsize, also.pdf = also.pdf
    )
  }
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title qbarplot.df - Barplot for tibbles or dataframes
#'
#' @description Draw and save a barplot for tibbles or dataframes
#' @param df The variable to plot.
#' @param x Colname to split along X axis. Def colnames(df)[1]
#' @param y Colname to count along y axis. Def colnames(df)[3]
#' @param fill Color (split) by along Y.
#' @param color Color (split) by along Y.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param hline Draw a horizontal line on the plot.
#' @param filtercol Color bars below / above the threshold with red / green. Define the direction by -1 or 1. Takes effect if "*line" is defined.
#' @param palette_use GGpubr Color palette to use.
#' @param xlab.angle Rotate X-axis labels by N degree. Default: 90
#' @param xlab X-axis label.
#' @param logY Make Y axis log10-scale.
#' @param label label
#' @param hide.legend hide legend
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize limitsize
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @examples my_tibble <- tibble(
#'   Column_1 = c("A", "A", "A", "B", "C", "C"),
#'   Column_2 = c("X", "Y", "Y", "Z", "X", "Z")
#' )
#' freq_table <- my_tibble |> count(Column_1, Column_2)
#' qbarplot.df(freq_table)
#'
#' @export qbarplot.df

qbarplot.df <- function(
    df,
    x = colnames(df)[1],
    y = colnames(df)[2],
    fill = colnames(df)[3],
    color = 1,
    label = NULL,
    also.pdf = FALSE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    plotname = FixPlotName(substitute(df)),
    subtitle = NULL, suffix = NULL, caption = suffix,
    filename = NULL,
    scale = TRUE,
    plot = TRUE,
    save = TRUE,
    mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    hline = FALSE, filtercol = 1,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    xlab.angle = 45, xlab = NULL,
    logY = FALSE,
    annotation_logticks_Y = logY,
    hide.legend = TRUE,
    max.names = 50,
    limitsize = FALSE,
    grid = "y",
    max.categ = 10,
    top = NULL,
    w = qqqAxisLength(df), h = 5,
    ...) {

  message(plotname)
  stopifnot(is.data.frame(df),
            ncol(df) > 2)

  if (is.null(xlab)) xlab <- if (scale) paste("%", x ) else x
  if (is.null(subtitle)) subtitle <- paste("Median:", iround(median(df[[2]])))

  if (is.numeric(df[[fill]])) {
    df[[fill]] <- as.factor(df[[fill]])
    nr_categ <- length(unique(df[[fill]]))
    stopifnot("3rd column has too many categories" = nr_categ < max.categ)
  }

  p <- ggpubr::ggbarplot(
    data = df, x = x, y = y,
    color = color,
    fill = fill,
    subtitle = subtitle,
    title = plotname, xlab = xlab,
    caption = caption,
    label = label,
    palette = palette_use,
    position = if(scale) position_fill() else position_stack(),
    ...
  ) +
    ggpubr::grids(axis = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)

  if (length(df) > max.names) p <- p + ggplot2::guides(x = "none")
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")
  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logY), "bar", ext)
  }
  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name, ext = ext,
      w = w, h = h, limitsize = limitsize, also.pdf = also.pdf
    )
  }
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title qscatter
#'
#' @description Draw and save a 2D-scatter plot.
#' @param df_XYcol Data, as 2 column data frame, where col.1 is X axis.
#' @param plotname The name of the file and title of the plot.
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param label Point labels
#' @param col Color of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
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
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param annotation_logticks_X Logical indicating whether to add annotation logticks on X-axis. Default follows the value of `logX`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#' @examples dfx <- as.data.frame(cbind("AA" = rnorm(500), "BB" = rnorm(500)))
#' qscatter(dfx, suffix = "2D.gaussian")
#'
#' @export
qscatter <- function(
    df_XYcol,
    x = 1, y = 2,
    plotname = FixPlotName(substitute(df_XYcol)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,
    col = c(NULL, 3)[1],
    label = NULL, repel = TRUE,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    hide.legend = FALSE,
    also.pdf = TRUE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    logX = FALSE, logY = FALSE,
    annotation_logticks_Y = logY,
    annotation_logticks_X = logX,
    xlab.angle = 90,
    hline = FALSE, vline = FALSE, abline = FALSE,
    add_contour_plot = FALSE,
    correlation_r2 = FALSE, # add as  c("pearson", "spearman")
    plot = TRUE, save = TRUE,
    mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    grid = "xy",
    # pt.size = NULL,
    w = 7, h = w,
    ...) {
  #
  print(plotname)
  stopifnot(ncol(df_XYcol) >= 2)
  if (is.matrix(df_XYcol)) df_XYcol <- as.data.frame(df_XYcol)

  vars <- colnames(df_XYcol)
  cat("Variable (column) names 1-5:", head(vars), "...\n")

  p <- ggpubr::ggscatter(
    data = df_XYcol, x = vars[x], y = vars[y],
    title = FixPlotName(plotname, suffix),
    subtitle = subtitle,
    caption = caption,
    palette = palette_use,
    color = col,
    label = label, repel = repel,
    # size = pt.size,
    ...
  ) +
    ggpubr::grids(axis = "xy") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (sum(hline)) p <- p + ggplot2::geom_hline(yintercept = hline) # sum is needed to deal with multiple lines (multiple values in if statement).
  if (sum(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)
  if (sum(abline)) p <- p + ggplot2::geom_abline(intercept = abline[1], slope = abline[2])
  if (add_contour_plot) p <- p + geom_density_2d()
  if (correlation_r2 %in% c("pearson", "spearman")) p <- p + stat_cor(method = correlation_r2)

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (annotation_logticks_X) p <- p + annotation_logticks(sides = "b")

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logX), flag.nameiftrue(logY), "scatter", ext)
  }
  if (plot) p
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)
  p
}




# ______________________________________________________________________________________________----
# List-based distributio plots ----
# ____________________________________________________________________



#' @title qboxplot
#'
#' @description Draw and save a boxplot
#' @param df_XYcol_or_list Data, as 2 column data frame, where col.1 is X axis, alternatively a uniquely named list ov values.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param outlier.shape outlier shape. NA to hide.
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
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @importFrom CodeAndRoll2 is.list2
#' @export
#' @examples data("ToothGrowth")
#' ToothLen.by.Dose <- ToothGrowth[, c("dose", "len")]
#' qboxplot(ToothLen.by.Dose)
qboxplot <- function(
    df_XYcol_or_list,
    x = 1, y = 2, col = NULL,
    fill = "gold",
    plotname = FixPlotName(substitute(df_XYcol_or_list)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,
    outlier.shape = NULL,
    stat.test = TRUE
    # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
    , stat.method = NULL, stat.label.y.npc = "top", stat.label.x = NULL,
    # , fill = c(NULL , 3)[1]
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    hide.legend = FALSE,
    also.pdf = TRUE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    ylab = NULL, # xlab = NULL,
    logY = FALSE, # , logX = FALSE
    annotation_logticks_Y = logY,
    xlab.angle = 90,
    hline = FALSE, vline = FALSE,
    plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    grid = "y",
    max.categ = 100,
    w = 7, h = w, ...) {

  # Define fill color
  df_XYcol <- if (CodeAndRoll2::is.list2(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list
  .assertMaxCategories(df_XYcol, col = x, max.categ)

  vars <- colnames(df_XYcol)
  if( !is.null(col)) {
    if( is.numeric(col) & col < length(vars) ) col <- col
    if( col %in% vars ) col <- vars[col]
    fill <- col # if col (color as a column name) is provided, fill is set to col
  }


  p <- ggpubr::ggboxplot(
    data = df_XYcol, x = vars[x], y = vars[y], fill = fill,
    title = plotname,
    subtitle = subtitle,
    caption = caption,
    # , fill = fill
    palette = palette_use,
    outlier.shape = outlier.shape,
    ...
  ) +
    ggplot2::labs(y = ylab)  +
    ggpubr::grids(axis = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logY), "boxplot", ext)
  }
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}


# _________________________________________________________________________________________________
#' @title qviolin
#'
#' @description Draw and save a violin plot
#' @param df_XYcol_or_list Data, as 2 column data frame, where col.1 is X axis, alternatively a uniquely named list ov values.
#' @param plotname Name of the plot
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
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
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @importFrom CodeAndRoll2 is.list2
#' @export
#' @examples data("ToothGrowth")
#' ToothLen.by.Dose <- ToothGrowth[, c("dose", "len")]
#' qviolin(ToothLen.by.Dose)
qviolin <- function(
    df_XYcol_or_list,
    x = 1, y = 2, col = NULL,
    fill = "gold",
    plotname = FixPlotName(substitute(df_XYcol_or_list)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,
    stat.test = FALSE,
    stat.method = NULL, stat.label.y.npc = "top", stat.label.x = 0.5,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    hide.legend = FALSE,
    also.pdf = FALSE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    logY = FALSE, # , logX = FALSE
    annotation_logticks_Y = logY,
    xlab.angle = 45,
    hline = FALSE, vline = FALSE,
    grid = FALSE,
    plot = TRUE, save = TRUE,
    mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    # , outlier.shape = NULL
    # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
    max.categ = 100,
    w = 7, h = w, ...) {

  df_XYcol <- if (CodeAndRoll2::is.list2(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list
  message("nrow(df_XYcol): ", nrow(df_XYcol))
  .assertMaxCategories(df_XYcol, col = x, max.categ)

  # Define fill color
  vars <- colnames(df_XYcol)
  if( !is.null(col)) {
    if( is.numeric(col) & col < length(vars) ) col <- col
    if( col %in% vars ) col <- vars[col]
    fill <- col # if col (color as a column name) is provided, fill is set to col
  }

  p <- ggpubr::ggviolin(
    data = df_XYcol, x = vars[x], y = vars[y], fill = fill,
    title = plotname,
    subtitle = subtitle,
    caption = caption,
    # , outlier.shape = outlier.shape
    palette = palette_use,
    ...
  ) +
    ggpubr::grids(axis = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, "violinplot", flag.nameiftrue(logY), ext)
  }
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}



# _________________________________________________________________________________________________
#' @title qstripchart
#'
#' @description Generates a stripchart and saves the plot for a given 2-column dataframe and offers several customizations.
#' @param df_XYcol_or_list Data, as 2 column data frame, where col.1 is X axis, alternatively a uniquely named list ov values.
#' @param plotname Name of the plot
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param plot Display the plot.
#' @param add Add boxplot or violin chart? Default  add = c("violin", "mean_sd"), it can be "boxplot" or only "mean_sd".
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
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
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @importFrom CodeAndRoll2 is.list2
#' @examples data("ToothGrowth")
#' ToothLen.by.Dose <- ToothGrowth[, c("dose", "len")]
#' qstripchart(ToothLen.by.Dose)
#'
#' @export
qstripchart <- function(
    df_XYcol_or_list,
    x = 1, y = 2, col = NULL,
    fill = "gold",
    plotname = FixPlotName(substitute(df_XYcol_or_list)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,
    ylab = NULL,
    plot = TRUE,
    add = c("violin", "mean_sd"),
    size.point = .2,
    stat.test = TRUE,
    stat.method = NULL, stat.label.y.npc = "png", stat.label.x = 0.75,
    palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    hide.legend = FALSE,
    also.pdf = TRUE,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    logY = FALSE, # , logX = FALSE
    annotation_logticks_Y = logY,
    xlab.angle = 90,
    hline = FALSE, vline = FALSE,
    save = TRUE, mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    grid = "y",
    max.categ = 100,
    w = 7, h = w,
    ...) {

  df_XYcol <- if (CodeAndRoll2::is.list2(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list
  .assertMaxCategories(df_XYcol, col = x, max.categ)

  # Define fill color
  vars <- colnames(df_XYcol)
  if( !is.null(col)) {
    if( is.numeric(col) & col < length(vars) ) col <- col
    if( col %in% vars ) col <- vars[col]
    fill <- col # if col (color as a column name) is provided, fill is set to col
  }

  p <- ggpubr::ggstripchart(
    data = df_XYcol, x = vars[x], y = vars[y], fill = fill,
    title = plotname,
    subtitle = subtitle,
    caption = caption,
    add = add,
    size = size.point,
    palette = palette_use,
    ...
  ) +
    ggplot2::ylab(ylab) +
    ggpubr::grids(axis = "y") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + grids(axis = grid)
  if (hline) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + annotation_logticks(sides = "l")

  if (stat.test) p <- p + stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  fix <- sppp("stripchart", sppp(add))
  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, fix, suffix, flag.nameiftrue(logY), "strip", ext)
  }
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}




# _________________________________________________________________________________________________
#' @title qvenn - Venn Diagram
#'
#' @description Draw and save a Venn Diagram using the `ggVennDiagram` package.
#' @param list The variable to plot.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle The subtitle of the plot. Default: paste (length(unique(unlist(list))), 'elements in total')
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = List element lengths parsed.
#' @param caption2 Optional text added to bottom right corner of the plot. Default = NULL.
#' @param filename Manually provided filename (optional). Default: parse from `plotname`,
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param mdlink Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".
#' @param col.min Color scale minimum, default: white
#' @param col.max Color scale maximum, default: red
#' @param hide.legend hide legend
#' @param x_exp Expand axis to show long set labels. Default: 0.2.
#' @param w Width of the plot.
#' @param h Height of the plot. Default: h = w * 0.75.
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should work).
#'
#' @importFrom ggVennDiagram ggVennDiagram
#' @examples LetterSets <- list("One" = LETTERS[1:7], "Two" = LETTERS[3:12])
#' qvenn(LetterSets)
#'
#' @export
qvenn <- function(
    list,
    also.pdf = FALSE,
    plotname = FixPlotName(substitute(list)),
    suffix = NULL,
    subtitle = paste(length(unique(unlist(list))), "elements in total"),
    caption = parseParamStringWNames(sapply(list, length)),
    caption2 = NULL,
    filename = NULL,
    ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    plot = TRUE, save = TRUE, mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
    # , palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4]
    # , col = as.character(1:3)[1]
    # , xlab.angle = 90
    col.min = "white", col.max = "red",
    hide.legend = FALSE,
    x_exp = .2,
    w = 8, h = 0.75 * w,
    ...) {
  #
  if(!is.null(caption2)) caption <- paste0(caption2, "\n", caption, "\n")

  p <- ggVennDiagram::ggVennDiagram(list, ..., ) +
    scale_fill_gradient(low = col.min, high = col.max) +
    ggplot2::labs(
      title = paste(" ", plotname),
      subtitle = paste(" ", subtitle, "\n"),
      caption = caption
    ) +
    scale_x_continuous(expand = expansion(mult = x_exp)) + # expand axis to show long set labels
    theme(plot.background = element_rect(fill = "white", colour = "white"))

  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  s1 <- paste0(length(list), "s")
  s2 <- kpp(s1, paste0(length(unique(unlist(list))), "el"))

  file_name <- if (!is.null(filename)) {
    kpp(filename, s2)
  } else {
    sppp(plotname, suffix, s2, "venn", ext)
  }

  if (save) qqSave(ggobj = p, title = plotname, fname = file_name,
                   ext = ext, w = w, h = h, also.pdf = also.pdf)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) p
}




# _________________________________________________________________________________________________
# #' @title Quick Heatmap Plot
# #'
# #' @description Generates a heatmap plot using the `ggheatmap` package with features such as automatic
# #' file saving and markdown link generation. This function simplifies the process of creating and
# #' customizing heatmaps, with support for clustering, annotations, and scaling.
# #'
# #' @param data_matrix The data matrix to be plotted; must be either a matrix or a data frame.
# #' Default: None (must be provided).
# #' @param also.pdf Logical; indicates whether to also save the plot as a PDF. Default: FALSE.
# #' @param ext The file extension for the saved plot image, adjusted based on `also.pdf`.
# #' Default: "png".
# #' @param title The main title for the heatmap. Default: "Heatmap".
# #' @param save Logical; indicates whether to save the plot to file. Default: TRUE.
# #' @param mdlink Logical; if TRUE and `save` is also TRUE, generates a markdown link for the saved
# #' plot image. Default: FALSE, using `MarkdownHelpers::unless.specified`.
# #' @param plotname The base name for the plot file. Default: "heatmap".
# #' @param subtitle The subtitle for the heatmap. Default: "NULL" (indicating no subtitle).
# #' @param caption The caption for the heatmap. Default: "mousse".
# #' @param suffix An optional suffix to append to the plot filename. Default: NULL.
# #' @param filename The specific filename to save the plot as; if NULL, a name is generated based on
# #' `plotname` and other parameters. Default: NULL.
# #' @param color The color palette for the heatmap values, using a blue-white-yellow gradient.
# #' Default: colorRampPalette(c( "#0073c2","white","#efc000"))(100).
# #' @param legendName The title of the heatmap legend. Default: "Intensity".
# #' @param scale Character; indicates how to scale the data ("none", "row", or "column").
# #' Default: "row".
# #' @param cluster_rows Logical; indicates whether to cluster rows. Default: FALSE.
# #' @param cluster_cols Logical; indicates whether to cluster columns. Default: FALSE.
# #' @param annotation_rows Data frame with row annotations. Default: NULL.
# #' @param annotation_cols Data frame with column annotations. Default: NULL.
# #' @param annotation_color List of colors for the annotations. Default: NULL.
# #' @param w Width of the saved plot image, in inches. Default: 10.
# #' @param h Height of the saved plot image, in inches. Default: 8.
# #' @param xlab The global label for the x-axis; this might not display due to limitations.
# #' Default: "Global X Label".
# #' @param ylab The global label for the y-axis; this might not display due to limitations.
# #' Default: "Global Y Label".
# #' @param ... Additional arguments passed to `ggheatmap`.
# #'
# #' @return A `ggplot` object representing the heatmap, conditionally modified based on the provided
# #' parameters.
# #'
# #' @examples
# #' \dontrun{
# #'   set.seed(123)
# #'   data_matrix <- matrix(rnorm(100), ncol = 10)
# #'   qheatmap(data_matrix, title = "My Heatmap", save = FALSE)
# #' }
# #'
# #' @importFrom ggheatmap ggheatmap
# #' @importFrom Stringendo FixPlotName
# #' @importFrom MarkdownHelpers ww.set.file.extension
# #' @importFrom MarkdownHelpers unless.specified
# #' @export
# qheatmap <- function(
#     data_matrix,
#     ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
#     plotname = paste("Heatmap of", substitute(data_matrix)),
#     subtitle = NULL,
#     caption = NULL,
#     suffix = NULL,
#     filename = NULL,
#     also.pdf = FALSE,
#     save = TRUE,
#     mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
#     color = colorRampPalette(c( "#0073c2","white","#efc000"))(100),
#     legendName = "Intensity",
#     scale = "row",
#     cluster_rows = F,
#     cluster_cols = F,
#     annotation_rows = NULL,
#     annotation_cols = NULL,
#     annotation_color = NULL,
#     w = 10,
#     h = 8,
#     xlab = "Global X Label",
#     ylab = "Global Y Label",
#     ...
# ) {
#   # Ensure the data input is correct
#   stopifnot(
#     "Data input should be a matrix or a data frame" = is.matrix(data_matrix) || is.data.frame(data_matrix)
#   )
#
#   if (is.data.frame(data_matrix)) {
#     data_matrix <- as.matrix(data_matrix)
#   }
#
#   # Create the heatmap
#   p <- ggheatmap::ggheatmap(
#     data = data_matrix,
#     color = color,
#     legendName = legendName,
#     scale = scale,
#     cluster_rows = cluster_rows,
#     cluster_cols = cluster_cols,
#     annotation_rows = annotation_rows,
#     annotation_cols = annotation_cols,
#     annotation_color = annotation_color,text_show_cols = "hhhhhhh"
#     , ...
#   )
#
#   # Add titles and captions
#   if (!cluster_rows & !cluster_cols) {
#     p <- p + labs(title = plotname, subtitle = subtitle, caption = caption)
#   } else {
#     message("Plot labeling (title, subtitle, caption) are only possible when both cluster_rows and cluster_cols are both false.")
#   }
#
#   # Handle file saving
#   file_name <- if (!is.null(filename)) {
#     filename
#   } else {
#     Stringendo::FixPlotName(plotname, suffix, "heatmap", flag.nameiftrue(cluster_rows), flag.nameiftrue(cluster_cols), ext)
#   }
#
#   if (save) {
#     qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf)
#   }
#
#   if (mdlink & save) {
#     qMarkdownImageLink(file_name)
#   }
#   return(p)
# }
#

# ______________________________________________________________________________________________----
# Auxiliary functions ----
# ____________________________________________________________________


#' @title qqSave
#'
#' @description Quick-Save ggplot objects
#' @param ggobj Plot as ggplot object.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param bgcol Plot background color
#' @param page page
#' @param title title field for pdf file (saved into file metadata)
#' @param fname fname
#' @param suffix A suffix added to the filename. NULL by default.
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#' @export
#'
#' @examples xplot <- ggplot2::qplot(12)
#' qqSave(ggobj = xplot)
#' qqSave(ggobj = xplot, ext = "pdf")
#' @importFrom cowplot save_plot

qqSave <- function(
    ggobj,
    ext = MarkdownHelpers::unless.specified("b.def.ext", def = "png"),
    also.pdf = FALSE,
    bgcol = "white",
    page = c(F, "A4p", "A4l", "A5p", "A5l")[1],
    title = FALSE,
    fname = FALSE,
    w = 4, h = w,
    suffix = NULL,
    ...) {
  #
  if (isFALSE(title)) title <- make.names(as.character(substitute(ggobj)))
  if (isFALSE(fname)) fname <- sppp(title, suffix, ext)
  fname <- sppp(make.names(fname))
  if (also.pdf) fname2 <- if (isFALSE(fname)) sppp(title, suffix, "pdf") else sppp(make.names(fname), "pdf")

  if (!isFALSE(page)) {
    wA4 <- 8.27
    hA4 <- 11.69
    if (page == "A4p") {
      w <- wA4
      h <- hA4
    }
    if (page == "A4l") {
      w <- hA4
      h <- wA4
    }
    if (page == "A5p") {
      w <- wA4 / 2
      h <- hA4 / 2
    }
    if (page == "A5l") {
      w <- hA4 / 2
      h <- wA4 / 2
    }
  }
  print(paste0(getwd(), "/", fname))

  # Set the plot background to white
  ggobj <- ggobj + theme(plot.background = element_rect(fill = bgcol, color = bgcol))

  if (also.pdf) {
    cowplot::save_plot(
      plot = ggobj, filename = fname2, base_width = w, base_height = h,
      title = ww.ttl_field(title, creator = "ggExpress"),
      ...
    )
  }
  cowplot::save_plot(
    plot = ggobj, filename = fname,
    base_width = w, base_height = h, ...
  )
}


# _________________________________________________________________________________________________
#' @title  q32vA4_grid_plot
#'
#' @description Plot up to 6 panels (3-by-2) on vertically standing A4 page.
#' @param plot_list A list of ggplot objects, each of which is one panel.
#' @param plot Show the plot? Default: F
#' @param plotname Plot name. Default: Autonaming.
#' @param suffix A suffix added to the filename. Default: NULL
#' @param nrow number of rows for panels on the page. Default: 2
#' @param ncol number of columns for panels on the page. Default: 2
#' @param scale Scaling factor of the canvas. Default: 1
#' @param h Height of the plot. Default: wA4 * scale
#' @param w Width of the plot. Default: hA4 * scale
#' @param ... Pass any other parameter to the internally called functions (most of them should work).
#' @param extension file extension
#' @importFrom cowplot plot_grid save_plot
#'
#' @examples # q32vA4_grid_plot()
#' @export
q32vA4_grid_plot <- function(
    plot_list,
    suffix = NULL,
    plotname = FixPlotName(substitute(plot_list), suffix),
    plot = F,
    nrow = 3, ncol = 2, extension = c("pdf", "png")[2],
    scale = 1,
    h = hA4 * scale, w = wA4 * scale,
    ...) { # Save 4 umaps on an A4 page.
  print("Plot panels on 3-by-2 vertical A4 page.")
  stopifnot(length(plot_list) < 7)

  # if (plotname==F) plotname =  sppp(substitute(plot_list), suffix)
  fname <- sppp(plotname, extension)
  p1 <- cowplot::plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol, labels = LETTERS[1:length(plot_list)], ...)
  cowplot::save_plot(plot = p1, filename = fname, base_height = h, base_width = w)
  ww.FnP_parser(fname)
}



# _________________________________________________________________________________________________
#' @title  qA4_grid_plot
#'
#' @description Plot up to 6 panels (3-by-1) on vertically standing A4 page.
#' @param plot_list A list of ggplot objects, each of which is one panel.
#' @param plotname Plot name. Default: Autonaming
#' @param suffix A suffix added to the filename. Default: NULL
#' @param nrow number of rows for panels on the page. Default: 2
#' @param ncol number of columns for panels on the page. Default: 2
#' @param plot Show the plot? Default: F
#' @param labels Panel labels. Default: LETTERS
#' @param max.list.length Max number of panels (per page). Default: 16
#' @param extension file extension
#' @param scale Scaling factor of the canvas. Default: 1
#' @param h Height of the plot. Default: wA4 * scale
#' @param w Width of the plot. Default: hA4 * scale
#' @param ... Pass any other parameter to the internally called functions (most of them should work).
#'
#' @importFrom cowplot plot_grid save_plot
#' @examples # qA4_grid_plot()
#'
#' @export
qA4_grid_plot <- function(
    plot_list,
    plotname = FixPlotName(substitute(plot_list), nrow, "by", ncol, suffix),
    suffix = NULL,
    nrow = 3, ncol = 2,
    plot = FALSE,
    labels = LETTERS[1:length(plot_list)],
    max.list.length = 16,
    extension = c("pdf", "png")[2],
    scale = 1,
    h = hA4 * scale, w = wA4 * scale,
    ...) { # Save 4 umaps on an A4 page.
  stopifnot(length(plot_list) < max.list.length)
  message("Plot panels ", nrow, " rows by ", ncol, " cols, on an A4 page.")

  # if (plotname==F) plotname =  sppp(substitute(plot_list), suffix)
  fname <- sppp(plotname, suffix, extension)
  p1 <- cowplot::plot_grid(plotlist = plot_list, nrow = nrow, ncol = ncol, labels = labels, ...) +
    theme(plot.background = element_rect(fill = "white"))
  cowplot::save_plot(plot = p1, filename = fname, base_height = h, base_width = w)
  ww.FnP_parser(fname)
}



# _________________________________________________________________________________________________
#' @title qMarkdownImageLink
#'
#' @description Insert Markdown image link to .md report
#' @param file_name file_name
#' @export
#'
#' @examples qMarkdownImageLink(file_name = "myplot.pdf")
#' @importFrom MarkdownHelpers llogit

qMarkdownImageLink <- function(file_name = "myplot.pdf") {
  MarkdownHelpers::llogit(paste0("![", file_name, "]", "(", file_name, ")", collapse = ""))
}



# ______________________________________________________________________________________________----
# Internal helper functions ----
# ____________________________________________________________________


#' @title qqqAxisLength
#'
#' @description Define Axis Length
#' @param vec The variable to plot.
#' @param minLength minLength
#'
#' @examples qqqAxisLength()
qqqAxisLength <- function(vec = 1:20, minLength = 6, factor = 0.4) {
  max(round(length(vec) * factor), minLength)
}


# _________________________________________________________________________________________________
#' @title qqqNamed.Vec.2.Tbl
#'
#' @description Covert a named vector to a table.
#' @param namedVec namedVec
#' @param verbose verbose
#' @param strip.too.many.names strip.too.many.names
#' @param thr thr
#'
#' @importFrom tibble tibble as_tibble
#' @examples qqqNamed.Vec.2.Tbl(namedVec = c("A" = 2, "B" = 29))
#'
qqqNamed.Vec.2.Tbl <- function(namedVec = 1:14, verbose = FALSE, strip.too.many.names = TRUE, thr = 50) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.

  # Check naming issues
  nr.uniq.names <- length(unique(names(namedVec)))
  if (nr.uniq.names > thr & verbose) iprint("Vector has", thr, "+ names. Can mess up auto-color legends.")
  if (nr.uniq.names < 1 & verbose) print("Vector has no names")
  an.issue.w.names <- (nr.uniq.names > thr | nr.uniq.names < 1)

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
#'
#' @description Covert a table to a named vector.
#' @param tibble.input tibble.input
#' @param name.column name.column
#' @param value.column value.column
#' @export
#'
#' @examples a <- 1:5
#' x <- tibble::tibble(a, a * 2)
#' qqqTbl.2.Vec(x)
qqqTbl.2.Vec <- function(tibble.input, name.column = 1, value.column = 2) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
  vec <- tibble.input[[value.column]]
  names(vec) <- tibble.input[[name.column]]
  vec
}


# _________________________________________________________________________________________________
#' @title qqqList.2.DF.ggplot
#'
#' @description Convert a list to a tow-column data frame to plot boxplots and violin plots
#' @param ls A list with all elements named
#' @importFrom CodeAndRoll2 is.list2
#' @examples LetterSets <- list("One" = LETTERS[1:7], "Two" = LETTERS[3:12])
#' qqqList.2.DF.ggplot(LetterSets)
#' @export
qqqList.2.DF.ggplot <- function(ls = LetterSets) {
  stopifnot(CodeAndRoll2::is.list2(ls))
  stopif(length(ls) != length(unique(names(ls))), message = "Not all list elements have a unique name! ")
  # utils::stack(unlist(ls))[, 2:1]
  utils::stack(ls)[, 2:1]
}

# _________________________________________________________________________________________________
#' @title Assert Maximum Categories in a DataFrame Column
#'
#' @description Checks if the number of unique categories in a column of a dataframe is within the allowed limit.
#' @param df A data frame containing the column to be checked.
#' @param col The column name or index in the data frame.
#' @param max.categ The maximum allowed number of unique categories.
#' @return Stops the function execution if the number of unique categories exceeds max.categ.
#' @examples
#' assertMaxCategories(df_XYcol, "x", 10)
.assertMaxCategories <- function(df, col, max.categ) {
  nrCategories <- length(unique(df[[col]]))

  # Assert that the number of categories is less than or equal to max.categ
  stopifnot(nrCategories <= max.categ)
}



# _________________________________________________________________________________________________



# _________________________________________________________________________________________________
# _________________________________________________________________________________________________



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
#   if (!is.null(suffix_tag) & !isFALSE(suffix_tag)) nm <- sppp(nm, suffix_tag)
#   return(nm)
# }

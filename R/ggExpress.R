# ____________________________________________________________________
# ggExpress is the fastest way to create, annotate and export plots in R.  ----
# ____________________________________________________________________
# try(source("~/GitHub/Packages/ggExpress/R/ggExpress.R"), silent = TRUE)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpress/main/ggExpress.R"), silent = TRUE)
# source('~/.pack.R')

# devtools::load_all("~/GitHub/Packages/ggExpress"); # devtools::document("~/GitHub/Packages/ggExpress")
# devtools::check_man("~/GitHub/Packages/ggExpress"); # devtools::document("~/GitHub/Packages/ggExpress")



# ______________________________________________________________________________________________----
# Define package options  ----

.onLoad <- function(libname, pkgname) {
  op <- options()

  pkg_op <- list(
    # insert at top

    gg.fill.col = "gold",
    gg.save.obj = FALSE,
    gg.palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb", "uchicago")[4],
    # gg.def.ext = "png",
    gg.mdlink = FALSE
  )

  # Only set options that are not already defined
  to_set <- pkg_op[!names(pkg_op) %in% names(op)]
  if (length(to_set)) options(to_set)

  invisible()
}


# ______________________________________________________________________________________________----
# Simple plots  ----
# ____________________________________________________________________



#' @title Quickly draw and save a histogram (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a histogram of a
#' distribution provided as a numeric vector. It is a wrapper around `ggpubr::gghistogram()`,
#' with the automation of many features. All `ggpubr` parameters can be accessed through
#' the `...` argument. It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param vec A numeric vector for which the histogram is to be plotted.
#'
#' @param ext File extension for the saved plot. Either '.pdf' or '.png'. Default is '.png'.
#' @param plot Logical indicating whether to display the plot. Default is TRUE.
#' @param save Logical indicating whether to save the plot into a file. Default is TRUE.
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param plotname Title of the plot and the name of the file (unless specified in `filename`). Default is parsed from `vec`.
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Optional filename for the saved plot. Default is parsed from `plotname`.
#'
#' @param xlab Label for the X-axis. By default, it uses the plot name.
#' @param add Character defining the type of plot annotations to add. Default is 'median'.
#' @param vline Numeric value at which to draw a vertical line on the plot. Default is FALSE (no line).
#' @param filtercol Numeric value indicating the direction to color bars above/below the threshold. Default is 0 (no color change).
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param col Color of the plot. Default is '1'.
#' @param xlab.angle Angle to rotate X-axis labels. Default is 90 degrees.
#' @param hide.legend Logical indicating whether to hide the legend. Default is TRUE.
#' @param max.names Maximum number of names to show on the axis. Default is 50.
#' @param annotation_logticks_X Logical indicating whether to add annotation logticks on X-axis. Default follows the value of `logX`.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param logX Logical indicating whether to make X axis on log10 scale. Default is FALSE.
#' @param logY Logical indicating whether to make Y axis on log10 scale. Default is FALSE.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
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
    plotname = FixPlotName(substitute(vec)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    also.pdf = FALSE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    save.obj = getOption("gg.save.obj", F),

    xlab = FALSE,
    logX = FALSE, logY = FALSE,
    annotation_logticks_X = logX, annotation_logticks_Y = logY,
    vline = FALSE, filtercol = 0,
    add = "median",
    palette_use = getOption("gg.palette_use", 'jco'),
    col = as.character(1:3)[1],
    xlab.angle = 90,
    hide.legend = TRUE,
    max.names = 50,
    grid = "y", mdlink = getOption("gg.mdlink", F),
    w = 5, h = w,
    ...) {
  stopifnot(is.numeric(vec), length(vec) > 0L, all(is.finite(vec)))
  if (isFALSE(xlab)) xlab <- plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)

  # Color handling (histogram-safe) _______________________________________
  pal <- if (length(palette_use) == 1)
    ggpubr::get_palette(palette_use, 2)
  else palette_use

  # Resolve `col` to an actual color
  col_resolved <- if (is.numeric(col) || grepl("^[0-9]+$", col))
    pal[as.integer(col)]
  else
    col

  # Default: single-color histogram
  df$colour <- factor("all")
  pal2 <- c(all = col_resolved)

  # Optional split by vline
  if (is.numeric(vline) && filtercol %in% c(1, -1)) {
    df$colour <- factor(
      if (filtercol == 1) df$value > vline else df$value < vline
    )
    pal2 <- setNames(pal[1:2], levels(df$colour))
  }

  p <- ggpubr::gghistogram(
    data = df, x = "value",
    title = plotname, xlab = xlab,
    add = add,
    # , color = "names", fill = "names"
    subtitle = subtitle,
    caption = caption,
    color = "colour", fill = "colour",
    palette = pal2, ...
  ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)) +
    if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (annotation_logticks_X) p <- p + ggplot2::annotation_logticks(sides = "b")

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")

  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logX), flag.nameiftrue(logY), "hist", ext)
  }
  file_name <- FixPlotName(file_name)

  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf, save.obj = save.obj)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}


# _________________________________________________________________________________________________
#' @title Quickly draw and save a density plot (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a density plot of a
#' distribution provided as a numeric vector. It is a wrapper around `ggpubr::ggdensity()`,
#' with the automation of many features. All `ggpubr` parameters can be accessed through
#' the `...` argument.  It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param vec The variable to plot.
#'
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param logX Make X axis log10-scale.
#' @param xlab X-axis label. Default: FALSE.
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param hide.legend Hide legend.
#' @param logY Make Y axis log10-scale.
#' @param max.names The maximum number of names still to be shown on the axis.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @export
#'
#' @examples weight <- rnorm(1000)
#' qdensity(weight)
qdensity <- function(
    vec,
    plotname = FixPlotName(substitute(vec)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    xlab = FALSE,
    xlab.angle = 90,
    logX = FALSE, logY = FALSE,
    palette_use = getOption("gg.palette_use", 'jco'),
    hide.legend = TRUE,
    max.names = 50,
    grid = FALSE, mdlink = getOption("gg.mdlink", F),
    w = 5, h = w, ...) {
  stopifnot(is.numeric(vec), length(vec) > 0L, all(is.finite(vec)))
  if (isFALSE(xlab)) xlab <- plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, thr = max.names)

  p <- ggpubr::ggdensity(
    data = df, x = "value", # , y = "..count.."
    title = plotname, xlab = xlab,
    add = "median",
    color = "names", fill = "names",
    subtitle = subtitle,
    caption = caption,
    palette = palette_use,
    ...
  ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)) +
    if (length(unique(df$"names")) == 1) ggplot2::theme(legend.position = "none")

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logX), flag.nameiftrue(logY), "dens", ext)
  }

  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf, save.obj = save.obj)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}


# _________________________________________________________________________________________________
#' @title Quickly draw and save a pie chart (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a pie chart of a
#' distribution provided as a numeric table. It is a wrapper around `ggpubr::ggpie()`,
#' with the automation of many features. All `ggpubr` parameters can be accessed through
#' the `...` argument. It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param vec The variable to plot.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param LegendSide Legend side.
#' @param LegendTitle Legend title.
#' @param NoLegend No legend.
#' @param pcdigits Number of digits for percentages.
#' @param NamedSlices Use named slices.
#' @param extended.canvas Make an extended canvas. Default: TRUE.
#' @param custom.margin Custom plot margin. Default: TRUE.
#' @param max.categories Maximum number of categories to be shown as a separate slice
#' @param decr.order Slices in the order of df. By default, they are ordered alphabetically in the plot.
#' @param both_pc_and_value Report both percentage and number.
#' @param custom.order Custom order.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param labels Slice labels. Set to NULL to remove slice names.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @export
#'
#' @examples xvec <- c("A" = 12, "B" = 29)
#' qpie(vec = xvec)
qpie <- function(
    vec,
    plotname = FixPlotName(substitute(vec)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    caption.ext = TRUE,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    NoLegend = FALSE,
    LegendSide = TRUE,
    LegendTitle = "",
    pcdigits = 2, NamedSlices = FALSE,
    custom.order = FALSE,
    extended.canvas = TRUE,
    palette_use = getOption("gg.palette_use", 'jco'),
    custom.margin = TRUE,
    max.categories = 100,
    max.names = 10,
    decr.order = TRUE,
    both_pc_and_value = FALSE,
    labels = "names", mdlink = getOption("gg.mdlink", F),
    w = 7, h = 5,
    ...) {
  stopifnot(is.numeric(vec), length(vec) > 0L, all(is.finite(vec)), all(vec >= 0))
  print(plotname)
  l.orig <- length(vec)
  sum.orig <- sum(vec)

  # Plot annotation ________________________________________________
  st <- paste("Sum:", sum.orig)
  subtitle <- if (is.null(subtitle)) st else paste0(subtitle, "\n", st)

  ct <- if (caption.ext) paste0("Total elements:", l.orig, "; shown:", (max.categories - 1), " | max.names:", max.names) else NULL
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
  stopifnot("Number of categories exceeds 'max.categories'." = nrCategories.DFcol1 <= max.categories)

  if (NamedSlices) labs <- paste(df$names, "\n", labs)
  if (custom.order != FALSE) df$"names" <- factor(df$"names", levels = custom.order)

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
    ggplot2::guides(fill = ggplot2::guide_legend(LegendTitle))
  # theme(legend.title = LegendTitle)

  if (LegendSide) p <- ggpubr::ggpar(p, legend = "right")
  if (extended.canvas) p <- p + ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 10, 10))
  if (custom.margin) p <- p + ggplot2::coord_polar(theta = "y", clip = "off")

  p <- if (NoLegend) p + ggplot2::theme(legend.position = "none", validate = TRUE) else p

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, "pie", ext)
  }

  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf, save.obj = save.obj)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}



# _________________________________________________________________________________________________
#' @title Quickly draw and save a bar plot (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a bar plot of a
#' distribution provided as a numeric vector. It is a wrapper around `ggpubr::ggbarplot()`,
#' with the automation of many features. All `ggpubr` parameters can be accessed through
#' the `...` argument. It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param vec The variable to plot.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param col The fill color of the bars. Default: 1st color of the palette.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param hline Draw a horizontal line on the plot.
#' @param filtercol Color bars below/above the threshold with red/green. Define the direction by
#' -1 or 1. Takes effect if "*line" is defined.
#' @param filtercol_default Use default red/green colors for filtering. If FALSE, use the
#' first two colors of the palette. Default: TRUE, `c("#008B45FF", "#EE0000FF")`
#'
#' @param xlab X-axis label. Default: "".
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param ylab Y-axis label. Default: NULL.
#' @param logY Make Y axis log10-scale.
#' @param ylim Y-axis limit values. Default: c(0, 1.1*max(vec))
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#'
#' @param label Label bars with their values, or by a custom string vector. Default: NULL (no labels).
#' @param hide.legend Hide legend. Default: TRUE.
#' @param legend.title Custom legend title. Provide a string.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize Limit size.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @return It returns a ggplot object if `plot` is TRUE.
#' @export
#'
#' @examples weight3 <- runif(12)
#' qbarplot(weight3, filtercol = -1, hline = .5)
#' qbarplot(weight3, filtercol = 1, hline = .5)
qbarplot <- function(
    vec,
    plotname = FixPlotName(substitute(vec)),
    subtitle = paste("Median:", iround(median(vec))),
    suffix = NULL,
    caption = suffix,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    hline = FALSE,
    filtercol = 1,
    filtercol_default = TRUE, # c("#008B45FF", "#EE0000FF") # Default green/red colors for filtering

    palette_use = getOption("gg.palette_use", 'jco'),
    col = 1,

    xlab = "", xlab.angle = 45,
    logY = FALSE,
    ylim = c(0, iround(1.1 * as.numeric(max(vec, na.rm = TRUE)))),
    annotation_logticks_Y = logY,
    label = NULL,
    hide.legend = TRUE,
    legend.title = NULL,
    max.names = 100,
    limitsize = FALSE,
    ylab = NULL,
    grid = "y", mdlink = getOption("gg.mdlink", F),
    w = qqqAxisLength(vec, factor = 0.25), h = 5,
    ...) {

  stopifnot(is.numeric(vec), length(vec) > 0L, all(is.finite(vec)))
  if (isFALSE(xlab)) xlab <- plotname
  df <- qqqNamed.Vec.2.Tbl(namedVec = vec, strip.too.many.names = FALSE)
  if (length(unique(df$"names")) == 1) df$"names" <- as.character(1:length(vec))



  # Name-aware color handling ____________________________________________________________
  col_src <- if (!is.null(names(col))) col else palette_use # Decide which vector carries names for color semantics
  if (!is.null(names(col_src)) && !is.null(names(vec))) {
    name_overlap <- intersect(names(col_src), names(vec))

    Stringendo::warnifnot(
      "No overlap between names(vec) and names(col) / names(palette)! Names are ignored, match by order!" = length(name_overlap) > 0,
      "Partial overlap between names(vec) and names(col). Non-matching names are grey (NA)." = (length(name_overlap) == 0 || length(name_overlap) == length(vec))
    )

    if (length(name_overlap) == length(vec)) { # IMPORTANT: only mutate col on FULL or ZERO overlap
      col <- col_src[names(vec)]
    } else if (length(name_overlap) == 0) {
      col <- unname(col)
    }
    # partial overlap: warn only, do NOT touch col
  }

  # Handling colors ________________________________________________________________
  # Palette argument
  pal <- if (length(palette_use) == 1)
    ggpubr::get_palette(palette_use, length(vec)) # For a name of a palette or a single color.
  else
    palette_use

  # Color argument_______________________________________________
  cols <- if (is.numeric(col))
    pal[rep(col, length.out = length(vec))]         # If numeric, use as indices into palette
  else rep(col, length.out = length(vec))           # Else use colors as is, recycling if needed.


  # Optional filter coloring via hline _______________________________________________
  if (is.numeric(hline) && filtercol %in% c(1, -1)) {
    pal_fc <- if  (isTRUE(filtercol_default))  c("#008B45FF", "#EE0000FF")  # If filtercol_default is TRUE, use default green/red colors
    else pal[1:2]                                                    # Else use first two colors of the palette
    fc <- pal_fc[if (filtercol == 1) c(1, 2) else c(2, 1)]            # Flip colors if filtercol is -1
    cols <- ifelse(vec > hline, fc[1], fc[2])                         # Overwrite colors based on hline comparison
  }

  # Assign colors to dataframe _______________________________________________
  keys <- as.character(seq_along(cols))
  df$colour <- keys

  pal2 <- setNames(cols, keys)

  # log10Y axis handling ____________________________________________________________
  if (logY) {

    if (any(vec <= 0)) {
      warning( sum(vec <= 0), " input value(s) are <=0. \nA pseudocount of min(vec)/10 will be added.")

      # minimal & explicit pseudocount
      pseudo <- min(vec[vec > 0]) / 10
      vec <- vec + pseudo
    }

    ylim <- NULL  # CRITICAL: never pass ylim to scale_y_log10()
  }

  # Plot _________________________________________________________________
  p <- ggpubr::ggbarplot(
    data = df, x = "names", y = "value",
    title = plotname, xlab = xlab,
    subtitle = subtitle,
    caption = caption,
    color = "colour", fill = "colour",
    palette = pal2,
    label = label,
    ylim = ylim,
    ...
  ) +
    ggplot2::labs(y = ylab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")
  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (!isFALSE(hline)) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (length(vec) > max.names) p <- p + ggplot2::guides(x = "none")
  if (!is.null(legend.title)) p <- p + ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title), color = "none") # Hide the color legend
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logY), "bar", ext)
  }

  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name, ext = ext,
      w = w, h = h, limitsize = limitsize, also.pdf = also.pdf, save.obj = save.obj
    )
  }
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}




# _________________________________________________________________________________________________
#' @title qbarplot.stacked.from.wide.df - Barplot for tibbles or dataframes
#'
#' @description Draw and save a stacked barplot for each row of a dataframe.
#' @param df The variable to plot.
#' @param x Colname to split along X axis. Default: "Samples".
#' @param y Colname to count along y axis. Default: "Fraction".
#' @param z Colname to split along Y axis. Default: "Category.
#' @param color Color (split) by along Y.
# #' @param fill Color (split) by along Y.
#'
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param scale Scale the y axis. Default: TRUE.
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param hline Draw a horizontal line on the plot.
# #' @param filtercol Color bars below/above the threshold with red/green. Define the direction by -1 or 1. Takes effect if "*line" is defined.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param xlab X-axis label. Default: `x`.
#' @param logY Make Y axis log10-scale.
#' @param label Label text.
#' @param hide.legend Hide legend.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize Limit size.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param max.categ The maximum allowed number of unique categories.
# #' @param top The number of top categories to keep. Default: NULL.
#'
#'  @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
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
#' @importFrom rlang sym
#' @export qbarplot.stacked.from.wide.df

qbarplot.stacked.from.wide.df <- function(
    df,
    x = "Samples",
    y = "Fraction",
    z = "Category",

    plotname = FixPlotName(substitute(df)),
    subtitle = NULL, suffix = NULL, caption = suffix,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    # fill = colnames(df)[3],
    color = 1,
    label = NULL,

    scale = TRUE,

    hline = FALSE,
    # filtercol = 1,
    palette_use = getOption("gg.palette_use", 'jco'),
    xlab.angle = 45, xlab = x,
    logY = FALSE,
    annotation_logticks_Y = logY,
    hide.legend = FALSE,
    max.names = 50,
    limitsize = FALSE,
    max.categ = 10,
    # top = NULL,
    grid = "y", mdlink = getOption("gg.mdlink", F),
    w = qqqAxisLength(df, factor = .7), h = 5,
    ...) {
  message(plotname)
  stopifnot(
    is.data.frame(df),
    ncol(df) > 2
  )

  # if (is.null(xlab)) xlab <- if (scale) paste("%", x ) else x
  if (is.null(subtitle)) subtitle <- paste("Median:", iround(median(df[[y]])))


  df_long <- df |>
    tibble::rownames_to_column(var = x) |> # Convert row names to a column
    tidyr::pivot_longer(
      cols = -!!rlang::sym(x), # Convert wide to long format
      names_to = z, # "category"
      values_to = y # "Fraction"
    )

  p <- ggpubr::ggbarplot(
    data = df_long, x = x, y = y,
    color = color,
    fill = z, # Use the 'category' column created in long format
    subtitle = subtitle,
    title = plotname, xlab = xlab,
    caption = caption,
    label = label,
    palette = palette_use,
    position = if (scale) position_fill() else position_stack(),
    ...
  ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")
  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (nrow(df) > max.names) p <- p + ggplot2::guides(x = "none") # Hide x-axis labels when too many categories are present
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")
  if (!isFALSE(hline)) p <- p + ggplot2::geom_hline(yintercept = hline)

  file_name <- if (!is.null(filename)) { filename } else { FixPlotName(plotname, suffix, flag.nameiftrue(logY), "bar", ext) }

  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name, ext = ext,
      w = w, h = h, limitsize = limitsize, also.pdf = also.pdf, save.obj = save.obj
    )
  }
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}


# _________________________________________________________________________________________________
#' @title qbarplot.df - Barplot for tibbles or dataframes
#'
#' @description Draw and save a barplot for tibbles or dataframes
#' @param df The variable to plot.
#' @param x Colname to split along X axis. Default: `colnames(df)[1]`.
#' @param y Colname to count along y axis. Default: `colnames(df)[2]`.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param fill Color (split) by along Y. Default: `colnames(df)[3]`.
#' @param color Color (split) by along Y.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param label Label text.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param scale Scale the Y axis to 100%.
#' @param position Position adjustment of the bars. If `scale` is TRUE, position is set to "fill",
#' otherwise to "dodge". Can also be "stack".
#'
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param xlab X-axis label. Default: NULL.
#' @param logY Make Y axis log10-scale.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#'
#' @param hline Draw a horizontal line on the plot.
#' @param filtercol Color bars below/above the threshold with red/green. Define the direction by -1 or 1. Takes effect if "*line" is defined.
#' @param hide.legend Hide legend.
#' @param max.names The maximum number of names still to be shown on the axis.
#' @param limitsize Limit size.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param max.categ Maximum number of categories to show on the plot. Default is 10.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
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
    plotname = FixPlotName(substitute(df)),
    subtitle = NULL, suffix = NULL, caption = suffix,
    filename = NULL,

    fill = colnames(df)[3],
    color = 1,
    palette_use = getOption("gg.palette_use", 'jco'),
    label = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    scale = TRUE,
    position = if(scale) "fill" else "dodge", # also can be "stack"

    xlab.angle = 45, xlab = NULL,
    logY = FALSE,
    annotation_logticks_Y = logY,

    hline = FALSE, filtercol = 1,
    hide.legend = TRUE,
    max.names = 50,
    limitsize = FALSE,
    max.categ = 10,


    grid = "y", mdlink = getOption("gg.mdlink", F),
    w = qqqAxisLength(df), h = 5,
    ...) {

  message(plotname)
  cols <- colnames(df)
  x <- if (is.numeric(x)) cols[x] else x
  y <- if (is.numeric(y)) cols[y] else y
  fill <- if (is.numeric(fill)) cols[fill] else fill

  # Check inputs ____________________________________________________________
  stopifnot(
    is.data.frame(df), ncol(df) > 2,
    "Some variable names are not found in data frame: (x, y, fill)!" = all(c(x, y, fill) %in% cols),
    is.numeric(df[[y]])
  )

  if (is.null(xlab)) xlab <- if (scale) paste("%", x) else x
  if (is.null(subtitle)) subtitle <- paste("Median:", iround(median(df[[y]])))

  if (is.numeric(df[[fill]])) {
    df[[fill]] <- as.factor(df[[fill]])
    nr_categ <- length(unique(df[[fill]]))
    stopifnot("3rd column has too many categories" = nr_categ < max.categ)
  }

  # Position handling ____________________________________________________________
  pos_gg <- switch(
    position,
    dodge = ggplot2::position_dodge(),
    stack = ggplot2::position_stack(),
    fill  = ggplot2::position_fill()
  )

  # Plot _________________________________________________________________
  p <- ggpubr::ggbarplot(
    data = df, x = x, y = y,
    color = color,
    fill = fill,
    subtitle = subtitle,
    title = plotname, xlab = xlab,
    caption = caption,
    label = label,
    palette = palette_use,
    position = pos_gg,
    ...
  ) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  # Additional elements _________________________________________________________________
  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")
  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (length(unique(df[[x]])) > max.names) p <- p + ggplot2::guides(x = "none") # Hide x-axis labels when number of unique categories exceeds limit
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  if (!isFALSE(hline)) p <- p + ggplot2::geom_hline(yintercept = hline)

  # Save and print _________________________________________________________________
  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logY), "bar", ext)
  }

  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name, ext = ext,
      w = w, h = h, limitsize = limitsize, also.pdf = also.pdf, save.obj = save.obj
    )
  }
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}


# _________________________________________________________________________________________________
#' @title Quickly draw and save a scatter plot (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a scatter plot of
#' two variables provided as a 2-column data frame or matrix. It is a wrapper around
#' `ggpubr::ggscatter()`, with the automation of many features. All `ggpubr` parameters can be
#' accessed through the `...` argument. It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param df_XYcol Data, as 2 column data frame, where col.1 is X axis.
#' @param x The index or name of the column to be plotted on the X axis. Default: `1`.
#' @param y The index or name of the column to be plotted on the Y axis. Default: `2`.
#' @param plotname The name of the file and title of the plot.
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#' @param also.pdf Save plot in both png and pdf formats.
#'
#' @param label Point labels. Default: NULL.
#' @param repel Repel labels from each other. Default: TRUE.
#' @param hide.legend Hide legend.
#' @param col Color of the plot.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#'
#' @param xlab X-axis label. Default: NULL.
#' @param ylab Y-axis label. Default: NULL.
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param logX Make X axis log10-scale.
#' @param logY Make Y axis log10-scale.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param annotation_logticks_X Logical indicating whether to add annotation logticks on X-axis. Default follows the value of `logX`.
#' @param xlim X-axis limit values. Default: NULL.
#' @param ylim Y-axis limit values. Default: NULL.
#' @param isometric Make both axes isometric (same scale).
#'
#' @param hline Draw a horizontal line on the plot, yintercept or FALSE
#' @param vline Draw a vertical line on the plot, xintercept or FALSE.
#' @param abline Draw a sloped line on the plot. Set to FALSE, or `intercept = abline[1], slope = abline[2]`.
#' @param line.col Color of the lines (vline, hline, abline). Default: "darkgrey".
#' @param line.width Width of the line (vline, hline, abline). Default: 0.5.
#' @param line.type Type of the line (vline, hline, abline). Default: "dashed".
#' @param add_contour_plot Add 2D contour plot. See: http://www.sthda.com/english/articles/32-r-graphics-essentials/131-plot-two-continuous-variables-scatter-graph-and-alternatives/#continuous-bivariate-distribution
#' @param correlation_r2 Add a correlation value to the plot. Set as "pearson" or "spearman". Default: FALSE.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
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

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    label = NULL, repel = TRUE,
    hide.legend = FALSE,
    col = c(NULL, 3)[1],
    # fill = NULL,
    palette_use = getOption("gg.palette_use", 'jco'),

    xlab = NULL, ylab = NULL,
    xlab.angle = 90,
    logX = FALSE, logY = FALSE,
    annotation_logticks_Y = logY,
    annotation_logticks_X = logX,
    xlim = NULL,
    ylim = NULL,
    isometric = FALSE,

    hline = FALSE, vline = FALSE, abline = FALSE,
    line.col = "darkgrey", line.width = 0.5, line.type = "dashed",
    add_contour_plot = FALSE,
    correlation_r2 = FALSE, # add as  c("pearson", "spearman")

    grid = "xy", mdlink = getOption("gg.mdlink", F),
    w = 7, h = w,
    ...) {

  print(plotname)

  # Input validation ____________________________________________________________
  stopifnot(
    is.data.frame(df_XYcol) || is.matrix(df_XYcol),
    ncol(df_XYcol) >= 2L,
    (is.numeric(x) && x <= ncol(df_XYcol)) || (is.character(x) && x %in% colnames(df_XYcol)),
    (is.numeric(y) && y <= ncol(df_XYcol)) || (is.character(y) && y %in% colnames(df_XYcol))
  )

  if (is.matrix(df_XYcol)) df_XYcol <- as.data.frame(df_XYcol)
  if (is.character(x)) x <- which(colnames(df_XYcol) == x)
  if (is.character(y)) y <- which(colnames(df_XYcol) == y)
  xv <- df_XYcol[[x]]
  yv <- df_XYcol[[y]]
  stopifnot(
    is.numeric(xv), is.numeric(yv),
    length(xv) > 0L, length(yv) > 0L,
    all(is.finite(xv)), all(is.finite(yv))
  )

  vars <- colnames(df_XYcol)
  names(vars) <- vars
  cat("Variable (column) names 1-5:", head(vars), "...\n")

  # Isometric axes handling ____________________________________________________________
  if (isometric) {
    rng <- df_XYcol[, c(vars[x], vars[y])] |> range()
    rng <- range(rng, xlim, ylim, na.rm = TRUE) |>
      CodeAndRoll2::iround()

    message("Isometric axes are calculated automatically from the data:\n",
            paste(rng, collapse = ", "), " - It can be extended by providing xlim arg.")
    xlim <- rng
    ylim <- rng
  }

  # Scatter plot ____________________________________________________________
  p <- ggpubr::ggscatter(
    data = df_XYcol, x = vars[x], y = vars[y],
    color = col,
    title = FixPlotName(plotname, suffix),
    subtitle = subtitle,
    caption = caption,
    # palette = palette_use, # There is an upstream, internal problem with ggscatter that producing warnings.
    label = label, repel = repel,
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim,
    ...
  ) +
    ggpubr::grids(axis = "xy") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1)) +
    ggplot2::scale_color_manual(values = palette_use)

  # Additional plot features ____________________________________________________________
  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (!isFALSE(hline)) p <- p + ggplot2::geom_hline(yintercept = hline, color = line.col, linewidth = line.width, linetype = line.type)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline, color = line.col, linewidth = line.width, linetype = line.type)
  if (!isFALSE(abline)) p <- p + ggplot2::geom_abline(intercept = abline[1], slope = abline[2], color = line.col, linewidth = line.width, linetype = line.type)
  if (add_contour_plot) p <- p + ggplot2::geom_density_2d()

  if (correlation_r2 %in% c("pearson", "spearman")) {
    p <- p + ggpubr::stat_cor(method = correlation_r2)
  } else if (!isFALSE(correlation_r2)) {
    warning("correlation_r2 must be either 'pearson' or 'spearman'")
  }

  if (logX) p <- p + ggplot2::scale_x_log10()
  if (annotation_logticks_X) p <- p + ggplot2::annotation_logticks(sides = "b")

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")

  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  # Save plot and okject ____________________________________________________________
  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logX), flag.nameiftrue(logY), "scatter", ext)
  }

  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h,
                   also.pdf = also.pdf, save.obj = save.obj)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}




# ______________________________________________________________________________________________----
# List-based distribution plots ----
# ____________________________________________________________________



#' @title Quickly draw and save a boxplot (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a boxplot from
#' a two-column data frame or a named list of values. The first column (or list names) is plotted on the X axis,
#' the second column (or list values) on the Y axis.  It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param df_XYcol_or_list Data as a two-column data frame, where column 1 is the X axis, alternatively a uniquely named list of values.
#' @param x The index or name of the column to be plotted on the X axis. Default: `1`.
#' @param y The index or name of the column to be plotted on the Y axis. Default: `2`.
#' @param col The index or name of the column to be used for coloring the plot. Default: `NULL`.
#' @param fill Fill color of the plot. Default: `gold`.
#'
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the plotname. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param ylab Y-axis label. Default: `NULL`.
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param outlier.shape outlier shape. NA to hide.
#' @param stat.test Do a statistical test?
#' @param stat.method stat method. NULL for default
#' @param stat.label.y.npc Stat label y position
#' @param stat.label.x Stat label x position
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param hide.legend Hide legend.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param max.categ The maximum allowed number of unique categories.
#' @param add Add additional graphical elements to the plot. Default: NULL.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @importFrom CodeAndRoll2 is.list.simple
#' @export
#' @examples data("ToothGrowth")
#' ToothLen.by.Dose <- ToothGrowth[, c("dose", "len")]
#' qboxplot(ToothLen.by.Dose)
qboxplot <- function(
    df_XYcol_or_list,
    x = 1, y = 2,
    plotname = FixPlotName(substitute(df_XYcol_or_list)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    col = NULL,
    fill = getOption("gg.fill.col", "gold"),
    outlier.shape = NULL,
    stat.test = TRUE,
    # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
    stat.method = NULL, stat.label.y.npc = "top", stat.label.x = NULL,
    palette_use = getOption("gg.palette_use", 'jco'),
    hide.legend = FALSE,
    ylab = NULL, # xlab = NULL,
    logY = FALSE, # , logX = FALSE
    annotation_logticks_Y = logY,
    xlab.angle = 90,
    hline = FALSE, vline = FALSE,

    max.categ = 100,
    add = NULL,
    # position = if(add == "jitter") position_dodge(width=.7) else NULL,
    # add.params = if(add == "jitter") list(shape = "supp"),
    grid = "y", mdlink = getOption("gg.mdlink", F),
    w = qqqAxisLength(df_XYcol_or_list), h = 6,
    ...) {

  stopifnot(
    is.data.frame(df_XYcol_or_list) | CodeAndRoll2::is.list.simple(df_XYcol_or_list),
    is.numeric(x) | is.character(x), is.numeric(y) | is.character(y),
    is.character(plotname),
    is.character(filename) | is.null(filename), is.character(subtitle) | is.null(subtitle),
    is.character(caption) | is.null(caption), is.character(ylab) | is.null(ylab),
    is.character(suffix) | is.null(suffix), is.character(ext),
    is.logical(logY), is.logical(hide.legend), is.logical(also.pdf), is.logical(save.obj), is.logical(save),
    is.logical(mdlink), is.logical(plot), is.logical(stat.test), is.logical(annotation_logticks_Y),
    is.logical(outlier.shape) | is.null(outlier.shape),
    is.character(ext), is.character(palette_use), is.character(grid),
    is.numeric(w), is.numeric(h), is.numeric(max.categ),
    is.null(xlab.angle) | is.numeric(xlab.angle)
  )


  # Define fill color
  if (CodeAndRoll2::is.list.simple(df_XYcol_or_list)) {
    lsX <- df_XYcol_or_list
    df_XYcol <- qqqList.2.DF.ggplot(lsX)
    if (length(fill) == length(lsX)) {
      fill <- rep(fill, sapply(lsX, length))
    }
  } else {
    df_XYcol <- df_XYcol_or_list
    stopifnot(fill %in% colnames(df_XYcol) | length(fill) == nrow(df_XYcol) |
      is.character(fill) | is.null(fill))
  }

  .assertMaxCategories(df_XYcol, col = x, max.categ)
  vars <- colnames(df_XYcol)
  names(vars) <- vars
  stopifnot(
    if (is.numeric(x)) x <= length(vars) else x %in% vars,
    if (is.numeric(y)) y <= length(vars) else y %in% vars,
    is.numeric(df_XYcol[[vars[y]]])
  )

  palette_use_bac <- palette_use
  if (length(fill) > 1) {
    stopifnot(
      "Length of fill must be 1 or equal to the number of rows in the data frame." =
        length(fill) == nrow(df_XYcol)
    )
  } else if (length(fill) == 1) {
    fill <- rep(fill, nrow(df_XYcol))
    palette_use <- fill
  } else {
    message("fill is NULL. Using default fill color.")
  }

  df_XYcol$"condition" <- fill
  fill <- "condition"


  if (!is.null(col)) {
    if (is.numeric(col) & col < length(vars)) col <- col
    if (col %in% vars) col <- vars[col]
    fill <- col # if col (color as a column name) is provided, fill is set to col
    palette_use <- palette_use_bac
  }

  p <- ggpubr::ggboxplot(
    data = df_XYcol, x = vars[x], y = vars[y],
    fill = fill,
    # size = 1,
    title = plotname,
    subtitle = subtitle,
    caption = caption,
    palette = palette_use,
    outlier.shape = outlier.shape,
    add = add,
    ...
  ) +
    ggplot2::labs(y = ylab) +

    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (!isFALSE(hline)) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")

  if (stat.test) p <- p + ggpubr::stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, flag.nameiftrue(logY), "boxplot", ext)
  }
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf, save.obj = save.obj)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}


# _________________________________________________________________________________________________
#' @title Quickly draw and save a violin plot (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a violin plot from
#' a two-column data frame or a named list of values. The first column (or list names) is plotted
#' on the X axis, the second column (or list values) on the Y axis. It is a wrapper around
#' `ggpubr::ggviolin()`, with the automation of many features. All `ggpubr` parameters can be
#' accessed through the `...` argument.#'  It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param df_XYcol_or_list Data as a two-column data frame, where column 1 is the X axis, alternatively a uniquely named list of values.
#' @param x The index or name of the column to be plotted on the X axis. Default: `1`.
#' @param y The index or name of the column to be plotted on the Y axis. Default: `2`.
#' @param col The index or name of the column to be used for coloring the plot. Default: `NULL`.
#' @param fill Fill color of the plot. Default: `gold`.
#' @param plotname Name of the plot
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param stat.test Do a statistical test?
#' @param stat.method stat method. NULL for default
#' @param stat.label.y.npc Stat label y position
#' @param stat.label.x Stat label x position
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param hide.legend Hide legend.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#' @param max.categ The maximum allowed number of unique categories.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @importFrom CodeAndRoll2 is.list.simple
#' @export
#' @examples data("ToothGrowth")
#' ToothLen.by.Dose <- ToothGrowth[, c("dose", "len")]
#' qviolin(ToothLen.by.Dose)
qviolin <- function(
    df_XYcol_or_list,
    x = 1, y = 2, col = NULL,
    plotname = FixPlotName(substitute(df_XYcol_or_list)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    fill = getOption("gg.fill.col", "gold"),
    stat.test = FALSE,
    stat.method = NULL, stat.label.y.npc = "top", stat.label.x = 0.5,
    palette_use = getOption("gg.palette_use", 'jco'),
    hide.legend = FALSE,
    logY = FALSE, # , logX = FALSE
    annotation_logticks_Y = logY,
    xlab.angle = 45,
    hline = FALSE, vline = FALSE,
    # , outlier.shape = NULL
    # , stat.method = "wilcox.test", stat.label.y.npc = 0, stat.label.x = .5
    max.categ = 100,
    grid = FALSE, mdlink = getOption("gg.mdlink", F),
    w = qqqAxisLength(df_XYcol_or_list), h = 6,
    ...) {
  df_XYcol <- if (CodeAndRoll2::is.list.simple(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list
  message("nrow(df_XYcol): ", nrow(df_XYcol))
  .assertMaxCategories(df_XYcol, col = x, max.categ)

  # Define fill color
  vars <- colnames(df_XYcol)
  names(vars) <- vars
  stopifnot(
    if (is.numeric(x)) x <= length(vars) else x %in% vars,
    if (is.numeric(y)) y <= length(vars) else y %in% vars,
    is.numeric(df_XYcol[[vars[y]]])
  )
  if (!is.null(col)) {
    if (is.numeric(col) & col < length(vars)) col <- col
    if (col %in% vars) col <- vars[col]
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
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")
  if (!isFALSE(hline)) p <- p + ggplot2::geom_hline(yintercept = hline)
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline)
  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (stat.test) p <- p + ggpubr::stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, "violinplot", flag.nameiftrue(logY), ext)
  }
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf, save.obj = save.obj)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}



# _________________________________________________________________________________________________
#' @title Quickly draw and save a stripchart (png, pdf, ggobj.qs)
#'
#' @description This all-in-one function draws, annotates, displays and saves a stripchart from
#' a two-column data frame or a named list of values. The first column (or list names) is plotted
#' on the X axis, the second column (or list values) on the Y axis. It is a wrapper around
#' `ggpubr::ggstripchart()`, with the automation of many features. All `ggpubr` parameters can be
#' accessed through the `...` argument. It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param df_XYcol_or_list Data as a two-column data frame, where column 1 is the X axis, alternatively a uniquely named list of values.
#' @param x The index or name of the column to be plotted on the X axis. Default: `1`.
#' @param y The index or name of the column to be plotted on the Y axis. Default: `2`.
#' @param col The index or name of the column to be used for coloring the plot. Default: `NULL`.
#' @param fill Fill color of the plot. Default: `gold`.
#'
#' @param plotname Name of the plot
#' @param subtitle Optional subtitle text added below the title. Default is NULL.
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default = suffix.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param ylab Y-axis label. Default: NULL.
#' @param add Add boxplot or violin chart? Default: add = c("violin", "mean_sd"); it can be "boxplot" or only "mean_sd".
#' @param logY Make Y axis log10-scale.
#' @param hline Draw a horizontal line on the plot.
#' @param vline Draw a vertical line on the plot.
#' @param stat.test Do a statistical test?
#' @param stat.method stat method. NULL for default
#' @param stat.label.y.npc Stat label y position
#' @param stat.label.x Stat label x position
#' @param size.point Size of points.
#' @param xlab.angle Rotate X-axis labels by N degrees. Default: 90
#' @param hide.legend Hide legend.
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param annotation_logticks_Y Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of `logY`.
#' @param grid Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.
#'
#' @param annotate_top_labels Logical indicating whether to add one label per x-category
#' at the top of the plot, or a character/numeric vector of labels to use directly.
#' @param custom_top_labels Optional character or numeric vector of custom labels,
#' one per x-category, overriding automatically computed labels.

#' @param max.categ The maximum allowed number of unique categories.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @importFrom CodeAndRoll2 is.list.simple
#' @examples data("ToothGrowth")
#' ToothLen.by.Dose <- ToothGrowth[, c("dose", "len")]
#' qstripchart(ToothLen.by.Dose)
#'
#' @export
qstripchart <- function(
    df_XYcol_or_list,
    x = 1, y = 2, col = NULL,
    plotname = FixPlotName(substitute(df_XYcol_or_list)),
    subtitle = NULL,
    suffix = NULL,
    caption = suffix,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    fill = getOption("gg.fill.col", "gold"),
    add = c("violin", "mean_sd"),
    size.point = .2,
    stat.test = TRUE,
    stat.method = NULL, stat.label.y.npc = "top", stat.label.x = 0.75,
    palette_use = getOption("gg.palette_use", 'jco'),
    hide.legend = FALSE,

    logY = FALSE, # , logX = FALSE
    annotation_logticks_Y = logY,
    xlab.angle = 90, xlab = "",
    ylab = NULL,
    hline = FALSE, vline = FALSE,

    annotate_top_labels = FALSE,
    custom_top_labels = FALSE,
    max.categ = 100,
    grid = "y", mdlink = getOption("gg.mdlink", F),
    w = qqqAxisLength(df_XYcol_or_list), h = 6,
    ...) {
  message("Column 1 should be the X-, Column 2 the Y-axis.")
  stopifnot(
    CodeAndRoll2::is.list.simple(df_XYcol_or_list) | is.data.frame(df_XYcol_or_list)
    # , length(df_XYcol_or_list) > 2
    , length(x) == 1, length(y) == 1,
    is.numeric(x) | is.character(x),
    is.numeric(y) | is.character(y),
    is.null(col) | is.numeric(col) | is.character(col),
    is.null(fill) | is.numeric(fill) | is.character(fill)
  )
  # df_XYcol <- df_XYcol_or_list
  df_XYcol <- if (CodeAndRoll2::is.list.simple(df_XYcol_or_list)) qqqList.2.DF.ggplot(df_XYcol_or_list) else df_XYcol_or_list
  .assertMaxCategories(df_XYcol, col = x, max.categ)

  # Define fill color
  vars <- colnames(df_XYcol)
  names(vars) <- vars
  stopifnot(
    if (is.numeric(x)) x <= length(vars) else x %in% vars,
    if (is.numeric(y)) y <= length(vars) else y %in% vars,
    is.numeric(df_XYcol[[vars[y]]])
  )
  # browser()
  if (!is.null(col)) {
    if (is.numeric(col) & col < length(vars)) col <- col
    if (col %in% vars) col <- vars[col]
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
    xlab = eval.parent(xlab),
    ...
  ) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = xlab.angle, hjust = 1))

  if (logY) p <- p + ggplot2::scale_y_log10()
  if (annotation_logticks_Y) p <- p + ggplot2::annotation_logticks(sides = "l")
  if (stat.test) p <- p + ggpubr::stat_compare_means(method = stat.method, label.y.npc = stat.label.y.npc, label.x = stat.label.x, ...)
  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")
  if (grid %in% c("xy", "x", "y")) p <- p + ggpubr::grids(axis = grid)
  if (!isFALSE(hline)) p <- p + ggplot2::geom_hline(yintercept = hline, color = "darkgrey", linewidth = 0.5, linetype = "dashed")
  if (!isFALSE(vline)) p <- p + ggplot2::geom_vline(xintercept = vline, color = "darkgrey", linewidth = 0.5, linetype = "dashed")



  # Top row annotation
  if (!isFALSE(annotate_top_labels)) {

    xvar <- vars[x]
    yvar <- vars[y]

    labels <- if (isFALSE(custom_top_labels)) {
      NULL
    } else {
      stopifnot(length(custom_top_labels) == dplyr::n_distinct(df_XYcol[[xvar]]))
      custom_top_labels
    }

    p <- p + qqqAnnotateTopLabels(
      df = df_XYcol,
      x  = xvar,
      y  = yvar,
      labels = labels
    )
  }

  # THIS WAS NEVER WORKING
  # if (!isFALSE(annotate_top_labels)) {
  #   toplabs <- qqqSummarize_mean_median(df_XYcol, x = eval.parent(x), y = eval.parent(y))
  #
  #   if (!isFALSE(custom_top_labels)) {
  #     stopifnot(length(custom_top_labels) == length(unique(df_XYcol[[vars[x]]])))
  #   } else {
  #     custom_top_labels
  #   }
  #   p <- p + annotate_top_labels()
  # }


  fix <- sppp("stripchart", sppp(add))
  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(plotname, suffix, fix, flag.nameiftrue(logY), ext)
  }
  if (save) qqSave(ggobj = p, title = plotname, fname = file_name, ext = ext, w = w, h = h, also.pdf = also.pdf, save.obj = save.obj)
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}




# _________________________________________________________________________________________________
#' @title Quickly draw and save a Venn Diagram (png, pdf, ggobj.qs)

#' @description This all-in-one function draws, annotates, displays and saves a Venn Diagram from
#' a named list of values. It is a wrapper around `ggVennDiagram::ggVennDiagram()`,
#' with the automation of many features. All `ggVennDiagram` parameters can be
#' accessed through the `...` argument. It can automatically save the plot as png (default) and/or
#' pdf files, and the ggplot object as a .qs file.
#'
#' @param list The variable to plot.
#' @param plotname The title of the plot and the name of the file (unless specified in `filename`).
#' @param subtitle The subtitle of the plot. Default: paste (length(unique(unlist(list))), 'elements in total')
#' @param suffix Optional suffix added to the filename. Default is NULL.
#' @param caption Optional text added to bottom right corner of the plot. Default is list element lengths parsed.
#' @param caption2 Optional text added to bottom right corner of the plot. Default is NULL.
#' @param filename Manually provided filename (optional). Default: parsed from `plotname`.
#'
#' @param plot Display the plot.
#' @param save Save the plot into a file.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf Save plot in both png and pdf formats.
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#'
#' @param col.min Color scale minimum. Default: white.
#' @param col.max Color scale maximum. Default: red.
#' @param hide.legend Hide legend.
#' @param x_exp Expand axis to show long set labels. Default: 0.2.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the plot.
#' @param h Height of the plot. Default: h = w * 0.75.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @importFrom ggVennDiagram ggVennDiagram
#' @examples LetterSets <- list("One" = LETTERS[1:7], "Two" = LETTERS[3:12])
#' qvenn(LetterSets)
#'
#' @export
qvenn <- function(
    list,

    plotname = FixPlotName(substitute(list)),
    suffix = NULL,
    subtitle = paste(length(unique(unlist(list))), "elements in total"),
    caption = parseParamStringWNames(sapply(list, length)),
    caption2 = NULL,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    col.min = "white", col.max = "red",
    hide.legend = FALSE,
    x_exp = .2,
    mdlink = getOption("gg.mdlink", F),
    w = 8, h = 0.75 * w,
    ...) {
  #
  stopifnot(is.list(list), length(list) > 0L)

  if (!is.null(caption2)) caption <- paste0(caption2, "\n", caption, "\n")

  p <- ggVennDiagram::ggVennDiagram(list, ...) +
    ggplot2::scale_fill_gradient(low = col.min, high = col.max) +
    ggplot2::labs(
      title = paste(" ", plotname),
      subtitle = paste(" ", subtitle, "\n"),
      caption = caption
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = x_exp)) + # expand axis to show long set labels
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = "white"))

  if (hide.legend) p <- p + ggplot2::theme(legend.position = "none")

  s1 <- paste0(length(list), "s")
  s2 <- kpp(s1, paste0(length(unique(unlist(list))), "el"))

  file_name <- if (!is.null(filename)) kpp(filename, s2) else sppp(plotname, suffix, s2, "venn", ext)
  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name,
      ext = ext, w = w, h = h, also.pdf = also.pdf, save.obj = save.obj
    )
  }
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}




# _________________________________________________________________________________________________
#' @title Quick Heatmap Plot
#'
#' @description Generates a heatmap plot using the `ggheatmap` package with features such as automatic
#' file saving and markdown link generation. This function simplifies the process of creating and
#' customizing heatmaps, with support for clustering, annotations, and scaling.
#'
#' @param data_matrix The data matrix to be plotted; must be either a matrix or a data frame.
#' Default: None (must be provided).
#'
#' @param title The main title for the heatmap. Default: "Heatmap".
#' @param plotname The base name for the plot file. Default: "heatmap".
#' @param subtitle The subtitle for the heatmap. Default: "NULL" (indicating no subtitle).
#' @param caption The caption for the heatmap. Default: "mousse".
#' @param suffix An optional suffix to append to the plot filename. Default: NULL.
#' @param filename The specific filename to save the plot as; if NULL, a name is generated based on
#' `plotname` and other parameters. Default: NULL.
#'
#' @param save Logical; indicates whether to save the plot to file. Default: TRUE.
#' @param ext The file extension for the saved plot image, adjusted based on `also.pdf`.
#' Default: "png".
#' @param also.pdf Logical; indicates whether to also save the plot as a PDF. Default: FALSE.
#'
#' @param color The color palette for the heatmap values, using a blue-white-yellow gradient.
#' Default: colorRampPalette(c( "#0073c2","white","#efc000"))(100).
#' @param legendName The title of the heatmap legend. Default: "Intensity".
#' @param scale Character; indicates how to scale the data ("none", "row", or "column").
#' Default: "row".
#' @param cluster_rows Logical; indicates whether to cluster rows. Default: FALSE.
#' @param cluster_cols Logical; indicates whether to cluster columns. Default: FALSE.
#' @param annotation_rows Data frame with row annotations. Default: NULL.
#' @param annotation_cols Data frame with column annotations. Default: NULL.
#' @param annotation_color List of colors for the annotations. Default: NULL.
#' @param xlab The global label for the x-axis; this might not display due to limitations.
#' @param ylab The global label for the y-axis; this might not display due to limitations.
#'
#' @param mdlink Insert .pdf and .png image links to a markdown report file, setup by
#' `MarkdownReports::setup_MarkdownReports()`, located at `path_of_report`. Default: FALSE
#' @param w Width of the saved plot image, in inches. Default: 10.
#' @param h Height of the saved plot image, in inches. Default: 8.
#' @param ... Additional arguments passed to `ggheatmap`.
#'
#' @return A `ggplot` object representing the heatmap, conditionally modified based on the provided
#' parameters.
#'
#' @examples
#' \dontrun{
#' # Create example matrix
#' mat <- as.matrix(mtcars)
#'
#' # Row annotations: cyl and gear
#' row_annot <- mtcars[, c("cyl", "gear")]
#'
#' # Column annotations: arbitrary grouping
#' col_annot <- data.frame(
#'   group = rep(c("A", "B"), length.out = ncol(mat)),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Example 1: Basic heatmap with default settings
#' qheatmap2_(
#'   mat,
#'   plotname = "mtcars basic heatmap",
#'   save = FALSE
#' )
#'
#' # Example 2: Heatmap scaled by column
#' qheatmap(
#'   mat,
#'   plotname = "mtcars heatmap scaled by column",
#'   scale = "column",
#'   save = FALSE
#' )
#'
#' # Example 3: Heatmap with row and column annotations
#' qheatmap(
#'   mat,
#'   plotname = "mtcars heatmap with annotations",
#'   scale = "column",
#'   row_annotation = row_annot,
#'   col_annotation = col_annot,
#'   save = FALSE
#' )
#' }
#'
#' @importFrom heatmaply ggheatmap
#' @importFrom ggplotify as.ggplot
# #' @importFrom MarkdownHelpers ww.set.file.extension
#'
#' @export
qheatmap <- function(
    data_matrix,

    plotname = FixPlotName("Heatmap of", substitute(data_matrix)), # default plot title
    subtitle = NULL,
    suffix = NULL,
    caption = "caption",
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    also.pdf = FALSE,
    save.obj = getOption("gg.save.obj", F),

    colors = grDevices::colorRampPalette(c("#313695", "#FFFFFF", "#A50026"))(256), # default diverging palette
    legend_title = "Intensity",
    scale = c("none", "row", "column"), # heatmaply scaling options
    cluster_rows = TRUE,
    cluster_cols = FALSE,
    row_annotation = NULL, # optional: row annotations
    col_annotation = NULL, # optional: column annotations
    row_annot_colors = NULL, # custom palette for row annotations
    col_annot_colors = NULL, # custom palette for column annotations

    xlab = "x axis",
    ylab = "y axis",
    mdlink = getOption("gg.mdlink", F),
    w = 7, h = 6,
    ...) {
  warning("   !!! qheatmap is in experimental status !!! ")
  # ___ Input validation ___
  stopifnot(
    (is.matrix(data_matrix) || is.data.frame(data_matrix)) && is.numeric(as.matrix(data_matrix)),
    # ensure input is matrix or dataframe of numerics
    nrow(data_matrix) > 0L, ncol(data_matrix) > 0L, # ensure non-empty
    is.logical(cluster_rows), is.logical(cluster_cols), # clustering flags must be logical
    is.logical(plot), is.logical(save), is.logical(also.pdf), is.logical(save.obj), is.logical(mdlink),
    is.numeric(w) && w > 0, is.numeric(h) && h > 0 # positive numeric dimensions
  )

  # ___ Prepare arguments for heatmaply::ggheatmap ___
  scale <- match.arg(scale) # ensure scale arg matches options
  extra_args <- list(...) # capture additional user-specified args
  hm_args <- list(x = data_matrix, colors = colors, scale = scale)

  # clustering options (unless already passed in ...)
  if (!"Rowv" %in% names(extra_args)) hm_args$Rowv <- isTRUE(cluster_rows)
  if (!"Colv" %in% names(extra_args)) hm_args$Colv <- isTRUE(cluster_cols)
  if (!"key.title" %in% names(extra_args)) hm_args$key.title <- legend_title

  # add row/col annotations and palettes
  if (!is.null(row_annotation)) {
    hm_args$row_side_colors <- row_annotation
    if (!is.null(row_annot_colors)) hm_args$row_side_palette <- row_annot_colors
  }
  if (!is.null(col_annotation)) {
    hm_args$col_side_colors <- col_annotation
    if (!is.null(col_annot_colors)) hm_args$col_side_palette <- col_annot_colors
  }

  # ___ Build heatmap ___
  # do.call allows passing both default and user-supplied args
  gghm_obj <- do.call(heatmaply::ggheatmap, c(hm_args, extra_args))

  # convert to a ggplot object for adding labs/themes
  gghm_obj <- ggplotify::as.ggplot(gghm_obj)

  # ___ Auto-generate subtitle if not provided ___
  if (is.null(subtitle)) {
    subtitle <- paste0(
      nrow(data_matrix), "-by-", ncol(data_matrix),
      " | Median:", median(data_matrix, na.rm = TRUE),
      " | CV:", round(cv(data_matrix), digits = 1)
    )
  }

  # ___ Add ggplot2 labels and theme ___
  gghm_obj <- gghm_obj +
    ggplot2::labs(
      title = plotname,
      subtitle = subtitle,
      caption = caption,
      x = xlab,
      y = ylab,
      fill = legend_title
    ) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90)) + # rotate y-axis title
    ggplot2::theme(axis.title.x = ggplot2::element_text())

  # ___ File name generation ___
  file_name <- if (!is.null(filename)) {
    filename
  } else {
    FixPlotName(
      plotname, suffix, "heatmap",
      flag.nameiftrue(isTRUE(cluster_rows)),
      flag.nameiftrue(isTRUE(cluster_cols)), ext
    )
  }

  # ___ Save the plot if requested ___
  if (save) {
    qqSave(
      ggobj = gghm_obj,
      title = plotname,
      fname = file_name,
      ext = ext,
      w = w,
      h = h,
      also.pdf = also.pdf,
      save.obj = save.obj
    )
  }

  # ___ Optionally add a markdown image link ___
  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) gghm_obj
  invisible(gghm_obj)
}


#' @title Draw and Save a Doubledecker Mosaic Plot
#'
#' @description
#' Creates a mosaic-style (doubledecker) stacked bar plot where the X-axis bar width is proportional
#' to the group size and the Y-axis variable determines the color (fill). This function wraps
#' `ggmosaic::geom_mosaic()` in the style of ggExpress plotting wrappers and supports automatic file
#' saving, Markdown linking, and default color palettes.
#'
#' @param df A data frame containing the variables to plot. Must include the X and Y columns.
#' @param x The column name (string) for the X-axis variable.
#' @param y The column name (string) for the Y-axis variable used for fill color.
#' @param weight Optional column name (string) specifying weights or frequencies.
#'   If `NULL`, the function looks for a "Freq" column or assumes equal weights.
#'   Default: `NULL`.
#'
#' @param plotname Title of the plot and base for the filename. Default: `"Mosaic plot: x vs y"`.
#' @param subtitle Optional subtitle text. Default: `NULL`.
#' @param caption Optional text to display at the bottom right of the plot. Default: `NULL`.
#' @param filename Optional manual filename override. Default: `NULL`.
#'
#' @param plot Display the plot after creation. Default: `TRUE`.
#' @param save Save the plot to file. Default: `TRUE`.
#' @param ext File extension for output image. Default:
#' @param also.pdf Save plot in both PNG and PDF formats. Default: `TRUE`.
#' @param save.obj Save the ggplot object as an `.RDS` file. Default: `FALSE`.
#' @param save_matrix If `TRUE`, saves the contingency table as a TSV file. Default: `FALSE`.
#' #'
#' @param palette_use Color palette to use. Either a `ggpubr::get_palette` palette or a custom vector of colors. Default: 'jco'.
#' @param mdlink Insert a Markdown image link in to a markdown report file,
#' setup by `MarkdownReports::setup_MarkdownReports()`. Default: FALSE
#' @param limitsize Passed to `ggsave()` to limit image size. Default: `FALSE`.
#' @param w Width of the saved plot (in inches). Default: `6`.
#' @param h Height of the saved plot (in inches). Default: `5`.
#' @param ... Additional arguments passed to `ggmosaic::geom_mosaic()`.
#'
#' @return A `ggplot` object representing the mosaic plot.
#' @export
#'
#' @importFrom ggmosaic geom_mosaic ddecker product
#' @importFrom ggplot2 ggplot aes labs theme_minimal scale_fill_brewer scale_fill_manual
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom scales hue_pal
#'
#' @examples
#' df <- data.frame(
#'   Group = rep(c("A", "B", "C"), each = 50),
#'   Outcome = sample(c("Yes", "No"), 150, replace = TRUE, prob = c(0.6, 0.4))
#' )
#' qmosaic(df = df, x = "Group", y = "Outcome")
qmosaic <- function(
    df,
    x, y,
    plotname = paste("Mosaic plot:", x, "vs", y),
    subtitle = NULL, caption = NULL,
    filename = NULL,

    plot = TRUE,
    save = TRUE,
    also.pdf = FALSE,
    ext = "png",
    # ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
    save.obj = getOption("gg.save.obj", F),
    save_matrix = FALSE,

    weight = NULL,
    palette_use = c("Set2", "Dark2", "Paired", "Pastel1", "Accent", "Set3", "Spectral")[1],
    limitsize = FALSE, mdlink = getOption("gg.mdlink", F),
    w = 6, h = 5,
    ...) {
  warning("   !!! qmosaic is in experimental status !!! ")

  stopifnot(
    is.data.frame(df),
    all(c(x, y) %in% colnames(df))
  )

  # detect weight column
  if (is.null(weight)) {
    if ("Freq" %in% colnames(df)) {
      weight <- "Freq"
    } else {
      df$Freq <- 1
      weight <- "Freq"
    }
  }

  # validate palette
  # fill_scale <- if (palette_use == "auto") {
  #   ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(df[[y]]))))
  # } else if (palette_use %in% rownames(RColorBrewer::brewer.pal.info)) {
  #   ggplot2::scale_fill_brewer(palette = palette_use)
  # } else {
  #   warning("Invalid palette name: ", palette_use, " — using 'Set2'.")
  #   ggplot2::scale_fill_brewer(palette = "Set2")
  # }

  if (!palette_use %in% rownames(RColorBrewer::brewer.pal.info)) {
    warning("Invalid palette: ", palette_use, " — using 'Set2'.")
    palette_use <- "Set2"
  }
  fill_scale <- ggplot2::scale_fill_brewer(palette = palette_use)


  # Build plot _______
  p <- ggplot2::ggplot(data = df) +
    ggmosaic::geom_mosaic(
      ggplot2::aes(
        x = ggmosaic::product(!!rlang::sym(x)),
        fill = !!rlang::sym(y),
        weight = !!rlang::sym(weight)
      ),
      divider = ggmosaic::ddecker(),
      colour = "white",
      ...
    ) +
    fill_scale +
    ggplot2::labs(
      title = plotname,
      subtitle = subtitle,
      caption = caption,
      x = x, y = y
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )


  # file name
  file_name <- if (!is.null(filename)) filename else FixPlotName(plotname, "ddecker", ext)

  # print matrix
  {
    xy_matrix <- table(df[[x]], df[[y]]) # build contingency table
    print(xy_matrix)
    if (save_matrix) write.simple.tsv(xy_matrix)
  }

  # save
  if (save) {
    qqSave(
      ggobj = p, title = plotname, fname = file_name, ext = ext,
      w = w, h = h, limitsize = limitsize, also.pdf = also.pdf, save.obj = save.obj
    )
  }

  if (mdlink & save) qMarkdownImageLink(file_name)
  if (plot) print(p)
  invisible(p)
}


# ______________________________________________________________________________________________----
# Auxiliary functions ----
# ____________________________________________________________________


#' @title qqSave
#'
#' @description Quick-Save ggplot objects to file with automatic file naming.
#' Default format: png, optionally also: .pdf and .qs ggplot object.
#' @param ggobj Plot as ggplot object.
#'
#' @param title title field for pdf file (saved into file metadata)
#' @param fname Manual filename
#' @param suffix A suffix added to the filename. Default: NULL.
#'
#' @param ext File extension. Default: "png".
#' @param png.subdir Save png files into a subdirectory. Default: FALSE.
#' @param png.dir.name Name of the png subdirectory. Default: "png".
#' @param also.pdf Save plot in both png and pdf formats. Default: FALSE.
#' @param pdf.subdir Save pdf files into a subdirectory. Default: FALSE.
#' @param pdf.dir.name Name of the pdf subdirectory. Default: "pdf".
#' @param save.obj Save the ggplot object to a file. Default: FALSE.
#' @param obj.subdir Save ggplot objects into a subdirectory. Default: FALSE.
#' @param obj.dir.name Name of the ggplot object subdirectory. Default: "ggobj".#'
#'
#' @param bgcol Plot background color. Default: "white".
#' @param max.obj.size Maximum allowed size of the ggplot object to be saved (in bytes). Default: 5e+06 (5 MB).
#' @param w Width of the plot.
#' @param h Height of the plot.
#' @param plot_on_page Use standard page sizes for plots? Options are:
#' FALSE, "A4p", "A4l", "A5p", "A5l". Overwrites w and h.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should work).
#'
#' @examples xplot <- ggplot2::qplot(12)
#' qqSave(ggobj = xplot)
#' qqSave(ggobj = xplot, ext = "pdf")
#'
#' @importFrom cowplot save_plot
#' @importFrom qs qsave
#' @importFrom tictoc tic toc
#'
#' @export
qqSave <- function(
    ggobj,
    title = FALSE,
    fname = FALSE,
    suffix = NULL,

    ## Argument `ext` is completely ignored atm.
    ## It is needed so that ggXXX do not fail
    ## Atm, png is always saved. This may be changed in the future.
    ext = "png",
    png.subdir = FALSE, png.dir.name = "pdf",
    also.pdf = FALSE,
    pdf.subdir = FALSE, pdf.dir.name = "pdf",
    save.obj = getOption("gg.save.obj", F),
    obj.subdir = FALSE, obj.dir.name = "ggobj",

    bgcol = "white",
    max.obj.size = 5e+06, # 5 MB
    w = 6, h = w,
    plot_on_page = list(FALSE, "A4p", "A4l", "A5p", "A5l")[[1]],
    ...) {

  stopifnot(
    is.ggplot(ggobj), is.numeric(max.obj.size), is.numeric(w), is.numeric(h),
    is.logical(also.pdf), is.logical(pdf.subdir), is.logical(save.obj), is.logical(obj.subdir),
    is.character(bgcol), is.character(pdf.dir.name), is.character(obj.dir.name),
    is.null(suffix) | is.character(suffix),
    is.logical(title) | is.character(title), is.logical(fname) | is.character(fname)
  )

  # Helper
  add_ext_if_missing <- function(x, ext) ifelse(grepl(paste0("\\.", ext, "$"), x), x, sppp(x, ext))

  tictoc::tic()
  if (isFALSE(title)) title <- make.names(as.character(substitute(ggobj)))
  fname <- if (isFALSE(fname)) sppp(title, suffix) else fname


  # Determine page size
  if (!isFALSE(plot_on_page)) {
    # Get A4 or A5 dimensions
    dimA4 <- c(8.27, 11.69) * ifelse(grepl("^A5", plot_on_page), 0.5, 1)
    # Flip for landscape
    if (grepl("l$", plot_on_page)) dimA4 <- rev(dimA4)
    w <- dimA4[1]; h <- dimA4[2]
  }

  # Create PDF / object subdirectories and adjust file paths if needed ___________________________
  if (also.pdf) {
    fname_pdf <- add_ext_if_missing(fname, "pdf")
    if (pdf.subdir) {
      dir.create(pdf.dir.name, showWarnings = FALSE)
      fname_pdf <- file.path(pdf.dir.name, fname_pdf)
    }
  }

  if (save.obj) {
    if (obj.subdir) {
      dir.create(obj.dir.name, showWarnings = FALSE)
      fname_qs <- file.path(obj.dir.name, fname)
    }
    fname_qs <- add_ext_if_missing(fname, "qs")
  }


  # Plotting ______________________________________________________________________________________
  # Set the plot background to white
  ggobj <- ggobj + ggplot2::theme(plot.background = ggplot2::element_rect(fill = bgcol, color = bgcol))

  # Save the plot
  cowplot::save_plot(
    plot = ggobj, filename = add_ext_if_missing(fname, 'png'),
    base_width = w, base_height = h, ...
  )


  # Saving ______________________________________________________________________________________
  if (also.pdf) {
    cowplot::save_plot(
      plot = ggobj, filename = fname_pdf, base_width = w, base_height = h,
      title = ww.ttl_field(title, creator = "ggExpress"),
      ...)
  }

  if (save.obj) {
    ggobj.size <- object.size(ggobj)
    if (ggobj.size > max.obj.size) {
      warning("The ggplot object is larger than " , max.obj.size/1e6, "MBs. It is: ", iround(ggobj.size/1e6),
              " MB. Increase max.obj.size to save the object.\n")
    } else {
      qs::qsave(ggobj, file = fname_qs)
      CMND <- paste0("ggplot_obj <- xread('", getwd(), "/", fname_qs, "')")
      message(CMND)
    }
  }
  tictoc::toc()
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
    plot = FALSE,
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
  if (plot) p1
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
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"))
  cowplot::save_plot(plot = p1, filename = fname, base_height = h, base_width = w)
  ww.FnP_parser(fname)
  if (plot) p1
}



# _________________________________________________________________________________________________
#' @title qMarkdownImageLink
#'
#' @description Insert Markdown image link to .md report
#' @param file_name File name (basename) of the image file to convert into a markdown image link.
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
#' @param factor Length adjustment factor.
#'
#' @examples qqqAxisLength()
qqqAxisLength <- function(vec = 1:20, minLength = 6, factor = 0.4) {
  max(round(length(vec) * factor), minLength)
}


# _________________________________________________________________________________________________
#' @title qqqNamed.Vec.2.Tbl
#'
#' @description Convert a named vector to a table.
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
#' @description Convert a table to a named vector.
#' @param tibble.input tibble.input
#' @param name.column name.column
#' @param value.column value.column
#' @export
#'
#' @examples a <- 1:5
#' x <- tibble::tibble(a, a * 2)
#' qqqTbl.2.Vec(x)
qqqTbl.2.Vec <- function(tibble.input, name.column = 1, value.column = 2) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
  setNames(tibble.input[[value.column]], tibble.input[[name.column]])
}


# _________________________________________________________________________________________________
#' @title qqqList.2.DF.ggplot
#'
#' @description Convert a list to a two-column data frame to plot boxplots and violin plots
#' @param ls A list with all elements named
#' @importFrom CodeAndRoll2 is.list.simple
#' @examples LetterSets <- list("One" = LETTERS[1:7], "Two" = LETTERS[3:12])
#' qqqList.2.DF.ggplot(LetterSets)
#' @export
qqqList.2.DF.ggplot <- function(ls = LetterSets) {
  stopifnot(
    CodeAndRoll2::is.list.simple(ls),
    "Not all list elements have a unique name!" =
      length(ls) == length(unique(names(ls)))
  )
  utils::stack(ls)[, 2:1]
}


# _________________________________________________________________________________________________
#' @title Annotate Top Labels on a ggplot
#'
#' @description Creates a ggplot2 text layer that annotates one label per x-category
#' at the top of the plotting panel. Labels can be computed from the data (default)
#' or provided explicitly.
#'
#' @param df A data frame containing the data to be plotted.
#' @param x Character string. Column name mapped to the x-axis.
#' @param y Character string. Numeric column name mapped to the y-axis.
#' @param labels Optional character or numeric vector of labels, one per x-category.
#' If NULL, labels are computed using \code{fun}.
#' @param fun Summary function used to compute labels when \code{labels} is NULL.
#' Default is \code{mean}.
#' @param digits Number of digits used to round computed labels. Default is 2.
#'
#' @return A ggplot2 layer (\code{geom_text}) to be added to a ggplot object.
#'
#' @examples
#' p + qqqAnnotateTopLabels(df, x = "dose", y = "len")
#' p + qqqAnnotateTopLabels(df, x = "dose", y = "len", labels = c("A", "B", "C"))
#'
qqqAnnotateTopLabels <- function(df, x, y, labels = NULL, fun = mean, digits = 2) {
  stopifnot(is.character(x), is.character(y))

  lab_df <- df |>
    dplyr::group_by(.data[[x]]) |>
    dplyr::summarise(
      lab = if (is.null(labels)) fun(.data[[y]], na.rm = TRUE) else labels[dplyr::cur_group_id()],
      .groups = "drop"
    ) |>
    dplyr::mutate(lab = round(lab, digits))

  ggplot2::geom_text(
    data = lab_df,
    ggplot2::aes(x = .data[[x]], y = Inf, label = lab),
    vjust = -0.6,
    inherit.aes = FALSE
  )
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

#' @title Summarize Mean and Median per Group
#'
#' @description
#' Converts either a list or a long-format data frame to a two-column data frame (group, value) and
#' calculates the mean and median for each group. This helper is compatible with ggplot-style input
#' and uses `ggExpress::qqqList.2.DF.ggplot()` to normalize list input.
#'
#' @param df_XYcol_or_list A data structure to summarize. Can be either:
#'   * A named list of numeric vectors (each element = one group), or
#'   * A data frame in long format with two columns: group and value.
#' @param x The index of the group column in the data frame (if applicable). Default: 1.
#' @param y The index of the value column in the data frame (if applicable). Default: 2.#'
#'
#' @details
#' The function unifies both input types into a consistent long-format data frame before summarizing.
#' The output contains one row per unique group, with corresponding mean and median values.
#'
#' @return A data frame with three columns:
#'   * `group`: group identifiers,
#'   * `mean`: group mean values, and
#'   * `median`: group median values.
#'
#' @examples
#' # Example with a named list
#' lst <- list(A = 1:3, B = 4:6)
#' summarize_mean_median(lst)
#'
#' # Example with a long-format data frame
#' df <- data.frame(group = rep(c("A", "B"), each = 3), value = 1:6)
#' summarize_mean_median(df)
#'
qqqSummarize_mean_median <- function(df_XYcol_or_list, x = 1, y = 2) {
  # --- Input checks
  stopifnot(
    "Input `x` must be a list or data.frame." = is.list(df_XYcol_or_list) || is.data.frame(df_XYcol_or_list),
    "Input `df_XYcol_or_list` must not be empty." = length(df_XYcol_or_list) > 0
  )

  # --- Convert list input to a long-format data.frame
  if (is.list.simple(df_XYcol_or_list)) df_XYcol_or_list <- qqqList.2.DF.ggplot(df_XYcol_or_list) # convert list → 2-column long-format DF

  # --- Validate normalized data
  stopifnot(
    "Input must have at least two columns (group, value)." = ncol(df_XYcol_or_list) >= 2,
    "First column (group) must not be empty." = all(!is.na(df_XYcol_or_list[[x]])),
    "Second column (value) must be numeric." = is.numeric(df_XYcol_or_list[[y]])
  )

  # --- Calculate mean and median per group
  groups <- unique(df_XYcol_or_list[[x]]) # extract unique group identifiers
  data.frame(
    group  = groups,
    mean   = sapply(groups, function(g) mean(df_XYcol_or_list[[y]][df_XYcol_or_list[[x]] == g], na.rm = TRUE)),
    median = sapply(groups, function(g) median(df_XYcol_or_list[[y]][df_XYcol_or_list[[x]] == g], na.rm = TRUE))
  )
}


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

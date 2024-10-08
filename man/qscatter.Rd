% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggExpress.R
\name{qscatter}
\alias{qscatter}
\title{qscatter}
\usage{
qscatter(
  df_XYcol,
  x = 1,
  y = 2,
  plotname = FixPlotName(substitute(df_XYcol)),
  subtitle = NULL,
  suffix = NULL,
  caption = suffix,
  filename = NULL,
  col = c(NULL, 3)[1],
  label = NULL,
  repel = TRUE,
  palette_use = c("RdBu", "Dark2", "Set2", "jco", "npg", "aaas", "lancet", "ucscgb",
    "uchicago")[4],
  hide.legend = FALSE,
  also.pdf = TRUE,
  ext = MarkdownHelpers::ww.set.file.extension(default = "png", also_pdf = also.pdf),
  logX = FALSE,
  logY = FALSE,
  annotation_logticks_Y = logY,
  annotation_logticks_X = logX,
  xlab.angle = 90,
  hline = FALSE,
  vline = FALSE,
  abline = FALSE,
  add_contour_plot = FALSE,
  correlation_r2 = FALSE,
  plot = TRUE,
  save = TRUE,
  mdlink = MarkdownHelpers::unless.specified("b.mdlink", def = FALSE),
  grid = "xy",
  w = 7,
  h = w,
  ...
)
}
\arguments{
\item{df_XYcol}{Data, as 2 column data frame, where col.1 is X axis.}

\item{plotname}{The name of the file and title of the plot.}

\item{subtitle}{Optional subtitle text added below the title. Default is NULL.}

\item{suffix}{Optional suffix added to the filename. Default is NULL.}

\item{caption}{Optional text added to bottom right corner of the plot. Default = suffix}

\item{filename}{Manually provided filename (optional). Default: parse from \code{plotname},}

\item{col}{Color of the plot.}

\item{label}{Point labels}

\item{palette_use}{GGpubr Color palette to use.}

\item{hide.legend}{hide legend}

\item{also.pdf}{Save plot in both png and pdf formats.}

\item{ext}{File extension (.pdf / .png).}

\item{logX}{Make X axis log10-scale.}

\item{logY}{Make Y axis log10-scale.}

\item{annotation_logticks_Y}{Logical indicating whether to add annotation logticks on Y-axis. Default follows the value of \code{logY}.}

\item{annotation_logticks_X}{Logical indicating whether to add annotation logticks on X-axis. Default follows the value of \code{logX}.}

\item{xlab.angle}{Rotate X-axis labels by N degree. Default: 90}

\item{hline}{Draw a horizontal line on the plot, yintercept or FALSE}

\item{vline}{Draw a vertical line on the plot, xintercept or FALSE.}

\item{abline}{Draw a sloped line on the plot. Set to FALSE, or intercept = abline\link{1}, slope = abline\link{2}.}

\item{add_contour_plot}{Add 2D contour plot. See: http://www.sthda.com/english/articles/32-r-graphics-essentials/131-plot-two-continuous-variables-scatter-graph-and-alternatives/#continuous-bivariate-distribution}

\item{correlation_r2}{Add a correlation value to the plot}

\item{plot}{Display the plot.}

\item{save}{Save the plot into a file.}

\item{mdlink}{Insert a .pdf and a .png image link in the markdown report, set by "path_of_report".}

\item{grid}{Character indicating the axis to add gridlines. Options are 'x', 'y', or 'xy'. Default is 'y'.}

\item{w}{Width of the plot.}

\item{h}{Height of the plot.}

\item{...}{Pass any other parameter of the corresponding plotting function(most of them should work).}
}
\description{
Draw and save a 2D-scatter plot.
}
\examples{
dfx <- as.data.frame(cbind("AA" = rnorm(500), "BB" = rnorm(500)))
qscatter(dfx, suffix = "2D.gaussian")

}

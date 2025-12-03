# ggExpress ![status: active](https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/active.svg)
Development of the ggExpress package for quick ggpubr based plotting ala 'Type less, Plot more'.
This package is a work in progress.

![image](https://github.com/vertesy/CON/assets/5101911/fb840023-9685-433b-af58-6003edd36830)

<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

```R
# install.packages("devtools"); # If you don't have it.
require("devtools")

# Install dependencies
devtools::install_github(repo = "vertesy/Stringendo", ref = "main", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter", ref = "main", upgrade = F)
devtools::install_github(repo = "vertesy/CodeAndRoll2", ref = "main", upgrade = F)
devtools::install_github(repo = "vertesy/MarkdownHelpers", ref = "main", upgrade = F)

# Install ggExpress
devtools::install_github(repo = "vertesy/ggExpress", ref = "main", upgrade = F)
```

...then simply load the package:

```R
require("ggExpress")
```

Alternatively, you can simply source it from the web.
*This way function help will not work, and you will have no local copy of the code on your hard drive.*

```r
source("https://raw.githubusercontent.com/vertesy/ggExpress/main/R/ggExpress.functions.R")
source("https://raw.githubusercontent.com/vertesy/ggExpress/main/R/ggExpress.auxiliary.functions.R")
```

### Troubleshooting

*If you encounter a **bug**, something doesn't work or is unclear, please let me know by raising an issue on [ggExpress](https://github.com/vertesy/ggExpress/issues) – Please check if it has been asked.*

## Usage

```r
# ggExpress mini-vignette: plot, annotate & export in one line
# -----------------------------------------------------------

library(ggExpress)

# (Optional) If you use MarkdownReports, switch on automatic markdown links once:
# b.mdlink <- TRUE   # q* functions will then append image links to the active report


# 1) Numeric vector → histogram + density, auto labels, auto filenames, ggplot objects ----

weight <- rnorm(1000)

# One line:
# - guesses a plot title from the object name 'weight'
# - draws a histogram with median line
# - saves PNG
# - returns the ggplot object for further customization
weight_hist <- qhistogram(
  vec       = weight, 
  plot = T,
  vline     = 0,
  caption   = "Auto-annotated histogram"
)


# 1b) You still have a regular ggplot object, that you can further modify and simply resave:
(weight_hist <- weight_hist + ggplot2::theme_minimal() + ggplot2::labs(subtitle = "Modified with theme_minimal") )
qqSave(weight_hist) # Saves with the auto-generated filename from the variable name

# Same vector, different geometry, same automatic annotations, 
# but this time also save as PDF and the ggplot object:
qdensity(
  vec       = weight,
  subtitle  = "Same variable, density view",
  also.pdf  = TRUE,     # save both .png and .pdf
  save.obj  = TRUE      # also save ggplot object as .qs
)
```
## Output
*Saved as .png by default.* 

Histogram | Density plot
-- | -- 
<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/3aa413d8-3cda-4f37-8b91-ff5bb122c947" /> | <img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/817cba48-9afe-41f9-b82d-364d9a184978" />




```R
# 2) Named vector → barplot & pie chart with auto labels and filenames --------------------

Medals <- c(Bulgaria = 12, Brazil = 29, Burkina = 5)

# One liners annotated and saved plots with full path printed on the console
qbarplot(Medals)
qpie(Medals)


# Barplot:
# - uses names(counts) as x-labels
# - auto-generates plot title from 'counts'
# - writes files like 'counts.demo.bar.png' / '.pdf'
qbarplot(
  vec       = Medals,
  label     = Medals,           # show values on bars
  ylab      = "Count",
  suffix    = "demo",     # Add a suffix string to the filename
  palette_use = "npg",
)

# Pie chart:
# - again uses names(counts) as slice labels
# - can show both % and absolute values
qpie(
  vec              = Medals,
  both_pc_and_value = TRUE,
  LegendTitle      = "Country"
)
```
Bar plot | Pie chart
-- | -- 
<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/d8654117-3b28-4be7-9f62-93c542e931ca" /> | <img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/e04c0044-4280-472a-84f9-aec747916309" />


```R
# 3) Data frame → scatter plot with automatic x/y labels and export -----------------------
patient_measurements <- data.frame(
  height_cm = rnorm(200, mean = 170, sd = 10),
  weight_kg = rnorm(200, mean = 70, sd = 12)
)

# Minimal scatter:
# - plot title from 'patient_measurements'
# - x/y labels from 'height_cm' and 'weight_kg'
# - filename guessed from the data frame name
qscatter(patient_measurements)

# You can add contours and correlation annotation, and any other ggpubr stats:
qscatter(
  patient_measurements,
  add_contour_plot = TRUE,
  correlation_r2 = 'pearson'
)

# At this point you have:
# - a set of .png and .pdf files with auto-generated, informative filenames
# - matching ggplot objects (p_hist, p_dens, p_bar, p_pie, p_scatter) that you can further modify or embed in other figures.
# - (optionally) auto-saved ggplot objects as .qs files.
```

Simple | Customized
-- | -- 
<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/0ad242eb-6c5f-4620-b5ac-196a2a9969ed" /> | <img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/7b869d88-cec5-4470-beb3-13be4803fbe8" />



## List of Functions in ggExpress.R (22) 
Updated: 2025/12/02 20:14

- #### 1 `qhistogram()`
Quickly draw and save a histogram (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a histogram of a  distribution provided as a numeric vector. It is a wrapper around `ggpubr::gghistogram()`,  with the automation of many features. All `ggpubr` parameters can be accessed through  the `...` argument. It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 2 `qdensity()`
Quickly draw and save a density plot (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a density plot of a  distribution provided as a numeric vector. It is a wrapper around `ggpubr::ggdensity()`,  with the automation of many features. All `ggpubr` parameters can be accessed through  the `...` argument.  It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 3 `qpie()`
Quickly draw and save a pie chart (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a pie chart of a  distribution provided as a numeric table. It is a wrapper around `ggpubr::ggpie()`,  with the automation of many features. All `ggpubr` parameters can be accessed through  the `...` argument. It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 4 `qbarplot()`
Quickly draw and save a bar plot (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a bar plot of a  distribution provided as a numeric vector. It is a wrapper around `ggpubr::ggbarplot()`,  with the automation of many features. All `ggpubr` parameters can be accessed through  the `...` argument. It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 5 `qbarplot.stacked.from.wide.df()`
qbarplot.stacked.from.wide.df - Barplot for tibbles or dataframes. Draw and save a stacked barplot for each row of a dataframe.

- #### 6 `qbarplot.df()`
qbarplot.df - Barplot for tibbles or dataframes. Draw and save a barplot for tibbles or dataframes

- #### 7 `qscatter()`
Quickly draw and save a scatter plot (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a scatter plot of  two variables provided as a 2-column data frame or matrix. It is a wrapper around  `ggpubr::ggscatter()`, with the automation of many features. All `ggpubr` parameters can be  accessed through the `...` argument. It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 8 `qboxplot()`
Quickly draw and save a boxplot (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a boxplot from  a two-column data frame or a named list of values. The first column (or list names) is plotted on the X axis,  the second column (or list values) on the Y axis.  It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 9 `qviolin()`
Quickly draw and save a violin plot (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a violin plot from  a two-column data frame or a named list of values. The first column (or list names) is plotted  on the X axis, the second column (or list values) on the Y axis. It is a wrapper around  `ggpubr::ggviolin()`, with the automation of many features. All `ggpubr` parameters can be  accessed through the `...` argument.#'  It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 10 `qstripchart()`
Quickly draw and save a stripchart (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a stripchart from  a two-column data frame or a named list of values. The first column (or list names) is plotted  on the X axis, the second column (or list values) on the Y axis. It is a wrapper around  `ggpubr::ggstripchart()`, with the automation of many features. All `ggpubr` parameters can be  accessed through the `...` argument. It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 11 `qvenn()`
Quickly draw and save a Venn Diagram (png, pdf, ggobj.qs). This all-in-one function draws, annotates, displays and saves a Venn Diagram from  a named list of values. It is a wrapper around `ggVennDiagram::ggVennDiagram()`,  with the automation of many features. All `ggVennDiagram` parameters can be  accessed through the `...` argument. It can automatically save the plot as png (default) and/or  pdf files, and the ggplot object as a .qs file. 

- #### 12 `qheatmap()`
Quick Heatmap Plot. Generates a heatmap plot using the `ggheatmap` package with features such as automatic  file saving and markdown link generation. This function simplifies the process of creating and  customizing heatmaps, with support for clustering, annotations, and scaling. 

- #### 13 `qmosaic()`
Draw and Save a Doubledecker Mosaic Plot.   Creates a mosaic-style (doubledecker) stacked bar plot where the X-axis bar width is proportional  to the group size and the Y-axis variable determines the color (fill). This function wraps  `ggmosaic::geom_mosaic()` in the style of ggExpress plotting wrappers and supports automatic file  saving, Markdown linking, and default color palettes. 

- #### 14 `qqSave()`
qqSave. Quick-Save ggplot objects

- #### 15 `q32vA4_grid_plot()`
q32vA4_grid_plot. Plot up to 6 panels (3-by-2) on vertically standing A4 page.

- #### 16 `qA4_grid_plot()`
qA4_grid_plot. Plot up to 6 panels (3-by-1) on vertically standing A4 page.

- #### 17 `qMarkdownImageLink()`
qMarkdownImageLink. Insert Markdown image link to .md report

- #### 18 `qqqAxisLength()`
qqqAxisLength. Define Axis Length

- #### 19 `qqqNamed.Vec.2.Tbl()`
qqqNamed.Vec.2.Tbl. Convert a named vector to a table.

- #### 20 `qqqTbl.2.Vec()`
qqqTbl.2.Vec. Convert a table to a named vector.

- #### 21 `qqqList.2.DF.ggplot()`
qqqList.2.DF.ggplot. Convert a list to a two-column data frame to plot boxplots and violin plots

- #### 22 `.assertMaxCategories()`
Assert Maximum Categories in a data frame Column. Checks if the number of unique categories in a column of a dataframe is within the allowed limit.


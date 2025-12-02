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
  suffix    = "demo",
  caption   = "Auto-annotated histogram"
)


# 1b) You still have a regular ggplot object, that you can further modify and simply resave:
(weight_hist <- weight_hist + ggplot2::theme_minimal())
qqSave(weight_hist) # Saves with the auto-generated filename from the variable name

# Same vector, different geometry, same automatic annotations, 
# but this time also save as PDF and the ggplot object:
qdensity(
  vec       = weight,
  suffix    = "demo",
  subtitle  = "Same variable, density view",
  also.pdf  = TRUE,     # save both .png and .pdf
  save.obj  = TRUE      # also save ggplot object as .qs
)
```
## Output
*Saved as pdf by default.* 

<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/3aa413d8-3cda-4f37-8b91-ff5bb122c947" />

<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/817cba48-9afe-41f9-b82d-364d9a184978" />


```R
# 2) Named vector → barplot & pie chart with auto labels and filenames --------------------

Medals <- c(Bulgaria = 12, Brazil = 29, Burkina = 5)

# Barplot:
# - uses names(counts) as x-labels
# - auto-generates plot title from 'counts'
# - writes files like 'counts.demo.bar.png' / '.pdf'
qbarplot(
  vec       = Medals,
  label     = Medals,           # show values on bars
  ylab      = "Count",
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
<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/d8654117-3b28-4be7-9f62-93c542e931ca" />

<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/e04c0044-4280-472a-84f9-aec747916309" />


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
<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/0ad242eb-6c5f-4620-b5ac-196a2a9969ed" />

<img width="536" height="510" alt="image" src="https://github.com/user-attachments/assets/7b869d88-cec5-4470-beb3-13be4803fbe8" />



## List of Functions in ggExpress.R (19) 
Updated: 2024/10/24 15:51

- #### 1 `qhistogram()`
Quick Histogram Plotting. This function generates a histogram and saves the plot for a given vector and offers several customizations.

- #### 2 `qdensity()`
qdensity. Draw and save a density plot.

- #### 3 `qpie()`
qpie. Draw and save a pie chart

- #### 4 `qbarplot()`
qbarplot. Draw and save a barplot.

- #### 5 `qbarplot.stacked.from.wide.df()`
qbarplot.stacked.from.wide.df - Barplot for tibbles or dataframes. Draw and save a stacked barplot for each row of a dataframe.

- #### 6 `qbarplot.df()`
qbarplot.df - Barplot for tibbles or dataframes. Draw and save a barplot for tibbles or dataframes

- #### 7 `qscatter()`
qscatter. Draw and save a 2D-scatter plot. 

- #### 8 `qboxplot()`
qboxplot. Draw and save a boxplot

- #### 9 `qviolin()`
qviolin. Draw and save a violin plot

- #### 10 `qstripchart()`
qstripchart. Generates a stripchart and saves the plot for a given 2-column dataframe and offers several customizations.

- #### 11 `qvenn()`
qvenn - Venn Diagram. Draw and save a Venn Diagram using the `ggVennDiagram` package.

- #### 12 `qqSave()`
qqSave. Quick-Save ggplot objects

- #### 13 `q32vA4_grid_plot()`
q32vA4_grid_plot. Plot up to 6 panels (3-by-2) on vertically standing A4 page.

- #### 14 `qA4_grid_plot()`
qA4_grid_plot. Plot up to 6 panels (3-by-1) on vertically standing A4 page.

- #### 15 `qMarkdownImageLink()`
qMarkdownImageLink. Insert Markdown image link to .md report

- #### 16 `qqqAxisLength()`
qqqAxisLength. Define Axis Length

- #### 17 `qqqNamed.Vec.2.Tbl()`
qqqNamed.Vec.2.Tbl. Convert a named vector to a table.

- #### 18 `qqqTbl.2.Vec()`
qqqTbl.2.Vec. Convert a table to a named vector.

- #### 19 `qqqList.2.DF.ggplot()`
qqqList.2.DF.ggplot. Convert a list to a two-column data frame to plot boxplots and violin plots


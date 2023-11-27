# ggExpress ![status: active](https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/active.svg)
Development of the ggExpress package for quick ggpubr based plotting ala 'Type less, Plot more'.
This package is work in progress.

![image](https://github.com/vertesy/CON/assets/5101911/fb840023-9685-433b-af58-6003edd36830)

<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

```R
# install.packages("devtools"); # If you don't have it.
require("devtools")

# Install dependencies
devtools::install_github(repo = "vertesy/Stringendo", upgrade = F)
devtools::install_github(repo = "vertesy/ReadWriter", upgrade = F)
devtools::install_github(repo = "vertesy/CodeAndRoll2", upgrade = F)
devtools::install_github(repo = "vertesy/MarkdownHelpers", upgrade = F)

# Install MarkdownHelpers
devtools::install_github(repo = "vertesy/ggExpress")
```

...then simply load the package:

```R
require("ggExpress")
```

Alternatively, you simply source it from the web. 
*This way function help will not work, and you will have no local copy of the code on your hard drive.*

```r
source("https://raw.githubusercontent.com/vertesy/ggExpress/main/R/ggExpress.functions.R")
source("https://raw.githubusercontent.com/vertesy/ggExpress/main/R/ggExpress.auxiliary.functions.R")
```

### Troubleshooting

*If you encounter a **bug**, something doesn't work or unclear, please let me know by raising an issue on [ggExpress](https://github.com/vertesy/ggExpress/issues) – Please check if it has been asked.*

## Usage

```r
require('ggpubr')
require('cowplot')
require('Stringendo')
require('ReadWriter')
require('CodeAndRoll2')
require('MarkdownHelpers')

require('ggExpress')


# Test ------------------

weight <- rnorm(1000); 
qhistogram(weight, vline = 3)
qdensity(weight)

weight3 <- runif (12)
qbarplot(weight3)

xvec <- c("A"=12, "B"=29)
qpie(vec = xvec)


dfx <- as.data.frame(cbind("AA"=rnorm(12), "BB"=rnorm(12)))
qscatter(dfx, suffix = "2D.gaussian")

```

## Output
*Saved as pdf by default.* 

![weight.dens](README.assets/weight.dens.png)
![weight.hist](README.assets/weight.hist.png)
![weight3.bar](README.assets/weight3.bar.png)
![xvec.pie](README.assets/xvec.pie.png)
![dfx.2D.gaussian.scatter](README.assets/dfx.2D.gaussian.scatter.png)



## List of Functions (17) 

Updated: 2023/11/24 16:40

- #### 1 `qhistogram()`

  Quick Histogram Plotting. This function generates a histogram and saves the plot for a given vector and offers several customizations.

- #### 2 `qdensity()`

  qdensity. Draw and save a density plot.

- #### 3 `qbarplot()`

  qbarplot. Draw and save a barplot.

- #### 4 `qbarplot.df()`

  qbarplot.df - Barplot for tibbles or dataframes. Draw and save a barplot for tibbles or dataframes

- #### 5 `qpie()`

  qpie. Draw and save a pie chart

- #### 6 `qboxplot()`

  qboxplot. Draw and save a boxplot

- #### 7 `qviolin()`

  qviolin. Draw and save a violin plot

- #### 8 `qstripchart()`

  qstripchart. Generates a stripchart and saves the plot for a given 2-column dataframe and offers several customizations.

- #### 9 `qscatter()`

  qscatter. Draw and save a 2D-scatter plot.

- #### 10 `qvenn()`

  qvenn - Venn Diagram. Draw and save a Venn Diagram using the `ggVennDiagram` package.

- #### 11 `qqSave()`

  qqSave. Quick-Save ggplot objects

- #### 12 `q32vA4_grid_plot()`

  q32vA4_grid_plot. Plot up to 6 panels (3-by-2) on vertically standing A4 page.

- #### 13 `qA4_grid_plot()`

  qA4_grid_plot. Plot up to 6 panels (3-by-1) on vertically standing A4 page.

- #### 14 `qMarkdownImageLink()`

  qMarkdownImageLink. Insert Markdown image link to .md report

- #### 15 `qqqAxisLength()`

  qqqAxisLength. Define Axis Length

- #### 16 `qqqNamed.Vec.2.Tbl()`

  qqqNamed.Vec.2.Tbl. Covert a named vector to a table.

- #### 17 `qqqTbl.2.Vec()`

  qqqTbl.2.Vec. Covert a table to a named vector.




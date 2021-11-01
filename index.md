# Rocinante

A collection of custom R functions. Helper functions complementing the new [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2). Many functionalities were part of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll).





<br><br>

## Installation

1.) [Download `Rocinante.R`](https://github.com/vertesy/Rocinante/blob/main/R/Rocinante.R), save as local `.R` file, and `source(~/path/to/Rocinante.R)`: 

2.) Directly source from the web:

```R
source("https://raw.githubusercontent.com/vertesy/Rocinante/main/R/Rocinante.R")
```

<br><br>

## List of functions

- #### stry  

  Silent try

- #### sourcePartial   

  Source parts of another script. Source: https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file

- #### eucl.dist.pairwise  

  Calculate pairwise euclidean distance

- #### sign.dist.pairwise  

  Calculate absolute value of the pairwise euclidean distance

- #### rowACF  

  RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

- #### colACF  

  RETURNS A LIST. Calculates the autocorrelation of each row of a numeric matrix / data frame.

- #### acf.exactLag  

  Autocorrelation with exact lag

- #### rowACF.exactLag   

  RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.

- #### colACF.exactLag   

  RETURNS A Vector for the "lag" based autocorrelation. Calculates the autocorrelation of each row of a numeric matrix / data frame.

- #### clip2clip.vector  

  Copy from clipboard (e.g. excel) to a R-formatted vector to the  clipboard

- #### clip2clip.commaSepString  

  Read a comma separated string (e.g. list of gene names) and properly format it for R.

- #### write_clip.replace.dot  

  Clipboard export for da wonderful countries with where "," is the decimal

- #### view.head   

  view the head of an object by console.

- #### view.head2  

  view the head of an object by View().

- #### iidentical.names  

  Test if names of two objects for being exactly equal

- #### iidentical  

  Test if two objects for being exactly equal

- #### iidentical.all  

  Test if two objects for being exactly equal.

- #### IfExistsAndTrue   

  Internal function. Checks if a variable is defined, and its value is TRUE.

- #### memory.biggest.objects  

  https://stackoverflow.com/questions/17218404/should-i-get-a-habit-of-removing-unused-variables-in-r

- #### link_google   

  Parse google search query links to your list of gene symbols. Strings "prefix" and ""suffix" will be searched for together with each gene ("Human ID4 neurons"). See many additional services in [DatabaseLinke.R](https://vertesy.github.io/DatabaseLinke.R/).

- #### link_bing   

  Parse bing search query links to your list of gene symbols. Strings "prefix" and ""suffix" will be searched for together with each gene ("Human ID4 neurons"). See many additional services in [DatabaseLinke.R](https://vertesy.github.io/DatabaseLinke.R/).

- #### GC_content  

  GC-content of a string (frequency of G and C letters among all letters).

- #### printEveryN   

  Report at every e.g. 1000

- #### '%!in%'   


- #### stopif2   

  Stop script if the condition is met. You can parse anything (e.g. variables) in the message

- #### say   

  Use system voice to notify (after a long task is done)

- #### sayy  

  Use system voice to notify (after a long task is done)

- #### oo  

  Open current working directory.

- #### unload  

  Unload a package. Source: https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r

- #### irequire  

  Load a package. If it does not exist, try to install it from CRAN.

- #### legend.col  

  Source: https://aurelienmadouasse.wordpress.com/2012/01/13/legend-for-a-continuous-color-scale-in-r/

- #### val2col   

  This function converts a vector of values("yourdata") to a vector of color levels. One must define the number of colors. The limits of the color scale("zlim") or the break points for the color changes("breaks") can also be defined. When breaks and zlim are defined, breaks overrides zlim.

- #### richColors  

  Alias for rich.colors in gplots

- #### Color_Check   

  Display the colors encoded by the numbers / color-ID-s you pass on to this function

- #### colSums.barplot   

  Draw a barplot from ColSums of a matrix.

- #### lm_equation_formatter   

  Renders the lm() function's output into a human readable text. (e.g. for subtitles)

- #### lm_equation_formatter2  

  Renders the lm() function's output into a human readable text. (e.g. for subtitles)

- #### lm_equation_formatter3  

  Renders the lm() function's output into a human readable text. (e.g. for subtitles)

- #### hist.XbyY   

  Split a one variable by another. Calculates equal bins in splitby, and returns a list of the corresponding values in toSplit.

- #### panel.cor.pearson   

  A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".

- #### panel.cor.spearman  

  A function to display correlation values for pairs() function. Default is pearson correlation, that can be set to  "kendall" or "spearman".

- #### quantile_breaks   

  Quantile breakpoints in any data vector http://slowkow.com/notes/heatmap-tutorial/

- #### hclust.getOrder.row   

  Extract ROW order from a pheatmap object.

- #### hclust.getOrder.col   

  Extract COLUMN order from a pheatmap object.

- #### hclust.getClusterID.row   

  Extract cluster ID's for ROWS of a pheatmap object.

- #### hclust.getClusterID.col   

  Extract cluster ID's for COLUMNS of a pheatmap object.

- #### hclust.ClusterSeparatingLines.row   

  Calculate the position of ROW separating lines between clusters in a pheatmap object.

- #### hclust.ClusterSeparatingLines.col   

  Calculate the position of COLUMN separating lines between clusters in a pheatmap object.

- #### Gap.Postions.calc.pheatmap  

  calculate gap positions for pheatmap, based a sorted annotation vector of categories

- #### matlabColors.pheatmap   

  Create a Matlab-like color gradient using "colorRamps".

- #### annot_col.create.pheatmap.vec   

  For VECTORS. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap

- #### annot_col.create.pheatmap.df  

  For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap

- #### annot_col.fix.numeric   

  fix class and color annotation in pheatmap annotation data frame's and lists.

- #### annot_row.create.pheatmap.df  

  For data frames. Auxiliary function for pheatmap. Prepares the 2 variables needed for "annotation_col" and "annotation_colors" in pheatmap

- #### sourceGitHub  


- #### # RemoveFinalSlash  

- #### ww.set.OutDir   

- #### backup  

  make a backup of an object into global env. Scheme: obj > obj.bac

- #### list.dirs.depth.n   

  list dirs recursive up to a certain level in R https://stackoverflow.com/questions/48297440/list-files-recursive-up-to-a-certain-level-in-r

- #### qHGNC   

  Parse HGNC links to your list of gene symbols.

- #### NrAndPc   

  Summary stat. text formatting for logical vectors (%, length)

- #### jjpegA4   

  Setup an A4 size jpeg

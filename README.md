# ggExpressDev
Development of ggExpress package for quick ggpubr based plotting

<img width="706" alt="image" src="https://user-images.githubusercontent.com/5101911/99193282-4c06b980-2778-11eb-8c74-37293a8a245c.png">





# Current functions

- #### `qhistogram()`

- #### `qdensity()`

- #### `qbarplot()`

- #### `qpie()`

- #### `qscatter()`




## Usage


```r
require(ggpubr)
require(cowplot)
require(MarkdownReports) # https://github.com/vertesy/MarkdownReportsDev

source("https://raw.githubusercontent.com/vertesy/ggExpressDev/main/ggExpress.functions.R")


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



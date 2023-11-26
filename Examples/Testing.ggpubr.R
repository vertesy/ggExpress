######################################################################
# Test.ggpubr.R
######################################################################
# source('~/GitHub/Packages/Seurat.pipeline/elements/Test.ggpubr.R')
# try(dev.off(), silent = T)
require(MarkdownReportsDev)
require(ggpubr)
require(cowplot)
# Parameters ------------------------

"gggbarplot"
"add grid"

# Setup ------------------------
set.seed(1989)
wdata = data.frame(
  sex = factor(rep(c("F", "M", "X"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58), rnorm(200, 56)))
head(wdata, 4)

# Metadata ------------------------
# ------------------------
ggdensity(wdata, x = "weight",
          add = "mean", rug = TRUE,
          # combine = T,
          color = "sex", fill = "sex")


# ------------------------
p <- gghistogram(wdata, x = "weight",
                 add = "median",
                 color = "sex", fill = "sex",
                 palette = 'jco'
                 # , alpha = 1
)
p
qqSave(p, w = 4)
require(cowplot)
# ------------------------
try.dev.off()
weight <- wdata[,2]
x<- ggdensity(weight,  fill =  c("#00AFBB") )
ggbarplot(t(weight),  fill =  c("#00AFBB") )+ grids(axis ='y')

# Load data
data("ToothGrowth")





# ------------------------
# Load data
data("ToothGrowth")
df <- ToothGrowth
head(df, 4)

#>    len supp dose
#> 1  4.2   VC  0.5
#> 2 11.5   VC  0.5
#> 3  7.3   VC  0.5
#> 4  5.8   VC  0.5

# Box plots with jittered points
# :::::::::::::::::::::::::::::::::::::::::::::::::::
# Change outline colors by groups: dose
# Use custom color palette
# Add jitter points and change the shape by groups
p <- ggboxplot(df, x = "dose", y = "len",
               color = "dose", palette ="jco",
               add = "jitter", shape = "dose")
p


# ------------------------

# Add p-values comparing groups
# Specify the comparisons you want
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)                   # Add global p-value



p <- ggviolin(df, x = "dose", y = "len", fill = "dose",
              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
              add = "boxplot", add.params = list(fill = "white"))
p +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif")+ # Add significance levels
  stat_compare_means(label.y = 50)


# ------------------------------------------------------------------------
# Load data
data("mtcars")
dfm <- mtcars
# Convert the cyl variable to a factor
dfm$cyl <- as.factor(dfm$cyl)
# Add the name colums
dfm$name <- rownames(dfm)
# Inspect the data
head(dfm[, c("name", "wt", "mpg", "cyl")])

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
p<- ggbarplot(dfm, x = "name", y = "mpg",
              fill = "cyl",               # change fill color by cyl
              color = "white",            # Set bar border colors to white
              palette = "jco",            # jco journal color palett. see ?ggpar
              sort.val = "desc",          # Sort the value in dscending order
              sort.by.groups = FALSE,     # Don't sort inside each group
              x.text.angle = 90           # Rotate vertically x axis texts
)+ grids(axis ='y')
p
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# Data: Create some data
# +++++++++++++++++++++++++++++++

df <- data.frame(
  group = c("Male", "Female", "Child"),
  value = c(25, 25, 50))

head(df)
#>    group value
#> 1   Male    25
#> 2 Female    25
#> 3  Child    50


# Basic pie charts
# ++++++++++++++++++++++++++++++++
ggpubr::ggpie(df, "value", label = "group")



# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
b.defpalette
gggpie <- function(df_or_nvec) {
  ggpubr::ggpie(df, "value", label = "group",
                fill = "group", color = "white",
                palette = c("#00AFBB", "#E7B800", "#FC4E07") )

}
# ------------------------------------------------------------------------

# Load data
data("mtcars")
df <- mtcars
df$cyl <- as.factor(df$cyl)
head(df[, c("wt", "mpg", "cyl")], 3)
#>                  wt  mpg cyl
#> Mazda RX4     2.620 21.0   6
#> Mazda RX4 Wag 2.875 21.0   6
#> Datsun 710    2.320 22.8   4

# Basic plot
# +++++++++++++++++++++++++++
ggscatter(df, x = "wt", y = "mpg", color = "cyl",
          palette = c("#00AFBB", "#E7B800", "#FC4E07")
          , add = "reg.line"
          # , add = "loess", conf.int = F
) + geom_density_2d()



# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------


# End ------------------------------------------------------------------------



x <- 1:10; y = x*x
# Basic plot
p <- qplot(x,y)
p

# Violin plot
qplot(group, weight, data = PlantGrowth,
      geom=c("violin"), trim = FALSE)



set.seed(1234)
mydata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
qplot(weight, data = mydata, geom = "histogram",
      fill = sex)

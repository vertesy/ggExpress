######################################################################################################
# Create_the_ggExpress_Package.R
######################################################################################################
# source("~/GitHub/Packages/ggExpress/Development/Create_the_ggExpress_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)

# Functions ------------------------
# install_version("devtools", version = "2.0.2", repos = "http://cran.at.r-project.org") # install.packages("devtools")
# require("devtools")
# require("roxygen2")
# require("stringr")

# # devtools::install_github(repo = "vertesy/CodeAndRoll2")
# require('CodeAndRoll2')
# require('Stringendo')

# Setup ------------------------
package.name <- "ggExpress"
package.version <- "0.7.4"
setwd("~/GitHub/Packages/")

RepositoryDir <- paste0("~/GitHub/Packages/", package.name, "/")
fname <-	paste0(package.name, ".R")
Package_FnP <-	paste0(RepositoryDir, "R/", fname)

BackupDir <- "~/GitHub/Packages/ggExpress/Development/"
dir.create(BackupDir)


# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "ggExpress is the fastest way to create, annotate and export plots in R"
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "a.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "ggExpress is a set of R functions that allows you to generate precise figures easily,
    and create clean markdown reports about what you just discovered with your analysis script. It is ggplot-based smaller brother of vertesy/MarkdownReports. It helps you to:
    1. Create scientifically accurate (annotated) figures with very short code, making use of variable-, row- and columnnames.
    2. Save figures automatically as vector graphic (.pdf), that you can use from presentation to posters anywhere.
    3. Incorporate your figures automatically in a markdown report file.
    4. Describe your figures & findings in the same report in a clear and nicely formatted way, parsed from your variables into english sentences.
    5. Share your report, by exporting your report to .pdf, .html or .docx, or via Github or a personal website."
    , "License" = "GPL-3 + file LICENSE"
    , "Version" = package.version
    , "Packaged" =  Sys.time()
    # , "Repository" =  "CRAN"
    , "Depends" =  "Stringendo, MarkdownHelpers, ggplot2, ggpubr"
    , "Imports" = "cowplot, graphics, grDevices, MarkdownReports, methods, RColorBrewer, sessioninfo, Seurat, sm, stats, tidyverse"
    # , "Suggests" = ""
    , "BugReports"= "https://github.com/vertesy/ggExpress/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
    getwd()
    try(file.remove(c("DESCRIPTION","NAMESPACE", "ggExpress.Rproj")))
    usethis::create_package(path = RepositoryDir, fields = DESCRIPTION, open = F)
}


# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile <-	(paste0(BackupDir, "Development", ".bac"))
AnnotatedFile <-	(paste0(BackupDir, "Development", ".annot.R"))
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(Package_FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
devtools::document()
warnings()

{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T
                  , "^version: v.+", paste0("version: v", package.version))
}

# Install your package ------------------------------------------------
# setwd(RepositoryDir)
devtools::install(RepositoryDir, upgrade = F)

# require("ggExpress")
# # remove.packages("ggExpress")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()

# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/ggExpress")

# require("ggExpress")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("ggExpress")

check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
#
#
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("ggExpress", "foo")
# # desc$get(ggExpress)
#
#
# system("cd ~/GitHub/ggExpress/; ls -a; open .Rbuildignore")
#
# Check package dependencies ------------------------------------------------
depFile = paste0(RepositoryDir, 'Development/Dependencies.R')

(f.deps <- NCmisc::list.functions.in.file(filename = Package_FnP))
# clipr::write_clip(f.deps)

sink(file = depFile); print(f.deps); sink()
p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
write(x = p.deps, file = depFile, append = T)
p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
p.dep.new <- sort(union( p.deps, p.dep.declared))
# clipr::write_clip(p.dep.new)



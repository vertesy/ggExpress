######################################################################################################
# Create_the_ggExpress_Package.R
######################################################################################################
# source("~/GitHub/Packages/ggExpress/Development/Create_the_ggExpress_Package.R")
# rm(list = ls(all.names = TRUE)); try(dev.off(), silent = TRUE)

# Functions ------------------------
require(PackageTools)

# Setup ------------------------
RepositoryDir <- "~/GitHub/Packages/ggExpress/"

"TAKE A LOOK AT"
"~/GitHub/Packages/ggExpress/Development/config.R"

PackageTools::document_and_create_package(RepositoryDir, config_file = 'config.R')
'git add commit push to remote'

# Install your package ------------------------------------------------
"disable rprofile by"
rprofile()

devtools::install_local(RepositoryDir, upgrade = F)
# devtools::

# Test if you can install from github ------------------------------------------------
pak::pkg_install("vertesy/PackageTools")
# unload(PackageTools)
# require("PackageTools")
# # remove.packages("PackageTools")

# CMD CHECK ------------------------------------------------
checkres <- devtools::check(RepositoryDir, cran = FALSE)



# Automated Codebase linting to tidyverse style ------------------------------------------------
styler::style_pkg(RepositoryDir)


# Extract package dependencies ------------------------------------------------
PackageTools::extract_package_dependencies(RepositoryDir)


# Visualize function dependencies within the package------------------------------------------------
{
  warning("works only on the installed version of the package!")
  pkgnet_result <- pkgnet::CreatePackageReport(package.name)
  fun_graph     <- pkgnet_result$FunctionReporter$pkg_graph$'igraph'

  PackageTools::convert_igraph_to_mermaid(graph = fun_graph, openMermaid = T, copy_to_clipboard = T)
}


# Try to find and add missing @importFrom statements------------------------------------------------
if (F) {
  # Add @importFrom statements
  (FNP <- package.FnP)
  (FNP <-  "~/GitHub/Packages/PackageTools/R/DependencyTools.R")
  PackageTools::add_importFrom_statements(FNP, exclude_packages = "")
}






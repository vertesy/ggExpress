% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggExpress.R
\name{.assertMaxCategories}
\alias{.assertMaxCategories}
\title{Assert Maximum Categories in a DataFrame Column}
\usage{
.assertMaxCategories(df, col, max.categ)
}
\arguments{
\item{df}{A data frame containing the column to be checked.}

\item{col}{The column name or index in the data frame.}

\item{max.categ}{The maximum allowed number of unique categories.}
}
\value{
Stops the function execution if the number of unique categories exceeds max.categ.
}
\description{
Checks if the number of unique categories in a column of a dataframe is within the allowed limit.
}
\examples{
assertMaxCategories(df_XYcol, "x", 10)
}

######################################################################
# ggExpress is the fastest way to create, annotate and export plots in R.
######################################################################
# try(source("~/GitHub/Packages/ggExpressDev/R/ggExpress.auxiliary.functions.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpressDev/main/ggExpress.functions.R"), silent = T)



######################################################################
# Explicit dependencies on MarkdownReports
######################################################################

# - llogit

######################################################################
# Explicit dependencies on CodeAndRoll
######################################################################

# - flag.nameiftrue

# flag.nameiftrue <- function(toggle, prefix = NULL, suffix = NULL, name.if.not = "") { # Returns the name and its value, if its TRUE.
#   output = if (toggle) { paste0(prefix, (substitute(toggle)), suffix)
#   } else {paste0(prefix, name.if.not, suffix)}
#   if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
#   return(output)
# } # returns the name if its value is true

######################################################################
# Duplicated functions to avoid dependencies
######################################################################



# MarkdownReports.R ------------------------------------------------------------------------------------------------
#' Collapse vector to a string, separated by dots
#'
#' @param ... Pass any other parameter of the corresponding plotting function(most of them should  work).
#' @export
#'
#' @examples kpp(1,3, 'sdasd'); kpp(1:3, 'sdasd')

kpp <- function(...) { stringr::str_remove(paste(..., sep = '.', collapse = '.'), "\\.+$") } # remove trailing dots


# CodeAndRoll.R ------------------------------------------------------------------------------------------------

#' flag.nameiftrue
#' Aux function for parsing filenames. Returns a variable's name if its value is TRUE.
#' @param toggle Variable name to return as string, if its value is TRUE.
#' @param prefix prefix
#' @param suffix suffix
#' @param name.if.not Alternative name to return, "" by default.
#'
#' @return
#' @export
#'
#' @examples a=TRUE; b=FALSE; flag.nameiftrue(a); flag.nameiftrue(b)

flag.nameiftrue <- function(toggle, prefix = NULL, suffix = NULL, name.if.not = "") { # Returns the name and its value, if its TRUE.
  output = if (toggle) { paste0(prefix, (substitute(toggle)), suffix)
  } else {paste0(prefix, name.if.not, suffix)}
  if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
  return(output)
} # returns the name if its value is true

######################################################################
# Original functions
######################################################################

# ------------------------------------------------------------------------------------------------
#' Quick-Save ggplot objects
#'
#' @param ggobj ggobj
#' @param w width of the plot.
#' @param h height of the plot.
#' @param ext File extension (.pdf / .png).
#' @param also.pdf also.pdf
#' @param page page
#' @param title title
#' @param fname fname
#' @param suffix A suffix added to the filename. NULL by default.
#' @param ... Pass any other parameter of the corresponding plotting function (most of them should  work).
#' @export
#'
#' @examples xplot <- ggplot2::qplot(12); qqSave(ggobj = xplot); qqSave(ggobj = xplot, ext = "pdf")

qqSave <- function(ggobj, w =4, h = w
                   , ext =c("png", "pdf")[1], also.pdf = FALSE
                   , page = c(F, "A4p", "A4l", "A5p", "A5l")[1]
                   , title = F, fname = F, suffix = NULL, ...) {
  if (isFALSE(title)) title <- as.character(substitute(ggobj))
  if (also.pdf) fname2 <- if (isFALSE(fname)) kpp(title, suffix, 'pdf') else kpp(fname, 'pdf')
  if (isFALSE(fname)) fname <- kpp(title, suffix, ext)

  if (!isFALSE(page)) {
    wA4 <- 8.27
    hA4 <- 11.69
    if ( page == "A4p" ) { w = wA4; h = hA4 }
    if ( page == "A4l" ) { w = hA4; h = wA4 }
    if ( page == "A5p" ) { w = wA4/2; h = hA4/2 }
    if ( page == "A5l" ) { w = hA4/2; h = wA4/2 }
  }
  print(paste0(getwd(),"/", fname))

  if (also.pdf) cowplot::save_plot(plot = ggobj, filename = fname2, base_width = w, base_height = h, ...)
  cowplot::save_plot(plot = ggobj, filename = fname, base_width = w, base_height = h, ...)
}



# ------------------------------------------------------------------------------------------------
#' Insert Markdown image link to .md report
#'
#' @param file_name file_name
#' @export
#'
#' @examples qMarkdownImageLink(file_name = "myplot.pdf")

qMarkdownImageLink <- function(file_name = 'myplot.pdf') {
  if (require(MarkdownReports)) MarkdownReports::llogit(paste0("![", file_name, "]", "(", file_name, ")", collapse = ''))
}




# ------------------------------------------------------------------------------------------------
#' Define Axis Length
#'
#' @param vec The variable to plot.
#' @param minLength minLength
#' @export
#'
#' @examples qqqAxisLength()

qqqAxisLength <- function(vec = 1:20, minLength=6) {
  max(round(length(vec)*0.2), minLength)
}


# ------------------------------------------------------------------------------------------------
#' qqqCovert.named.vec2tbl
#' Covert a named vector to a table.
#' @param namedVec namedVec
#' @param verbose verbose
#' @param strip.too.many.names strip.too.many.names
#' @param thr thr
#' @export
#'
#' @examples qqqCovert.named.vec2tbl(namedVec = c("A"=2, "B"=29) )

qqqCovert.named.vec2tbl <- function(namedVec=1:14, verbose = F, strip.too.many.names = TRUE, thr = 50) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.

  # Check naming issues
  nr.uniq.names <- length(unique(names(namedVec)))
  if (nr.uniq.names > thr & verbose)  print("Vector has", thr, "+ names. Can mess up auto-color legends.")
  if (nr.uniq.names < 1 & verbose) print("Vector has no names")
  an.issue.w.names <- (nr.uniq.names > thr | nr.uniq.names < 1 )
  if (strip.too.many.names & an.issue.w.names) names(namedVec) <- rep("x", length(namedVec))
  if (length(unique(names(namedVec))) > thr) print("Vector has", thr, "+ names. Can mess up auto-color legends.")

  df <- tibble::as_tibble(cbind("value" = namedVec))
  nm <- names(namedVec)
  df$"names" <- if (!is.null(nm)) nm else rep(".", length(namedVec))
  df
}




# ------------------------------------------------------------------------------------------------
#' qqqCovert.tbl2vec
#' Covert a table to a named vector.
#' @param tibble.input tibble.input
#' @param name.column name.column
#' @param value.column value.column
#' @export
#'
#' @examples a=1:5; x= tibble::tibble(a, a * 2); qqqCovert.tbl2vec(x)

qqqCovert.tbl2vec <- function(tibble.input, name.column = 1, value.column = 2) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
  vec <- tibble.input[[value.column]]
  names(vec) <- tibble.input[[name.column]]
  vec
}

# ------------------------------------------------------------------------------------------------
#' qqqParsePlotname
#' Parse Plotname from variable name.
#' @param string string
#' @param suffix_tag suffix_tag
#' @export
#'
#' @examples qqqParsePlotname()

qqqParsePlotname <- function(string = "sadsad", suffix_tag= NULL) { # parse plot name from variable name and suffix
  nm <- make.names(as.character(substitute(string)))
  if (!is.null(suffix_tag) & !isFALSE(suffix_tag)) nm <- kpp(nm, suffix_tag)
  return(nm)
}

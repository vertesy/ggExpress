######################################################################
# ggExpress is the fastest way to create, annotate and export plots in R.
######################################################################
# try(source("~/GitHub/Packages/ggExpressDev/ggExpress.auxiliary.functions.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/ggExpressDev/main/ggExpress.functions.R"), silent = T)



######################################################################
# Explicit dependencies on MarkdownReportsDev
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


# MarkdownReportsDev.R

#' unless.specified
#'
#' Return value X (TRUE by default) unless the variable is defined.
#' If defined, it returns the variable.
#' @param NameOfaVariable Name of a possibly defined variable to be tested.
#' @param def Default return value
#' @export
#' @examples unless.specified("xsadasf32", 2); Num = 22; unless.specified("Num", 1); unless.specified("c", 333)

unless.specified <- function(NameOfaVariable, def = TRUE) {
  if (exists(NameOfaVariable))
    get(NameOfaVariable)
  else
    def
}


# MarkdownReportsDev.R ------------------------------------------------------------------------------------------------
#' ww.ttl_field
#'
#' Internal function. Creates the string written into the PDF files "Title' (metadata) field.
#' @param flname Name of the plot
#' @param creator String X in: "plotblabla by X". Default: "ggExpress".
#' @export
#' @examples ww.ttl_field("/Users/myplot.jpg")

ww.ttl_field <- function(plotname, creator = "ggExpress") {
  paste0(basename(plotname), " by "
         , unless.specified("b.scriptname", def = creator) )
}


# MarkdownReportsDev.R ------------------------------------------------------------------------------------------------
#' percentage_formatter
#'
#' Parse a string of 0-100% from a number between 0 and 1.
#' @param x A vector of numbers between 0-1.
#' @param digitz Number of digits to keep. 3 by default.
#' @export
#' @examples percentage_formatter(x = 4.2822212, digitz = 3)

percentage_formatter <- function(x, digitz = 3) {
  a = paste(100 * signif(x, digitz), "%", sep = " ")
  a[a == "NaN %"] = NaN
  a[a == "NA %"] = NA
  return(a)
}

# MarkdownReportsDev.R ------------------------------------------------------------------------------------------------
kpp <- function(...) { stringr::str_remove(paste(..., sep = '.', collapse = '.'), "\\.+$") } # remove trailing dots

######################################################################
# Original functions
######################################################################

# ------------------------------------------------------------------------------------------------
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
# xplot <- qplot(12)
# qqSave(ggobj = xplot)
# qqSave(ggobj = xplot, ext = "pdf")


# ------------------------------------------------------------------------------------------------
qMarkdownImageLink <- function(file_name = fname) {
  if (require(MarkdownReports)) llogit(kollapse("![", file_name, "]", "(", file_name, ")", print = FALSE))
}



# ------------------------------------------------------------------------------------------------
qqqAxisLength <- function(vec = 1:20, minLength=6) {
  max(round(length(vec)*0.2), minLength)
}


# ------------------------------------------------------------------------------------------------
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
# qqqCovert.named.vec2tbl(namedVec = c("A"=2, "B"=29) )




# ------------------------------------------------------------------------------------------------
qqqCovert.tbl2vec <- function(tibble.input = pld.rate.HQ.UVI, name.column = 1, value.column = 2) { # Convert a named vector to a 2 column tibble (data frame) with 2 columns: value, name.
  vec <- tibble.input[[value.column]]
  names(vec) <- tibble.input[[name.column]]
  vec
}

# ------------------------------------------------------------------------------------------------
qqqParsePlotname <- function(string = "sadsad", suffix= NULL) { # parse plot name from variable name and suffix
  nm <- make.names(as.character(substitute(string)))
  if (!is.null(suffix) & !isFALSE(suffix)) nm <- kpp(nm, suffix)
  return(nm)
}
# qqqParsePlotname()


# ------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------



#' bib2academic: Converting Bibtex Records to .md Files
#'   for the Hugo Academic Theme
#'
#' \code{bib2academic} takes a .bib file and generates for each
#' record a .md file. The name of the .md file is generated from
#' the year and entry field. It has the format \code{year-month-day_key.md}
#'
#'
#' @section Required parameters:
#' The bib2acad functions needs one parameters.
#'
#' \strong{bibfile}: A string to the path of the .bib file ("path/to/bibfile")
#'
#' @section Optional parameters:
#' The bib2acad functions has three optional parameters
#'
#' \strong{copybib}: TRUE or FALSE, default = TRUE
#'    Should .bib files generated?
#'
#' \strong{abstract}: TRUE or FALSE, default = TRUE
#'    Should abstracts also added to the .md files?
#'
#' \strong{overwrite}: TRUE or FALSE, default = FALSE
#'    Should .md files with the same name overwritten? If .bib files are also
#'    generated, then this parameter applies to .bib files as well.
#'
#' @section Usage:
#' Put your .bib file on the top level of your project directory and call
#' `bib2acad("<your.bib.file.with.file.extension>")`
#'
#' This generates two folders `my-md-folder` and `my-bib-folder`
#' where the two types of generated files will be put.
#'
#' @docType package
#' @name bib2academic-package
NULL
#' Convert .bib Files to .md Files for the Hugo Academic Theme
#'
#' \code{bib2acad} takes a .bib file and covert it to a .md file for the
#' publication folder of the Hugo Academic theme
#' (\url{https://sourcethemes.com/academic/})
#'
#' @param bibfile A string in the format "path/to/bibfile": required
#' @param outfold A string in the format "path/to/outfold": required,
#'   the folder must exist
#' @param bibfold A string "path/to/bibfold": optional,
#'   if used: the folder must exist
#' @param copybib   Logical, optional
#' @param abstract  Logical, optional
#' @param overwrite Logical, optional
#'
#' @return NULL
#'
# Peter Baumgartner, July, 2018
# License: href="http://creativecommons.org/licenses/by/4.0/"
# General structure and idea taken from Lorenzo Busetto
# https://lbusettspatialr.blogspot.com/2018/03/automatically-importing-publications.html
# my changes:
# - changed instead of `document_type` which results in an error, I used `bibtype`:
#       the columname RefMangeR creates
# - instead of the generation of the filename from date + 20 char. from titel I used:
#       date + bibtex key
# - added booktitle as legitime, so the program not only works with œarticle but also
#       with @book, @booklet, @conference, @incollection, @inproceedings
#       but not with @inbook (replace @inbook with @incollection and delete field chapter)
# - I added an extra utility for gnerating bibTex files for every entry
#       with the same generated name (This can be used in the folder /static/files/citation.
#       This generates automatically the button `cite` which open the bibTex code
#       for copying or downloading.
# - I added an utility for cleaned up inline text
# - I added a progressbar: important with many bib records
#
# Source this file and call the conversion utility with:
#
# bib2acad(bibfile   = "path/to/my_bibfile",
#          outfold   = "path/to/my_md_folder",
#          bibfold   = "path/to/my_bib_folder", # optional: default = ""
#          copybib   = TRUE, # optional: default = FALSE
#          abstract  = TRUE, # optional: default = FALSE
#          overwrite = TRUE) # optional: default = FALSE
#
# if copybib is TRUE then bibfold has to have the path to a folder.
# all folders have to be existent (created), before the program runs.



################################################################################
bib2acad <- function(bibfile = "",
                     outfold = "",
                     bibfold = "",
                     copybib = FALSE,
                     abstract = TRUE,
                     overwrite = FALSE) {

    require(RefManageR)
    require(dplyr)
    require(stringr)
    require(anytime)
    require(pbapply)


    # Import the bibtex file and convert to data.frame
    mypubs   <- ReadBib(bibfile, check = "warn", .Encoding = "UTF-8") %>%
        as.data.frame()
    mypubs$key <- rownames(mypubs)


    # assign "categories" to the different types of publications
    mypubs   <- mypubs %>%
        dplyr::mutate(
            pubtype = dplyr::case_when(bibtype == "Article" ~ "2",
                                       bibtype == "Article in Press" ~ "2",
                                       bibtype == "InProceedings" ~ "1",
                                       bibtype == "Proceedings" ~ "1",
                                       bibtype == "Conference" ~ "1",
                                       bibtype == "Conference Paper" ~ "1",
                                       bibtype == "MastersThesis" ~ "3",
                                       bibtype == "PhdThesis" ~ "3",
                                       bibtype == "Manual" ~ "4",
                                       bibtype == "TechReport" ~ "4",
                                       bibtype == "Book" ~ "5",
                                       bibtype == "InCollection" ~ "6",
                                       bibtype == "InBook" ~ "6",
                                       bibtype == "Misc" ~ "0",
                                       TRUE ~ "0"))
    # create a function which populates the md template based on the info
    # about a publication

    create_md <- function(x) {

        # define a date and create filename_md by appending date and bibTex key
        if (!is.na(x[["year"]])) {
            x[["date"]] <- paste0(x[["year"]], "-01-01")
        } else {
            x[["date"]] <- "2999-01-01"
        }

        filename_md <- paste0(x[["date"]], "_", x[["key"]], ".md")

        # start writing
        if (!file.exists(file.path(outfold, filename_md)) | overwrite) {
            fileConn <- file.path(outfold, filename_md)
            write("+++", fileConn)

            # Title and date
            # title has sometimes with older bibTex files special characters "{}"
            # escape " and \ (e.g. the "ampersand"\&) with funtion escapeStr


            write(paste0("title = \"", cleanStr(x[["title"]]), "\""), fileConn, append = T)
            # write(paste0("title = \"",
            #              gsub("[{}]", '', x[["title"]]), "\""), fileConn, append = T)
            write(paste0("date = \"", anydate(x[["date"]]), "\""), fileConn, append = T)

            # Publication type. Legend:
            # 0 = Uncategorized, 1 = Conference paper, 2 = Journal article
            # 3 = Manuscript, 4 = Report, 5 = Book,  6 = Book section
            write(paste0("publication_types = [\"", x[["pubtype"]],"\"]"),
                  fileConn, append = T)


            if (!is.na(x[["author"]])) {
                # Authors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
                authors <- str_replace_all(str_squish(x["author"]), " and ", "\", \"")
                authors <- stringi::stri_trans_general(authors, "latin-ascii")
                write(paste0("authors = [\"", authors,"\"]"), fileConn, append = T)
            } else {
                # Editors. Comma separated list, e.g. `["Bob Smith", "David Jones"]`.
                editors <- str_replace_all(str_squish(x["editor"]), " and ", "\", \"")
                editors <- stringi::stri_trans_general(editors, "latin-ascii")
                write(paste0("editors = [\"", editors,"\"]"), fileConn, append = T)
            }


            # Publication details:
            # start with title: every bib entry has a title field!
            # then check if field booktitle or journal
            # if booktitle, then: booktitle, year and maybe pages
            # if journal, then: journal, check if volume, number, pages
            # check first if field is available and then if it is.na
            # only if both conditions are satiesfied: write field content
            # NOTE: Keep in mind that this will not generate a complete citation record
            # NOTE: Only summary information providing links to detailed infos
            # NOTE: One of these infos is to see & copy the complete bib record


            publication <- NULL # variable to collect data and to write it to file

            if ("booktitle" %in% names(x) && !is.na(x[["booktitle"]])) {
                publication <- paste0(publication, "In: ", cleanStr(x[["booktitle"]]))
                if ("publisher" %in% names(x) && !is.na(x[["publisher"]])) {
                    publication <- paste0(publication, ", ", cleanStr(x[["publisher"]]))
                }
                if ("address" %in% names(x) && !is.na(x[["address"]])) {
                    publication <- paste0(publication, ", ", cleanStr(x[["address"]]))
                }
                if ("pages" %in% names(x) && !is.na(x[["pages"]])) {
                    publication <- paste0(publication, ", _pp. ", cleanStr(x[["pages"]]), "_")
                }
            }

            if ("journal" %in% names(x) && !is.na(x[["journal"]])) {
                publication <- paste0(publication, "In: ", cleanStr(x[["journal"]]))
                if ("volume" %in% names(x) && !is.na(x[["volume"]])) {
                    publication <- paste0(publication, ", (", cleanStr(x[["volume"]]), ")")
                }
                if ("number" %in% names(x) && !is.na(x[["number"]])) {
                    publication <- paste0(publication, ", ", cleanStr(x[["number"]]))
                }
                if ("pages" %in% names(x) && !is.na(x[["pages"]])) {
                    publication <- paste0(publication, ", _pp. ", cleanStr(x[["pages"]]), "_")
                }
                if ("doi" %in% names(x) && !is.na(x[["doi"]])) {
                    publication <- paste0(publication, ", ", paste0("https://doi.org/",
                                                                    cleanStr(x[["doi"]])))
                }
                if ("url" %in% names(x) && !is.na(x[["url"]])) {
                    publication <- paste0(publication, ", ",  cleanStr(x[["url"]]))
                }


            }

            write(paste0("publication = \"", publication, "\""), fileConn, append = T)

            # Abstract and optional shortened version.
            if ( (abstract) && "abstract" %in% names(x) && !is.na(x[["abstract"]]))
            {
                write(paste0("abstract = \"", cleanStr(x[["abstract"]]), "\""), fileConn, append = T)
            } else {
                write("abstract = \"\"", fileConn, append = T)
            }
            write(paste0("abstract_short = \"","\""), fileConn, append = T)


            # other possible fields are kept empty. They can be customized later by
            # editing the created md

            write("image_preview = \"\"", fileConn, append = T)
            write("selected = false", fileConn, append = T)
            write("projects = []", fileConn, append = T)
            write("tags = []", fileConn, append = T)
            #links
            write("url_pdf = \"\"", fileConn, append = T)
            write("url_preprint = \"\"", fileConn, append = T)
            write("url_code = \"\"", fileConn, append = T)
            write("url_dataset = \"\"", fileConn, append = T)
            write("url_project = \"\"", fileConn, append = T)
            write("url_slides = \"\"", fileConn, append = T)
            write("url_video = \"\"", fileConn, append = T)
            write("url_poster = \"\"", fileConn, append = T)
            write("url_source = \"\"", fileConn, append = T)
            #other stuff
            write("math = true", fileConn, append = T)
            write("highlight = true", fileConn, append = T)
            # Featured image
            write("[header]", fileConn, append = T)
            write("image = \"\"", fileConn, append = T)
            write("caption = \"\"", fileConn, append = T)

            write("+++", fileConn, append = T)



        }
        # write bibTex Data into separate file
        # these bib files can be stored under "static/files/citations"
        # then a button "cite" is generated automatically in the academic framework
        if (copybib) {
            filename_bib <- (gsub(".md", ".bib", filename_md))
            y <- as.list(x)
            y["pubtype"] <- NULL
            y <- as.BibEntry(y)
            if (!file.exists(file.path(bibfold, filename_bib)) | overwrite) {
                RefManageR::WriteBib(y,
                        file = file.path(bibfold, filename_bib), verbose = FALSE)
            }
        }
    }

    # apply the "create_md" function over the publications list to generate
    # the different "md" files.

    pb <- startpb(min = 0, max = nrow(mypubs))
    pbapply(mypubs, FUN = function(x) create_md(x), MARGIN = 1)
    closepb(pb)

}

cleanStr <- function(str) {
    # if special character has in front a "\": replace it with "\\\\"
    str <- gsub('\\', '\\\\', str, fixed = TRUE)
    # delete all "{" and "}" in old bibtex files
    str <- gsub("[{}]", '', str)
    # replace all inline quotes '"' with "four '\\\\"'
    str <- gsub('"', '\\\\"', str)
    # delete extra lines, tabs and spaces; especially important with field 'abstract'
    # and return the cleaned string
    return(stringr::str_squish(str))
}



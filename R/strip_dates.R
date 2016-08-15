#' Creates a vector of publication dates given a folder of files from the Modern Journals Project
#' 
#' @param xmlfolder input folder of files to parse dates from
#' @param pattern file endings of the files. Defaults to ".xml"
#' @return POSIXCT vector of date published for each of the files
#' @export

# this function takes the XML files associated with the Egoist and creates a vector of dates
# that correspond to those files
# all you need to supply is a folder with these files. They can be found here:
# http://sourceforge.net/projects/mjplab/files/

strip_dates <- function(xmlfolder, pattern = ".xml"){
  library(lubridate); library(stringr);
  journal.paths <- list.files(path=xmlfolder, pattern = pattern, full.names = TRUE)
  months <- c(month.name, month.abb)
  months <- c(months, toupper(months), tolower(months))
  monthsexp <- paste(months, collapse = "|")
  dateConverter_XML <- lapply(journal.paths, function(i){
    j <- paste0(scan(i, what = character(), comment.char='', quote=NULL),  collapse = " ")
    j <- substr(j, 1, 3000)
    journal <- journal.paths[i]
    date <- vector()
    if (!is.na(str_extract(j, paste0("(",monthsexp, ')[:punct:]{0,2} \\d{4}')))){
      k <- str_extract(j, paste0("(",monthsexp, ')[:punct:]{0,2} \\d{4}'))
      k <- str_replace(k, '[:punct:]', "")
      k <- paste("1", k, sep = " ")
      date[i] <- dmy(k)
    } else if (!is.na(str_extract(j, paste0("(",monthsexp, ')[:punct:]? \\d{1,2}[:blank:]?[a-z]{0,2}[:punct:] \\d{4}')))){
      k <- str_extract(j, paste0("(",monthsexp, ')[:punct:]? \\d{1,2}[:blank:]?[a-z]{0,2}[:punct:] \\d{4}'))
      # sometimes the month is a weird number of characters and can't be read by lubridate
      # for example, "Sept." can't be read, but "Sep" can
      k <- str_replace(k, word(k, 1), substring(word(k, 1), 1, 3))
      k <- mdy(k)
      try(if(year(k)==1013) year(k) <- 1913)
      date[i] <- k
    }  else if (!is.na(str_extract(j, '[a-zA-Z]+[:punct:]? [a-z][:blank:]?[a-z]{0,2}[:punct:] \\d{4}'))){
      k <- str_extract(j, '[a-zA-Z]+[:punct:]? [a-z][:blank:]?[a-z]{0,2}[:punct:] \\d{4}')
      k <- sub('i', '1', k)
      k <- mdy(k)
      date[i] <- k
      # I could do something more elegant, but it's not worth it for this much
      # Actually this doesn't even work
    } else if (grepl("Egoist057_4_09", journal)) {
      date[i] <- dmy("1 October, 1917")
    } else date[i] <- dmy("1 October, 1917")
  })
  # guess this is an okay workaround
  date_published <- unlist(dateConverter_XML)
  date_published <- as.POSIXct(date_published, origin="1970-1-2")
  date_published
}
# date_vec <- strip_dates("~/Dropbox/The Egoist PDFs/R/litMagModelling/Package/Data/XML")

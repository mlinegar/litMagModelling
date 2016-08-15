#' Create a database of text and metadata to feed into make_model
#' 
#' @param xmlfolder string input folder with text files to run. Typically contains either XML files
#'  from the MJP or files cleaned with stripXML
#' @param date_vec vector vector of dates, usually from strip_dates
#' @param wordsToRemove vector a vector of words to remove from the corpus; an initial stopword list
#' @param stemDoc logical if TRUE, runs SnowballC's stemDocument function
#' @param pattern string pattern of the endings of the files of xmlfolder; defaults to
#'  '.tei.xml', but if xmlfolder is taken from stripXML, should be '.txt'
#' @return a data.frame of text for running with MALLET
#' @seealso \code{\link{strip_dates}} and \code{\link{stripXML}} which this depends on, as well as 
#' \code{\link{make_model}}, which takes the data.frame created by this function as input
#' @export
#' @examples 
#' dataframe <- stripped_xmlData

# this function runs some of the snowballC and tm corpus prep functions so that we can 
# feed into MALLET
# takes as input the folder where the xml files are stored, and the vector of dates stripped from
# those files


prep_corpus <- function(xmlfolder, date_vec, wordsToRemove=NA, stemDoc=NA, pattern = '.tei.xml'){
  library(tm);
  source <- DirSource(xmlfolder)
  xml_words <- c("ab", "div", "npb", "abthe", "tei", "</body>", "<body>", "</text>", "<text>", "TEI",
                 "<ab>", "</ab>", "type=", "teiHeader", "fileDesc", "titleStmt", "publicationStmt",
                 "</title>", "<title>", "idno", "n=", "sourceDesc", "</p>", "<p>", "npb", "pb")
  
  # not used right now
#   xml_words <- c("filedesc", "titlestmt", "titleblast", "issu", "titl", "titlestmt",
#                  "egoisttitl", "titleblasttitl", "publicationstmt", "idno", "typecdi",
#                  "nmojp", "typemetsid", "sourcedesc", "platerp", "titleblasttitle",
#                  "platerp", "div", "tei", "teihead", "text", "front", "pb", "nfoutsidecov",
#                  "teiheader", "nfoutsidecover", "nfrontinsidecover", "titlethe", "egoisttitle",
#                  "pan", "electronic", "transcription", "modernist", "journals", "project", "ocrd",
#                  "taggedpdfteixslp", "pan", "titlethe", "egoisttitle", "insert", "mojp", "id",
#                  "transcription", "titlepage", "titlepart", "th", "blast", "edited"
#   )
#   # these either
#   journal_words <- c("joyce", "dedalus", "bloom", "rouveyr", "lg", "pthe", "everi",
#                      "large", "obvious", "possibly", "inde", "pwe", "deasi", "nd", "ab",
#                      "mme", "thee", "little", "realize", "post", "section", "yang", "quit",
#                      "top", "mere", "note", "definit", "large", "sir", "perhap", "hot", "arghol",
#                      "notic", "yo", "jean", "tabl", "juli", "section", "di", "fanett", "net",
#                      "armi", "becom", "miss", "alreadi", "dolmetsch", "decemb", "dun", "roll",
#                      "stood", "told", "something", "found", "develop", "meet", "relate",
#                      "berkeley", "richard", "fraulein", "marsden", "effect", "appear",
#                      "dont", "thi", "thou", "alway", "cran", "set", "chapter", "comm",
#                      "pankhurst", "tri", "pa", "leave", "something", "stand", "direct",
#                      "phe", "german", "english", "abdiv", "pascas", "freewoman",
#                      "bitzenko", "februari", "januari", "els", "blenner", "pi", "yer",
#                      "novemb", "alphons", "karo", "mean", "cant", "typearticl", "op",
#                      "miriam", "conme", "sawat", "attent", "imit", "mackenzi", "laddhako",
#                      "weining", 
#                      "em", "ess", "vo", "hobson", "roddi", "florence", "drysdal",
#                      "cours", "norman", "item", "pari", "diomed", "noth", "howev",
#                      "cunningham", "debussi", "edward", "paracelsus",
#                      "ibsen", "kreisler", "starestski", "les", "qui",
#                      "que", "des","plus", "pour", "est", "pas", "like", 
#                      "may", "dans", "une", "upon", "esse", "can",
#                      "nos", "avec", "hir", "january", "february", "
#                      march", "april", "june", "july", "august", "september",
#                      "october", "november", "december", "schlaf", "maria",
#                      "james", "must", "nous", "sur", "tout", "vous",
#                      "supplement", "comme", "ces", "bien", "aux", "mon", 
#                      "ma", "jamess", "conmee", "felt", "twelve", "two", 
#                      "three", "four", "five", "six", "seven", "eight", "nine",
#                      "ten", "eleven", "polti", "roddy", "stephen", "bloy", "roddy",
#                      "bertha", "kueifei", "medtner", "alphonse", "berthe", "george", 
#                      "casey", "jane", "delius", "laura", "tarr", "anastasya", "rebecca",
#                      "péguy", "soltyk", "one", "egoist", "thing", "men")
  myCorpus <- Corpus(source, readerControl=list(reader=readPlain))
  myCorpus <- tm_map(myCorpus, PlainTextDocument)
  library(SnowballC);
  myCorpus <- tm_map(myCorpus, removeWords, xml_words)
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("french"))
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("german"))
  
  # this is just to check, should be removed
#   dtm <- DocumentTermMatrix(myCorpus)
#   tdm <- TermDocumentMatrix(myCorpus)
#   freq <- colSums(as.matrix(dtm))   
#   ord <- order(freq)   
#   freq[tail(ord,100)]   
  
  # remove specific words
  # not run right now
  if (!any(is.na(wordsToRemove))) myCorpus <- tm_map(myCorpus, removeWords, wordsToRemove)
  #myCorpus <- tm_map(myCorpus, removeWords, c(journal_words, xml_words))
  if (!is.na(stemDoc)) myCorpus <- tm_map(myCorpus, removeWords, stemDocument)
  
  #myCorpus <- tm_map(myCorpus, stemDocument)  
  # start here
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, PlainTextDocument)
  
  # load corpus into a data.frame so it can be run by MALLET

  # use this instead
  textdata <- sapply(myCorpus, toString)
  textdata <- unname(textdata)
  dataframe <- data.frame(text = textdata, date = date_vec, stringsAsFactors = FALSE)

  id <- list.files(xmlfolder, pattern = paste0('*', pattern))
  id <- sapply(id, FUN = function(i) sub(pattern, '', i))
  dataframe$id <- id
  journal <- vector()
  for (i in 1:length(myCorpus)){
    if (grepl("Egoist", id[i])){
      journal[i] <- "Egoist"
    } else if (grepl("Blast", id[i])){
      journal[i] <- "Blast"
    } else if (grepl("NewFreewoman", id[i])){
      journal[i] <- "NewFreewoman"
    }else if (grepl("Poetry", id[i])){
      journal[i] <- "Poetry Magazine"
    } else{
      journal[i] <- "Freewoman"
    }
  }
  dataframe$journal <- journal
  # from here try left_join on journal and date with data.frame of the metadata
  dataframe
}
# example
# date_vec <- strip_dates("~/Dropbox/The Egoist PDFs/XML")
# dataframe <- prep_corpus("~/Dropbox/The Egoist PDFs/XML/textfiles", date_vec)


#' Create a Mallet instance list; from here we can gather word frequency statistics
#' 
#' @param dataframe data.frame usually taken from prep_corpus, a data.frame with fields for text, journal, date
#' @param n.topics numeric number of topics that we want from LDA
#' @param idcolname string the name of the id column from dataframe
#' @param textcolname string the name of the text column from dataframe
#' @param datecolname string the name of the date column from dataframe
#' @param journalVec vector a vector of the names of journals to be included. 
#' possibile examples are "Blast", "Egoist", "Poetry Magazine", "Freewoman", "NewFreewoman"
#' @param yearRangeRule string the rule specifying which years are to be included. Can take values like
#' "> 1900", "==1919", etc.
#' @param stopListFile string location of the stop list file to be run with Mallet. Defaults to the
#' blank file provided by this package
#' @return a Mallet object that from which we can get word frequencies, and can run LDA on
#' @seealso \code{\link{prep_corpus}}, which creates the data.frame this function draws on
#' @export
#' @examples 
#' date_vec <- date_vec
#' dataframe <- stripped_xmlData
#' marsden_journals <- make_model(dataframe, 10, journalVec = c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
#' old_poetryMagazine <- make_model(dataframe, 10, journalVec = "Poetry Magazine", yearRangeRule = "< 1919")
#' poetry_vocab <- old_poetryMagazine$getVocabulary()
#' poetry_word.freqs <- mallet.word.freqs(marsden_journals)

make_model <- function(dataframe, n.topics, idcolname='id', textcolname='text', datecolname = 'date', journalVec = NA, yearRangeRule = NA, stopListFile = "~/Dropbox/The Egoist PDFs/R/blankstopwords.txt"){
  library(devtools); library(repmis); library(lubridate); require(dplyr);require(mallet);
  # note that this stopword list won't really change anything, as we've already stemmed the doc
  df <- dataframe
  df$years <- year(df[,datecolname])
  if (!any(is.na(journalVec))) df <- subset(df, subset = journal %in% journalVec)
  if (!is.na(yearRangeRule)) df <- subset(df, subset = eval(base::parse(text = paste("years", yearRangeRule))))
  stopwords <- stopListFile
  df$years <- NULL
  mallet.instances <- mallet.import(df[,idcolname], df[,textcolname], stopwords)
  ## Create a topic trainer object.
  topic.model <- MalletLDA(num.topics=n.topics)
  topic.model$loadDocuments(mallet.instances)
  topic.model
}
# from here we can get vocab, for example
# marsden_journals <- make_model(dataframe, 10, journalVec = c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
# old_poetryMagazine <- make_model(dataframe, 10, journalVec = "Poetry Magazine", yearRangeRule = "< 1919")
# poetry_vocab <- old_poetryMagazine$getVocabulary()
# poetry_word.freqs <- mallet.word.freqs(marsden_journals)
# poetry_word.freqs <- arrange(word.freqs, desc(poetry_word.freqs))
# to get all of dataframe, just do make_model(dataframe, 10)

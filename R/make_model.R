#' Create a Mallet object; from here we can gather word frequency statistics, and feed into createLDAvis
#'
#' @param dataframe data.frame usually taken from prep_corpus, a data.frame with fields for text, journal, date
#' @param n.topics numeric number of topics that we want from LDA
#' @param idcolname string the name of the id column from dataframe
#' @param textcolname string the name of the text column from dataframe
#' @param datecolname string the name of the date column from dataframe
#' @param optFreq numeric how often should mallet optimize parameters?
#' @param burnIn numeric number of burn in iterations for LDA
#' @param numRuns numeric total number of runs Mallet should run before finalizing a model
#' @param stopListFile string location of the stop list file to be run with Mallet. Defaults to the
#' blank file provided by this package. Note that this file must have a single word per line,
#' and no special characters, and MUST be a .txt file
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

make_model <- function(dataframe, n.topics, idcolname='id', textcolname='text', datecolname = 'date',
                       stopListFile = "~/Dropbox/The Egoist PDFs/R/blankstopwords.txt",
                       optFreq=20, burnIn=50, numRuns=200){
  library(rJava)
  # note that this stopword list won't really change anything, as we've already stemmed the doc
  df <- dataframe
  mallet.instances <- mallet::mallet.import(df[,idcolname], df[,textcolname], stoplist.file = stopListFile)
  ## Create a topic trainer object.
  topic.model <- mallet::MalletLDA(num.topics=n.topics)
  topic.model$loadDocuments(mallet.instances)
  ## Optimize hyperparameters every optFreq iterations,
  ##  after burnIn burn-in iterations.
  topic.model$setAlphaOptimization(optFreq, burnIn)

  ## Now train a model. Note that hyperparameter optimization is on, by default.
  ##  We can specify the number of iterations. Here we'll use a large-ish round number.
  topic.model$train(numRuns)
  topic.model
}
# from here we can get vocab, for example
# marsden_journals <- make_model(dataframe, 10, journalVec = c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
# old_poetryMagazine <- make_model(dataframe, 10, journalVec = "Poetry Magazine", yearRangeRule = "< 1919")
# poetry_vocab <- old_poetryMagazine$getVocabulary()
# poetry_word.freqs <- mallet.word.freqs(marsden_journals)
# poetry_word.freqs <- arrange(word.freqs, desc(poetry_word.freqs))
# to get all of dataframe, just do make_model(dataframe, 10)

#' Run LDA on a Mallet instance
#' 
#' @param topic.model topic.model usually taken from make_model, a Mallet object
#' @param dataframe data.frame usually taken from prep_corpus, a data.frame with fields for text, journal, date
#' @param n.topics numeric number of topics that we want from LDA
#' @param datecolname string the name of the date column from dataframe
#' @param journalVec vector a vector of the names of journals to be included. Must be identical to values
#'  that generated topic.model
#' possibile examples are "Blast", "Egoist", "Poetry Magazine", "Freewoman", "NewFreewoman"
#' @param yearRangeRule string the rule specifying which years are to be included. Can take values like
#' "> 1900", "==1919", etc. Must be identical to values that generated topic.model
#' @param optFreq numeric how often should mallet optimize parameters?
#' @param burnIn numeric number of burn in iterations for LDA
#' @param numRuns numeric total number of runs Mallet should run before finalizing a model
#' @param num.topwords numeric number of words to display on topic labels
#' @return a data.frame of topic frequencies by document that we can analyze and graph
#' @seealso \code{\link{prep_corpus}}, which creates the data.frame this function draws on
#' \code{\link{make_model}}, which creates the mallet object this function uses
#' @export
#' @examples 
#' date_vec <- date_vec
#' dataframe <- stripped_xmlData
#' marsden_journals <- make_model(dataframe, 10, journalVec = c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
#' marz.df <- train_model(marsden_journals, dataframe, c("Blast", "Egoist", "Freewoman", "NewFreewoman"))


train_model <- function(topic.model, dataframe, n.topics, optFreq=20, burnIn=50, numRuns=200, num.topwords=5, datecolname = 'date', journalVec = NA, yearRangeRule = NA){
  # Quick and dirty solution - journalVec and yearRangeRule have to be the same as in the last function
  ## Optimize hyperparameters every optFreq iterations, 
  ##  after burnIn burn-in iterations.
  topic.model$setAlphaOptimization(optFreq, burnIn)
  
  ## Now train a model. Note that hyperparameter optimization is on, by default.
  ##  We can specify the number of iterations. Here we'll use a large-ish round number.
  topic.model$train(numRuns)
  ## Get the probability of topics in documents and the probability of words in topics.
  ## By default, these functions return raw word counts. Here we want probabilities,
  ## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
  doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
  topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
  
  topics.labels <- rep("", n.topics)
  for(i in 1:n.topics){
    topics.labels[i] <- paste(mallet.top.words(topic.model,
                                               topic.words[i,], num.top.words=num.topwords)$words, collapse=" ")
  }
  doc.topics <- as.data.frame(doc.topics)
  colnames(doc.topics) <- topics.labels
  df <- dataframe
  df$years <- year(df[,datecolname])
  if (!any(is.na(journalVec))) df <- subset(df, subset = journal %in% journalVec)
  if (!is.na(yearRangeRule)) df <- subset(df, subset = eval(base::parse(text = paste("years", yearRangeRule))))
  df$years <- NULL
  df <- cbind(df, doc.topics)
  df
}



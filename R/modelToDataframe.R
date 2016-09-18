#' Run LDA on a Mallet instance
#'
#' @param topic.model topic.model usually taken from make_model, a Mallet object
#' @param prepped_corpus data.frame usually taken from prep_corpus, a data.frame with fields for text, journal, date
#' @param textcolname string the name of the text column from prepped_corpus
#' @param num.topwords numeric number of words to display on topic labels
#' @param incCorpusText logical whether or not to include the raw text data from prepped_corpus in the final data.frame to be output
#' @return a data.frame of topic frequencies by document that we can analyze and graph
#' @seealso \code{\link{prep_corpus}}, which creates the data.frame this function draws on
#' \code{\link{make_model}}, which creates the mallet object this function uses
#' @export
#' @examples
#' date_vec <- date_vec
#' dataframe <- stripped_xmlData
#' marsden_journals <- make_model(dataframe, 10, journalVec = c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
#' marz.df <- train_model(marsden_journals, dataframe, c("Blast", "Egoist", "Freewoman", "NewFreewoman"))


modelToDataframe <- function(topic.model, prepped_corpus, n.topics = n.topics, num.topwords=5, textcolname = 'text', incCorpusText = TRUE){
  # will eventually make incCorpusText = FALSE by default, just need to figure out eval-parse for strings with ',' first
  ## Get the probability of topics in documents and the probability of words in topics.
  ## By default, these functions return raw word counts. Here we want probabilities,
  ## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
  doc.topics <- mallet::mallet.doc.topics(topic.model, smoothed=T, normalized=T)
  topic.words <- mallet::mallet.topic.words(topic.model, smoothed=T, normalized=T)

  topics.labels <- rep("", n.topics)
  for(i in 1:n.topics){
    topics.labels[i] <- paste(mallet::mallet.top.words(topic.model,
                                               topic.words[i,], num.top.words=num.topwords)$words, collapse=" ")
  }
  doc.topics <- as.data.frame(doc.topics)
  colnames(doc.topics) <- topics.labels
  df <- prepped_corpus
  if(!incCorpusText) df[,textcolname] <- NULL
  df <- cbind(df, doc.topics)
  df
}



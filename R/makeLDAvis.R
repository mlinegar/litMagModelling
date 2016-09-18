#' Easy interface to LDAvis, only requires a trained topic.model as input
#' @import rJava
#' @param topic.model Mallet-object a trained topic.model from MALLET
#' @param outDir character directory to store html/js/json files
#' @param openBrowser logical should R open a browser to create visualizations?
#' @param asGist logical should the vis be uploaded as a gist? Requires gistr
#' @param ... arguments passed onto gistr::gist_create
#' @return html/js/json files of a LDAvis visualization
#' @export

makeLDAvis <- function(topic.model, outDir = tempfile(), openBrowser = TRUE, asGist = FALSE, ...){
  phi <- mallet::mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)
  theta <- mallet::mallet.doc.topics(topic.model, smoothed = TRUE, normalized = TRUE)
  doc.length <- rowSums(mallet::mallet.doc.topics(topic.model, smoothed = FALSE, normalized = FALSE))
  word.freqs <- mallet::mallet.word.freqs(topic.model)
  vocab <- topic.model$getVocabulary()
  json <- list(
    phi = phi, theta = theta, doc.length = doc.length, vocab = vocab,
    term.frequency = droplevels(word.freqs)$term.freq)
  jsonLDA <- LDAvis::createJSON(phi = json$phi, theta = json$theta, doc.length = json$doc.length,
                                vocab = json$vocab, term.frequency = json$term.frequency)
  # if you want to put it on Github, need to havae gistr installed
  # this can be done by:
  # devtools::install_github('rOpenSci/gistr')
  # library(gistr)
  # see help("serVis") for more details
  if(asGist==TRUE) library(gistr)
  LDAvis::serVis(jsonLDA, out.dir = outDir, open.browser = openBrowser, as.gist = asGist, ... = ...)
}

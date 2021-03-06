% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_model.R
\name{train_model}
\alias{train_model}
\title{DEPRICATED IN V0.1.1, see new example file. Run LDA on a Mallet instance}
\usage{
train_model(topic.model, dataframe, n.topics, optFreq = 20, burnIn = 50,
  numRuns = 200, num.topwords = 5, datecolname = "date",
  journalVec = NA, yearRangeRule = NA)
}
\arguments{
\item{topic.model}{topic.model usually taken from make_model, a Mallet object}

\item{dataframe}{data.frame usually taken from prep_corpus, a data.frame with fields for text, journal, date}

\item{n.topics}{numeric number of topics that we want from LDA}

\item{optFreq}{numeric how often should mallet optimize parameters?}

\item{burnIn}{numeric number of burn in iterations for LDA}

\item{numRuns}{numeric total number of runs Mallet should run before finalizing a model}

\item{num.topwords}{numeric number of words to display on topic labels}

\item{datecolname}{string the name of the date column from dataframe}

\item{journalVec}{vector a vector of the names of journals to be included. Must be identical to values
 that generated topic.model
possibile examples are "Blast", "Egoist", "Poetry Magazine", "Freewoman", "NewFreewoman"}

\item{yearRangeRule}{string the rule specifying which years are to be included. Can take values like
"> 1900", "==1919", etc. Must be identical to values that generated topic.model}
}
\value{
a data.frame of topic frequencies by document that we can analyze and graph
}
\description{
DEPRICATED IN V0.1.1, see new example file. Run LDA on a Mallet instance
}
\examples{
date_vec <- date_vec
dataframe <- stripped_xmlData
marsden_journals <- make_model(dataframe, 10, journalVec = c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
marz.df <- train_model(marsden_journals, dataframe, c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
}
\seealso{
\code{\link{prep_corpus}}, which creates the data.frame this function draws on
\code{\link{make_model}}, which creates the mallet object this function uses
}


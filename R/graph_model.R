#' Graph results of LDA
#' @param df data.frame a trained LDA model, taken from train_model
#' @param baseColNames vector string names of the "base" columns - text, date, id, and journal
#' @param chunkLen the number of graphs per page. If less than the number of topics, which is found
#' automatically, splits the dataframe into n.topics/chunkLen sections, each of which has at most
#' chunkLen plots of topic makeup over the documents
#' @return n.topics graphs spread of chunkLen pages
#' @seealso \code{\link{train_model}}, which runs LDA on a Mallet instance
#' @export
#' @examples
#' date_vec <- date_vec
#' dataframe <- stripped_xmlData
#' marsden_journals <- make_model(dataframe, 10, journalVec = c("Blast", "Egoist", "Freewoman", "NewFreewoman"))
#' graph_model(marsden_journals, chunkLen = 6)

graph_model <- function(df, baseColNames=c("text", "date", "id", "journal"), chunkLen=10){
  topic.df <- df[, -which(names(df) %in% baseColNames)]
  base.df <- df[, which(names(df) %in% baseColNames)]
  n.topics <- ncol(topic.df)
  if (n.topics > chunkLen) {
    topic.cols <- split(1:n.topics, ceiling(seq_along(1:n.topics)/chunkLen))
    for (i in seq_len(length(topic.cols))) {
      colsToGraph <- topic.cols[[i]]
      topicsToGraph <- topic.df[,min(colsToGraph):max(colsToGraph)]
      subdf <- cbind(base.df, topicsToGraph)
      # if the names of the columns ever change this will have to as well
      # for now I'll leave it
      M <- tidyr::gather(subdf,topic,value,-id,-date,-journal,-text) %>%
        dplyr::group_by(topic,date,journal, id) %>%
        dplyr::summarize(value=mean(value))
      plot <- ggplot2::ggplot(M,aes(x=date,color=journal,y=value)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::facet_grid(topic~.) +
        ggplot2::ggtitle(paste0("Topic makeup over time for topics ", min(colsToGraph), ":", max(colsToGraph)))
      print(plot)
    }
  } else {
    M <- tidyr::gather(df,topic,value,-id,-date,-journal,-text) %>%
      dplyr::group_by(topic,date,journal, id) %>%
      dplyr::summarize(value=mean(value))
    plot <- ggplot2::ggplot(M,aes(x=date,color=journal,y=value)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::facet_grid(topic~.)
    print(plot)
  }
}

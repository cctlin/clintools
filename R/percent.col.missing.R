#' Percent missing of each column of data frame
#'
#' Takes in a dataframe and gives you what percent of each row is missing 
#' Outputs missingess of each column
#' @param df dataframe to analyze
#' @keywords blank
#' @export
#' @examples percent.col.missing(df) # just pass it a dataframe
#' percent.col.missing()


percent.col.missing <- function(df){
  count <-df %>% is.na() %>% colSums()
  perc <- count / dim(df)[1] * 100
  missingperc <- bind_cols(colnames(imdb) %>% as.tibble(), count %>% as.tibble(), perc %>% as.tibble())
  colnames(missingperc) <- c("Label", "Count", "Percentage")
  missingperc <- missingperc %>% arrange(desc(Percentage))
  missingperc
}
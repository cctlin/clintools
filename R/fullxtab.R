#' Improved xtab
#'
#' Gets the xtab, calculates the percentages and gets the p-value
#' @param working_data data frame to work with
#' @param var1 colname as a string, the categories of this var will be the row names for the xtab
#' @param var2 colname as a string, will the cols for the xtab, needs to be in 0 / 1 form
#' @keywords blank
#' @export
#' @examples fullxtab_function(var1, var2)
#' fullxtab_function()

#

fullxtab <- function(working_data, var1, var2){
  # calculate the crosstab
  crosstab <- substitute(
    xtabs(~ var1 + var2, data = working_data),
    list(var1 = as.name(var1),var2 = as.name(var2))
  ) %>% eval()
  # tidy up the crosstab
  crosstab <- crosstab %>%
    tidy() %>%
    spread_( key = var2, value = "Freq")
  colnames(crosstab) <- c("Term", "no", "yes")
  # get percentages
  crosstab <- crosstab %>% mutate(no_perc = no / sum(no), yes_perc = yes / sum(yes))
  # get fisher exact test p.value
  fish <- substitute(
    fisher.test(x = working_data$var1, y = working_data$var2) %>% tidy(),
    list(var1 = as.name(var1),var2 = as.name(var2))
  ) %>% eval()
  pval <- tribble (~"p.value", fish["p.value"][1,1], "")
  # combine into full output
  fullxtab <- bind_cols(crosstab, pval)

  # output
  fullxtab
}

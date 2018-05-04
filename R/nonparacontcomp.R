#' Non-parametric Comparison of Variable Columns
#'
#' Gets the median, calculates the IQR and gets the p-value using Fisher's Exact test
#' @param var1 var1 for comparison
#' @keywords var2 for comparison
#' @export
#' @examples
#' nonparacontcomp()

nonparacontcomp <- function(var1,var2){
  comparison <- substitute( 
    working_data %>% group_by(var1) %>% summarise(median(var2), IQR(var2)),
    list(var1 = as.name(var1),var2 = as.name(var2))
  ) %>% eval()
  
  mann.whit.p <- substitute( 
    wilcox.test(working_data$var2 ~ working_data$var1) %>% tidy(), # independent 2-group Mann-Whitney U Test 
    list(var1 = as.name(var1),var2 = as.name(var2))
  ) %>% eval()
  
  pval <- tribble (~"p.value", mann.whit.p["p.value"][1,1], "")
  fullcomp <- bind_cols(comparison, pval)
  fullcomp
}
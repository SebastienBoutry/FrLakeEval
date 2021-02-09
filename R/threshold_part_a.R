#' Determine the first threshold's vector
#'
#' @param code_sandre_parameter Sandre's code of the physico-chemical parameter
#' @param Zmoy average depth
#'
#' @return the first threshold's vector
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr filter


threshold_part_a <- function(code_sandre_parameter,Zmoy){
  a <- tibble_valeurs_seuils %>%
    filter(cd_sandre_parametre==code_sandre_parameter) %>%
    pull(a)
  b <- tibble_valeurs_seuils %>%
    filter(cd_sandre_parametre==code_sandre_parameter) %>%
    pull(b)
  if(code_sandre_parameter %in% c(1350,1335,1332)){
    metric_a <-(a*Zmoy^b)
  }else{
    metric_a <- NA_integer_}
  return(metric_a)
}


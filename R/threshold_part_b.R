#' Determine the second threshold's vector
#'
#' @param code_sandre_parameter Sandre's code of the physico-chemical parameter
#' @param Zmoy average depth
#'
#' @return the second threshold's vector
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr filter

threshold_part_b <- function(code_sandre_parameter,Zmoy){
  c<- tibble_valeurs_seuils %>%
    filter(cd_sandre_parametre==code_sandre_parameter) %>%
    pull(c)
  d <- tibble_valeurs_seuils %>%
    filter(cd_sandre_parametre==code_sandre_parameter) %>%
    pull(d)
  if(code_sandre_parameter %in% c(1350,1335,1332)){
    metric_b <-(c*(Zmoy +1)^d)
  }else{
    metric_b <- NA_integer_}
  return(metric_b)
}


#' Determine the parameter's class
#'
#' @param value parameter value
#' @param code_sandre_parameter Sandre's code of the physico-chemical parameter
#' @param Zmoy average depth
#'
#' @return the parameter's class
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom dplyr mutate

eval_chemical_class <- function(value,code_sandre_parameter,Zmoy){
  if(!code_sandre_parameter %in% c(1350,1335,1332,1340)){
    stop("l'argument code_sandre_parameter est different de 1350,1335,1332,1340")
  }
  if(!Zmoy>0){
    stop("l'argument Zmoy n'est pas un numerique positif")
  }
  if(code_sandre_parameter==1332){
    class_eval <- as.character(
      cut(value,
          breaks=threshold_values(code_sandre_parameter,Zmoy),
          labels=c("TB","B","Mo","Me","Ma")[5:1]))
  }else{
    class_eval <- as.character(
      cut(value,
          breaks=threshold_values(code_sandre_parameter,Zmoy),
          labels=c("TB","B","Mo","Me","Ma")))
  }
  return(class_eval)
}

#' Select the threshold's vector
#'
#' @param code_sandre_parameter Sandre's code of the physico-chemical parameter
#' @param Zmoy average depth
#' @keywords threshold's values, vector
#'
#' @return the threshold's vector
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate


threshold_values <- function(code_sandre_parameter,Zmoy){
  if(!code_sandre_parameter %in% c(1350,1335,1332,1340)){
    stop("l'argument code_sandre_parameter est different de 1350,1335,1332,1340")
  }
  limites <- tibble_valeurs_seuils %>%
    dplyr::filter(cd_sandre_parametre == code_sandre_parameter) %>%
    dplyr::pull(limite)

  valeurs_seuils <- tibble::tibble(seuil_a=threshold_part_a(code_sandre_parameter,Zmoy),
                           seuil_b=threshold_part_b(code_sandre_parameter,Zmoy)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(min_val = min(seuil_a,seuil_b), max_val = max(seuil_a,seuil_b))
  if(code_sandre_parameter %in% c(1350,1335,1332)){
    if(code_sandre_parameter %in% c(1350,1335)){
      valeurs_seuils <- valeurs_seuils %>%
        dplyr::pull(min_val)
      valeurs_seuils <- round(valeurs_seuils,1)

    }else{
      valeurs_seuils <- valeurs_seuils %>%
        dplyr::pull(max_val)
      valeurs_seuils <-round(valeurs_seuils,2)
    }
    names(valeurs_seuils) <- limites
  }
  if(code_sandre_parameter %in% c(1340)){
    if(Zmoy<=15){
      valeurs_seuils <-c(2200,5300,12600,30100)
    }else{
      valeurs_seuils <-c(1200,2600,5600,12100)
    }
    names(valeurs_seuils) <- limites
  }
  if(code_sandre_parameter==1332){
    valeurs_seuils <- c(+Inf,valeurs_seuils,0)
  }else{
    valeurs_seuils <- c(0,valeurs_seuils,+Inf)
  }
  return(valeurs_seuils)
}

#' A Hilmo tidy function
#'
#' This function allows you to change the data from long to wide format
#'
#' Every row represents one patient
#' @param dataname the name of your data you are working with
#' @keywords  hilmo tidy
#' @export
#' @examples
#' Hilmo_tidy()

Hilmo_tidy <- function(dataname) {

  dataname%>%
    group_by(tnro) %>%
    summarise(SUKUP=toString(unique(SUKUP)),
              IKA=paste(IKA, collapse=" - "),
              TMPKOODI=paste(TMPKOODI, collapse=" - "),
              KOODI1 = paste(KOODI1, collapse=" - "),
              pvm=paste(pvm, collapse=" - "),
              year=paste(year, collapse=" - "),
              incluusio=paste(incluusio, collapse=" - "))


}

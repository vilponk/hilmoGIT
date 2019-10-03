#' A Hilmo hoitojakso tidy function
#'
#' This function allows you to change the data from long to wide format by adding all diagnose codes into one row.
#'
#' Every row represents one hoitojakso
#' @param dataname the name of your data you are working with
#' @keywords  hilmo tidy hoitojakso
#' @export
#' @examples
#' Hilmo_tidy_hoitojakso()
#'
Hilmo_tidy_hoitojakso <- function(dataname) {

  dataname %>%
    group_by(ID) %>%
    summarise(tnro=toString(unique(tnro)),
              SUKUP=toString(unique(SUKUP)),
              IKA=toString(unique(IKA)),
              TMPKOODI=toString(unique(TMPKOODI)),
              KOODI1 = toString(unique(KOODI1)),
              lpvm= toString(unique(lpvm)),
              year= toString(unique(year)),
              incluusio= toString(unique(incluusio))) %>%
    mutate(SUKUP=factor(SUKUP, levels=c(1,2), labels=c("Male", "Female")),
           IKA=as.numeric(IKA),
           lpvm=as.Date(lpvm, format= "%Y-%m-%d"),
           year=as.factor(year))


}

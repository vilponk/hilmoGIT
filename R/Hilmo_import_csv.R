
#' A Hilmo import .csv function
#'
#' This function allows you to import csv files in more tidy format and it creates ID column from lahtopaiva and tnro
#' @param filename the name of your file you wish to import
#' @keywords csv hilmo import
#' @export
#' @examples
#' Hilmo_import_csv()


Hilmo_import_csv <- function(filename) {

  library(tidyverse)

  data <- read_delim(filename,";", escape_double = FALSE, trim_ws = TRUE)

  data %>%
    select(SUKUP,IKA, TMPKOODI, KOODI1, lahtopvm, tnro, PALTU, PALTUTAR, KOKU) %>%
    mutate(ID=rep(NA)) %>%
    unite(ID, tnro, lahtopvm, sep="", remove=F) %>%
    mutate(ID = str_remove(ID,"/")) %>%
    mutate(ID = str_remove(ID,"/")) %>%
    mutate(lpvm=as.Date(lahtopvm, format= "%d/%m/%Y")) %>%
    separate(lahtopvm, into=c("day","month","year"), sep="/") %>%
    select(-day, -month) %>%
    select(ID, tnro, SUKUP, IKA, PALTU, PALTUTAR, KOKU, TMPKOODI, KOODI1, lpvm, year)

}

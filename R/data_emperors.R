#' Emperors datacube documentation
#' @description
#'   `r describe_datacube(emperors)`
#' @format 
#' \describe{
#' \item{Wikipedia: }{A dataset with 68 observations and the
#' following 15 variables:
#' ID, Begin, End, FullName, Birth, Death,
#' CityBirth, ProvinceBirth, Rise, Cause,
#' Killer, Dynasty, Era, Notes, Verif.}
#' \item{UNRV: }{A dataset with 99 observations and the
#' following 7 variables: ID, Begin, End,
#' Birth, Death, FullName, Dynasty.}
#' \item{Britannica: }{A dataset with 87 observations and the
#' following 3 variables: ID, Begin, End.}
#' }
#' @source
#'   `r call_citations(emperors, output = "help")`
#' @section URL:
#' * wikipedia: \url{https://en.wikipedia.org/wiki/List_of_Roman_emperors}
#' * UNRV: \url{https://www.unrv.com/government/emperor.php}
#' * britannica: \url{https://www.britannica.com/topic/list-of-Roman-emperors-2043294}
#' @section Mapping:
#' * wikipedia:
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | name | ID |
#' | reign.start | Begin |
#' | reign.end | End |
#' | name.full | FullName |
#' | birth | Birth |
#' | death | Death |
#' | birth.cty | CityBirth |
#' | birth.prv | ProvinceBirth |
#' | rise | Rise |
#' | cause | Cause |
#' | killer | Killer |
#' | dynasty | Dynasty |
#' | era | Era |
#' | notes | Notes |
#' | verif.who | Verif |
#' 
#' * UNRV:
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | 'Common Name' | ID |
#' |  Beg | Begin |
#' | 'Full Name/Imperial Name' | FullName |
#' | 'Dynasty/Class/Notes' | Dynasty |
#' 
#' * britannica:
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Name | ID |
#' | reign_start | Begin |
#' | reign_end | End |
#' 
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(emperors, mreport)
#' ```
"emperors"

info_emperors <- tibble::tibble(Dataset = c("Wikipedia","UNRV","Britannica"),
                                Source = c("Wikipedia, 'List_of_Roman_emperors',  https://en.wikipedia.org/wiki/List_of_Roman_emperors, Accessed on 2021-07-22.",
                                           "UNRV, 'Roman Emperor list', https://www.unrv.com/government/emperor.php, Accessed on 2021-07-22.",
                                           "Britannica, 'List of Roman emperors', https://www.britannica.com/topic/list-of-Roman-emperors-2043294, Accessed on 2021-07-22."),
                                URL = c("https://en.wikipedia.org/wiki/List_of_Roman_emperors",
                                        "https://www.unrv.com/government/emperor.php",
                                        "https://www.britannica.com/topic/list-of-Roman-emperors-2043294"))


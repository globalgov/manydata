#' Emperors database documentation
#'
#' @format The emperors database is a list that contains the
#' following 3 datasets: wikipedia, UNRV, britannica.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()`.
#' functions.
#' \describe{
#' \item{wikipedia: }{A dataset with 68 observations and the
#' following 15 variables:
#' ID, Begin, End, FullName, Birth, Death,
#' CityBirth, ProvinceBirth, Rise, Cause,
#' Killer, Dynasty, Era, Notes, Verif.}
#' \item{UNRV: }{A dataset with 99 observations and the
#' following 7 variables: ID, Begin, End,
#' Birth, Death, FullName, Dynasty.}
#' \item{britannica: }{A dataset with 87 observations and the
#' following 3 variables: ID, Begin, End.}
#' }
#' @source
#' \itemize{
#' \item{wikipedia: }{
#' Wikipedia, List_of_Roman_emperors,
#' \url{https://en.wikipedia.org/wiki/List_of_Roman_emperors}, Accessed on 2021-07-22
#' }
#' \item{UNRV: }{
#' UNRV, Roman Emperor list,
#' \url{https://www.unrv.com/government/emperor.php},
#' Accessed on 2021-07-22
#' }
#' \item{britannica: }{
#' Britannica, List of Roman emperors,
#' \url{https://www.britannica.com/topic/list-of-Roman-emperors-2043294},
#' Accessed on 2021-07-22
#' }
#' }
#' @section URL:
#' \itemize{
#' \item{wikipedia: }{
#' \url{https://en.wikipedia.org/wiki/List_of_Roman_emperors}
#' }
#' \item{UNRV: }{
#' \url{https://www.unrv.com/government/emperor.php}
#' }
#' \item{britannica: }{
#' \url{https://www.britannica.com/topic/list-of-Roman-emperors-2043294}
#' }
#' }
#' @section Mapping:
#' \itemize{
#' \item{wikipedia: }{
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
#' }
#' \item{UNRV: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | 'Common Name' | ID |
#' |  Beg | Begin |
#' | 'Full Name/Imperial Name' | FullName |
#' | 'Dynasty/Class/Notes' | Dynasty |
#' 
#' }
#' \item{britannica: }{
#' Variable Mapping
#' 
#' |  *from*  | *to*
#' |:------------:|:------------:|
#' | Name | ID |
#' | reign_start | Begin |
#' | reign_end | End |
#' 
#' }
#' }
#' @md
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(emperors, messydates::mreport)
#' ```
"emperors"

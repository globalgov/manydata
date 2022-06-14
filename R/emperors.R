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
#' ID, Beg, End, FullName, Birth, Death,
#' CityBirth, ProvinceBirth, Rise, Cause,
#' Killer, Dynasty, Era, Notes, Verif.}
#' \item{UNRV: }{A dataset with 99 observations and the
#' following 7 variables: ID, Beg, End,
#' Birth, Death, FullName, Dynasty.}
#' \item{britannica: }{A dataset with 87 observations and the
#' following 3 variables: ID, Beg, End.}
#' }
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(emperors, descriptr::ds_screener)
#' ```
"emperors"

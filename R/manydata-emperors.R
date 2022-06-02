#' emperors database documentation
#'
#' @format The emperors database is a list that contains the
#' following 3 datasets: wikipedia, UNRV, britannica.
#' For more information and references to each of the datasets used,
#' please use the `data_source()` and `data_contrast()` functions.
#'\describe{
#' \item{wikipedia: }{A dataset with 68 observations and the following
#' 16 variables: index, name, name.full, birth, death, birth.cty, birth.prv,
#' rise, reign.start, reign.end, cause, killer, dynasty, era, notes, verif.who.}
#' \item{UNRV: }{A dataset with 99 observations and the following
#' 7 variables: ID, Beg, End, Birth, Death, FullName, Dynasty.}
#' \item{britannica: }{A dataset with 87 observations and the following
#' 3 variables: ID, Beg, End.}
#' }
#' @details
#' ``` {r, echo = FALSE, warning = FALSE}
#' lapply(emperors, skimr::skim_without_charts)
#' ```
"emperors"

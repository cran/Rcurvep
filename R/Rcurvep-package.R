#' Rcurvep: Concentration-Response Data Analysis using Curvep
#'
#' Provide an R interface for processing concentration-response datasets using Curvep, a response noise filtering algorithm. The algorithm was described in the publications (see references below).
#' Other parametric fitting approaches (e.g., Hill equation) are also adopted for ease of comparison.
#' Also, methods for calculating the confidence interval around the activity metrics are also provided.
#' The methods are based on the bootstrap approach to simulate the datasets.
#' The simulated datasets can be used to derive the baseline noise threshold in an assay endpoint.
#' This threshold is critical in the toxicological studies to derive the point-of-departure (POD).
#'
#' @details
#' Different strategies are used to simulate the datasets:
#' * Curvep - bootstrapping the responses of replicates at each concentration
#' * Hill equation - bootstrapping the residuals and adding back to the fitted responses (by Hill) at each concentration
#'
#' For Curvep the bootstrapping strategy is different depending on the type of datasets.
#' Datasets can be grouped into three types:
#'
#' \enumerate{
#'   \item dichotomous binary incidence data (e.g. mortality data from alternative animal model data)
#'   \item continuous data with high number of replicates (e.g. alternative animal model data)
#'   \item continuous data with low number of replicates (e.g. in vitro data)
#' }
#'
#' Bootstrapping strategies:
#'
#' \enumerate{
#'   \item bootstrap incidence out of total animals per concentration then calculate percentage of incidence
#'   \item bootstrap replicate responses per concentration directly
#'   \item bootstrap vehicle control responses and add back to the fitted responses by linear regression per concentration (experimental)
#' }
#'
#' To learn more about Rcurvep start with the vignettes:
#' `browseVignettes(package = "Rcurvep")`
#'

#'
#'
#' @references{
#' ## Curvep
#'   \insertRef{PMID:20980217}{Rcurvep}\cr
#'
#'   \insertRef{PMID:27518631}{Rcurvep}\cr
#' ## Bootstrap
#'   \insertRef{PMID:30944845}{Rcurvep}\cr
#'
#'   \insertRef{PMID:30321397}{Rcurvep}\cr
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom Rdpack reprompt
#' @importFrom methods is
#' @importFrom stats constrOptim cor dnorm dt fitted lm mad median na.omit nls optim predict quantile sd smooth.spline
#' @importFrom utils data head modifyList tail
#' @docType package
#' @name Rcurvep
NULL
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

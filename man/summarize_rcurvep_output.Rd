% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcurvep_summary.R
\name{summarize_rcurvep_output}
\alias{summarize_rcurvep_output}
\title{Clean and summarize the output of rcurvep object}
\usage{
summarize_rcurvep_output(
  d,
  inactivate = NULL,
  ci_level = 0.95,
  clean_only = FALSE
)
}
\arguments{
\item{d}{The rcurvep object from \code{\link[=combi_run_rcurvep]{combi_run_rcurvep()}} and \code{\link[=run_rcurvep]{run_rcurvep()}}.}

\item{inactivate}{A character string, default = NULL,
to make the curve with this string in the Comments column as inactive.
or a vector of index for the rows in the act_set that needs to be inactive}

\item{ci_level}{Default = 0.95 (95 percent of confidence interval).}

\item{clean_only}{Default = FALSE, only the 1st, 2nd task will be performed (see Details).}
}
\value{
A list of named components: result and config (and act_summary).
The result and config are the copy of the input d (but with modifications if \emph{inactivate} is not NULL).
If (clean_only = FALSE), an \emph{act_summary} is added.

Suffix meaning in column names in \emph{act_summary}: med (median), cil (lower end confidence interval),
ciu (higher end confidence interval)
Often used columns in \emph{act_summary}: n_curves (number of curves used in summary), hit_confidence (fraction of active in n_curves)
}
\description{
Clean and summarize the output of rcurvep object
}
\details{
The function can perform the following tasks:
\enumerate{
\item add an column, hit, in the \emph{act_set}
\item unhit (make result as inactive) if the Comments column contains a certain string
\item summarize the results
}
The curve is considered as "hit" if its responses are monotonic after processing by Curvep.
However, often, if the curve is "INVERSE" (yet monotonic) is not considered as an active curve.
By using the information in the Comments column, we can "unhit" these cases.

When (clean_only = FALSE, default), a tibble, act_summary is generated with confidence intervals of the activity metrics.
The quantile approach is used to calculate the confidence interval.
For potency activity metrics, if value is NA, highest tested concentration is used in the summary.
For other activity metrics, if value is NA, 0 is used in the summary.
}
\examples{

data(zfishbeh)

# original datasets
out <- combi_run_rcurvep(zfishbeh, n_samples = NULL, TRSH = c(5, 10))
out_res <- summarize_rcurvep_output(out)

\donttest{
# unhit when comment has "INVERSE"
out <- summarize_rcurvep_output(out, inactivate = "INVERSE")

# unhit for certain rows in act_set
out <- summarize_rcurvep_output(out, inactivate = c(2,3))

# simulated datasets
out <- combi_run_rcurvep(zfishbeh, n_samples = 3, TRSH = c(5, 10))
out_res <- summarize_rcurvep_output(out)
}

}
\seealso{
\code{\link[=combi_run_rcurvep]{combi_run_rcurvep()}}, \code{\link[=run_rcurvep]{run_rcurvep()}}
}

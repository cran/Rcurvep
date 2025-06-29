% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcurvep_simulate.R
\name{create_dataset}
\alias{create_dataset}
\title{Create concentration-response datasets that can be applied in the \code{run_rcurvep()}}
\usage{
create_dataset(d, n_samples = NULL, vdata = NULL)
}
\arguments{
\item{d}{Datasets with concentration-response data.
Examples are \link{zfishbeh} and \link{zfishdev}.}

\item{n_samples}{NULL (default) for not to simulate responses or an integer number to indicate the number of responses per concentration to simulate.}

\item{vdata}{NULL (default) for not to simulate responses or a vector of numeric responses in vehicle control wells to use as error.
This parameter only works when n_samples is not NULL; an experimental feature.}
}
\value{
The original dataset with a new column, sample_id (if n_samples is not NULL) or
the summarized dataset with columns as \link{zfishbeh}.
}
\description{
The input dataset is created either by summarizing the response data
or by simulating the response data.
}
\details{
Curvep requires 1-to-1 concentration response relationship.
For the dataset that does not meet the requirement, the following strategies are applied:
\subsection{Summary (when n_samples = NULL)}{
\itemize{
\item For dichotomous responses, percentage is reported (n_in/N*100).
\item For continuous responses, median value of responses per concentration is reported.
}
}

\subsection{Simulation (when n_samples is a positive integer)}{
\itemize{
\item For dichotomous responses, bootstrap approach is used on the "n_in" vector
to create a vector of percent response.
\item For continuous responses, options are a) direct sampling;
b) responses from the linear fit using the original data + error of responses based on the supplied vehicle control data
}
}
}
\examples{

# datasets with continuous response data
data(zfishbeh)

## default
d <- create_dataset(zfishbeh)

## add samples
d <- create_dataset(zfishbeh, n_samples = 3)

## add samples and vdata
d <- create_dataset(zfishbeh, n_samples = 3, vdata = rnorm(100))

# dataset with dichotomous response data
data(zfishdev)

## default
d <- create_dataset(zfishdev)

## add samples
d <- create_dataset(zfishdev, n_samples = 3)

}
\seealso{
\code{\link[=run_rcurvep]{run_rcurvep()}}
}

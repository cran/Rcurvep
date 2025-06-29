% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{zfishdev}
\alias{zfishdev}
\title{Subsets of concentration response datasets from zebrafish developmental toxicity assays}
\format{
A tibble with 96 rows and 5 columns:

\describe{
\item{endpoint}{endpoint name + at time point measured}
\item{chemical}{chemical name + CASRN}
\item{conc}{concentrations in log10(M) format}
\item{n_in}{number of incidence}
\item{N}{number of embryos}
}
}
\source{
Biobide study S-BBD-00016/15
}
\usage{
zfishdev
}
\description{
The datasets contain 4 toxicity endpoints and 3 chemicals.
}
\keyword{datasets}

\name{extract}
\alias{extract}
\title{Dyad Ratios Alrogrithm}
\description{
Estimates the Dyad Ratios Algorithm for constructing latent time series from survey research marginals. 
}
\usage{
extract(varname, date, index, ncases = NULL, unit = "A", mult = 1, 
  begindt = NA, enddt = NA, npass = 1, smoothing = TRUE, 
  endmonth = 12, plot=FALSE, verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varname}{String giving the name of the input series to be smoothed.  This should identify similar or comparable values in the series.   Values in the series that have the same \code{varname} will be assumed to come from the same source. }
  \item{date}{ISO numeric representation of the date the survey was in the field (usually the start, end or median date).}
  \item{index}{Numeric value of the series.  It might be a percent or proportion responding in a single category (e.g., the approve response in presidential approval) or some multi-response summary.  For ease of interpretation, polarity should be the same for all items. }
  \item{ncases}{Number of cases e.g., sample size, of the survey.  This provides differential weighting for the values.  Setting this to \code{NULL} or leaving it blank will weight each value equally.}
  \item{unit}{Aggregation period - one of \sQuote{D} (daily), \sQuote{M} (monthly), \sQuote{Q} (quarterly), \sQuote{A} (annualy) or \sQuote{O} (for multi-year aggregation).}
  \item{mult}{Number of years, only used if \code{unit} is \sQuote{O}.}
  \item{begindt}{Beginning date of the analysis.  Defaults to earliest date in the dataset.  Should be specified with the \code{ISOdate} function.}
  \item{enddt}{Ending date for the analysis.  Defaults to the latest date in the data. }
  \item{npass}{Not yet implemented.}
  \item{smoothing}{Specifies whether or not exponential smoothing is applied to the intermediate estimates during the iterative solution process.  Defaults to \code{TRUE}. }
  \item{endmonth}{Ending month of the analysis. }
  \item{plot}{Logical indicating whether a plot of the latent variable should be generated.}
  \item{verbose}{Logical indicating whether printed output should be sent to console.}
}
\value{
A list with the following elements
\item{formula}{reproduces the user call.}
\item{setup}{basic information about the options and iterative solution.}
\item{period}{list of aggregation periods.}
\item{varname}{List in order of the variables used in the analysis.}
\item{loadings}{Item-scale correlations from the final solution.  Their square is the validity estimate used in weighting.}
\item{means}{item descriptive information.}
\item{std.deviations}{item descriptive information.}
\item{latent}{Estiamted time-series.}
}

\references{
Stimson, J. A. (2018) \sQuote{The Dyad Ratios Algorithm for Estimating Latent Public Opinion: Estimation, Testing, and Comparison to Other Approaches}, Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique, 137–138(1), pp. 201–218. doi: 10.1177/0759106318761614.
}

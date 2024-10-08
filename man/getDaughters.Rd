% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_utils.R
\name{getDaughters}
\alias{getDaughters}
\title{Get details on parent/daughter ion values}
\usage{
getDaughters(srm_in)
}
\arguments{
\item{srm_in}{Input data from an MSNbase SRM object}
}
\value{
Data frame of precursor and product m/z values
}
\description{
Get details on parent/daughter ion values
}
\examples{
data <- MSnbase::readSRMData("C:/my_tq_data/my_file.mzML")
temp_df <- getDaughters(data)
}

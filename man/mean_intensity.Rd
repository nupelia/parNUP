% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_intensity.R
\name{mean_intensity}
\alias{mean_intensity}
\title{Calculate the mean intensity for each host}
\usage{
mean_intensity(data)
}
\arguments{
\item{data}{A data frame with host in the first column and intensity values in the subsequent columns}
}
\value{
A data frame with the mean intensity for each host
}
\description{
Calculate the mean intensity for each host
}
\examples{

data <- data.frame(host = c(1, 1, 2, 2, 3, 3),
                   A = c(2, 0, 0, 0, 1, 0),
                   B = c(0, 0, 0, 0, 0, 0))
mean_intensity(data)

}

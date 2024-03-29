% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unpack_unprocessedDataset.R
\name{unpack_unprocessedDataset}
\alias{unpack_unprocessedDataset}
\title{unpack_unprocessedDataset}
\usage{
unpack_unprocessedDataset(saveDir)
}
\arguments{
\item{saveDir}{(String) Directory to save the unprocessed dataset to}
}
\description{
Unpacking the unprocessed zooplankton dataset to a specific directory
             The result will be a directory named "unprocessedDataset"
             being put in the specified save directory, containing all the image files
             from the dataset that comes with this package, but before
             processing was applied
}
\examples{
\dontrun{
saveDir <- 'Users/$(whoami)/Images/ZooplanktonImages/'
unpack_unprocessedDataset(saveDir)
}
}

#' @title unpack_unprocessedDataset
#'
#' @description Unpacking the unprocessed zooplankton dataset to a specific directory
#'              The result will be a directory named "unprocessedDataset"
#'              being put in the specified save directory, containing all the image files
#'              from the dataset that comes with this package, but before
#'              processing was applied
#' @param saveDir (String) Directory to save the unprocessed dataset to
#' @examples
#' \dontrun{
#' saveDir <- 'Users/$(whoami)/Images/ZooplanktonImages/'
#' unpack_unprocessedDataset(saveDir)
#' }
#' @export
#' @importFrom utils untar
#'


unpack_unprocessedDataset <-function(saveDir) {
  # Loading the .tar file we save the images to from where this package was installed
  pathToUnprocessedTarFile <- system.file('extdata', 'unprocessedDataset.tar', package='ZooID')
  untar(pathToUnprocessedTarFile, exdir=saveDir)

}
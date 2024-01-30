#' @title unpack_processedDataset
#'
#' @description Unpacking the processed zooplankton dataset to a specific directory.
#'              The result will be a directory named "processedDataset"
#'              being put the specified save directory, containing all the image files
#'              from the processed dataset that comes with this package
#' @param saveDir (String) Directory to save the processed dataset to
#' @examples
#' \dontrun{
#' saveDir <- 'Users/$(whoami)/Images/ZooplanktonImages/'
#' unpack_processedDataset(saveDir)
#' }
#' @export
#' @importFrom utils untar
#'


unpack_processedDataset <-function(saveDir) {
  # Loading the .tar file we save the images to from where this package was installed
  pathToProcessedTarFile <- system.file('extdata', 'processedDataset.tar', package='ZooID')
  untar(pathToProcessedTarFile, exdir=saveDir)
}
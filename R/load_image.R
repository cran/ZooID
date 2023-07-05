#' @title load_image
#'
#' @description Loads a Zooplankton image, so that they can be segmented and then classified
#' @param filePath A string that contains the path to a .jpg, .jpeg, .png, or .tiff image to be loaded
#' @return image An image (in greyscale) that corresponds to the filepath specified
#' @examples
#' filepath <- 'Users/$(whoami)/Images/ZooplanktonImages/ostracod1.png'
#' ostracod1Image <- load_image(filepath)
#' @export
#' @importFrom opencv "%>"

load_image <- function(filePath) {

  testStr = "This is just a template string till I add the actual functionality in here"
  return(testStr)
}

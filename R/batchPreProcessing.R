#' @title batchPreProcessing
#'
#' @description Preprocessing a directory of zooplankton images
#' @param zpFileDir Directory full of zooplankton images to extract
#' @param zpSaveDir Directory to save the extracted images to
#' @param minPixels Integer specifying the minimum number of pixels allowed in an extracted image
#'                  Any extracted image with fewer pixels will not be saved
#'                  [Default:  5000 (~70 by 70 pixels)]
#' @param imageExtension String specifying the extension the images will have (must include ".")
#'                       [Default: ".jpg"]
#' @param noRepeats Boolean variable to decide whether to ignore images that are already present in
#'                  the save dir, or not [Default: TRUE]
#' @param verbose Boolean variable indicating whether the script should print info to console
#'                [Default: TRUE]
#' @examples
#' \dontrun{
#' zpFileDir <- 'Users/$(whoami)/Images/ZooplanktonImages/ostracod'
#' zpSaveDir <- 'Users/$(whoami)/Images/ExtractedImages/ostracod'
#' batchPreProcessing(zpFileDir, zpSaveDir)
#' }
#' @export
#' @importFrom magick image_write image_data

batchPreProcessing <-function(zpFileDir, zpSaveDir, minPixels=5000, imageExtension=".jpg", noRepeats=TRUE, verbose=TRUE) {
  
  # Making sure the directory paths end in a "/" character for parsing reasons
  lastChar <- substr(zpFileDir, nchar(zpFileDir), nchar(zpFileDir))
  if(lastChar != "/"){
    zpFileDir <- paste(zpFileDir, "/", sep="")
  }
  
  lastChar <- substr(zpSaveDir, nchar(zpSaveDir), nchar(zpSaveDir))
  if(lastChar != "/"){
    zpSaveDir <- paste(zpSaveDir, "/", sep="")
  }
  
  # Listing the files but not the sub-directories in the specified directory
  # Code taken from: https://stackoverflow.com/questions/22069095/r-get-list-of-files-but-not-of-directories #
  zpImageFilenames <- setdiff(list.files(zpFileDir), list.dirs(zpFileDir,recursive = FALSE, full.names = FALSE))
  
  
  for(targZpImageFname in zpImageFilenames){
    # Parsing out the image filenames sans the extension
    # Code stolen from: https://stackoverflow.com/questions/14173754/splitting-a-file-name-into-name-extension #
    splitStrList <- strsplit(targZpImageFname, imageExtension)[[1]]
    
    # Creating new filename that appends "_extracted" before the extension
    targZpImageExtFname <- paste(splitStrList[1], "_extracted", imageExtension, sep="")
    # Further messing with paths to get the right directories for the image to be saved/loaded from
    targZpImageExtFullPath <- paste(zpSaveDir, targZpImageExtFname, sep="")

    targZpImageFullPath <- paste(zpFileDir, targZpImageFname, sep="")
    
    # Checking to see if the image exists in the save directory already
    saveImageAlreadyExists <- file.exists(targZpImageExtFullPath)
    # If the file image does exist, or noRepeats is set to FALSE, 
    # then we go ahead and extract the image
    if(!(saveImageAlreadyExists & noRepeats)){
      # Extracting the image
      targZpExtractedImage <- imagePreProcessing(targZpImageFullPath)
      
      # Checking to make sure the image was not corrupted (otherwise skip this image)
      if(!is.null(targZpExtractedImage)){
        # Checking to see if the image is large enough to be worth saving
        imageData <- image_data(targZpExtractedImage)
        numPixelsInImage <- dim(imageData)[2] * dim(imageData)[3]
        if(!(numPixelsInImage <= minPixels)){
          # Saving the extracted image
          image_write(targZpExtractedImage, path=targZpImageExtFullPath)
          if(verbose){
            debugStr <- paste("Saved the extracted version of: '", targZpImageFullPath, "' to: '", targZpImageExtFullPath, "'", sep="")
            message(debugStr)
          }
        }else{
          if(verbose){
            # Telling the user that no large enough image could be extracted for this particular file
            debugStr <- paste("For the file: '", targZpImageFullPath, "', the algorithm found an extracted image that was too small, and therefore didn't save it", sep="")
            warning(debugStr)
          }
        }
    
    }
    }
    
  }
  
}
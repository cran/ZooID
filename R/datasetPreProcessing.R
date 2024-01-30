#' @title datasetPreProcessing
#'
#' @description Preprocessing a directory structure of zooplankton images, and saving the resultant images
#'              to another directory structure
#' @param zpFileDir Directory of subdirectories containing zooplankton images
#' @param zpSaveDir Directory to setup the subdirectory structure for the extracted images
#' @param minPixels Integer specifying the minimum number of pixels allowed in an extracted image
#'                  See "batch_imagePreProcessing" for details [Default: 5000]
#' @param imageExtension String specifying the extension the images will have (must include ".")
#'                      [Default: ".jpg"]
#' @param verbose Boolean variable indicating whether the script should print info to console
#'                [Default: TRUE]
#' @examples
#' \dontrun{
#' zpFileDir <- 'Users/$(whoami)/Images/ZooplanktonImages'
#' zpSaveDir <- 'Users/$(whoami)/Images/ExtractedImages'
#' batchPreProcessing(zpImgDir, zpSaveDir)
#' }
#' @export
#' 

datasetPreProcessing <- function(zpFileDir, zpSaveDir, minPixels=5000, imageExtension=".jpg", verbose=TRUE){
  # Making sure the directory paths end in a "/" character for parsing reasons
  lastChar <- substr(zpFileDir, nchar(zpFileDir), nchar(zpFileDir))
  if(lastChar != "/"){
    zpFileDir <- paste(zpFileDir, "/", sep="")
  }
  
  lastChar <- substr(zpSaveDir, nchar(zpSaveDir), nchar(zpSaveDir))
  if(lastChar != "/"){
    zpSaveDir <- paste(zpSaveDir, "/", sep="")
  }
  
  if(!dir.exists(zpSaveDir)){
    # If the save directory does not exist then create it
    if(verbose){
      message(paste("Directory: '", zpSaveDir, "' does not exist, so we're creating it",  sep=""))
    }
    dir.create(zpSaveDir, showWarnings = TRUE)
  }
  
  
  targDirStructure <- list.dirs(path=zpFileDir, full.names=TRUE, recursive=TRUE)
  
  # Removing the first element since its just "zpFileDir" listed again
  targDirStructure <- targDirStructure[-1]
  
  # Seeing if the directory structure already exists for the save image directory
  for(targSubDir in targDirStructure){
    # Removing the first bit of targSubDir, since this is specific to zpFileDir, and doesn't need to be copied
    extractedTargSubDir <- unlist(strsplit(targSubDir, split=zpFileDir, fixed=TRUE))[2]
    
    
    subdirPath_SaveDir <- paste(zpSaveDir, extractedTargSubDir, sep="")
    if(!dir.exists(subdirPath_SaveDir)){
      if(verbose){
        message(paste("Directory: '", subdirPath_SaveDir, "' does not exist, so we're creating it",  sep=""))
      }
      dir.create(subdirPath_SaveDir, showWarnings = TRUE)
    }
    
    # Checking to see if there are any images in this subdirectory
    fileList <- setdiff(list.files(targSubDir, pattern=imageExtension), list.dirs(targSubDir, recursive = FALSE, full.names = FALSE))
    if(length(fileList > 0)){
      # Because of how the code is formatted, it will look at all files in the directory regardless of whether they are image files or
      # not, and regardless of the extension, so users should make sure that they do not include non-image files
      # in the directories they want images processed from
      batchPreProcessing(zpFileDir=targSubDir, zpSaveDir = subdirPath_SaveDir, imageExtension=imageExtension, noRepeats=TRUE, verbose=verbose)
    }
    
  }
  
  
}



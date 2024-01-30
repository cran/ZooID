#' @title imagePreProcessing
#'
#' @description Preprocessing the passed image
#' @param zpFilepath Filepath to the zooplankton image to be processed
#' @return extractedImage The section of the image extracted by the algorithm,
#'                        or NULL if the image was corrupted
#' @examples
#' \dontrun{
#' zpFilepath <- 'Users/$(whoami)/Images/ZooplanktonImages/ostracod1.png'
#' ostracod1Image <- imagePreProcessing(zpFilepath)
#' }
#' @export
#' @importFrom magick image_data image_canny image_read image_convolve image_crop
#' @importFrom stats kmeans


imagePreProcessing <-function(zpFilepath) {
  ##################################################################
                ## Loading the image as grayscale ##
  ##################################################################
  
  # Wrapping this in a try catch block so the function won't error out and break upstream 
  # code if the image is corrupted
  im <- image_read(zpFilepath)
  greyImVals <- tryCatch({
    as.integer(image_data(im, channels='gray'))
  },
  error= function(cond){
    warning("The jpg appears to be faulty, the following is the error message: ")
    warning(conditionMessage(cond))
    # Return NULL if an error happens
    return(NULL)
  })
  
  if(is.null(greyImVals)){
    message(paste("\nThe file: '", zpFilepath, "', appears to be corrupted. An error message will be printed at the end with more details\n", sep=""))
    # Returns NULL if extracting the image errored out
    return(NULL)
  }else{
    ##################################################################
                  ## Dynamically thresholding using KMeans ##
    ##################################################################
    greyKmeans <- kmeans(greyImVals, centers=2)
    
    # Getting the indices of pixels within each cluster
    lab1Inds <- which(greyKmeans$cluster %in% 1)
    lab2Inds <- which(greyKmeans$cluster %in% 2)
    
    # Converting the image to a vector so that we can set the label1 indexes to be 0
    greyImVectorized <- as.vector(greyImVals)
    
    greyImVecKMThresh <- greyImVectorized
    if(length(lab1Inds) > length(lab2Inds)){
      greyImVecKMThresh[lab1Inds] <- 0
    }else{
      greyImVecKMThresh[lab2Inds] <- 0
    }
    
    # Converting the 1d vector back into 3d array, and then back into greyscale image
    resizedGreyImAfterKMThresh <- array(greyImVecKMThresh, dim(greyImVals))
    greyImAftKMThresh <- image_read(resizedGreyImAfterKMThresh)
    # Applying Canny edge detection to the image
    imCannyAfterKmeans <- image_canny(greyImAftKMThresh, geometry="0x1+60%+90%")
    
    ##################################################################
                    ## Widening contours using kernel ##
    ##################################################################
    imageKern <- matrix(data=1, nrow=7, ncol=7)
    processedContours <- image_convolve(imCannyAfterKmeans, imageKern)
    
    ##################################################################
                        ## Bounding Box Creation ##
    ##################################################################
    # Some parts stolen from: https://www.r-bloggers.com/2020/08/image-contours-in-r/
    mat <- image_data(processedContours, channels='gray')
    mat <- drop(as.integer(mat, transpose=TRUE))
    
    # The better algorithm implemented in Python version roughly follows this paper: https://www.nevis.columbia.edu/~vgenty/public/suzuki_et_al.pdf
    nonZeroVals <- which(mat>0, arr.ind = TRUE)
    
    x <- nonZeroVals[,1]
    y <- nonZeroVals[,2]
    
    
    # Creating a bounding box instead of a convex hull
    maxX <- max(x)
    minX <- min(x)
    maxY <- max(y)
    minY <- min(y)
    
    boxMaskArray <- array(0, dim=c(dim(mat)[1], dim(mat)[2], 3))
    boxMaskArray[minX:maxX, minY:maxY, ] <- 255
    
    boxMaskImage <- image_read(boxMaskArray)
    
    
    
    # Converting to bitmap format to make life easier
    # (This might be wasteful since the mask was already formatted, 
    # but getting it to be a bitmap proved annoying so this works instead)
    bitmapIm <- image_data(im, channels='gray')
    bitmapMask <- image_data(boxMaskImage, channels='gray')
    
    # Bitwise ANDing the mask and the image, and then converting back to an image
    maskedImage <- image_read(bitmapIm & bitmapMask)
    
    ##################################################################
                        ## Image Cropping ##
    ##################################################################
    
    ind1 <- maxY - minY
    ind2 <- maxX - minX
    
    targGeometry <- paste(as.character(ind1), "x", as.character(ind2), "+", as.character(minY), "+", as.character(minX), sep="")
    
    extractedImage <- image_crop(maskedImage, geometry=targGeometry)
    
    return(extractedImage)
  }
  
}

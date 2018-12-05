#https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/


# create("Filips.ML.package")


# setwd("./cats")
# document()


#' When update to package is done (for Filips ML package): 
#' 1. Set working directory to package. 
#' 2. setwd("~/Desktop/Programmeringsprojekt/Machine Learning/Filips ML package/Filips.ML.package")
#'    document()
#' 3. Set working directory to folder above and reinstall the package with updates. 
#'    setwd("~/Desktop/Programmeringsprojekt/Machine Learning/Filips ML package")
#'    install("Filips.ML.package")
#' 

# To use the ML functions, run the code below. 

library("devtools")
library(roxygen2)

setwd("/Users/filipcornell/Desktop/Programmeringsprojekt/ML-Implementations/Filips ML package/Filips.ML.package")
document()
setwd("/Users/filipcornell/Desktop/Programmeringsprojekt/ML-Implementations/Filips ML package")
install("Filips.ML.package")
#this.dir <- dirname(parent.frame(2)$ofile) #To set it to source file location
#setwd(this.dir) #To set it to source file location
library("Filips.ML.package")


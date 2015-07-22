# Write a function that takes a directory of data files and a threshold for 
# complete cases and calculates the correlation between sulfate and nitrate 
# for monitor locations where the number of completely observed cases 
# (on all variables) is greater than the threshold. The function should 
# return a vector of correlations for the monitors that meet the threshold 
# requirement. If no monitors meet the threshold requirement, then the 
# function should return a numeric vector of length 0. A prototype of this 
# function follows

# 'directory' is a character vector of length 1 indicating
# the location of the CSV files

# 'threshold' is a numeric vector of length 1 indicating the
# number of completely observed observations (on all
# variables) required to compute the correlation between
# nitrate and sulfate; the default is 0

# Return a numeric vector of correlations

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating the location of
  ## the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the number of
  ## completely observed observations (on all variables) required to compute
  ## the correlation between nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  df = complete(directory)
  ids = df[df["nobs"] > threshold, ]$id
  corrr = numeric()
  for (i in ids) {
    
    newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
                             ".csv", sep = ""))
    dff = newRead[complete.cases(newRead), ]
    corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
  }
  return(corrr)
}
#cr <- corr("specdata", 150)
#head(cr)
#summary(cr)

#cr <- corr("specdata", 400)
#head(cr)
#summary(cr)

#cr <- corr("specdata", 5000)
#summary(cr)
#length(cr)

#cr <- corr("specdata")
#summary(cr)
#length(cr)
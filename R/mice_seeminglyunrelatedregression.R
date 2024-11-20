#' Fit a seeminglyunrelated regression to mice data and get the coefficents and covariance matrix after applying Rubin's rules

#' @param imputed_data_ is your final imputed dataset given by the mice function
#' mice
#' @param imputation_numb_ is the number of imputations conducted
#' @param system_ is the list of regression equations you want to fit
#' @param numb_covar_ is the number of covariates you have in your statistical models
#' @param method_ is the type of model you want systemfit to use. The options are the same
#' as the method option in systemfit.
#' @return data_toreturn is a list, the first object is the coefficents of the regression
#' across the imputed datasets. The second object is the covariance matrix combined across the
#' imputed datasets
#' @export
mice_seeminglyunrelatedregression <- function(
    imputed_data_,
    imputation_numb_,
    system_,
    numb_covar_,
    method_
    ){

#Split the imputed data into a list with 80 dimensions
imputed_data_list <- lapply(1:imputation_numb_, function(i)complete(imputed_data_,i))
#Each Element of the list is a dataframe with a different set of imputed values

###Make a matrix to store the coefficents
regressioncoefficents <- matrix(data=NA, nrow = numb_covar_, ncol = imputation_numb_)
covmatrices <- array(data = NA, dim = c(numb_covar_,numb_covar_,80))

##MICE and systemfit are not compatible, need to split them out into seperate dataframes
#Fit SURREG to each imputed dataset
for (i in 1:imputation_numb_){
  #Fit the seemingly unrelated regression to the current imputed dataset
  temp <- systemfit::systemfit(system_, method = method_, data = imputed_data_list[[i]])
  #Record the coefficients
  #Difference rows are different regression results
  regressioncoefficents[,i] <- temp$coefficients

  #Record the coefficient covariance matrix
  #Different covariance matrices into different 3rd dimensions of the list
  covmatrices[,,i] <- temp$coefCov

}

#Get the mean values of the coefficents
meancoefs <- matrix(data=NA, nrow = numb_covar_, ncol = 1)
for (z in 1:length(meancoefs)){
  meancoefs[z,] <- mean(regressioncoefficents[z,])
  }

#After we have the mean values, get the mean value in each position of each covariance matrix
#i.e. mean value of the variance between the treatment effects
#Within imputation covariance
meancovariance <- apply(covmatrices,c(1,2),mean)
#Get the difference between the coefficent and mean values for that coefficent
imputedcoefdifferencesfrommean <-  sweep(regressioncoefficents,1,meancoefs, FUN = "-")


#Apply Rubin's Rules to get the imputed covariance
#based on Enders 2010 Applied Missing Data Analyis, pg 234

#For the first datset calculate fitted values for the 1st dataset minus the mean values across all datasets,
#matrix multplied by the transpose of this same vector
Qdiff <- imputedcoefdifferencesfrommean[,1]%*%t(imputedcoefdifferencesfrommean[,1])
#Loop for everything after the first dataset
for (i in 2:imputation_numb_){
  #calculate the matrix of the differences between the fitted coefficents for this dataset from the mean of the fitted values
  #and add it onto the existing matrix
  Qdiff <- Qdiff + imputedcoefdifferencesfrommean[,i]%*%t(imputedcoefdifferencesfrommean[,i])
}

#Multiply the summed matrices by the number of imputations minus 1, gives the between-imputation variance
Vb <- (1/(imputation_numb_-1))*Qdiff

#Rubin's rules, adding the mean covariance to 1+1/numeber of imputations by
imputedcovariance <- meancovariance + (1+1/imputation_numb_)*Vb

data_toreturn <- list(meancoefs, imputedcovariance)

return(data_toreturn)

}

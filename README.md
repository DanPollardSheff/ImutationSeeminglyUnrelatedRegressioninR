Package: RegressionandMultipleImputation
Title: Fits regressions in different imputed datasets and returns the coefficents and covariance matrix across all of the imputed data sets
Version: 0.0.0.1
Authors@R: 
    person("Dan", "Pollard", , "d.j.pollard@sheffield.ac.uk", role = c("cre"),
           comment = c(ORCID = "0000-0001-5630-0115"))
Description: It is a function that use multiple imputed data from the mice package as an input, and it fits a regression to each imputed data set (currently only regressions in the systemfit pacakge work), and applies Rubin's rules to produce the coefficents and covariance matrix across the imputed datasets. This is useful for health economic analyses of study data were simulating study effects on costs and QALYs is necessary for the health economic analsyes. This could also be applied in other areas too though. 
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Imports: systemfit (>= 1.1)
Suggests: 
    knitr,
    rmarkdown,
    mice
VignetteBuilder: knitr

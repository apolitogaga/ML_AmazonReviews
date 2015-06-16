install.packages('devtools')
devtools::install_github('rstudio/shinyapps')
shinyapps::setAccountInfo(name='apolitogaga',
                          token='2EE7EE03D1D464654B98138655E4BB15',
                          secret='4icOY2Bl67ysV8uu+OJ4pkUix6CFNX++s8YwC4FV')

library(shinyapps)
shinyapps::deployApp('/Users/hectorapolorosalespulido/Documents/dev/github/ML_AmazonReviews/')
install.packages("shiny")
library(shiny)

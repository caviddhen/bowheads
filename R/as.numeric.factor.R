#' @title as.numeric.factor
#' @description super simple helper function to convert factors to numeric
#' @return factors as numeric
#' @param x to convert to numeroc
#' @author Silja Zimmermann
#' @export

as.numeric.factor <- function(x) {

  as.numeric(levels(x))[x]
  }

## Functions to fortify simulation results into ggplot2-friendly,
## "tidy" data.frames: http://vita.had.co.nz/papers/tidy-data.pdf

## Note, I'm implementing these as I encounter new simresult types. I
## anticipate refactoring these as common strategies emerge


fortify.oprobitsim <- function(result, counterfactual, keep=NULL) {
  ## Takes an oprobitsim result and counterfactual object, and
  ## optionally a vector manually defining which counterfactual
  ## variables to retain; returns an easily plottable dataframe.

  if (!is.null(keep)) {
    cfgrid <- subset(counterfactual$x, select=keep)
  } else {
    ## Get the terms we used to originally build the cf
    cfset <- ! mapply(identical, counterfactual$x, counterfactual$xpre)
    cfgrid <- subset(counterfactual$x, select=cfset)
  }
  pe_wide <- cbind(cfgrid, result$pe)
  pe <- melt(pe_wide, id.vars=names(cfgrid),
             value.name="pe", variable.name="category")
  upper <- melt(result$upper, value.name="upper")$upper
  lower <- melt(result$lower, value.name="lower")$lower
  cbind(pe, upper, lower) # Return tidy data
}

fortify.logitsim <- function(result, counterfactual, keep=NULL) {
  ## Takes a logitsim result and counterfactual object, and optionally
  ## a vector manually defining which counterfactual variables to
  ## retain; returns an easily plottable dataframe.

  if (!is.null(keep)) {
    cfgrid <- subset(counterfactual$x, select=keep)
  } else {
    ## Get the terms we used to originally build the cf
    cfset <- ! mapply(identical, counterfactual$x, counterfactual$xpre)
    cfgrid <- subset(counterfactual$x, select=cfset)
  }
  cbind(cfgrid, result) # Return tidy data
}

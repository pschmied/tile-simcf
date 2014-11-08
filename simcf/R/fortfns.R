## Functions to fortify simulation results into ggplot2-friendly,
## "tidy" data.frames: http://vita.had.co.nz/papers/tidy-data.pdf

## Note, I'm implementing these as I encounter new simresult types. I
## anticipate refactoring these as common strategies emerge

######################################
## Simulation results that are "wide"
######################################

fortify.widesim <- function(result, counterfactual, keep=NULL) {
  ## Takes a wide-format result and counterfactual object, and
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

fortify.oprobitsim <- function(result, counterfactual, keep=NULL) {
  ## Fortifies an oprobitsim result
  fortify.widesim(result, counterfactual, keep=NULL)
}

######################################
## Simulation results that are "long"
######################################

fortify.longsim <- function(result, counterfactual, keep=NULL) {
  ## Takes a long-format result and counterfactual object, and
  ## optionally a vector manually defining which counterfactual
  ## variables to retain; returns an easily plottable dataframe.

  if (!is.null(keep)) {
    cfgrid <- subset(counterfactual$x, select=keep)
  } else {
    ## Get the terms we used to originally build the cf
    cfset <- ! mapply(identical, counterfactual$x, counterfactual$xpre)
    cfgrid <- subset(counterfactual$x, select=cfset)
  }
  cbind(cfgrid, result) # Return tidy data
}

fortify.logitsim <- function(result, counterfactual, keep=NULL) {
  ## fortifies a logitsim result
  fortify.longsim(result, counterfactual, keep=NULL)
}

fortify.loglinsim <- function(result, counterfactual, keep=NULL) {
  ## fortifies a loglinsim result
  fortify.longsim(result, counterfactual, keep=NULL)
}

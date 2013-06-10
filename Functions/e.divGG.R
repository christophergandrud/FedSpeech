#' Function for creating nonparamentric multiple change point plots with estimates from the ecp package.
#'
#' @param data A data frame with the covariates and time variable.
#' @param Vars A character vector listing the names of the variates from \code{data} to include in the nonparametric multiple change point analysis.
#' @param TimeVar A character string naming the time variable in \code{data}. 
#' @param sig.lvl The level at which to sequentially test if a proposed change point is statistically significant.
#' @param eps The uniform error bound used in the implementation of the permutaiton test as outlined in Gandy (2009).
#' @param half A constant used to control the epsilon spending rate. Futher details can be found in Gandy (2009).
#' @param R The maximum number of random permutations to use in each iteration of the permutaiton test. The permutation test p-value is calculated usign the method outlined in Gandy (2009).
#' @param k Number of change point locations to estimate, surpressing the permutation based testing. If \code{k=NULL} then only the statistically significant estimated change points are returned.
#' @param PlotVars A character string of the variables from the analysis to plot.
#' @param Grid Logical. If \code{TRUE} then each variable will be given its own plot. If \code{FALSE} then values from all of the variables in \code{Var} will be plotted on the same figure.
#' @param palette If a string, it will use that named palette. If a number, will index into the list of palettes of appropriate type,
#' @param leg.name A character string. If \code{Facet = FALSE}, it allows you to specify the legend name.
#' 
#' @return a ggplot2 object 
#' 
#' @details Uses \code{\link{e.divisive}} to run a nonparametric multiple chang point analysis (James and Matteson, 2013) on variables in a data frame. It then uses \code{\link{ggplot2}} to plot the variable values with vertical dashed lines indicating the estimated change points. 
#' 
#' @source James N.A., Matteson D.S. (2013). ecp: An R Package for Nonparametric Multiple Change Point Analysis of Multivariate Data.
#' 

e.divGG <- function(data, Vars, TimeVar, sig.lvl = 0.05, R = 199, eps = 1e-3, half = 1000, k = NULL, min.size = 30, alpha = 1, PlotVars = NULL, Grid = TRUE, palette = "Set1", leg.name = "")
{
  # Load required packages
  require(ecp)
  require(reshape2)
  require(ggplot2)
  require(gridExtra)
  
  # Create T x d matrix
  DataMatrix <- as.matrix(data[, Vars])
  
  # Turn of faceting if DataMatrix only has 1 column
  if (ncol(DataMatrix) == 1){
    Facet <- FALSE
  }
  
  # Estimate change points
  CP <- e.divisive(X = DataMatrix, sig.lvl = sig.lvl, R = R, eps = eps, half = half, 
                   k = k, min.size = min.size, alpha = alpha)

  # Extract change points
  CPEstimates <- CP$estimates
  # Remove first and last points (these are the first and last values of the matrix)
  CPEstSub <- CPEstimates[c(-1, -length(CPEstimates))]
  if (length(CPEstSub) == 0){
    stop(paste("No change points found at the", sig.lvl, "significance level."))
  }
  
  #Find corresponding TimeVar value
  CPTimes <- as.POSIXct(data[CPEstSub, TimeVar])
  
  # Report estimates
  CPMessage <- lapply(CPTimes, function(x) paste(x, "\n"))
  message("Change points estimated at:")
  message(CPMessage)
  
  # Melt data frame so that it can be plotted
  if (!is.null(PlotVars)){
    DataSub <- data[, c(TimeVar, PlotVars)]
    DataMolten <- melt(data = DataSub, id.vars = TimeVar, measure.vars = PlotVars)
  } 
  else if (is.null(PlotVars)){
    DataSub <- data[, c(TimeVar, Vars)]
    DataMolten <- melt(data = DataSub, id.vars = TimeVar, measure.vars = Vars)  
  }
          
  # Clean pre plotting
  names(DataMolten) <- c("Time", "GroupVar", "Value")
  DataMolten$Time <- as.POSIXct(DataMolten$Time)
  DataMolten <- merge(DataMolten, CPTimes, all = TRUE)
  names(DataMolten) <- c("Time", "GroupVar", "Value", "Lines")
  DataMolten$Lines[DataMolten$Time != DataMolten$Lines] <- NA

  # Plot
  if (length(unique(DataMolten$GroupVar)) == 1){
    ggplot(data = DataMolten, aes(x = Time, y = Value)) +
          geom_line() +
          geom_vline(aes(xintercept = as.numeric(Lines)), 
                      linetype = "longdash", colour = "#DE2D26") +
          xlab("") + ylab("") +
          theme_bw()
  } 
  else if (length(unique(DataMolten$GroupVar)) > 1) {
    if (Grid == FALSE){
      ggplot(data = DataMolten, aes(x = Time, y = Value, group = GroupVar, 
                                    colour = GroupVar)) +
              geom_line() +
              geom_vline(aes(xintercept = as.numeric(Lines)), linetype = "longdash") +
              scale_colour_brewer(palette = palette, name = leg.name) +
              xlab("") + ylab("") +
              theme_bw()
    }
    else if (Grid == TRUE){
      eachVar <- unique(DataMolten$GroupVar)
      p <- list()
      for (i in eachVar){
        SubData <- subset(DataMolten, GroupVar == i)
        p[[i]] <-   ggplot(data = SubData, 
                           aes(x = Time, y = Value)) +
                          geom_line() +
                          geom_vline(aes(xintercept = as.numeric(Lines)), 
                                     linetype = "longdash", colour = "#DE2D26") +
                          xlab("") + ylab("") + ggtitle(paste(i, "\n")) +
                          theme_bw()
      }
      do.call(grid.arrange, p)
    }
  }
}
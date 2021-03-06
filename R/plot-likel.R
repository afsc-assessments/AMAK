#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M list object created by read_admb function
#' @return dataframe of spawning biomass
#' @export
#' 
.get_like_df <- function(M)
{
    n <- length(M)
    mdf <- NULL
    for (i in 1:n)
    {
        A <- M[[i]]
        df <- data.frame(type = as.factor(1:21))
        df$Model <- names(M)[i]
        df$NLL   <- A$Like_comp
        A <-M[[1]]
        print(A$surv_like)
        print( A$cpue_like)
        print( A$avo_like)
        print( A$sel_like)
        print( A$sel_like_dev)
        print( A$age_like)
        print( A$rec_like)
        mdf     <- rbind(mdf, df)
    }
    return(mdf)
}


#' Plot predicted spawning stock biomass (ssb)
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M List object(s) created by read_admb function
#' @param xlab the x-label of the figure
#' @param ylab the y-label of the figure
#' @param ylim is the upper limit of the figure
#' @param alpha the opacity of the ribbon
#' @return Plot of model estimates of spawning stock biomass 
#' @export
#' 
plot_likes <- function(M, xlab = "Natural Mortality", ylab = "relative -ln Likelihood", ylim = NULL, xlim=NULL, alpha = 0.1,legend=TRUE)
{
    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")
    
    mdf <- .get_like_df(M)
    
    p <- ggplot(mdf) + labs(x = xlab, y = ylab)
    
    if (!is.null(xlim))
        p <- p + xlim(xlim[1], xlim[2])        

    if (is.null(ylim))
    {
        p <- p + expand_limits(y = 0)
    } else {
        p <- p + ylim(ylim[1], ylim[2])        
    }
    if (!legend)
        p <- p + theme(legend.position="none")
    
    p <- p + geom_line(aes(x = M, y = NLL, col=type)) 
    
    print(p + .THEME)
}
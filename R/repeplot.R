
## Function repeplot
##
##' Function \code{repeplot} plots the random effects parameter
##' estimates from one or two fitted \code{\link{asreml}} models.  If
##' two models (termed the 'current' and 'previous' models) are
##' specified, the plot compares the parameter estimates from the
##' current model to those from the previous.
##'
##' Function \code{repeplot} plots the random effects parameter
##' estimates from one or two fitted \code{\link{asreml}} model
##' objects.  The random effects parameter labels that are used are
##' those given in the \code{varcomp} component of the object returned
##' by \code{\link{summary.asreml}}.  'Gamma' values (see below), or
##' unscaled parameter values can be plotted.  Residual variance
##' parameters are highlighted on the plot via vertical dashed lines
##' when \code{rlines} is \code{TRUE} (the default), and the position
##' of the origin is highlighted by a vertical grey line when
##' \code{zline} is \code{TRUE} (the default).
##'
##' Regarding 'gamma' values: when a linear mixed model contains a
##' single residual variance \code{\link{asreml}} will parameterise
##' other variances in the model \emph{relative} to the residual
##' variance.  The resulting scale factors are referred to as 'gamma'
##' parameters.  The \code{varcomp} component of the object returned
##' by \code{\link{summary.asreml}} reports these gamma values, along
##' with the unscaled component values, and \code{repeplot} allows you
##' to plot either, via the argument \code{gammas} (default value
##' \code{TRUE}).  Note that no such scaling is performed when a model
##' contains more than one residual variance parameter, and the
##' reported gamma estimates are identical to the unscaled component
##' estimates.  For further details on how \code{\link{asreml}}
##' parameterises the linear mixed model, see the \pkg{asreml} package
##' manual.
##'
##' Parameters that lie 'on the boundary' (and hence indicated by
##' "Boundary" in the \code{varcomp$condition} component of the
##' summary object), are highlighted on the plot by a short vertical
##' bar through the plotted parameter.
##'
##' When two models are specified in the \code{repeplot} call, these are
##' termed the 'current' and 'previous' models. \code{repeplot} aims to
##' highlight the changes in the parameter estimates in moving from
##' the previous to the current model.
##'
##' As such, parameters that are present in one model and not the
##' other are plotted in a different colour to those parameters
##' present in both models.  Parameters present in the \emph{previous}
##' model, but not the \emph{current} model, are also overplotted with
##' a cross, highlighting their absence from the current model.
##'
##' For parameters present in both models, the values for the current
##' model are plotted, and horizontal line segments are used to
##' indicate how these parameters have changed between the previous
##' and current models.  Note that these line segments are plotted
##' \emph{behind} the corresponding points, so if parameter changes
##' are very small, the line segments may be too short to be seen.
##'
##' Caveat: note that a change in the random effects model
##' specification between 'previous' and 'current' models, can
##' sometimes result in an equivalent parameter being given
##' \emph{different labels} by \code{\link{asreml}} between the two
##' models.  \code{repeplot} cannot detect such cases, and hence will
##' plot the affected parameter as two distinct parameters - one
##' removed from the previous model, the other added to the current.
##'
##' Not all random effects parameters are variances, and as such may
##' have different scales to the variances.  Autocorrelation
##' estimates, for example, lie between -1 and 1, and have no
##' relationship to the scale of the variances.  \code{repeplot} does
##' not attempt to distinguish between parameters on different scales,
##' and if you wish to 'zoom in' on a particular range of parameter
##' values to see the estimates clearly, you can do so using the usual
##' \pkg{ggplot2} \code{xlim} function syntax,
##' e.g. \code{repeplot(a1,a0) + xlim(-1,1)}.
##'
##' \code{repeplot} requires that the \pkg{asreml}, \pkg{dplyr} and
##' \pkg{ggplot2} packages be installed.  \code{repeplot} returns a
##' \code{\link{ggplot}} object, and this object can be further
##' modified using the usual \pkg{ggplot2} package
##' syntax. \code{repeplot} uses the \code{\link{theme_bw}} theme by
##' default.
##'
##' @title Plot \pkg{asreml} random model parameters
##' @param curr \code{\link{asreml}} object: The 'current'
##' \code{\link{asreml}} model for which the estimated random effects
##' parameters are to be plotted.
##' @param prev \code{\link{asreml}} object, or \code{NULL}: If a
##' 'previous' \code{\link{asreml}} model is specified via
##' \code{prev}, the plot will compare random effects parameters
##' between the previous and current models.  Default is \code{NULL}.
##' See the Details section for more.
##' @param gammas logical: Should the plot display 'gamma' values or
##' actual components?  Default is \code{TRUE}. See the Details
##' section for more.
##' @param size numeric: The size parameter for plotting points.
##' Equivalent to the \code{\link{geom_point}} '\code{size}'
##' parameter.
##' @param lwd numeric: The width parameter for line segments
##' indicating changes in parameters common to the current and
##' previous models.  Equivalent to the \code{\link{geom_segment}}
##' '\code{size}' parameter.
##' @param zline logical: Display a grey vertical line (the 'zero
##' line') passing through the x-axis origin? Default is \code{TRUE}.
##' @param rlines logical: Display dashed vertical line(s) passing
##' through the residual variance parameter estimate(s)? Default is
##' \code{TRUE}.
##' @param bw logical: Produce a black-and-white version of the plot?
##' Default is FALSE.  If \code{bw=TRUE}, parameters present in both
##' models are plotted as grey points and parameters in only one model
##' are plotted as unfilled circles.
##' @param pointcol Specifies the point colour for parameters present
##' in both models.  Default is \code{"#0072B2"} (a shade of blue), or
##' \code{"grey50"} when \code{bw=TRUE}.
##' @param altcol Specifies the point colour for parameters present in
##' only one model.  Default is \code{"#E69F00"} (a shade of orange)
##' when \code{bw=FALSE}.  When \code{bw=TRUE}, this parameter
##' controls the colour of the point \emph{outline}, and the default
##' value is \code{"black"}.
##' @return A \pkg{ggplot2}-package \code{ggplot} object.
##' @author Alexander Zwart (alec.zwart at csiro.au)
##' @export
##' @import ggplot2
##' @import asreml
##'
repeplot <- function(curr,prev=NULL,gammas=TRUE,size=6,lwd=2,
                   zline=TRUE,rlines=TRUE,
                   bw=FALSE,
                   pointcol=ifelse(bw,"grey50","#0072B2"),
                   altcol=ifelse(bw,"black","#E69F00"))
  {
    stopifnot(is.numeric(size))
    stopifnot(is.numeric(lwd))
    stopifnot(class(curr)=="asreml")
    if (!is.null(prev)) stopifnot(class(prev)=="asreml")
    ##
    cr <- summary(curr)$varcomp[,c(1,2,5)]
    cr$constraint=as.character(cr$constraint)
    cr$Term <- row.names(cr)
    rownames(cr) <- NULL
    termLevs <- rev(cr$Term)
    ## Extract the residual variance estimates from the _current_
    ## model - these will be needed later...
    cr_rv <- cr[grepl("!variance$",cr$Term),]
    ##
    if (!is.null(prev))  # ...and, if prev model is specified...
      {
        pr <- summary(prev)$varcomp[,c(1,2,5)]
        pr$constraint=as.character(pr$constraint)
        pr$Term <- row.names(pr)
        rownames(pr) <- NULL
        ##
        inboth <- dplyr::inner_join(x=cr,y=pr,by="Term")
        ## Note: '.x' => current, '.y' => previous
        removed <- pr[!(pr$Term %in% inboth$Term),]
        added <- cr[!(cr$Term %in% inboth$Term),]
        if (nrow(removed) == 0) removed <- NULL
        if (nrow(added) == 0) added <- NULL
        if (nrow(inboth) == 0) inboth <- NULL  ## Test this line!
        ##
        if (!is.null(removed)) termLevs <- c(termLevs,rev(removed$Term))
      } else {
        added <- cr
        removed <- NULL
        inboth <- NULL
      }
    ##
    plt <- ggplot() +
     ylab("Random effects parameter") +
      xlab(ifelse(gammas,
                  "Gamma value estimate",
                  "Parameter value estimate")) +
                   geom_vline(aes(xintercept=cr_rv[[ifelse(gammas,
                                      "gamma",
                                      "component")]]),
                              linetype=2,size=0.6,color="grey60") +
                               geom_vline(aes(xintercept=0),
                                          size=1,color="grey80")
    ##
    if (!is.null(added))
      {
        ## Re color argument: if there is no previous model, plot
        ## points in colour 'pointcol'.  Else, plot in colour
        ## 'altcol'.
        added$Term <- factor(added$Term,levels=termLevs)
        plt <- plt + geom_point(aes_string(x=ifelse(gammas,
                                               "gamma",
                                               "component"),
                                           y="Term"),
                                data=added,
                                size=size,
                                color=ifelse(is.null(prev),
                                    pointcol,altcol),
                                fill="white", ## redundant unless bw=TRUE
                                stroke=1.5, ## unused unless bw=TRUE
                                shape=ifelse(!is.null(prev) & bw,21,16))
        ##
        if (any(added$constraint=="Boundary")) {
          added_b <- added[added$constraint=="Boundary",]
          plt <- plt + geom_point(aes_string(x=ifelse(gammas,
                                                 "gamma",
                                                 "component"),
                                             y="Term"),
                                  data=added_b,
                                  size=size*1.3,
                                  shape="|")
          ## size=size*1.3, shape="|" is not ideal, but it will do for
          ## now.  The unicode option is nicer, but it's not clear that
          ## the symbol will be available to all users...
        }
      }  ## End handling of 'added'
    ##
    if (!is.null(removed))
      {
        removed$Term <- factor(removed$Term,levels=termLevs)
        plt <- plt +
         geom_point(aes_string(x=ifelse(gammas,
                                   "gamma",
                                   "component"),
                               y="Term"),
                    data=removed,
                    size=size,
                    color=altcol,
                    fill="white", ## unused unless bw=TRUE
                    stroke=1.5, ## unused unless bw=TRUE
                    shape=ifelse(bw,21,16)) +
                     geom_point(aes_string(x=ifelse(gammas,
                                               "gamma",
                                               "component"),
                                           y="Term"),
                                data=removed,
                                size=size,
###                   shape="\u2715",
                                shape=4,
                                stroke=1.5)
        ##
        if (any(removed$constraint=="Boundary")) {
          removed_b <- removed[removed$constraint=="Boundary",]
          plt <- plt + geom_point(aes_string(x=ifelse(gammas,
                                                 "gamma",
                                                 "component"),
                                             y="Term"),
                                  data=removed_b,
###                                shape="\u2503",
                                  shape="|",
                                  size=size*1.3)
          ## size=size*1.3, shape="|" is not ideal, but it will do for
          ## now.  The unicode option is nicer, but I'm not sure
          ## whether the symbol will be available to all users?
        }
      }  ## End handling of 'removed'
    ##
    if (!is.null(inboth))
      {
        inboth$Term <- factor(inboth$Term,levels=termLevs)
        plt <- plt +  ## Point overplots line
         geom_segment(aes_string(x=ifelse(gammas,
                                     "gamma.x",
                                     "component.x"),
                                 xend=ifelse(gammas,
                                     "gamma.y",
                                     "component.y"),
                                 y="Term",
                                 yend="Term"),
                      data=inboth,
                      size=lwd) +
                       geom_point(aes_string(x=ifelse(gammas,
                                                 "gamma.x",
                                                 "component.x"),
                                             y="Term"),
                                  data=inboth,
                                  size=size,
                                  color=pointcol)
        ##
        if (any(inboth$constraint.x=="Boundary")) {
          inboth_b_x <- inboth[inboth$constraint.x=="Boundary",]
          plt <- plt + geom_point(aes_string(x=ifelse(gammas,
                                                 "gamma.x",
                                                 "component.x"),
                                             y="Term"),
                                  data=inboth_b_x,
                                  size=size*1.3,
                                  shape="|")
          ## size=size*1.3, shape="|" is not ideal, but it will do for
          ## now.  The unicode option is nicer, but it's not clear that
          ## the symbol will be available to all users...
        }
        if (any(inboth$constraint.y=="Boundary")) {
          inboth_b_y <- inboth[inboth$constraint.y=="Boundary",]
          plt <- plt + geom_point(aes_string(x=ifelse(gammas,
                                                 "gamma.y",
                                                 "component.y"),
                                             y="Term"),
                                  data=inboth_b_y,
                                  size=size*1.3,
                                  shape="|")
          ## size=size*1.3, shape="|" is not ideal, but it will do for
          ## now.  The unicode option is nicer, but it's not clear that
          ## the symbol will be available to all users...
        }
      }  ## End handling of 'inboth'
    ## Finishing touches:
    plt <- plt + theme_bw()
    ##
    return(plt)
  }


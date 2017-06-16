
This is a method - define the generic and default!

require(asreml)
require(ggplot2)
require(dataUtilz)

dfr <- asreml.read.table("C:\\00-Work\\Research\\Food Futures\\Analyses_2016\\Yanco_8way_2015_Field\\R\\BaseModels\\Univariate_23.28_no_covars\\Yanco_8way_2015_Master_Cleaned.csv",
                  header=TRUE, sep=',')

a0 <- asreml(fixed = aerialct_2309_0900 ~ Type - 1,
             random = ~ id + BAY + BLOCK,
             rcov = ~ at(BAY):(ROW):(RANGE),
             data = dfr,
             na.method.X = 'include')

## Note - I have removed BAY cause I need an example of removing a
## parameter in a1!
a1 <- asreml(fixed = aerialct_2309_0900 ~ Type - 1,
             random = ~ id + BLOCK + at(BAY):ROW,
             rcov = ~ at(BAY):(ROW):(RANGE),
             data = dfr,
             na.method.X = 'include')

curr <- a1
prev <- a0
size <- 6
lwd <- 2
gammas=TRUE
tritanope=FALSE

summary(a0)$varcomp[,c(1,2,5)]
summary(a1)$varcomp[,c(1,2,5)]


##' Plot the random effects parameter estimates from one or two fitted
##' \code{\link{asreml}} models.  If two models (termed the 'current' and
##' 'previous' models) are specified, the plot compares the parameter
##' estimates from the current model to those from the previous.
##'
##' The \code{asreml} method \code{replot} plots the random effects
##' parameter estimates from one or two fitted \code{link{asreml}}
##' model objects.  The random effects parameter labels that are used
##' are those given in the \code{varcomp} component of the object
##' returned by \code{\link{summary.asreml}}.  Gamma values (the
##' default), or unscaled parameter values can be plotted.  Residual
##' variance parameters are highlighted on the plot via vertical
##' dashed lines, and the position of the origin is highlighted by a
##' vertical grey line.
##'
##' Parameters that lie 'on the boundary' (and hence indicated by
##' "Boundary" in the \code{varcomp$condition} component of the asreml
##' summary object), are highlighted on the plot by a short vertical
##' bar through the plotted parameter.
##'
##' When two models are specified in the \code{replot} call, these are
##' termed the 'current' and 'previous' models, and \code{replot} aims
##' to show the changes in the parameter estimates in going from the
##' previous to the current model.
##'
##' Hence, parameters that are present in one model and not the other
##' are plotted in a different colour to those parameters present in
##' both models.  Parameters present in the \emph{previous} model, but
##' not the \emph{current} model, are also overplotted with a cross,
##' highlighting their absence from the current model.
##'
##' For parameters present in both models, the values for the current
##' model are plotted, and horizontal line segments are used to
##' indicate how these parameters have changed between the previous and
##' current models.  Note that if parameter changes are very small,
##' the line segments may be too short to be seen.
##'
##' Caveat: note that a change in the random effects model
##' specification between 'previous' and 'current' models, can
##' sometimes result in an equivalent parameter being given different
##' labels by \code{\link{asreml}} between the models.  \code{replot}
##' cannot detect such cases, and hence will plot the affected
##' parameter as two distinct parameters - one removed from the
##' previous model, the other added to the current.
##'
##' \code{replot} requires that the \pkg{dplyr} and \pkg{ggplot2}
##' packages be installed.  \code{replot} returns a \code{ggplot}
##' object, and this object can be further modified using the usual
##' \pkg{ggplot2} package syntax.  For example (assuming asreml model
##' objects '\code{a1}' and '\code{a0}'), one can change the x-axis
##' scale of the object via, say, '\code{replot(a1,a0) + xlim(-1,1)}'.
##'
##' @title Plot \pkg{asreml} random model parameters
##' @param curr asreml object: The 'current' asreml model for which
##' the estimated random effects parameters are to be plotted.
##' @param prev asreml object, or NULL.  If a 'previous' asreml model
##' is specified for \code{prev}, the plot will compare random effects
##' parameters between the previous and current models.  See the
##' details section for more.
##' @param gammas logical: Should the plot display 'gamma' values or
##' @param size numeric: The size parameter for plotting points.
##' Equivalent to the \code{\link{geom_point()}} '\code{size}'
##' parameter.
##' @param lwd numeric: width parameter for the line segments that
##' indicate changes in parameters common to the current and previous
##' models.  Equivalent to the \code{\link{geom_segment()}}
##' '\code{size}' parameter.
##' @param tritanope logical: The default colors used by \code{replot)
##' should (I hope!) suit non-colourblind users, as well as those with
##' from protanopia and deuteranopia. Tritanopia-friendly colors can
##' be obtained by setting \code{tritanopia=TRUE}.  Default is FALSE.
##' @param zline logical: Display a grey vertical line (the 'zero
##' line') passing through the x-axis origin? Default is TRUE.
##' @param rlines logical: Display dashed vertical lines passing
##' through the residual variance parameter estimates? Default is
##' TRUE.
##' @return A \pkg{ggplot2)-package \code{ggplot) object.
##' @author Alexander Zwart (alec.zwart at csiro.au)
##'
replot.asreml <- function(curr,prev=NULL,gammas=TRUE,size=6,lwd=2,
                          tritanopia=FALSE,zline=TRUE,rlines=TRUE)
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
    if (!is.null(prev))  # ..and, if prev model is specified...
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
      xlab(ifelse(gammas,"Gamma value estimate","Parameter value estimate")) +
     geom_vline(aes(xintercept=cr_rv[[ifelse(gammas,"gamma","component")]]),
                linetype=2,size=0.6,color="grey60") +
     geom_vline(aes(xintercept=0),
                size=1,color="grey80")
    ##
    if (!is.null(added))
      {
        ## re color argument: if there is no previous model, plot
        ## points in blue ("#0072B2").  Else, plot in green
        ## ("#009E73"), or orange ("#E69F00") for those with
        ## tritanopia.
        added$Term <- factor(added$Term,levels=termLevs)
        plt <- plt + geom_point(aes_string(x=ifelse(gammas,"gamma","component"),
                                         y="Term"),
                              data=added,
                              size=size,
###                              color=ifelse(tritanopia,"#E69F00","#009E73"))
                              color=ifelse(is.null(prev),"#0072B2",
                                  ifelse(tritanopia,"#E69F00","#009E73")))
        ##
        if (any(added$constraint=="Boundary")) {
          added_b <- added[added$constraint=="Boundary",]
          plt <- plt + geom_point(aes_string(x=ifelse(gammas,"gamma","component"),
                                           y="Term"),
                                data=added_b,
                                size=size*1.3,
                                shape="|")
          ## size=size*1.3, shape="|" is not ideal, but it will do for
          ## now.  The unicode option is nicer, but its not clear that
          ## the symbol will be available to all users...
        }
      }  ## End handling of 'added'
    ##
    if (!is.null(removed))
      {
        removed$Term <- factor(removed$Term,levels=termLevs)
        plt <- plt +
         geom_point(aes_string(x=ifelse(gammas,"gamma","component"),
                               y="Term"),
                    data=removed,
                    size=size,
                    color=ifelse(tritanopia,"#E69F00","#009E73")) +
        geom_point(aes_string(x=ifelse(gammas,"gamma","component"),
                              y="Term"),
                   data=removed,
                   size=size,
###                   shape="\u2715",
                   shape=4,
                   stroke=1.5)
        ##
        if (any(removed$constraint=="Boundary")) {
          removed_b <- removed[removed$constraint=="Boundary",]
          plt <- plt + geom_point(aes_string(x=ifelse(gammas,"gamma","component"),
                                           y="Term"),
                                data=removed_b,
###                                shape="\u2503",
                                shape="|",
                                size=size*1.3)
          ## size=size*1.3, shape="|" is not ideal, but it will do for
          ## now.  The unicode option is nicer, but its not clear that
          ## the symbol will be available to all users...
        }
      }  ## End handling of 'removed'
    ##
    if (!is.null(inboth))
      {
        inboth$Term <- factor(inboth$Term,levels=termLevs)
        ## plt <- plt +  ## Line overplots point
        ##  geom_point(aes_string(x=ifelse(gammas,"gamma.x","component.x"),
        ##                        y="Term"),
        ##             data=inboth,
        ##             size=size,
        ##             color="#0072B2") +
        ## geom_segment(aes_string(x=ifelse(gammas,"gamma.x","component.x"),
        ##                         xend=ifelse(gammas,"gamma.y","component.y"),
        ##                         y="Term",
        ##                         yend="Term"),
        ##              data=inboth,
        ##              size=lwd)
        plt <- plt +  ## Point overplots line
         geom_segment(aes_string(x=ifelse(gammas,"gamma.x","component.x"),
                                 xend=ifelse(gammas,"gamma.y","component.y"),
                                 y="Term",
                                 yend="Term"),
                      data=inboth,
                      size=lwd) +
                       geom_point(aes_string(x=ifelse(gammas,"gamma.x","component.x"),
                                             y="Term"),
                                  data=inboth,
                                  size=size,
                                  color="#0072B2")
        ##
        if (any(inboth$constraint=="Boundary")) {
          inboth_b <- inboth[inboth$constraint=="Boundary",]
          plt <- plt + geom_point(aes_string(x=ifelse(gammas,"gamma","component"),
                                           y="Term"),
                                data=inboth_b,
                                size=size*1.3,
                                shape="|")
          ## size=size*1.3, shape="|" is not ideal, but it will do for
          ## now.  The unicode option is nicer, but its not clear that
          ## the symbol will be available to all users...
        }
      }  ## End handling of 'inboth'
    ## Finishing touches:
    plt <- plt + theme_bw()
    ##
    return(plt)
  }

## blue:  #0072B2

replot.asreml(a1,a0)

replot.asreml(a1,a1)  ## Hah! nice.  Better check how that happens though...

replot.asreml(a0,a1)

replot.asreml(a1)

replot.asreml(a0)


stop()

## ######################################################################
## ######################################################################
##
## ##' Plot random effects parameter estimates from one or two
## ##' \code{asreml} model objects.
## ##'
## ##' \code{REplot} plots the estimates for the random effects (RE)
## ##' parameters for one (\code{curr}, "current") or two (\code{curr} &
## ##' \code{prev}, "previous") \code{asreml} model objects.
## ##'
## ##' \begin{itemize}
## ##'
## ##' \item When two models are specified, parameters present in both
## ##' models are compared by plotting a line drawn from the
## ##' \emph{previous} model estimate to the \emph{current} model
## ##' estimate.  The current model estimates are also plotted as a blue
## ##' point. Note that if a change in estimates is very small, the line
## ##' may be hidden by the point.
## ##'
## ##' \item When two models are specified, parameters in the current
## ##' model \emph{not} present in the previous model are highlighted by
## ##' plotting them in green.  TODO
## ##'
## ##' \item When two models are specified, parameters present in the
## ##' previous model but not the current model are plotted as a asterisk
## ##' if not on a boundary, or as an asterisk in a circle if on a
## ##' boundary (boundary status indicated by
## ##' \code{constraint=="Boundary" in the asreml RE parameter estimates
## ##' table). TODO
## ##'
## ##' \item Parameters in the current model are plotted as circles when
## ##' on a boundary.
## ##'
## ##' \item Residual variance parameter values (indicated by suffix
## ##' \code{variance} in the asreml's RE parameter naming scheme) are
## ##' highlighted by vertical dashed lines on the plot, to ease
## ##' comparisons of random effect sizes to the residual.
## ##'
## ##' \items RE parameters can occur on different scales (correlation
## ##' parameters vs variance parameters, for example).  \code{REplot}
## ##' does not try to account for such scale differences - instead,
## ##' specify an explicit plot range if you wish to focus on certain
## ##' parameters, e.g.  \code{REplot(a1,a0) + xlim(-1,1)}.  See the
## ##' \pkg{ggplot2} documentation for more on this syntax.
## ##'
## ##' @title \pkg{asreml} random effects parameter estimates plot.
## ##' @param curr Object of class \code{asreml}.
## ##' @param prev Object of class \code{asreml}. Default \code{NULL}.
## ##' @param size Numeric: the \code{ggplot2::geom_point} point size.
## ##' Default is 4.
## ##' @param lwd Numeric: the \code{ggplot2::geom_segment} line
## ##' thickness.  Default is 2.
## ##' @return A \code{ggplot} object.  Note that qualities of the plot
## ##' can be adjusted via the usual \pkg{ggplot2} functions and syntax,
## ##' e.g. \code{REplot(a1,a0) + theme_bw()}.  Note also that when
## ##' invoked inside a function or for loop, display of the plot must be
## ##' forced by wrapping the REplot call in \code{\link{print}}.
## ##' @author Alexander Zwart (alec.zwart at csiro.au)
## ##' @export
## ##'
## REplot <- function(curr,prev=NULL,size=4,lwd=2)
##   {
##     stopifnot(is.numeric(size))
##     stopifnot(is.numeric(lwd))
##     stopifnot(class(curr)=="asreml")
##     if (!is.null(prev)) stopifnot(class(prev)=="asreml")
##     ##
##     cr <- summary(curr)$varcomp[,c(1,2,5)]
##     cr$constraint=as.character(cr$constraint)
##     cr$Term <- row.names(cr)
##     rownames(cr) <- NULL
##     cr$Model <- "Curr."
##     ## Use the level order implied by the curr model...
##     termLevs <- rev(cr$Term)
##     if (!is.null(prev))  # ..and, if prev model is specified...
##       {
##         pr <- summary(prev)$varcomp[,c(1,2,5)]
##         pr$constraint=as.character(pr$constraint)
##         pr$Term <- row.names(pr)
##         rownames(pr) <- NULL
##         pr$Model <- "Prev."
## ##        inBoth <- termLevs[termLevs %in% pr$Term]
##         inBoth <- cr$Term[cr$Term %in% pr$Term]
##         ## ...then append any new levels from the prev model
##         inPNotC <- setdiff(pr$Term,cr$Term)
##         inCNotP <- setdiff(cr$Term,pr$Term)
##         ## Does setdiff preserve order?  Do I care?
##         termLevs <- c(termLevs,rev(inPNotC))
##         br <- rbind(pr,cr)
##       } else {
##         br <- cr
##       }
##     ##
##     gg <- ggplot() +
##      scale_y_discrete(limits=termLevs) +
##       ylab("") +
##        xlab("RE parameter estimate")
## ##
## ######################################################################
## ######################################################################
##     ## At this point we have:
##     ##
##     ## - a data frame containing both parameter sets (if 'prev' model
##     ## was specified, or the curr model parameters (if prev model not
##     ## specified).  Column 'Model' indicates the parameter set.
##     ##
##     ## - vector 'inBoth' (possibly empty!) indicating parameters with
##     ## the same name in both models.
##     ##
##     ## - vector inPNotC (possibly empty) indicating parameters present
##     ## in previous model, but not in current model
##     ##
##     ## - vector inCNotP (possibly empty) indicating parameters present
##     ## in current model, but not in previous model.
##     ## What do we need to do?
##     ##
##     ##
##
## ######################################################################
## ######################################################################
##
##     ##
##     ## Add lines from previous (if specified) model estimates, to
##     ## current model estimates.  The 'inBoth' check below allows for
##     ## the possibility of no common terms between models (unlikely,
##     ## but not impossible).
##     if (!is.null(prev) & length(inBoth)!=0)
##       {
##         rownames(cr) <- cr$Term
##         rownames(pr) <- pr$Term
##         dd <- data.frame(Term=inBoth,
##                          cComp=cr[inBoth,"component"],
##                          pComp=pr[inBoth,"component"],
##                          stringsAsFactors=FALSE)
##         ##
##         gg <- gg + geom_segment(aes(x=pComp,
##                                     y=Term,
##                                     xend=cComp,
##                                     yend=Term),
##                                 data=dd,
##                                 colour="black",
##                                 size=lwd,
##                                 lineend="round")
##       }
##     ## Add curr model estimates
##     gg <- gg +
##      geom_point(aes(x = component,
##                     y = Term,
##                     shape = constraint=="Boundary"),
## ###                          data=subset(br,Model=="Curr."),
##                 data=cr,
##                 size=size,
##                 colour="blue") +
##                  scale_shape_manual(values = c("TRUE"=8,"FALSE"=16)) +
##                   guides(shape=FALSE)
##     ## TODO - check that the above works when Boundary values are present.
##     ##
##     ## TODO - need to highlight the inCNotP points!  Lack of a 'tail'
##     ## doesn't do tje job, since other paramters may not have a
##     ## noticeable tail...
##     ##
##     ## TODO consider whether it wouldnt be easier to set up one data
##     ## frame of all points to plot, with appropriate manual
##     ## scales controlling colour and shape as relevant?
##
##     ## ##  Add curr model estimates, not on boundary.  Assume these are
##     ## ##  present - could wrap in an if(){} as per the 'on boundary' case
##     ## ##  below, for consistency or to avoid weird errors.
##     ## gg <- gg + geom_point(aes(y=Term,x=component),
##     ##                       data=subset(br,Model=="Curr." &
##     ##                                    constraint != "Boundary"),
##     ##                       size=size,colour="blue")
##     ## ##
##     ## ## Add curr model estimates on boundary (as crosses)
##     ## if (any(br$Model=="Curr." & br$constraint == "Boundary"))
##     ##   {
##     ##     gg <- gg + geom_point(aes(y=Term,x=component),
##     ##                           data=subset(br,Model=="Curr." &
##     ##                                        constraint == "Boundary"),
##     ##                           size=size,colour="blue",shape=4)
##     ##   }
##
##     ## Finally, plot any parameters present in the prev model not
##     ## present in the curr model - use different symbols for points on
##     ## the boundary.
##     if (length(inPNotC)!=0)
##       {
##         if (any(br$Term %in% inPNotC &
##                  br$constraint != "Boundary"))
##           {
##             gg <- gg + geom_point(aes(y=Term,x=component),
##                                   data=subset(br,
##                                       Term %in% inPNotC &
##                                        constraint != "Boundary"),
##                                   size=size,colour="darkorchid",shape=8)
##           }
##         ##
##         if (any(br$Term %in% inPNotC &
##                  br$constraint == "Boundary"))
##           {
##             gg <- gg + geom_point(aes(y=Term,x=component),
##                                   data=subset(br,
##                                       Term %in% inPNotC &
##                                        constraint == "Boundary"),
##                                   size=size,colour="darkorchid",shape=13)
##           }
##       }
##     ##
##     return(gg)
##   }
##
##
## repep(a1,a0)
## ##
## ## Use '+ coord_trans(x="log")' for log transform.
## ## Use, e.g  '+ xlim(0,0.2)' to change plot range.
##
## stop()
##
## ##' Plot variance parameters for one or two \pkg{asreml} models.
## ##'
## ##' @title Plot and compare asreml model variance parameters
## ##' @param curr object of class \code{asreml}: the current
## ##' \pkg{asreml} model, whose variance parameters are to be plotted.
## ##' @param prev (optional) object of class \code{asreml}: the previous
## ##' \pkg{asreml} model, whose variance parameters are to be comapred
## ##' to the current \pkg{asreml} model \code{curr}.
## ##' @param logt logical: log-transform the gamma axis? (Default
## ##' \code{FALSE}).
## ##' @param size numeric: point size parameter (default 4) see
## ##' \code{\link{geom_point}} in package \pkg{ggplot2}
## ##' @return A \code{\link{ggplot}} object.
## ##' @author Alexander Zwart (alec.zwart at csiro.au) and Alex Whan
## ##' (alex.whan at csiro.au)
## ##' @export
## ##'
## pvp <- function(curr,prev=NULL,logt=FALSE,size=4)
##   {
##     ## This all needs some serious thought and refactoring
##     ##
##     stopifnot(is.logical(logt))
##     stopifnot(is.numeric(size))
##     stopifnot(class(curr)=="asreml")
##     if (!is.null(prev)) stopifnot(class(prev)=="asreml")
##     ##
##     cvc <- summary(curr)$varcomp[,c(1,2,5)]
##     cvc$Term <- row.names(cvc)
##     cvc$Model <- "Curr."
##     ##
##     if (is.null(prev))
##       {
##         bvc <- cvc
##       } else {
##         pvc <- summary(prev)$varcomp[,c(1,2,5)]
##         pvc$Term <- row.names(pvc)
##         pvc$Model <- "Prev."
##         bvc <- rbind(pvc,cvc)
##       }
##     names(bvc)[names(bvc)=="gamma"] <- "Gamma"
##     bvc$Termcol <- "black"
##     bvc$Termcol[grepl("var$",bvc$Term)] <- "blue"
##     bvc$Termcol[grepl("variance$",bvc$Term)] <- "green4"
##     rvar <- with(bvc,Gamma[grepl("variance$",Term) & Model=="Curr."])
##     if (logt) rvar <- log(rvar)
##     ##
##     if (is.null(prev))
##       {
##         if (logt)
##           {
##             return(ggplot() +
##                     geom_point(aes(y=Term,x=log(Gamma)),
##                                data=bvc,size=size) +
##                                 geom_vline(xintercept=rvar,linetype=2) +
##                                 theme_bw() +
##                                  theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol)))
##           } else {
##             return(ggplot(bvc) +
##                     geom_point(aes(y=Term,x=Gamma),
##                                size=size) +
##                                 geom_vline(xintercept=rvar,linetype=2) +
##                                 theme_bw() +
##                                  theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol))
##                    )
##           }
##       } else {
##         tt <- with(bvc,table(Term))
##         newTerms <- names(tt)[tt==1]
##         bvc$Model[bvc$Term %in% newTerms] <- "New"
##         bvc$Model <- factor(bvc$Model,levels=c("Prev.","Curr.","New"))
##         ##
##         if (logt)
##           {
##             return(ggplot() +
##                     geom_point(aes(y=Term,x=log(Gamma),colour=Model),
##                                data=bvc,size=size) +
##                                 geom_vline(xintercept=rvar,linetype=2) +
##                                 theme_bw() +
##                                  theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol)))
##           } else {
##             return(ggplot() +
##                     geom_point(aes(y=Term,x=Gamma,colour=Model),
##                                data=bvc,size=size) +
##                                 geom_vline(xintercept=rvar,linetype=2) +
##                                 theme_bw() +
##                                  theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol)))
##           }
##       }
##   }
##




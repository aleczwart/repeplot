
##' Plot variance parameters for one or two \pkg{asreml} models.
##'
##' @title Plot and compare asreml model variance parameters
##' @param curr object of class \code{asreml}: the current
##' \pkg{asreml} model, whose variance parameters are to be plotted.
##' @param prev (optional) object of class \code{asreml}: the previous
##' \pkg{asreml} model, whose variance parameters are to be comapred
##' to the current \pkg{asreml} model \code{curr}.
##' @param logt logical: log-transform the gamma axis? (Default
##' \code{FALSE}).
##' @param size numeric: point size parameter (default 4) see
##' \code{\link{geom_point}} in package \pkg{ggplot2}
##' @return A \code{\link{ggplot}} object.
##' @author Alexander Zwart (alec.zwart at csiro.au) and Alex Whan
##' (alex.whan at csiro.au)
##' @export
##'
pvp <- function(curr,prev=NULL,logt=FALSE,size=4)
  {
    ## This all needs some serious thought and refactoring
    ##
    stopifnot(is.logical(logt))
    stopifnot(is.numeric(size))
    stopifnot(class(curr)=="asreml")
    if (!is.null(prev)) stopifnot(class(prev)=="asreml")
    ##
    cvc <- summary(curr)$varcomp[,c(1,2,5)]
    cvc$Term <- row.names(cvc)
    cvc$Model <- "Curr."
    ##
    if (is.null(prev))
      {
        bvc <- cvc
      } else {
        pvc <- summary(prev)$varcomp[,c(1,2,5)]
        pvc$Term <- row.names(pvc)
        pvc$Model <- "Prev."
        bvc <- rbind(pvc,cvc)
      }
    names(bvc)[names(bvc)=="gamma"] <- "Gamma"
    bvc$Termcol <- "black"
    bvc$Termcol[grepl("var$",bvc$Term)] <- "blue"
    bvc$Termcol[grepl("variance$",bvc$Term)] <- "green4"
    rvar <- with(bvc,Gamma[grepl("variance$",Term) & Model=="Curr."])
    if (logt) rvar <- log(rvar)
    ##
    if (is.null(prev))
      {
        if (logt)
          {
            return(ggplot() +
                    geom_point(aes(y=Term,x=log(Gamma)),
                               data=bvc,size=size) +
                                geom_vline(xintercept=rvar,linetype=2) +
                                theme_bw() +
                                 theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol)))
          } else {
            return(ggplot(bvc) +
                    geom_point(aes(y=Term,x=Gamma),
                               size=size) +
                                geom_vline(xintercept=rvar,linetype=2) +
                                theme_bw() +
                                 theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol))
                   )
          }
      } else {
        tt <- with(bvc,table(Term))
        newTerms <- names(tt)[tt==1]
        bvc$Model[bvc$Term %in% newTerms] <- "New"
        bvc$Model <- factor(bvc$Model,levels=c("Prev.","Curr.","New"))
        ##
        if (logt)
          {
            return(ggplot() +
                    geom_point(aes(y=Term,x=log(Gamma),colour=Model),
                               data=bvc,size=size) +
                                geom_vline(xintercept=rvar,linetype=2) +
                                theme_bw() +
                                 theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol)))
          } else {
            return(ggplot() +
                    geom_point(aes(y=Term,x=Gamma,colour=Model),
                               data=bvc,size=size) +
                                geom_vline(xintercept=rvar,linetype=2) +
                                theme_bw() +
                                 theme(axis.text.y=element_text(hjust=1,colour=bvc$Termcol)))
          }
      }
  }

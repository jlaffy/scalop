##--------------------------------------------------------------------------------------##
## This file contains methods definitions for rowttests, colttest, rowFtests, colFtests ##
##--------------------------------------------------------------------------------------##
# copyright, Bioconductor's genefilter

# change "dm" to "foldchange" in c code
# add methods in colttests and rowttests with fac as character in signature definition

#' @keywords internal
#' @useDynLib scalop rowcolttests
rowcoltt =  function(x, fac, tstatOnly, alternative, pmethod, which, na.rm) {
  if (!missing(tstatOnly) && (!is.logical(tstatOnly) || is.na(tstatOnly)))
      stop(sQuote("tstatOnly"), " must be TRUE or FALSE.")
  
  f = checkfac(fac)
  if ((f$nrgrp > 2) || (f$nrgrp <= 0))
    stop("Number of groups is ", f$nrgrp, ", but must be >0 and <=2 for 'rowttests'.")

  if (typeof(x) == "integer")
      x[] <- as.numeric(x)

  cc = .Call("rowcolttests", x, f$fac, f$nrgrp, which-1L, na.rm,
             PACKAGE="scalop")
    
  res = data.frame(statistic = cc$statistic,
                   foldchange = cc$foldchange,
                   row.names = dimnames(x)[[which]])

  if (!tstatOnly) {
      if (alternative == 'greater') {
          res = cbind(res, p.value = stats::pt(res$statistic, cc$df, lower.tail=FALSE))
      }
      
      else if (alternative == 'less') {
          res = cbind(res, p.value = stats::pt(res$statistic, cc$df, lower.tail=TRUE))
      }
      
      else {
          res = cbind(res, p.value = 2*stats::pt(abs(res$statistic), cc$df, lower.tail=FALSE))
      }
      
      res = cbind(res, p.adj = stats::p.adjust(res$p.value, method = pmethod))
  }


  attr(res, "df") = cc$df    
  return(res)
}


#' @keywords internal
rowcolFt =  function(x, fac, var.equal, which) {
  
  if(!(which %in% c(1L, 2L)))
    stop(sQuote("which"), " must be 1L or 2L.")
  
  if(which==2L)
    x = t(x)

  if (typeof(x) == "integer")
      x[] <- as.numeric(x)

  sqr = function(x) x*x
  
  stopifnot(length(fac)==ncol(x), is.factor(fac), is.matrix(x))
  x   <- x[,!is.na(fac), drop=FALSE]
  fac <- fac[!is.na(fac)]

  ## Number of levels (groups)
  k <- nlevels(fac)

  ## xm: a nrow(x) x nlevels(fac) matrix with the means of each factor
  ## level
  xm <- matrix(
     sapply(levels(fac), function(fl) rowMeans(x[,which(fac==fl), drop=FALSE])),
     nrow = nrow(x),
     ncol = nlevels(fac))

  ## x1: a matrix of group means, with as many rows as x, columns correspond to groups 
  x1 <- xm[,fac, drop=FALSE]

  ## degree of freedom 1
  dff    <- k - 1

  if(var.equal){
    ## x0: a matrix of same size as x with overall means
    x0 <- matrix(rowMeans(x), ncol=ncol(x), nrow=nrow(x))
  
    ## degree of freedom 2
    dfr    <- ncol(x) - dff - 1

    ## mean sum of squares
    mssf   <- rowSums(sqr(x1 - x0)) / dff
    mssr   <- rowSums(sqr( x - x1)) / dfr

    ## F statistic
    fstat  <- mssf/mssr

  } else{

    ## a nrow(x) x nlevels(fac) matrix with the group size  of each factor
    ## level
    ni <- t(matrix(tapply(fac,fac,length),ncol=nrow(x),nrow=k))

    ## wi: a nrow(x) x nlevels(fac) matrix with the variance * group size of each factor
    ## level
    sss <- sqr(x-x1)
    x5 <- matrix(
       sapply(levels(fac), function(fl) rowSums(sss[,which(fac==fl), drop=FALSE])),
       nrow = nrow(sss),
       ncol = nlevels(fac))          
    wi <- ni*(ni-1) /x5

    ## u : Sum of wi
    u  <- rowSums(wi)

    ## F statistic
    MR <- rowSums(sqr((1 - wi/u)) * 1/(ni-1))*1/(sqr(k)-1)
    fsno <- 1/dff * rowSums(sqr(xm - rowSums(wi*xm)/u) * wi)
    fsdeno <- 1+ 2* (k-2)*MR
    fstat <- fsno/fsdeno

    ## degree of freedom 2: Vector with length nrow(x)
    dfr <- 1/(3 * MR)
  
  }
  
  res = data.frame(statistic = fstat,
                   p.value   = stats::pf(fstat, dff, dfr, lower.tail=FALSE),
                   row.names = rownames(x))

  attr(res, "df") = c(dff=dff, dfr=dfr)
  return(res)
}

## ==========================================================================
## rowttests and colttests methods for 'matrix'
## ==========================================================================
#' @keywords internal
setMethod("rowttests", signature(x="matrix", fac="factor"),
          function(x, fac, tstatOnly=FALSE, alternative='greater', pmethod='BH', na.rm=FALSE)
          rowcoltt(x, fac, tstatOnly, alternative, pmethod, 1L, na.rm))

#' @keywords internal
setMethod("colttests", signature(x="matrix", fac="factor"),
          function(x, fac, tstatOnly=FALSE, alternative='greater', pmethod='BH', na.rm=FALSE)
          rowcoltt(x, fac, tstatOnly, alternative, pmethod, 2L, na.rm))

#' @keywords internal
setMethod("rowttests", signature(x="matrix", fac="missing"),
          function(x, fac, tstatOnly=FALSE, alternative='greater', pmethod='BH', na.rm=FALSE)
          rowcoltt(x, factor(integer(ncol(x))), tstatOnly, alternative, pmethod, 1L, na.rm))

#' @keywords internal
setMethod("colttests", signature(x="matrix", fac="missing"),
          function(x, fac, tstatOnly=FALSE, alternative='greater', pmethod='BH', na.rm=FALSE)
          rowcoltt(x, factor(integer(ncol(x))), tstatOnly, alternative, pmethod, 2L, na.rm))

## new
#' @keywords internal
setMethod("rowttests", signature(x="matrix", fac="character"),
          function(x, fac, tstatOnly=FALSE, alternative='greater', pmethod='BH', na.rm=FALSE)
          rowcoltt(x, makefac(fac, colnames(x)), tstatOnly, alternative, pmethod, 1L, na.rm))

#' @keywords internal
setMethod("colttests", signature(x="matrix", fac="character"),
          function(x, fac, tstatOnly=FALSE, alternative='greater', pmethod='BH', na.rm=FALSE)
          rowcoltt(x, makefac(fac, colnames(x)), tstatOnly, alternative, pmethod, 2L, na.rm))


## ==========================================================================
## rowFtests and colFtests methods for 'matrix'
## ==========================================================================
#' @keywords internal
setMethod("rowFtests", signature(x="matrix", fac="factor"),
          function(x, fac, var.equal=TRUE)
          rowcolFt(x, fac, var.equal, 1L))

#' @keywords internal
setMethod("colFtests", signature(x="matrix", fac="factor"),
          function(x, fac, var.equal=TRUE)
          rowcolFt(x, fac, var.equal, 2L))



## ------------------------------------------------------------
## convert fac from factor or numeric to integer and then
## make sure it is an integer 
## ------------------------------------------------------------
#' @keywords internal
checkfac = function(fac) {

  if(is.numeric(fac)) {
    nrgrp = as.integer(max(fac, na.rm=TRUE)+1)
    fac   = as.integer(fac)
  }
  ## this must precede the factor test
  if(is.character(fac))
    fac = factor(fac)

  if (is.factor(fac)) {
    nrgrp = nlevels(fac)
    fac   = as.integer(as.integer(fac)-1)
  } 
  if(!is.integer(fac))
    stop("'fac' must be factor, character, numeric, or integer.")
  
  if(any(fac<0, na.rm=TRUE))
    stop("'fac' must not be negative.")
    
  return(list(fac=fac, nrgrp=nrgrp))
}

#' @keywords internal
makefac = function(fac, cols) {
    factor(as.integer(!cols %in% fac) + 1)
}


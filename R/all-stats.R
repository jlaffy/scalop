
#' @keywords internal
x_in_y_overA = function(A = 0.1) {
    function(x, y) {
        x = sort(x, decreasing = T)
        y = sort(y, decreasing = F)[1:length(x)]
        sum( x >= y ) >= A
    }
}

#' @keywords internal
kOverA <- function(k, A=100, na.rm = TRUE) {
  function(x) {
      if(na.rm)
	x <- x[!is.na(x)]
      sum( x > A ) >= k
  }
}

#' @keywords internal
maxA <- function(A=75, na.rm=TRUE) {
    function(x) {max(x, na.rm=na.rm) >= A }
}


#' @keywords internal
pOverA <-  function(p=0.05, A=100, na.rm = TRUE) {
  function(x) {
      if(na.rm)
	 x<-x[!is.na(x)]
      sum( x > A )/length(x) >= p
  }
}


#' @keywords internal
cv <- function(a=1, b=Inf, na.rm=TRUE) {
    function(x) {
        	sdx <- stats::sd(x, na.rm=na.rm)
        if(is.na(sdx) || sdx == 0 ) return(FALSE)
	val <- sdx/abs(mean(x, na.rm=na.rm))
        if(val < a ) return(FALSE)
        if(val > b ) return(FALSE)
        return(TRUE)
            }
}

#' @keywords internal
anova <- function(cov, p=0.05, na.rm=TRUE)
{
    function(x) {
        if( na.rm ) {
            drop <- is.na(x)
            x <- x[!drop]
            cov <- cov[!drop]
        }
        m1 <- stats::lm(x~cov)
        m2 <- stats::lm(x~1)
        av <- stats::anova(m2,m1)
        fstat <- av[["Pr(>F)"]][2]
        if( fstat < p )
            return(TRUE)
        return(FALSE)
    }
}

#' @keywords internal
coxfilter <- function(surt, cens, p) {
   autoload("coxph", "survival")
   function(x) {
       srvd <- try(coxph(Surv(surt,cens)~x))
       if( inherits(srvd, "try-error") )
           return(FALSE)
       ltest <- -2*(srvd$loglik[1] - srvd$loglik[2])
       pv <- 1 - stats::pchisq(ltest, 1)
       if( pv < p )
           return(TRUE)
       return(FALSE)
   }
}

#' @keywords internal
ttest <- function(m, p=0.05, na.rm=TRUE) {
    if( length(m) == 1)
        function(x) {
            n <- length(x)
            if( m>n ) stop("m is larger than the number of samples")
            sub1 <- x[1:m]
            sub2 <- x[(m+1):n]
            if(na.rm) {
                drop <- is.na(x)
                sub1 <- sub1[!drop[1:m]]
                sub2 <- sub2[!drop[(m+1):n]]
            }
            t.test(sub1, sub2 )$p.value < p
        }
    else
        function(x) {
            if(na.rm) {
                drop <- is.na(x) | is.na(m)
                x<- x[!drop]
                m<- m[!drop]
            }
            t.test(x~m)$p.value < p
        }
  }



# gf functions.

#' @keywords internal
genescale <- function (m, axis=2, method=c("Z", "R"), na.rm=TRUE) {
    ##scale by the range
    RscaleVector <- function(v, na.rm) {
        mm <- range(v, na.rm=na.rm)
        (v - mm[1]) / (mm[2] - mm[1])
    }
    ##scale using Zscore
    ZscaleVector <- function(v, na.rm)
        (v - mean(v, na.rm=na.rm))/stats::sd(v, na.rm=na.rm)
#
# scales a matrix using the scaleVector function.
#
    which <- match.arg(method)
    method <- switch(which,
                     Z = ZscaleVector,
                     R = RscaleVector)
    if( is.matrix(m) || is.data.frame(m) ) {
        rval <- apply (m, axis, method, na.rm=na.rm)
        if( axis==1 ) return(t(rval))
        return(rval)
    }
    else
	method(m, na.rm=na.rm)
}


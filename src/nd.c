/* Copyright The Bioconductor Foundation 2007, all rights reserved */
/* this is patterned on the R code in library/stats/src/distance.c as
   we want to have similar values, but does not handle NA/Inf
   identically, allows weights and solves the problem of finding
   distances to a particular value, not necessarily all pairwise
   distances */

/* Modified in April 2007 for use with S-PLUS ArrayAnalyzer
   by Insightful Corp.

   Replaced all int declarations with RSInt declarations.
   RSInt is defined in S-PLUS's R.h as:

       typedef long RSInt;

   Other changes are if-def-ed with if defined(_R_) around the
   original code.
 */

/* and further modified since S.h in R defines USING_R - not
  _R_ !!
*/

#include "R.h"

#if defined(USING_R) /*( R-specific stuff */

#define  S_CDECL
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
/* we need this first to get the right options for math.h */
#include <R_ext/Arith.h>

#include "scalop.h"
#include <Rmath.h>
#include "R_ext/Error.h"
#include "R_ext/Applic.h" 

#else /*) Splus-specific stuff */

#define S_COMPATIBILITY 1
#include "rsplus.h"
#endif

typedef struct {
    RSInt geneNum;
    double geneDist;
} gene_t;


static void detectTies(RSInt geneNum, RSInt nResults, RSInt nRows, gene_t *data) {
    /* Will scan through the first nResults+1 distances in the */
    /* data array, and if it detects any ties, will flag a R */
    /* warning */
    RSInt i; /* Loop indices */
    
    /* If nResults == nRows, do not exceed nResults - otherwise exceed it */
    /* by 1 in order to see if there were trailing ties */
    if (nResults == nRows) {
	nResults = nRows-1;
    }
    
    for (i = 1; i < nResults; i++) {
	if (data[i].geneDist == data[i+1].geneDist) {
	    PROBLEM "There are distance ties in the data for gene %d\n",geneNum
            WARN;
	    break;
	}
    }
}
static int S_CDECL distCompare(const void *p1, const void *p2)
{
    const gene_t *i = p1;
    const gene_t *j = p2;

    if (!R_FINITE(i->geneDist ))
      return(1);
    if (!R_FINITE(j->geneDist))
      return(-1);

    if (i->geneDist > j->geneDist) 
	return (1);
    if (i->geneDist < j->geneDist) 
	return (-1);
    return (0);
    
}

static double gf_correlation(double *x, double *wval, RSInt nr, RSInt nc, RSInt i1, RSInt i2) {
  RSInt i; /* Loop index */
  RSInt a,b; /* Used as array indices for i1 and i2 */
  double xAvg, yAvg; /* Averages of the i1 and i2 rows */
  double wA, wB; /* Weighted x[a] and x[b] */
  double upTot = 0; /* Upper summation */
  double botTotL, botTotR; /* The lower two summations */
  double botVal; /* Bottom value for Rho */
  double Rho, ans;
  
  botTotL = botTotR = 0;
  xAvg = yAvg = 0;
  a = i1;
  b = i2;
  
  /* Calculate the averages for the i1 and i2 rows */
  for (i = 0; i < nc; i++) {
    if (R_FINITE(x[a])) {
      xAvg += (wval[i] * x[a]);
	}
    if (R_FINITE(x[b])) {
      yAvg += (wval[i] * x[b]);
    }
    a += nr;
    b += nr;
    }
  xAvg /= (double)nc;
  yAvg /= (double)nc;
    /* Reset a & b */
  a = i1; b = i2;
  
  /* Build up the three summations in the equation */
  for (i = 0; i < nc; i++) {
        if (R_FINITE(x[a]) && R_FINITE(x[b])) {
	  wA = (x[a] - xAvg);
	  wB = (x[b] - yAvg);
	  upTot += wval[i]*wA*wB;
	  botTotL += wval[i]*pow(wA,2);
	  botTotR += wval[i]*pow(wB,2);
        }
        a += nr;
        b += nr;
  }
  
  /* Compute Rho & Distance (1 - R) */
  botVal = sqrt((botTotL * botTotR));
  Rho = upTot / botVal;
  ans = 1 - Rho;
    
  return(ans);
}

static double gf_euclidean(double *x, double *wval, RSInt nr, RSInt nc, RSInt i1, RSInt i2) 
{
    double dev, ans;
    RSInt ct, j;
    
    ct = 0;
    ans = 0;

    for(j = 0 ; j < nc ; j++) {
	if(R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	    dev = (x[i1] - x[i2]);
	    dev = dev * dev;
	    /* Apply weight and add the total */
	    ans += (wval[j] * dev);
	    ct++;
	}
	i1 += nr;
	i2 += nr;
    }
    if(ct == 0) return NA_REAL;
    if(ct != nc) ans /= ((double)ct/nc);
    return sqrt(ans);
}

static double gf_maximum(double *x, double *wval, RSInt nr, RSInt nc, RSInt i1, RSInt i2) 
{
    double dev, ans;
    RSInt ct, j;

    ct = 0;
    ans = -DBL_MAX;
    for(j = 0 ; j < nc ; j++) {
	if(R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	    dev = fabs(x[i1] - x[i2]);
	    /* apply the weight */
	    dev *= wval[j];
	    if(dev > ans)
		ans = dev;
	    ct++;
	}
	i1 += nr;
	i2 += nr;
    }
    if(ct == 0) return NA_REAL;
    return ans;
}

static double gf_manhattan(double *x, double *wval, RSInt nr, RSInt nc, RSInt i1, RSInt i2)
{
    double ans;
    RSInt ct, j;

    ct = 0;
    ans = 0;
    for(j = 0 ; j < nc ; j++) {
	if(R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	    ans += (wval[j] * fabs(x[i1] - x[i2]));	    
	    ct++;
	}
	i1 += nr;
	i2 += nr;
    }
    if(ct == 0) return NA_REAL;
    if(ct != nc) ans /= ((double)ct/nc);
    return ans;
}


static double gf_canberra(double *x, double *wval, RSInt nr, RSInt nc, RSInt i1, RSInt i2)
{
    double ans, sum, diff;
    RSInt ct, j;


    ct = 0;
    ans = 0;
    for(j = 0 ; j < nc ; j++) {
	if(R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	    sum = fabs(x[i1] + x[i2]);
	    diff = fabs(x[i1] - x[i2]);
	    if (sum > DBL_MIN || diff > DBL_MIN) {
		ans += wval[j]*(diff/sum);
		ct++;
	    }
	}
	i1 += nr;
	i2 += nr;
    }
    if(ct == 0) return NA_REAL;
    if(ct != nc) ans /= ((double)ct/nc);
    return ans;
}

static double gf_dist_binary(double *x, double *wval, RSInt nr, RSInt nc, RSInt i1, RSInt i2)
{
    RSInt total, ct, ans;
    RSInt j;

    total = 0;
    ct = 0;
    ans = 0;
    for(j = 0 ; j < nc ; j++) {
      if(R_FINITE(x[i1]) && R_FINITE(x[i2])) {
	if(x[i1] || x[i2]){
	  ct += wval[j];
	  if( !(x[i1] && x[i2]) ) ans += wval[j];
	}
	total++;
      }
      i1 += nr;
      i2 += nr;
    }
    


    if(total == 0) return NA_REAL;
    if(ct == 0) return 0;
    return (double) ans / ct;
}



enum { EUCLIDEAN=1, MAXIMUM, MANHATTAN, CANBERRA, CORRELATION, BINARY};
/* == 1,2,..., defined by order in the R function dist */

void gf_distance(double *x, RSInt *nr, RSInt *nc, RSInt *g, double *d, 
		 RSInt *iRow, RSInt *nInterest, RSInt *nResults, 
		 RSInt *method, double *wval) {
    /*
      x -> Data Array
      nr -> Number of rows in X
      nc -> number of columns in X
      g -> The nResults closest genes to the genes of interest
      d -> The distances of the genes from g, 1 to 1 mapping
      iRow -> rows of X that we are interested in
      nInterest -> Number of elements in iRow
      nResults -> The top X results to pass back
      method -> which distance method to use
    */
    
    RSInt  i,j, k;  /* Loop indices */
    RSInt baseIndex; /* Used to index data arrays */
    gene_t *tmp; /* Temporary array to hold the distance data */
    double (*distfun)(double*, double*, RSInt, RSInt, RSInt, RSInt) = NULL;

    /* Sanity check the nResults vs. number of rows in the data */
    if (*nResults > *nr) {
	warning("Number of results selected is greater than number of rows, using the number of rows instead\n");
	*nResults = *nr-1;
    }
    
    /* Size of tmp == *nr, as each gene we're interested in will
       generate *nr distance points */

    tmp = (gene_t *)R_alloc(*nr, sizeof(gene_t));
    
    /* Determine which distance function to use */
    switch(*method) {
    case EUCLIDEAN:
	distfun = gf_euclidean;
	break;
    case MAXIMUM:
	distfun = gf_maximum;
	break;
    case MANHATTAN:
	distfun = gf_manhattan;
	break;
    case CANBERRA:
	distfun = gf_canberra;
	break;
    case CORRELATION:
	distfun = gf_correlation;
	break;
    case BINARY:
	distfun = gf_dist_binary;
	break;
    default:
	error("invalid distance");
    }

    for (j = 0; j < *nInterest; j++) {  
	/* Get the distances for this gene, store in tmp array */

	for(i = 0 ; i < (*nr) ; i++) {
	    tmp[i].geneNum = i; 
	    tmp[i].geneDist = distfun(x, wval, *nr, *nc, 
				      iRow[j]-1, i);       
	}
	
	/* Run a sort on the temp array */
	qsort(tmp, *nr, sizeof(gene_t), distCompare);    

	/* Detect any ties */
	detectTies(iRow[j], *nResults, *nr, tmp); 

	/* Copy the 1<->nResults data points into the final array */
	baseIndex = *nResults * j;
	for (k = 1; k <= *nResults; k++) {
	    g[baseIndex + (k-1)] = tmp[k].geneNum; 
	    d[baseIndex + (k-1)] = tmp[k].geneDist; 
	}
    }
}



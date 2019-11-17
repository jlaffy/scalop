/* Copyright Bioconductor Foundation NA, 2007, all rights reserved */
#include <R.h>
#include <Rinternals.h>

typedef int RSInt;

void gf_distance(double *x, RSInt *nr, RSInt *nc, RSInt *g, double *d, 
		 RSInt *iRow, RSInt *nInterest, RSInt *nResults, 
		 RSInt *method, double *wval);

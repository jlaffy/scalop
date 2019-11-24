/* Copyright Bioconductor Foundation of NA, 2007, all rights reserved */

#include "R.h"
#include "scalop.h"
#include "R_ext/Rdynload.h"

static const R_CMethodDef CEntries[] = {
    {"gf_distance", (DL_FUNC) &gf_distance, 10},
    {NULL, NULL, 0}
};

void R_init_scalop(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
}

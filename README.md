
## ```scalop```

<!-- badges: start -->
<!-- badges: end -->

## **_S_**ingle-**_C_**ell **_A_**na**_L_**ysis **_OP_**perations

```scalop``` is an implementation and extension of the main methods in [this 2019 paper] on glioblastoma. We wrote the package with an emphasis on modularity and adaptability, and to that end included only those methods whose performances _should_ be robust across datasets:

1. Cell QC
2. Gene QC
3. Expression normalisation
4. Intra-tumour cell clustering
5. Defining cancer expression programs
6. Defining meta-programs from pan-tumour integration of programs
7. Scoring cells for programs / gene signatures. 
8. Integrating cancer programs cross-tumour to define meta-prorgams 
9. Metaprogram definition from meta-clusters of cells

## Installation

You can install the development version of scalop with:

``` r
devtools::install_github('jlaffy/scalop@devel')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(scalop)
## basic example code
```


An introduction to graph analysis and modeling
================

> This repository regroups the material (slides, practicals, projects)
> associated to the course about “graph analysis and modeling”, as a
> part of the [MSc in Statistics for Smart
> Data](http://www.ensai.fr/formation/msc-in-statistics-for-smart-data.html).

## Schedule (tentative)

### Descriptive Analysis of Network Data

November the 6th, 2018

  - *Course* Statistics on network data, Graph Partitionning -
    [slides](https://github.com/jchiquet/CourseStatNetwork/raw/master/slides/DescriptiveAnalysis/DescriptiveAnalysis.pdf)
  - *Tutorial* Basical graph manipulation and Spectral Clustering
    [sheet](https://github.com/jchiquet/CourseStatNetwork/raw/master/practicals/DescriptiveAnalysis/tuto_DescriptiveAnalysis.pdf)

### Statistical Models for Networks Data

November the 15th, 2018

  - *Course*: Mixture Models, (variational) EM algorithm, Stochastic
    Block Model -
    [slides](https://github.com/jchiquet/CourseStatNetwork/raw/master/slides/GraphModel/GraphModel.pdf)
  - *Tutorial*: Stochastic Block Model and variational inference
    [sheet](https://github.com/jchiquet/CourseStatNetwork/raw/master/practicals/GraphModel/tuto_GraphModel.pdf)

### Statistical Models for Networks Data: extension

November the 22th, 2018

## Computer requirements

You need to have a recent version of
[Rstudio](https://www.rstudio.com/products/rstudio/download/) installed
with [R](https://cran.r-project.org) \>= 3.5.1 and the following
packages installed:

### Basic packages for R extensions

``` r
install.packages("devtools")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("aricode")
install.packages("Matrix")
```

### Packages for graph manipulation

``` r
install.packages("igraph")
install.packages("sna")
install.packages("network")
```

### Packages for stochastic block models

``` r
install.packages("blockmodels")
install.packages("mixer") ## you must install from source
```

### Packages for fancy plotting

``` r
install.packages("tidyverse")
install.packages("ggraph")
```

## Projects: extension of the stochastic block model

Coming soon

## References

  - [Rstudio cheat
    sheets](https://www.rstudio.com/resources/cheatsheets/)

Some book (not freely available, sorry)

  - [Statistical Analysis of Network Data: Methods and Models, by Eric
    D.
    Kolaczyk](https://books.google.fr/books?id=Q-GNLsqq7QwC&source=gbs_book_similarbooks)
  - [Statistical Analysis of Network Data with R, by Eric D. Kolaczyk,
    Gábor
    Csárdi](https://books.google.fr/books?id=cNMhBAAAQBAJ&source=gbs_navlinks_s)
  - Bishop, C. (2000). Introduction to graphical modelling, 2nd edn.
    Springer, New York.
  - Højsgaard, S., Edwards , D., Lauritzen, S. (2012). Graphical Models
    with R. Springer, New York.

Some material online

  - [Eric D. Kolazcyk’s course
    slides](http://math.bu.edu/ness12/ness2012-shortcourse-kolaczyk.pdf)
  - [Catherine Matias’s course page (in
    French)](http://cmatias.perso.math.cnrs.fr/Cours_Graphes.html)

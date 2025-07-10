# Run Analysis Tools

R package **modelstats**, version **0.27.5**

[![CRAN status](https://www.r-pkg.org/badges/version/modelstats)](https://cran.r-project.org/package=modelstats) [![R build status](https://github.com/pik-piam/modelstats/workflows/check/badge.svg)](https://github.com/pik-piam/modelstats/actions) [![codecov](https://codecov.io/gh/pik-piam/modelstats/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/modelstats) [![r-universe](https://pik-piam.r-universe.dev/badges/modelstats)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

A collection of tools to analyze model runs.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("modelstats")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("rs2")          # Run statistics 2 (rs2)
vignette("testingSuite") # Testing Suite
```

## Questions / Problems

In case of questions / problems please contact Anastasis Giannousakis <giannou@pik-potsdam.de>.

## Citation

To cite package **modelstats** in publications use:

Giannousakis A, Richters O (2025). "modelstats: Run Analysis Tools." Version: 0.27.5, <https://github.com/pik-piam/modelstats>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {modelstats: Run Analysis Tools},
  author = {Anastasis Giannousakis and Oliver Richters},
  date = {2025-07-10},
  year = {2025},
  url = {https://github.com/pik-piam/modelstats},
  note = {Version: 0.27.5},
}
```

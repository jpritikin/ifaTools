# ifaTools

The purpose of `ifaTools` is to collect useful tools for conducting Item Factor Analysis.

`ifaTools` is not available on CRAN yet because a dependency,
`OpenMx`, is not available on CRAN. You will need to install from
github,

```R
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("jpritikin/ifaTools")
```

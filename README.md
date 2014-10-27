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

`ifaTools` includes 2 shiny apps:

* itemModelExplorer() -- To develop an intuitive sense of item response models
* modelBuilder() -- An easy way to generate code to conduct IFA analysis

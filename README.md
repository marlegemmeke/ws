# Workshop best practices for writing a reproducible R code : multinomial fall prediction model

Version 0.1.0

This project was set up during a workshop. The aim of the project is to predict falls by medication use. In this project a fake database is used. The predictive performance of the models is calculated using the PDI.


Assumptions on the data input:
- Gender is defined as either male (M) or female (V); age is defined in years
- The patients should be categorized in 3 categories of falls (no falls (val=0), one fall (val=1), recurrent falls (val=2)).
- The Drug Burden Index (DBI) should be calculated for each patient and should be 0 or > 0.
- For all drug categories patients should be either using (= 1) or not using (= 0) the drug.

NOTE: for the FAKE dataset the assumptions are not met!!!

This project is build under R version 3.6.3.

The following packages need to be installed:
library(nnet);
library(dplyr);
library(bayesm);
library(VGAM);
library(mcca);

## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).

# LWLetterInventory

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/leeway64/LWLetterInventory)

LWLetterInventory is an R class that holds an inventory of letters. That is, it can store how many
of each letter are in a string, along with other features.


## Installation
```bash
git clone https://github.com/leeway64/LWLetterInventory.git
cd LWLetterInventory
R
renv::restore()
```

`renv` stands for reproducible environments. To learn more about `renv`, refer to [their introduction page](https://rstudio.github.io/renv/articles/renv.html).


## Unit tests
To run unit tests, run:
```bash
R
testthat::test_dir(here::here("test"))
```


## Acknowledgements
The specifications for this project are provided on the
[UW Seattle CSE 143 autumn 2017 website](https://courses.cs.washington.edu/courses/cse143/17au/homework.shtml).
The original assignment was meant to be completed in Java, but I completed it in R for this project.


## Third-party software
- [renv](https://rstudio.github.io/renv/index.html) (MIT License): Reproducible environments for R
- [here](https://here.r-lib.org/) (MIT License): Easy file referencing for R
- [testthat](https://testthat.r-lib.org/) (MIT License): R unit testing library
- [box](https://klmr.me/box/) (MIT License): R library for modular code

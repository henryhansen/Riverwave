
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Riverwave

<!-- badges: start -->
<!-- badges: end -->

The goal of Riverwave is to transform standard gauge station data into
relevant ecological theory to foster understanding of river behavior.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("henryhansen/Riverwave")
```

## Example gauge station download

This is a basic example which shows you how to download some gauge
station data from SMHI:

``` r
library(Riverwave)
## basic example
head(Riverwave::smhi_csv(274))
#> [1] "EDEBÄCK_274"
#>   Datum (svensk sommartid) Vattenföring Kvalitet
#> 1               1910-01-01           70        G
#> 2               1910-01-02           70        G
#> 3               1910-01-03           70        G
#> 4               1910-01-04           69        G
#> 5               1910-01-05           69        G
#> 6               1910-01-06           69        G
```

The visualization concept for this package follows the idea presented in
the [riverwave
paper](https://academic.oup.com/bioscience/article/64/10/870/1780369):
![riverwave-concept](images/Riverwave_concept.jpg)

```PlantUML
@startuml

SMHI : Date
SMHI : Discharge
SMHI : Quality

SMHI -> Riverwave
Riverwave --> [*]
Riverwave : Calculations
Riverwave : Conversions
Riverwave : Estimation

Riverwave -> Riverwave_Shiny
Riverwave_Shiny : Mapping
Riverwave_Shiny : Visualization
Riverwave_Shiny : Reports

Riverwave_Shiny --> [*]

@enduml
```

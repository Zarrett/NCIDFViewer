# NCIDFViewer

This repository contains an R library which calculates IDF Curves on data from NC files

## Usage

Library contains ``` NCtoIDF(folder, ids)``` function which accepts folder path and cell IDs for IDF Curve calculation

## Installing the library

```R
# install.packages("devtools")
devtools::install_github(repo = "strnda/advRpckg")
```

```R
library(NCViewer)

NCtoIDF("path/to/forder", cellIDs = c(296, 263, 264, 265, 295, 297, 327, 328, 329))
```

### Example output

![examplePlot.png](https://github.com/Zarrett/NCIDFViewer/blob/main/pictures/examplePlot.png)

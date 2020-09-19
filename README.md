# Tidy Tuesday August 8th, 2020: 
This repo contains the code to generate a ```{gt}``` table showing the emotions of characters in the first four seasons of the TV series *Friends*.

![image](https://user-images.githubusercontent.com/56490913/93687653-45680880-fa8d-11ea-95a2-9dfeca41f177.png)

## Getting Started
To generate the table, download the repo and run Table_Creation.R. The Line_Chart.R file was a first attempt to chart the emotions over time, but was not seen through to completion since we only have emotions for the first 4 seasons. 

### Prerequisites

To run the code on your computer you just need R and the following packages installed and loaded:

```
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(gt)
library(paletteer)

```

## Note 
This was my first attempt at using ```{gt}``` to make a table, so the code is not as elegant or visually pleasing as the output, and I would not suggest using it as a template. It will serve as a marker for how I started with ```{gt}```, which I will compare with future tables. 

## Author

* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16)

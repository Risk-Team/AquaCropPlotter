# AquaCrop Vis

## Motivation
AquaCrop is the crop-water productivity model developed by the NSL division at FAO. Currently, AquaCrop produces output txt files that can be difficult to process and visualize. We intend to fill that gap by developing a R Shiny app that would automatically read the output simulations of aquacrop and produce meaningfull plots and statistics that the user can automatically embed into reports. 

## How to use the app

AquaCropVis is currently hosted in shinyapps.io so that you do not need to install anything in your local computer. Simply connect to the server and enjoy using the app. However, if you want to run AquaCropVis locally, simply install it from GitHub:

```
library(devtools)
install_github("Risk-Team/aquacrop_shiny")
```
  
  Then
  
  `library(AquaCropVis)
  aquacropvis()
  `

# AquaCropVis

## Motivation
AquaCrop is the crop-water productivity model developed by the NSL division at FAO. Currently, AquaCrop produces output txt files that can be difficult to process and visualize. We intend to fill that gap by developing a R Shiny app that would automatically read the output simulations of aquacrop and produce meaningfull plots and statistics that the user can automatically embed into reports. 
AquaCropVis works with the outouts of both AquaCrop version 6 and 7. Previous version are not recognised by the app. 

## Where to find AquaCropVis

AquaCropVis is currently hosted in shinyapps.io so that you do not need to install anything in your local computer. Simply connect to the server and enjoy using the app. However, if you want to run AquaCropVis locally, simply install it from GitHub:

```
library(devtools)
install_github("Risk-Team/aquacrop_shiny")
```
  
Then to launch the app
  
```
library(AquaCropVis)
aquacropvis()
```
## How to use the app

Regardless of whether you use the app locally or remotely in the shiny server, the app would work in the same way. However, when you install the app from GitHub it comes with a small example dataset that you can use to get acquaninted with its functionalities. In general, the app is very easy to use and allow you to work with the outputs of AquaCrop version 6 and 7. 

![workflow](https://user-images.githubusercontent.com/40058235/204907854-4cb0e6b5-6d20-4c73-8f71-5ead3dce22fc.png)
|:--:| 
| Workflow of AquaCropVis |

### Step 1: load the data



## Status

**The app is now officially launched. Training materials will be released soon**.

[Link to the app](https://foodandagricultureorganization.shinyapps.io/AquaCropPlotter/)
## Background
AquaCrop is the crop-water productivity model developed by FAO. Currently, AquaCrop produces output txt files that can be difficult to process and visualize. AquaCropPlotter fills that gap by automatically reading the output simulations of AquaCrop and producing meaningful plots and statistics that the user can automatically embed into reports. 
AquaCropPlotter works with the outputs of both AquaCrop versions 6 and 7. Previous versions are not recognized by the app. 

## Where to find AquaCropPlotter

AquaCropPlotter is currently hosted in shinyapps.io so you do not need to install anything on your local computer. [Simply connect to the server](https://foodandagricultureorganization.shinyapps.io/AquaCropPlotter/). However, if you want to run AquaCropPlotter locally, simply install it from GitHub:

```
# if you are using windows you would likely need to resolve some dependencies issues when installing the library devtools
install.packages("devtools")
library(devtools)
install_github("Risk-Team/AquaCropPlotter/AquaCropPlotter")
```
  
Then to launch the app:
  
```
library(AquaCropPlotter)
aquacropplotter()
```

The app comes with some example data. To find the data on your local computer after installing the app, run:

```
system.file("data/", package="AquaCropPlotter")

```
## AquaCropPlotter workflow

Regardless of whether you use the app locally or online, the app would work in the same way.

![workflow](https://github.com/Risk-Team/AquaCropPlotter/assets/10773204/add551fc-2d98-48de-86ca-21449cbdab02)
|:--:| 
| Workflow of AquaCropPlotter |

## AquaCropPlotter tutorial

Tutorials will be released soon


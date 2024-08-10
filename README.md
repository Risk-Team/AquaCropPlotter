<h1 align="center">
<img src="logo.png" width = "400" height = "100" align="center" />
  <br>
  <h4 align="center">AquaCropPlotter: R Shiny application for visualizing AquaCrop model results</h4>
  <br>
<div align="center">
   <img src="https://img.shields.io/github/v/release/Risk-team/AquaCropPlotter" alt="GitHub R package version" style="display: inline-block;">
  <a href="http://hits.dwyl.com/Risk-team/AquaCropPlotter"><img src="http://hits.dwyl.com/Risk-team/AquaCropPlotter.svg"/></a>
</div>
</h1>

## Status 

[Link to the app](https://foodandagricultureorganization.shinyapps.io/AquaCropPlotter/)
[Link to the manual](https://openknowledge.fao.org/server/api/core/bitstreams/a6c9db7b-ae5c-4d72-a307-813621835966/content)

## Background
AquaCrop is the crop-water productivity model developed by FAO. Currently, AquaCrop produces output txt files that can be difficult to process and visualize. AquaCropPlotter fills that gap by automatically reading the output simulations of AquaCrop and producing meaningful plots and statistics that the user can automatically embed into reports. 
AquaCropPlotter works with the outputs of both AquaCrop versions 6 and 7. Previous versions are not recognized by the app. 

## Where to find AquaCropPlotter

### Online
AquaCropPlotter is currently hosted in shinyapps.io so you do not need to install anything on your local computer. [Simply connect to the server](https://foodandagricultureorganization.shinyapps.io/AquaCropPlotter/). 

### Local installation through R
However, if you want to run AquaCropPlotter locally, simply install it from GitHub:

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
### Local installation through installer (Windows only)

If you are on Windows, you can also install the app locally by [executing the installer](https://github.com/Risk-Team/AquaCropPlotter/releases/download/standalone_v2.1.1/AquaCropPlotter_installer.exe)

## AquaCropPlotter workflow

Regardless of whether you use the app locally or online, the app would work in the same way.
![workflow](https://github.com/Risk-Team/AquaCropPlotter/assets/10773204/af494fcb-e21f-4f1f-ad3d-d23effc51c04)


## AquaCropPlotter Manual

The manual can be found [here](https://www.fao.org/3/cd0086en/cd0086en.pdf)


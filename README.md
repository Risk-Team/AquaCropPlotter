<h1 align="center">
<img src="logo.png" width = "400" height = "100" align="center" />
  <br>
  <h4 align="center">AquaCropPlotter: R package for visualizing AquaCrop model results</h4>
  <br>
<div align="center">
   <img src="https://img.shields.io/github/v/release/Risk-team/AquaCropPlotter" alt="GitHub R package version" style="display: inline-block;">
</div>
</h1>

## Status

**The app will be officially launched in October 2023. Training materials will be released soon**

## Background
AquaCrop is the crop-water productivity model developed by FAO. Currently, AquaCrop produces output txt files that can be difficult to process and visualize. AquaCropPlotter fills that gap by automatically reading the output simulations of AquaCrop and producing meaningful plots and statistics that the user can automatically embed into reports. 
AquaCropPlotter works with the outputs of both AquaCrop versions 6 and 7. Previous versions are not recognized by the app. 

## Where to find AquaCropPlotter

AquaCropPlotter is currently hosted in shinyapps.io so you do not need to install anything on your local computer. [Simply connect to the server](https://tntps.shinyapps.io/aquacropplotter/). However, if you want to run AquaCropPlotter locally, simply install it from GitHub:

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
## How to use the app

Regardless of whether you use the app locally or online, the app would work in the same way.

![workflow](https://user-images.githubusercontent.com/40058235/204907854-4cb0e6b5-6d20-4c73-8f71-5ead3dce22fc.png)
|:--:| 
| Workflow of AquaCropPlotter |

### Step 1: Upload the data

The first step when you launch the app is to upload the data containing AquaCrop output files (PRM and OUT files) (Fig 1). This data is also available from the AquaCropPlotter package (Fig 1) and it is stored in your computer once you install the app as described in the steps above. **If you run AquaCrop with the plug-in, the OUT and PRM files will be in two separate folders. Simply put them together before uploading the data in AquaCropPlotter. If you run AquaCrop without the plug-in, you still need to create the project file and upload the PRO files together with the OUT files.**

![image](https://user-images.githubusercontent.com/40058235/204923476-b684600c-dc9e-4d72-941a-488666447333.png)
|:--:| 
|*Fig 1*. Type of data required for visualization in AquaCropPlotter. This example data is also available locally when you install AquaCropPlotter. Run this line of code to localize the data: system.file("data/", package="AquaCropPlotter")|

Upload the data following the instructions in Figure 2. If you ran AquaCrop in plugin mode (like in this case) select plugin and then upload the files by selecting them in their respective folder. 

#### IMPORTANT

AquaCropPlotter will automatically identify the parameters you used for running AquaCrop. **However** certain variables cannot be detected automatically. For example, if you ran AquaCrop in different locations or different crops, you need to specify that in your file name separated by underscore. For instance, if you want to analyse the AquaCrop results of several crops, you could name your files as follow:  <br />
crop1_...  <br />
crop2_...  <br />

If you have several crops in several locations, you could name the files as follow:  <br />
crop1_location1_...  <br />
crop2_location2_...  <br />

**AquaCropPlotter allows you to load hundreds of files as long as you specify the file name correctly. check the help page for more information**

![step_1](https://user-images.githubusercontent.com/40058235/204923509-d43c87ad-fe62-4cb3-a7ae-1f412b6bf286.png)
|:--:| 
|*Fig 2*. The data is uploaded. AquaCropPlotter will automatically sort the PRM and OUT files. In this case, AquaCrop was run for the same crop but in different locations. The file name reflects this.|

### Step 2: Combine the data

AquaCropPlotter would now have created a combined dataset for all your AquaCrop simulations! In Figure 3 you can see the resulting data frame with all the columns. You can already download this data frame to your computer

![step_2](https://user-images.githubusercontent.com/40058235/204923560-757a5ecf-f403-4680-879b-214ffdbb9515.png)
|:--:| 
|*Fig 3*. AquaCropPlotter combines all the AquaCrop simulations into one data frame that can be exported in Excel and downloaded locally.|

AquaCropPlotter also allows you to change column names (variables that are detected automatically from the file names and also from the PRM files) and retain only certain variables (Figure 4). 

![step_3](https://user-images.githubusercontent.com/40058235/204923596-a4d2c93b-b8ea-4884-83ac-5a564e6881e1.png)
|:--:| 
|*Fig 4*. AquaCropPlotter allows great flexibility in terms of customizing variables and names.|


### Step 3: Plot

Finally, you are now able to visualize your results (Figure 5). This step is among the most flexible steps of AquaCropPlotter allowing you to customize your plots and analysis greatly. 

![step_4](https://user-images.githubusercontent.com/40058235/204923633-daefefe7-cd6f-4803-970c-0bad4b12f485.png)
|:--:| 
|*Fig 5*. AquaCropPlotter allows you to change the plot appearances and more!|

### Step 4: Analysis

In the last step of AquaCropPlotter you have the possibility of diving deeper into your AquaCrop results. For example, you can perform linear regression, get summary statistics for specific time frames, and more.


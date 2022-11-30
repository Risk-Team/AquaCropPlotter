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

### Step 1: Upload the data

The first step when you launch the app is to upload the data containing AquaCrop output files (PRM and OUT files) (Fig 1). This data is also availbale from the AquaCropVis package (Fig 1) and it is stored in your computer once you install the app as described in the steps above.

![image](https://user-images.githubusercontent.com/40058235/204909982-e593638b-bf36-4525-b93c-94eac98e727b.png)
|:--:| 
|*Fig 1*. Type of data required for visualization in AquaCropVis. This example data is also available locally when you install AquaCropVis. Run this line of code to localize the data: system.file("data/", package="AquaCropVis")|

Upload the data following the instruction in Figure 2. If you ran AquaCrop in plugin mode (like this case) select plugin and then upload the files by selecting them in their respective folder. 

#### IMPORTANT

AquaCropVis will automatically identify the parameters you used for running AquaCrop. **However** certain variables cannot be detected automatically. For example, if you ran AquaCrop in different locations or different crops, you need to specify that in your file name separated by underscore. For instance, if you want to analyse the AquaCrop results of several crops, you could name your files as follow:
crop1_...
crop2_...

If you have several crops in several locations, you could name the file as follow:
crop1_location1_...
crop2_location2_...

**AquaCropVis allows you to load hundreds of files as long as you specify the file name correctly.**

![step_1](https://user-images.githubusercontent.com/40058235/204911514-3eab9052-67eb-4121-99f3-698a9c477563.png)
|:--:| 
|*Fig 2*. The data is uploaded. AquaCropVis will automatically sort the PRM and OUT files. In this case, AquaCrop was run for the same crop but in different locations. The file name reflect this.|

### Step 2: Combine the data

AquaCropVis would now have created a combined dataset for all your AquaCrop simulations! In Figure 3 you can see the resulting data frame with all the columns. You can already download this data frame in your computer

![step_2](https://user-images.githubusercontent.com/40058235/204913512-895cad3a-7f66-43e4-827a-ae65db3d8231.png)
|:--:| 
|*Fig 3*. AquaCropVis combines all the AquaCrop simulations into one data frame that can be exported in excel and downloaded locally.|

AquaCropVis also allows you to change column name (variables that are detected automatically from the file names and also from the PRM files) and retain only certain variables (Figure 4). 

![step_2](https://user-images.githubusercontent.com/40058235/204915423-a9100498-3d72-4db9-bfcb-787b8e6623ea.png)
|:--:| 
|*Fig 4*. AquaCropVis allows great flexibility in terms of customizing variables and names.|


### Step 3: Plot

Finally, you are now able to visualize your results (Figure 5). This step is among the most flexible steps of AquaCropVis allowing you to customize your plots and analysis greatly. 

![step_3](https://user-images.githubusercontent.com/40058235/204919442-42d4754f-5154-4622-bc72-36698396c42a.png)
|:--:| 
|*Fig 5*. AquaCropVis allows you to change the plot appearances and more!|

### Step 4: Analysis

In the last step of AquaCropVis you have the possibility of diving deeper into your AquaCrop results. For example, you can perform linear regression, identify stages at which stresses are affecting your crop and even summarise information on specific variables (e.g yield) (Figure 6). 

![step_4](https://user-images.githubusercontent.com/40058235/204922458-cae2027b-49bd-4e32-b804-260db26433f4.png)
|:--:| 
|*Fig 6*. AquaCropVis allows you to change the plot appearances and more!|

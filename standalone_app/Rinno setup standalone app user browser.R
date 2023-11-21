#first need to install on the windows computer to be used to create the standalone app
#- Git (https://git-scm.com/download/win),
#- Nodejs (https://nodejs.org/en/download/current),
#- Inno set up (https://jrsoftware.org/isdl.php)
#install from installer available from their sites

#follow RInno package from  https://github.com/ficonsulting/RInno
#but there is a bug preventing R version 4 from working in the package, issue fixed with this branch so install from branch instead of main
install.packages("remotes")
library(remotes)
install_github("ficonsulting/RInno", ref = "fe8f2cd")

#run this script inside the app folder as working directory
library(RInno)

#set up components of the app in the folder
create_app(
  app_name    = "AquaCropPlotter",
  app_version = "1.0.0",
  pkgs        = c("installr","jsonlite", "shiny", "magrittr","shinydashboard", "tidyverse", "DT", "lubridate", "shinyjs", "shinyBS", "furrr", "broom", "scales"),  # CRAN-like repo packages
  include_R   = TRUE, # Download R and install it with your app, if necessary
  R_version   = "4.1.0",
  default_dir = "userdocs",
  user_browser = "ie",
  include_Chrome = FALSE,
  privilege = "lowest",
  setup_icon = "aquacropplotter_icon.ico",
  app_icon = "aquacropplotter_icon.ico",
  desktop_icon = FALSE,
  info_before = "info_before.txt",
  info_after = "info_after.txt")

#when almost done, R will ask to update packages, say No

#when done, replace icon (default.ico, setup.ico) in the current working directory and 
#and (icon.ico) in \nativefier-app\AquaCropPlotter-win32-x64\resources\app
#also replace infobefore, infoafter and licence with custom edited file.

#compile to make an installer
compile_iss()



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
library(RInno)

create_app(
  app_name    = "AquaCropPlotter", 
  #app_dir = "./try_shinyapp", #directory where the app folder is
  pkgs        = c("installr","jsonlite", "shiny", "magrittr","shinydashboard", "tidyverse", "DT", "lubridate", "shinyjs", "shinyBS", "furrr", "broom", "scales"),  # CRAN-like repo packages
  include_R   = TRUE) # Download R and install it with your app, if necessary

compile_iss()

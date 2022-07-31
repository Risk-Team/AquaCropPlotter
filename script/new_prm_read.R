library(tidyverse)
library(lubridate)

###import and process parameter files (prm) from the directory
param.data = 
  #get a list of prm file names from a directory
  data.frame(prm.file.list = list.files("../data/andrea")) %>%
  filter(str_detect(prm.file.list, "\\.PRM$")) %>%
  #read in each prm file from the list and extract parameter of interest
  mutate(parameter = map(prm.file.list, function(prm.file.list){
    #read in each prm file from the list
    prm.file = read_file(paste0("../data/andrea/", prm.file.list))
    #extract parameter from each param file 
    #in this case we use str_extract to get parameters of the first time point
    #if we use str_extract_all to get parameters of all simulation time points (should be all the same, but year of sowing date)
    #get sowing date
    sowing.date = str_extract(prm.file,"(?<=First day of simulation period - ).+(?=\\r\\n)") %>%
      unlist() %>%
      dmy()
    #get climate file
    climate.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.CLI\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")
    #get temperature file
    temperature.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.((Tnx)|(TMP))\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")    
    #get reference eto file
    reference.ET.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.ETo\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")  
    #get rain  file
    rain.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.PLU\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")  
    #get co2  file
    co2.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.CO2\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")  
    #get crop
    crop.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.CRO\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")
    #get irrigation
    irrigation.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.IRR\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")
    #get field management
    field.management.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.MAN\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")    
    #get soil
    soil.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.SOL\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")
    #get groundwater table
    groundwater.table.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.GWT\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")
    #get initial condition 
    initial.condition.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.SW0\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")
    #get offseason condition 
    offseason.condition.file = str_extract(prm.file, "(?<=\\s).+?(?=\\.OFF\\r\\n)") %>%
      unlist() %>%
      str_replace_all("\\s","")
    #put parameters together in a table
    param.table = data.frame(sowing.date, climate.file, temperature.file, reference.ET.file, rain.file, co2.file, crop.file, irrigation.file, field.management.file, soil.file, groundwater.table.file, initial.condition.file, offseason.condition.file)
  })) %>%
  unnest(parameter)

#check climate file for _ delimiter to extract different variables out of climate file name and give number
n.climate.file.var = str_split(param.data$climate.file,"_") %>%
  map(length) %>%
  unlist() %>%
  max()

param.data.split = param.data %>%
  separate(climate.file, into = paste0("climate.file.var",c(1:n.climate.file.var)), sep = "_", remove = FALSE)

#change colname
old.colname = "climate.file.var1"
new.colname = "xxxx"

colnames(param.data.split)[which(colnames(param.data.split) == old.colname)] = new.colname


####new regex extract simpler and fix bug that stop working
###import and process parameter files (prm) from the directory
param.data = 
  #get a list of prm file names from a directory
  data.frame(prm.file.list = list.files("../data/andrea")) %>%
  filter(str_detect(prm.file.list, "\\.PRM$")) %>%
  #read in each prm file from the list and extract parameter of interest
  mutate(parameter = map(prm.file.list, function(prm.file.list){
    #read in each prm file from the list
    prm.file = read_file(paste0("../data/andrea/", prm.file.list))
    #extract parameter from each param file 
    #in this case we use str_extract to get parameters of the first time point
    #if we use str_extract_all to get parameters of all simulation time points (should be all the same, but year of sowing date)
    #get sowing date
    sowing.date = str_extract(prm.file,"(?<=First day of simulation period - ).+") %>%
      unlist()
    #get climate file
    climate.file = str_extract(prm.file, ".+?\\.CLI") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.CLI)","")
    #get temperature file
    temperature.file = str_extract(prm.file, ".+?\\.((Tnx)|(TMP))") %>%
      unlist() %>%
      str_replace_all("(\\s)|((\\.Tnx)|(\\.TMP))","")    
    #get reference eto file
    reference.ET.file = str_extract(prm.file, ".+?\\.ETo") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.ETo)","")  
    #get rain  file
    rain.file = str_extract(prm.file, ".+?\\.PLU") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.PLU)","")  
    #get co2  file
    co2.file = str_extract(prm.file, ".+?\\.CO2") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.CO2)","")  
    #get crop
    crop.file = str_extract(prm.file, ".+?\\.CRO") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.CRO)","")  
    #get irrigation
    irrigation.file = str_extract(prm.file, ".+?\\.IRR") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.IRR)","")  
    #get field management
    field.management.file = str_extract(prm.file, ".+?\\.MAN") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.MAN)","")  
    #get soil
    soil.file = str_extract(prm.file, ".+?\\.SOL") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.SOL)","")  
    #get groundwater table
    groundwater.table.file = str_extract(prm.file, ".+?\\.GWT") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.GWT)","")  
    #get initial condition 
    initial.condition.file = str_extract(prm.file, ".+?\\.SW0") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.SW0)","")  
    #get offseason condition 
    offseason.condition.file = str_extract(prm.file, ".+?\\.OFF") %>%
      unlist() %>%
      str_replace_all("(\\s)|(\\.OFF)","")  
    #put parameters together in a table
    param.table = data.frame(sowing.date, climate.file, temperature.file, reference.ET.file, rain.file, co2.file, crop.file, irrigation.file, field.management.file, soil.file, groundwater.table.file, initial.condition.file, offseason.condition.file)
  })) %>%
  unnest(parameter)

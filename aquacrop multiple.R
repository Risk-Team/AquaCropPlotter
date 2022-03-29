library(tidyverse)
library(lubridate)

start.time = Sys.time()

###import and process output datasets from the directory
output.data = 
  #get a list of output file names from a directory
  data.frame(output.file.list = list.files("./output")) %>%
  #import dataset and clean up, format into dataframe
  mutate(dataset = map(output.file.list, function(output.file.list){
    #read in and clean up each data file from the list
    #modify file to convert variable number of spaces that separate values into tab so data can be loaded as tsv
    file.clean = read_file(paste0("./output/",output.file.list)) %>%
      #regex pattern to replace any number of space string before a non-space character (\\S) with a tab
      str_replace_all(" +?(?=\\S)","\t") 
      #format the cleaned data file into dataframe form
      #read in heading
      heading = read_lines(file = file.clean, skip = 2, n_max = 1) %>%
        str_split("\\t") %>%
        unlist() %>%
        #add last heading (parameter file (prm)) to the end of the list
        c(., "prm.file") 
      #read in data in tsv format, skipping heading lines, add headings from before
      data = read_tsv(file = file.clean, skip = 4, col_names = heading) %>%
        select(-1) #remove blank column in the first position
  })) %>%
  unnest(dataset) 

###import and process parameter files (prm) from the directory
param.data = 
  #get a list of prm file names from a directory
  data.frame(prm.file.list = list.files("./prm")) %>%
  #read in each prm file from the list and extract parameter of interest
  mutate(parameter = map(prm.file.list, function(prm.file.list){
    #read in each prm file from the list
    prm.file = read_file(paste0("./prm/", prm.file.list))
    #extract parameter from each param file 
    #in this case we use str_extract to get parameters of the first time point
    #if we use str_extract_all to get parameters of all simulation time points (should be all the same, but year of sowing date)
      #get sowing date
      sowing.date = str_extract(prm.file,"(?<=First day of simulation period - ).+(?=\\r\\n)") %>%
        unlist() %>%
        dmy()
      #get climate model
      climate.model = str_extract(prm.file, "(?<=\\s).+?(?=\\.CLI\\r\\n)") %>%
        unlist() %>%
        str_replace_all("\\s","")
      #from climate model, get location, rcp
      location = str_extract(climate.model, regex(".+(?=\\(rcp)", ignore_case = TRUE))
      rcp = str_extract(climate.model, regex("rcp[0-9]{2}", ignore_case = TRUE))
      #get crop
      crop = str_extract(prm.file, "(?<=\\s).+?(?=\\.CRO\\r\\n)") %>%
        unlist() %>%
        str_replace_all("\\s","")
      #get irrigation
      irrigation = str_extract(prm.file, "(?<=\\s).+?(?=\\.IRR\\r\\n)") %>%
        unlist() %>%
        str_replace_all("\\s","")
      #get soil
      soil = str_extract(prm.file, "(?<=\\s).+?(?=\\.SOL\\r\\n)") %>%
        unlist() %>%
        str_replace_all("\\s","")
      #put parameters together in a table
      param.table = data.frame(sowing.date, climate.model, location, rcp, crop, irrigation, soil)
  })) %>%
  unnest(parameter)
  
#add parameters to the output dataset
output.data = output.data %>%
  left_join(param.data, by = c("prm.file" = "prm.file.list"))
    
stop.time = Sys.time()

print(paste0("output files read: ", paste(length(list.files("./output")))))
print(paste0("prm files read: ", paste(length(list.files("./prm")))))
print(paste0("simulation data points read: ", paste(nrow(output.data))))
print(paste0("start: ", start.time, " | stop: ", stop.time, " | elaspe: ", paste(stop.time - start.time)))


#try plotting
ggplot(output.data, aes(x=Year1, y=Yield, group=irrigation, col=irrigation))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(rcp~interaction(location,soil))
  
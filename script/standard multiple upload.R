library(tidyverse)

data1 = data.frame(name = list.files("../output_standard"),
                  metadata = "a") %>%
  mutate(datapath = paste0("../output_standard/", name))
data2 = data.frame(name = list.files("../output_standard"),
                  metadata = "b") %>%
  mutate(datapath = paste0("../output_standard/", name))
data = bind_rows(data1, data2)

#list of file extensions of all output files (9 files)
file.extension = c("Clim.OUT","CompEC.OUT","CompWC.OUT","Crop.OUT","Inet.OUT","Prof.OUT","Run.OUT","Salt.OUT","Wabal.OUT")

#read data
#input$upload_data_files_standard %>% #for one upload  
data_load = data %>% #for multiple upload
  #detect file extensions
  mutate(extension = str_extract(name, paste(file.extension, sep="|"))) %>%
  mutate(dataset = map2(datapath, extension, function(datapath, extension){
    #read in data as lines, to identify blank lines that separate sections
    file.line = read_lines(paste0(datapath))
    line.before.data = which(file.line == "")[1] #first blank line comes before dataset
    line.after.data = which(file.line == "")[2] #second blank line comes after dataset
    
    #read in data as whole and clean spaces to tabs to allow read_tsv later
    file.clean = read_file(paste0(datapath)) %>%
      str_replace_all(" +?(?=\\S)","\t") 
    
    #read heading
    heading.skip = ifelse(extension == "Run.OUT", line.before.data, 
                          ifelse(extension %in% c("CompEC.OUT","CompWC.OUT"),line.before.data+2, line.before.data+1))
    heading = read_lines(file = file.clean, skip = heading.skip, n_max = 1) %>%
      str_split("\\t") %>%
      unlist() 
    
    #read in data as tsv, specify lines from blank lines sectioning identified before, add heading
    line.skip = ifelse(extension == "Run.OUT", line.before.data + 2, line.before.data + 3)
    line.n = ifelse(extension == "Run.OUT", 1, line.after.data - line.before.data - 4)
    data = read_tsv(file = file.clean, skip = line.skip, n_max = line.n, col_names = heading) %>%
      select(-1) #remove blank column at the start
  })) 


#daily data combined
data_load_daily = data_load %>%
  filter(extension != "Run.OUT") %>%
  #group_by(climate,location,rcp,irrigation,crop,soil,sowing_date,note) %>%
  group_by(metadata) %>%
  nest() %>%
  mutate(all_data = map(data, function(data){
    data$dataset %>%
      reduce(left_join, by = c("Day", "Month", "Year", "DAP", "Stage"))
  })) %>%
  select(-data) %>%
  unnest(all_data) %>%
  mutate(date = dmy(paste(Day, Month, Year, sep="-")))

#seasonal data combined
data_load_seasonal = data_load %>%
  filter(extension == "Run.OUT") %>%
  unnest(dataset) 
  #%>% select(-size, -type, -datapath) 

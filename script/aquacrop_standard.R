library(tidyverse)

###try reading and cleaning one output files
#read in data as lines, to identify blank lines that separate sections
file.line = read_lines("./output_standard/Maize_1206_RCP45_net_15April_LoamClim.OUT")
line.before.data = which(file.line == "")[1] #first blank line comes before dataset
line.after.data = which(file.line == "")[2] #second blank line comes after dataset

#read in data as whole and clean spaces to tabs to allow read_tsv later
file.clean = read_file("./output_standard/Maize_1206_RCP45_net_15April_LoamClim.OUT") %>%
  str_replace_all(" +?(?=\\S)","\t") 

#read heading
heading = read_lines(file = file.clean, skip = line.before.data+1, n_max = 1) %>%
  str_split("\\t") %>%
  unlist() 

#read in data as tsv, specify lines from blank lines sectioning identified before, add heading
line.skip = line.before.data + 3
line.n = line.after.data - line.before.data - 5
data = read_tsv(file = file.clean, skip = line.skip, n_max = line.n, col_names = heading) %>%
  select(-1) #remove blank column at the start

####
###import and process output datasets from the directory

#list of file extensions of all output files (9 files)
file.extension = c("Clim.OUT","CompEC.OUT","CompWC.OUT","Crop.OUT","Inet.OUT","Prof.OUT","Run.OUT","Salt.OUT","Wabal.OUT")

#read data
output.data = 
  #get a list of output file names from a directory
  data.frame(output.file.list = list.files("./output_standard")) %>%
  #detect file extensions
  mutate(extension = str_extract(output.file.list, paste(file.extension, sep="|"))) %>%
  mutate(dataset = map2(output.file.list, extension, function(output.file.list, extension){
    #read in data as lines, to identify blank lines that separate sections
    file.line = read_lines(file.path("./output_standard/", output.file.list))
    line.before.data = which(file.line == "")[1] #first blank line comes before dataset
    line.after.data = which(file.line == "")[2] #second blank line comes after dataset
    
    #read in data as whole and clean spaces to tabs to allow read_tsv later
    file.clean = read_file(file.path("./output_standard/", output.file.list)) %>%
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

#combined all daily data  
combined_data_daily = reduce(output.data$dataset[output.data$extension != "Run.OUT"], #filter out Run.OUT file as data format (seasonal)  different from others (daily)
                             left_join, by = c("Day", "Month", "Year", "DAP", "Stage"))
#seasonal data
combined_data_seasonal = as.data.frame(output.data$dataset[output.data$extension == "Run.OUT"])


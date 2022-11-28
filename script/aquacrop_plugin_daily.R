library(tidyverse)

###read in one output file for daily data 
#read in data as lines, clean up spaces to allow reading as tsv
file.clean = read_lines("../data/andrea/NO_0°-0%PRMday.OUT") %>%
  str_replace_all(" +?(?=\\S)","\t") 

#read heading, line 4
heading = file.clean[4] %>%
  str_replace_all("(?<=WC|ECe)\\t(?=[2-9])", "0") %>%
  str_split("\\t") %>%
  unlist() 

#find index of blank lines that separate sections, different Runs (seasons)
file.line.blank = which(file.clean == "") 
#find index of lines that are not data, starts from every blank line and 3 consecutive following lines, also remove first line
file.line.remove = c(1, file.line.blank, file.line.blank+1, file.line.blank+2, file.line.blank+3)
#recreate data file, remove unwanted lines 
file.clean = file.clean[-file.line.remove] %>%
  paste0(collapse = "\n") 

#read in data as tsv
data = read_tsv(file = file.clean, col_names = heading) %>%
  select(-1) #remove blank column at the start

#remove duplicated columns
heading[which(duplicated(heading))]


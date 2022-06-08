library(tidyverse)

data = data.frame(location = c("a","b","c"))

var = "location"
from = "a"
to = "x"

data_rename = data %>%
  mutate({{var}} := ifelse(.data[[var]] == from, to, .data[[var]]))


data[var][data[var]==from] = to

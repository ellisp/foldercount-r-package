library(foldercount)
library(tidyverse)

# shoudl be an error
count_lines_ext_v(binary_exts, lf)

count_files_ext_v(binary_exts, lf, label = "sally")

example(count_lines_ext_v)

lf <- tolower(list.files("social_housing-master", all.files = TRUE, recursive = TRUE, full.names = TRUE))
lf <- tolower(list.files("../forecastHybrid", all.files = TRUE, recursive = TRUE, full.names = TRUE))

tmp <- count_lines_ext_v(textfile_exts, lf, label = "frank")


tmp %>%
  group_by(ext) %>%
  summarise(number = n(),
            median = median(lines),
            mean = mean(lines),
            longest = max(lines))


systems <- c("social_housing-master", "../forecastHybrid")

the_system <- system[i]
count_files_ext_v(text_exts, the_system)

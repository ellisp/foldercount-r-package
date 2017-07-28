library(foldercount)
library(tidyverse)

# shoudl be an error
count_lines_ext_v(lf, binary_exts)

count_files_ext_v(lf, binary_exts, label = "sally")

example(count_lines_ext_v)

lf <- tolower(list.files("social_housing-master", all.files = TRUE, recursive = TRUE, full.names = TRUE))
lf <- tolower(list.files("../forecastHybrid", all.files = TRUE, recursive = TRUE, full.names = TRUE))

tmp <- count_lines_ext_v(lf, text_exts, label = "frank")


tmp %>%
  group_by(ext) %>%
  summarise(number = n(),
            median = median(lines),
            mean = mean(lines),
            longest = max(lines))


foldernames <- c("social_housing-master", "../forecastHybrid", "../ggseas")


folder_file_counts(foldernames, binary_exts)
folder_file_counts(foldernames, text_exts)


flc <- folder_line_counts(foldernames, text_exts)

flc %>%
  group_by(label, ext) %>%
  summarise(Files = n(),
            Mean = mean(lines),
            Median = median(lines),
            Sum = sum(lines),
            Max = max(lines),
            Min = min(lines),
            SD = sd(lines))
}

summary.flc(flc)

flc %>%
  ggplot(aes(x = label, weight = lines, fill = ext)) +
  geom_bar(position = "dodge")

View(x)


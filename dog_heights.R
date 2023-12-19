dog_breeds_height <- read_excel("dog_breeds_height.xlsx")


test <- dog_breeds_height %>%  # to convert character to numeric
  mutate(across(2:6, parse_number))


mean(test$`Male ave height cm`)

hist(test$`Male ave height cm`)


sum(test$`Male ave height cm`>60)/length(test$`Male ave height cm`)

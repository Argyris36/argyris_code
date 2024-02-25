dog_breeds <- read_excel("dog_breeds_height.xlsx")


dog_breeds <- dog_breeds %>%  # to convert character to numeric
  mutate(across(2:6, parse_number))


mean(dog_breeds$`Male ave height cm`)

hist(dog_breeds$`Male ave height cm`)


sum(dog_breeds$`Male ave height cm`>60)/length(dog_breeds$`Male ave height cm`)


dog_breeds[dog_breeds$Breed== "Beagle"]

fox_hound_vs_beagle <- dog_breeds %>% 
  filter(Breed == "Beagle " | Breed == "English Foxhound " )

knitr:: kable(fox_hound_vs_beagle )



sum(dog_breeds$`Male ave height cm`> 64  )/length(dog_breeds$`Male ave height cm`)

beagle = dog_breeds[dog_breeds$Breed == "Beagle ",]$`Male ave height cm`
engl_foxhound = dog_breeds[dog_breeds$Breed == "English Foxhound ",]$`Male ave height cm`
av_height_male_dogs <- mean(dog_breeds$`Male ave height cm`)
median_height_male_dogs <- median(dog_breeds$`Male ave height cm`)

dog_breeds %>% 
  ggplot(aes(`Male ave height cm`)) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = beagle, colour = "blue", linetype = "dashed")+
  geom_vline(xintercept = engl_foxhound, colour = "blue", linetype = "dotted")+
  geom_vline(xintercept = av_height_male_dogs, colour = "red")+
  geom_text(label = "beagle", x= beagle-2, y = 10, colour = "grey45", angle = 90, )+
  geom_text(label = "english foxhound", x= engl_foxhound-2, y = 10, colour = "grey45", angle = 90)+
  geom_text(label = "average", x= av_height_male_dogs-2, y = 10, colour = "grey45", angle = 90)+
  ggtitle("Male Dog Heights", subtitle = "Beagles Vs English Foxhounds")+
  xlab("Average Height for Various Male Dog Breeds in cm")




beagle_kg = dog_breeds[dog_breeds$Breed == "Beagle ",]$`Male ave weight kg`
engl_foxhound_kg = dog_breeds[dog_breeds$Breed == "English Foxhound ",]$`Male ave weight kg`
median_weight_male_dogs_kg <- median(dog_breeds$`Male ave weight kg`)

dog_breeds %>% 
  ggplot(aes(`Male ave weight kg`)) +
  geom_histogram(alpha = 0.5) +
  geom_vline(xintercept = beagle_kg, colour = "blue", linetype = "dashed")+
  geom_vline(xintercept = engl_foxhound_kg, colour = "blue", linetype = "dotted")+
  geom_vline(xintercept = av_height_male_dogs_kg, colour = "red")+
  geom_text(label = "beagle", x= beagle_kg-2, y = 10, colour = "grey45", angle = 90, )+
  geom_text(label = "english foxhound", x= engl_foxhound_kg-2, y = 10, colour = "grey45", angle = 90)+
  geom_text(label = "median", x= median_weight_male_dogs_kg-2, y = 10, colour = "grey45", angle = 90)+
  ggtitle("Male Dog Weights", subtitle = "Beagles Vs English Foxhounds")+
  xlab("Average Weight for Various Male Dog Breeds in kg")



sum(dog_breeds$`Male ave weight kg`>  engl_foxhound_kg )/length(dog_breeds$`Male ave weight kg`)


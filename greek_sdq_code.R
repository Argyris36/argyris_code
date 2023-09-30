my_packages <- c("tidyverse",  "foreign", "stringr", "haven", "readr",
                 
                 "openxlsx", "gginnards", "patchwork")



lapply(my_packages, library, character.only = TRUE)





greek_sdq <- read.csv("~/Downloads/greek_sdq.csv")

View(greek_sdq)





# first change name of three first variables

colnames(greek_sdq)[2] <- "name"

colnames(greek_sdq)[3] <- "gender"

colnames(greek_sdq)[4] <- "dob"



# drop the last three variables which are not relevant

greek_sdq <- greek_sdq[, - ((ncol(greek_sdq)-3):ncol(greek_sdq))]



# drop a few other columns that are unnecessary and not part of sdq scoring

greek_sdq <- greek_sdq[, -c(30, 32,33)]





# grab sdq variable names from sdq website 

x <- "consid

restles

somatic

shares

tantrum

loner

obeys

worries

caring

fidgety

friend

fights

unhappy

popular

distrac

clingy

kind

lies

bullied

helpout

reflect

steals

oldbest

afraid

attends

ebddiff

distres

imphome

impfrie

impclas

impleis"



# turn these into indivdual strings FANTASTIC FUNCTIONS BELOW, found them on STACK EXCH

sdq_names <- scan(text=x, what="")

sdq_names <- dput(sdq_names)

sdq_names

repository <- 0

for(i in 1: length(sdq_names)){
  
  repository[i] <- paste0("s", sdq_names[i])
  
}

sdq_names <- repository

sdq_names



# pass these names to turn the sdq variables into their official names to be able to use the code

colnames(greek_sdq)[5:length(colnames(greek_sdq))] <- sdq_names



# now change the variable labels

symptom_item_cols <- greek_sdq[,5: length(greek_sdq)] # use the abridged dataset incl only the sdq



sdq_symp_df <- matrix(NA, nrow = nrow(symptom_item_cols), ncol = ncol(symptom_item_cols)) # run a loop to pass the labels

for (i in 1: ncol(symptom_item_cols )){
  
  
  
  sdq_symp_df [,i] <-  dplyr::recode(symptom_item_cols [,i],
                                     
                                     "Δεν ισχύει"  = 0,"Όχι" =0,"Καθόλου"=0
                                     
                                     , "Ισχύει κάπως" =1 ,"Ναι- κάποιες δυσκολίες" =1,"Μόνο λίγο" =1
                                     
                                     ,"Ίσχύει σίγουρα"= 2, "Ισχύει σίγουρα"  =2, "Ναι- αρκετές δυσκολίες"=2  ,"Πάρα πολύ"=3,"Αρκετά"=2)
  
  sdq_symp_df <- data.frame(sdq_symp_df)
  
}





# now pass them back to the original columns for the sdq symptoms



greek_sdq[,5: length(greek_sdq)] <- sdq_symp_df



greek_sdq$gender <- gsub(".*(Α|A|a|α).*", "male", greek_sdq$gender)

greek_sdq$gender <- gsub(".*(K|Κ|k|κ).*", "female", greek_sdq$gender)

ifelse(greek_sdq$gender=="male", "male",ifelse(greek_sdq$gender=="female", "female", NA) )









# now find the ages

greek_sdq <- greek_sdq %>%
  
  mutate(new_dobs = format(as.Date(dob, '%d/%m/%Y'), '%Y-%m-%d'),
         
         new_dobs = gsub('/', '-', new_dobs))



greek_sdq$age <-  (as.Date(greek_sdq$new_dobs) - Sys.Date() )/365





greek_sdq[, 5:35]<-sapply(greek_sdq[, 5:35],as.numeric)

greek_sdq$age <- as.numeric(greek_sdq$age)



attach(greek_sdq) ###remeber to turn to numeric

# Recoding variables and then scoring the youth self-report SDQ scores





robeys <- dplyr::recode(sobeys, "0"=2, "1"=1, "2"=0)

rreflect  <- dplyr::recode(sreflect, "0"=2, "1"=1, "2"=0)

rattends <- dplyr::recode(sattends, "0"=2, "1"=1, "2"=0)

rfriend <- dplyr::recode(sfriend, "0"=2, "1"=1, "2"=0)

rpopular <- dplyr::recode(spopular, "0"=2, "1"=1, "2"=0)





rrdistres <- dplyr::recode (sdistres, "0" = 0, "1" = 0 , "2"=1, "3"=2)

rrimphome <- dplyr::recode (simphome, "0" = 0, "1" = 0 , "2"=1, "3"=2)

rrimpfrie <- dplyr::recode (simpfrie, "0" = 0, "1" = 0 , "2"=1, "3"=2)

rrimpclas <- dplyr::recode (simpclas, "0" = 0, "1" = 0 , "2"=1, "3"=2)

rrimpleis <- dplyr::recode (simpleis, "0" = 0, "1" = 0 , "2"=1, "3"=2)







df.semotion <- data.frame(ssomatic, sworries, sunhappy, sclingy, safraid)

snemotion <- apply(df.semotion, 1, function(x) sum(is.na(x)))

semotion <- ifelse(snemotion<3, rowMeans(df.semotion, na.rm=TRUE), NA)

semotion <- as.numeric(semotion) * 5

semotion <- floor(0.5 + semotion)



df.sconduct <- data.frame(stantrum, robeys, sfights, slies, ssteals)

snconduct <- apply(df.sconduct, 1, function(x) sum(is.na(x)))

sconduct <- ifelse(snconduct<3, rowMeans(df.sconduct, na.rm=TRUE), NA)

sconduct <- as.numeric(sconduct) * 5

sconduct <- floor(0.5 + sconduct)



df.shyper <- data.frame(srestles, sfidgety, sdistrac, rreflect, rattends)

snhyper <- apply(df.shyper, 1, function(x) sum(is.na(x)))

shyper <- ifelse(snhyper<3, rowMeans(df.shyper, na.rm=TRUE), NA)

shyper <- as.numeric(shyper) * 5

shyper <- floor(0.5 + shyper)



df.speer <- data.frame(sloner, rfriend, rpopular, sbullied, soldbest)

snpeer <- apply(df.speer, 1, function(x) sum(is.na(x)))

speer <- ifelse(snpeer<3, rowMeans(df.speer, na.rm=TRUE), NA)

speer <- as.numeric(speer) * 5

speer <- floor(0.5 + speer)



df.sprosoc <- data.frame(sconsid, sshares, scaring, skind, shelpout)

snprosoc <- apply(df.sprosoc, 1, function(x) sum(is.na(x)))

sprosoc <- ifelse(snprosoc<3, rowMeans(df.sprosoc, na.rm=TRUE), NA)

sprosoc <- as.numeric(sprosoc) * 5

sprosoc <- floor(0.5 + sprosoc)



df.simpact <- data.frame(sdistres, simphome, simpfrie, simpclas, simpleis)

snimpact <- apply(df.simpact, 1, function(x) sum(is.na(x)))

simpact <- ifelse(!snimpact==5, rrdistres+rrimphome+rrimpfrie+rrimpclas+rrimpleis, NA)

simpact <- ifelse(sebddiff==0, 0, simpact)

simpact <- as.numeric(simpact)



sebdtot <- semotion+sconduct+shyper+speer



rm (robeys, rreflect, rattends, rfriend, rpopular, rrdistres, rrimphome, rrimpfrie, rrimpclas, rrimpleis, snemotion, snconduct, snhyper, snpeer, snprosoc, snimpact, df.semotion, df.sconduct, df.shyper, df.speer, df.sprosoc, df.simpact)



greek_sdq <- data.frame(cbind(greek_sdq, semotion, sconduct, shyper, sprosoc, speer, simpact, sebdtot))



####note several people have put either no date or today's date or a future date as DOB

greek_sdq$age <- -(greek_sdq$age)

greek_sdq$age  <- ifelse(greek_sdq$age>18 | greek_sdq$age<12  , NA, greek_sdq$age)

mean(greek_sdq$age, na.rm= T)

range (greek_sdq$age , na.rm= T)



# there are also some rogue gender data, hence the following trick

table(greek_sdq$gender)[4]/(table(greek_sdq$gender)[4]+table(greek_sdq$gender)[5])



# here is the percentage above the 90 percentile

perc_above_thresh_sebdtot <- 100*(length(greek_sdq$sebdtot[greek_sdq$sebdtot>17])/length(greek_sdq$sebdtot[!is.na(greek_sdq$sebdtot)]))

greek_sdq %>% ggplot(aes(sebdtot))+
  
  geom_histogram(binwidth = 1)+
  
  xlim (0,40)+
  
  
  ylim(0,70) + 
  
  geom_vline(xintercept = 17, colour = "red", linetype = "dashed", size = 1) +
  
  labs(title = "Overall mental health problems",
       subtitle ="red line threshold is 90th percentile in typical population samples", 
       caption = "Data source: Mazi Schools Project")+
  

  xlab("total psychopathology score")+
  
  ylab("") +
  
  annotate("text",x = 17 + 5, y = 50, label = paste(round(perc_above_thresh_sebdtot,1),  "%\n above threshold") ) +
  theme_light()

###plot SDQ subscales
target_variables <- c("semotion", "sconduct", "shyper", "speer")

# create title and cutoff lists
titles <- c("emotional symptoms", "conduct symptoms", "hyperactivity", "peer problems")
cutoffs <- c(5, 4, 6, 3)

# # get proportions above cutoff

perc_above_threshold <- 0
for (i in 1: length(target_variables)){
  
perc_above_threshold[i] <-  100* (sum(greek_sdq[,target_variables[i]] > cutoffs[i], na.rm = T)/
                                       sum(!is.na(greek_sdq[,target_variables[i]])))
   
 }
 
 perc_above_threshold

# create a smaller dataset with what you want to plot
df_plot_sdq <- greek_sdq %>% 
  select(Timestamp, semotion, sconduct, shyper, speer) %>% 
  drop_na()
  
# reshape it to use facets rather than loops
library(reshape2)
df_plot_sdq_resh <-melt(df_plot_sdq, idvar = "Timestamp", variable.name  = "subscales" , value.name = "score")

head(df_plot_sdq_resh)

# create dataset to add line and text to each facet
df_threshold <- data.frame(subscales = unique(df_plot_sdq_resh$subscales), line = cutoffs, 
                           text = paste(round(perc_above_threshold,1), "%\nabove threshold" ) )

# create vector to relabel facets
new_labels <- c("semotion" = "emotion problems", "sconduct" = "conduct problems", 
                "shyper" = "hyperactivity problems", "speer" = "peer problems")

df_plot_sdq_resh %>% 
  ggplot(aes(score))+
  geom_histogram(binwidth = 1.2) +
  facet_wrap(~subscales, labeller = labeller(subscales = new_labels)) +
  geom_vline(data = df_threshold,aes(xintercept = line), linetype = "dashed", colour = "red" ) +
  labs(title = "Youth scores on different problem domains",
        subtitle ="red line threshold is 90th percentile in typical population samples", 
       caption = "Data source: Mazi Schools Project")+
  geom_text(
    data    = df_threshold,
    mapping = aes(x = line+2.7, y = 200, label = text),
  ) +
theme_light()+
  theme(axis.title.y=element_blank())

  
 
  




################################END OF SDQ WORK #################################

##USETHIS: tidyr::separate(df, datetime, c("date", "time"), sep = " ")

# here is the satisfaction stuff

library(readxl)
greek_feedback <- read_excel("greek_feedback_May_2023.xlsx")

# keep only those columns with new data (since implementing changes see below)
greek_feedback <- greek_feedback %>% 
  select(c(1,8,13,18, 19, 20, 21, 22))
View(greek_feedback )


# will want to keep only the rows since the implementation of changes to the questionnaire. This was in November 2022

greek_feedback <- separate(greek_feedback, Timestamp, c("date", "time"), sep = " ", remove = FALSE)



greek_feedback <- greek_feedback %>% 
  filter(date >= "2022-11-15")


#now rename the columns
greek_feedback <- greek_feedback %>% 
  rename(useful = 4, new_knowledge = 5,
         open_comments = 6, communication = 7, 
         preparation = 8, accessibility = 9, 
         answered_questions = 10)

# now create subset for analyses
greek_feedback_subset <- greek_feedback %>% 
  select(c(Timestamp, useful, new_knowledge, communication, preparation, accessibility, answered_questions))

# turn to factor
greek_feedback_subset[,2:7]<-  lapply(greek_feedback_subset[,2:7], factor)

View(greek_feedback_subset )


# now relabel the factor levels

#Note that in the Answered Questions column there are "4,5" values. Turn these to NA
greek_feedback_subset$answered_questions <- recode_factor(greek_feedback_subset$answered_questions, "4, 5"= NA_character_)
greek_feedback_subset$preparation<- recode_factor(greek_feedback_subset$preparation, "4, 5"= NA_character_)

cols_to_update <- colnames(greek_feedback_subset[,-1])

greek_feedback_subset[, cols_to_update] <-
  lapply(greek_feedback_subset[,cols_to_update], function(x)
    plyr::revalue(x, c("1" = "not at all",
                       "2" = "a bit",
                       "3" = "fairly good",
                       "4" = "very good",
                       "5" = "excellent")))


#create a table for the numbers of 
n_table_feedback <- matrix(NA, nrow = 5 , ncol = 6)

for(i in 1: length(cols_to_update)){

n_table_feedback[,i ]<- unname(table(greek_feedback_subset[,cols_to_update[i]]))


}
n_table_feedback<- data.frame(n_table_feedback)
colnames(n_table_feedback) <- cols_to_update
n_table_feedback <- cbind(categories =levels(greek_feedback_subset$useful), n_table_feedback)
n_table_feedback 


n_table_feedback_perc <- matrix(NA, nrow = 5 , ncol = 6)

for(i in 1: length(cols_to_update)){
  
  n_table_feedback_perc[,i ]<- round(100*(unname(table(greek_feedback_subset[,cols_to_update[i]]))/
    sum(unname(table(greek_feedback_subset[,cols_to_update[i]])))),1)
  
  
}
n_table_feedback_perc<- data.frame(n_table_feedback_perc)
colnames(n_table_feedback_perc) <- cols_to_update
n_table_feedback_perc <- cbind(categories = levels(greek_feedback_subset$useful), n_table_feedback_perc)
n_table_feedback_perc 

knitr::kable(head(n_table_feedback_perc), "simple")

library(reshape2)

n_table_feedback_perc_resh <-n_table_feedback_perc %>% 
  drop_na() %>% 
  melt( id.vars = "categories", variable.name  = "questions" , value.name = "percent")

head(n_table_feedback_perc_resh)
dim((n_table_feedback_perc_resh))


# reorder levels of the factor
n_table_feedback_perc_resh$categories <-
  factor(n_table_feedback_perc_resh$categories, levels = c("not at all",
                                                           "a bit",
                                                           "fairly good",
                                                           "very good",
                                                           "excellent"))

labels_for_facet <- c("useful" = "seminar was useful",
                      "new_knowledge" = "learnt new stuff",
                      "communication"= "content well communicated",
                      "preparation" = "team well prepared",
              
                      "accessibility" = "info was accessible",
                      "answered_questions" = "team answered question")



n_table_feedback_perc_resh %>% 
ggplot(aes(x = categories, y =  percent, fill = categories)) +
geom_col() +
  facet_wrap(~questions,labeller = as_labeller(labels_for_facet) )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Youth Feedback",
       subtitle ="answers to questions about seminar quality", 
       caption = "Data source: Mazi Schools Project") +
  theme(axis.title.x=element_blank())+
  ylab("%")+
  geom_text(aes(label = percent), vjust = -0.2, size = 3)+
  theme_light()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(axis.title.x=element_blank())



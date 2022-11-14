my_packages <- c("tidyverse", "lme4", "foreign", "stringr", "haven", "readr",
                  "openxlsx")
lapply(my_packages, require, character.only = TRUE)


greek_sdq <- read.csv("~/Downloads/SDQ-Hel A11-17 (Responses) - Form responses 1.csv")
  View(`SDQ.Hel.A11.17.(Responses)...Form.responses.1`)


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
length(greek_sdq$sebdtot[greek_sdq$sebdtot>17])/length(greek_sdq$sebdtot[!is.na(greek_sdq$sebdtot)])

# and now an example plot
greek_sdq %>% ggplot(aes(sebdtot))+
  geom_histogram()+
  ylim (0,20)+
  geom_vline(xintercept = 17, colour = "red", linetype = "dashed", size = 1) +
  ggtitle("Severity of Psychopathology in Grava \ntotal sample n = 184 ") +
   annotate("text", x = 24, y = 17, label = "red line indicates \n90% of community sample \nthreshold\n
            we have about 20% \nabove that threshold")+
  xlab("total psychopathology score")+
  ylab("")
 
################################END OF SDQ WORK #################################

# here is the satisfaction stuff

greek_feedback <- read_excel("~/Downloads/έντυπο αξιολόγησης (Responses).xlsx")
View(greek_feedback )

# column names
y <-
"Timestamp
Put_x
id
overall
time
aims
met_expect
helpful
understand_self
self_conf
general_assess
facilitator_assess
Knowledge
Communication
preparation
Teaching
Prep_answer
comments
"

qual_names <- scan(text=y, what="")
qual_names <- dput(qual_names)
qual_names
repository <- 0
for(i in 1: length(qual_names)){
  repository[i] <- paste0("s", qual_names[i])
}
qual_names <- repository
qual_names

colnames(greek_feedback) <- qual_names



z <- 
"Αρκετα
Πολύ
Καθόλου
Εξαιρετική
Πολύ Καλή
Καλή 
Μέτρια
"

# again use these lovely functions to create array with individual symptoms
var_labs <- scan(text=z, what="")
var_labs  <- dput(var_labs )
var_labs 

greek_feedback <- data.frame(greek_feedback) # this needed doing for some odd reason.
greek_feedback[,1] <- as.numeric(greek_feedback[,1])

labs_df <- matrix(NA, nrow = nrow(greek_feedback), ncol = ncol(greek_feedback)) # run a loop to pass the labels

for (i in 1: ncol(greek_feedback)){
  
  labs_df [,i] <-  dplyr::recode(greek_feedback[,i],
                                 
                                 "Αρκετά"   = "medium",
                                 "Αρκετά, Λίγο"="a little",
                                 
                                 "Πολύ, Αρκετά"="medium",
                                 "Εξαιρετική, Πολύ καλή"= "very good",
                                 
                                 "Πολύ"       = "a lot",
                                 
                                 "Καθόλου"    = "not at all" , 
                                 "Καθολου"    = "not at all" ,
                                 
                                 "λίγο, Καθολου"    = "not at all" ,
                                 
                                 "Εξαιρετική"="excellent",
                                 
                                 "Πολύ καλή"      ="very good",  
                                 
                                 "Καλή"       = "good",
                                 
                                 "Μέτρια"    ="medium",
                                 
                                 "Λίγο"= "a little",
                                 
                                 "Λίγο,Αρκετά"= "a little",
                                 
                                 "Λίγο Καθόλου"= "a little",
                                 
                                 "Λίγο, Καθόλου" = "a little",
                                 .default = NA_character_)
                                 
                                 # "Πολύ καλή, Πολύ κακή" = "NA",
                                 # 
                                 # "Καλή, Μέτρια"=   "medium",
                                 # 
                                 # "Πολύ, Καθολου"=  "NA",
                                 # "Πολύ, Λίγο"=  "NA",
                                 # "Παρατηρήσεις"=   "NA",
                                 # "Πολύ, Αρκετά, Λίγο"=  "NA")
                                 
  
  labs_df <- data.frame(labs_df)
  
  
}


colnames(labs_df) <- colnames(greek_feedback)

greek_feedback[, 5: length(greek_feedback)] <- labs_df[,5:length(labs_df)]



# grab just the columns you need from 5: 17
feedback_cols <- greek_feedback[,5:11]
naming_list <-  c("time", "aims",
                  "met_expect",
                  "helpful",
                  "understand_self",
                  "self_conf",
                  "general_assess")

df_feedback <- list()

for(i in 1: ncol(feedback_cols)){
  
df_feedback[[i]] <-  feedback_cols %>%  count(feedback_cols[,i]) 



  
}

  
for(i in 1: length(df_feedback)){
  
  df_feedback[[i]] <-  df_feedback[[i]] %>% filter(!is.na(`feedback_cols[, i]`))
  
}


for(i in 1: length(df_feedback)){
df_feedback[[i]] <-  df_feedback[[i]] %>% mutate (perc = (n/sum(n)*100))

}

names(df_feedback) <- naming_list

for(i in 1: length(df_feedback)){
# names(df_feedback[[i]][names(df_feedback[[i]]) == "feedback_cols[, i]"]) <- names (df_feedback)[i]

colnames(df_feedback[[i]])[1] <- names(df_feedback)[i]
}

write.xlsx(df_feedback, '/Users/stringarisa/Downloads/df_feedback.xlsx')


# for(i in 1: length(df_feedback)){
#   df_feedback[[i]] %>% 
#     ggplot(aes(y = perc, x = factor(saims, c("a little", "medium", "a lot")), 
#                fill = factor(saims, c("a little", "medium", "a lot")))) +
#     geom_bar(stat="identity") +
#     xlab ("") +
#     ylab("percent")+
#     theme(legend.title=element_blank()) +
#     ggtitle("Aims of seminar")
#   
# }

examp_bplot <- df_feedback$helpful %>% ggplot(aes(y = perc, x = factor(helpful, c("not at all", "a little", "medium", 
                                                                                        "a lot")), 
                         fill = factor(helpful, c("not at all", "a little", "medium", "a lot")))) +
  geom_bar(stat="identity") +
  xlab ("") +
  ylab("percent")+
  theme(legend.title=element_blank()) +
  ggtitle("Was the seminar helpful?")


examp_bplot 

examp_bplot_2 <- df_feedback$helpful %>% ggplot(aes(y = perc, x = factor(helpful, c("not at all", "a little", "medium", 
                                                                                  "a lot")), 
                                                  fill = factor(helpful, c("not at all", "a little", "medium", "a lot")))) +
  geom_bar(stat="identity") +
  xlab ("") +
  ylab("percent")+
  theme(legend.title=element_blank()) +
  ggtitle("Was the seminar helpful?")


examp_bplot_2  <- df_feedback$general_assess %>% ggplot(aes
                                                        (y = perc, 
                                                          x = factor(general_assess, c("medium", "good", "very good", 
                                                                                     "excellent")), 
                                                     fill = factor(general_assess, c("medium", "good", "very good", 
                                                                              "excellent")))) +
  geom_bar(stat="identity") +
  xlab ("") +
  ylab("percent")+
  theme(legend.title=element_blank()) +
  ggtitle("General Assessment")

examp_bplot_2 



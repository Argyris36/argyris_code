# code for emotion preferences in the CAT-D

library(tidyverse)
#import data
demo_res_20220114 <- read.csv("~/Downloads/mood_opinions_data/catd_mood_opinions_data/demo_res_20220114.csv")
MASTER_DATABASE_BEHAVIOURAL_14_Nov_2021 <- read.csv("~/Downloads/MASTER_DATABASE_BEHAVIOURAL_14_Nov_2021.csv")
mfq_res_20220104 <- read.csv("~/Downloads/mfq_res_20220104.csv")
pairwise_res_20220114 <- read.csv("~/Downloads/mood_opinions_data/catd_mood_opinions_data/pairwise_res_20220114.csv")
filter_res_20220114 <- read.csv("~/Downloads/mood_opinions_data/catd_mood_opinions_data/filter_res_20220114.csv")

# get ids of CAT-D adolescents who participated in the emotion preference study to get participant type
# i.e. whether depressed or not
ids <- unique(demo_res_20220114$SDAN)

part_type <- MASTER_DATABASE_BEHAVIOURAL_14_Nov_2021 %>% 
  filter( SDAN %in%  ids) %>% 
  dplyr::select(SDAN, Participant_Type, DOB) 
 
part_type <- part_type[!duplicated(part_type$SDAN),]

# also get age
part_type$age <-round(as.numeric((as.Date("2022-01-14") - as.Date(part_type$DOB,"%d/%m/%Y" ))/365),0)
part_type <- subset(part_type, select = -DOB )


# get mfq score for each participant
mfq_df <- mfq_res_20220114 %>% 
  dplyr::select(MFQ_response, id) %>% 
  group_by(id) %>% 
  mutate(mfq_sum = sum(MFQ_response))

mfq_df <- mfq_df[!duplicated(mfq_df$id),]
mfq_df

#now merge them
#first the participant type and age
demo_res_new <- merge(demo_res_20220114, part_type, by = "SDAN")
demo_res_new <- subset(demo_res_new, select = -age.x )
demo_res_new$age <- demo_res_new$age.y
demo_res_new <- subset(demo_res_new, select = -age.y )
head(demo_res_new)

demo_res_new <- merge(demo_res_new, mfq_df, by = "id")
dim(demo_res_new)
demo_res_new <- subset(demo_res_new, select = c(id, SDAN, sex, code, complete, Participant_Type, age, 
                                                mfq_sum))
head(demo_res_new)

# now get some item metrics
n_items <- 14

ranks <- pairwise_res_20220114 %>% 
  count(winner)

# now get the ranks of the items.
for (i in 1: nrow(ranks)){
  
  ranks$perc[i] <-   prop.test (ranks $n[i], (n_items-1)*nrow(demo_res_new))$estimate[[1]]
  ranks$lower[i] <-   prop.test (ranks $n[i], (n_items-1)*nrow(demo_res_new))$conf.int[[1]]
  ranks$upper[i] <-   prop.test (ranks $n[i], (n_items-1)*nrow(demo_res_new))$conf.int[[2]]
 
}

ranks

# now also add names of items
filter_res_20220114[1:14,]$item
ranks <- data.frame(cbind(ranks,items = filter_res_20220114[1:14,]$item))

# and make more beautiful
ranks <- ranks %>% 
  rename(
    item_label = items, 
    item_number = winner,
    n_wins = n,
    lower_ci = lower,
    upper_ci = upper
  )
ranks


# and this is the way to get the comparison of proportions



sig_rank <- data.frame(cbind(data.frame(t(combn(ranks$item_number,2 )))),
                       data.frame(t(combn(ranks$n_wins,2 ))))


sig_rank <- sig_rank %>% 
rename(
  
  item_1 = X1, 
  item_2 = X2, 
  wins_1 = X1.1,
  wins_2 = X2.1
  
)


p_val <- 0
for(i in 1:nrow(sig_rank)){
  
  p_val[i] <- prop.test(c(sig_rank$wins_1[i], sig_rank$wins_2[i]), c(962, 962))$p.value
  
}

sig_rank <- data.frame(sig_rank, p_val)
sig_rank


# now create a vector displaying the winning items from the competition 
winning_items <- 0

for(i in 1:nrow(sig_rank)){
  if ((sig_rank$wins_1[i] > sig_rank$wins_2[i])==TRUE)  {
    winning_items[i]<- sig_rank$item_1[i]
  } else {
    if((sig_rank$wins_1[i] < sig_rank$wins_2[i])==TRUE){ 
      winning_items[i]<- sig_rank$item_2[i]
    } else{
      winning_items[i]<- print("equality")
    }  
  }
}

# add the p-values next to it
df_sig <-data.frame(winning_items, p_value = sig_rank$p_val)



# now filter out the significant ones and count per item
# first show how many significant and give a percentage of it
df_sig %>% 
  filter(p_value<0.05) %>% 
  count %>% 
  mutate(perc = n/((n_items*(n_items-1))/2))

# then show which ones they are by item. 
items_rank_by_sig <- df_sig %>% 
  filter(p_value<0.05) %>% 
  group_by(winning_items) %>% 
  count


items_rank_by_sig <- items_rank_by_sig[order(as.numeric(as.character(items_rank_by_sig$winning_items))),]

# realise 12 is missing from the list because it had zero wins. Will insert it.
item_12_df= data.frame(winning_items  ="12", n =  0)
items_rank_by_sig <- rbind(items_rank_by_sig , item_12_df)
items_rank_by_sig <- items_rank_by_sig[order(as.numeric(as.character(items_rank_by_sig$winning_items))),]
colnames(items_rank_by_sig) <- c("winning_items", "n_signiricant_wins")
# and here is the final list with number of significant wins at p<0.05 
# CAVE: there may be others missing if your p-value is more stringent
# add labels
items_rank_by_sig <- data.frame(items_rank_by_sig, item_labels = ranks$item_label)
# rank by n_wins
items_rank_by_sig <- items_rank_by_sig[order(as.numeric(as.character(items_rank_by_sig$n_signiricant_wins))),]
# final
items_rank_by_sig 





# 
# 
# 
# 
# ### trying to work with the CAT-D dataset that is online: 
# # https://github.com/transatlantic-comppsych/CATD-study/blob/main/behavioural_database.csv
# # but having difficulties.
# test <- behavioural_database %>% 
#   filter( SDAN %in%  ids) %>% 
#   dplyr::select(SDAN, Participant_Type) 
# dim(test)
#   
# 
# test <- test[!duplicated(test$SDAN),]
# 
# 
# sum((unique(behavioural_database$SDAN) %in% ids)==TRUE)
# 
# setdiff(ids, unique(behavioural_database$SDAN))
# 
# testaki<-0
# testara<-0
# pvalues<-0
# for (i in 1:length(ranks$item_number)){
#   for (j in 1: length(ranks$item_number)){
#     
#     testaki[i]<- ranks$n_wins[i]
#     testara[i]<-ranks$n_wins[i+1]
#     
#     pvalues[i]<-   prop.test(x=c(testaki[i], testara[i]),n = c(962, 962))$p.value
#     
#   }
#   
# }
# 
# 
# x<-c(1,2,3,2,1,3,4,2,5,2,6,5,5)
# y<-c(5,5,6,2,1,4,4,2,1,2,1,5,5)
# 
# x<-seq(from = 1, to = 14)
# y<-seq(from = 14, to = 1)
# res<-cor.test(x,y, method="kendall")
# res                 

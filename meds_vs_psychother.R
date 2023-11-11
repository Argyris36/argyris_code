library(tidyverse)
library(readxl)
library(lme4)

#Dataset_for_Argyris_25_10_23 <- read_excel("~/Downloads/Dataset_for_Argyris_25.10.23.xlsx")

Dataset_for_Argyris_25_10_23 <- read_excel("~/Downloads/Dataset_for_Argyris_25.10.23-2.xlsx")

Dataset_for_Argyris_25_10_23$study_n <- as.numeric(Dataset_for_Argyris_25_10_23$study_n)


Dataset_for_Argyris_25_10_23 <- Dataset_for_Argyris_25_10_23 %>% 
  mutate(efficacy = as.numeric(active_response_rate)-as.numeric(control_response_rate)) 

Dataset_for_Argyris_25_10_23$active_response_rate <-as.numeric(Dataset_for_Argyris_25_10_23$active_response_rate)
Dataset_for_Argyris_25_10_23$control_response_rate <-as.numeric(Dataset_for_Argyris_25_10_23$control_response_rate)


Dataset_for_Argyris_25_10_23$psy_or_med <-as.factor(Dataset_for_Argyris_25_10_23$psy_or_med)
Dataset_for_Argyris_25_10_23$psy_or_med <- Dataset_for_Argyris_25_10_23$psy_or_med %>% 
  case_match("0" ~ "anti-depressants", 
         "1" ~ "psychological therapy")
  
# mean_ci_efficacy <- Dataset_for_Argyris_25_10_23 %>% 
#   group_by(psy_or_med) %>% 
#   summarise(mean_cl_boot(efficacy))

Dataset_for_Argyris_25_10_23 %>% 
  ggplot(aes(x = as.factor(psy_or_med) , y = efficacy) )+
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
  labs (x = "", y = "response rate % overall (active - control)")  +
  ggtitle("response rates for anti-depressants vs psychotherapy", subtitle = "Overall Response Rate: active - control")+
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=12))
 

Dataset_for_Argyris_25_10_23 %>% 
  ggplot(aes(x = as.factor(psy_or_med) , y = active_response_rate) )+
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
  labs (x = "", y = "response rate % active only")  +
  ggtitle("response rates for anti-depressants vs psychotherapy", subtitle = "Active Response Rate")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))


Dataset_for_Argyris_25_10_23 %>% 
  ggplot(aes(x = as.factor(psy_or_med) , y = control_response_rate) )+
  geom_point() +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
  labs (x = "", y = "response rate % control only") +
  ggtitle("response rates for anti-depressants vs psychotherapy", subtitle = "Control Response Rate")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

dim(Dataset_for_Argyris_25_10_23)

Dataset_for_Argyris_25_10_23_long <- pivot_longer(Dataset_for_Argyris_25_10_23, 
                                                  ends_with("rate"), names_to = "type_response_rate", 
                                                  values_to = "response_rate", ) 

dim(Dataset_for_Argyris_25_10_23_long)



sum_stats_long_psy_med <- Dataset_for_Argyris_25_10_23_long %>% 
  group_by(psy_or_med, type_response_rate) %>% 
  summarise(n = n(), avg_resp_rate = mean(response_rate, na.rm = T), sd_resp_rate = sd(response_rate, na.rm = T))
sum_stats_long_psy_med  


# as histograms
Dataset_for_Argyris_25_10_23_long %>% 
  ggplot(aes(response_rate))+
  geom_histogram(aes(fill = psy_or_med), alpha = 0.4)+
  facet_wrap(~type_response_rate)+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression")


# as histograms without WL
Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  ggplot(aes(response_rate))+
  geom_histogram(aes(fill = psy_or_med), alpha = 0.4)+
  facet_wrap(~type_response_rate)+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression")


# as violin plots
Dataset_for_Argyris_25_10_23_long %>% 
  ggplot(aes(x = type_response_rate, y = response_rate))+
  geom_violin(aes(fill = psy_or_med), alpha = 0.4)+
  geom_point(aes(colour = psy_or_med ), position = position_jitterdodge ())+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression")



# as violin plots without WL
Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  ggplot(aes(x = type_response_rate, y = response_rate))+
  geom_violin(aes(fill = psy_or_med), alpha = 0.4)+
  geom_point(aes(colour = psy_or_med ), position = position_jitterdodge ())+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression")


# as slope plot
pd <- position_dodge(0)
Dataset_for_Argyris_25_10_23_long %>% 
  ggplot(aes(x = type_response_rate, y = response_rate, group = study))+
  geom_line(aes(colour = psy_or_med),alpha = 0.5, position = pd)+
  #geom_jitter(aes(colour = psy_or_med, alpha = 0.5))
  geom_point(aes(colour = psy_or_med), alpha = 0.5, position = pd) +
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression")

summary(lm(response_rate~type_response_rate, data = Dataset_for_Argyris_25_10_23_long))

# as slope plot excluding WL 
pd <- position_dodge(0)
Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  ggplot(aes(x = type_response_rate, y = response_rate, group = study))+
  geom_line(aes(colour = psy_or_med),alpha = 0.5, position = pd)+
  #geom_jitter(aes(colour = psy_or_med, alpha = 0.5))
  geom_point(aes(colour = psy_or_med), alpha = 0.5, position = pd) +
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression")





# as boxplot
Dataset_for_Argyris_25_10_23_long %>% 
  ggplot(aes(x = type_response_rate, y = response_rate), colour = psy_or_med)+
  geom_boxplot(aes(colour = psy_or_med) )+
  geom_point(aes(colour = psy_or_med ), position = position_jitterdodge (0.1))+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression")+
  ylab( "Response Rate %")


# as boxplot without WL 
Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  ggplot(aes(x = type_response_rate, y = response_rate), colour = psy_or_med)+
  geom_boxplot(aes(colour = psy_or_med) )+
  geom_point(aes(colour = psy_or_med ), position = position_jitterdodge (0.1))+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  xlab(NULL)+
  ggtitle("Active and Control Arm Response Rates in Adolescent Depression", subtitle = "excluding WL")+
  ylab( "Response Rate %")




#slope graph with means and CIs
Dataset_for_Argyris_25_10_23_long %>% 
  ggplot(aes(x = type_response_rate, y = response_rate)) + 
  geom_boxplot()+ 
  
  geom_line(aes(group = study), size=1, color='gray', alpha=0.6)+ 
  
  geom_point(aes(group=study),size=5,shape=21, alpha = 0.4)+
  facet_wrap(~psy_or_med)




#slope graph with means and CIs MY FAVOURITE
Dataset_for_Argyris_25_10_23_long %>% 
  ggplot(aes(x = type_response_rate, y = response_rate)) + 
  geom_line(aes(group = study), size=1, color='gray', alpha=0.6)+ 
  
  geom_point(aes(group=study),size=5,shape=21, alpha = 0.4)+
  facet_wrap(~psy_or_med)+
  ggtitle("Response rates of depressed adolescents to medication and psychotherapy",
          subtitle = "data from RCTs INCLUDING WL; means and 95%CIs in red")+
  ylab("Response Rate %")+
  scale_x_discrete(breaks=c("active_response_rate","control_response_rate"),
                   labels=c("response to active", "response to control"))+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 12, vjust = 0.9, hjust = 1))+
  theme(strip.text.x = element_text(size = rel(1.5), face = "bold"))+
  theme(plot.title = element_text(size = rel(2)))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(axis.title.y = element_text(size= rel(1.5)))+
  xlab(NULL) +
  stat_summary(aes(y = response_rate, group = psy_or_med), 
               fun.data = "mean_cl_boot", colour = "red", size = 1, geom= "line")+
  stat_summary(aes(y = response_rate, group = psy_or_med), 
               fun.data = "mean_cl_boot", colour = "red", size = 1)


#slope graph with means and CIs without WL MY OTHER FAVOURITE
# I will add here statistics re: sample size

stats_for_study_size <- Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  group_by(psy_or_med) %>% 
  summarise(avg_n = mean(study_n, na.rm = T), med_n = median(study_n, na.rm = T), 
            min_n = min(study_n, na.rm = T),  max_n = max(study_n, na.rm = T), 
            spearman_efficacy_study_size = cor(study_n, efficacy, use = "complete", "spearman")) 


df_psychol_only <-Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl" & psy_or_med == "psychological therapy")
cor.test(df_psychol_only$study_n , df_psychol_only$efficacy, method = "pearson")

df_psychol_only <-Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl" & psy_or_med == "anti-depressants")
cor.test(df_psychol_only$study_n , df_psychol_only$efficacy, method = "pearson")
# 
# df_labels <-  data.frame(categories = rev(unique(Dataset_for_Argyris_25_10_23_long$psy_or_med)),
#                            values = paste("correlation between efficacy and study size = ", 
#                                          format(stats_for_study_size$correlation_efficacy_study_size, digits =2,
#                                                 scientific = F)) )
# df_labels




Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  ggplot(aes(x = type_response_rate, y = response_rate)) + 
  geom_line(aes(group = study), size=1, color='gray', alpha=0.6)+ 
  
  geom_count(aes(group=study,size = study_n), shape=21, alpha = 0.4)+
  facet_wrap(~psy_or_med)+
  ggtitle("Response rates of depressed adolescents to medication and psychotherapy",
          subtitle = "data from RCTs EXCLUDING WL; means and 95%CIs in red")+
  ylab("Response Rate %")+
  scale_x_discrete(breaks=c("active_response_rate","control_response_rate"),
                   labels=c("response to active", "response to control"))+
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 12, vjust = 0.9, hjust = 1))+
  theme(strip.text.x = element_text(size = rel(1.5), face = "bold"))+
  theme(plot.title = element_text(size = rel(2)))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(axis.title.y = element_text(size= rel(1.5)))+
  xlab(NULL) +
    stat_summary(aes(y = response_rate, group = psy_or_med), 
                   fun.data = "mean_cl_boot", colour = "red", size = 1, geom= "line")+
stat_summary(aes(y = response_rate, group = psy_or_med), 
                   fun.data = "mean_cl_boot", colour = "red", size = 1) 


# now graph efficacy vs sample size relationship
library(ggpmisc)
Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  ggplot(aes(x = efficacy, y = study_n, group = psy_or_med)) +
  geom_point (aes(colour = psy_or_med))+
  geom_smooth(aes(colour = psy_or_med),method= lm)+
  annotate(geom = 'table',
           x=4,
           y=600,
           label=list(stats_for_study_size))+
  ggtitle("relationship between sample size and efficacy", 
          subtitle = "waitlist studies excluded")+
  xlab("Efficacy (Active - Control Response Rate)")+
  theme(plot.title = element_text(size = rel(2)))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(axis.title.y = element_text(size= rel(1.5))) +
  theme(axis.title.x = element_text(size= rel(1.5)))



lmer(efficacy ~ study_n + (1| study), data = Dataset_for_Argyris_25_10_23_long)


# now the LMEs
model_null<- lmer(response_rate ~ 1 + (1 | study) , data = Dataset_for_Argyris_25_10_23_long)
performance::icc(model_null )

model_with_type <- lmer(response_rate ~ type_response_rate + (1| study) , data = Dataset_for_Argyris_25_10_23_long)
summary(model_with_type)

model_with_type_by_psy_or_med <- lmer(response_rate ~ type_response_rate:psy_or_med + (1| study) , data = Dataset_for_Argyris_25_10_23_long)
summary(model_with_type_by_psy_or_med)

# the following is with RE
# model_with_type_by_psy_or_med_RE <- lmer(response_rate ~ type_response_rate:psy_or_med + ( type_response_rate | study) , data = Dataset_for_Argyris_25_10_23_long)
# summary(model_with_type_by_psy_or_med_RE)
# 


# 

df_for_perm <- Dataset_for_Argyris_25_10_23_long[Dataset_for_Argyris_25_10_23_long$type_response_rate=="control_response_rate", ]
n_shuffle <- 1000
vec_for_perm <- 0
shuffled_data<-list()

for (i in 1: n_shuffle){
  
#shuffled_data <-lapply(1:n_shuffle, function(x) df_for_perm[sample(nrow(df_for_perm),  replace = F),])
  
shuffling_function <- function(df){df[sample(nrow(df),  replace = F),]}
  
shuffled_dfs <- map(1:n_shuffle, ~shuffling_function(df_for_perm))
  
vec_for_perm[i] <- t.test(df_for_perm$response_rate~ shuffled_dfs[[i]]$psy_or_med,  na.rm = T)$estimate[[1]]-
                   t.test(df_for_perm$response_rate~ shuffled_dfs[[i]]$psy_or_med,  na.rm = T)$estimate[[2]]
    
}
mean(vec_for_perm) # these are the permuted values for the difference in response rates between groups. The null distribution


observed_difference <- t.test(df_for_perm$response_rate~ df_for_perm$psy_or_med,  na.rm = T)$estimate[[1]]-
    t.test(df_for_perm$response_rate~ df_for_perm$psy_or_med,  na.rm = T)$estimate[[2]] # this is the observed difference



#this depicts the difference: 
  df_plot_perm <- data.frame(vec_for_perm)

  df_plot_perm  %>% 
    ggplot(aes(vec_for_perm))+
    geom_histogram(binwidth = 2)+ 
    geom_vline(xintercept = observed_difference, colour = "red", linetype = "dashed", size = 3)+
    ggtitle("Difference between medication and psychotherapy control arms", subtitle = "histogram of permutation derived null distribution, red line is observed difference") +
    theme(plot.title = element_text(size = rel(2)))+
    theme(plot.subtitle = element_text(size = rel(1.5)))


# try out 
  Dataset_for_Argyris_25_10_23$study_n <- as.numeric(Dataset_for_Argyris_25_10_23$study_n)
   
  Dataset_for_Argyris_25_10_23 %>% 
    ggplot(aes(x = as.factor(psy_or_med) , y = control_response_rate) )+
    geom_count(aes(size = study_n), shape=21, alpha = 0.4) +
    stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
    labs (x = "", y = "response rate % control only") +
    ggtitle("response rates for anti-depressants vs psychotherapy", subtitle = "Control Response Rate")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12))
  
  
  Dataset_for_Argyris_25_10_23 %>% 
    ggplot(aes(x = as.factor(psy_or_med) , y = control_response_rate) )+
    geom_point() +
    stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
    labs (x = "", y = "response rate % control only") +
    ggtitle("response rates for anti-depressants vs psychotherapy", subtitle = "Control Response Rate")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12))
  
# effect of publication year 
# first extract pub year
Dataset_for_Argyris_25_10_23_long$year_study_published <-  str_extract(Dataset_for_Argyris_25_10_23_long$study, "[0-9]{4}")  
Dataset_for_Argyris_25_10_23_long$year_study_published




stats_for_pub_year <- Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  group_by(psy_or_med) %>% 
  summarise(spearman_efficacy_study_size = cor(as.numeric(year_study_published), efficacy, use = "complete", "spearman")) 
stats_for_pub_year

efficacy_by_year <- list()
psy_or_med <- unique(Dataset_for_Argyris_25_10_23_long$psy_or_med) 
for(i in 1: length(psy_or_med)){
  
  efficacy_by_year[[i]] <- cor.test(
   as.numeric( Dataset_for_Argyris_25_10_23_long
    [Dataset_for_Argyris_25_10_23_long$psy_or_med== psy_or_med[i]
      & Dataset_for_Argyris_25_10_23_long$control_type != "wl", ]$year_study_published ), 
                                    
    Dataset_for_Argyris_25_10_23_long
    [Dataset_for_Argyris_25_10_23_long$psy_or_med== psy_or_med[i]
      & Dataset_for_Argyris_25_10_23_long$control_type != "wl", ]$efficacy, method = "pearson")
  
}





library(ggpmisc)
Dataset_for_Argyris_25_10_23_long %>% 
  filter(!control_type=="wl") %>% 
  drop_na() %>% 
  ggplot(aes(y = efficacy, x = year_study_published, group = psy_or_med)) +
  geom_count (aes(colour = psy_or_med,size = study_n))+
  geom_smooth(aes(colour = psy_or_med),method= lm)+
   annotate(geom = 'text',
            x="2013",
            y=40,
            label= paste("r_antidepressants = ", round(efficacy_by_year[[2]]$estimate, 3), "p = ", round(efficacy_by_year[[2]]$p.value, 4),
                         "\nr_psychotherapy = ", round(efficacy_by_year[[1]]$estimate, 3), "p = ", round(efficacy_by_year[[1]]$p.value, 4)                   
                         )   , size =5        )+
  ggtitle("relationship between publication year and efficacy", 
          subtitle = "waitlist studies excluded")+
  xlab("publication year")+
  ylab("Efficacy (Active - Control Response Rate")
  theme(plot.title = element_text(size = rel(2)))+
  theme(plot.subtitle = element_text(size = rel(1.5)))+
  theme(axis.title.y = element_text(size= rel(1.5))) +
  theme(axis.title.x = element_text(size= rel(1.5)))
  
  
  
  Dataset_for_Argyris_25_10_23_long %>% 
    filter(!control_type=="wl" & type_response_rate == "control_response_rate") %>% 
    drop_na() %>% 
    ggplot(aes(y = response_rate , x = year_study_published, group = psy_or_med)) +
    geom_count (aes(colour = psy_or_med,size = study_n))+
    geom_smooth(aes(colour = psy_or_med),method= lm)+
    annotate(geom = 'text',
             x="2013",
             y=40,
             label= paste("r_antidepressants = ", round(efficacy_by_year[[2]]$estimate, 3), "p = ", round(efficacy_by_year[[2]]$p.value, 4),
                          "\nr_psychotherapy = ", round(efficacy_by_year[[1]]$estimate, 3), "p = ", round(efficacy_by_year[[1]]$p.value, 4)                   
             )   , size =5        )+
    ggtitle("relationship between publication year and efficacy", 
            subtitle = "waitlist studies excluded")+
    xlab("publication year")+
    ylab("Response to control") +
  theme(plot.title = element_text(size = rel(2)))+
    theme(plot.subtitle = element_text(size = rel(1.5)))+
    theme(axis.title.y = element_text(size= rel(1.5))) +
    theme(axis.title.x = element_text(size= rel(1.5)))  
  
  efficacy_by_year_controls <- list()
  psy_or_med <- unique(Dataset_for_Argyris_25_10_23_long$psy_or_med) 
  for(i in 1: length(psy_or_med)){ 
    efficacy_by_year_controls[[i]] <- cor.test(
   as.numeric( Dataset_for_Argyris_25_10_23_long
    [Dataset_for_Argyris_25_10_23_long$psy_or_med== psy_or_med[i]
      & Dataset_for_Argyris_25_10_23_long$control_type != "wl" &
        Dataset_for_Argyris_25_10_23_long$type_response_rate == "control_response_rate", ]$year_study_published ), 
                                    
    Dataset_for_Argyris_25_10_23_long
    [Dataset_for_Argyris_25_10_23_long$psy_or_med== psy_or_med[i]
      & Dataset_for_Argyris_25_10_23_long$control_type != "wl" &
        Dataset_for_Argyris_25_10_23_long$type_response_rate == "control_response_rate", ]$response_rate, method = "pearson")
  
}
  

  
  Dataset_for_Argyris_25_10_23_long %>% 
    filter(!control_type=="wl" & type_response_rate == "control_response_rate") %>% 
    drop_na() %>% 
    ggplot(aes(y = response_rate , x = year_study_published, group = psy_or_med)) +
    geom_count (aes(colour = psy_or_med,size = study_n))+
    geom_smooth(aes(colour = psy_or_med),method= lm)+
    annotate(geom = 'text',
             x="2013",
             y=40,
             label= paste("r_antidepressants = ", round(efficacy_by_year_controls[[2]]$estimate, 3), "p = ", round(efficacy_by_year_controls[[2]]$p.value, 4),
                          "\nr_psychotherapy = ", round(efficacy_by_year_controls[[1]]$estimate, 3), "p = ", round(efficacy_by_year_controls[[1]]$p.value, 4)                   
             )   , size =5        )+
    ggtitle("relationship between publication year and efficacy", 
            subtitle = "waitlist studies excluded")+
    xlab("publication year")+
    ylab("Response to control") +
    theme(plot.title = element_text(size = rel(2)))+
    theme(plot.subtitle = element_text(size = rel(1.5)))+
    theme(axis.title.y = element_text(size= rel(1.5))) +
    theme(axis.title.x = element_text(size= rel(1.5)))  
  
  
  
  
  efficacy_by_year_active <- list()
  psy_or_med <- unique(Dataset_for_Argyris_25_10_23_long$psy_or_med) 
  for(i in 1: length(psy_or_med)){ 
    efficacy_by_year_active[[i]] <- cor.test(
      as.numeric( Dataset_for_Argyris_25_10_23_long
                  [Dataset_for_Argyris_25_10_23_long$psy_or_med== psy_or_med[i]
                    & Dataset_for_Argyris_25_10_23_long$control_type != "wl" &
                      Dataset_for_Argyris_25_10_23_long$type_response_rate == "active_response_rate", ]$year_study_published ), 
      
      Dataset_for_Argyris_25_10_23_long
      [Dataset_for_Argyris_25_10_23_long$psy_or_med== psy_or_med[i]
        & Dataset_for_Argyris_25_10_23_long$control_type != "wl" &
          Dataset_for_Argyris_25_10_23_long$type_response_rate == "active_response_rate", ]$response_rate, method = "pearson")
    
  }
  
  
  
  Dataset_for_Argyris_25_10_23_long %>% 
    filter(!control_type=="wl" & type_response_rate == "active_response_rate") %>% 
    drop_na() %>% 
    ggplot(aes(y = response_rate , x = year_study_published, group = psy_or_med)) +
    geom_count (aes(colour = psy_or_med,size = study_n))+
    geom_smooth(aes(colour = psy_or_med),method= lm)+
    annotate(geom = 'text',
             x="2013",
             y=40,
             label= paste("r_antidepressants = ", round(efficacy_by_year_active[[2]]$estimate, 3), "p = ", round(efficacy_by_year_active[[2]]$p.value, 4),
                          "\nr_psychotherapy = ", round(efficacy_by_year_active[[1]]$estimate, 3), "p = ", round(efficacy_by_year_active[[1]]$p.value, 4)                   
             )   , size =5        )+
    ggtitle("relationship between publication year and active resposne", 
            subtitle = "waitlist studies excluded")+
    xlab("publication year")+
    ylab("Response to active") +
    theme(plot.title = element_text(size = rel(2)))+
    theme(plot.subtitle = element_text(size = rel(1.5)))+
    theme(axis.title.y = element_text(size= rel(1.5))) +
    theme(axis.title.x = element_text(size= rel(1.5)))  
  
  
  
  if (!require("devtools")) {
    install.packages("devtools")
  }
  devtools::install_github("MathiasHarrer/dmetar")
  library(meta)  
  library(dmetar)
  
  
df_for_met_test <- df_for_perm %>% 
  drop_na() %>% 
  filter(type_response_rate == "control_response_rate",
         str_detect(study, "TADS", negate = TRUE)) %>% 
  mutate(half_n = study_n/2) %>% 
  mutate(event = (response_rate*half_n)/100, resp_rate = response_rate) %>% 
  select(study, event, half_n, resp_rate, response_rate, psy_or_med)
  

  m.prop <- metaprop(event = event,
                     n = half_n,
                     studlab = study,
                     data = df_for_met_test,
                     method = "GLMM",
                     sm = "PLOGIT",
                     fixed = FALSE,
                     random = TRUE,
                     hakn = TRUE,
                     subgroup =  psy_or_med, 
                     subgroup.name = "psy_or_med",
                     print.subgroup.name =T, 
                     test.subgroup = T, 
                     prediction.subgroup = T,
                     
                     title = "test")

summary(m.prop)

pdf(file = "forestplot.pdf", width = 15, height = 20)
forest.meta(m.prop, 
            sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftlabs = c("Author", "g", "SE"))
dev.off()




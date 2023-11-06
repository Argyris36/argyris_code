Dataset_for_Argyris_25_10_23 <- read_excel("~/Downloads/Dataset_for_Argyris_25.10.23.xlsx")
library(tidyverse)
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
  #geom_boxplot()+ 

  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
  geom_line(aes(group = study), size=1, color='gray', alpha=0.6)+ 

  geom_point(aes(group=study),size=5,shape=21, alpha = 0.4)+
  facet_wrap(~psy_or_med)





# now the LMEs
model_null<- lmer(response_rate ~ 1 + (1| study) , data = Dataset_for_Argyris_25_10_23_long)
performance::icc(model_null )

model_with_type <- lmer(response_rate ~ type_response_rate + (1| study) , data = Dataset_for_Argyris_25_10_23_long)
summary(model_with_type)

model_with_type_by_psy_or_med <- lmer(response_rate ~ type_response_rate:psy_or_med + (1| study) , data = Dataset_for_Argyris_25_10_23_long)
summary(model_with_type_by_psy_or_med)

# the following is with RE
model_with_type_by_psy_or_med_RE <- lmer(response_rate ~ type_response_rate:psy_or_med + ( type_response_rate | study) , data = Dataset_for_Argyris_25_10_23_long)
summary(model_with_type_by_psy_or_med_RE)



# examine just the controls



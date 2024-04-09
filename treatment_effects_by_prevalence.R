pop_var <- seq(from = 1000, to = 10000, length.out= 17) 
#response <- 0.9
side_effect <- .1




response <-   seq(from = 0.5, to = 0.1, by = -.025)
  
responders <- 0
non_responders <- 0
responders_SE <- 0
responders_no_SE <- 0
non_responders_SE <- 0
non_responders_no_SE <- 0
ratio_responders_no_SE_all <- 0

for (i in 1: length(response)){
responders[i] <- pop_var[i]*response[i]
non_responders[i] <- pop_var[i]-responders[i]

responders_SE[i] <- responders[i]*side_effect 
responders_no_SE[i] <- responders[i] -responders_SE[i]

non_responders_SE[i] <- non_responders[i]*side_effect 
non_responders_no_SE[i] <- non_responders[i]-non_responders_SE[i]



ratio_responders_no_SE_all[i] <- responders_no_SE[i]/(responders_SE[i]+non_responders_SE[i]+non_responders_no_SE[i])
ratio_responders_no_SE_all[i]

}

ratio_responders_no_SE_all

df_response_ratio <- data.frame(ratio_responders_no_SE_all = ratio_responders_no_SE_all, 
                                pop_var = pop_var ,  response = response )

library(tidyverse)
ratio_over_prevalence <- df_response_ratio%>% 
  ggplot(aes(x= pop_var , y =ratio_responders_no_SE_all )) +
  geom_point()+
  ylim(0,1)+
  ggtitle("Effects of treatment response dilution over prevalence") +
  labs(subtitle = "plotted over prevalence")+
  ylab("ratio of responding without side effects over rest")+
  xlab("number of people treated")
ratio_over_prevalence 


ratio_over_response_rate <- df_response_ratio%>% 
  ggplot(aes(x= response , y =ratio_responders_no_SE_all )) +
  geom_point()+
  ylim(0,1)+
  ggtitle("Effects of treatment response dilution over rate of response") +
  labs(subtitle = "plotted over response rate")+
  ylab("ratio of responding without SE over rest")+
  xlab("rate of response")+
  scale_x_reverse()
ratio_over_response_rate



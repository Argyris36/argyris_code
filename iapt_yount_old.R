

### some code to estimate expected values for responders
iapt_young_no_improv <- 105181
iapt_young_improv <- 203065
iapt_old_no_improv <- 368702
iapt_old_improv <- 915606


iapt_cont_table = data.frame(matrix(
  c(iapt_young_no_improv, 
    iapt_young_improv,
    iapt_old_no_improv, 
    iapt_old_improv),
  nrow = 2,
  ncol = 2
)
)

colnames(iapt_cont_table) <- c("young", "old")
rownames(iapt_cont_table) <- c("no_improv", "improv")
iapt_cont_table 

# marginal totals
iapt_cont_table_row_marg_no_improv <- rowSums(iapt_cont_table)[[1]]# first row
iapt_cont_table_row_marg_improv<-  rowSums(iapt_cont_table)[[2]] # second row

iapt_cont_table_col_marg_young <- colSums(iapt_cont_table)[[1]] # first column
iapt_cont_table_col_marg_old<-  colSums(iapt_cont_table)[[2]]# second column

iapt_cont_table_marg_total <- iapt_cont_table_col_marg_young + iapt_cont_table_col_marg_test_old # the total, i.e. the population

# get expected values
# these are nothing but a rule of three results
# the marginal of the row is found in the total: row1_marginal/total, then (under the null hypothesis) 
# what should we expect of the first cell (position row = 1, column = 1); obviously that it be 
# the equivalent for whatever the column total is, threfore cell_1 = row1_maringal/total; and 
# the equivalent for all cells

ev_cell_11 <- (iapt_cont_table_row_marg_no_improv/iapt_cont_table_marg_total)*iapt_cont_table_col_marg_young
ev_cell_12 <- (iapt_cont_table_row_marg_no_improv/iapt_cont_table_marg_total)*iapt_cont_table_col_marg_old

ev_cell_21 <- (iapt_cont_table_row_marg_improv/iapt_cont_table_marg_total)*iapt_cont_table_col_marg_young
ev_cell_22 <- (iapt_cont_table_row_marg_improv/iapt_cont_table_marg_total)*iapt_cont_table_col_marg_old

# prepare the values for the chi-square test which is Sum(O-E)^2/expected
value_1 <- ((iapt_cont_table[1,1] - ev_cell_11)^2)/ev_cell_11
value_2 <- ((iapt_cont_table[1,2] - ev_cell_12)^2)/ev_cell_12
value_3 <- (iapt_cont_table[2,1] - ev_cell_21)^2/ev_cell_21
value_4 <- (iapt_cont_table[2,2] - ev_cell_22)^2/ev_cell_22

chi_square <- sum(value_1, value_2, value_3, value_4)
chi_square

# which is what you also get from R's inbuilt chi-square

chisq.test (matrix(
  c(iapt_young_no_improv, iapt_young_improv, iapt_old_no_improv, 
    iapt_old_improv),
  nrow = 2,
  ncol = 2
),correct=F
)$statistic


chisq.test (matrix(
  c(iapt_young_no_improv, iapt_young_improv, iapt_old_no_improv, 
    iapt_old_improv),
  nrow = 2,
  ncol = 2
),correct=F
)$expected




mean_base_young <- 15.70
sd_base_young <- 5.25
mean_outcome_young <- 10.19
sd_outcome_young <- 6.58

mean_base_old <- 15.93
sd_base_old <- 5.55
mean_outcome_old <- 9.44
sd_outcome_old <- 6.8


threshold_phq <- 6
set.seed(1974)
phq_val_base_young <- truncnorm::rtruncnorm(n = iapt_cont_table_col_marg_young, 0, 27, mean_base_young , sd_base_young )

phq_val_outcome_young <- truncnorm::rtruncnorm(n = iapt_cont_table_col_marg_young, 0, 27, mean_outcome_young , sd_outcome_young )

dif_phq_val_young <- phq_val_base_young - phq_val_outcome_young

perc_young_reliable_recovery_phq <- sum(dif_phq_val_young>=threshold_phq)/length(dif_phq_val_young)
perc_young_reliable_recovery_phq 




phq_val_base_old <- truncnorm::rtruncnorm(n = iapt_cont_table_col_marg_old, 0, 27, mean_base_old , sd_base_old )

phq_val_outcome_old<- truncnorm::rtruncnorm(n = iapt_cont_table_col_marg_old, 0, 27, mean_outcome_old , sd_outcome_old )

dif_phq_val_old <- phq_val_base_old - phq_val_outcome_old

perc_old_reliable_recovery_phq <- sum(dif_phq_val_old>=threshold_phq)/length(dif_phq_val_old)

perc_old_reliable_recovery_phq

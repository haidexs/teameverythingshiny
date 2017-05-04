#! /usr/bin/env Rscript

# set up constants
n_prob = 30;
top_ratio = 50;

# read data from csv files
usr_data = read.csv("midterm-results.csv");
prob_data = read.csv("quiz-categories.csv");

# split auditor and non-auditor
audit_usr_data = usr_data[usr_data$audit == 1, ];
nonaudit_usr_data = usr_data[usr_data$audit == 0, ];
n_audit = dim(audit_usr_data)[1];
n_nonaudit = dim(nonaudit_usr_data)[1];

# calculate the mean correct rate for each student
audit_usr_data$ave_cor = rowMeans(audit_usr_data[ , 3:32], na.rm = FALSE, dims = 1);
nonaudit_usr_data$ave_cor = rowMeans(nonaudit_usr_data[ , 3:32], na.rm = FALSE, dims = 1);

# plot the density of correct rate
plot(density(nonaudit_usr_data$ave_cor));

# calculate the mean correct rate for audit and nonaudit groups
audit_ave_cor = mean(audit_usr_data$ave_cor);
nonaudit_ave_cor = mean(nonaudit_usr_data$ave_cor);

# calculate the correct rate for each question
for (i in 1:30){
  prob_data$cor_rate[i] = mean(nonaudit_usr_data[ , 2+i]);
}

# calculate odds ratios for certain students
n_student = round(n_nonaudit*top_ratio/100);
nonaudit_usr_data_top = nonaudit_usr_data[order(-nonaudit_usr_data$ave_cor), ];
nonaudit_usr_data_top = nonaudit_usr_data_top[1:n_student, ];

n_case = sum(nonaudit_usr_data_top[ , 3:32]);
n_noncase = n_student*dim(prob_data)[1] - n_case;
n_case_exposed = matrix(0, 1, 11);
n_noncase_exposed = matrix(0, 1, 11);
for (i in 1:11){
  for (j in 1:30){
    if (prob_data[j,i+1] == 1){
      n_case_exposed[i] = sum(nonaudit_usr_data_top[ , 2+j]) + n_case_exposed[i];
      n_noncase_exposed[i] = (n_student - sum(nonaudit_usr_data_top[ , 2+j])) + n_noncase_exposed[i];
    }
  }
}
prob_categ_OR = matrix(0, 1, 11);
for (i in 1:11){
  prob_categ_OR[i] = (n_case_exposed[i]/(n_case - n_case_exposed[i]))/(n_noncase_exposed[i]/(n_noncase - n_noncase_exposed[i]));
}

#
#as.numeric(rownames(audit_usr_data))
#rowSums(prob_data[ , -1])
return(0)
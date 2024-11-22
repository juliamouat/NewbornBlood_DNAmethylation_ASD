# load packages
library(dplyr)
library(tidyr)
library(writexl)
library(readxl)
#=====================================================================================
#
#  Code chunk 1- ReCHARGE variables recoding and cleaning
#
#=====================================================================================

# load data for ReCHARGE
getwd()
setwd("/Users/gekuodza/Documents/GitHub/NewbornBlood_DNAmethylation_ASD")

DBS_Metadata_original <- read_excel("DBS_Metadata_original.xlsx")

ASDandTDChildrenwith <- read.csv("ASDandTDChildrenwithDNAfromDBS_M5514_13NOV23(ASDandTDChildrenwithDNAfromDBS_).csv")

ASDandTDChildrenwith$Batch

# Filter for "Jules" only
jules_df <- subset(ASDandTDChildrenwith, Batch == "Jules")

# View the new dataset
print(jules_df)

colnames(jules_df)
colnames(DBS_Metadata_original)

# Combine the two datasets by the "IBC" column
combined_df_children_parents <- merge(jules_df, DBS_Metadata_original, by = "IBC", all = TRUE)

# View the combined dataset
print(combined_df_children_parents)

combined_df_children_parents$IBC

combined_df_children_parents$subject_id

# Remove rows where 'subject_id' is NA
combined_df_children_parents <- combined_df_children_parents[!is.na(combined_df_children_parents$subject_id), ]

rownames(combined_df_children_parents) <- combined_df_children_parents$IBC
colnames(combined_df_children_parents)
# View the dataset
print(combined_df_children_parents)

write_xlsx(combined_df_children_parents, path = "combined_df_children_parents.xlsx")

combined_df_children_parents$ChildRace_5cat

# Convert 'correctsex.x' to a factor with levels "M" and "F"
combined_df_children_parents$correctsex.x <- factor(combined_df_children_parents$correctsex.x, 
                                                    levels = c("M", "F"), 
                                                    labels = c("Male", "Female"))

# verify changes 
str(combined_df_children_parents$correctsex.x)

# 'ChildRace_5cat' as a factor with specified labels
combined_df_children_parents$ChildRace_5cat <- factor(
  combined_df_children_parents$ChildRace_5cat,
  levels = c(1, 2, 4, 6, 9),
  labels = c("Non-hispanic white", "Non-hispanic black", "Non-hispanic asian", "Hispanic any race", "Multi-racial/Other")
)

# Verify the changes
str(combined_df_children_parents$ChildRace_5cat)

#  'MomEdu_4cat' as a factor with specified labels
combined_df_children_parents$MomEdu_4cat <- factor(
  combined_df_children_parents$MomEdu_4cat,
  levels = c(2, 3, 4, 5),
  labels = c("High school diploma/GED or less", 
             "Some college (incl. vocational, 2-yr degree)", 
             "Bachelor degree", 
             "Graduate or professional degree")
)

# Verify the changes
str(combined_df_children_parents$MomEdu_4cat)

# DadEdu as a factor with specified levels
combined_df_children_parents$DadEdu_4cat <- factor(
  combined_df_children_parents$DadEdu_4cat, 
  levels = c(2,3,4,5),
  labels = c("High School diploma/GED or lesss",
             "Some college (incl. vocational, 2-yr degree)",
             "Bachelor degree",
             "Graduate or professional degree")
)

# verfiy changes
str(combined_df_children_parents$DadEdu_4cat)

# "payment delivery" as a factor
combined_df_children_parents$PaymentDelivery.x <- factor(
  combined_df_children_parents$PaymentDelivery.x,
  levels = c(1,2),
  labels = c("Government Program/No insurance",
             "Insurance")
)

#verify changes
str(combined_df_children_parents$PaymentDelivery.x)

#OwnHome as a factor
combined_df_children_parents$OwnHome <- factor(
  combined_df_children_parents$OwnHome,
  levels = c(0,1),
  labels = c("No", "Yes")
)

#verfiy changes
str(combined_df_children_parents$OwnHome)

#Mom race as a factor
combined_df_children_parents$MomRace_4cat <- factor(
  combined_df_children_parents$MomRace_4cat, 
  levels = c(1,2,4,9),
  labels = c("Non-hispanic white", 
             "Non-hispanic black",
             "Non-hispanic asian",
             "Multi-racial/Other")
)

#verfiy changes
str(combined_df_children_parents$MomRace_4cat)

#Dad race as a factor 
combined_df_children_parents$DadRace_4cat <- factor(
  combined_df_children_parents$DadRace_4cat,
  levels = c(1,2,4,9),
  labels = c("Non-hispanic white", 
             "Non-hispanic black",
             "Non-hispanic asian",
             "Multi-racial/Other")
)

#verify changes
str(combined_df_children_parents$DadRace_4cat)

#ASD dx as a factor
combined_df_children_parents$dx2.x <- factor(
  combined_df_children_parents$dx2.x,
  levels = c(2,5),
  labels = c("ASD", "Typically developing")
)

str(combined_df_children_parents$dx2.x)


#=====================================================================================
#
#  Code chunk 2- ReCHARGE variables using dplyr and tidyr
#
#=====================================================================================

#structure
str(combined_df_children_parents$dx2.x)

#generates summary stats for all not separated by group
summary(combined_df_children_parents)

summary_CHARGE_all <- as.data.frame(summary(combined_df_children_parents))

write_xlsx(summary_CHARGE_all, path = "CHARGE_Summary_stats.xlsx")

#chec unique values
sapply(combined_df_children_parents [, c("correctsex.x", "ChildRace_5cat", "YOB.x", "AgeMomYrs", 
                                         "AgeDadYrs", "MomRace_4cat", "DadRace_4cat", "MomEdu_4cat", 
                                         "DadEdu_4cat", "PaymentDelivery.x", "OwnHome", "dx2.x")], unique)
#continous variables
continous_vars <- c("YOB.x", "AgeMomYrs", "AgeDadYrs")

#cat variables
categorical_vars <- c("correctsex.x", "ChildRace_5cat", "MomRace_4cat", "DadRace_4cat", 
                      "MomEdu_4cat", "DadEdu_4cat", "PaymentDelivery.x", "OwnHome", "dx2.x")


#this function will generate descriptive statitistics for continuous and categorical variables  
generate_statistics <- function(data, group_var, continuous_vars, categorical_vars) {
  library(dplyr)
  library(tidyr)
  
  results <- list()
  
  # Continuous variables
  cont_results <- lapply(continuous_vars, function(var) {
    stats <- data %>%
      summarise(
        mean_sd = sprintf("%.2f (%.2f)", mean(.data[[var]], na.rm = TRUE), sd(.data[[var]], na.rm = TRUE)),
        median_min_max = sprintf("%.2f (%.2f, %.2f)", median(.data[[var]], na.rm = TRUE),
                                 min(.data[[var]], na.rm = TRUE), max(.data[[var]], na.rm = TRUE))
      )
    
    # Perform tests
    if (length(unique(data[[group_var]])) > 2) {
      # ANOVA for multiple groups
      anova_result <- aov(data[[var]] ~ as.factor(data[[group_var]]), data = data)
      p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
    } else {
      # t-test for two groups
      t_test_result <- t.test(data[[var]] ~ as.factor(data[[group_var]]), data = data)
      p_value <- t_test_result$p.value
    }
    
    # Combine stats and p-value
    data.frame(Variable = var, mean_sd = stats$mean_sd, 
               median_min_max = stats$median_min_max, 
               p_value = sprintf("%.4f", p_value))
  }) %>%
    bind_rows()
  
  results$continuous <- cont_results
  
  # Categorical variables
  cat_results <- lapply(categorical_vars, function(var) {
    freq_table <- data %>%
      count(.data[[var]], .data[[group_var]]) %>%
      group_by(.data[[group_var]]) %>%
      mutate(percent = n / sum(n) * 100) %>%
      summarise(frequency = paste0(n, " (", sprintf("%.1f", percent), "%)"), .groups = "drop")
    
    # Perform tests
    if (any(table(data[[var]], data[[group_var]]) < 5)) {
      # Fisher's exact test for sparse data
      fisher_result <- fisher.test(table(data[[var]], data[[group_var]]))
      p_value <- fisher_result$p.value
    } else {
      # Chi-square test
      chi_result <- chisq.test(table(data[[var]], data[[group_var]]))
      p_value <- chi_result$p.value
    }
    
    # Combine frequency table and p-value
    data.frame(Variable = var, frequency = paste(freq_table$frequency, collapse = "; "), 
               p_value = sprintf("%.4f", p_value))
  }) %>%
    bind_rows()
  
  results$categorical <- cat_results
  
  return(results)
}



CHARGE_descriptive_stats <- generate_statistics(data = combined_df_children_parents,
                                                group_var = "dx2.x",
                                                continuous_vars = continous_vars,
                                                categorical_vars = categorical_vars)

# View results
(CHARGE_descriptive_stats$continuous)
CHARGE_descriptive_stats$categorical

results_list <- list(
  Continuous = CHARGE_descriptive_stats$continuous,
  Categorical = CHARGE_descriptive_stats$categorical
)

write_xlsx(results_list, path = "CHARGE_descriptive_stats.xlsx")

#=================================================================================
#
#  Code chunk 3- CHDS variables recoding and cleaning
#
#=====================================================================================
#load your data
CHDS_F2 <- read_excel("CHDS F2.xlsx")

CHDS_F1 <- read_excel("CHDS F1.xlsx")

# Combine the two datasets by the "batchorder" column
CHDS_F1_and_F2 <- merge(CHDS_F2, CHDS_F1, by = "batchorder", all = TRUE)

rownames(CHDS_F1_and_F2) <- CHDS_F1_and_F2$batchorder
colnames(CHDS_F1_and_F2)


# Convert 'FSex' to a factor with levels "M" and "F"
CHDS_F1_and_F2$F2SEX <- factor(CHDS_F1_and_F2$F2SEX, 
                               levels = c(0,1), 
                               labels = c("Male", "Female"))

# verify changes 
str(CHDS_F1_and_F2$F2SEX)


#Create new variable for mom race/ethnicity
CHDS_F1_and_F2$mom_race_new <- ifelse(
  CHDS_F1_and_F2$`F1MOMRACE1 Words` == "White" & CHDS_F1_and_F2$`F1HISPMOM Words` == "not spanish/hispanic", 1,
  ifelse(
    CHDS_F1_and_F2$`F1MOMRACE1 Words` == "Black" & CHDS_F1_and_F2$`F1HISPMOM Words` == "not spanish/hispanic", 2,
    ifelse(
      (CHDS_F1_and_F2$`F1MOMRACE1 Words` %in% c("Asian-Chinese", "Filipino", "Chinese", "Asian-Unspecified")) & 
        CHDS_F1_and_F2$`F1HISPMOM Words` == "not spanish/hispanic", 3,
      ifelse(
        CHDS_F1_and_F2$`F1HISPMOM Words` %in% c("mexican/mexican-american/chicano", "puerto rican", 
                                                "other spanish/hispanic (born in the U.S.)", "central or south american"), 4,
        ifelse(
          CHDS_F1_and_F2$`F1MOMRACE1 Words` %in% c("Other-Specified", "Refused to state", "unknown") & 
            CHDS_F1_and_F2$`F1HISPMOM Words` == "not spanish/hispanic", 5, 
          NA )))))

# Convert mom_race_new to a factor with correct labels
CHDS_F1_and_F2$mom_race_new <- factor(CHDS_F1_and_F2$mom_race_new,
                                      levels = 1:5,
                                      labels = c("Non-hispanic white", "Non-hispanic black", 
                                                 "Non-hispanic asian", "Hispanic", 
                                                 "Multi-racial/Other"))

table(CHDS_F1_and_F2$mom_race_new)

sum(is.na(CHDS_F1_and_F2$mom_race_new))
#sample 49,74 I had to manually change for multiracial or other

CHDS_F1_and_F2$mom_race_new[49] <- "Multi-racial/Other"
CHDS_F1_and_F2$mom_race_new[74] <- "Multi-racial/Other"
CHDS_F1_and_F2$mom_race_new[139] <- "Non-hispanic asian"
CHDS_F1_and_F2$mom_race_new[167] <- "Hispanic"

#verfiy changes you made
sum(is.na(CHDS_F1_and_F2$mom_race_new))

#Create new variable for dad race/ethnicity
CHDS_F1_and_F2$dad_race_new <- ifelse(
  CHDS_F1_and_F2$`F1FATHERRACE1 Words` == "White" & CHDS_F1_and_F2$`F1HISPFAT Words` == "not spanish/hispanic", 1,
  ifelse(
    CHDS_F1_and_F2$`F1FATHERRACE1 Words` == "Black" & CHDS_F1_and_F2$`F1HISPFAT Words` == "not spanish/hispanic", 2,
    ifelse(
      (CHDS_F1_and_F2$`F1FATHERRACE1 Words` %in% c("Asian-Japanese", "Asian-Chinese","Asian Indian (Excludes American Indian, Aleut, Eskimo)",
                                                   "Filipino", "Chinese", "Asian-Unspecified")) & 
        CHDS_F1_and_F2$`F1HISPFAT Words` == "not spanish/hispanic", 3,
      ifelse(
        CHDS_F1_and_F2$`F1HISPFAT Words` %in% c("mexican/mexican-american/chicano", "puerto rican", 
                                                "other spanish/hispanic (born in the U.S.)", "central or south american"), 4,
        ifelse(
          CHDS_F1_and_F2$`F1FATHERRACE1 Words` %in% c("American Indian", "Other-Specified", "Refused to state", "unknown") & 
            CHDS_F1_and_F2$`F1HISPFAT Words` == "not spanish/hispanic","unknown", 5 
        )))))

# Convert dad_race_new to a factor with correct labels
CHDS_F1_and_F2$dad_race_new <- factor(CHDS_F1_and_F2$dad_race_new,
                                      levels = 1:5,
                                      labels = c("Non-hispanic white", "Non-hispanic black", 
                                                 "Non-hispanic asian", "Hispanic", 
                                                 "Multi-racial/Other"))

str(CHDS_F1_and_F2$dad_race_new)
#sample 8 I had to manually change for multiracial or other
CHDS_F1_and_F2$dad_race_new[8] <- "Multi-racial/Other"
str(CHDS_F1_and_F2$dad_race_new)

#Mom education make a factor variable
CHDS_F1_and_F2$Mom_edu_new <- ifelse(
  CHDS_F1_and_F2$`F1MOMEDU Words` %in% c("3 elementary or secondary" , "12 elementary or secondary", "9 elementary or secondary", "9th through 12th, no diploma",
                                         "8 elementary or secondary", "6 elementary or secondary", 
                                         "11 elementary or secondary", "high school graduate/GED completed"),
  2,
  ifelse(
    CHDS_F1_and_F2$`F1MOMEDU Words` %in% c("1 year of college", "2 years of college", "3 years of college", 
                                           "some college credit, but no degree", "associate degree"),
    3,
    ifelse(
      CHDS_F1_and_F2$`F1MOMEDU Words` %in% c("4 years of college", "bachelor's degree"),
      4,
      ifelse(
        CHDS_F1_and_F2$`F1MOMEDU Words` %in% c("5 or more years of college", "master's degree", 
                                               "graduate or professional degree"), 5, 
        NA_integer_ # Assign NA for unknown values
      ))))

# Convert the new variable to a factor with specified levels and labels
CHDS_F1_and_F2$Mom_edu_new <- factor(
  CHDS_F1_and_F2$Mom_edu_new,
  levels = c(2, 3, 4, 5),
  labels = c("High school diploma/GED or less",
             "Some college (incl. vocational, 2-yr degree)",
             "Bachelor degree",
             "Graduate or professional degree")
)

# Verify the changes
sum(CHDS_F1_and_F2$`F1MOMEDU Words` == "unknown", na.rm = TRUE)

sum(is.na(CHDS_F1_and_F2$Mom_edu_new))

table(CHDS_F1_and_F2$Mom_edu_new)
str(CHDS_F1_and_F2$Mom_edu_new)


#Dad education make a factor variable
CHDS_F1_and_F2$Dad_edu_new <- ifelse(
  CHDS_F1_and_F2$`F1FATHEREDU Words` %in% c("12 elementary or secondary", "9 elementary or secondary", "9th through 12th, no diploma",
                                            "8 elementary or secondary", "6 elementary or secondary", "3 elementary or secondary" ,
                                            "11 elementary or secondary", "high school graduate/GED completed","10 elementary or secondary"),
  2,
  ifelse(
    CHDS_F1_and_F2$`F1FATHEREDU Words` %in% c("1 year of college", "2 years of college", "3 years of college", 
                                              "some college credit, but no degree", "associate degree"),
    3,
    ifelse(
      CHDS_F1_and_F2$`F1FATHEREDU Words` %in% c("4 years of college", "bachelor's degree"),
      4,
      ifelse(
        CHDS_F1_and_F2$`F1FATHEREDU Words` %in% c("5 or more years of college", "master's degree", 
                                                  "graduate or professional degree"), 5, 
        NA_integer_ # Assign NA for unknown values
      ))))

# Convert the new variable to a factor with specified levels and labels
CHDS_F1_and_F2$Dad_edu_new <- factor(
  CHDS_F1_and_F2$Dad_edu_new,
  levels = c(2, 3, 4, 5),
  labels = c("High school diploma/GED or less",
             "Some college (incl. vocational, 2-yr degree)",
             "Bachelor degree",
             "Graduate or professional degree")
)

# Verify the changes
# Count the number of "unknown" values in the column
sum(CHDS_F1_and_F2$`F1FATHEREDU Words` == "unknown", na.rm = TRUE)
sum(is.na(CHDS_F1_and_F2$Dad_edu_new))
str(CHDS_F1_and_F2$Dad_edu_new)
table(CHDS_F1_and_F2$Dad_edu_new)


#insurance info
CHDS_F1_and_F2$F2PRENATALPAY...14
CHDS_F1_and_F2$Prenatalinsurance <- ifelse(
  CHDS_F1_and_F2$`F2PRENATALPAY...14` %in% c("Medi-cal (Except CPS Program)", 
                                             "Medi-Cal without CPSP Support Services", 
                                             "Medi-cal CPS Program", 
                                             "Self-pay", NA),
  1, # Government Program/No insurance
  2  # Insurance
)


CHDS_F1_and_F2$Prenatalinsurance <- factor(
  CHDS_F1_and_F2$Prenatalinsurance,
  levels = c(1, 2),
  labels = c("Government Program/No insurance", "Insurance")
)

#verfiy changes
table(CHDS_F1_and_F2$Prenatalinsurance)
str(CHDS_F1_and_F2$Prenatalinsurance)

sum(is.na(CHDS_F1_and_F2$F2PRENATALPAY...14))
sum(is.na(CHDS_F1_and_F2$PaymentDelivery))



#ASD dx as a factor
CHDS_F1_and_F2$F2Aut <- factor(
  CHDS_F1_and_F2$F2Aut,
  levels = c(0,1),
  labels = c("Typically developing", "ASD")
)

#verfiy changes
str(CHDS_F1_and_F2$F2Aut)
table(CHDS_F1_and_F2$F2Aut)

write_xlsx(CHDS_F1_and_F2, path = "CHDS_F1_and_F2.xlsx")



#=====================================================================================
#
#  Code chunk 4- CHDS variables using dplyr and tidyr
#
#=====================================================================================


#structure
str(CHDS_F1_and_F2)
colnames(CHDS_F1_and_F2)

CHDS_F1_and_F2$F1FATHERAGE
#replaced 99 with NA because that was used to code for missing
CHDS_F1_and_F2$F1FATHERAGE[177] <- NA

#generates summary stats for all not separated by group
summary(CHDS_F1_and_F2)

summary_CHDS_all <- as.data.frame(summary(CHDS_F1_and_F2))

write_xlsx(summary_CHDS_all, path = "CHDS_Summary_stats.xlsx")

#chec unique values
sapply(CHDS_F1_and_F2 [, c("F1MOMAGE", "F1FATHERAGE", "F2BirthYear.y", "F2SEX", 
                           "mom_race_new", "dad_race_new", "Mom_edu_new", "Dad_edu_new", 
                           "Prenatalinsurance", "F2Aut")], unique)
#continous variables
continous_vars <- c("F2BirthYear.y", "F1MOMAGE", "F1FATHERAGE")

#cat variables
categorical_vars <- c("F2SEX", "mom_race_new", "dad_race_new", "Mom_edu_new", 
                      "Dad_edu_new", "Prenatalinsurance", "F2Aut")

#this function will generate descriptive statitistics for continuous and categorical variables  
generate_statistics <- function(data, group_var, continuous_vars, categorical_vars) {
  library(dplyr)
  library(tidyr)
  
  results <- list()
  
  # Continuous variables
  cont_results <- lapply(continuous_vars, function(var) {
    stats <- data %>%
      summarise(
        mean_sd = sprintf("%.2f (%.2f)", mean(.data[[var]], na.rm = TRUE), sd(.data[[var]], na.rm = TRUE)),
        median_min_max = sprintf("%.2f (%.2f, %.2f)", median(.data[[var]], na.rm = TRUE),
                                 min(.data[[var]], na.rm = TRUE), max(.data[[var]], na.rm = TRUE))
      )
    
    # Perform tests
    if (length(unique(data[[group_var]])) > 2) {
      # ANOVA for multiple groups
      anova_result <- aov(data[[var]] ~ as.factor(data[[group_var]]), data = data)
      p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
    } else {
      # t-test for two groups
      t_test_result <- t.test(data[[var]] ~ as.factor(data[[group_var]]), data = data)
      p_value <- t_test_result$p.value
    }
    
    # Combine stats and p-value
    data.frame(Variable = var, mean_sd = stats$mean_sd, 
               median_min_max = stats$median_min_max, 
               p_value = sprintf("%.4f", p_value))
  }) %>%
    bind_rows()
  
  results$continuous <- cont_results
  
  # Categorical variables
  cat_results <- lapply(categorical_vars, function(var) {
    freq_table <- data %>%
      count(.data[[var]], .data[[group_var]]) %>%
      group_by(.data[[group_var]]) %>%
      mutate(percent = n / sum(n) * 100) %>%
      summarise(frequency = paste0(n, " (", sprintf("%.1f", percent), "%)"), .groups = "drop")
    
    # Perform tests
    if (any(table(data[[var]], data[[group_var]]) < 5)) {
      # Fisher's exact test for sparse data
      fisher_result <- fisher.test(table(data[[var]], data[[group_var]]))
      p_value <- fisher_result$p.value
    } else {
      # Chi-square test
      chi_result <- chisq.test(table(data[[var]], data[[group_var]]))
      p_value <- chi_result$p.value
    }
    
    # Combine frequency table and p-value
    data.frame(Variable = var, frequency = paste(freq_table$frequency, collapse = "; "), 
               p_value = sprintf("%.4f", p_value))
  }) %>%
    bind_rows()
  
  results$categorical <- cat_results
  
  return(results)
}





CHDS_descriptive_stats <- generate_statistics(data = CHDS_F1_and_F2,
                                              group_var = "F2Aut",
                                              continuous_vars = continous_vars,
                                              categorical_vars = categorical_vars)

# View results
(CHDS_descriptive_stats$continuous)
CHDS_descriptive_stats$categorical

results_list <- list(
  Continuous = CHDS_descriptive_stats$continuous,
  Categorical = CHDS_descriptive_stats$categorical
)

write_xlsx(results_list, path = "CHDS_descriptive_stats.xlsx")

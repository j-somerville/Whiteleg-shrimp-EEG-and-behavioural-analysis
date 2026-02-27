####EEG script from Jasmine csv files from labchart#####
###Jan and Feb ES+CS data 2024-2025 - 2-2.5V 5s and 2.5-3V 20s
####Cleaning csv files that have been extracted from Labchart using a power template
##One function for single files and another for double
##ensure time csv files are in the format hh:mm:ss before reading in R
###Microsoft co pilot used to debug code 

library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(zoo)
library(stringr)
library(purrr)
library(tidyr)





#Define the folder path
folder_path <- "C:.."

# Get all CSVs except those with 'double' in their filename
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

filtered_files <- csv_files[!grepl("double", basename(csv_files), ignore.case = TRUE)]


# Read each CSV, rename the first column, add filename as a new column
data_list <- lapply(filtered_files, function(file) {
  df <- read.csv(file,stringsAsFactors = FALSE)
  
  df <- df[-c(1, 2), ]
  
  
  # Rename first column to 'time'
  colnames(df)[1] <- "Time"
  colnames(df)[2] <-"Raw.EEG"
  colnames(df)[3] <-"Beta"
  colnames(df)[4] <-"Alpha"
  colnames(df)[5] <-"Delta"
  colnames(df)[6] <-"Theta"
  colnames(df)[7]<-"Raw.EEG.SEF"
  colnames(df)[8]<-"Raw.EEG.Median.0_500hz"
  colnames(df)[9]<-"All.0.5.32.Hz"
  colnames(df)[10]<-"Raw.EEG.Median.0.5_32hz"
  colnames(df)[11] <- "Comments"
  
  # Add source file column
  df$Source_File <- basename(file)
  
  return(df)
})
signal <- c("Beta","Alpha","Delta","Theta")


# Combine all data frames
combined_data <- do.call(rbind, data_list)

combined_data[ ,2:10] <- lapply(combined_data[ ,2:10], function(x) as.numeric(x))
##get rid of nas
clean_combined_data <- na.omit(combined_data)

clean_combined_data <- clean_combined_data %>%
  mutate(Time = as.character(Time)) %>%  # Ensure time is character
  mutate(time_seconds = period_to_seconds(hms(Time))) %>%
  group_by(Source_File) %>%
  mutate(relative_time = time_seconds - min(time_seconds)) %>%
  ungroup()

###plot before MAD filtering

df_long <- clean_combined_data %>%
  pivot_longer(
    cols = "All.0.5.32.Hz",
    names_to = "band",
    values_to = "power"
  )

## some files have <=0 values that can't be log scaled so filter out
df_long_filtered <- df_long %>%
  filter(power > 0)


plots_before <- df_long_filtered %>%
  group_split(Source_File) %>%
  map(~ ggplot(.x, aes(x = relative_time, y = power, color = band)) +
        geom_line(linewidth = 0.5) +  scale_y_log10()+
        labs(title = unique(.x$Source_File),
             x = "Time (s)", y = "Power (µV²)", color = "EEG Band") +
        theme_minimal())
print(plots_before)


###Before stun on
clean_combined_data2 <- clean_combined_data %>%
  group_by(Source_File) %>%
  mutate(
    stun_time = first(relative_time[Comments == "stun on"])
  ) %>%
  group_by(Source_File) %>%
  mutate(
    Med = median(All.0.5.32.Hz[relative_time >= 0 & relative_time < (stun_time - 100)], na.rm = TRUE),
    MAD = mad(All.0.5.32.Hz[relative_time >= 0 & relative_time < (stun_time - 100)], na.rm = TRUE),
    Keep = if_else(relative_time >= 0 & relative_time < (stun_time - 100),
                   abs(All.0.5.32.Hz - Med) <= 0.5 * MAD,
                   TRUE)
  ) %>%
  filter(Keep) %>%
  select(-Med, -MAD, -Keep, -stun_time)


###after in slurry
clean_combined_data2 <- clean_combined_data2 %>%
  group_by(Source_File) %>%
  mutate(
    stun_time = first(relative_time[Comments == "in slurry"])
  ) %>%
  group_by(Source_File) %>%
  mutate(
    Med = median(All.0.5.32.Hz[relative_time >= (stun_time + 120)], na.rm = TRUE),
    MAD = mad(All.0.5.32.Hz[relative_time >= (stun_time + 120)], na.rm = TRUE),
    Keep = if_else(relative_time >= (stun_time + 120),
                   abs(All.0.5.32.Hz - Med) <= 1 * MAD,
                   TRUE)
  ) %>%
  filter(Keep) %>%
  select(-Med, -MAD, -Keep, -stun_time)

clean_combined_data2$ten_percent<-10
clean_combined_data2$hundred_percent<-100


####Plot data after cleaning######
###convert total power to PERCENTAGE###
df_long <- clean_combined_data2 %>%
  pivot_longer(
    cols = "All.0.5.32.Hz",
    names_to = "band",
    values_to = "power"
  )

## some files have <=0 values that can't be log scaled so filter out
df_long_filtered <- df_long %>%
  filter(power > 0)


plots <- df_long_filtered %>%
  group_split(Source_File) %>%
  map(~ ggplot(.x, aes(x = relative_time, y = power, color = band)) +
        geom_line(linewidth = 0.5) +  scale_y_log10()+
        labs(title = unique(.x$Source_File),
             x = "Time (s)", y = "Power (µV²)", color = "EEG Band") +
        theme_minimal())
print(plots)

#### facet plot
CLEAN2<-ggplot(df_long_filtered, aes(x = relative_time, y = power, color = band)) +
  geom_line(linewidth=0.5) + scale_y_log10()+
  facet_wrap(~ Source_File, scales = "free_x") +
  theme_minimal()
CLEAN2
### trying to plot vertical lines with comments ###

comment_times <- clean_combined_data2 %>%
  filter(str_detect(Comments, regex("in slurry", ignore_case = TRUE))) %>%
  group_by(Source_File) %>%
  slice_min(relative_time, n = 1) %>%
  ungroup() %>%
  select(Source_File, relative_time)


plots <- df_long_filtered %>%
  group_split(Source_File) %>%
  map(function(file_df) {
    fname <- unique(file_df$Source_File)
    comment_time <- comment_times %>% filter(Source_File == fname) %>% pull(relative_time)
    
    ggplot(file_df, aes(x = relative_time, y = power, color = band)) +
      geom_line() + scale_y_log10()+
      geom_vline(xintercept = comment_time, linetype = "dashed", color = "red") +
      annotate("text",
               x = comment_time,
               y = max(file_df$power, na.rm = TRUE),
               label = "In slurry",
               hjust = -0.1, vjust = 1.2,
               color = "red", size = 3.5) +
      labs(title = fname, x = "Time (s)", y = "Power (µV²)", color = "EEG Band") +
      theme_minimal()
  })

print(plots)

eeg_df<-clean_combined_data2

######Find percentage of baseline and create epochs######
# STEP 1: Identify time of 'in slurry' per file
slurry_times <- eeg_df %>%
  filter(str_detect(Comments, regex("in slurry", ignore_case = TRUE))) %>%
  group_by(Source_File) %>%
  summarise(slurry_time = min(relative_time), .groups = "drop")


# STEP 2: Join slurry time to full data
eeg_annotated <- eeg_df %>%
  left_join(slurry_times, by = "Source_File") %>%
  mutate(relative_time2 = relative_time - slurry_time)


baseline_df3 <- eeg_annotated %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "in slurry"])) %>%
  filter(relative_time >= 0 & relative_time < (stun_time - 100)) %>%
  arrange(Source_File, relative_time) %>%
  ungroup()

baseline_medians <- baseline_df3 %>%
  group_by(Source_File) %>%
  summarise(across(c(All.0.5.32.Hz),
                   ~median(.x, na.rm = TRUE),
                   .names = "baseline_{.col}"))

eeg_percent <- eeg_annotated %>%
  left_join(baseline_medians, by = "Source_File") %>%
  mutate(across(c(All.0.5.32.Hz),
                ~ (.x / get(paste0("baseline_", cur_column()))) * 100,
                .names = "pct_{.col}"))
###baseline parts should be near 100% (100 seconds before in slurry)

###Label baseline sample. As each animal is different length of data, we are labelling epoch as "-300" seconds
###average all these data points
###getting data from after 100 seconds and 100 seconds before stun_on 
##labelling as -300 seconds
###first define before stun on and 100 seconds before 0
baseline_window <- eeg_percent %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "stun on"])) %>%
  filter(relative_time >= 100 & relative_time < (stun_time - 100)) %>%
  ungroup() %>%
  mutate(relative_time2 = -300)

baseline_window_count<- eeg_percent %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "stun on"])) %>%
  filter(relative_time >= 100 & relative_time < (stun_time - 100)) %>%
  mutate(n = n()) %>%      # count per group
  ungroup() %>%
  mutate(relative_time2 = -300)

# One unique n per Source_File
baseline_window_unique_n <- baseline_window_count %>%
  distinct(Source_File, n)
#write.csv

###average all these data points as we do not the same number of values for each

###1 unique average across new percent column

baseline_summary <- baseline_window %>%
  group_by(Source_File, ten_percent,hundred_percent) %>%
  summarise(
    across(starts_with("pct_"), ~mean(.x, na.rm = TRUE)),
    relative_time2 = -300,
    bin = -1,
    .groups = "drop"
  )

#write.csv


# 1. Compute bins and anchor times
eeg_binned <- eeg_percent %>%
  filter(relative_time2 >= 0 & relative_time2 <= 3600) %>%
  mutate(bin = floor(relative_time2 / 60),
         anchor_time = bin * 60)

# 2. For each Source_File + bin, pick the datapoint closest to the anchor
start_times <- eeg_binned %>%
  group_by(Source_File, bin) %>%
  summarise(
    start_time = relative_time2[which.min(abs(relative_time2 - anchor_time))],
    .groups = "drop"
  )

# 3. Join start_time back to full data and extract the 20-second window
data_used_for_avg <- start_times %>%
  left_join(eeg_binned, by = c("Source_File", "bin")) %>%
  filter(relative_time2 >= start_time & relative_time2 < start_time + 20)

# 4. Summarise the averages
subsampled_summary <- data_used_for_avg %>%
  group_by(Source_File, bin) %>%
  summarise(
    across(starts_with("pct_"), ~mean(.x, na.rm = TRUE)),
    ten_percent = first(ten_percent),
    hundred_percent = first(hundred_percent),
    relative_time2 = first(start_time),
    n_points = n(),   # how many datapoints contributed
    .groups = "drop"
  )

subsampled_summary<- subsampled_summary |>
  dplyr::filter(bin <= 30)

####remove these files as noisy artefacts or too short####
subsampled_summary<-subsampled_summary %>%
  filter(!str_detect(Source_File, "120624_610_v3.csv")&
           !str_detect(Source_File, "120624_609.csv")&
          !str_detect(Source_File, "140124_07.csv")&
           !str_detect(Source_File, "120624_611_v3.csv"))
#write.csv 

subsampled<-subset(subsampled_summary, select = -c(n_points) )


#combine with baseline
new_subsample<-rbind(subsampled,baseline_summary)

#####remove these files as noisy artefacts or too short####

new_subsample<-new_subsample %>%
  filter(!str_detect(Source_File, "120624_610_v3.csv")&
           !str_detect(Source_File, "120624_609.csv")&
           !str_detect(Source_File, "140124_07.csv")&
           !str_detect(Source_File, "120624_611_v3.csv"))


boxplot_data <- new_subsample %>%
  select(Source_File, bin, ten_percent, hundred_percent, starts_with("pct_")) %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "band",
    values_to = "pct_value"
  )


boxplot_data<- boxplot_data %>%
  mutate(band = recode(band,
                       "pct_All.0.5.32.Hz" = "0.5-32Hz"))

###cut boxplot data at bin 30

boxplot_data<- boxplot_data |>
  dplyr::filter(bin <= 30)

ggplot(boxplot_data, aes(x = factor(bin), y = pct_value, fill = band)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7, aes(colour = Source_File)) +
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
  facet_wrap(~ band, scales = "free_y") +
  scale_y_log10() +
  labs(
    title = "EEG Band Power (% of Baseline) at 5 minute intervals",
    x = "Time relative to slurry (minutes)",
    y = "Percentage of Baseline"
  ) +
  theme_minimal()

tail_flip_bp<- boxplot_data %>%
  filter(!str_detect(Source_File, "120624"))

no_tail_flip <- boxplot_data%>%
  filter(str_detect(Source_File, "120624"))
##plot separetly

###tail flippers/ responders#####
tail_flip_bp_4_shrimp<-tail_flip_bp %>%
  filter(!str_detect(Source_File, "120124_32_v2.csv") &
           !str_detect(Source_File, "180124_25.csv")&
           !str_detect(Source_File, "140124_07.csv")&
           !str_detect(Source_File, "190624_616.csv")&
           !str_detect(Source_File, "180925_92.csv")&
           !str_detect(Source_File, "150125_15.csv"))


tf2<-tail_flip_bp_4_shrimp%>% 
  filter(!bin %in% c(0,1)) %>%
  ggplot(aes(x = factor(bin), y = pct_value)) +
  geom_boxplot(outlier.shape = NA, alpha=0.2, fill="lightblue") +
  geom_jitter(width = 0.1, size = 2, alpha = 1, aes(colour = Source_File), show.legend = FALSE) +
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
 scale_y_log10()+
  labs(title = "ES-CS-R: 2-2.5 V cm⁻¹ for 5s",
       x = "Time relative to slurry immersion (minutes)",
       y = "Ptot (%)") +
  theme_minimal()+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  scale_x_discrete(labels = c(
    "-1" = "<0"))

tf2
#ggsave(filename = "Reversible_stun.png", plot = tf2, width = 14, height = 10, dpi = 300)

library(lmerTest)
library(broom.mixed)

tail_flip_bp_4_shrimp$sqrt_pct <- sqrt(tail_flip_bp_4_shrimp$pct_value)

tail_flip_bp_4_shrimp<-tail_flip_bp_4_shrimp%>%
  filter(!bin %in% c(0,1))

tail_flip_bp_4_shrimp$bin<-as.factor(tail_flip_bp_4_shrimp$bin)
tail_flip_bp_4_shrimp$Source_File<-as.factor(tail_flip_bp_4_shrimp$Source_File)

m2 <- lmer(sqrt_pct ~ bin + (1|Source_File), data = tail_flip_bp_4_shrimp)
summary(m2)

results_tbl_2.5 <- broom.mixed::tidy(m2)

#write.csv

#Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: sqrt_pct ~ bin + (1 | Source_File)
#   Data: tail_flip_bp_4_shrimp

#REML criterion at convergence: 587.3

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-5.9044 -0.3425 -0.0183  0.2352  4.0156 

#Random effects:
# Groups      Name        Variance Std.Dev.
# Source_File (Intercept) 25.702   5.07    
# Residual                 1.715   1.31    
#Number of obs: 179, groups:  Source_File, 6

#Fixed effects:
#            Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)  10.2597     2.1376   5.6630   4.800  0.00351 ** 
#bin2         -3.4938     0.7561 143.9997  -4.621 8.41e-06 ***
#bin3         -3.5711     0.7561 143.9997  -4.723 5.46e-06 ***
#bin4         -3.8680     0.7561 143.9997  -5.116 9.80e-07 ***
#bin5         -3.6970     0.7561 143.9997  -4.890 2.66e-06 ***
#bin6         -4.0158     0.7561 143.9997  -5.311 4.04e-07 ***
#bin7         -4.1627     0.7561 143.9997  -5.506 1.64e-07 ***
#bin8         -4.1373     0.7561 143.9997  -5.472 1.92e-07 ***
#bin9         -4.3378     0.7561 143.9997  -5.737 5.47e-08 ***
#bin10        -4.1378     0.7561 143.9997  -5.473 1.91e-07 ***
#bin11        -4.2638     0.7561 143.9997  -5.639 8.73e-08 ***
#bin12        -4.4466     0.7561 143.9997  -5.881 2.73e-08 ***
#bin13        -4.6422     0.7561 143.9997  -6.140 7.62e-09 ***
#bin14        -4.1992     0.7561 143.9997  -5.554 1.31e-07 ***
#bin15        -4.2560     0.7561 143.9997  -5.629 9.17e-08 ***
#bin16        -4.4191     0.7561 143.9997  -5.845 3.25e-08 ***
#bin17        -4.0770     0.7561 143.9997  -5.392 2.78e-07 ***
#bin18        -3.7435     0.7561 143.9997  -4.951 2.04e-06 ***
#bin19        -3.7841     0.7561 143.9997  -5.005 1.61e-06 ***
#bin20        -3.9990     0.7561 143.9997  -5.289 4.47e-07 ***
#bin21        -4.5235     0.7561 143.9997  -5.983 1.66e-08 ***
#bin22        -4.1491     0.7561 143.9997  -5.488 1.79e-07 ***
#bin23        -3.7079     0.7561 143.9997  -4.904 2.50e-06 ***
#bin24        -4.4416     0.7561 143.9997  -5.875 2.81e-08 ***
#bin25        -4.0373     0.7561 143.9997  -5.340 3.55e-07 ***
#bin26        -3.7688     0.7561 143.9997  -4.985 1.76e-06 ***
#bin27        -4.3595     0.7561 143.9997  -5.766 4.76e-08 ***
#bin28        -3.9894     0.7561 143.9997  -5.276 4.74e-07 ***
#bin29        -4.0989     0.7561 143.9997  -5.421 2.43e-07 ***
#bin30        -4.1904     0.7942 144.0018  -5.276 4.75e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

####non responders/tail flippers#####
shimp92<-subset(tail_flip_bp, tail_flip_bp$Source_File=="180925_92.csv")
shrimp616<-subset(tail_flip_bp,tail_flip_bp$Source_File=="190624_616.csv")
shrimp15<-subset(tail_flip_bp,tail_flip_bp$Source_File=="150125_15.csv")
Ntail_flip<-no_tail_flip %>%
  filter(!str_detect(Source_File, "120624_610_v3.csv")&
 !str_detect(Source_File, "120624_609.csv")&
   !str_detect(Source_File, "120624_611_v3.csv"))

Ntail_fliptwo<-rbind(Ntail_flip, shimp92,shrimp616,shrimp15)

ntf3<-Ntail_fliptwo%>% filter(!bin %in% c(0,1)) %>%
  ggplot(aes(x = factor(bin), y = pct_value))+
  geom_boxplot(outlier.shape = NA, alpha=0.2,fill="firebrick1") +
  geom_jitter(width = 0.1, size = 2, alpha = 1, aes(colour = Source_File), show.legend = FALSE)+
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
scale_y_log10()+
  labs(title = "ES-CS-NR: 2.5-3 V cm⁻¹  for 20s" ,     x = "Time relative to slurry immersion (minutes)",
       y = "Ptot (%)")  +

  theme_minimal()+ geom_point(aes(x = 3, y =0.4), 
                              shape = 8, 
                              size = 1, 
                              color = "black")+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  scale_x_discrete(labels = c(
    "-1" = "<0"))
ntf3


Ntail_fliptwo$sqrt_pct <- sqrt(Ntail_fliptwo$pct_value)

Ntail_fliptwo<-Ntail_fliptwo%>%
  filter(!bin %in% c(0,1))

Ntail_fliptwo$bin<-as.factor(Ntail_fliptwo$bin)
Ntail_fliptwo$Source_File<-as.factor(Ntail_fliptwo$Source_File)

m2 <- lmer(sqrt_pct ~ bin + (1|Source_File), data = Ntail_fliptwo)
summary(m2)
results_tbl_3 <- broom.mixed::tidy(m2)

#write.csv

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-5.0393 -0.4297 -0.0496  0.4197  3.0910 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Source_File (Intercept) 0.10103  0.3178  
#Residual                0.06462  0.2542  
#Number of obs: 112, groups:  Source_File, 4

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  10.3890     0.2035  7.6043   51.05 6.38e-11 ***
#  bin2         -9.1687     0.2215 79.0512  -41.40  < 2e-16 ***
#  bin3         -9.1250     0.1798 79.0053  -50.76  < 2e-16 ***
#  bin4         -9.1791     0.1798 79.0053  -51.06  < 2e-16 ***
#  bin5         -9.2882     0.1798 79.0053  -51.67  < 2e-16 ***
#  bin6         -9.2668     0.1798 79.0053  -51.55  < 2e-16 ***
#  bin7         -9.2285     0.1798 79.0053  -51.34  < 2e-16 ***
#  bin8         -9.2598     0.1798 79.0053  -51.51  < 2e-16 ***
#  bin9         -9.3098     0.1798 79.0053  -51.79  < 2e-16 ***
#  bin10        -9.4102     0.1798 79.0053  -52.35  < 2e-16 ***
#  bin11        -9.5128     0.1798 79.0053  -52.92  < 2e-16 ***
#  bin12        -9.3721     0.1798 79.0053  -52.14  < 2e-16 ***
#  bin13        -9.5329     0.1798 79.0053  -53.03  < 2e-16 ***
#  bin14        -9.4393     0.1798 79.0053  -52.51  < 2e-16 ***
#  bin15        -9.4351     0.1798 79.0053  -52.49  < 2e-16 ***
#  bin16        -9.4369     0.1798 79.0053  -52.50  < 2e-16 ***
#  bin17        -9.3938     0.1798 79.0053  -52.26  < 2e-16 ***
#  bin18        -9.3317     0.1798 79.0053  -51.91  < 2e-16 ***
#  bin19        -9.3809     0.1798 79.0053  -52.19  < 2e-16 ***
#  bin20        -9.3028     0.1798 79.0053  -51.75  < 2e-16 ***
#  bin21        -9.3464     0.1798 79.0053  -51.99  < 2e-16 ***
#  bin22        -9.5204     0.1798 79.0053  -52.96  < 2e-16 ***
#  bin23        -9.3294     0.1798 79.0053  -51.90  < 2e-16 ***
#  bin24        -9.3183     0.1798 79.0053  -51.84  < 2e-16 ***
#  bin25        -9.4122     0.1947 79.0315  -48.33  < 2e-16 ***
#  bin26        -9.5074     0.1947 79.0315  -48.82  < 2e-16 ***
#  bin27        -9.4119     0.1947 79.0315  -48.33  < 2e-16 ***
#  bin28        -9.2869     0.1947 79.0315  -47.69  < 2e-16 ***
#  bin29        -9.2626     0.1947 79.0315  -47.56  < 2e-16 ***
#  bin30        -9.3359     0.1947 79.0315  -47.94  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

both<-ggarrange(tf2, ntf3, ncol=1, nrow=2, common.legend = TRUE)
both

#ggsave(filename = "bothstun_all_freq3_0.5baseline.png", plot = both, width = 14, height = 10, dpi = 300)



all_data <- bind_rows(
  Ntail_fliptwo %>% mutate(condition = "2.5v"),
  tail_flip_bp_4_shrimp%>% mutate(condition = "3v"))


########NOISE POST STUN#####

slurry_times <- clean_combined_data %>%
  filter(grepl("in slurry", Comments, ignore.case = TRUE)) %>%
  group_by(Source_File) %>%
  summarise(slurry_time = first(relative_time), .groups = "drop")
plot_data <- clean_combined_data %>%
  left_join(slurry_times, by = "Source_File")


plots_close <- plot_data %>%
  group_split(Source_File) %>%
  map(function(df) {
    
    fname <- unique(df$Source_File)
    slurry_time <- unique(df$slurry_time)
    
    # Crop window: 50 sec before → 400 sec after slurry
    cropped <- df %>%
      filter(
        relative_time >= slurry_time - 50,
        relative_time <= slurry_time + 400
      )
    
    # Handle empty windows safely
    if (nrow(cropped) == 0) {
      message("No data in window for: ", fname)
      return(
        ggplot() +
          labs(title = paste(fname, "(no data in window)")) +
          theme_minimal()
      )
    }
    
    ggplot(cropped, aes(x = relative_time, y = Raw.EEG)) +
      geom_line() +
      scale_y_log10() +
      geom_vline(
        xintercept = slurry_time,
        color = "red",
        linetype = "dashed"
      ) +
      annotate(
        "text",
        x = slurry_time,
        y = max(cropped$Raw.EEG, na.rm = TRUE),
        label = "In slurry",
        hjust = -0.1,
        vjust = 1.2,
        color = "red"
      ) +
      labs(
        title = fname,
        x = "Time (s)",
        y = "Power (µV²)"
      ) +
      theme_minimal()
  })
plots_close


plots_far<- plot_data %>%
  group_split(Source_File) %>%
  map(function(df) {
    
    fname <- unique(df$Source_File)
    slurry_time <- unique(df$slurry_time)
    
    ggplot(df, aes(x = relative_time, y = Raw.EEG)) +
      geom_line() +
      scale_y_log10() +
      geom_vline(
        xintercept = slurry_time,
        color = "red",
        linetype = "dashed"
      ) +
      annotate(
        "text",
        x = slurry_time,
        y = max(df$Raw.EEG, na.rm = TRUE),
        label = "In slurry",
        hjust = -0.1,
        vjust = 1.2,
        color = "red"
      ) +
      labs(
        title = fname,
        x = "Time (s)",
        y = "Power (µV²)"
      ) +
      theme_minimal()
  })
plots_far

##manually finding minimum value after CS immersion (when high amplitude stops) from looking at graphs
###before filtering so clean_combined data

shrimp2<-subset(clean_combined_data, clean_combined_data$Source_File=="311024_02.csv")
min_row_2 <- shrimp2  %>%
  filter(relative_time >= 950, relative_time <= 975) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##955

shrimp1<-subset(clean_combined_data, clean_combined_data$Source_File=="311024_01.csv")
min_row_1 <- shrimp1  %>%
  filter(relative_time >= 500,relative_time<= 550) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
###515

shrimp98<-subset(clean_combined_data, clean_combined_data$Source_File=="240925_98.csv")
min_row_98 <- shrimp98  %>%
 filter(relative_time >= 990, relative_time <= 1000) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
###995 - no 98 ...check

shrimp97<-subset(clean_combined_data, clean_combined_data$Source_File=="240925_97.csv")
min_row_97 <- shrimp97  %>%
  filter(relative_time>= 1220, relative_time <= 1239) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##1228

shrimp94<-subset(clean_combined_data, clean_combined_data$Source_File=="230925_94.csv")
min_row_94 <- shrimp94  %>%
  filter(relative_time>= 1270, relative_time<= 1300) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#1385

shrimp92<-subset(clean_combined_data, clean_combined_data$Source_File=="180925_92.csv")
min_row_92 <- shrimp92  %>%
  filter(relative_time >= 1080, relative_time <= 1100) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#1099

shrimp613<-subset(clean_combined_data, clean_combined_data$Source_File=="120624_613_v3.csv")
min_row_613<- shrimp613  %>%
  filter(relative_time >= 700, relative_time<= 730) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##712

shrimp611<-subset(clean_combined_data, clean_combined_data$Source_File=="120624_611_v3.csv")
min_row_611 <- shrimp611%>%
  filter(relative_time >= 950, relative_time<= 980) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#965

shrimp5<-subset(clean_combined_data, clean_combined_data$Source_File=="071124_05.csv")
min_row_5 <- shrimp5 %>%
  filter(relative_time >= 980, relative_time <= 990) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#988


shrimp616<-subset(clean_combined_data, clean_combined_data$Source_File=="190624_616.csv")
min_row_616 <- shrimp616 %>%
  filter(relative_time >= 770, relative_time <= 800) %>%
slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)

shrimp15<-subset(clean_combined_data, clean_combined_data$Source_File=="150125_15.csv")
min_row_15 <- shrimp15 %>%
  filter(relative_time >= 540, relative_time <= 550) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)

shrimp_duration_3v<-rbind(min_row_15,min_row_613,min_row_616,min_row_92)

shrimp_duration_2.5v<-rbind(min_row_1,min_row_2,min_row_5,min_row_97,min_row_94, min_row_98)

shrimp_duration_2.5v <- shrimp_duration_2.5v %>%
  rename(noise_time = relative_time)

shrimp_duration_3v <- shrimp_duration_3v %>%
  rename(noise_time = relative_time)

shrimp_duration_2.5v <- shrimp_duration_2.5v %>%
  left_join(slurry_times, by = "Source_File")

shrimp_duration_3v <- shrimp_duration_3v %>%
  left_join(slurry_times, by = "Source_File")

shrimp_duration_2.5v<-shrimp_duration_2.5v%>%
  mutate(Total= (noise_time-slurry_time))

shrimp_duration_2.5v$stun_parameter<-"2.5V"


####As the noise for these animals is not the same as 2.5v and they did not move, any noise is likely
###neurological. As we cannot tell, we put the noise to 1s to encompass this posibility and minimal movement
shrimp_duration_3v<-shrimp_duration_3v%>%
  mutate(Total= (noise_time-slurry_time))
shrimp_duration_3v$Total=1
shrimp_duration_3v$stun_parameter<-"3V"

#write.csv
shrimp_duration_plot<-ggplot(shrimp_duration_2.5v, aes(x=stun_parameter, y=Total)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=1)+
  labs(x = "Animals stunned at 2.5v/cm² for 5s", 
       y = "Tail flip duration (s)")+
  theme_bw()+
 # theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=11))
shrimp_duration_plot

###read in temp duration plot. rbind change name of temp column to stun_parametr

shrimp_both<-read.csv("C:/....shrimp_eeg_noise_duration_all.csv")

shrimp_duration_plot <- ggplot(shrimp_both,
  aes(x = stun_parameter, y = Total, fill = stun_parameter)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.2) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), size = 3) +
  labs(x = "",
    y = "Duration of increased amplitude (s)",
    fill = "Protocol") +
  theme_bw() +guides(fill = "none") +
  theme(axis.text.x = element_text(size = 17),
    text = element_text(size = 20)  ) +  scale_x_discrete(
    labels = c("-2.5" = "-2.5°C",
      "0" = "0°C",
      "2.5_5" = "2.5 - 5°C",
      "2.5V" = "2-2.5 V cm⁻¹ 5s",
      "3V"="2.5-3 V cm⁻¹ 20s"))+
scale_fill_manual(values = c("-2.5" = "blue",
  "0" = "grey",
  "2.5_5" = "orange",
  "2.5V" = "lightblue",
  "3V"="firebrick1"))+
  annotate("segment",
           x = 3.5, xend = 3.5,
           y = -Inf, yend = Inf,
           linetype = "dashed", colour = "red", linewidth = 0.7)

shrimp_duration_plot
#ggsave(filename = "shrimp_duration_plot_temp_stun.png", plot = shrimp_duration_plot, width = 11, height = 9, dpi = 300)
#

#####ANALYSIS#####
shrimp_both$Total<-as.numeric(shrimp_both$Total)
shrimp_both$Source_File<-as.factor(shrimp_both$Source_File)
shrimp_both$stun_parameter<-as.factor(shrimp_both$stun_parameter)

summary_tbl <- shrimp_both%>%
  group_by(stun_parameter) %>%
  summarise(
    medn_value = median(Total, na.rm = TRUE),
    range   = range(Total, na.rm = TRUE),
    n          = n()
  )

##model = zero degrees plus stunning parameters

noise_data<-shrimp_both %>%
  filter(str_detect(stun_parameter, "0")|
           str_detect(stun_parameter, "2.5V")|
           str_detect(stun_parameter, "3V"))

noise<- glm(Total~ stun_parameter, data = noise_data)
summary(noise)

#0degs plus stunning parameters

#Call:
#  glm(formula = Total ~ stun_parameter, data = noise_data)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          85.600      7.601  11.261 9.78e-08 ***
#  stun_parameter2.5V  -64.100     10.292  -6.228 4.40e-05 ***
#  stun_parameter3V    -84.600     11.402  -7.420 8.06e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#(Dispersion parameter for gaussian family taken to be 288.8917)
#
#Null deviance: 21899.6  on 14  degrees of freedom
#Residual deviance:  3466.7  on 12  degrees of freedom
#AIC: 132.21

#Number of Fisher Scoring iterations: 2

#######now just temp#####

temp_data<-shrimp_both %>%
  filter(str_detect(stun_parameter, "0")|
           str_detect(stun_parameter, "2.5_5")|
           str_detect(stun_parameter, "-2.5"))


#temp_data$stun_parameter <- factor(temp_data$stun_parameter,
#                       levels = c("0", "-2.5", "2.5_5"))
temp_noise<- glm(Total~ stun_parameter, data = temp_data)
summary(temp_noise)

#Call:
#  glm(formula = Total ~ stun_parameter, data = temp_data)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            47.40      16.23   2.921 0.012807 *  
#  stun_parameter0        38.20      22.95   1.665 0.121825    
#stun_parameter2.5_5   100.80      22.95   4.393 0.000876 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 1316.267)

#Null deviance: 41693  on 14  degrees of freedom
#Residual deviance: 15795  on 12  degrees of freedom
#AIC: 154.96

#Number of Fisher Scoring iterations: 2


#######0.5 sampling post stun- slurry subsection####
#Define the folder path
folder_path <- "C:/...."

# Get all CSVs except those with 'double' in their filename
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

filtered_files <- csv_files[!grepl("double", basename(csv_files), ignore.case = TRUE)]


# Read each CSV, rename the first column, add filename as a new column
data_list <- lapply(filtered_files, function(file) {
  df <- read.csv(file,stringsAsFactors = FALSE)
  
  df <- df[-c(1, 2), ]
  
  
  # Rename first column to 'time'
  colnames(df)[1] <- "Time"
  colnames(df)[2] <-"Raw.EEG"
  colnames(df)[3] <-"Beta"
  colnames(df)[4] <-"Alpha"
  colnames(df)[5] <-"Delta"
  colnames(df)[6] <-"Theta"
  colnames(df)[7]<-"Raw.EEG.SEF"
  colnames(df)[8]<-"Raw.EEG.Median.0_500hz"
  colnames(df)[9]<-"All.0.5.32.Hz"
  colnames(df)[10]<-"Raw.EEG.Median.0.5_32hz"
  colnames(df)[11] <- "Comments"
  
  # Add source file column
  df$Source_File <- basename(file)
  
  return(df)
})
signal <- c("Beta","Alpha","Delta","Theta")


# Combine all data frames
combined_data <- do.call(rbind, data_list)

###seems to be rows with no data in 
combined_data[ ,2:10] <- lapply(combined_data[ ,2:10], function(x) as.numeric(x))
##get rid of nas
clean_combined_data <- na.omit(combined_data)

clean_combined_data <- clean_combined_data %>%
  mutate(Time = as.character(Time)) %>%  # Ensure time is character
  mutate(time_seconds = period_to_seconds(hms(Time))) %>%
  group_by(Source_File) %>%
  mutate(relative_time = time_seconds - min(time_seconds)) %>%
  ungroup()

###plot before MAD filtering

df_long <- clean_combined_data %>%
  pivot_longer(
    cols = "All.0.5.32.Hz",
    names_to = "band",
    values_to = "power"
  )

## some files have <=0 values that can't be log scaled so filter out
df_long_filtered <- df_long %>%
  filter(power > 0)


plots_before <- df_long_filtered %>%
  group_split(Source_File) %>%
  map(~ ggplot(.x, aes(x = relative_time, y = power, color = band)) +
        geom_line(linewidth = 0.5) +  scale_y_log10()+
        labs(title = unique(.x$Source_File),
             x = "Time (s)", y = "Power (µV²)", color = "EEG Band") +
        theme_minimal())
print(plots_before)


###removing big spikes using MAD before "in slurry" so we have a better  baseline reading


clean_combined_data2 <- clean_combined_data %>%
  group_by(Source_File) %>%
  mutate(
    stun_time = first(relative_time[Comments == "stun on"])
  ) %>%
  group_by(Source_File) %>%
  mutate(
    Med = median(All.0.5.32.Hz[relative_time >= 0 & relative_time < (stun_time - 100)], na.rm = TRUE),
    MAD = mad(All.0.5.32.Hz[relative_time >= 0 & relative_time < (stun_time - 100)], na.rm = TRUE),
    Keep = if_else(relative_time >= 0 & relative_time < (stun_time - 100),
                   abs(All.0.5.32.Hz - Med) <= 0.5 * MAD,
                   TRUE)
  ) %>%
  filter(Keep) %>%
  select(-Med, -MAD, -Keep, -stun_time)


clean_combined_data2 <- clean_combined_data2 %>%
  group_by(Source_File) %>%
  mutate(
    stun_time = first(relative_time[Comments == "in slurry"])
  ) %>%
  group_by(Source_File) %>%
  mutate(
    Med = median(All.0.5.32.Hz[relative_time >= (stun_time + 120)], na.rm = TRUE),
    MAD = mad(All.0.5.32.Hz[relative_time >= (stun_time + 120)], na.rm = TRUE),
    Keep = if_else(relative_time >= (stun_time + 120),
                   abs(All.0.5.32.Hz - Med) <= 1 * MAD,
                   TRUE)
  ) %>%
  filter(Keep) %>%
  select(-Med, -MAD, -Keep, -stun_time)

clean_combined_data2$ten_percent<-10
clean_combined_data2$hundred_percent<-100


###convert total power to PERCENTAGE###
df_long <- clean_combined_data2 %>%
  pivot_longer(
    cols = "All.0.5.32.Hz",
    names_to = "band",
    values_to = "power"
  )

## some files have <=0 values that can't be log scaled so filter out
df_long_filtered <- df_long %>%
  filter(power > 0)


plots <- df_long_filtered %>%
  group_split(Source_File) %>%
  map(~ ggplot(.x, aes(x = relative_time, y = power, color = band)) +
        geom_line(linewidth = 0.5) +  scale_y_log10()+
        labs(title = unique(.x$Source_File),
             x = "Time (s)", y = "Power (µV²)", color = "EEG Band") +
        theme_minimal())
print(plots)

#### facet plot
CLEAN2<-ggplot(df_long_filtered, aes(x = relative_time, y = power, color = band)) +
  geom_line(linewidth=0.5) + scale_y_log10()+
  facet_wrap(~ Source_File, scales = "free_x") +
  theme_minimal()
CLEAN2

comment_times <- clean_combined_data2 %>%
  filter(str_detect(Comments, regex("in slurry", ignore_case = TRUE))) %>%
  group_by(Source_File) %>%
  slice_min(relative_time, n = 1) %>%
  ungroup() %>%
  select(Source_File, relative_time)



plots <- df_long_filtered %>%
  group_split(Source_File) %>%
  map(function(file_df) {
    fname <- unique(file_df$Source_File)
    comment_time <- comment_times %>% filter(Source_File == fname) %>% pull(relative_time)
    
    ggplot(file_df, aes(x = relative_time, y = power, color = band)) +
      geom_line() + scale_y_log10()+
      geom_vline(xintercept = comment_time, linetype = "dashed", color = "red") +
      annotate("text",
               x = comment_time,
               y = max(file_df$power, na.rm = TRUE),
               label = "In slurry",
               hjust = -0.1, vjust = 1.2,
               color = "red", size = 3.5) +
      labs(title = fname, x = "Time (s)", y = "Power (µV²)", color = "EEG Band") +
      theme_minimal()
  })

print(plots)

eeg_df<-clean_combined_data2

# STEP 1: Identify time of 'in terminal anaesthesia' per file
slurry_times <- eeg_df %>%
  filter(str_detect(Comments, regex("in slurry", ignore_case = TRUE))) %>%
  group_by(Source_File) %>%
  summarise(slurry_time = min(relative_time), .groups = "drop")


# STEP 2: Join slurry time to full data
eeg_annotated <- eeg_df %>%
  left_join(slurry_times, by = "Source_File") %>%
  mutate(relative_time2 = relative_time - slurry_time)

#write.csv

baseline_df3 <- eeg_annotated %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "in slurry"])) %>%
  filter(relative_time >= 0 & relative_time < (stun_time - 100)) %>%
  arrange(Source_File, relative_time) %>%
  ungroup()

baseline_medians <- baseline_df3 %>%
  group_by(Source_File) %>%
  summarise(across(c(All.0.5.32.Hz),
                   ~median(.x, na.rm = TRUE),
                   .names = "baseline_{.col}"))

eeg_percent <- eeg_annotated %>%
  left_join(baseline_medians, by = "Source_File") %>%
  mutate(across(c(All.0.5.32.Hz),
                ~ (.x / get(paste0("baseline_", cur_column()))) * 100,
                .names = "pct_{.col}"))

baseline_window <- eeg_percent %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "stun on"])) %>%
  filter(relative_time >= 100 & relative_time < (stun_time - 100)) %>%
  ungroup() %>%
  mutate(relative_time2 = -300)

baseline_window_count<- eeg_percent %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "in slurry"])) %>%
  filter(relative_time >= 100 & relative_time < (stun_time - 100)) %>%
  mutate(n = n()) %>%      # count per group
  ungroup() %>%
  mutate(relative_time2 = -300)

# One unique n per Source_File
baseline_window_unique_n <- baseline_window_count %>%
  distinct(Source_File, n)
#write.csv()

###average all these data points as we do not the same number of values for each


###1 unique average across new percent column

baseline_summary <- baseline_window %>%
  group_by(Source_File, ten_percent,hundred_percent) %>%
  summarise(
    across(starts_with("pct_"), ~mean(.x, na.rm = TRUE)),
    relative_time2 = -300,
    bin = -1,
    .groups = "drop"
  )




####boxplots
###add 5 and minus 5 seconds to reduce noise
stun_times <- eeg_percent %>%
  group_by(Source_File) %>%
  summarise(
    stun_off_time = first(relative_time[Comments == "stun off"]) + 5,
    in_slurry_time = first(relative_time[Comments == "in slurry"]) - 5
  )

post_stun_window <- eeg_percent %>%
  left_join(stun_times, by = "Source_File") %>%
  filter(relative_time >= stun_off_time, relative_time <= in_slurry_time)


###apply MAD+/-1 for this part
post_stun_window2<- post_stun_window %>%
  pivot_longer(cols = c(All.0.5.32.Hz),
               names_to = "Band", values_to = "Value") %>%
  group_by(Source_File, Band) %>%
  mutate(
    Med = median(Value, na.rm = TRUE),
    MAD = mad(Value, na.rm = TRUE),
    Keep = abs(Value - Med) <= 1 * MAD  # adjust threshold as needed
  ) %>%
  filter(Keep) %>%
  select(-Med, -MAD, -Keep) %>%
  pivot_wider(names_from = Band, values_from = Value)


###divide by baseline x100

post_stun_window2<- post_stun_window %>%
  mutate(
    ps_pct_All_sub = (All.0.5.32.Hz/ baseline_All.0.5.32.Hz) * 100)

####change time

post_stun_window2 <- post_stun_window2 %>%
  group_by(Source_File) %>%
  mutate(time = row_number() - 1) %>%
  ungroup()

###find number of values used here.
summary_post_stun<-post_stun_window2 %>% group_by(Source_File) %>% summarise(all_count= length(!is.na( ps_pct_All_sub)))
#write.csv()

###doubles data points

#######EEG TRACE PLOT#######

post_stun_window2_long <- post_stun_window2 %>%
  pivot_longer(cols = starts_with("ps_pct"),
               names_to = "band",
               values_to = "percent_post_stun") 


library(scales)

ggplot(post_stun_window2_long, aes(x = time, y = percent_post_stun, color = band)) +
  geom_line() +
  scale_y_log10() +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) +  # Suggests ~6 "nice" breaks like 0, 1, 2, ...
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
  facet_wrap(~ Source_File, scales = "free_x") +
  labs(title = "EEG Band Power Over Time",
       x = "Time (s)", y = "% of baseline", color = "EEG Band") +
  theme_minimal()



###then we got mean of each band
post_stun_summary <- post_stun_window2 %>%
  group_by(Source_File,ten_percent,hundred_percent) %>%
  summarise(
    pct_All.0.5.32.Hz = mean(pct_All.0.5.32.Hz, na.rm = TRUE),
    .groups = "drop"
  )
####combine and plot 
post_stun_summary$label<-"post_stun"

baseline_summary$label<-"baseline"


new_baseline_summary<-baseline_summary[, c(1,2,3,4,7)] 
new_ps<-rbind(post_stun_summary,new_baseline_summary)

###get rid of files with artefacts###
new_ps<- new_ps %>%
  filter(!str_detect(Source_File, "120624_610_v3.csv"))
new_ps<- new_ps %>%
  filter(!str_detect(Source_File, "120624_609.csv"))

new_ps_2.5v<-new_ps%>%
  filter(!str_detect(Source_File, "150125_15subsample.csv") &
           !str_detect(Source_File, "120624_613subsample.csv")&
           !str_detect(Source_File, "190624_616subsample.csv")&
           !str_detect(Source_File, "180925_92subsample.csv"))
new_ps_2.5v$stun_parameter<-"2.5v"

new_ps_3v<-new_ps%>%
  filter(str_detect(Source_File, "150125_15subsample.csv")|
           str_detect(Source_File, "120624_613subsample.csv")|
           str_detect(Source_File, "190624_616subsample.csv")|
           str_detect(Source_File, "180925_92subsample.csv"))
new_ps_3v$stun_parameter<-"3v"

new_ps2<-rbind(new_ps_3v,new_ps_2.5v)


ps <- ggplot(new_ps2,aes(x = label,y = pct_All.0.5.32.Hz))+
geom_boxplot(aes(fill = stun_parameter),outlier.shape = NA, alpha = 0.3) +
geom_jitter(colour="black",width = 0.1,size = 2,alpha = 1,show.legend = FALSE) +
  geom_line(aes(group = Source_File),alpha = 0.7,colour="grey",show.legend = FALSE) +
  geom_hline(aes(yintercept = ten_percent), color = "red",linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black",linetype = "dotted") +
  scale_y_log10() +
  scale_x_discrete(labels = c("baseline" = "Baseline",
                              "post_stun" = "Post-stun")) +
  labs(x = "EEG section",
    y = "Ptot (%)",
    fill = "Stun parameter"
  ) +
  facet_wrap(~ stun_parameter, nrow = 1) +
  theme_minimal() +
  theme(
    text = element_text(size = 9),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )+
  scale_fill_manual(labels= c("2.5v"="ES-CS-R: 2-2.5 V cm⁻¹ 5s", 
                              "3v"="ES-CS-NR: 2.5-3 V cm⁻¹ 20s"),
                              values = c("2.5v" = "orange",
                               "3v"   = "firebrick1"))+
  theme(strip.text = element_blank(),
        strip.background = element_blank())+
  theme(axis.text.x = element_text(size = 20),
        text = element_text(size = 20))

ps
#ggsave(filename = "post_stun_slurry_0.5_subsample.png", plot = ps, width = 10, height = 6, dpi = 300)


summary_tbl <- new_ps2%>%
  group_by(stun_parameter, label) %>%
  summarise(
    median_value = median(pct_All.0.5.32.Hz, na.rm = TRUE),
    range  = range(pct_All.0.5.32.Hz, na.rm = TRUE),
    n          = n()
  )

####stats
###squareroot

new_ps2$sqrt_pct <- sqrt(new_ps2$pct_All.0.5.32.Hz)
new_ps2$stun_parameter<-as.factor(new_ps2$stun_parameter)
new_ps2$label<-as.factor(new_ps2$label)

new_ps2$Source_File<-as.factor(new_ps2$Source_File)

m2 <- glm(sqrt_pct ~ stun_parameter+label, data = new_ps2)
summary(m2)
results_tbl_3 <- broom.mixed::tidy(m2)
###no sig diff
#Call:
#glm(formula = sqrt_pct ~ stun_parameter + label, data = new_ps2)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)         20.81      22.71   0.917  0.37217   
#stun_parameter3v   -26.19      28.39  -0.923  0.36901   
#labelpost_stun      91.23      27.81   3.280  0.00442 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 3867.541)

#Null deviance: 110654  on 19  degrees of freedom
#Residual deviance:  65748  on 17  degrees of freedom
#AIC: 226.71

#Number of Fisher Scoring iterations: 2

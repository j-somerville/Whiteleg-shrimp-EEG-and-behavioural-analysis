####EEG script from Jasmine csv files from labchart#####
###Jan and Feb ELECTRICAL STUNNING RECOVERY data 2024
####Cleaning csv files that have been extracted from Labchart using a power tempplate
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

###All csv files are in power apart from SEF

# Define the folder path
folder_path <- "C:..."

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


###Before stun
clean_combined_data2 <- clean_combined_data %>%
  group_by(Source_File) %>%
  mutate(
    stun_time = first(relative_time[Comments == "Stun"])
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


###after stun
clean_combined_data2 <- clean_combined_data2 %>%
  group_by(Source_File) %>%
  mutate(
    stun_time = first(relative_time[Comments == "stun off"])
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

###convert power to PERCENTAGE###
df_long <- clean_combined_data2 %>%
  pivot_longer(
    cols = "All.0.5.32.Hz",
    names_to = "band",
    values_to = "power"
  )

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
library(dplyr)
library(ggplot2)
library(ggrepel)

comments_df <- df_long_filtered %>%
  filter(str_detect(Comments, "Stun Stage")) %>%
  group_by(Source_File, Comments) %>%
  arrange(relative_time) %>%
  slice(1) %>%
  ungroup()

ggplot(df_long_filtered, aes(x = relative_time, y = power, color = band)) +
  geom_line(linewidth = 0.5) +
  scale_y_log10() +
  facet_wrap(~ Source_File, scales = "free_x") +
  theme_minimal() +
  geom_vline(
    data = comments_df,
    aes(xintercept = relative_time),
    color = "red", linetype = "dashed", linewidth = 0.3
  ) +
  geom_text_repel(
    data = comments_df,
    aes(x = relative_time, y = max(df_long_filtered$power), label = Comments),
    angle = 90, size = 2.5, inherit.aes = FALSE,
    nudge_y = 0.1,    # push labels slightly above the line
    direction = "y",  # repel vertically
    segment.color = NA # hide connecting lines if you don’t want them
  )


### trying to plot vertical lines with comments ###
###do by in slurry comment

comment_times <- clean_combined_data2 %>%
  filter(str_detect(Comments, regex("stun off", ignore_case = TRUE))) %>%
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
               label = "Stun off",
               hjust = -0.1, vjust = 1.2,
               color = "red", size = 3.5) +
      labs(title = fname, x = "Time (s)", y = "Power (µV²)", color = "EEG Band") +
      theme_minimal()
  })

print(plots)


eeg_df<-clean_combined_data2

# STEP 1: Identify time of 'in slurry' per file
slurry_times <- eeg_df %>%
  filter(str_detect(Comments, regex("stun off", ignore_case = TRUE))) %>%
  group_by(Source_File) %>%
  summarise(slurry_time = min(relative_time), .groups = "drop")


# STEP 2: Join slurry time to full data
eeg_annotated <- eeg_df %>%
  left_join(slurry_times, by = "Source_File") %>%
  mutate(relative_time2 = relative_time - slurry_time)

#write.csv( eeg_annotated, "~/Vannemei project/VannameiData/LabChartFiles/TerminalAnaesthesiaFiles/R_outputfiles/eeg_annotated.csv")

baseline_df3 <- eeg_annotated %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "Stun"])) %>%
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
  mutate(stun_time = first(relative_time[Comments == "Stun"])) %>%
  filter(relative_time >= 100 & relative_time < (stun_time - 100)) %>%
  ungroup() %>%
  mutate(relative_time2 = -300)

baseline_window_count<- eeg_percent %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "Stun"])) %>%
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



#write.csv
subsampled<-subset(subsampled_summary, select = -c(n_points) )


#combine with baseline
new_subsample<-rbind(subsampled,baseline_summary)


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


recover<-ggplot(boxplot_data %>% 
                  filter(!bin %in% c(0)),
                aes(x = factor(bin), y = pct_value, fill = band)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(labels = c(
    "-1" = "<0"))+
  geom_jitter(width = 0.2, size = 2, alpha = 0.7, aes(colour = Source_File),show.legend = FALSE) +
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
  facet_wrap(~ band, scales = "free_y") +
  scale_y_log10() +
  labs(
    title = "ES recovery",
    x = "Time relative to ES (minutes)",
    y = "Percentage of Baseline"
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    strip.background = element_blank()
  )+  theme(text = element_text(size = 15))
recover

boxplot_data$sqrt_pct <- sqrt(boxplot_data$pct_value)

boxplot_data<-boxplot_data%>%
  filter(!bin %in% c(0))

boxplot_data$bin<-as.factor(boxplot_data$bin)
boxplot_data$Source_File<-as.factor(boxplot_data$Source_File)

m3 <- lmer(sqrt_pct ~ bin + (1|Source_File), data = boxplot_data)
summary(m3)

results_tbl<- broom.mixed::tidy(m3)

#write.csv
#ggsave(filename = "recovery_es.png", plot = recover, width = 14, height = 10, dpi = 300)



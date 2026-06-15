####EEG script from Jasmine csv files from labchart#####
###Jan and Feb COLD SHOCK data 2024 - -2.5, 0 and 2.5-5 degrees
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


# Define the folder path
folder_path <- ""

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

###removing big spikes using MAD before "in slurry" so we have a better  baseline reading

clean_combined_data2 <- clean_combined_data %>%
  group_by(Source_File) %>%
  mutate(
    stun_time = first(relative_time[Comments == "in slurry"])
  ) %>%
  group_by(Source_File) %>%
  mutate(
    Med = median(All.0.5.32.Hz[relative_time >= 0 & relative_time < (stun_time - 100)], na.rm = TRUE),
    MAD = mad(All.0.5.32.Hz[relative_time >= 0 & relative_time < (stun_time - 100)], na.rm = TRUE),
    Keep = if_else(relative_time >= 0 & relative_time < (stun_time - 100),
                   abs(All.0.5.32.Hz - Med) <= 0.5* MAD,
                   TRUE)
  ) %>%
  filter(Keep) %>%
  select(-Med, -MAD, -Keep, -stun_time)

###post slurry

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


###convert power to PERCENTAGE###
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

eeg_df<-clean_combined_data2

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
  mutate(n = n()) %>%      # count per group
  ungroup()

###find unique n number per group

baseline_df3_unique_n <- baseline_df3 %>%
  distinct(Source_File, n)

baseline_medians <- baseline_df3 %>%
  group_by(Source_File) %>%
  summarise(across(c(All.0.5.32.Hz),
                   ~median(.x, na.rm = TRUE),
                   .names = "baseline_{.col}"))

###then work out as percentage?
eeg_percent <- eeg_annotated %>%
  left_join(baseline_medians, by = "Source_File") %>%
  mutate(across(c(All.0.5.32.Hz),
                ~ (.x / get(paste0("baseline_", cur_column()))) * 100,
                .names = "pct_{.col}"))
###baseline parts should be near 100% (100 seconds before in slurry)

###Label baseline sample. As it is patchy, we are labelling it as "-300 secconds
###average all these data points as we do not have a lot
###getting data from after 100 seconds and 100 seconds before stun_on 
##labelling as -300 seconds
###first define before stun on and 100 seconds before 0
baseline_window <- eeg_percent %>%
  group_by(Source_File) %>%
  mutate(stun_time = first(relative_time[Comments == "in slurry"])) %>%
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
#write.csv


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

subsampled<-subset(subsampled_summary, select = -c(n_points) )
#write.csv
new_subsample<-rbind(subsampled,baseline_summary)


# 1. Extract baseline percent values per animal
baseline_ref <- baseline_summary %>%
  select(Source_File, starts_with("pct_")) %>%
  rename_with(~ paste0("ref_", .x), starts_with("pct_"))

# 2. Join baseline reference to subsampled data
subsampled_renorm <- new_subsample %>%
  left_join(baseline_ref, by = "Source_File") %>%
  mutate(
    across(
      starts_with("pct_"),
      ~ (.x / get(paste0("ref_", cur_column()))) * 100,
      .names = "new_{.col}"
    )
  )

#write.csv(subsampled_renorm, "subsampled_renorm_CS_all_epochs.csv")

#combine with baseline

boxplot_data <- subsampled_renorm  %>%
  select(Source_File, bin, ten_percent, hundred_percent, starts_with("new_")) %>%
  pivot_longer(
    cols = starts_with("new_"),
    names_to = "band",
    values_to = "pct_value"
  )


boxplot_data<- boxplot_data %>%
  mutate(band = recode(band,
                       "pct_All.0.5.32.Hz" = "0.5-32Hz"))


ggplot(boxplot_data, aes(x = factor(bin), y = pct_value, fill = band)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7, aes(colour = Source_File)) +
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
  facet_wrap(~ band, scales = "free_y") +
  scale_y_log10() +
  labs(
    title = "EEG Band Power (% of Baseline)",
    x = "Time relative to slurry (minutes)",
    y = "Percentage of Baseline"
  ) +
  theme_minimal()
######subset for temps-find temps in original dataframe and combine

clean_combined_data_temps <- clean_combined_data2  %>%
  group_by(Source_File) %>%
  mutate(
    Temp = case_when(
      any(grepl("\\+2", Comments, ignore.case = TRUE))   ~ 2.5,
      any(grepl("-2\\.", Comments, ignore.case = TRUE))  ~ -2.5,
      any(grepl("-0deg", Comments, ignore.case = TRUE))  ~ 0,
      any(grepl("0deg", Comments, ignore.case = TRUE))   ~ 0,
      any(grepl("0 deg", Comments, ignore.case = TRUE))   ~ 0,
      any(grepl("5deg", Comments, ignore.case = TRUE))   ~ 5,
      any(grepl("5 deg", Comments, ignore.case = TRUE))   ~ 5,
      any(grepl("5 degrees", Comments, ignore.case = TRUE))   ~ 5,
      
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

clean_temp_unique <- clean_combined_data_temps %>%
  group_by(Source_File) %>%
  slice(1) %>%
  ungroup()

boxplot_temps <- boxplot_data %>%
  left_join(clean_temp_unique %>% select(Source_File, Temp), by = "Source_File")


boxplot_temps<- boxplot_temps |>
  dplyr::filter(bin <= 30)

temp0<-subset(boxplot_temps,boxplot_temps$Temp=="0")
temp2.5<-subset(boxplot_temps,boxplot_temps$Temp=="2.5")
tempm2.5<-subset(boxplot_temps,boxplot_temps$Temp=="-2.5")
temp5<-subset(boxplot_temps,boxplot_temps$Temp=="5")


###-2.5
minus <- tempm2.5 %>% 
  filter(!bin %in% c(0,1)) %>%
  ggplot(aes(x = factor(bin), y = pct_value)) +
  geom_boxplot(outlier.shape = NA, fill="blue", alpha=0.2) +
  geom_jitter(width = 0.1, size = 1, alpha = 1,
              aes(colour = Source_File), show.legend = FALSE) +
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
  scale_y_log10() +
  labs(
    title = "-2.5°C",
    x = "Time relative to slurry immersion (minutes)",
    y = "Ptot (%)",
    fill = "Band"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)
  )+scale_x_discrete(labels = c(
    "-1" = "<0"))+
  geom_point(aes(x = 4, y =0.6), 
             shape = 8, 
             size = 4, 
             color = "black")+
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

  
minus

tempm2.5$sqrt_pct <- sqrt(tempm2.5$pct_value)

tempm2.5<-tempm2.5%>%
  filter(!bin %in% c(0,1))

tempm2.5$bin<-as.factor(tempm2.5$bin)
tempm2.5$Source_File<-as.factor(tempm2.5$Source_File)

hist(tempm2.5$sqrt_pct)

library(lmerTest)
m2 <- lmer(sqrt_pct ~ bin + (1|Source_File), data = tempm2.5)
summary(m2)

##log

library(broom.mixed)
results_tbl_minus2.5_log <- broom.mixed::tidy(m2)

#write.csv(results_tbl_minus2.5_log, "results_new_minus2.5.csv")

zero<-temp0 %>% 
  filter(!bin %in% c(0,1)) %>%
  ggplot(aes(x = factor(bin), y = pct_value)) +
  geom_boxplot(outlier.shape = NA, fill="grey",alpha=0.2) +
  geom_jitter(width = 0.1, size = 1, alpha = 1, aes(colour = Source_File), show.legend = FALSE) +
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
   scale_y_log10()+
  labs(title = "0°C",
       x = "Time relative to slurry immersion (minutes)",
       y = "Ptot (%)") +
  theme_minimal()+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+scale_x_discrete(labels = c(
          "-1" = "<0"))+
  geom_point(aes(x = 9, y =0.7), 
               shape = 8, 
               size = 4, 
               color = "black")+
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

zero


temp0$sqrt_pct <- sqrt(temp0$pct_value)

temp0<-temp0%>%
  filter(!bin %in% c(0,1))

temp0$bin<-as.factor(temp0$bin)
temp0$Source_File<-as.factor(temp0$Source_File)
hist(temp0$sqrt_pct)

library(lmerTest)

m <- lmer(sqrt_pct ~ bin + (1|Source_File), data = temp0)
summary(m)


results_tbl_zero_log <- broom.mixed::tidy(m)
#write.csv(results_tbl_zero_log, "results_new_0.csv")


###both 2.5-5####
both<-rbind(temp2.5,temp5)

both$sqrt_pct <- sqrt(both$pct_value)

both<-both%>%
  filter(!bin %in% c(0,1))

both$bin<-as.factor(both$bin)
both$Source_File<-as.factor(both$Source_File)

m3 <- lmer(sqrt_pct~ bin + (1|Source_File), data = both)
summary(m3)

#results_tbl_both <- broom.mixed::tidy(m3)

#write.csv(results_tbl_both, "results_both_cs_new.csv")


both_temp<-both%>%filter(!bin %in% c(0,1)) %>%
  ggplot(aes(x = factor(bin), y = pct_value)) +
  geom_boxplot(outlier.shape = NA, fill="orange",alpha=0.2) +
  geom_jitter(width = 0.1, size = 1, alpha = 1, aes(colour = Source_File), show.legend = FALSE) +
  geom_hline(aes(yintercept = ten_percent), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = hundred_percent), color = "black", linetype = "dotted") +
   scale_y_log10()+
  labs(title = "2.5-5°C",
       x = "Time relative to slurry immersion (minutes)",
       y = "Ptot (%)") +
  theme_minimal()+
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
  scale_x_discrete(labels = c(
    "-1" = "<0"))+
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA)
  )



both_temp

all<-ggarrange(both_temp,zero,minus, ncol=1, nrow=3, common.legend = TRUE)
all

#ggsave(filename = "alltemp_all_freq4_0.5.png", plot = all, width = 14, height = 10, dpi = 300)


############NOISE POST STUN############

#####add all filtered dataframes to get RAW eeg####
plots <- clean_combined_data %>%
  group_split(Source_File) %>%
  map(function(file_df) {
    
    fname <- unique(file_df$Source_File)
    
    # Extract slurry time from your new dataframe
    comment_time_val <- clean_combined_data %>%
      filter(Source_File == fname, Comments == "in slurry") %>%
      slice(1) %>%
      pull(relative_time)
    
    # Crop window
    file_df <- file_df %>%
      filter(
        relative_time >= comment_time_val - 100,
        relative_time <= comment_time_val + 600
      )
    
    ggplot(file_df, aes(x = relative_time, y = Raw.EEG)) +
      geom_line() +
      scale_y_log10() +
      geom_vline(xintercept = comment_time_val,
                 linetype = "dashed",
                 color = "red") +
      annotate("text",
               x = comment_time_val,
               y = max(file_df$Raw.EEG, na.rm = TRUE),
               label = "In slurry",
               hjust = -0.1, vjust = 1.2,
               color = "red", size = 3.5) +
      labs(
        title = fname,
        x = "Time (s)",
        y = "Power (µV²)"
      ) +
      theme_minimal()
  })
plots 

##manually finding minimum value after CS immersion (when high amplitude stops) from looking at graphs
###before filtering so clean_combined data

shrimp61<-subset(clean_combined_data, clean_combined_data$Source_File=="240225_61.csv")
min_row_61 <- shrimp61  %>%
  filter(relative_time >= 980, relative_time <= 1000) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##986 min time

##no shrimp 28
shrimp28<-subset(clean_combined_data, clean_combined_data$Source_File=="220225_28.csv")
min_row_28 <- shrimp28  %>%
  filter(relative_time >= 789,relative_time<= 820) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
###808

shrimp23<-subset(clean_combined_data, clean_combined_data$Source_File=="200225_23.csv")
min_row_23 <- shrimp23  %>%
  filter(relative_time >= 935, relative_time <= 950) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
###939 

shrimp19<-subset(clean_combined_data, clean_combined_data$Source_File=="200225_19.csv")
min_row_19 <- shrimp19  %>%
  filter(relative_time>= 850, relative_time <= 880) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##880

shrimp51<-subset(clean_combined_data, clean_combined_data$Source_File=="180225_51.csv")
min_row_51 <- shrimp51  %>%
  filter(relative_time>= 1340, relative_time<= 1350) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#1344...double check this one

shrimp50<-subset(clean_combined_data, clean_combined_data$Source_File=="180225_50.csv")
min_row_50 <- shrimp50  %>%
  filter(relative_time >= 1400, relative_time <= 1450) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#1408

shrimp49<-subset(clean_combined_data, clean_combined_data$Source_File=="180225_49.csv")
min_row_49 <- shrimp49  %>%
  filter(relative_time >= 1750, relative_time <= 1760) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
###slurry label seems very wrong by 100s
###1753

shrimp17<-subset(clean_combined_data, clean_combined_data$Source_File=="150125_17.csv")
min_row_17 <- shrimp17  %>%
  filter(relative_time >= 980, relative_time<= 1000) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##997

shrimp47<-subset(clean_combined_data, clean_combined_data$Source_File=="130225_47v2.csv")
min_row_47 <- shrimp47%>%
  filter(relative_time >= 1000, relative_time<= 1110) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##think second spike here might be interference due to flat nature of next peak
#1031

shrimp46<-subset(clean_combined_data, clean_combined_data$Source_File=="130225_46a.csv")
min_row_46 <- shrimp46 %>%
  filter(relative_time >= 1286, relative_time <= 1300) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#1300

shrimp35<-subset(clean_combined_data, clean_combined_data$Source_File=="030225_35_36.csv")
min_row_35 <- shrimp35 %>%
  filter(relative_time >= 750, relative_time <= 800) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
##776


shrimp33<-subset(clean_combined_data, clean_combined_data$Source_File=="030225_33_34.csv")
min_row_33 <- shrimp33 %>%
  filter(relative_time >= 800, relative_time <= 845) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#825


shrimp32<-subset(clean_combined_data, clean_combined_data$Source_File=="030225_32.csv")
min_row_32 <- shrimp32 %>%
  filter(relative_time >= 750, relative_time<= 800) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#676

shrimp31<-subset(clean_combined_data, clean_combined_data$Source_File=="030225_31.csv")
min_row_31 <- shrimp31 %>%
  filter(relative_time >= 720, relative_time <= 750) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)
#739

shrimp30<-subset(clean_combined_data, clean_combined_data$Source_File=="030225_30.csv")
min_row_30 <- shrimp30 %>%
  filter(relative_time >= 650, relative_time <= 700) %>%
  slice_min(order_by = Raw.EEG, n = 1, with_ties = FALSE)

shrimp_duration<-rbind(min_row_17,min_row_19,min_row_23,min_row_28,min_row_30,
                       min_row_32,min_row_33,min_row_35,min_row_46,min_row_31,
                       min_row_47,min_row_50,min_row_51,min_row_61, min_row_49)


shrimp_duration <- shrimp_duration %>%
  rename(noise_time = relative_time)

shrimp_duration <- shrimp_duration %>%
  left_join(slurry_times, by = "Source_File")

shrimp_duration<-shrimp_duration%>%
  mutate(Total= (noise_time-slurry_time))

shrimp_duration <- shrimp_duration%>%
  left_join(clean_temp_unique %>% select(Source_File, Temp), by = "Source_File")

shrimp_duration$Temp<-as.factor(shrimp_duration$Temp)
shrimp_duration_plot<-ggplot(shrimp_duration, aes(x=Temp, y=Total, fill=Temp)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=1)+
  labs(x = "Slurry ice temperature (°C)", 
       y = "Tail flip duration (s)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 1, vjust = 0.5, hjust=1))+
  theme(text = element_text(size=11))+
  scale_fill_manual(values = c("-2.5" = 'steelblue2', 
                               "0" = 'lightblue', 
                               "2.5" = 'peachpuff',
                               "5"="chocolate2"))
shrimp_duration_plot


shrimp_duration <- shrimp_duration %>%
  mutate(Temp = if_else(Temp == "2.5", "2.5_5", Temp))
shrimp_duration <- shrimp_duration %>%
  mutate(Temp = if_else(Temp == "5", "2.5_5", Temp))

#write.csv
shrimp_duration$Temp<-as.factor(shrimp_duration$Temp)
shrimp_duration_plot<-ggplot(shrimp_duration, aes(x=Temp, y=Total, fill=Temp)) + 
  geom_boxplot(outlier.shape = NA)+geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitter(width=0.1, height=.1), size=3)+
  labs(x = "Slurry ice temperature (°C)", 
       y = "Tail flip duration (s)",
       fill="Temperature")+
  theme_bw()+
  theme(axis.text.x = element_text(20))+
  scale_x_discrete(labels = c("2.5_5" = "2.5-5"))+
  theme(text = element_text(size=30))+
  scale_fill_manual(values = c("-2.5" = 'steelblue2', 
                               "0" = 'lightblue', 
                               "2.5_5"= "chocolate2"), labels=c("-2.5"="-2.5°C", "0"="0°C","2.5_5"="2.5 - 5°C"))
shrimp_duration_plot
#ggsave(filename = "shrimp_duration_plot.png", plot = shrimp_duration_plot, width = 14, height = 10, dpi = 300)


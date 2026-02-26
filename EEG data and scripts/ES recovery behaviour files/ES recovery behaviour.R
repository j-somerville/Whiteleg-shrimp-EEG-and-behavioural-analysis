####Buckieburn graphs - recovery in shrimp that were either stunned:
###at 3V for 20 seconds ("irreversible") n=6
###at 2.5V for 5 seconds ("reversible") n=6
###Experiments conducted on 10th and 11th of December 2025 at Nigel Bromage Research Centre
###animal recovery was observed (in person, and with video) after stunning for 30minutes
###stage 1 = immobile (y/n), stage 2=uncoordinated movement, stage 3 =coordinated and stage 4=completely righted/full recovery
####for the paper, we changed stages to 1=immobile, 2=coordinated, 3=righted


####3v animals - none of them completely righted, 2 got to coordinated but died around approx 2hrs
###2.5 animals all righted within half hour-time to right graph

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(dplyr)
library(stringr)


#setwd
recover<-read.csv("main_behaviour.csv")
summary(recover)
recover$time_seconds_stage<-as.numeric(recover$time_seconds_stage)
recover$stun_parameter<-as.factor(recover$stun_parameter)
recover$animal_id<-as.factor(recover$animal_id)

#####1s data
recover_germany<-read.csv("germany_recover_jasmine.csv")
summary(recover_germany)
recover_germany$time_seconds_stage<-as.numeric(recover_germany$time_seconds_stage)
recover_germany$stun_parameter<-as.factor(recover_germany$stun_parameter)
recover_germany$animal_id<-as.factor(recover_germany$animal_id)


####Just stage 3 and 4
new_recover<-dplyr::filter(recover, !grepl("stage_2",recovery_stage))
new_recover_germany<-dplyr::filter(recover_germany, !grepl("stage_2",recovery_stage))
recover_2.5<-subset(new_recover,new_recover$stun_parameter=="2.5_5")
recover_3<-subset(new_recover,new_recover$stun_parameter=="3_20")
recover_1_2v<-subset(new_recover_germany,new_recover_germany$stun_parameter=="2_1")
recover_1_2.5v<-subset(new_recover_germany,new_recover_germany$stun_parameter=="2.5_1")




###############newist version

library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

# Step 1: Define bins
max_time <- max(new_recover$time_seconds_stage, na.rm = TRUE)
breaks   <- seq(0, max_time + 60, by = 60)

# Step 2: Assign bins
recover_binned <- new_recover %>%
  mutate(time_bin = cut(time_seconds_stage, breaks = breaks, right = FALSE))

# Step 3: Grid per stun_parameter
grid <- expand.grid(
  animal_id = unique(recover_binned$animal_id),
  time_bin  = levels(recover_binned$time_bin),
  stun_parameter = unique(recover_binned$stun_parameter)
)

recover_ff <- grid %>%
  left_join(recover_binned, by = c("animal_id","time_bin","stun_parameter")) %>%
  group_by(stun_parameter, animal_id) %>%
  arrange(animal_id, time_bin) %>%
  mutate(recovery_stage = zoo::na.locf(recovery_stage, na.rm = FALSE)) %>%
  ungroup()

# Step 4: Correct denominators: distinct animals per stun_parameter
total_animals <- new_recover %>%
  group_by(stun_parameter) %>%
  summarise(n = n_distinct(animal_id), .groups="drop")

# Step 5: Proportions per stage per bin per stun_parameter
plot_data <- recover_ff %>%
  group_by(stun_parameter, time_bin, recovery_stage) %>%
  summarise(proportion = n_distinct(animal_id) /
              total_animals$n[match(stun_parameter,total_animals$stun_parameter)] * 100,
            .groups="drop") %>%
  complete(time_bin, recovery_stage = paste0("stage_",3:4),
           stun_parameter, fill = list(proportion=0)) %>%
  mutate(proportion = tidyr::replace_na(proportion,0)) %>%
  separate(time_bin, into=c("start","end"), sep=",", remove=FALSE) %>%
  mutate(start = as.numeric(gsub("\\[|\\(|\\]", "", start)),
         end   = as.numeric(gsub("\\[|\\)|\\]", "", end)),
         time_mid = (start+end)/2) %>%
  filter(!is.na(recovery_stage))

# Step 6: Carry forward 100% once reached (flat line to end of time axis)
plot_data <- plot_data %>%
  group_by(recovery_stage, stun_parameter) %>%
  arrange(time_mid) %>%
  mutate(
    first_full = ifelse(proportion == 100, time_mid, NA_real_),
    cutoff     = suppressWarnings(min(first_full, na.rm = TRUE)),
    proportion = ifelse(!is.infinite(cutoff) & time_mid >= cutoff, 100, proportion)
  ) %>%
  ungroup()

# Step 7: Plot
both<-ggplot(plot_data, aes(x = time_mid, y = proportion,
                      group = interaction(recovery_stage, stun_parameter))) +
  geom_line(aes(color = recovery_stage, linetype = stun_parameter), size = 1.2) +
  labs(x = "Time (s)", y = "Proportion of animals (%)",
       color = "Recovery stage", linetype = "Stun parameter")+
  theme_minimal() +
  guides(
    color    = guide_legend(override.aes = list(linetype = c("solid","dashed"))),
  )+
    scale_color_manual(values = c("orange","palegreen3"),
                     labels = c("stage_3"="Stage 2: Coordinated","stage_4"="Stage 3: Fully righted")) +
  scale_linetype_manual(values  = c("2.5_5"="solid","3_20"="21"), labels=c("2.5_5"="2.5 V cm⁻¹ 5s", "3_20"="3 V cm⁻¹ 20s")) +
  ylim(0,100) + xlim(0,2000)+
  theme(text = element_text(size = 30),
        axis.text.x = element_text(angle = 0, hjust = 0.5))
both
#ggsave(filename = "recovery_behaviour3.png", plot = both, width = 14, height = 10, dpi = 300)


###analysis####
summary_tbl <- recover_binned%>%
  group_by(stun_parameter, recovery_stage) %>%
  summarise(
    mean = mean(time_seconds_stage, na.rm = TRUE),
    sd   = sd(time_seconds_stage, na.rm = TRUE),
    n          = n()
  )

recover_binned$time_seconds_stage<-as.numeric(recover_binned$time_seconds_stage)
recover_binned$stun_parameter<-as.factor(recover_binned$stun_parameter)

hist(recover_binned$time_seconds_stage)


behaviour<- glm(time_seconds_stage~ recovery_stage+stun_parameter, data = recover_binned)
summary(behaviour)

#stage 3 takes signifcantly longer in 3v animals compared to 2v animals.
###stage 4 takes signifcantly longer than stage 3 for
##very high deviance, again poor model fit due to missing stage 4 data as animals never reached stage 4 for 3v

#Call:
#  glm(formula = time_seconds_stage ~ stun_para_recover, data = recover_binned)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                       179.7      143.3   1.254  0.23586    
#stun_para_recover2.5_5_stage_4   1256.0      202.6   6.198 6.73e-05 ***
#  stun_para_recover3_20_stage_3    1303.8      286.6   4.550  0.00083 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 123179.9)

#Null deviance: 6870588  on 13  degrees of freedom
#Residual deviance: 1354979  on 11  degrees of freedom
#(10 observations deleted due to missingness)
#AIC: 208.45



#####average time to stage table
recover_germany<-read.csv("germany_recover_jasmine.csv")
summary(recover_germany)
recover_germany$time_seconds_stage<-as.numeric(recover_germany$time_seconds_stage)
recover_germany$stun_parameter<-as.factor(recover_germany$stun_parameter)
recover_germany$animal_id<-as.factor(recover_germany$animal_id)

recover_germany2<-subset(recover_germany, recover_germany$location=="germany")
recover_germany3 <- recover_germany2 %>% filter(!is.na(time_seconds_stage))
recover_germany3$time_seconds_stage<-as.numeric(recover_germany3$time_seconds_stage)

summary_table <- recover_germany3 %>%
  group_by(recovery_stage, stun_parameter) %>%
  summarise(
    mean_time = mean(time_seconds_stage, na.rm = TRUE),
    sd_time   = sd(time_seconds_stage, na.rm = TRUE),
    range_time = range(time_seconds_stage, na.rm = TRUE),
    count_individuals = n()
  )


hist(log(recover_germany3$time_seconds_stage))

behaviour_germany<- glm(time_seconds_stage~ recovery_stage+stun_parameter, data = recover_germany3)
summary(behaviour_germany)


#Call:
#glm(formula = time_seconds_stage ~ recovery_stage + stun_parameter, 
 #  data = recover_germany3)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)              20.02      10.02   1.998  0.05313 . 
#recovery_stagestage_4    35.45      11.57   3.063  0.00407 **
#  stun_parameter2_1       -24.45      11.57  -2.112  0.04146 * 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for gaussian family taken to be 1339.647)

#Null deviance: 68112  on 39  degrees of freedom
#Residual deviance: 49567  on 37  degrees of freedom
#AIC: 406.4

#Number of Fisher Scoring iterations: 2


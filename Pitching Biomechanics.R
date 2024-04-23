##clear working directory
rm(list=ls())

install.packages("car")

library(tidyverse)
library(readxl)
library(ggplot2)
library(car)




################# Data Management #################
################# Data Management #################
################# Data Management #################



metadata <-read.csv("/Users/rtgast27/Library/Mobile Documents/com~apple~CloudDocs/SPRT 6110/Final Project/metadata.csv")
joint_angles <-read.csv("/Users/rtgast27/Library/Mobile Documents/com~apple~CloudDocs/SPRT 6110/Final Project/joint_angles 2.csv")
poi_metrics <-read.csv("/Users/rtgast27/Library/Mobile Documents/com~apple~CloudDocs/SPRT 6110/Final Project/poi_metrics.csv")



length(unique(metadata$user))

trimmed_joint_angles <- subset(joint_angles, select = -c(session_pitch,time,pkh_time,fp_10_time,fp_100_time,MER_time,BR_time,MIR_time) )

upper_joint_angles <- trimmed_joint_angles[,c(4:6,13:18,25:27,34:42)]

upper_joint_angles <- subset(upper_joint_angles, select = -c(elbow_angle_y, glove_wrist_angle_z, glove_elbow_angle_y) )

# correlation of entire dataframe 
cor_results <- as.data.frame(cor(upper_joint_angles))



expanded_UJA <- joint_angles[, c(1:2,6:8,15:20,27:29,36:44,52)]

BR_POI <- poi_metrics[, c(1,3,5,42,45,70,75)]

UJA_at_BR <- subset(expanded_UJA, time == BR_time)

upper_metrics <- merge(BR_POI,UJA_at_BR, by="session_pitch")



################# Linear Regression #################
################# Linear Regression #################
################# Linear Regression #################



##Linear Model 1
linearModel1 <- lm(pitch_speed_mph ~ torso_rotation_br + stride_length + 
                     lead_grf_x_max + elbow_angle_x + elbow_angle_z +
                     shoulder_angle_x + shoulder_angle_y + shoulder_angle_z + 
                     wrist_angle_x + wrist_angle_y + wrist_angle_z , data=upper_metrics)
summary(linearModel1)


##Linear Model 2
linearModel2 <- lm(pitch_speed_mph ~ torso_rotation_br + 
                     lead_grf_x_max + elbow_angle_x + elbow_angle_z +
                     shoulder_angle_x + shoulder_angle_y, data=upper_metrics)
summary(linearModel2)


##Linear Model 3
linearModel3 <- lm(pitch_speed_mph ~ torso_rotation_br + 
                     lead_grf_x_max + elbow_angle_x + elbow_angle_z +
                     shoulder_angle_x + shoulder_angle_y + glove_elbow_angle_x + 
                     glove_elbow_angle_z + glove_shoulder_angle_x + 
                     glove_shoulder_angle_y, data=upper_metrics)

summary(linearModel3)


##Linear Model 4
linearModel4 <- lm(pitch_speed_mph ~ torso_rotation_br + 
                     lead_grf_x_max + elbow_angle_x + elbow_angle_z, data=upper_metrics)
summary(linearModel4)


##Linear Model 5
linearModel5 <- lm(pitch_speed_mph ~ torso_rotation_br + 
                     lead_grf_x_max +
                     shoulder_angle_x + shoulder_angle_y, data=upper_metrics)
summary(linearModel5)

##Linear Model 6
linearModel6 <- lm(pitch_speed_mph ~ torso_rotation_br + 
                     lead_grf_x_max + elbow_angle_x + elbow_angle_z +
                     shoulder_angle_x + shoulder_angle_y + glove_shoulder_angle_x + 
                     glove_shoulder_angle_y, data=upper_metrics)

summary(linearModel6)

##Linear Model 7
linearModel7 <- lm(pitch_speed_mph ~ torso_rotation_br + 
                     lead_grf_x_max + rear_grf_x_max + elbow_angle_x + elbow_angle_z +
                     shoulder_angle_x + shoulder_angle_y + glove_shoulder_angle_x + 
                     glove_shoulder_angle_y, data=upper_metrics)

summary(linearModel7)

vif_values <- vif(linearModel6)
print(vif_values)

vif_values <- vif(linearModel7)
print(vif_values)



################# t tests #################
################# t tests #################
################# t tests #################


## Sort pitchers' upper body metrics in decreasing order by pitch speed
pitch_speed_sort <- upper_metrics[order(upper_metrics$pitch_speed_mph,decreasing = TRUE),]


## Create subset of pitchers in the top 20% of velocity
top_20percent <-head(pitch_speed_sort, 82)

summary(top_20percent)

top_20percent$group <- "Top20"


## Create subset of pitchers in the bottom 20% of velocity
bottom_20percent <- tail(pitch_speed_sort, 82)

summary(bottom_20percent)

bottom_20percent$group <- "Bottom20"


## Torso Rotation t-test
result <- t.test(top_20percent$torso_rotation_br, bottom_20percent$torso_rotation_br)

print(result)

sd(top_20percent$torso_rotation_br)

sd(bottom_20percent$torso_rotation_br)


## Elbow angle x t-test
result <- t.test(top_20percent$elbow_angle_x, bottom_20percent$elbow_angle_x)

print(result)

sd(top_20percent$elbow_angle_x)

sd(bottom_20percent$elbow_angle_x)


## Elbow angle z t-test
result <- t.test(top_20percent$elbow_angle_z, bottom_20percent$elbow_angle_z)

print(result)

sd(top_20percent$elbow_angle_z)

sd(bottom_20percent$elbow_angle_z)


## Shoulder angle x t-test
result <- t.test(top_20percent$shoulder_angle_x, bottom_20percent$shoulder_angle_x)

print(result)

sd(top_20percent$shoulder_angle_x)

sd(bottom_20percent$shoulder_angle_x)


## Shoulder angle y t-test
result <- t.test(top_20percent$shoulder_angle_y, bottom_20percent$shoulder_angle_y)

print(result)

sd(top_20percent$shoulder_angle_y)

sd(bottom_20percent$shoulder_angle_y)


## Glove shoulder angle x t-test
result <- t.test(top_20percent$glove_shoulder_angle_x, bottom_20percent$glove_shoulder_angle_x)

print(result)

sd(top_20percent$glove_shoulder_angle_x)

sd(bottom_20percent$glove_shoulder_angle_x)


## Glove shoulder angle y t-test
result <- t.test(top_20percent$glove_shoulder_angle_y, bottom_20percent$glove_shoulder_angle_y)

print(result)

sd(top_20percent$glove_shoulder_angle_y)

sd(bottom_20percent$glove_shoulder_angle_y)


## Lead GRF t-test
result <- t.test(top_20percent$lead_grf_x_max, bottom_20percent$lead_grf_x_max)

print(result)

sd(top_20percent$lead_grf_x_max)

sd(bottom_20percent$lead_grf_x_max, na.rm = TRUE)


## Rear GRF t-test
result <- t.test(top_20percent$rear_grf_x_max, bottom_20percent$rear_grf_x_max)

print(result)

sd(top_20percent$rear_grf_x_max)

sd(bottom_20percent$rear_grf_x_max, na.rm = TRUE)




################# Data Visualizations #################
################# Data Visualizations #################
################# Data Visualizations #################


## Join top and bottom 20% datasets 
top_and_bottom_20 <- rbind(top_20percent,bottom_20percent)


## Pitch speed mph
ggplot(top_and_bottom_20, aes(x = group, y = pitch_speed_mph, fill = group)) +
  geom_boxplot() +
  labs(title = "Pitch Speed Comparison", x = "Percentile", y = "Pitch Speed (mph)") +
  theme_minimal() +
  scale_fill_manual(breaks = top_and_bottom_20$group, values = c("#1b98e0", "#FF2C3C")) 


## Torso Rotation
ggplot(top_and_bottom_20, aes(x = group, y = torso_rotation_br, fill = group)) +
  geom_boxplot() +
  labs(title = "Average Torso Rotation at Release", x = "Percentile", y = "Torso Rotation (deg)") +
  theme_minimal() +
  scale_fill_manual(breaks = top_and_bottom_20$group, values = c("#1b98e0", "#FF2C3C")) 



## Comparison of ground reaction force of push off leg between top and bottom 20% fastball velocity pitchers
avg_poGRF_top20 <- round(mean(top_20percent$rear_grf_x_max),2)

avg_poGRF_bottom20 <- round(mean(bottom_20percent$rear_grf_x_max,na.rm = TRUE),2)



## Comparison of ground reaction force of plant leg between top and bottom 20% fastball velocity pitchers
avg_plGRF_top20 <- round(mean(top_20percent$lead_grf_x_max),2)

avg_plGRF_bottom20 <- round(mean(bottom_20percent$lead_grf_x_max, na.rm = TRUE),2)



## Lead GRF
ggplot(top_and_bottom_20, aes(x = group, y = lead_grf_x_max, fill = group)) +
  geom_boxplot() +
  labs(title = "Average GRF of Plant Leg at Release", x = "Percentile", y = "GRF (N)") +
  theme_minimal() +
  scale_fill_manual(breaks = top_and_bottom_20$group, values = c("#1b98e0", "#FF2C3C")) 


## Push off GRF
ggplot(top_and_bottom_20, aes(x = group, y = rear_grf_x_max, fill = group)) +
  geom_boxplot() +
  labs(title = "Average GRF of Push Off Leg", x = "Percentile", y = "GRF (N)") +
  theme_minimal() +
  scale_fill_manual(breaks = top_and_bottom_20$group, values = c("#1b98e0", "#FF2C3C")) 



## Calculate average elbow angle x for pitchers in the top and bottom 20% for velocity
avg_eax_top20 <- round(mean(top_20percent$elbow_angle_x),2)

avg_eax_bottom20 <- round(mean(bottom_20percent$elbow_angle_x),2)


## Elbow angle x top 20
ggplot(top_20percent, aes(x=elbow_angle_x)) +   
  geom_histogram(color="black", fill="#1b98e0")+
  labs(title = "Elbow Angle x of Top 20%") +
  scale_y_continuous(name="Number of Pitchers", breaks = seq(0, 15, 2),limits=c(0, 15)) +
  scale_x_continuous(name="Elbow Angle x (deg)")+
  geom_vline(data = top_20percent,
      mapping = aes(xintercept=avg_eax_top20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_eax_top20-1, y=10, label=avg_eax_top20)


## Elbow angle x bottom 20
ggplot(bottom_20percent, aes(x=elbow_angle_x)) +   
  geom_histogram(color="black", fill="#1b98e0")+
  labs(title = "Elbow Angle x of Bottom 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 15)) +
  scale_x_continuous(name="Elbow Angle x (deg)")+
  geom_vline(data = bottom_20percent,
             mapping = aes(xintercept=avg_eax_bottom20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_eax_bottom20-1, y=10, label=avg_eax_bottom20)



## Calculate average elbow angle z for pitchers in the top and bottom 20% for velocity
avg_eaz_top20 <- round(mean(top_20percent$elbow_angle_z),2)

avg_eaz_bottom20 <- round(mean(bottom_20percent$elbow_angle_z),2)


## Elbow angle z top 20
ggplot(top_20percent, aes(x=elbow_angle_z)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Elbow Angle z of Top 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Elbow Angle z (deg)")+
  geom_vline(data = top_20percent,
             mapping = aes(xintercept=avg_eaz_top20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_eaz_top20-3, y=10, label=avg_eaz_top20)


## Elbow angle z bottom 20
ggplot(bottom_20percent, aes(x=elbow_angle_z)) +   
  geom_histogram(color="black", fill="#1b98e0")+
  labs(title = "Elbow Angle z of Bottom 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Elbow Angle z (deg)")+
  geom_vline(data = bottom_20percent,
             mapping = aes(xintercept=avg_eaz_bottom20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_eaz_bottom20-2, y=10, label=avg_eaz_bottom20)




## Calculate average shoulder angle x for pitchers in the top and bottom 20% for velocity
avg_sax_top20 <- round(mean(top_20percent$shoulder_angle_x),2)

avg_sax_bottom20 <- round(mean(bottom_20percent$shoulder_angle_x),2)


## Shoulder angle x top 20
ggplot(top_20percent, aes(x=shoulder_angle_x)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Shoulder Angle x of Top 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Shoulder Angle x (deg)")+
  geom_vline(data = top_20percent,
             mapping = aes(xintercept=avg_sax_top20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_sax_top20-1.5, y=10, label=avg_sax_top20)


## Shoulder angle x bottom 20
ggplot(bottom_20percent, aes(x=shoulder_angle_x)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Shoulder Angle x of Bottom 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Shoulder Angle x (deg)")+
  geom_vline(data = bottom_20percent,
             mapping = aes(xintercept=avg_sax_bottom20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_sax_bottom20-1.5, y=10, label=avg_sax_bottom20)



## Calculate average shoulder angle y for pitchers in the top and bottom 20% for velocity
avg_say_top20 <- round(mean(top_20percent$shoulder_angle_y),2)

avg_say_bottom20 <- round(mean(bottom_20percent$shoulder_angle_y),2)



## Shoulder angle y top 20
ggplot(top_20percent, aes(x=shoulder_angle_y)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Shoulder Angle y of Top 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Shoulder Angle y (deg)")+
  geom_vline(data = top_20percent,
             mapping = aes(xintercept=avg_say_top20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_say_top20-1.5, y=10, label=avg_say_top20)



## Shoulder angle y bottom 20
ggplot(bottom_20percent, aes(x=shoulder_angle_y)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Shoulder Angle y of Bottom 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Shoulder Angle y (deg)")+
  geom_vline(data = bottom_20percent,
             mapping = aes(xintercept=avg_say_bottom20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_say_bottom20-1.5, y=10, label=avg_say_bottom20)




## Calculate average glove shoulder angle x for pitchers in the top and bottom 20% for velocity
avg_gsax_top20 <- round(mean(top_20percent$glove_shoulder_angle_x),2)

avg_gsax_bottom20 <- round(mean(bottom_20percent$glove_shoulder_angle_x),2)



## Glove shoulder angle x top 20
ggplot(top_20percent, aes(x=glove_shoulder_angle_x)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Glove Shoulder Angle x of Top 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Glove Shoulder Angle x (deg)")+
  geom_vline(data = top_20percent,
             mapping = aes(xintercept=avg_gsax_top20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_gsax_top20-3.5, y=10, label=avg_gsax_top20)



## Glove shoulder angle x bottom 20
ggplot(bottom_20percent, aes(x=glove_shoulder_angle_x)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Glove Shoulder Angle x of Bottom 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Glove Shoulder Angle x (deg)")+
  geom_vline(data = bottom_20percent,
             mapping = aes(xintercept=avg_gsax_bottom20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_gsax_bottom20-3.5, y=10, label=avg_gsax_bottom20)




## Calculate average glove shoulder angle y for pitchers in the top and bottom 20% for velocity
avg_gsay_top20 <- round(mean(top_20percent$glove_shoulder_angle_y),2)

avg_gsay_bottom20 <- round(mean(bottom_20percent$glove_shoulder_angle_y),2)



## Glove shoulder angle y top 20
ggplot(top_20percent, aes(x=glove_shoulder_angle_y)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Glove Shoulder Angle y of Top 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Glove Shoulder Angle y (deg)")+
  geom_vline(data = top_20percent,
             mapping = aes(xintercept=avg_gsay_top20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_gsay_top20-1.5, y=10, label=avg_gsay_top20)



## Glove shoulder angle y bottom 20
ggplot(bottom_20percent, aes(x=glove_shoulder_angle_y)) +   
  geom_histogram(color="black", fill="#1b98e0") +
  labs(title = "Glove Shoulder Angle y of Bottom 20%") +
  scale_y_continuous(name="Number of Pitchers",breaks = seq(0, 15, 2),limits=c(0, 12)) +
  scale_x_continuous(name="Glove Shoulder Angle y (deg)")+
  geom_vline(data = bottom_20percent,
             mapping = aes(xintercept=avg_gsay_bottom20),linewidth=1, linetype="dashed") +
  annotate("text", x=avg_gsay_bottom20-2, y=10, label=avg_gsay_bottom20)





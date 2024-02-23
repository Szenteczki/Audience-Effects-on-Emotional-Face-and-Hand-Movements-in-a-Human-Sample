library(tidyverse)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggplot2)
library(rstatix)
library(bayestestR)
library(brms)
library(bayesplot)

home <- "~/" # FILL IN YOUR WORKING DIRECTORY HERE
setwd(home)

set.seed(123)
options(scipen=999) # no scientific notation in DFs
ppi <- 300

## DATA IMPORT & FILTERING FUNCTION
library(data.table)
read_infiles <- function(flnm) {
  read_csv(flnm) %>%
    mutate(filename = flnm) %>% # append file name for summary stats
    select(filename, confidence, AU01_r, AU02_r, AU04_r, AU05_r, AU06_r, AU07_r, 
           AU09_r, AU10_r, AU12_r, AU14_r, AU15_r, AU17_r, AU20_r, AU23_r, AU25_r, 
           AU26_r, AU45_r, AU28_c, 
           AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, 
           AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, AU45_c) %>% # select all AUs, in quant (r) and presence (c) formats
    filter(confidence > 0.95) %>% # remove low-confidence frames
    select(-confidence) %>% # drop col now that filtering is done
    group_by(filename) %>%
    summarise_if(.predicate = function(x) is.numeric(x),
                 .funs = funs(mean="mean")) %>% # calculate mean values for each AU, in each file
    mutate(private_id = str_extract(filename, "\\d{7}")) %>%
    mutate(valence = case_when(str_detect(filename, regex('fear', ignore_case = T)) ~ "fear",
                               str_detect(filename, regex('amusement', ignore_case = T)) ~ "amusement", 
                               str_detect(filename, regex('neutral', ignore_case = T)) ~ "neutral",
                               TRUE ~ filename)) %>% # case-insensitive method for getting valence of each video
    mutate(stimulus = str_extract(filename, "(?<=_)[^_ ]+(?=\\.csv)")) %>% # method for getting video name (between _ and file extension)
    select(-filename) # drop col now that we've extracted necessary info
}

## IMPORT ALONE CSV DATASETS (N = 480) UNCOMMENT TO RE-CALCULATE RESULT FROM RAW DATA
# alone_dir <- "~/of/input/ALONE_CSV"
# setwd(alone_dir)
# alone_df <-list.files(pattern = "*.csv", full.names = T) %>% 
#   map_df(~read_infiles(.))
# setwd(home)
# write.table(alone_df, file = "output/alone.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

## merge metadata (cleared for public release) from private database to produce final output (below)
alone_df <- read.table("input/alone.txt", header = T) # load pre-computed dataset w/metadata

numeric_columns <- alone_df %>%
  select_if(is.numeric) %>%
  select(-c(age, private_id), -contains("_c"))

melted_data <- melt(numeric_columns)

qq_plot_alone <- ggplot(melted_data, aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Q-Q Plots for Alone Condition", y="", x="")
print(qq_plot_alone)

## IMPORT SOCIAL CSV DATASETS (N = 480) UNCOMMENT TO RE-CALCULATE RESULT FROM RAW DATA
# social_dir <- "~/of/input/SOCIAL_CSV"
# setwd(social_dir)
# social_df <-list.files(pattern = "*.csv", full.names = T) %>% 
#   map_df(~read_infiles(.))
# setwd(home)
# write.table(social_df, file = "output/social.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

## merge metadata (cleared for public release) from private database to produce final output (below)
social_df <- read.table("input/social.txt", header = T) # load pre-computed dataset w/metadata

numeric_columns <- social_df %>%
  select_if(is.numeric) %>%
  select(-c(age, private_id), -contains("_c"))

melted_data <- melt(numeric_columns)

qq_plot_social <- ggplot(melted_data, aes(sample = value)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Q-Q Plots for Social Condition", y="", x="")
print(qq_plot_social)

## master datasheet (all data)
all_df <- rbind(alone_df, social_df)
str(all_df)

fix <- c("private_id", "age", "gender", "ethnicity", "condition", "valence", "stimulus", 
         "valence_self_report", "recognize_vid_self_report",
         "recognize_vid_binary", "partner_relationship","face_touching", "gaze") # columns to change from chr to factor
all_df[fix] <- lapply(all_df[fix], factor)  ## as.factor() could also be used
str(all_df)

###########################################################################
## Descriptives and Supplementary figures ##
###########################################################################

#Descriptives
## ALONE CONDITION INTENSITY
alone_intensity_summary_stat <- alone_df %>%
  select(valence, AU01, AU02, AU04, AU05, AU06, AU07,
         AU09, AU10, AU12, AU14, AU15, AU17, AU20, AU23, AU25,
         AU26, AU45) %>%
  group_by(valence) %>%
  summarise_all(funs(mean, sd))
write.table(alone_intensity_summary_stat, "output/alone_intensity_summary_stat.txt",sep="\t",row.names=F)

## SOCIAL CONDITION INTENSITY
social_intensity_summary_stat <- social_df %>%
  select(valence, AU01, AU02, AU04, AU05, AU06, AU07,
         AU09, AU10, AU12, AU14, AU15, AU17, AU20, AU23, AU25,
         AU26, AU45) %>%
  group_by(valence) %>%
  summarise_all(funs(mean, sd))
write.table(social_intensity_summary_stat, "output/social_intensity_summary_stat.txt",sep="\t",row.names=F)

## ALONE CONDITION ACTIVITY
alone_activity_summary_stat <- alone_df %>%
  select(valence, AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c,
         AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c,
         AU26_c, AU28_c, AU45_c) %>%
  group_by(valence) %>%
  summarise_all(funs(mean, sd))
write.table(alone_activity_summary_stat, "output/alone_activity_summary_stat.txt",sep="\t",row.names=F)

## SOCIAL CONDITION ACTIVITY
social_activity_summary_stat <- social_df %>%
  select(valence, AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c,
         AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c,
         AU26_c, AU28_c, AU45_c) %>%
  group_by(valence) %>%
  summarise_all(funs(mean, sd))
write.table(social_activity_summary_stat, "output/social_activity_summary_stat.txt",sep="\t",row.names=F)

#Panel Figure S1 (AU activity)
obj1_alone_vs_social_all_ac <- all_df %>%
  select(condition, private_id, valence, AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, 
         AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, 
         AU26_c, AU28_c, AU45_c) %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_activity", AU01_c_1:AU45_c_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_activity = as.numeric(AU_activity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_vs_social_all_ac)

obj1_alone_vs_social_all_stat_ac <- obj1_alone_vs_social_all_ac %>%
  group_by(AU) %>% 
  wilcox_test(AU_activity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_all_stat_ac, "output/obj1_alone_vs_social_all_ac.txt",sep="\t",row.names=F)

Fig_S1a <- ggplot(obj1_alone_vs_social_all_ac, aes(x=condition, y=AU_activity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_all_stat_ac, label = "p.adj.signif",  y.position = c(1.1)) +
  ylim(0,1.2) + xlab("") + ylab("") +
  ggtitle("A - All Valences") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

obj1_alone_vs_social_fear_ac <- all_df %>%
  select(condition, private_id, valence, AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, 
         AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, 
         AU26_c, AU28_c, AU45_c) %>%
  filter(., valence == "fear") %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_activity", AU01_c_1:AU45_c_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_activity = as.numeric(AU_activity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_vs_social_fear_ac)

obj1_alone_vs_social_fear_stat_ac <- obj1_alone_vs_social_fear_ac %>%
  group_by(AU, ncol = 3) %>% 
  wilcox_test(AU_activity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_fear_stat_ac, "output/obj1_alone_vs_social_fear_ac.txt",sep="\t",row.names=F)

Fig_S1b <- ggplot(obj1_alone_vs_social_fear_ac, aes(x=condition, y=AU_activity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_fear_stat_ac, label = "p.adj.signif",  y.position = c(1.1)) +
  ylim(0,1.2) + xlab("") + ylab("") +
  ggtitle("B - Fear") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

obj1_alone_vs_social_neutral_ac <- all_df %>%
  select(condition, private_id, valence, AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, 
         AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, 
         AU26_c, AU28_c, AU45_c) %>%
  filter(., valence == "neutral") %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_activity", AU01_c_1:AU45_c_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_activity = as.numeric(AU_activity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_vs_social_neutral_ac)

obj1_alone_vs_social_neutral_stat_ac <- obj1_alone_vs_social_neutral_ac %>%
  group_by(AU) %>% 
  wilcox_test(AU_activity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_neutral_stat_ac, "output/obj1_alone_vs_social_neutral_ac.txt",sep="\t",row.names=F)

Fig_S1c <- ggplot(obj1_alone_vs_social_neutral_ac, aes(x=condition, y=AU_activity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_neutral_stat_ac, label = "p.adj.signif",  y.position = c(1.1)) +
  ylim(0,1.2) + xlab("") + ylab("") +
  ggtitle("C - Neutral") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

obj1_alone_vs_social_amusement_ac <- all_df %>%
  select(condition,  private_id, valence, AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, 
         AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, 
         AU26_c, AU28_c, AU45_c) %>%
  filter(., valence == "amusement") %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_activity", AU01_c_1:AU45_c_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_activity = as.numeric(AU_activity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_vs_social_amusement_ac)

obj1_alone_vs_social_amusement_stat_ac <- obj1_alone_vs_social_amusement_ac %>%
  group_by(AU) %>% 
  wilcox_test(AU_activity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_amusement_stat_ac, "output/obj1_alone_vs_social_amusement_ac.txt",sep="\t",row.names=F)

Fig_S1d <- ggplot(obj1_alone_vs_social_amusement_ac, aes(x=condition, y=AU_activity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_amusement_stat_ac, label = "p.adj.signif",  y.position = c(1.1)) +
  ylim(0,1.2) + xlab("") + ylab("") +
  ggtitle("D - Amusement") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

Fig_S1 <- ggarrange(Fig_S1a, Fig_S1b, Fig_S1c, Fig_S1d, ncol=2, nrow = 2, legend = NULL, common.legend = FALSE, legend.grob = NULL)
Fig_S1 <- annotate_figure(Fig_S1, left = textGrob("AU Activity", rot = 90, gp = gpar(cex = 1)),
                             bottom = textGrob("Condition", gp = gpar(cex = 1)))

png("output/Fig_S1.png", width=12*ppi, height=12*ppi, res=ppi)
plot(Fig_S1)
dev.off()

#Panel Figure S2 (AU intensity)
obj1_alone_vs_social_all <- all_df %>%
  select(condition, private_id, valence, AU01, AU02, AU04, AU05, AU06, AU07, 
         AU09, AU10, AU12, AU14, AU15, AU17, AU20, AU23, AU25, 
         AU26, AU45) %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_intensity", AU01_1:AU45_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_intensity = as.numeric(AU_intensity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_stat_df)

obj1_alone_vs_social_all_stat <- obj1_alone_vs_social_all %>%
  group_by(AU) %>% 
  wilcox_test(AU_intensity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_all_stat, "output/obj1_alone_vs_social_all.txt",sep="\t",row.names=F)

Fig_S2a <- ggplot(obj1_alone_vs_social_all, aes(x=condition, y=AU_intensity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_all_stat, label = "p.adj.signif",  y.position = c(4)) +
  ylim(0,5) + xlab("") + ylab("") +
  ggtitle("A - All Valences") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

obj1_alone_vs_social_fear <- all_df %>%
  select(condition, private_id, valence, AU01, AU02, AU04, AU05, AU06, AU07, 
         AU09, AU10, AU12, AU14, AU15, AU17, AU20, AU23, AU25, 
         AU26, AU45) %>%
  filter(., valence == "fear") %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_intensity", AU01_1:AU45_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_intensity = as.numeric(AU_intensity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_vs_social_fear)

obj1_alone_vs_social_fear_stat <- obj1_alone_vs_social_fear %>%
  group_by(AU, ncol = 3) %>% 
  wilcox_test(AU_intensity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_fear_stat, "output/obj1_alone_vs_social_fear.txt",sep="\t",row.names=F)

Fig_S2b <- ggplot(obj1_alone_vs_social_fear, aes(x=condition, y=AU_intensity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_fear_stat, label = "p.adj.signif",  y.position = c(4)) +
  ylim(0,5) + xlab("") + ylab("") +
  ggtitle("B - Fear") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

obj1_alone_vs_social_neutral <- all_df %>%
  select(condition, private_id, valence, AU01, AU02, AU04, AU05, AU06, AU07, 
         AU09, AU10, AU12, AU14, AU15, AU17, AU20, AU23, AU25, 
         AU26, AU45) %>%
  filter(., valence == "neutral") %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_intensity", AU01_1:AU45_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_intensity = as.numeric(AU_intensity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_vs_social_neutral)

obj1_alone_vs_social_neutral_stat <- obj1_alone_vs_social_neutral %>%
  group_by(AU) %>% 
  wilcox_test(AU_intensity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_neutral_stat, "output/obj1_alone_vs_social_neutral.txt",sep="\t",row.names=F)

Fig_S2c <- ggplot(obj1_alone_vs_social_neutral, aes(x=condition, y=AU_intensity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_neutral_stat, label = "p.adj.signif",  y.position = c(4)) +
  ylim(0,5) + xlab("") + ylab("") +
  ggtitle("C - Neutral") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

obj1_alone_vs_social_amusement <- all_df %>%
  select(condition, private_id, valence, AU01, AU02, AU04, AU05, AU06, AU07, 
         AU09, AU10, AU12, AU14, AU15, AU17, AU20, AU23, AU25, 
         AU26, AU45) %>%
  filter(., valence == "amusement") %>%
  group_by(condition, private_id, valence) %>%
  summarise(across(everything(), list(mean))) %>%
  gather(key = "AU", value = "AU_intensity", AU01_1:AU45_1) %>%
  lapply(., function(x) {gsub("_1", "", x)}) %>% as.data.frame() %>% # remove "_1" added by previous group_by / summary step
  lapply(., function(x) {gsub("_c", "", x)}) %>% as.data.frame() %>% # remove "_c"
  convert_as_factor(private_id, valence, AU) %>% #fix
  mutate(., AU_intensity = as.numeric(AU_intensity)) %>% # fix
  mutate(., valence = factor(valence, levels = c("amusement","neutral","fear"))) # refactor so neutral is middle plot
summary(obj1_alone_vs_social_amusement)

obj1_alone_vs_social_amusement_stat <- obj1_alone_vs_social_amusement %>%
  group_by(AU) %>% 
  wilcox_test(AU_intensity ~ condition, paired = T, detailed = TRUE) %>%
  adjust_pvalue(p.col = "p", output.col = "p.adj", method = "BH") %>%
  add_significance()
write.table(obj1_alone_vs_social_amusement_stat, "output/obj1_alone_vs_social_amusement.txt",sep="\t",row.names=F)

Fig_S2d <- ggplot(obj1_alone_vs_social_amusement, aes(x=condition, y=AU_intensity)) +
  facet_wrap(~AU, ncol = 3) +
  geom_boxplot(outlier.size = 0.33) +
  geom_jitter(position = position_jitter(width = 0.2), size = 0.33, colour = 'black') +
  stat_pvalue_manual(obj1_alone_vs_social_amusement_stat, label = "p.adj.signif",  y.position = c(4)) +
  ylim(0,5) + xlab("") + ylab("") +
  ggtitle("D - Amusement") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())

Fig_S2 <- ggarrange(Fig_S2a, Fig_S2b, Fig_S2c, Fig_S2d, ncol=2, nrow = 2, legend = NULL, common.legend = FALSE, legend.grob = NULL)
Fig_S2 <- annotate_figure(Fig_S2, left = textGrob("AU Intensity", rot = 90, gp = gpar(cex = 1)),
                            bottom = textGrob("Condition", gp = gpar(cex = 1)))

png("output/Fig_S2.png", width=12*ppi, height=12*ppi, res=ppi)
plot(Fig_S2)
dev.off()

### CREATE SUMMARY TABLES FOR PY-FEAT ANALYSES (Heatmap vizalizations) ###
# THESE AUs NOT OUTPUT BY OPENFACE - must add columns of 0s for visualization in Py-Feat
AU18 <- replicate(960, 0)
AU24 <- replicate(960, 0)
AU43 <- replicate(960, 0)

pyfeat_df <- all_df %>%
  select(condition, private_id, valence, stimulus, AU01, AU02, AU04, AU05, AU06, AU07, AU09, AU10, AU12, AU14, AU15, AU17,
         AU20, AU23, AU25, AU26, AU28_c) %>%
  rename(., AU28 = AU28_c) %>%
  unite(ID, c(condition, private_id, valence, stimulus), sep = "_", remove = FALSE) %>% # create filename for PNG outputs 
  add_column(., AU18, .after = "AU17") %>%
  add_column(., AU24, .after = "AU23") %>%
  add_column(., AU43, .after = "AU28")

pyfeat_input_ALL <- pyfeat_df %>%
  select(-c(condition, private_id, valence, stimulus)) %>% #cleanup
  mutate_if(is.numeric, round, 6)
write.table(pyfeat_input_ALL, "output/pyfeat_input_ALL.csv", sep=",", row.names=F, quote = F)

pyfeat_df_AVG_BYVALENCE <- pyfeat_df %>% 
  select(-c(ID, private_id, stimulus)) %>% # drop unused cols 
  group_by(condition,valence) %>%
  summarise(across(everything(), list(mean), .names = "{.col}")) %>%
  unite(ID, c(condition, valence), sep = "_", remove = TRUE) %>% # create filename for PNG outputs 
  mutate_if(is.numeric, round, 6)
write.table(pyfeat_df_AVG_BYVALENCE, "output/pyfeat_input_BYVALENCE.csv", sep=",", row.names=F, quote = F)

pyfeat_df_AVG_BYSTIMULUS <- pyfeat_df %>% 
  select(-c(ID, private_id)) %>% # drop unused cols 
  group_by(condition,valence, stimulus) %>%
  summarise(across(everything(), list(mean), .names = "{.col}")) %>%
  unite(ID, c(condition, valence, stimulus), sep = "_", remove = TRUE) %>% # create filename for PNG outputs 
  mutate_if(is.numeric, round, 6)
write.table(pyfeat_df_AVG_BYSTIMULUS, "output/pyfeat_input_BYSTIMULUS.csv", sep=",", row.names=F, quote = F)
  
############################################################################
## IDENTIFY DRIVERS OF VARIATION IN AU INTENSITY / ACTIVITY ##
############################################################################
#install.packages("pacman")
#devtools::install_github("mvuorre/brmstools")
#pacman::p_load(sjmisc, sjstats, ggmcmc, sjPlot, brms, shinystan, bayesplot,
               #brmstools, loo, rstan, grid, patchwork)
model_df <- all_df %>% 
  rowwise()  %>%
  mutate(AU_intensity = mean(c(AU04:AU45))+.0001) %>% # recalculate
  mutate(AU_activity = mean(c(AU28_c:AU45_c))+.0001) %>% # recalculate
  mutate(., valence = factor(valence, levels = c("neutral","amusement","fear"))) 
str(model_df)

# Calculate IQR and bounds for AU_intensity
q1_int <- quantile(model_df$AU_intensity, 0.25)
q3_int <- quantile(model_df$AU_intensity, 0.75)
iqr_int <- q3_int - q1_int
lower_bound_int <- q1_int - 1.5 * iqr_int
upper_bound_int <- q3_int + 1.5 * iqr_int

# Calculate IQR and bounds for AU_activity
q1_act <- quantile(model_df$AU_activity, 0.25)
q3_act <- quantile(model_df$AU_activity, 0.75)
iqr_act <- q3_act - q1_act
lower_bound_act <- q1_act - 1.5 * iqr_act
upper_bound_act <- q3_act + 1.5 * iqr_act

# Filter out rows with outliers for both AU_intensity and AU_activity
model_df_filtered_int <- model_df %>%
  filter(!(AU_intensity < lower_bound_int | AU_intensity > upper_bound_int))

model_df_filtered_act <- model_df %>%
  filter(!(AU_activity < lower_bound_act | AU_activity > upper_bound_act))

# Calculate mean and standard deviation for AU_intensity and AU_activity
# Calculate Z-scores for AU_intensity and AU_activity
# Filter out rows with Z-scores >2 for both AU_intensity and AU_activity
mean_intensity <- mean(model_df$AU_intensity)
sd_intensity <- sd(model_df$AU_intensity)

mean_activity <- mean(model_df$AU_activity)
sd_activity <- sd(model_df$AU_activity)

model_df <- model_df %>%
  mutate(z_intensity = (AU_intensity - mean_intensity) / sd_intensity)

model_df_filtered_int <- model_df %>%
  filter(!abs(z_intensity) > 2) %>%
  select(-z_intensity)  # Remove the temporary Z-score column

model_df <- model_df %>%
  select(-z_intensity) %>% # Remove the temporary Z-score column
  mutate(z_activity = (AU_activity - mean_activity) / sd_activity)

model_df_filtered_act <- model_df %>%
  filter(!abs(z_activity) > 2) %>%
  select(-z_activity)  # Remove the temporary Z-score column

## Model 1: AU activity
model1 <- brm(formula = AU_activity ~  condition * valence  + gender + ethnicity + 
                recognize_vid_binary + (1|private_id) + (1|stimulus), data=model_df, 
              family="zero_one_inflated_beta", warmup= 2000, iter = 10000, chains = 4, 
              control = list(adapt_delta = 0.99), file = "output/model1")
summary(model1)
prior_summary(model1)
hist(model_df$AU_activity)
pp_check(model1, type = "stat", stat = 'mean', ndraws = 1000)
traceplot_model1 <-plot(model1)#did chains behave well?

#### Model 1_outliers: AU activity (excluding z score outliers -- see if results remain stable - same direction)
model1_outliers <- brm(formula = AU_activity ~  condition * valence  + gender + ethnicity + 
                recognize_vid_binary + (1|private_id) + (1|stimulus), data=model_df_filtered_act, 
              family="zero_one_inflated_beta", warmup= 2000, iter = 10000, chains = 4, 
              control = list(adapt_delta = 0.99), file = "output/model1_outliers")
summary(model1_outliers)
prior_summary(model1_outliers)
hist(model_df_filtered_act$AU_activity)
pp_check(model1_outliers, type = "stat", stat = 'mean', ndraws = 1000)
traceplot_model1 <-plot(model1_outliers)#did chains behave well?

#get posterior distributions
posterior <- as.matrix(model1)

#computation of pd
result_pd<-p_direction(model1, method = "kernel")# Probability of direction 
print(result_pd)

#apply colorscheme:
color_scheme_set("blue")
Figmodel1 <- mcmc_intervals(posterior,
                    pars=c("b_conditionsocial", "b_valenceamusement", 
                           "b_valencefear","b_conditionsocial:valenceamusement","b_conditionsocial:valencefear"),
                    prob=0.8,
                    prob_outer=0.95,
                    point_est = "mean")
Figure1_posterior <- Figmodel1 +scale_x_continuous(limits=c(-1,2),breaks=scales::pretty_breaks(n=8))+
  labs(x="Parameter estimate", y= " ", title=" ")+
  theme(axis.text.y=element_blank())+ theme_classic(base_size = 16, base_family = "sans")+
  scale_y_discrete(labels=c("b_conditionsocial"="alone vs. social condition", "b_valenceamusement"= "neutral vs. amusement", 
                            "b_valencefear"= "neutral vs. fear",
                            "b_conditionsocial:valenceamusement" = "condition x valence type (neutral vs amusement)", 
                            "b_conditionsocial:valencefear" = "condition x valence type (neutral vs fear)"))+
  theme(panel.grid = element_blank())+theme(axis.text.y=element_text(size=16,hjust=1),axis.text.x=element_text(size=12))

# diagnostics
# get the plots generated by conditional_effects()
p1 = plot(conditional_effects(model1), plot=FALSE)

## Model 2: AU intensity
model2 <- brm(formula = AU_intensity ~ condition * valence + gender + ethnicity + 
                recognize_vid_binary + (1|private_id) + (1|stimulus), data=model_df, 
              family=weibull(), warmup= 2000, iter = 10000, chains = 4, 
              control = list(adapt_delta = 0.99), file = "output/model2")
hist(model_df$AU_intensity)
range(model_df$AU_intensity)
mean(model_df$AU_intensity)
summary(model2)#warnings about max tree depth are unproblematic, see: https://mc-stan.org/misc/warnings.html
prior_summary(model2)
plot(model2)
pp_check(model2, ndraws = 1000)
pp_check(model2, type = "scatter_avg", ndraws = 1000)


#### Model 2_outliers: AU intensity (excluding z score outliers -- see if results remain stable - same direction)
model2_outliers <- brm(formula = AU_intensity ~ condition * valence + gender + ethnicity + 
                recognize_vid_binary + (1|private_id) + (1|stimulus), data=model_df_filtered_int, 
              family=weibull(), warmup= 2000, iter = 10000, chains = 4, 
              control = list(adapt_delta = 0.99), file = "output/model2_outliers")
summary(model2_outliers)#warnings about max tree depth are unproblematic, see: https://mc-stan.org/misc/warnings.html

#get posterior distributions
posterior <- as.matrix(model2)

#computation of pd
result_pd<-p_direction(model2, method = "kernel")# Probability of direction 
print(result_pd)

#Figure
Figmodel2 <- mcmc_intervals(posterior,
                            pars=c("b_conditionsocial", "b_valenceamusement", 
                                   "b_valencefear","b_conditionsocial:valenceamusement","b_conditionsocial:valencefear"),
                            prob=0.8,
                            prob_outer=0.95,
                            point_est = "mean")
Figure2_posterior <- Figmodel2 +scale_x_continuous(limits=c(-1,2),breaks=scales::pretty_breaks(n=8))+
  labs(x="Parameter estimate", y= " ", title="a")+
  theme(axis.text.y=element_blank())+ theme_classic(base_size = 16, base_family = "sans")+
  scale_y_discrete(labels=c("b_conditionsocial"="alone vs. social condition", "b_valenceamusement"= "neutral vs. amusement", 
                            "b_valencefear"= expression(~bold("neutral vs. fear")),
                            "b_conditionsocial:valenceamusement" = "condition x valence type (neutral vs amusement)", 
                            "b_conditionsocial:valencefear" = "condition x valence type (neutral vs fear)"))+
  theme(panel.grid = element_blank())+theme(axis.text.y=element_text(size=16,hjust=1),axis.text.x=element_text(size=12))

## Model 3: Gesture 
model3 <- brm(formula = face_touching ~  condition * valence + gender + ethnicity + 
                recognize_vid_binary + (1|private_id) + (1|stimulus), data=model_df, 
              family=bernoulli(), warmup= 2000, iter = 10000, chains = 4, 
              control = list(adapt_delta = 0.99), file = "output/model3")
summary(model3)
prior_summary(model3)
plot(model3)
pp_check(model3, ndraws = 1000)


# get the plots generated by conditional_effects()
p3 = plot(conditional_effects(model3), plot=FALSE)

#get posterior distributions
posterior <- as.matrix(model3)

#computation of pd
result_pd<-p_direction(model3, method = "kernel")# Probability of direction 
print(result_pd)

#Figure
Figmodel3 <- mcmc_intervals(posterior,
                            pars=c("b_conditionsocial", "b_valenceamusement", 
                                   "b_valencefear","b_conditionsocial:valenceamusement","b_conditionsocial:valencefear"),
                            prob=0.8,
                            prob_outer=0.95,
                            point_est = "mean")
Figure3_posterior <- Figmodel3 +scale_x_continuous(limits=c(-6,6),breaks=scales::pretty_breaks(n=8))+
  labs(x="Parameter estimate", y= " ", title="b")+
  theme(axis.text.y=element_blank())+ theme_classic(base_size = 16, base_family = "sans")+
  scale_y_discrete(labels=c("b_conditionsocial"=expression(~bold("alone vs. social condition")), "b_valenceamusement"= expression(~bold("neutral vs. amusement")), 
                            "b_valencefear"= expression(~bold("neutral vs. fear")),
                            "b_conditionsocial:valenceamusement" = "condition x valence type (neutral vs amusement)", 
                            "b_conditionsocial:valencefear" = "condition x valence type (neutral vs fear)"))+
  theme(panel.grid = element_blank())+theme(axis.text.y=element_text(size=16,hjust=1),axis.text.x=element_text(size=12))


library(gridExtra)
Figure1_posterior #corresponds to Figure S3
Figure2 <- grid.arrange(Figure2_posterior,Figure3_posterior)

png("output/Figure2.png", width=9*ppi, height=8*ppi, res=ppi)
plot(Figure2)
dev.off()

png("output/FigureS3.png", width=10*ppi, height=5*ppi, res=ppi)
plot(Figure1_posterior)
dev.off()

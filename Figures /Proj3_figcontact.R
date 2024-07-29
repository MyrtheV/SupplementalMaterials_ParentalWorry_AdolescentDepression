##########################################################################################
# Figure 3 
##########################################################################################
# Contact figure 

##########################################################################################
# Load data - not openly available 
##########################################################################################
proj3_evalcontact_datacomb_f 

##########################################################################################
# Load packages 
##########################################################################################
library(ggplot2)
library(ggthemes) 
library(ggpattern)

##########################################################################################
# Figure 
#####################################################################################################
# Control 
proj3_contactbarcontrol_perc_2 <- ggplot(proj3_evalcontact_datacomb_f[proj3_evalcontact_datacomb_f$depr == 0,], 
                                       aes(x = Person2, 
                                           fill = factor(Evaluation, levels = c("7", "6", "5", "4", "3", "2", "1")), ordered = TRUE)) + 
  geom_bar_pattern(aes(y = (..count..)/sum(..count..), pattern = factor(Evaluation, levels = c("7", "6", "5", "4", "3", "2", "1")), pattern_angle = as.factor(Evaluation), pattern_spacing = as.factor(Evaluation)), 
                   stat = "count", alpha = 0.1, pattern_color = NA,
                   pattern_density = 0.1,
                   pattern_key_scale_factor = 0.1) + 
  geom_bar(stat = "count", alpha = 0.8, aes(y = (..count..)/sum(..count..))) +  
  ylab("") + 
  xlab("") + 
  theme_classic() + 
  ggtitle("A. Control") + 
  theme(panel.grid.major = element_line(color = "#ececec")) + 
  scale_fill_manual("Evaluation", breaks = c(seq(7,1)), 
                    labels = c("7: Very enjoyable", "6", "5", "4", "3", "2", "1: Not enjoyable at all"), 
                    values = colorset_newtheme_bluegreyorange3) + 
  theme(legend.position = "None", 
        axis.text = element_text(size = 11)) + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))

# Depression sample 
proj3_contactbardepr_perc_2 <- ggplot(proj3_evalcontact_datacomb_f[proj3_evalcontact_datacomb_f$depr == 1,], 
                                    aes(x = Person2, 
                                        fill = factor(Evaluation, levels = c("7", "6", "5", "4", "3", "2", "1")), ordered = TRUE)) + 
  geom_bar_pattern(aes(y = (..count..)/sum(..count..), pattern = factor(Evaluation, levels = c("7", "6", "5", "4", "3", "2", "1")), pattern_angle = as.factor(Evaluation), pattern_spacing = as.factor(Evaluation)), 
                   alpha = 0.1, pattern_color = NA,
                   pattern_density = 0.1,
                   pattern_key_scale_factor = 0.1) + 
  geom_bar(alpha = 0.8, aes(y = (..count..)/sum(..count..))) + 
  ylab("") + 
  xlab("") + 
  theme_classic() + 
  ggtitle("B. Depression") + 
  theme(panel.grid.major = element_line(color = "#ececec"), 
        axis.text = element_text(size = 11)) + 
  scale_fill_manual("Evaluation", breaks = c(seq(7,1)), 
                    labels = c("7: Very enjoyable", "6", "5", "4", "3", "2", "1: Not enjoyable at all"), 
                    values = colorset_newtheme_bluegreyorange3)  + 
  guides(pattern = "none", pattern_angle = "none", pattern_spacing = "none") + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))

# Save figures 
proj3_contactbar_perc_2_fig <- arrangeGrob(proj3_contactbarcontrol_perc_2, 
                                         proj3_contactbardepr_perc_2, nrow = 1, ncol = 2, widths = c(1, 1.3))
ggsave(file = "proj3_contactbar_perc_fig_v4.png", proj3_contactbar_perc_2_fig, width = 13, height = 7)





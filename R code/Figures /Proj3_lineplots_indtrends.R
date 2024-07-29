################################################################################################
# Figures 5 and E1 of Appendix E 
################################################################################################
# Figure individual trends 

################################################################################################
# Load data 
################################################################################################
# proj3_mooda_comb2

################################################################################################
# Load packages 
################################################################################################
library(tidyverse)
library(gridExtra)

################################################################################################
# Figure 5 
################################################################################################
sadlistenfighc_v2 <- ggplot(proj3_mooda_comb2[proj3_mooda_comb2$depr ==0,] , aes(y = SAD, x = LISTENC_p)) + 
  #geom_line(aes(group = as.factor(PPN)), color = "grey") + 
  theme(legend.position="none") + 
  geom_line(aes(group = as.factor(PPN)), stat = "smooth", method  = "lm", se = FALSE, color = "lightgrey", linewidth = 0.8, alpha = 0.9, linetype = 2) + 
  geom_smooth(method  = "lm", se = FALSE, color = "black", size = 0.5) + 
  theme_classic() + 
  ylab("Adolescent sadness") + 
  xlab("Parental listening") + 
  scale_x_continuous(breaks = round(seq(min(1), max(7), by = 1),1)) + 
  scale_y_continuous(breaks = round(seq(min(1), max(7), by = 1),1), limits = c(1,7)) + 
  ggtitle("A. Control") + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        title = element_text(size = 14)) 

sadlistenfigdepr_v2 <- ggplot(proj3_mooda_comb2[proj3_mooda_comb2$depr == 1,] , aes(y = SAD, x = LISTENC_p)) + 
  #geom_line(aes(group = as.factor(PPN)), color = "grey") + 
  theme(legend.position="none") + 
  geom_line(aes(group = as.factor(PPN)), stat = "smooth", method  = "lm", se = FALSE, color = "grey", linewidth = 0.8, alpha = 0.9, linetype = 2) + 
  geom_smooth(method  = "lm", se = FALSE, color = "black", size = 0.5, alpha = 0.5) + 
  theme_classic() + 
  ylab("") + 
  xlab("Parental listening") + 
  scale_x_continuous(breaks = round(seq(min(1), max(7), by = 1),1)) + 
  scale_y_continuous(breaks = round(seq(min(1), max(7), by = 1),1), limits = c(1,7)) + 
  ggtitle("B. Depression") + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        title = element_text(size = 14)) 



# Plot together 
proj3_indtrends_v2 <- arrangeGrob(sadlistenfighc_v2, sadlistenfigdepr_v2, nrow = 1, ncol = 2, widths = c(1, 1))
ggsave(file = "proj3_indtrends_v2.png", proj3_indtrends_v2, width = 14, height = 6)


################################################################################################  
# Figure E1 
################################################################################################  
dataforindtrendfigapp <- proj3_mooda_comb2[, c("PPN", "Day2", "Family", "depr", "HAPPY", "SAD", "IRRI", "RELAX", 
                      "LISTENC_p", "UNDERC_p", "CRITICC_p", "DOMINC_p")]

dataforindtrendfigapplong <- dataforindtrendfigapp %>% 
  pivot_longer(-c(Family, PPN, Day2, depr), names_to = "variables", values_to = "value")

dataforindtrendfigapplong$group <- rep(c(rep("Affect", 4), rep("Parenting behavior", 4)), nrow(dataforindtrendfigapplong)/8)
dataforindtrendfigapplong$variables2 <- dataforindtrendfigapplong$variables
dataforindtrendfigapplong$value2 <- dataforindtrendfigapplong$value

dataforindtrendfigapp_long2 <- dataforindtrendfigapp %>% 
  pivot_longer(-c(Family, PPN, Day2, depr, HAPPY, SAD, RELAX, IRRI), names_to = "variables", values_to = "value")

happyindtrendfigapp_v2 <- ggplot(dataforindtrendfigapp_long2, aes(x = HAPPY, y = value)) + 
  facet_grid(variables ~ depr, labeller = labeller(depr = c(`0` = "Control", `1` = "Depression"), 
                                                   variables = c("CRITICC_p" = "Criticism", "DOMINC_p" = "Dominance", "LISTENC_p" = "Listening", "UNDERC_p" = "Understanding"))) + 
  #geom_line(aes(group = as.factor(PPN)), color = "grey") + 
  theme(legend.position="none") + 
  geom_line(aes(group = as.factor(PPN)), stat = "smooth", method  = "lm", se = FALSE, color = "lightgrey", linewidth = 0.8, alpha = 0.9, linetype = 2)+ 
  geom_smooth(aes(color = as.factor(depr)), method  = "lm", se = FALSE, size = 0.5, color = "black") + 
  # theme_classic() + 
  ylab("Value") + 
  xlab("") + 
  scale_x_continuous(breaks = round(seq(min(1), max(7), by = 1),1)) + 
  scale_y_continuous(breaks = round(seq(min(1), max(7), by = 1),1), limits = c(1,7)) + 
  ggtitle("A. Happy") + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        title = element_text(size = 14), 
        strip.text.y = element_blank()) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF")))

sadindtrendfigapp_v2 <- ggplot(dataforindtrendfigapp_long2, aes(x = SAD, y = value)) + 
  facet_grid(variables ~ depr, labeller = labeller(depr = c(`0` = "Control", `1` = "Depression"), 
                                                   variables = c("CRITICC_p" = "Criticism", "DOMINC_p" = "Dominance", "LISTENC_p" = "Listening", "UNDERC_p" = "Understanding"))) + 
  #geom_line(aes(group = as.factor(PPN)), color = "grey") + 
  theme(legend.position="none") + 
  geom_line(aes(group = as.factor(PPN)), stat = "smooth", method  = "lm", se = FALSE, color = "lightgrey", linewidth = 0.8, alpha = 0.9, linetype = 2)+ 
  geom_smooth(aes(color = as.factor(depr)), method  = "lm", se = FALSE, size = 0.5, color = "black") + 
  # theme_classic() + 
  ylab("Value") + 
  xlab("Value") + 
  scale_x_continuous(breaks = round(seq(min(1), max(7), by = 1),1)) + 
  scale_y_continuous(breaks = round(seq(min(1), max(7), by = 1),1), limits = c(1,7)) + 
  ggtitle("C. Sad") + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        title = element_text(size = 14), 
        strip.text.y = element_blank()) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF")))

relaxindtrendfigapp_v2 <- ggplot(dataforindtrendfigapp_long2, aes(x = RELAX, y = value)) + 
  facet_grid(variables ~ depr, labeller = labeller(depr = c(`0` = "Control", `1` = "Depression"), 
                                                   variables = c("CRITICC_p" = "Criticism", "DOMINC_p" = "Dominance", "LISTENC_p" = "Listening", "UNDERC_p" = "Understanding"))) + 
  #geom_line(aes(group = as.factor(PPN)), color = "grey") + 
  theme(legend.position="none") + 
  geom_line(aes(group = as.factor(PPN)), stat = "smooth", method  = "lm", se = FALSE, color = "lightgrey", linewidth = 0.8, alpha = 0.9, linetype = 2)+ 
  geom_smooth(aes(color = as.factor(depr)), method  = "lm", se = FALSE, size = 0.5, color = "black") + 
  # theme_classic() + 
  ylab("") + 
  xlab("") + 
  scale_x_continuous(breaks = round(seq(min(1), max(7), by = 1),1)) + 
  scale_y_continuous(breaks = round(seq(min(1), max(7), by = 1),1), limits = c(1,7)) + 
  ggtitle("B. Relaxed") + 
  theme(axis.text.x = element_text(size = 12), 
        # axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        title = element_text(size = 14), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF")))

irriindtrendfigapp_v2 <- ggplot(dataforindtrendfigapp_long2, aes(x = IRRI, y = value)) + 
  facet_grid(variables ~ depr, labeller = labeller(depr = c(`0` = "Control", `1` = "Depression"), 
                                                   variables = c("CRITICC_p" = "Criticism", "DOMINC_p" = "Dominance", "LISTENC_p" = "Listening", "UNDERC_p" = "Understanding"))) + 
  #geom_line(aes(group = as.factor(PPN)), color = "grey") + 
  theme(legend.position="none") + 
  geom_line(aes(group = as.factor(PPN)), stat = "smooth", method  = "lm", se = FALSE, color = "lightgrey", linewidth = 0.8, alpha = 0.9, linetype = 2)+ 
  geom_smooth(aes(color = as.factor(depr)), method  = "lm", se = FALSE, size = 0.5, color = "black") +  # color = "#8a4d18"
  # theme_classic() + 
  ylab("") + 
  xlab("Value") + 
  scale_x_continuous(breaks = round(seq(min(1), max(7), by = 1),1)) + 
  scale_y_continuous(breaks = round(seq(min(1), max(7), by = 1),1), limits = c(1,7)) + 
  ggtitle("D. Irritated") + 
  theme(axis.text.x = element_text(size = 12), 
        #axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), 
        title = element_text(size = 14), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF")))

# Plot together 
# Save figures 
proj3_indtrendfig_app_v2 <- arrangeGrob(happyindtrendfigapp_v2, 
                                     relaxindtrendfigapp_v2, 
                                     sadindtrendfigapp_v2, 
                                     irriindtrendfigapp_v2, nrow = 2, ncol = 2, widths = c(1, 1))

ggsave(file = "proj3_indtrendfig_app_v2.png", proj3_indtrendfig_app_v2, width = 13, height = 13)





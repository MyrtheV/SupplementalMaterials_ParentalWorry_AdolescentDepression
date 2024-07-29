#######################################################################################
# Figures B2 and C2 of Appendix B and C 
#######################################################################################

#######################################################################################
# Load data - not openly avaible 
#######################################################################################
# For network 1 
proj3_endofday_amf_net_hc_mean
proj3_endofday_amf_net_depr2

# For network 2 
proj3_mooda_comb2_contact_hc 
proj3_mooda_comb2_contact_depr

#######################################################################################
# Load packages 
#######################################################################################
library(dplyr)
library(ggplot2)

#######################################################################################
# Network 1 
#######################################################################################
# Prepare data for figure 
# Control 
proj3_endofday_amf_net_hc_mean_long <- proj3_endofday_amf_net_hc_mean %>% 
  pivot_longer(everything(), names_to = "variables", values_to = "value")

proj3_endofday_amf_net_hc_mean_long$group1 <- rep("Control", nrow(proj3_endofday_amf_net_hc_mean_long))
proj3_endofday_amf_net_hc_mean_long$group2 <- rep(c(rep("Adolescents", 6), rep("Parents", 4)), nrow(proj3_endofday_amf_net_hc_mean_long)/10)



# Depression 
proj3_endofday_amf_net_depr2_long <- proj3_endofday_amf_net_depr2 %>% 
  pivot_longer(everything(), names_to = "variables", values_to = "value")

proj3_endofday_amf_net_depr2_long$group1 <- rep("Depression", nrow(proj3_endofday_amf_net_depr2_long))
proj3_endofday_amf_net_depr2_long$group2 <- rep(c(rep("Adolescents", 6), rep("Parents", 4)), nrow(proj3_endofday_amf_net_depr2_long)/10)

# Combine data 
proj3_mooda_comb1_contact_comb_long2 <- rbind(proj3_endofday_amf_net_hc_mean_long, proj3_endofday_amf_net_depr2_long)

proj3_mooda_comb1_contact_comb_long2$variablename <- rep(c("A: Happy", "A: Sad", "A: Relaxed", "A: Irritated", 
                                                          "A: Critical", "A: Warm", "P: Critical", "P: Warm",
                                                          "P: Worry", "P: Self-efficacy"), nrow(proj3_mooda_comb1_contact_comb_long2)/10)  # nrow () / nr var (10) 

# Figure 
ggplot(proj3_mooda_comb1_contact_comb_long2, aes(x = value, fill = group2, color = group2)) + 
  geom_histogram(position = "dodge", binwidth = 0.9, alpha=.75) + 
  theme_bw() + 
  scale_fill_manual(values = (c("#E68129", "#9FB1BCFF"))) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF"))) + 
  # scale_color_manual(values = (c("#D3D0CBFF", "#9FB1BCFF"))) + 
  facet_grid(group1~fct_inorder(variablename), scales = "free") + 
  xlab("Response") + 
  ylab("Count") + 
  theme(legend.position = "right", 
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = 1:7) 

#######################################################################################
# Network 2 
#######################################################################################
# For second network 

# Prepare data for figure 
# Control 
proj3_mooda_comb2_contact_hc_long <- proj3_mooda_comb2_contact_hc %>% 
  pivot_longer(everything(), names_to = "variables", values_to = "value")

proj3_mooda_comb2_contact_hc_long$group1 <- rep("Control", nrow(proj3_mooda_comb2_contact_hc_long))
proj3_mooda_comb2_contact_hc_long$group2 <- rep(c(rep("Affect", 4), rep("Parenting behavior", 4)), 34048/8)

# Depr 
proj3_mooda_comb2_contact_depr_long <- proj3_mooda_comb2_contact_depr %>% 
  pivot_longer(everything(), names_to = "variables", values_to = "value")

proj3_mooda_comb2_contact_depr_long$group1 <- rep("Depression", nrow(proj3_mooda_comb2_contact_depr_long))
proj3_mooda_comb2_contact_depr_long$group2 <- rep(c(rep("Affect", 4), rep("Parenting behavior", 4)), nrow(proj3_mooda_comb2_contact_depr_long)/8)

proj3_mooda_comb2_contact_comb_long <- rbind(proj3_mooda_comb2_contact_hc_long, proj3_mooda_comb2_contact_depr_long)

proj3_mooda_comb2_contact_comb_long$variablename <- rep(c("Happy", "Sad", "Irritated", "Relaxed", 
                                                       "Listening", "Understanding", 
                                                       "Critical", "Dominant"), nrow(proj3_mooda_comb2_contact_comb_long)/8)  # nrow () / nr var (8) 

# Figure 
ggplot(proj3_mooda_comb2_contact_comb_long, aes(x = value, fill = group2, color = group2)) + 
  geom_histogram(position = "dodge", binwidth = 0.9, alpha=.75) + 
  theme_bw() + 
  scale_fill_manual(values = (c("#E68129", "#9FB1BCFF"))) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF"))) + 
  # scale_color_manual(values = (c("#E68129", "#9FB1BCFF"))) + 
  facet_grid(group1~fct_inorder(variablename), scales = "free") + 
  xlab("Response") + 
  ylab("Count") + 
  theme(legend.position = "right", 
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = 1:7) 




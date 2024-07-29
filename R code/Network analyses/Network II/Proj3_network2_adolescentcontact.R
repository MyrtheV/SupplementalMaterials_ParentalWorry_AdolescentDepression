####################################################################################
# Network 2 
####################################################################################
# Includes code for network estimation and Figures 3, C3, and C4 of Appendix C 

####################################################################################
# Load packages 
####################################################################################
library(qgraph)
library(bootnet)
library(ggplot2)
library(gridExtra)

####################################################################################
# Load data - not openly available  
####################################################################################
proj3_mooda_comb2 <- read_csv("proj3_endofday_amf_net_hc.csv")

####################################################################################
# Network estimation 
####################################################################################

####################################################################################
# Depr sample 
####################################################################################
proj3_mooda_comb2_contact_depr <- proj3_mooda_comb2[proj3_mooda_comb2$depr == 1, c("HAPPY", "SAD", "IRRI", "RELAX", 
                                                                                   "LISTENC_p", "UNDERC_p", "CRITICC_p", "DOMINC_p")]
# use EBICglasso here as well - use for paper 
proj3_contact_net_depr <- estimateNetwork(proj3_mooda_comb2_contact_depr, "EBICglasso")
saveRDS(proj3_contact_net_depr, "proj3_contact_net_depr24102023.rds")
# proj3_contact_net_depr <- readRDS("proj3_contact_net_depr24102023.rds")
proj3_mooda_comb2_contact_depr

# Stability 
proj3_booted_part2depr <- bootnet(proj3_contact_net_depr, bootstraps = 1000)
proj3_booted_part2depr_res <- plot(proj3_booted_part2depr, labels = FALSE, order = "sample")
saveRDS(proj3_booted_part2depr, "proj3_booted_part2depr24102023.rds")
# proj3_booted_part2depr <- readRDS("proj3_booted_part2depr26092023.rds")

# Plot network 
# From before 
colors_mf2 <- c("#D3D0CBFF", "#9FB1BCFF")

# Specify groups  
Groups_contact_mf2 <- list(Affect = c(1:4), Parenting = c(5:8))  # to get nice legend 


layout_net2 <-  matrix(c(
  -7.071068e-01, 7.071068e-01, 
  -1.000000e+00, -2.220446e-1, 
  -7.071068e-01, -7.071068e-01,
  2.220446e-16, -1.000000e+00, 
  7.071068e-01, -7.071068e-01, 
  1.000000e+00,  0.000000e+00, 
  7.071068e-01,  7.071068e-01, 
  0.000000e+00,  1.000000e+00), 8, 2, byrow = TRUE)

# depr 
Proj3_net_a_contact_depr <- plot(proj3_contact_net_depr , 
                                 labels = c("Happy", "Sad", "Irritated",  "Relaxed", 
                                            "Listening", "Understanding", "Critical", "Dominant"),
                                 edge.labels = FALSE,
                                 edge.label.cex = 1,
                                 groups = Groups_contact_mf2, 
                                 layout = layout_net2,
                                 theme = "gray",
                                 color = c("#E68129", "#9FB1BCFF"),  # previous color = colors_mf2 
                                 negDashed = TRUE,
                                 negCol = "#8a4d18",
                                 posCol = "black",
                                 edge.width = 0.75, 
                                 legend.cex = 0.8,
                                 vsize = 12,
                                 label.cex = 3,
                                 esize = 37,
                                 cut = 0, 
                                 mar = c(4,4,4,0), 
                                 asize = 5, 
                                 legend = TRUE, 
                                 title = "B. Depression", 
                                 title.cex = 2, 
                                 # legend.cex = 5,
                                 # GLratio = 1, 
                                 # layoutScale = c(2,2),
                                 maximum = 0.7)

Proj3_net_a_contact_depr_wl <- plot(proj3_contact_net_depr , 
                                 labels = c("Happy", "Sad", "Irritated",  "Relaxed", 
                                            "Listening", "Understanding", "Critical", "Dominant"),
                                 edge.labels = FALSE,
                                 edge.label.cex = 1,
                                 groups = Groups_contact_mf2, 
                                 layout = layout_net2,
                                 theme = "gray",
                                 color = c("#E68129", "#9FB1BCFF"),  # previous color = colors_mf2 
                                 negDashed = TRUE,
                                 negCol = "#8a4d18",
                                 posCol = "black",
                                 edge.width = 0.75, 
                                 legend.cex = 0.8,
                                 vsize = 12,
                                 label.cex = 3,
                                 esize = 37,
                                 cut = 0, 
                                 mar = c(4,4,4,4), 
                                 asize = 5, 
                                 legend = FALSE, 
                                 title = "B. Depression", 
                                 title.cex = 2, 
                                 # legend.cex = 5,
                                 # GLratio = 1, 
                                 # layoutScale = c(2,2),
                                 maximum = 0.7)

Proj3_net_a_contact_depr_ol <- plot(proj3_contact_net_depr , 
                                 labels = c("Happy", "Sad", "Irritated",  "Relaxed", 
                                            "Listening", "Understanding", "Critical", "Dominant"),
                                 edge.labels = FALSE,
                                 edge.label.cex = 1,
                                 groups = Groups_contact_mf2, 
                                 layout = layout_net2,
                                 theme = "gray",
                                 color = c("#E68129", "#9FB1BCFF"),  # previous color = colors_mf2 
                                 negDashed = TRUE,
                                 negCol = "#8a4d18",
                                 posCol = "black",
                                 edge.width = 0.75, 
                                 #legend.cex = 0.8,
                                 vsize = 12,
                                 label.cex = 3,
                                 esize = 37,
                                 cut = 0, 
                                 mar = c(6,0,0,7), 
                                 asize = 5, 
                                legend = TRUE, 
                                 title = "", 
                                 title.cex = 2, 
                                 legend.cex = 1,
                                 #GLratio = 0.00000000000000000000001, 
                                layoutOffset = c(-3, 0),
                                 # legend = FALSE,
                                 maximum = 0.7)

#####################################################################################################
# Healthy controls 
#####################################################################################################

proj3_mooda_comb2_contact_hc <- proj3_mooda_comb2[proj3_mooda_comb2$depr == 0, c("HAPPY", "SAD", "IRRI", "RELAX", 
                                                                                 "LISTENC_p", "UNDERC_p", "CRITICC_p", "DOMINC_p")]

# use EBICglasso here as well - use for paper 
proj3_contact_net_hc <- estimateNetwork(proj3_mooda_comb2_contact_hc, "EBICglasso")
saveRDS(proj3_contact_net_hc, "proj3_contact_net_hc24102023.rds")
proj3_contact_net_hc$graph
# proj3_contact_net_hc <- readRDS("proj3_contact_net_hc24102023.rds")

# Stability 
proj3_booted_part2hc <- bootnet(proj3_contact_net_hc, bootstraps = 1000)
proj3_booted_part2hc_res <- plot(proj3_booted_part2hc, labels = FALSE, order = "sample")
saveRDS(proj3_booted_part2hc, "proj3_booted_part2hc24102023.rds")
# proj3_booted_part2hc <- readRDS("proj3_booted_part2hc24102023.rds")

# Plot network 
Proj3_net_a_contact_hc <- plot(proj3_contact_net_hc, 
                               labels = c("Happy", "Sad", "Irritated",  "Relaxed", 
                                          "Listening", "Understanding", "Critical", "Dominant"),
                               edge.labels = FALSE,
                               edge.label.cex = 1,
                               groups = Groups_contact_mf2, 
                               layout = layout_net2,
                               theme = "gray",
                               color = c("#E68129", "#9FB1BCFF"),  # previous color = colors_mf2 
                               negDashed = TRUE,
                               negCol = "#8a4d18",
                               posCol = "black",  
                               negDashed = TRUE,
                               edge.width = 0.75, 
                               legend.cex = 0.7,
                               vsize = 12,  # 9 before 
                               label.cex = 3,
                               esize = 37,  # 23 before 
                               cut = 0, 
                               mar = rep(4,4), 
                               asize = 5, 
                               legend = FALSE, 
                               title = "A. Control", 
                               title.cex = 2, 
                               maximum = 0.7)

#####################################################################################################
# Make panel figure 
#####################################################################################################
# Panel figure - need to add A to B numbering and  groups (hc vs. depr)
# HC: mother, father (incl legend)
# Depr: mother, father 
layout(matrix(c(1, 2, 2), ncol = 3, nrow = 1, byrow = TRUE), 
       widths = c(1, 1, 0.05))
plot(Proj3_net_a_contact_hc)     # HC 
plot(Proj3_net_a_contact_depr)   # depr + legend 
# Export pdf 9:17  , other network did 8:13
dev.off()

# With legend plotted outside figure 
layout(matrix(c(1, 2, 3), ncol = 3, nrow = 1, byrow = TRUE), 
       widths = c(1, 1, 0.4))
plot(Proj3_net_a_contact_hc)     # HC 
plot(Proj3_net_a_contact_depr_wl)  # depr
plot(Proj3_net_a_contact_depr_ol) # legend 
# Export pdf 9:17  , other network did 8:13
dev.off()

#####################################################################################################
# Stability - figure 
####################################################################################
## Depression 
proj3_booted_part2depr_res <- plot(proj3_booted_part2depr, 
                                   order = "id", 
                                   sampleColor = "#6E8898FF", samplelwd = 0.4, labels = TRUE, 
                                   meanlwd = 0.5, meanColor = "black")

savelabelsedgestab_depr2 <- ggplot_build(proj3_booted_part2depr_res)$layout$panel_params[[1]]$y$get_labels()  # get labels, same as in  

proj3_booted_part2depr_res2 <- proj3_booted_part2depr_res + 
  theme(legend.position = "right") + 
  scale_y_continuous(breaks = c(1:28), 
                     labels = rep("", 28)) + 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "grey80")) + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank()) + 
  labs(title = "", subtitle = "B. Depression sample", x = "Weights")

## hc 
proj3_booted_part2hc_res <- plot(proj3_booted_part2hc, 
                                 order = "id", 
                                 sampleColor = "#6E8898FF", samplelwd = 0.4, labels = TRUE, 
                                 meanlwd = 0.5, meanColor = "black")

savelabelsedgestab_hc2 <- ggplot_build(proj3_booted_part2hc_res)$layout$panel_params[[1]]$y$get_labels()  # get labels, same as in  

savelabelsedgestab_hc2 == savelabelsedgestab_depr2  # all TRUE so same order edges 

proj3_booted_part2hc_res2 <- proj3_booted_part2hc_res + 
  theme(legend.position = "right") + 
  scale_y_continuous(breaks = c(1:28),   # 28 edges in total 
                     labels = c(1:28)) + 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "grey80")) + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        legend.position = "none") + 
  labs(title = "", subtitle = "A. Control sample", x = "Weights", y = "Edges")

proj3_booted_part2hc_res + scale_linetype_manual("", values = c("solid", "dashed"), labels = c("Bootstrap mean","Sample"))

grid.arrange(proj3_booted_part2hc_res2, proj3_booted_part2depr_res2, nrow = 1, ncol = 2)

# Save figure as png 
proj3_booted_part2_all <- arrangeGrob(proj3_booted_part2hc_res2, proj3_booted_part2depr_res2, nrow = 1, ncol = 2, widths = c(1, 1.2))
ggsave(file = "Proj3_fig_stab_part2_26092023.png", proj3_booted_part2_all, width = 11, height = 10)


########################################################################################
# Create own figure using data from original figure - Figure B4 
####################################################################################
## Control 
proj3_booted_part2hc_res3 <- ggplot(proj3_booted_part2hc_res$data, 
                                   aes(x = value, y = numericID, color = var, fill = var, group = var)) +
                              geom_ribbon(aes(xmin = q2.5, xmax = q97.5), 
                                          color = NA, fill = "grey", alpha = 0.2) + 
                              geom_path(aes(linetype = var), size = 0.7) + 
                              geom_point(alpha = 0.6, size = 3) + 
                              geom_point(size = 2, shape = 21, fill = NA, stroke = 1) + 
                              scale_y_continuous(breaks = seq(1:length(unique(proj3_booted_part2hc_res$data$numericID))),
                                                 labels = seq(1:length(unique(proj3_booted_part2hc_res$data$numericID)))) +   # levels(proj3_booted_part2hc_res$data$id)
                              scale_color_manual("", values = c("#494F55", "#6E8898FF"), labels = c("Bootstrap mean","Sample")) + 
                              scale_fill_manual("", values = c("#494F55", "#6E8898FF"), labels = c("Bootstrap mean","Sample")) + 
                              scale_linetype_manual("", values = c("solid","dashed"), labels = c("Bootstrap mean","Sample"))+ 
                              theme_classic() + 
                              theme(panel.grid.major = element_line(color = "grey80"),
                                    panel.grid.minor.x = element_line(color = "grey80"),
                                    strip.background = element_blank(), 
                                    strip.text = element_blank(), 
                                    #axis.text.y = element_blank(),
                                    legend.position = "none") + 
                              labs(title = "", subtitle = "A. Control sample", x = "Weights", y = "Edges")  
                              
## Depr 
proj3_booted_part2depr_res3 <- ggplot(proj3_booted_part2depr_res$data, 
                                    aes(x = value, y = numericID, color = var, fill = var, group = var)) +
  geom_ribbon(aes(xmin = q2.5, xmax = q97.5), 
              color = NA, fill = "grey", alpha = 0.2) + 
  geom_path(aes(linetype = var), size = 0.7) + 
  geom_point(alpha = 0.6, size = 3) + 
  geom_point(size = 2, shape = 21, fill = NA, stroke = 1) + 
  scale_y_continuous(breaks = seq(1: length(unique(proj3_booted_part2depr_res$data$numericID))),
                     labels = levels(proj3_booted_part2depr_res$data$id)) +   # levels(proj3_booted_part2hc_res$data$id)
  scale_color_manual("", values = c("#494F55", "#6E8898FF"), labels = c("Bootstrap mean","Sample")) + 
  scale_fill_manual("", values = c("#494F55", "#6E8898FF"), labels = c("Bootstrap mean","Sample")) + 
  scale_linetype_manual("", values = c("solid","dashed"), labels = c("Bootstrap mean","Sample"))+ 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor.x = element_line(color = "grey80"),
        strip.background = element_blank(), 
        strip.text = element_blank(), 
        axis.text.y = element_blank()) + 
  labs(title = "", subtitle = "B. Depression sample", x = "Weights", y = "")  

# Save figure as png 
proj3_booted_part2_all2 <- arrangeGrob(proj3_booted_part2hc_res3, proj3_booted_part2depr_res3, nrow = 1, ncol = 2, widths = c(1, 1.2))
ggsave(file = "Proj3_fig_stab_part2_24102023.png", proj3_booted_part2_all2, width = 11, height = 9)

min(abs(proj3_booted_part2depr_res$data$q97.5 - proj3_booted_part2depr_res$data$q2.5))
min(abs(proj3_booted_part2hc_res$data$q97.5 - proj3_booted_part2hc_res$data$q2.5))

max(abs(proj3_booted_part2depr_res$data$q97.5 - proj3_booted_part2depr_res$data$q2.5))
max(abs(proj3_booted_part2hc_res$data$q97.5 - proj3_booted_part2hc_res$data$q2.5))




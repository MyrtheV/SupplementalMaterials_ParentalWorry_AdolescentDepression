####################################################################################
# Networks 1 
####################################################################################
# Includes code for network estimation and Figures 1, B3, and B4 of Appendix B 

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
# HC 
proj3_hc_endofday_meanaf2  # list that contains data for mothers and fathers 

# Depr 
proj3_depr_endofday_meanaf_am  # mothers
proj3_depr_endofday_meanaf_af  # fathers 

####################################################################################
# Network estimation 
####################################################################################
## HC 
### Data prep 
varnetworkendofday_meanaf <- c("HAPPY_m", "SAD_m", "RELAX_m", "IRRI_m", "E_CRITICM", "E_WARMM",   # nodes adolescent
                               "E_CRITIC", "E_WARM", "E_WORRY", "E_PESTEEM")  # nodes parent 


varnetworkendofday2_meanaf <- c("HAPPY_m", "SAD_m", "RELAX_m", "IRRI_m", "E_CRITICF12", "E_WARMF12",   # nodes adolescent
                                "E_CRITIC", "E_WARM", "E_WORRY", "E_PESTEEM")  # nodes parent 

proj3_endofday_af_net_hc2_mean <- proj3_hc_endofday_meanaf2$father[, varnetworkendofday2_meanaf]
proj3_endofday_am_net_hc2_mean <- proj3_hc_endofday_meanaf2$mother[, varnetworkendofday_meanaf]

varnamesmfpar1 <- c("HAPPY", "SAD", "RELAX", "IRRI", 
                     "CRITIC_a", "WARM_a", "CRITIC_p", "WARM_P", 
                     "WORRY", "PESTEEM")
 
names(proj3_endofday_af_net_hc2_mean) <- varnamesmfpar1 
names(proj3_endofday_am_net_hc2_mean) <- varnamesmfpar1 

proj3_endofday_amf_net_hc_mean <- rbind(proj3_endofday_am_net_hc2_mean, proj3_endofday_af_net_hc2_mean)
nrow(proj3_endofday_amf_net_hc_mean)  # 2212, correct double 

varnamesmfpar1 <- c("HAPPY", "SAD", "RELAX", "IRRI", 
                    "CRITIC_a", "WARM_a", "CRITIC_p", "WARM_P", 
                    "WORRY", "PESTEEM")

### Network estimation 
proj3_EL_endofday_meanaf_amf_net_hc2 <- estimateNetwork(proj3_endofday_amf_net_hc_mean, "EBICglasso")
saveRDS(proj3_EL_endofday_meanaf_amf_net_hc2, "proj3_EL_endofday_meanaf_amf_net_hc2_16102023.rds")
# proj3_EL_endofday_meanaf_amf_net_hc2 <- readRDS("proj3_EL_endofday_meanaf_amf_net_hc2_16102023.rds")

### Network stability 
proj3_booted_amfnethc2 <- bootnet(proj3_EL_endofday_meanaf_amf_net_hc2, bootstraps = 1000)
saveRDS(proj3_booted_amfnethc2, "proj3_booted_amfnethc2_16102023.rds")
# proj3_booted_amfnethc2 <- readRDS("proj3_booted_amfnethc1_16102023.rds")

## Depr 
### Combine datasets 
proj3_endofday_am_net3_mean <- proj3_depr_endofday_meanaf_am[,varnetworkendofday_meanaf]
proj3_endofday_af_net3_mean <- proj3_depr_endofday_meanaf_af[,varnetworkendofday2_meanaf]

names(proj3_endofday_am_net3_mean) <- varnamesmfpar1
names(proj3_endofday_af_net3_mean) <- varnamesmfpar1
 
proj3_endofday_amf_net_depr2 <- rbind(proj3_endofday_am_net3_mean, proj3_endofday_af_net3_mean)
nrow(proj3_endofday_amf_net_depr2)  # 924, correct double from 462 

### Network estimation 
proj3_EL_endofday_meanaf_amf_net_depr2 <- estimateNetwork(proj3_endofday_amf_net_depr2, "EBICglasso")
saveRDS(proj3_EL_endofday_meanaf_amf_net_depr2, "proj3_EL_endofday_meanaf_amf_net_depr2_16102023.rds")
# proj3_EL_endofday_meanaf_amf_net_depr2 <- readRDS("proj3_EL_endofday_meanaf_amf_net_depr2_16102023.rds")

# Check number of observations per variable 
sum(is.na(proj3_endofday_amf_net_depr2$HAPPY))

### Network stability 
proj3_booted_amfnetdepr2 <- bootnet(proj3_EL_endofday_meanaf_amf_net_depr2, bootstraps = 1000)
saveRDS(proj3_booted_amfnetdepr2, "proj3_booted_amfnetdepr2_16102023.rds")
# proj3_booted_amfnetdepr <- readRDS("proj3_booted_amfnetdepr2_16102023.rds")

######################################################################################
# Network figure - used for Figure 1 
######################################################################################
Groups_amf_daily <- c(rep("Adolescent", 6), rep("Parent", 4))
colors_amf2 <- c("#D3D0CBFF", "#9FB1BCFF")

layout_m <-  matrix(c(-8.660254e-01, 0.25, 
                      -8.660254e-01, -0.25, 
                      -4.660254e-01, 0.75, 
                      -4.660254e-01, -0.75, 
                      0, 0.5, 
                      0, -0.5, 
                      4.660254e-01, 0.75, 
                      4.660254e-01, -0.75, 
                      8.660254e-01, 0.25, 
                      8.660254e-01, -0.25), 10, 2, byrow = TRUE)

Proj3_net_amf_hc2 <- plot(proj3_EL_endofday_meanaf_amf_net_hc2, 
                         labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                    "Critical", "Warmth", "Worry", "Self-efficacy"),
                         layout = layout_m,
                         edge.labels = FALSE,
                         edge.label.cex = 1,
                         #repulsion = 0.75,
                         groups = Groups_amf_daily, 
                         theme = "gray",
                         color = c("#E68129", "#9FB1BCFF"),  
                         negDashed = TRUE,
                         negCol = "#8a4d18",
                         posCol = "black",
                         edge.width = 0.75, 
                         legend.cex = 0.8,
                         vsize = 12,  # from 9 to 12
                         label.cex = 2.5,  # from 2 to 2.5 
                         esize = 37,  # from 23 to 37 
                         cut = 0, 
                         mar = c(4,4,4,4), 
                         asize = 5, 
                         title = "A. Control", 
                         title.cex = 2, 
                         legend = FALSE, 
                         maximum = 0.7  # for comparison with other network 
) 


Proj3_net_amf_depr2 <- plot(proj3_EL_endofday_meanaf_amf_net_depr2, 
                           labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                      "Critical", "Warmth", "Worry", "Self-efficacy"),
                           layout = layout_m,
                           edge.labels = FALSE,
                           edge.label.cex = 1,
                           #repulsion = 0.75,
                           groups = Groups_amf_daily, 
                           theme = "gray",
                           color = c("#E68129", "#9FB1BCFF"),  
                           negDashed = TRUE,
                           negCol = "#8a4d18",
                           posCol = "black",
                           edge.width = 0.75, 
                           legend.cex = 0.8,
                           vsize = 12,
                           label.cex = 2.5,
                           esize = 37,
                           cut = 0, 
                           mar = c(4,4,4,0), 
                           asize = 5, 
                           title = "B. Depression", 
                           title.cex = 2, 
                           legend = FALSE, 
                           maximum = 0.7  # to allow comparison with other network 
) 

Proj3_net_amf_depr2_ol <- plot(proj3_EL_endofday_meanaf_amf_net_depr2, 
                            labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                       "Critical", "Warmth", "Worry", "Self-efficacy"),
                            layout = layout_m,
                            edge.labels = FALSE,
                            edge.label.cex = 1,
                            #repulsion = 0.75,
                            groups = Groups_amf_daily, 
                            theme = "gray",
                            color = c("#E68129", "#9FB1BCFF"),  
                            negDashed = TRUE,
                            negCol = "#8a4d18",
                            posCol = "black",
                            edge.width = 0.75, 
                            legend.cex = 0.8,
                            vsize = 12,
                            label.cex = 2.5,
                            esize = 37,
                            cut = 0, 
                            mar = c(6,0,0,7), 
                            asize = 5, 
                            title = "", 
                            title.cex = 2, 
                            layoutOffset = c(-3, 0),
                            maximum = 0.7  # to allow comparison with other network 
) 

# Panel figure - need to add A to B numbering (hc vs. depr)
layout(matrix(c(1, 2, 2), ncol = 3, nrow = 1, byrow = TRUE), 
       widths = c(1, 1, 0.15))
plot(Proj3_net_amf_hc2)    # Mother hc 
plot(Proj3_net_amf_depr2)    # Father hc + legend 
# Export pdf 8:13  
dev.off()

######################################################################################
# Stability figure 
######################################################################################
## hc 
proj3_booted_amfnethc_res_2 <- plot(proj3_booted_amfnethc2, order = "id", 
                                  sampleColor = "#6E8898FF", samplelwd = 0.4, labels = TRUE, 
                                  meanlwd = 0.5, meanColor = "darkgrey")

savelabelsedgestab_amfhc2 <- ggplot_build(proj3_booted_amfnethc_res_2)$layout$panel_params[[1]]$y$get_labels()  # get labels 

proj3_booted_amfnethc_res2_2 <- proj3_booted_amfnethc_res_2  + 
  theme(legend.position = "right") + 
  scale_y_continuous(breaks = c(1:45), 
                     labels = c(1:45)) + 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "grey80")) + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        legend.position = "none") + 
  labs(title = "A. Control", x = "Weights", y = "Edges") 

## depr 
proj3_booted_amfnetdepr_res_2 <- plot(proj3_booted_amfnetdepr2, order = "id", 
                                    sampleColor = "#6E8898FF", samplelwd = 0.4, labels = TRUE, 
                                    meanlwd = 0.5, meanColor = "darkgrey")

savelabelsedgestab_amfdepr2 <- ggplot_build(proj3_booted_amfnetdepr_res_2)$layout$panel_params[[1]]$y$get_labels()  # get labels 

proj3_booted_amfnetdepr_res2 <- proj3_booted_amfnetdepr_res_2  + 
  theme(legend.position = "right") + 
  scale_y_continuous(breaks = c(1:45), 
                     labels = rep("", 45)) + 
  theme_classic() + 
  theme(panel.grid.major = element_line(color = "grey80")) + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank(), 
        legend.position = "right") + 
  labs(title = "B. Depression", x = "Weights") 


## Together 
proj3_booted_amfnet_all2 <- arrangeGrob(proj3_booted_amfnethc_res2_2, 
                                       proj3_booted_amfnetdepr_res_2, nrow = 1, ncol = 2, widths = c(1, 1.2))
#ggsave(file = "Proj3_fig_stab_combamf11102023.png", proj3_booted_amfnet_all, width = 11, height = 7)

###################################################################################################
# Make own Stability figure to improve readability - Figure B4 of Appendix B 
###################################################################################################
## Control 
proj3_booted_part1hc_res3_2 <- ggplot(proj3_booted_amfnethc_res_2$data, 
                                    aes(x = value, y = numericID, color = var, fill = var, group = var)) +
  geom_ribbon(aes(xmin = q2.5, xmax = q97.5), 
              color = NA, fill = "grey", alpha = 0.2) + 
  geom_path(aes(linetype = var), size = 0.7) + 
  geom_point(alpha = 0.6, size = 3) + 
  geom_point(size = 2, shape = 21, fill = NA, stroke = 1) + 
  scale_y_continuous(breaks = seq(1:length(unique(proj3_booted_amfnethc_res$data$numericID))),
                     labels = seq(1:length(unique(proj3_booted_amfnethc_res$data$numericID)))) +   # levels(proj3_booted_part2hc_res$data$id)
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
proj3_booted_part1depr_res3_2 <- ggplot(proj3_booted_amfnetdepr_res_2$data, 
                                      aes(x = value, y = numericID, color = var, fill = var, group = var)) +
  geom_ribbon(aes(xmin = q2.5, xmax = q97.5), 
              color = NA, fill = "grey", alpha = 0.2) + 
  geom_path(aes(linetype = var), size = 0.7) + 
  geom_point(alpha = 0.6, size = 3) + 
  geom_point(size = 2, shape = 21, fill = NA, stroke = 1) + 
  scale_y_continuous(breaks = seq(1: length(unique(proj3_booted_amfnetdepr_res$data$numericID))),
                     labels = levels(proj3_booted_amfnetdepr_res$data$id)) +   # levels(proj3_booted_part2hc_res$data$id)
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
proj3_booted_part1_all2_2 <- arrangeGrob(proj3_booted_part1hc_res3_2, proj3_booted_part1depr_res3_2, nrow = 1, ncol = 2, widths = c(1, 1.2))
ggsave(file = "Proj3_fig_stab_part1v2_16102023.png", proj3_booted_part1_all2, width = 11, height = 9)

# Look at widths BCI intervals - adjust in text 
min(abs(proj3_booted_amfnethc_res_2$data$q97.5 - proj3_booted_amfnethc_res_2$data$q2.5))
max(abs(proj3_booted_amfnethc_res_2$data$q97.5 - proj3_booted_amfnethc_res_2$data$q2.5))

# Depr 
min(abs(proj3_booted_amfnetdepr_res_2$data$q97.5 - proj3_booted_amfnetdepr_res_2$data$q2.5))
max(abs(proj3_booted_amfnetdepr_res_2$data$q97.5 - proj3_booted_amfnetdepr_res_2$data$q2.5))




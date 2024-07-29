#############################################################################################################
# Appendix A (Figure A1 and A2)
#############################################################################################################

#############################################################################################################
# Sensitivity I - Figure A1 
#############################################################################################################
# Remove observations listwise - looks similar, only difference as expected is strength of relations changes a bit 
## Control 
proj3_EL_endofday_meanaf_amf_net_hc_l2 <- estimateNetwork(proj3_endofday_amf_net_hc_mean, "EBICglasso", missing = "listwise")
saveRDS(proj3_EL_endofday_meanaf_amf_net_hc_l2, "proj3_EL_endofday_meanaf_amf_net_listwise2_hc17102023.rds")
# proj3_EL_endofday_meanaf_amf_net_hc_12 <- readRDS("proj3_EL_endofday_meanaf_amf_net_listwise2_hc17102023.rds")

Proj3_net_amf_hc_l2 <- plot(proj3_EL_endofday_meanaf_amf_net_hc_l2, 
                            labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                       "Critical", "Warmth", "Worry", "Self-efficacy"),
                            layout = layout_m,
                            edge.labels = TRUE,
                            edge.label.cex = 2.5,
                            #repulsion = 0.75,
                            groups = Groups_amf_daily, 
                            theme = "gray",
                            color = c("#E68129", "#9FB1BCFF"),   # previous colors_amf2
                            negDashed = TRUE,
                            negCol = "#8a4d18", 
                            edge.width = 0.75, 
                            legend.cex = 0.8,
                            vsize = 12,
                            label.cex = 2.5,
                            esize = 37,
                            cut = 0, 
                            mar = c(4,4,4,4), 
                            asize = 5, 
                            title = "A. Control", 
                            title.cex = 2, 
                            legend = FALSE,
                            maximum = 0.7  # to allow comparison with other network 
) 




## Depression
proj3_EL_endofday_meanaf_amf_net_depr_l2 <- estimateNetwork(proj3_endofday_amf_net_depr2, "EBICglasso", missing = "listwise")
saveRDS(proj3_EL_endofday_meanaf_amf_net_depr_l2, "proj3_EL_endofday_meanaf_amf_net_listwise2_depr17102023.rds")
# proj3_EL_endofday_meanaf_amf_net_depr_l2 <- readRDS("proj3_EL_endofday_meanaf_amf_net_listwise2_depr17102023.rds")


Proj3_net_amf_depr_l2 <- plot(proj3_EL_endofday_meanaf_amf_net_depr_l2, 
                              labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                         "Critical", "Warmth", "Worry", "Self-efficacy"),
                              layout = layout_m,
                              edge.labels = TRUE,
                              edge.label.cex = 2.5,
                              #repulsion = 0.75,
                              groups = Groups_amf_daily, 
                              theme = "gray",
                              color = c("#E68129", "#9FB1BCFF"), # previous colors_amf2  
                              negDashed = TRUE,
                              negCol = "#8a4d18", 
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
                              maximum = 0.7  # to allow comparison with other network 
) 

# Panel figure - need to add A to B numbering (hc vs. depr)
layout(matrix(c(1, 2, 2), ncol = 3, nrow = 1, byrow = TRUE), 
       widths = c(1, 1, 0.15))
plot(Proj3_net_amf_hc_l2)    # Mother hc 
plot(Proj3_net_amf_depr_l2)    # Father hc + legend 
# Export pdf 7:13  
dev.off()

#############################################################################################################
# Sensitivity II - differences between mothers versus fathers - Figure A2 
#############################################################################################################

## Control 
proj3_endofday_am_net_hc2_mean  # mother 
proj3_endofday_af_net_hc2_mean  # father 

### Mother 
proj3_sens2_mc <- estimateNetwork(proj3_endofday_am_net_hc2_mean, "EBICglasso")
saveRDS(proj3_sens2_mc, "proj3_sens2_mc_17012024.rds")

### Father 
proj3_sens2_fc <- estimateNetwork(proj3_endofday_af_net_hc2_mean, "EBICglasso")
saveRDS(proj3_sens2_fc, "proj3_sens2_fc_17012024.rds")

## Depression 
proj3_endofday_am_net3_mean  # mothers 
proj3_endofday_af_net3_mean  # fathers

### Mother 
proj3_sens2_md <- estimateNetwork(proj3_endofday_am_net3_mean, "EBICglasso")
saveRDS(proj3_sens2_md, "proj3_sens2_md_17012024.rds")

### Father 
proj3_sens2_fd <- estimateNetwork(proj3_endofday_af_net3_mean, "EBICglasso")
saveRDS(proj3_sens2_fd, "proj3_sens2_fd_17012024.rds")

# Figure 
proj3_sens2_mc_net <- plot(proj3_sens2_mc, 
                           labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                      "Critical", "Warmth", "Worry", "Self-efficacy"),
                           layout = layout_m,
                           edge.labels = TRUE,
                           edge.label.cex = 2.5,
                           #repulsion = 0.75,
                           groups = Groups_amf_daily, 
                           theme = "gray",
                           color = c("#E68129", "#9FB1BCFF"), # previous colors_amf2  
                           negDashed = TRUE,
                           negCol = "#8a4d18", 
                           edge.width = 0.75, 
                           legend.cex = 0.8,
                           vsize = 12,
                           label.cex = 2.5,
                           esize = 37,
                           cut = 0, 
                           mar = c(4,4,4,4), 
                           asize = 5, 
                           title = "Mothers \n A ", 
                           title.cex = 2, 
                           legend = FALSE,
                           maximum = 0.7  # to allow comparison with other network 
) 

proj3_sens2_fc_net <- plot(proj3_sens2_fc, 
                           labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                      "Critical", "Warmth", "Worry", "Self-efficacy"),
                           layout = layout_m,
                           edge.labels = TRUE,
                           edge.label.cex = 2.5,
                           #repulsion = 0.75,
                           groups = Groups_amf_daily, 
                           theme = "gray",
                           color = c("#E68129", "#9FB1BCFF"), # previous colors_amf2  
                           negDashed = TRUE,
                           negCol = "#8a4d18", 
                           edge.width = 0.75, 
                           legend.cex = 0.8,
                           vsize = 12,
                           label.cex = 2.5,
                           esize = 37,
                           cut = 0, 
                           mar = c(4,4,4,0), 
                           asize = 5, 
                           title = "Fathers \n B ", 
                           title.cex = 2, 
                           maximum = 0.7  # to allow comparison with other network 
) 


proj3_sens2_md_net <- plot(proj3_sens2_md, 
                           labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                      "Critical", "Warmth", "Worry", "Self-efficacy"),
                           layout = layout_m,
                           edge.labels = TRUE,
                           edge.label.cex = 2.5,
                           #repulsion = 0.75,
                           groups = Groups_amf_daily, 
                           theme = "gray",
                           color = c("#E68129", "#9FB1BCFF"), # previous colors_amf2  
                           negDashed = TRUE,
                           negCol = "#8a4d18", 
                           edge.width = 0.75, 
                           legend.cex = 0.8,
                           vsize = 12,
                           label.cex = 2.5,
                           esize = 37,
                           cut = 0, 
                           mar = c(4,4,4,4), 
                           asize = 5, 
                           title = "C", 
                           title.cex = 2, 
                           legend = FALSE,
                           maximum = 0.7  # to allow comparison with other network 
) 

proj3_sens2_fd_net <- plot(proj3_sens2_fd, 
                           labels = c("Happy", "Sad", "Relaxed", "Irritated", "Critical", "Warmth", 
                                      "Critical", "Warmth", "Worry", "Self-efficacy"),
                           layout = layout_m,
                           edge.labels = TRUE,
                           edge.label.cex = 2.5,
                           #repulsion = 0.75,
                           groups = Groups_amf_daily, 
                           theme = "gray",
                           color = c("#E68129", "#9FB1BCFF"), # previous colors_amf2  
                           negDashed = TRUE,
                           negCol = "#8a4d18", 
                           edge.width = 0.75, 
                           legend.cex = 0.8,
                           vsize = 12,
                           label.cex = 2.5,
                           esize = 37,
                           cut = 0, 
                           mar = c(4,4,4,4), 
                           legend = FALSE,
                           asize = 5, 
                           title = "D", 
                           title.cex = 2, 
                           maximum = 0.7  # to allow comparison with other network 
) 

# Plot together 
layout(matrix(c(1, 2, 2), ncol = 3, nrow = 1, byrow = TRUE), 
       widths = c(1, 1, 0.15))
plot(Proj3_net_amf_hc2)    # Mother hc 
plot(Proj3_net_amf_depr2)    # Father hc + legend 
# Export pdf 8:13  
dev.off()

layout(matrix(c(1, 2, 2, 3, 4, 0), ncol = 3, nrow = 2, byrow = TRUE), 
       widths = c(1, 1, 0.2))
plot(proj3_sens2_mc_net)    # Mother hc 
plot(proj3_sens2_fc_net)    # Father hc + legend 
plot(proj3_sens2_md_net)  # Mother depr 
plot(proj3_sens2_fd_net)  # Father depr
# Export pdf 9:14
dev.off()





########################################################################################
# Sensitivity network 2: All contact moments - Figure A4 
########################################################################################
# Estimate networks on full dataset 
# (except for participants excluded because of technical issues)

########################################################################################
# Packages 
########################################################################################
# Load packages 
library(qgraph)
library(bootnet)
library(ggplot2)
library(gridExtra)

########################################################################################
# Data - not openly available 
########################################################################################
# Use this data: proj3_mooda_comb3

########################################################################################
# Network Estimation 
########################################################################################
# Depr sample 
proj3_net2_sens1_depr <- proj3_mooda_comb3[proj3_mooda_comb3$depr == 1, c("HAPPY", "SAD", "IRRI", "RELAX", 
                                                                                   "LISTENC_p", "UNDERC_p", "CRITICC_p", "DOMINC_p")]
# use EBICglasso 
proj3_net2_sens1_deprnet <- estimateNetwork(proj3_net2_sens1_depr, "EBICglasso")
saveRDS(proj3_net2_sens1_deprnet, "proj3_net2_sens1_deprnet.rds")
# proj3_net2_sens1_deprnet <- readRDS("proj3_net2_sens1_deprnet.rds")
proj3_net2_sens1_deprnet

# Stability 
proj3_net2_sens1_deprnet_boot <- bootnet(proj3_net2_sens1_deprnet, bootstraps = 1000)
proj3_net2_sens1_deprnet_boot_res <- plot(proj3_net2_sens1_deprnet_boot, labels = FALSE, order = "sample")
saveRDS(proj3_net2_sens1_deprnet_boot_res, "proj3_net2_sens1_deprnet_boot_res.rds")
# proj3_net2_sens1_deprnet_boot_res <- readRDS("proj3_net2_sens1_deprnet_boot_res.rds")

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
proj3_net2_sens1_deprnet_fig <- plot(proj3_net2_sens1_deprnet, 
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

proj3_net2_sens1_deprnet_fig_wl <- plot(proj3_net2_sens1_deprnet, 
                                    labels = c("Happy", "Sad", "Irritated",  "Relaxed", 
                                               "Listening", "Understanding", "Critical", "Dominant"),
                                    edge.labels = TRUE,
                                    edge.label.cex = 3,
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

proj3_net2_sens1_deprnet_fig_ol <- plot(proj3_contact_net_depr , 
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

proj3_net2_sens1_hc <- proj3_mooda_comb3[proj3_mooda_comb3$depr == 0, c("HAPPY", "SAD", "IRRI", "RELAX", 
                                                                               "LISTENC_p", "UNDERC_p", "CRITICC_p", "DOMINC_p")]

# use EBICglasso 
proj3_net2_sens1_hc_net <- estimateNetwork(proj3_net2_sens1_hc, "EBICglasso")
saveRDS(proj3_net2_sens1_hc_net, "proj3_net2_sens1_hc_net.rds")
proj3_net2_sens1_hc_net$graph
# proj3_contact_net_hc <- readRDS("proj3_net2_sens1_hc_net.rds")

# Stability 
proj3_net2_sens1_hc_boot <- bootnet(proj3_net2_sens1_hc_net, bootstraps = 1000)
proj3_net2_sens1_hc_boot_res <- plot(proj3_net2_sens1_hc_boot, labels = FALSE, order = "sample")
saveRDS(proj3_net2_sens1_hc_boot, "proj3_net2_sens1_hc_boot.rds")
# proj3_net2_sens1_hc_boot <- readRDS("proj3_net2_sens1_hc_boot.rds")

# Network 
proj3_net2_sens1_hc_net_fig <- plot(proj3_net2_sens1_hc_net, 
                               labels = c("Happy", "Sad", "Irritated",  "Relaxed", 
                                          "Listening", "Understanding", "Critical", "Dominant"),
                               edge.labels = TRUE,
                               edge.label.cex = 3,
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
plot(proj3_net2_sens1_hc_net_fig)     # HC 
plot(Proj3_net_a_contact_depr)   # depr + legend 
# Export pdf 9:17  , other network did 8:13
dev.off()

# With legend plotted outside figure 
layout(matrix(c(1, 2, 3), ncol = 3, nrow = 1, byrow = TRUE), 
       widths = c(1, 1, 0.4))
plot(proj3_net2_sens1_hc_net_fig)     # HC 
plot(proj3_net2_sens1_deprnet_fig_wl)  # depr
plot(proj3_net2_sens1_deprnet_fig_ol) # legend 
# Export pdf 9:17  , other network did 8:13
dev.off()

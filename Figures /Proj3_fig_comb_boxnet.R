####################################################################################
# Figure 1 & 4 
####################################################################################
# Combined figure boxplots with means/sds and network 

####################################################################################
# Load data - not openly available 
####################################################################################
# The following objects should be loaded: 

### Part I 
## Within person means and sds 
meansd_hcdepr1_long_comb_v3 <- read.csv("R objects/Means/meansd_hcdepr1_long_comb_v3") 
mean_meansd_hcdepr1_long_comb_v2 <- read.csv("mean_meansd_hcdepr1_long_comb_v2.csv")

## Networks 1
Proj3_net_amf_hc2   # from network estimation (run file Proj3_network1_parentworry_meanaff.R)
Proj3_net_amf_depr2  # from network estimation (run file Proj3_network1_parentworry_meanaff.R)

## Legend 
Proj3_net_amf_depr2_ol  # from network estimation (run file Proj3_network1_parentworry_meanaff.R)

# Part II 
## Within person means and sds 
meansd_hcdepr2_long2 <- read.csv("meansd_hcdepr2_long2") 
mean_meansd_hcdepr2_long_comb <- read.csv("mean_meansd_hcdepr2_long_comb.csv")

## Networks 2 
Proj3_net_a_contact_hc  # from network estimation (run file Proj3_network2_adolescentcontact.R)
Proj3_net_a_contact_depr_wl  # from network estimation (run file Proj3_network2_adolescentcontact.R)

## Legend 
Proj3_net_a_contact_depr_ol  # from network estimation (run file Proj3_network2_adolescentcontact.R)

####################################################################################
# Load packages 
####################################################################################
require(ggplot2)
require(qgraph)

####################################################################################
# Networks I 
####################################################################################
## save boxplot 
proj3_boxplot_net1 <- ggplot(meansd_hcdepr1_long_comb_v3, aes(x = as.factor(depr), y = value, fill = as.factor(depr2), color = as.factor(depr2))) + 
  geom_point(position=position_jitterdodge(), size = 1.5,
             alpha = .5) +
  geom_boxplot(alpha = .4) + 
  facet_grid(statistic~fct_inorder(varnames), scales = "free") + 
  xlab("") + ylab("Value") +
  scale_y_continuous(breaks = 0:7) + 
  theme_bw() + 
  scale_fill_manual(values = (c("#E68129", "#9FB1BCFF", "#E68129", "#9FB1BCFF")), labels = c("Control", "Depression", "Control", "Depression")) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF", "#8a4d18", "#6E8898FF")), labels = c("Control", "Depression", "Control", "Depression")) + 
  theme(legend.position = "None", 
        legend.title = element_blank()) + 
  stat_summary(fun = mean, position = position_dodge(0.75),
               geom = "point", color = "black", shape = 20, size = 3.5,
               show.legend = FALSE) + 
  scale_x_discrete(labels= c("Control", "Depression")) + 
  geom_text(data = mean_meansd_hcdepr1_long_comb_v3, color = "black", aes(label = round(value, 2)), 
            vjust = -0.1, hjust = -0.2, size = 3.5) 

## Plot 
par(mar = c(2, 2, 1, 1))
layout(matrix(c(1, 2, 3, 4, 1, 2, 3, 5, 1, 7, 3, 6), ncol = 3), 
       heights = c(1, 5, 1, 8), 
       widths = c(2, 2, 1))
plot.new()
text(0, 0.5, "A. Boxplots", cex = 2, font = 1)  # text on top figure 

plot.new()
vps <- baseViewports()
pushViewport(vps$figure)
vp.TopRight <- viewport(height=unit(1, "npc"), width=unit(1, "npc"), 
                        just=c("left","top"), 
                        y = 1, x = 0)

###  Boxplots 
print(proj3_boxplot_net1, vp = vp.TopRight) 

### Intermezzo 
plot.new()
text(0, 0.7, "B. Networks", cex = 2, font = 1)  # text in between figures 

### Networks 
qgraph(Proj3_net_amf_hc2, title = "          B1. Control", title.cex = 1.7)     # HC 
qgraph(Proj3_net_amf_depr2, title = "B2. Depression", title.cex = 1.7)  # depr
plot(Proj3_net_amf_depr2_ol) # legend 

### Turn off layout 
dev.off()



####################################################################################
# Networks II 
####################################################################################

## save boxplot
proj3_boxplot_net2 <- ggplot(meansd_hcdepr2_long2, aes(x = as.factor(depr), y = value, fill = as.factor(vartype), color = as.factor(vartype))) + 
  geom_point(position=position_jitterdodge(), size = 1.5,
             alpha = .5) +
  geom_boxplot(alpha = .4) + 
  facet_grid(statistic~fct_inorder(varnames), scales = "free") + 
  xlab("") + ylab("Value") +
  scale_y_continuous(breaks = 0:7) + 
  theme_bw() + 
  scale_fill_manual(values = (c("#E68129", "#9FB1BCFF")), labels = c("Control", "Depression")) + 
  scale_color_manual(values = (c("#8a4d18", "#6E8898FF")), labels = c("Control", "Depression")) + 
  theme(legend.position = "None", 
        legend.title = element_blank()) + 
  stat_summary(fun = mean, position = position_dodge(0.75),
               geom = "point", color = "black", shape = 20, size = 3.5,
               show.legend = FALSE) + 
  scale_x_discrete(labels= c("Control", "Depression")) + 
  geom_text(data = mean_meansd_hcdepr2_long_comb, color = "black", aes(label = round(value, 2)), 
            vjust = -0.1, hjust = -0.3, size = 3.5) 


## Plot 
par(mar = c(2, 2, 1, 1))
layout(matrix(c(1, 2, 3, 4, 1, 2, 3, 5, 1, 7, 3, 6), ncol = 3), 
       heights = c(1, 5, 1, 8), 
       widths = c(2, 2, 1))
plot.new()
text(0, 0.5, "A. Boxplots", cex = 2, font = 1)  # text on top figure 

plot.new()
vps <- baseViewports()
pushViewport(vps$figure)
vp.TopRight <- viewport(height=unit(1, "npc"), width=unit(1, "npc"), 
                        just=c("left","top"), 
                        y = 1, x = 0)

###  Boxplots 
print(proj3_boxplot_net2, vp = vp.TopRight) 

### Intermezzo 
plot.new()
text(0, 0.7, "B. Networks", cex = 2, font = 1)  # text in between figures 

### Networks 
qgraph(Proj3_net_a_contact_hc, title = "          B1. Control", title.cex = 1.7)     # HC 
qgraph(Proj3_net_a_contact_depr_wl, title = "B2. Depression", title.cex = 1.7)  # depr
plot(Proj3_net_a_contact_depr_ol) # legend 

### Turn off layout 
dev.off()




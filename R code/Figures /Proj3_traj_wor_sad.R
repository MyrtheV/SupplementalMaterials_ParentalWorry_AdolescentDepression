####################################################################################
# Figure 2 
####################################################################################
# Trajectory affect and worry over time 

####################################################################################
# Load packages 
####################################################################################
library(ggplot2)
library(tidyverse)
library(dplyr)
library(directlabels) 

####################################################################################
# Load data - not openly available 
####################################################################################
affecttraj_hc1_data_v2
affecttraj_depr1_data_v2

####################################################################################
# Figure  
####################################################################################

cricism_traj_worry_plot_v3 <- function(data, individuals, familyi, titletext, yaxistext){
  
  data <- data[individuals == unique(individuals)[familyi] & data$parent == 1, c("Family", "PPN", "depr", "day", "CRITIC_p", "WORRY")]
  colnames(data) <- c("Family", "PPN", "depr", "day", "Criticism", "Worry")
  data <- data %>% 
    pivot_longer(-c(Family, PPN, depr, day), names_to = "variables", values_to = "value")
  
  #data$day <- c(rep(1:14, each = 4), rep(1:14, each = 4))
  

  
  # If control sample then first figure, otherwise second figure (else)
  if(data$depr == "control"){
    addrow1 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean parental criticism", ), 
                          value = rep(meancrihc1_2, 14))
    
    addrow2 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean parental worry", ), 
                          value = rep(meanworryhc1_2, 14))
    
    data <- rbind(data, addrow1, addrow2)
    
    after_imp2 <- ggplot(data, aes(y = value, x = day, color = variables, group = variables, linetype = variables)) + 
      geom_path(size = 1) + 
      geom_point(aes(shape = variables), fill = "white", size = 3, stroke = 1.5) + 
      xlab("Day") + 
      ylab(yaxistext) + 
      ggtitle(titletext) + 
      theme_classic() + 
      theme(panel.grid.major = element_line(color = "#ececec"), 
            legend.title = element_blank(), 
            legend.position = "none", 
            axis.text.x = element_text(size = 13), 
            axis.text.y = element_text(size = 13),
            axis.title = element_text(size = 14), 
            title = element_text(size = 14)) + 
      scale_x_continuous(breaks = c(1:14), 
                         labels = c(1:14),  
                         limits = c(1, 14), 
                         expand = c(0.02, 0.02)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), limits = c(1, 7)) + 
      scale_alpha(guide = "none") + 
      # geom_dl(aes(label = variables, color = as.factor(variables)), 
      #         method = list(dl.trans(x = x + 0.3), dl.trans(y = y + 0.3), "last.qp", cex = 1.2)) +
      scale_colour_manual(name = "",                               # works alphabetically 
                          labels = c("Parental criticism", "Parental criticism - group average", "Parental worry - group average", "Parental worry"),
                          values = c("#E68129", "#E68129", "#6E8898FF", "#6E8898FF")) + 
      scale_shape_manual(name = "",
                         labels = c("Parental criticism", "Parental criticism - group average", 
                                    "Parental worry - group average", "Parental worry"),
                         values = c(21, NA, NA, 21)) + 
      scale_linetype_manual(name = "", 
                            labels = c("Parental criticism", "Parental criticism - group average", "Parental worry - group average", "Parental worry"),
                            values = c(1, 2, 2, 1))
    
  }else {
    addrow1 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean parental criticism", ), 
                          value = rep(meancridepr1_2, 14))
    
    addrow2 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean parental worry", ), 
                          value = rep(meanworrydepr1_2, 14))
    
    data <- rbind(data, addrow1, addrow2)
    
    after_imp2 <- ggplot(data, aes(y = value, x = day, color = variables, group = variables, linetype = variables)) + 
      geom_path(size = 1) + 
      geom_point(aes(shape = variables), fill = "white", size = 3, stroke = 1.5) + 
      xlab("Day") + 
      ylab(yaxistext) + 
      ggtitle(titletext) + 
      theme_classic() + 
      theme(panel.grid.major = element_line(color = "#ececec"), 
            legend.text = element_text(size = 13),
            axis.text.x = element_text(size = 13), 
            axis.text.y = element_text(size = 13),
            axis.title = element_text(size = 14), 
            title = element_text(size = 14)) + 
      scale_x_continuous(breaks = c(1:14), 
                         labels = c(1:14),  
                         limits = c(1, 14), 
                         expand = c(0.02, 0.02)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), limits = c(1, 7)) + 
      scale_alpha(guide = "none") + 
      scale_colour_manual(name = "",                               # works alphabetically 
                          labels = c("Parental criticism", "Parental criticism - group average", "Parental worry - group average", "Parental worry"),
                          values = c("#E68129", "#E68129", "#6E8898FF", "#6E8898FF")) + 
      scale_shape_manual(name = "",
                         labels = c("Parental criticism", "Parental criticism - group average", "Parental worry - group average", "Parental worry"),
                         values = c(21, NA, NA, 21)) + 
      scale_linetype_manual(name = "", 
                            labels = c("Parental criticism", "Parental criticism - group average", "Parental worry - group average", "Parental worry"),
                            values = c(1, 2, 2, 1))
    
  }
  
  
  
  return(after_imp2)
}

# Hc 
cricism_traj_worry_plot_hc_v5_av <- cricism_traj_worry_plot_v3(affecttraj_hc1_data_v2, affecttraj_hc1_data_v2$Family, 3, titletext = "Family A", yaxistext = "Response")  # 36, one of the ones least missing data 

# depr
cricism_traj_worry_plot_depr_v5_av <- cricism_traj_worry_plot_v3(affecttraj_depr1_data_v2, affecttraj_depr1_data_v2$Family, 17, titletext = "Family B", yaxistext = "")  # one of the ones with least missing data      

# Plot together 
proj3_cricism_traj_worry_plot_all_av_v4 <- arrangeGrob(cricism_traj_worry_plot_hc_v3_av, 
                                                       cricism_traj_worry_plot_depr_v3_av, nrow = 1, ncol = 2, widths = c(0.9, 1))
ggsave(file = "proj3_cricism_traj_worry_plot_all_av_v4.png", proj3_cricism_traj_worry_plot_all_av_v4, width = 15, height = 7)


# Now do same for adolescent sadness and parental worry 
affect_traj_worry_plot_av4 <- function(data, individuals, familyi, titletext, yaxistext){
  
  data <- data[individuals == unique(individuals)[familyi] & data$parent == 1, c("Family", "PPN", "depr", "day", "SAD", "WORRY")]
  colnames(data) <- c("Family", "PPN", "depr", "day", "Criticism", "Worry")
  data <- data %>% 
    pivot_longer(-c(Family, PPN, depr, day), names_to = "variables", values_to = "value")
  
  #data$day <- c(rep(1:14, each = 4), rep(1:14, each = 4))
  
  
  
  # If control sample then first figure, otherwise second figure (else)
  if(data$depr == "control"){
    addrow1 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean adolescent sadness", ), 
                          value = rep(meansadhc1_2, 14))
    
    addrow2 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean parental worry", ), 
                          value = rep(meanworryhc1_2, 14))
    
    data <- rbind(data, addrow1, addrow2)
    
    after_imp2 <- ggplot(data, aes(y = value, x = day, color = variables, group = variables, linetype = variables)) + 
      # geom_line(aes(y = meancrihc1_2), col = "#E68129", linetype = 2, alpha = 0.5) + 
      # geom_line(aes(y = meanworryhc1_2), col = "#6E8898FF", linetype = 2, alpha = 0.5) + 
      # ggplot2::annotate("text", x = 15.6, y = meancrihc1_2 + 0.1, label = "Criticism - average", color = "#8a4d18") + 
      # ggplot2::annotate("text", x = 15.4 , y = meanworryhc1_2 - 0.2, label = "Worry - average", color = "#6E8898FF") +   
      geom_path(size = 1) + 
      geom_point(aes(shape = variables), fill = "white", size = 3, stroke = 1.5) + 
      xlab("Day") + 
      ylab(yaxistext) + 
      ggtitle(titletext) + 
      theme_classic() + 
      theme(panel.grid.major = element_line(color = "#ececec"), 
            legend.title = element_blank(), 
            legend.position = "none", 
            axis.text.x = element_text(size = 13), 
            axis.text.y = element_text(size = 13),
            axis.title = element_text(size = 14), 
            title = element_text(size = 14)) + 
      scale_x_continuous(breaks = c(1:14), 
                         labels = c(1:14),  
                         limits = c(1, 14), 
                         expand = c(0.02, 0.02)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), limits = c(1, 7)) + 
      scale_alpha(guide = "none") + 
      # geom_dl(aes(label = variables, color = as.factor(variables)), 
      #         method = list(dl.trans(x = x + 0.3), dl.trans(y = y + 0.3), "last.qp", cex = 1.2)) +
      scale_colour_manual(name = "",                               # works alphabetically 
                          labels = c("Adolescent sadness", "Adolescent sadness - group average", "Parental worry - group average", "Parental worry"),
                          values = c("#E68129", "#E68129", "#6E8898FF", "#6E8898FF")) + 
      scale_shape_manual(name = "",
                         labels = c("Adolescent sadness", "Adolescent sadness - group average", 
                                    "Parental worry - group average", "Parental worry"),
                         values = c(21, NA, NA, 21)) + 
      scale_linetype_manual(name = "", 
                            labels = c("Adolescent sadness", "Adolescent sadness - group average", "Parental worry - group average", "Parental worry"),
                            values = c(1, 2, 2, 1))
    
  }else {
    addrow1 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean adolescent sadness", ), 
                          value = rep(meansaddepr1_2, 14))
    
    addrow2 <- data.frame(Family = rep(unique(data$Family), 14), 
                          PPN = rep(unique(data$PPN), 14), 
                          depr = rep(unique(data$depr), 14), 
                          day = 1:14, 
                          variables = rep("Group level mean parental worry", ), 
                          value = rep(meanworrydepr1_2, 14))
    
    data <- rbind(data, addrow1, addrow2)
    
    after_imp2 <- ggplot(data, aes(y = value, x = day, color = variables, group = variables, linetype = variables)) + 
       geom_path(size = 1) + 
      geom_point(aes(shape = variables), fill = "white", size = 3, stroke = 1.5) + 
      xlab("Day") + 
      ylab(yaxistext) + 
      ggtitle(titletext) + 
      theme_classic() + 
      theme(panel.grid.major = element_line(color = "#ececec"), 
            legend.text = element_text(size = 13),
            axis.text.x = element_text(size = 13), 
            axis.text.y = element_text(size = 13),
            axis.title = element_text(size = 14), 
            title = element_text(size = 14)) + 
      scale_x_continuous(breaks = c(1:14), 
                         labels = c(1:14),  
                         limits = c(1, 14), 
                         expand = c(0.02, 0.02)) + 
      scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), limits = c(1, 7)) + 
      scale_alpha(guide = "none") + 
      # geom_dl(aes(label = variables, color = as.factor(variables)), 
      #         method = list(dl.trans(x = x + 0.3), dl.trans(y = y + 0.3), "last.qp", cex = 1.2)) +
      scale_colour_manual(name = "",                               # works alphabetically 
                          labels = c("Adolescent sadness", "Adolescent sadness - group average", "Parental worry - group average", "Parental worry"),
                          values = c("#E68129", "#E68129", "#6E8898FF", "#6E8898FF")) + 
      scale_shape_manual(name = "",
                         labels = c("Adolescent sadness", "Adolescent sadness - group average", "Parental worry - group average", "Parental worry"),
                         values = c(21, NA, NA, 21)) + 
      scale_linetype_manual(name = "", 
                            labels = c("Adolescent sadness", "Adolescent sadness - group average", "Parental worry - group average", "Parental worry"),
                            values = c(1, 2, 2, 1))
    
  }
  
  
  
  return(after_imp2)
}

# Hc 
affect_traj_worry_plot_hc_v4_av <- affect_traj_worry_plot_av4(affecttraj_hc1_data_v2, affecttraj_hc1_data_v2$Family, 36, titletext = "Family C", yaxistext = "Response")  # 36, one of the ones least missing data 

# depr
affect_traj_worry_plot_depr_v4_av <- affect_traj_worry_plot_av4(affecttraj_depr1_data_v2, affecttraj_depr1_data_v2$Family, 23, titletext = "Family D", yaxistext = "")  # one of the ones with least missing data      

# Plot together 
# proj3_affect_traj_worry_plot_all_av_v5 <- arrangeGrob(affect_traj_worry_plot_hc_v4_av, 
#                                                       affect_traj_worry_plot_depr_v4_av, nrow = 1, ncol = 2, widths = c(0.8, 1))
# ggsave(file = "proj3_affect_traj_worry_v5_av.png", proj3_affect_traj_worry_plot_all_av_v5, width = 15, height = 7)


# Save together 
plot_both_text_5 <- affect_traj_worry_plot_hc_v4_av + affect_traj_worry_plot_depr_v4_av + 
  plot_annotation(title = "II. Adolescent sadness and parental worry", theme = theme(plot.title = element_text(size = 20)))

plot_both_text_6 <- cricism_traj_worry_plot_hc_v5_av + cricism_traj_worry_plot_depr_v5_av + 
  plot_annotation(title = "I. Parental worry and criticism", theme = theme(plot.title = element_text(size = 20)))

combtex65 <- wrap_elements(plot_both_text_6) / wrap_elements(plot_both_text_5) 
ggsave(file = "proj3_affect_traj_cri_worry_v5_av.png", combtex65, width = 15, height = 12)



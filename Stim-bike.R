setwd("C:/Users/gra09318/Documents/Universite Jean Monnet/FATGAIT/Stim-bike")

library(readxl)
library(dplyr)
library(tidyverse)
library(purrrlyr)
library(ggpubr)
library(ggplot2)
library(tidyr)
library(forcats)
library(ggh4x)
library(rstatix)
library(pacman)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
my_comparisons <- list( c("CP_O", "CP_Y"), c("CP_O", "TD_O"), c("CP_O", "TD_Y") )
stim_bike <- read_excel("stim_bike_data_july.xlsx", 
            sheet = "Data")
emg_bike <- read_excel("stim_bike_data_july.xlsx", 
                       sheet = "EMG")
emg <- read_excel("emg_percentage.xlsx")
stim_bike <- stim_bike %>%
      arrange(id)
emg_bike <- emg_bike %>%
      arrange(id)
emg <- emg %>%
      arrange(id)

stim_bike <- stim_bike %>%
      rowwise() %>%
      mutate(#"va_max_pre" = max(c(va_pre1,va_pre2,va_pre3,va_pre4)),
             'pot_twitch_max_pre' = max(c(twitch_pot_pre1,twitch_pot_pre2,twitch_pot_pre3,twitch_pot_pre4), na.rm = TRUE),
             'itt_max_pre' = max(c(ittmax_pre1,ittmax_pre2,ittmax_pre3,ittmax_pre4), na.rm = TRUE)
             ) %>%
      mutate('pot_twitch_max_pre_norm_ITT' = (`pot_twitch_max_pre`/`itt_max_pre`)*100)
stim_bike <- stim_bike %>%
      group_by(id) %>%
      mutate(normalized_twitch_pre = (pot_twitch_max_pre/weight),
             normalized_ITT_pre = (itt_max_pre/weight),
             final_ittmax_norm = (final_ittmax / weight),
             final_twitch_norm = (final_twitch / weight)) %>%
      mutate(relative_loss_itt = (1-(normalized_ITT_pre - final_ittmax_norm)/normalized_ITT_pre),
             relative_loss_tw = (1-(normalized_twitch_pre - final_twitch_norm)/normalized_twitch_pre),
             relative_loss_va = (1-(va_max_pre - final_va)/va_max_pre),
             relative_watt = (stage1_watt/itt_max_pre))
stim_bike$va_max_pre
stim_bike$pot_twitch_max_pre
stim_bike$itt_max_pre
stim_bike$pot_twitch_max_pre_norm_ITT

# Removing the -inf that appear when I divide by NA's. Turning them into NA's instead. 

stim_bike[mapply(is.infinite, stim_bike)] <- NA

# do.call(data.frame,lapply(stim_bike, function(x) replace(x, is.infinite(x),NA)))

# A dataset for when we need to exclude those who did not like stimulation
# This comes handy when I want to omit the missing data for VA, ITT and Twitch
stim_bike_no_p15 <- read_excel("stim_bike_data_july_noP15.xlsx", 
                               sheet = "Data")
stim_bike_no_p15 <- stim_bike_no_p15 %>%
      rowwise() %>%
      mutate("va_max_pre" = max(c(va_pre1,va_pre2,va_pre3,va_pre4), na.rm = TRUE),
             'pot_twitch_max_pre' = max(c(twitch_pot_pre1,twitch_pot_pre2,twitch_pot_pre3,twitch_pot_pre4), na.rm = TRUE),
             'itt_max_pre' = max(c(ittmax_pre1,ittmax_pre2,ittmax_pre3,ittmax_pre4), na.rm = TRUE)
      ) %>%
      mutate('pot_twitch_max_pre_norm_ITT' = (`pot_twitch_max_pre`/`itt_max_pre`)*100)
stim_bike_no_p15 <- stim_bike_no_p15 %>%
      group_by(id) %>%
      mutate(normalized_twitch_pre = (pot_twitch_max_pre/weight),
             normalized_ITT_pre = (itt_max_pre/weight),
             final_ittmax_norm = (final_ittmax / weight),
             final_twitch_norm = (final_twitch / weight)) %>%
      mutate(relative_loss_itt = (1-(normalized_ITT_pre - final_ittmax_norm)/normalized_ITT_pre),
             relative_loss_tw = (1-(normalized_twitch_pre - final_twitch_norm)/normalized_twitch_pre),
             relative_loss_va = (1-(va_max_pre - final_va)/va_max_pre))

# Group means -------------------------------------------------------------


stim_bike_mean <- subset(stim_bike, mvc1 >0) %>%
      group_by(group) %>%
      mutate("norm_twitch4" = (twitch_pot4/`pot_twitch_max_pre`), na.rm=TRUE,
             "norm_twitch6" = (twitch_pot6/`pot_twitch_max_pre`), na.rm=TRUE,
             "norm_twitch5" = (twitch_pot5/`pot_twitch_max_pre`)) %>%
      summarise_at(vars(`va_max_pre`, `itt_max_pre`, `pot_twitch_max_pre`,
                        twitch_pot4, twitch_pot5, norm_twitch5,
                        norm_twitch4, norm_twitch6,weight, final_va, 
                        final_stage, ittmax4, va4, ittmax6, relative_watt), 
                   list(name = mean, sd = sd), na.rm = TRUE)


# Consider changing participant names to "Group_Px", that way they're constantly
# close to each other in figures, possibly why you don't have them grouped nicely


# Stages completed by group -----------------------------------------------

stages <- ggplot(data = stim_bike,
                 aes(x = group, y = last_stage_min, col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pair)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif", 
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(x = "Group", y = "Stages completed",
           subtitle = "Stages completed in fatiguing test") +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
stages
ggsave(dpi = 150, scale = 1.5,
       "Number coded.jpg")


# For Thomas --------------------------------------------------------------
# Examples for Thomas: 

stages_colour <- ggplot(data = stim_bike,
                        aes(x = group, y = last_stage_min)) +
      geom_point(aes(col = as.factor(stim_bike$pair),
                  shape = as.factor(stim_bike$agegroup)),
                  position = position_dodge(width=.4), size = 5) +
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=25)) +
      labs(x = "Group", y = "Stages completed",
           subtitle = "Stages completed in fatiguing test") +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
stages_colour
ggsave(dpi = 150, scale = 1.5, "Colour-coded with mean plus sd.jpg")

stim_bike_example <- stim_bike
stim_bike_example$group <- factor(stim_bike_example$group, 
                                  levels = c("CP_O","TD_O","CP_Y", "TD_Y"),
                                  labels = c("CP_O" = expression(CP[O]),
                                             "TD_O" = expression(TD[O]),
                                             "CP_Y" = expression(CP[Y]),
                                             "TD_Y" = expression(TD[Y])))


stages_linked <- ggplot(data = stim_bike_example,
                        aes(x = group, y = last_stage_min, position = group)) + 
      geom_point(size = 3) +
      geom_line(linetype = "dashed",aes(group = pairs)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 25, shape = '-', position=position_dodge(width=1)) +

      theme_pubr(legend = "none") +
      theme(text = element_text(size=25)) +
      labs(x = "Group", y = "Stages completed",
           subtitle = "Stages completed in fatiguing test")
stages_linked
ggsave(dpi = 100, scale = 1.5, "Linked.png")

stages_linked_colour <- ggplot(data = stim_bike_example,
                               aes(x = group, y = last_stage_min, position = group)) + 
      geom_point(aes(col = as.factor(stim_bike$pair)), size = 3) +
      geom_line(linetype = "dashed",aes(group = pairs)) +
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + 
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 25, shape = '-', position=position_dodge(width=1)) +
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=25)) +
      labs(x = "Group", y = "Stages completed",
           subtitle = "Stages completed in fatiguing test")
stages_linked_colour
ggsave(dpi = 100, scale = 1.5, "Linked and colour-coded.png")
# Voluntary activation ----------------------------------------------------

vol_bike <- dplyr::select(stim_bike, c(1:3,`va_max_pre`,43,51,59,67,75,83,91,99,107,115,123)) %>%
            rename(
                  "Maximum Pre VA" = 4,
                  "After stage 1" = 5,
                  "After stage 2" = 6,
                  "After stage 3" = 7,
                  "After stage 4" = 8,
                  "After stage 5" = 9,
                  "After stage 6" = 10,
                  "After stage 7" = 11,
                  "After stage 8" = 12,
                  "After stage 9" = 13,
                  "After stage 10" = 14,
                  "After stage 11" = 15
                  ) %>%
      pivot_longer(cols = c(4:15))
vol_bike$name <- as.factor(vol_bike$name)
level_order <- c("Maximum Pre VA",
                 "After stage 1" ,
                 "After stage 2" ,
                 "After stage 3",
                 "After stage 4",
                 "After stage 5",
                 "After stage 6",
                 "After stage 7",
                 "After stage 8",
                 "After stage 9",
                 "After stage 10",
                 "After stage 11")
# vol_bike$name <- factor(vol_bike$name, levels = vol_bike$name[order(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)])

voluntary_activation <- ggplot(data = subset(vol_bike, group !="0" & value !=0), 
      aes(x = factor(name, level = level_order), y = value, col = group, position = group)) +
      geom_point(position=position_dodge(width=1), alpha = 0.2) +
      geom_line(position=position_dodge(width=1), alpha = 0.2, 
                linetype = "dashed",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 10, shape = '-',position=position_dodge(width=1)) +
      theme_pubr() +
      labs(y = 'Voluntary activtion [%]', x = '',
           subtitle = "") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
voluntary_activation

# Same figure, but only up to latest common stage and fatigue

vol_bike_common <- dplyr::select(stim_bike, c(1:3,`va_max_pre`,43,51,59,67,131, 152:154)) %>%
      rename(
            "Maximum Pre VA" = 4,
            "After stage 1" = 5,
            "After stage 2" = 6,
            "After stage 3" = 7,
            "Latest common stage" = 8,
            "Task failure" = 9) %>%
      pivot_longer(cols = c(4:9)) 
level_order_common <- c("Maximum Pre VA",
                        "After stage 1" ,
                        "After stage 2" ,
                        "After stage 3",
                        "Latest common stage",
                        "Task failure")

voluntary_activation_common <- ggplot(
      data = subset(vol_bike_common, group !="0" & value !=0), 
            aes(x = factor(name, level_order_common),
            y = value, col = group, position = group)) +
      geom_point(aes(group = id, shape = as.factor(pairs)),
            position=position_dodge(width=.3), alpha = 1, size = 1.5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      geom_line(position=position_dodge(width=.3), alpha = .1, 
                linetype = "solid",aes(group = id)) +
#      stat_summary(
 #           geom = "point", fun.y = "mean",
  #          size = 15, shape = '-', position=position_dodge(width=.5)) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.5)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.5)) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'Voluntary activation [%]', x = '',
           subtitle = "Voluntary evolution during common stages and task failure") + 
      geom_vline(xintercept = 5.5, linetype = "dashed") +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 5.5, trunc_lower = 0.4))
voluntary_activation_common
ggsave(dpi = 150, scale = 1.5,
       "Everything.jpg")

voluntary_activation_common_onlymeans <- ggplot(
      data = subset(vol_bike_common, group !="0" & value !=0), 
      aes(x = factor(name, level_order_common),
          y = value, col = group, position = group)) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.5)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.5)) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'Voluntary activation [%]', x = '',
           subtitle = "Voluntary evolution during common stages and task failure") + 
      geom_vline(xintercept = 5.5, linetype = "dashed") +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 5.5, trunc_lower = 0.4))
voluntary_activation_common_onlymeans
ggsave(dpi = 150, scale = 1.5, 
       "OnlyMeans.jpg")

voluntary_activation_common_MeansAndDots <- ggplot(
      data = subset(vol_bike_common, group !="0" & value !=0), 
      aes(x = factor(name, level_order_common),
          y = value, col = group, position = group)) +
      geom_point(aes(group = id, shape = as.factor(pair)),
                 position=position_dodge(width=.5), alpha = 1, size = 1.5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.5)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.5)) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'Voluntary activation [%]', x = '',
           subtitle = "Voluntary evolution during common stages and task failure") + 
      geom_vline(xintercept = 5.5, linetype = "dashed") +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 5.5, trunc_lower = 0.4))
voluntary_activation_common_MeansAndDots
ggsave(dpi = 150, scale = 1.5, 
       "MeansAndDots.jpg")
# For Tallinn

vol_bike_Tallinn <- dplyr::select(stim_bike, c(1:3,`va_max_pre`,67,131)) %>%
      rename(
            "Maximum Pre VA" = 4,
            "Latest common stage" = 5,
            "Task failure" = 6) %>%
      pivot_longer(cols = c(4:6)) 
level_order_Tallinn <- c("Maximum Pre VA",
                        "Latest common stage",
                        "Task failure")

voluntary_activation_Tallinn <- ggplot(
      data = subset(vol_bike_Tallinn, group !="0" & value !=0), 
      aes(x = factor(name, level_order_Tallinn),
          y = value, col = group, position = group)) +
      geom_point(aes(group = id, shape = group),
                 position=position_dodge(width=.8), alpha = 0.6, size = 2) +
      geom_line(position=position_dodge(width=.8), alpha = 0.6, 
                linetype = "dashed",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 15, shape = '-', position=position_dodge(width=1)) +
      theme_pubr() +
      labs(y = 'Voluntary activtion [%]', x = '',
           subtitle = "") + 
      geom_vline(xintercept = 4.5, linetype = "dashed") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 4.5, trunc_lower = 0.4))
voluntary_activation_Tallinn
ggsave(dpi = 600, scale = 1.5,
       "Voluntary activation Tallinn.tiff")

facet_VA_TALLINN <- ggplot(
      data = subset(vol_bike_Tallinn, group !="0" & value !=0), 
      aes(x = factor(name, level_order_Tallinn),
          y = value, col = group, position = group)) +
      geom_point(aes(group = id, shape = group), alpha = 0.6, size = 3) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 25, shape = '-', position=position_dodge(width=1)) +
      facet_wrap(vars(group)) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=25)) +
      labs(y = 'Voluntary activation [%]', x = '',
           title = "Voluntary activation after common stage and task failure") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
facet_VA_TALLINN
ggsave(dpi = 600, scale = 1.5,
       "Normalized VA Common Tallinn facet.tiff")

# Voluntary activation - Baseline data ------------------------------------

va_baseline <- ggplot(
      data = stim_bike,
      aes(x = group,
          y = va_max_pre,
          col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") + 
      theme(text = element_text(size=15)) +
      labs(y = 'Voluntary activation [%]', x = '',
           title = "Voluntary activation at baseline") + 
      scale_y_continuous(limits = c(70,110)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
va_baseline
ggsave(dpi = 600, scale = 1.5,
       "VA Baseline.tiff")
compare_means(va_max_pre ~group, data = stim_bike, comparisons = my_comparisons, method = "t.test", p.adjust.method = "bonferroni", pool.sd=TRUE)


# Voluntary activation - Fatigue ------------------------------------------

va_fatigue <- ggplot(data = stim_bike,
                      aes(x = group,
                          y = relative_loss_va*100,
                          col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'VA relative to Pre [%]', x = '',
           title = "VA at task failure relative to Pre-values") + 
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_y_continuous(limits = c(0,140)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
va_fatigue
ggsave(dpi = 600, scale = 1.5,
       "VA relative to pre.tiff")

va_fatigue_absolute <- ggplot(data = stim_bike,
                              aes(x = group,
                                  y = final_va,
                                  col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") + 
      theme(text = element_text(size=15)) +
      labs(y = 'VA [%]', x = '',
           title = "VA at task failure") + 
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_y_continuous(limits = c(0,130)) + 
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
va_fatigue_absolute
ggsave(dpi = 600, scale = 1.5,
       "VA Fatigue.tiff")

va_pre_post <- ggarrange(va_baseline, va_fatigue)
va_pre_post
ggsave(dpi = 600, scale = 1.5, "VA baseline and Fatigue.tiff")

# Potentiated twitch ------------------------------------------------------

twitch <- dplyr::select(stim_bike, c(1:3,`pot_twitch_max_pre`,48,56,64,72,80,88,96,104,112,120,128,136)) %>%
            rename(
                  "Maximum Pre VA" = 4,
                  "After stage 1" = 5,
                  "After stage 2" = 6,
                  "After stage 3" = 7,
                  "After stage 4" = 8,
                  "After stage 5" = 9,
                  "After stage 6" = 10,
                  "After stage 7" = 11,
                  "After stage 8" = 12,
                  "After stage 9" = 13,
                  "After stage 10" = 14,
                  "After stage 11" = 15,
                  "Task failure" = 16
            ) %>%
      pivot_longer(cols = c(4:16))
twitch$name <- as.factor(twitch$name)

twitch <- twitch %>%
      group_by(id) %>%
      mutate(normalized_twitch_max = (value/value[name=='Maximum Pre VA'])*100)

potentiated_twitch <- ggplot(data = subset(twitch, group !="0" & value !=0), 
                               aes(x = fct_inorder(name), y = value, col = group, position = group)) +
      geom_point(position=position_dodge(width=1), alpha = 0.2) +
      geom_line(position=position_dodge(width=1), alpha = 0.2, 
                linetype = "dashed",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 10, shape = '-',position=position_dodge(width=1)) +
      theme_pubr() +
      labs(y = 'Potentiated twitch force [Nm]', x = '',
           subtitle = "") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
potentiated_twitch

# Common stage calculation

twitch_common <- dplyr::select(stim_bike, c(1:3,'pot_twitch_max_pre',48,56,64,72,136,152:154)) %>%
      rename(
            "Maximum Pre Twitch" = 4,
            "After stage 1" = 5,
            "After stage 2" = 6,
            "After stage 3" = 7,
            "Latest common stage" = 8,
            "Task failure" = 9) %>%
      pivot_longer(cols = c(4:9)) 
level_order_common <- c("Maximum Pre Twitch",
                        "After stage 1" ,
                        "After stage 2" ,
                        "After stage 3",
                        "Latest common stage",
                        "Task failure")
twitch_common <- twitch_common %>%
      group_by(id) %>%
      mutate(normalized_twitch_max = (value/value[name=='Maximum Pre Twitch'])*100)

potentiated_twitch_norm_common <- ggplot(
      data = subset(twitch_common, group !="0" & value !=0 & name !="Maximum Pre Twitch"), 
                        aes(x = factor(name, level_order_common),
                        y = normalized_twitch_max, 
                        col = group, 
                        position = group)) +
      geom_point(aes(group = id, shape = as.factor(pairs)),
                 position=position_dodge(width=.5), alpha = 1, size = 1.5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.5)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.5)) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'Twitch force [%] normalized to Pre value', x = '',
           subtitle = "Twitch force until last common stage and task failure") + 
      geom_vline(xintercept = 4.5, linetype = "dashed") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      guides(x = guide_axis_truncated(trunc_upper = 4.5, trunc_lower = 0.4))
potentiated_twitch_norm_common
ggsave(dpi = 600, scale = 1.5,
       "Normalized twitch common stage.tiff")

# Normalized values

twitch_maximum_norm <- ggplot(data = subset(twitch , group !="0" & value !=0), 
                           aes(x = fct_inorder(name), y = normalized_twitch_max, col = group, position = id)) +
      geom_point(position=position_dodge(width=0.2)) +
      geom_line(position=position_dodge(width=0.2), 
                linetype = "dashed",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 10, shape = '-',position=position_dodge(width=1)) +
      theme_pubr() +
      labs(y = 'Twitch force [%] normalized to Pre value', x = '',
           subtitle = "") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
twitch_maximum_norm

# For Tallinn

twitch_max_common_T <- dplyr::select(stim_bike, c(1:3,'pot_twitch_max_pre',72,136)) %>%
      rename(
            "Maximum Pre" = 4,
            "Latest common stage" = 5,
            "Task failure" = 6) %>%
      pivot_longer(cols = c(4:6)) 
twitch_order_common_T <- c("Maximum Pre",
                          "Latest common stage",
                          "Task failure")

twitch_max_common_T <- twitch_max_common_T %>%
      group_by(id) %>%
      mutate(normalized_tw_max = (value/value[name=='Maximum Pre'])*100)

twitch_common_Test_T <- ggplot(
      data = subset(twitch_max_common_T, group !="0" & value !=0), 
      aes(x = factor(name, twitch_order_common_T),
          y = normalized_tw_max, col = group, position = group)) +
      geom_point(aes(group = id, shape = group), alpha = 0.6, size = 3) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 25, shape = '-', position=position_dodge(width=1)) +
      facet_wrap(vars(group)) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=25)) +
      labs(y = 'Normalized Twitch force', x = '',
           title = "Loss of twitch force [%] after common stage and task failure") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
twitch_common_Test_T
ggsave(dpi = 600, scale = 1.5,
       "Normalized twitch Common Tallinn facet.tiff")


# Potentiated twitch - Baseline -------------------------------------------

tw_baseline <- ggplot(
      data = stim_bike,
      aes(x = group,
          y = normalized_twitch_pre,
          col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") + 
      theme(text = element_text(size=15)) +
      labs(y = 'Potentiated Twitch normalized to bodyweight [N/kg]', x = '',
           title = "Potentiated Twitch normalized to bodyweight at baseline") + 
      scale_y_continuous(limits = c(0,3)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
tw_baseline
ggsave(dpi = 600, scale = 1.5,
       "Twitch Baseline.tiff")
compare_means(normalized_twitch_pre ~group, data = stim_bike, comparisons = my_comparisons, method = "t.test", p.adjust.method = "bonferroni", pool.sd=TRUE)


# Potentiated twitch - Fatigue --------------------------------------------

tw_fatigue <- ggplot(data = stim_bike,
                      aes(x = group,
                          y = relative_loss_tw*100,
                          col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") + 
      theme(text = element_text(size=15)) +
      labs(y = 'Twitch relative to Pre [%]', x = '',
           title = "Twitch at task failure relative to Pre-values") + 
      scale_y_continuous(limits = c(0,120)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
tw_fatigue
ggsave(dpi = 600, scale = 1.5,
       "Twitch fatigue relative to pre.tiff")

tw_pre_post <- ggarrange(tw_baseline, tw_fatigue)
tw_pre_post
ggsave(dpi = 600, scale = 1.5, "Twitch baseline and Fatigue.tiff")

# voluntary force ---------------------------------------------------------

itt_max <- dplyr::select(stim_bike, c(1:3,`itt_max_pre`,47,55,63,71,79,87,95,103,111,119,127,135)) %>%
      rename(
            "Maximum Pre VA" = 4,
            "After stage 1" = 5,
            "After stage 2" = 6,
            "After stage 3" = 7,
            "After stage 4" = 8,
            "After stage 5" = 9,
            "After stage 6" = 10,
            "After stage 7" = 11,
            "After stage 8" = 12,
            "After stage 9" = 13,
            "After stage 10" = 14,
            "After stage 11" = 15,
            "Task failure" = 16
      ) %>%
      pivot_longer(cols = c(4:16))
itt_max$name <- as.factor(itt_max$name)

itt_max <- itt_max %>%
      group_by(id) %>%
      mutate(normalized_itt_max = (value/value[name=='Maximum Pre VA'])*100)

itt_maximum <- ggplot(data = subset(itt_max , group !="0" & value !=0), 
                      aes(x = fct_inorder(name), y = value, col = group, position = group)) +
      geom_point(position=position_dodge(width=1), alpha = 0.2) +
      geom_line(position=position_dodge(width=1), alpha = 0.2, 
                linetype = "dashed",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 10, shape = '-',position=position_dodge(width=1)) +
      theme_pubr() +
      labs(y = 'Maximal voluntary force [Nm]', x = '',
           subtitle = "") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
itt_maximum

# in percentage

itt_maximum_norm <- ggplot(data = subset(itt_max , group !="0" & value !=0), 
                      aes(x = fct_inorder(name), y = normalized_itt_max, col = group, position = id)) +
      geom_point(position=position_dodge(width=0.2)) +
      geom_line(position=position_dodge(width=0.2), 
                linetype = "dashed",aes(group = id)) +
      theme_pubr() +
      labs(y = 'Force [%] normalized to Pre value', x = '',
           subtitle = "") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
itt_maximum_norm


# Latest common stage

itt_max_common <- dplyr::select(stim_bike, c(1:3,`itt_max_pre`,47,55,63,71,135,152:154)) %>%
      rename(
            "Maximum Pre" = 4,
            "After stage 1" = 5,
            "After stage 2" = 6,
            "After stage 3" = 7,
            "Latest common stage" = 8,
            "Task failure" = 9) %>%
      pivot_longer(cols = c(4:9)) 
level_order_common <- c("Maximum Pre",
                        "After stage 1" ,
                        "After stage 2" ,
                        "After stage 3",
                        "Latest common stage",
                        "Task failure")

itt_max_common <- itt_max_common %>%
      group_by(id) %>%
      mutate(normalized_itt_max = (value/value[name=='Maximum Pre'])*100)

itt_common <- ggplot(
      data = subset(itt_max_common, group !="0" & name !="Maximum Pre"), 
      aes(x = factor(name, level_order_common),
          y = normalized_itt_max, col = group, position = group)) +
      geom_point(aes(group = id, shape = as.factor(pair)),
                 position=position_dodge(width=.5), alpha = 1, size = 1.5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.5)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.5)) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'Normalized maximal voluntary force [%]', x = '',
           subtitle = "Maximal voluntary force until last common stage and task failure") + 
      geom_vline(xintercept = 4.5, linetype = "dashed") +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 4.5, trunc_lower = 0.4))
itt_common
ggsave(dpi = 600, scale = 1.5,
       "Normalized ITT Common.tiff")

# For Tallinn

itt_max_common_T <- dplyr::select(stim_bike, c(1:3,`itt_max_pre`,71,135)) %>%
      rename(
            "Maximum Pre" = 4,
            "Latest common stage" = 5,
            "Task failure" = 6) %>%
      pivot_longer(cols = c(4:6)) 
level_order_common_T <- c("Maximum Pre",
                        "Latest common stage",
                        "Task failure")

itt_max_common_T <- itt_max_common_T %>%
      group_by(id) %>%
      mutate(normalized_itt_max = (value/value[name=='Maximum Pre'])*100)

itt_common_T <- ggplot(
      data = subset(itt_max_common_T, group !="0" & value !=0 & name != "Maximum Pre"), 
      aes(x = factor(name, level_order_common_T),
          y = normalized_itt_max, col = group, position = group)) +
      geom_point(aes(group = id, shape = group),
                 position=position_dodge(width=1), alpha = 0.6, size = 2) +
      geom_line(position=position_dodge(width=1), alpha = 0.6, 
                linetype = "dashed",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 15, shape = '-', position=position_dodge(width=1)) +
      theme_pubr() +
      labs(y = 'Normalized maximal voluntary force', x = '',
           subtitle = "") + 
      geom_vline(xintercept = 4.5, linetype = "dashed") +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 4.5, trunc_lower = 0.4))
itt_common_T
ggsave(dpi = 600, scale = 1.5,
       "Normalized ITT Common Tallinn.tiff")

itt_common_Test <- ggplot(
      data = subset(itt_max_common_T, group !="0" & value !=0), 
      aes(x = factor(name, level_order_common_T),
          y = normalized_itt_max, col = group, position = group)) +
      geom_point(aes(group = id, shape = group), alpha = 0.6, size = 3) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 25, shape = '-', position=position_dodge(width=1)) +
      facet_wrap(vars(group)) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=25)) +
      labs(y = 'Normalized maximal voluntary force', x = '') +#,
           #title = "Loss of force [%] after common stage and task failure") + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
itt_common_Test
ggsave(dpi = 600, scale = 1.5,
       "Normalized ITT Common Tallinn facet.tiff")


# MVC - Baseline  ---------------------------------------------------------

mvc_baseline <- ggplot(
      data = stim_bike,
      aes(x = group,
          y = normalized_ITT_pre,
          col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'MVC normalized to bodyweight [N/kg]', x = '',
           title = "MVC normalized to bodyweight at baseline") + 
      scale_y_continuous(limits = c(0,12)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
mvc_baseline
ggsave(dpi = 600, scale = 1.5,
       "MVC Baseline.tiff")

mvc_baseline_absolute <- ggplot(
      data = stim_bike,
      aes(x = group,
          y = itt_max_pre,
          col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(y = 'MVC normalized to bodyweight [N/kg]', x = '',
           title = "MVC normalized to bodyweight at baseline") + 
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
mvc_baseline_absolute
ggsave(dpi = 600, scale = 1.5,
       "MVC Baseline absolute.tiff")

compare_means(normalized_ITT_pre ~group, data = stim_bike_no_p15, comparisons = my_comparisons, method = "t.test", p.adjust.method = "bonferroni", pool.sd=TRUE)


# MVC - Fatigue -----------------------------------------------------------

mvc_fatigue <- ggplot(data = stim_bike,
                      aes(x = group,
                          y = relative_loss_itt*100,
                          col = group)) + 
      geom_point(aes(shape = as.factor(stim_bike$pairs)), 
                 position = position_dodge(width=.2), size = 5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=1)) +
      stat_summary(fun = "mean", geom = "point") + 
      stat_compare_means(method = "kruskal.test", label = "p.format") +
      stat_compare_means(method = "wilcox", label = "p.signif",
                         comparisons = my_comparisons,
                         method.args = list(var.equal = TRUE, p.adjust.method = "bonferroni")) +
      theme_pubr(legend = "none") + 
      theme(text = element_text(size=15)) +
      labs(y = 'MVC relative to Pre [%]', x = '',
           title = "MVC at task failure relative to Pre-values") + 
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      
      scale_y_continuous(limits = c(0,120)) + 
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
mvc_fatigue
ggsave(dpi = 600, scale = 1.5,
       "MVC fatigue relative to pre.tiff")

mvc_pre_post <- ggarrange(mvc_baseline, mvc_fatigue)
mvc_pre_post
ggsave(dpi = 600, scale = 1.5, "MVC baseline and Fatigue.tiff")

post_figures <- ggarrange(mvc_fatigue, tw_fatigue, 
                          va_fatigue, stages,
                          labels = c("A","B","C", "D"))
post_figures
ggsave(dpi = 600, scale = 1.5,
       "Fatigue figures.tiff")

# difference MVC and ITT  -------------------------------------------------

# Using a paired T-test to check for differences in maximal force from MVIC
# and ITT efforts

minimal_df <- dplyr::select(stim_bike_no_p15,c(1:3,11,153))

diff_MVC_ITT <- t.test(minimal_df$mvc_max,minimal_df$itt_max_pre, paired = TRUE)
diff_MVC_ITT

ggplot(data = minimal_df, aes(x = mvc_max, y = itt_max_pre)) +
      geom_point() +
      theme_pubr()


# Statistical approaches - Anthropometry ----------------------------------

# Using T-tests to investigate group differences in height, weight, age. 
# Taking the anthropometric data from the overall database

overall_stimbike <- read_excel("U:/FATGAIT/Data/Anthropometrics.xlsx", 
                      sheet = "overall")
overall_stimbike_summary <- overall_stimbike
overall_stimbike_summary <- overall_stimbike_summary %>%
      group_by(group) %>%
      summarise_at(vars(age,weight,height,bmi), 
                   list(name = mean, sd = sd), na.rm = TRUE)

anthro <- names(overall_stimbike)[c(7,8,9,10)]
ttests <- lapply(anthro, function(x) t.test(reformulate("cp",x), data = overall_stimbike))
ttests # No differences in any measure

# This test is across all with CP and all without - One might argue for old vs old 
# and young vs young as a more appropriate test. 

ttests_old <- lapply(anthro, function(x) t.test(reformulate("cp",x), 
                  data = subset(overall_stimbike, age > 26)))
ttests_old

ttests_young <- lapply(anthro, function(x) t.test(reformulate("cp",x), 
                  data = subset(overall_stimbike, age < 26)))
ttests_young

#Make the t-tests between old groups and young groups here - Put in Powerpoint


# Statistical approaches - Baseline data ----------------------------------

# I start by adding all the normalized data to the "stim_bike" df



baseline_summary <- group_by(stim_bike_no_p15, group) %>%
      summarise_at(vars(`va_max_pre`, `itt_max_pre`, `pot_twitch_max_pre`,
                        final_va, final_stage, normalized_twitch_pre, normalized_ITT_pre, lat_stage), 
                   list(mean = mean, sd = sd), na.rm = TRUE)
baseline_summary

# Voluntary activation
aov_VA <- aov(va_max_pre ~ group, data = stim_bike_no_p15)
summary(aov_VA)
kruskal.test(va_max_pre ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$va_max_pre, stim_bike$group, p.adjust.method = "bonf")
pairwise.t.test(stim_bike$va_max_pre, stim_bike$group, p.adjust.method = "bonferroni")
# We see significant differences from the one-way ANOVA and significant post.hoc
# differences between TD and CP_old, but not between CP_O and CP_Y. Similarly,
# no differences between TD populations.
# Testing assumptions
# Homogeneity: 
plot(aov_VA, 1)
levene_test(stim_bike, va_max_pre ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_VA, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_VA))
# Looks fine. 

# ITT Maximal force with stimulation
aov_ITT <- aov(itt_max_pre ~ group, data = stim_bike_no_p15)
summary(aov_ITT)
kruskal.test(itt_max_pre ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$itt_max_pre, stim_bike$group, p.adjust.method = "bonf")
pairwise.t.test(stim_bike$itt_max_pre, stim_bike$group, p.adjust.method = "bonferroni")
# No significant differences found in the One-way ANOVA nor in the post.hoc.
# Testing assumptions
# Homogeneity: 
plot(aov_ITT, 1)
levene_test(stim_bike, itt_max_pre ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_ITT, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_ITT))
# Indicates significant violation of normality

# Potentiated twitch force
aov_TW <- aov(pot_twitch_max_pre ~ group, data = stim_bike_no_p15)
summary(aov_TW)
kruskal.test(pot_twitch_max_pre ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$pot_twitch_max_pre, stim_bike$group, p.adjust.method = "bonf")
pairwise.t.test(stim_bike$pot_twitch_max_pre, stim_bike$group, p.adjust.method = "bonferroni")
# CP_O significantly different from TD_Y
# Testing assumptions
# Homogeneity: 
plot(aov_TW, 1)
levene_test(stim_bike, pot_twitch_max_pre ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_TW, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_TW))
# Indicates no violation of normality

# Normalized ITT force
aov_ITT_norm <- aov(normalized_ITT_pre ~ group, data = stim_bike_no_p15)
summary(aov_ITT_norm)
kruskal.test(normalized_ITT_pre ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$normalized_ITT_pre, stim_bike$group, p.adjust.method = "bonf")
pairwise.t.test(stim_bike$normalized_ITT_pre, stim_bike$group, p.adjust.method = "bonferroni")
# No significant differences found in the One-way ANOVA nor in the post.hoc.
# Testing assumptions
# Homogeneity: 
plot(aov_ITT_norm, 1)
levene_test(stim_bike, normalized_ITT_pre ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_ITT_norm, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_ITT_norm))
# Indicates significant violation of normality

# Normalized potentiated twitch force
aov_TW_norm <- aov(normalized_twitch_pre ~ group, data = stim_bike_no_p15)
summary(aov_TW_norm)
kruskal.test(normalized_twitch_pre ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$normalized_twitch_pre, stim_bike$group, p.adjust.method = "bonf")
pairwise.t.test(stim_bike$normalized_twitch_pre, stim_bike$group, p.adjust.method = "bonferroni")
# CP_O significantly different from TD_Y
# Testing assumptions
# Homogeneity: 
plot(aov_TW_norm, 1)
levene_test(stim_bike, normalized_twitch_pre ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_TW_norm, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_TW))
# Indicates no violation of normality

# Number of stages performed
aov_stages <- aov(last_stage_min ~ group, data = stim_bike)
summary(aov_stages)
kruskal.test(last_stage_min ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$last_stage_min, stim_bike$group, p.adjust.method = "bonf")
pairwise.t.test(stim_bike$last_stage_min, stim_bike$group, p.adjust.method = "bonferroni")
# CP_O significantly different from TD_Y and TD_O
# Testing assumptions
# Homogeneity: 
plot(aov_stages, 1)
levene_test(stim_bike, lat_stage ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_TW_norm, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_stages))
# Indicates no violation of normality


# Statistical approaches - GEE 3 timepoints -------------------------------

library(geepack)
library(emmeans)

# I start by making a new variable including the three timepoints of interest
# and the variables I want to test: ITT, Twitch and VA. Normalized and not

gee5point <- dplyr::select(stim_bike_no_p15, c(1:3,'va_max_pre',43,51,59,67,131,
                               'pot_twitch_max_pre',48,56,64,72,136,
                               'itt_max_pre',47,55,63,71,135)) %>%
      rename(
            "1Maximum Pre VA" = 4,
            "2After stage 1 VA" = 5,
            "3After stage 2 VA" = 6,
            "4After stage 3 VA" = 7,
            "5Latest common stage VA" = 8,
            "6Task failure VA" = 9,
            "1Maximum Pre TW" = 10,
            "2After stage 1 TW" = 11,
            "3After stage 2 TW" = 12,
            "4After stage 3 TW" = 13,
            "5Latest common stage TW" = 14,
            "6Task failure TW" = 15,
            "1Maximum Pre ITT" = 16,
            "2After stage 1 ITT" = 17,
            "3After stage 2 ITT" = 18,
            "4After stage 3 ITT" = 19,
            "5Latest common stage ITT" = 20,
            "6Task failure ITT" = 21) %>%
      group_by(id) %>%
      mutate(normalized_twitch_pre = (`1Maximum Pre TW`/weight),
            normalized_twitch_1 = (`2After stage 1 TW`/weight), 
            normalized_twitch_2 = (`3After stage 2 TW`/weight),
            normalized_twitch_3 = (`4After stage 3 TW`/weight),
            normalized_twitch_common = (`5Latest common stage TW`/weight), 
            normalized_itt_fat = (`6Task failure TW`/weight),
            normalized_itt_pre = (`1Maximum Pre TW`/weight),
            normalized_itt_1 = (`2After stage 1 TW`/weight), 
            normalized_itt_2 = (`3After stage 2 TW`/weight),
            normalized_itt_3 = (`4After stage 3 TW`/weight),
            normalized_itt_common = (`5Latest common stage TW`/weight), 
            normalized_itt_fat = (`6Task failure TW`/weight)
      )
gee5point$id <- as.factor(gee5point$id)
gee5point$group <- as.factor(gee5point$group)
gee4point_va <- gee5point %>%
      pivot_longer(cols = c(4:8)) 
level_order_common <- c("Maximum Pre",
                        "After stage 1" ,
                        "After stage 2" ,
                        "After stage 3",
                        "Latest common stage")
gee4point_itt <- gee5point %>%
      pivot_longer(cols = c(16:20)) %>%
      group_by(id) %>%
      mutate(normalized_itt_max = (value/value[name=='1Maximum Pre ITT'])*100)
gee4point_tw <- gee5point %>%
      pivot_longer(cols = c(10:14)) %>%
      group_by(id) %>%
      mutate(normalized_twitch_max = (value/value[name=='1Maximum Pre TW'])*100)
gee4point_tw$group <- as.factor(gee4point_tw$group)
gee4point_va$group <- as.factor(gee4point_va$group)
gee4point_itt$group <- as.factor(gee4point_itt$group)

gee_va <- geeglm(value ~ group * name, 
                         id = id, data = gee4point_va, corstr = "ar1")
summary(gee_va)
gee_tw_norm <- geeglm(normalized_twitch_max ~ group * name, 
                         id = id, data = gee4point_tw, corstr = "ar1")
summary(gee_tw_norm)
gee_itt_norm <- geeglm(normalized_itt_max ~ group * name,
                         id = id, data = gee4point_itt, corstr = "ar1")
summary(gee_itt_norm)

# Now i can use package multcomp to conduct post-hoc testing
library(multcomp)

summary(glht(gee_va))
summary(glht(gee_tw_norm))
summary(glht(gee_itt_norm))

# Statistical approaches - Fatigue ----------------------------------------

# Same approach as baseline data, so same tests and order. First we need to have
# individual declines in all neuromuscular measures at between baseline -> fatigue
# This is done using mutate in the beginning of this script.

# Voluntary activation
aov_VA_fat <- aov(final_va ~ group, data = stim_bike)
summary(aov_VA_fat)
kruskal.test(final_va ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$final_va, stim_bike$group, p.adjust.method = "BH")
pairwise.t.test(stim_bike$final_va, stim_bike$group, p.adjust.method = "bonferroni")
# We see significant differences from the one-way ANOVA and significant post.hoc
# differences between TD and CP_old, but not between CP_O and CP_Y. Similarly,
# no differences between TD populations.
# Testing assumptions
# Homogeneity: 
plot(aov_VA_fat, 1)
levene_test(stim_bike, final_va ~ group)
# The plot indicates outliers
# Testing QQ plots
plot(aov_VA, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_VA_fat))
# Looks bad. 

# ITT Maximal force with stimulation
aov_ITT_fat <- aov(final_ittmax ~ group, data = stim_bike)
summary(aov_ITT_fat)
kruskal.test(final_ittmax ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$final_ittmax, stim_bike$group, p.adjust.method = "BH")
pairwise.t.test(stim_bike$final_ittmax, stim_bike$group, p.adjust.method = "bonferroni")
# No significant differences found in the One-way ANOVA nor in the post.hoc.
# Testing assumptions
# Homogeneity: 
plot(aov_ITT_fat, 1)
levene_test(stim_bike, final_ittmax ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_ITT_fat, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_ITT_fat))
# Indicates significant violation of normality

# Potentiated twitch force
aov_TW_fat <- aov(final_twitch ~ group, data = stim_bike)
summary(aov_TW_fat)
kruskal.test(final_twitch ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$final_twitch, stim_bike$group, p.adjust.method = "BH")
pairwise.t.test(stim_bike$final_twitch, stim_bike$group, p.adjust.method = "bonferroni")
# CP_O significantly different from TD_Y
# Testing assumptions
# Homogeneity: 
plot(aov_TW_fat, 1)
levene_test(stim_bike, final_twitch ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_TW, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_TW_fat))
# Indicates no violation of normality

# Normalized ITT force
aov_ITT_norm_fat <- aov(final_ittmax_norm ~ group, data = stim_bike)
summary(aov_ITT_norm_fat)
kruskal.test(final_ittmax_norm ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$final_ittmax_norm, stim_bike$group, p.adjust.method = "BH")
pairwise.t.test(stim_bike$final_ittmax_norm, stim_bike$group, p.adjust.method = "bonferroni")
# No significant differences found in the One-way ANOVA nor in the post.hoc.
# Testing assumptions
# Homogeneity: 
plot(aov_ITT_norm_fat, 1)
levene_test(stim_bike, final_ittmax_norm ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_ITT_norm, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_ITT_norm_fat))
# Indicates significant violation of normality

# Normalized potentiated twitch force
aov_TW_norm_fat <- aov(final_twitch_norm ~ group, data = stim_bike)
summary(aov_TW_norm_fat)
kruskal.test(final_twitch_norm ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$final_twitch_norm, stim_bike$group, p.adjust.method = "BH")
pairwise.t.test(stim_bike$final_twitch_norm, stim_bike$group, p.adjust.method = "bonferroni")
# CP_O significantly different from TD_Y
# Testing assumptions
# Homogeneity: 
plot(aov_TW_norm_fat, 1)
levene_test(stim_bike, final_twitch_norm ~ group)
# The plot indicates outliers - Levenes test indicates normal distribution
# Testing QQ plots
plot(aov_TW_norm, 2)
# It's not beautiful, same outliers as before. Testing Shapiro-Wilk
shapiro.test(x = residuals(aov_TW_norm_fat))
# Indicates no violation of normality

kruskal.test(relative_loss_itt ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$relative_loss_itt, stim_bike$group, p.adjust.method = "BH")

kruskal.test(relative_loss_tw ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$relative_loss_tw, stim_bike$group, p.adjust.method = "BH")

kruskal.test(relative_loss_va ~ group, data = stim_bike)
pairwise.wilcox.test(stim_bike$relative_loss_va, stim_bike$group, p.adjust.method = "BH")

# To test for significant with-in group differences from Pre-Fatigue, GEE was
# used with two timepoints (Pre, Fatigue)

gee2point_tw <- gee5point %>%
      pivot_longer(cols = c(10,15)) %>%
      group_by(id) %>%
      mutate(normalized_twitch_max = (value/value[name=='1Maximum Pre TW'])*100)

gee_tw_fat <- geeglm(value ~ group * name, 
                 id = id, data = gee2point_tw, corstr = "ar1")
summary(gee_tw_fat)

gee2point_itt <- gee5point %>%
      pivot_longer(cols = c(16,21)) %>%
      group_by(id) %>%
      mutate(normalized_itt_max = (value/value[name=='1Maximum Pre ITT'])*100)

gee_itt_fat <- geeglm(value ~ group * name, 
                     id = id, data = gee2point_itt, corstr = "ar1")
summary(gee_itt_fat)

gee2point_va <- gee5point %>%
      pivot_longer(cols = c(4,9)) %>%
      group_by(id) %>%
      mutate(normalized_va_max = (value/value[name=='1Maximum Pre VA'])*100)

gee_va_fat <- geeglm(value ~ group * name, 
                      id = id, data = gee2point_va, corstr = "ar1")
summary(gee_va_fat)


# EMG normalization part --------------------------------------------------

EMG_common <- dplyr::select(emg_bike, 1:3,58,62:65, 74) %>%
      rename(
      "Pre" = 4,
      "stage 1" = 5,
      "stage 2" = 6,
      "stage 3" = 7,
      "Latest common stage" = 8,
      "Task failure" = 9) %>%
      pivot_longer(cols = c(4:9)) 
level_order_common <- c("Pre",
                        "stage 1" ,
                        "stage 2" ,
                        "stage 3",
                        "Latest common stage",
                        "Task failure")


EMG_common_plot <- ggplot(
      data = subset(EMG_common, value !=0), 
      aes(x = factor(name, level_order_common),
          y = value*100, col = group, position = group)) +
      geom_point(aes(group = id, shape = group),
                 position=position_dodge(width=.3), alpha = .3) +
      geom_line(position=position_dodge(width=.3), alpha = .3, 
                linetype = "solid",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 15, shape = '-', position=position_dodge(width=.5)) +
      theme_pubr() +
      labs(y = 'EMG normalized to Twitch [%]', x = '',
           subtitle = "EMG during MVC normalized to subsequent potentiated twitch EMG") + 
      geom_vline(xintercept = 5.5, linetype = "dashed") +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 5.5, trunc_lower = 0.4))
EMG_common_plot
ggsave(dpi = 600, scale = 1.5,
       "EMG_common.tiff")


# EMG stages normalized  --------------------------------------------------

# Using imported dataset and making a figure. This starts with naming values
# I'll rename then pivot

emg_selected <- dplyr::select(emg, 1,2,10,11,14,15,18,19,22,23,56:58) %>%
      rename(
            "Start stage 1" = 3,
            "End stage 1" = 4,
            "Start stage 2" = 5,
            "End stage 2" = 6,
            "Start stage 3" = 7,
            "End stage 3" = 8,
            "Start stage 4" = 9,
            "End stage 4" = 10,
            "Task failure" = 11
      ) %>%
      pivot_longer(cols = c(3:11))
level_order_common <- c("Start stage 1",
                        "End stage 1",
                        "Start stage 2",
                        "End stage 2",
                        "Start stage 3",
                        "End stage 3",
                        "Start stage 4",
                        "End stage 4",
                        "Task failure")

EMG <- ggplot(
      data = emg_selected, 
      aes(x = factor(name, level_order_common),
          y = value, col = group, position = group)) +
      geom_point(aes(group = id, shape = group),
                 position=position_dodge(width=.3), alpha = .3) +
      geom_line(position=position_dodge(width=.3), alpha = .3, 
                linetype = "solid",aes(group = id)) +
      stat_summary(
            geom = "point", fun.y = "mean",
            size = 15, shape = '-', position=position_dodge(width=.5)) +
      theme_pubr() +
      theme(text = element_text(size=15)) +
      labs(x = "", y = "EMG Normalized to max",
           subtitle = "EMG normalized to RMS max at PRE") +
      geom_vline(xintercept = 8.5, linetype = "dashed") +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 8.5, trunc_lower = 0.4)) +
      scale_x_discrete(labels = c("CP_O" = expression(CP[O]),
                                  "TD_O" = expression(TD[O]),
                                  "CP_Y" = expression(CP[Y]),
                                  "TD_Y" = expression(TD[Y])))
EMG
ggsave("EMG relative to PRE-Max RMS.tiff", dpi = 600, scale = 1.5)

# Creating a new figure using only the end of stages

emg_selected_end <- dplyr::select(emg, 1,2,11,15,19,23,56:58) %>%
      rename(
            "End stage 1" = 3,
            "End stage 2" = 4,
            "End stage 3" = 5,
            "End stage 4" = 6,
            "Task failure" = 7
      ) %>%
      pivot_longer(cols = c(3:7))
level_order_common <- c("End stage 1",
                        "End stage 2",
                        "End stage 3",
                        "End stage 4",
                        "Task failure")

EMG_end <- ggplot(
      data = emg_selected_end, 
      aes(x = factor(name, level_order_common),
          y = value, col = group, position = group)) +
      geom_point(aes(shape = as.factor(emg_selected_end$pair)), 
                 position = position_dodge(width=.5), size = 1.5) +
      scale_shape_manual(values = c("1","2","3","4","5","6","7","8",
                                    "A","B","C","D","E","F","G")) +#rep(10:24, len = 16)) +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      stat_summary(fun.data = "mean_cl_normal",
                   geom = "errorbar",
                   width = .1,
                   position=position_dodge(width=.5)) +
      stat_summary(fun = "mean", geom = "point", position=position_dodge(width=.5)) + 
      theme_pubr(legend = "none") +
      theme(text = element_text(size=15)) +
      labs(x = "", y = "EMG Normalized to max",
           subtitle = "EMG normalized to RMS max at Pre") +
      geom_vline(xintercept = 4.5, linetype = "dashed") +
      scale_color_manual(values = c("#009E73","#D55E00", "#56B4E9","#E69F00")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      guides(x = guide_axis_truncated(trunc_upper = 4.5, trunc_lower = 0.4)) +
EMG_end
ggsave("END EMG relative to PRE-Max RMS.tiff", dpi = 600, scale = 1.5)


# Packages used -----------------------------------------------------------

# Export Citations as Bibtex (.bib)
write_bib(file="Bibliography of packages.bib")

# Table (.csv) with all information on the packages
appendix_packages <- data.frame(Packagename = character(),
                                Version = character(),
                                Maintainer = character())

require(pacman)
for (pkg in p_loaded()){
      appendix_packages <- appendix_packages %>% add_row(
            Packagename = pkg,
            Version = as.character(packageVersion(pkg)),
            Maintainer = maintainer(pkg)
      )
}

write.csv(x = appendix_packages, file = "List_of_packages.csv", row.names = F)


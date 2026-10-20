library(lme4)
library(car)
library(tidyverse)
library(ggplot2)

# 1. Treatment effects on community dispersion ----------------------------

# loading data
treat <- read.table("treatment_list09_20.txt", header = T, sep = "\t", row.names = 1)
dispersion <- read.csv("Community dispersion.csv", header = T, row.names = 1)

# scale data
treat$year <- treat$year - 2009  
dispersion <- scale(dispersion) %>% data.frame()

# match data
disp_treat <- treat %>% rownames_to_column(var = 'Name') %>% 
  left_join(dispersion %>% rownames_to_column(var = 'Name'), 
            by = 'Name')

# linear mixed model
LMM_lst_g <- list()
LMM_df_g <- data.frame()

for (i in colnames(dispersion)) {
  
  i_formula <- formula(paste(i, '~ warm * precip * clip + (1|year) + (1|block)'))
  
  i_LMM <- lmer(formula = i_formula, data = disp_treat)
  i_summary <- summary(i_LMM)
  i_anova <- car::Anova(i_LMM, type = 2)
  
  # result --
  LMM_lst_g[[i]] <- list(model = i_LMM, 
                         summary = i_summary, 
                         anova2 = i_anova)
  
  i_result <- coef(i_summary) %>% data.frame() %>% 
    rownames_to_column(var = 'Var') %>% 
    rename(Std.Error = Std..Error) %>% 
    left_join(
      i_anova %>% data.frame() %>% 
        rownames_to_column(var = 'Var') %>% 
        select(-Df) %>% 
        rename(P.Chisq = Pr..Chisq.), 
      by = 'Var'
    ) %>% 
    mutate(Distance = i) %>% 
    select(Distance, everything())
  
  LMM_df_g <- bind_rows(LMM_df_g, i_result)
  
}

write.csv(LMM_df_g, 'LMM_dispersion.csv', row.names = F)


# 2. Visualization of treatment effects -----------------------------------

LMM_effect <- read.csv("LMM_year_main.csv")
names(LMM_effect)

LMM_effect$label <- p2star(LMM_effect$P.Chisq)

LMM_effect$Var <- ordered(LMM_effect$Var, 
                          levels = c('Warming','Precipitation','Clipping'))

LMM_effect$Taxa <- ordered(LMM_effect$Taxa, 
                           levels = c('Bacteria','Fungi',
                                      'Protists','Nematodes'))

LMM_effect$Distance <- ordered(LMM_effect$Distance,
                               levels = c('Sorensen','Unweighted.UniFrac'))

# Plot treatment effects
(fig <- ggplot(LMM_effect, aes(x = Var, y = Estimate, 
                               fill = Taxa)) +
    geom_bar(stat = "identity", position = "dodge",
             alpha = 0.7) +
    geom_errorbar(aes(ymin = Estimate - Std.Error,
                      ymax = Estimate + Std.Error),
                  position = position_dodge(0.9), 
                  width = 0.3,
                  col = "grey", linewidth = 0.1) +
    facet_wrap(~ Distance,
               nrow = 1) +
    scale_fill_manual(values = c('#a04635','#e8ae43',
                                 '#4f8255','#494079')) +
    geom_vline(xintercept = c(1.5, 2.5),
               linetype = "dashed",col = "grey",
               linewidth = 0.2) +
    geom_text(data = LMM_effect %>% 
                mutate(Estimate = ifelse(Estimate > 0, Estimate, NA)), 
              aes(y = Estimate + Std.Error + 0.1, label = label),
              position = position_dodge(0.9),
              col = "#7b0008", size = 4) +
    geom_text(data = LMM_effect %>% 
                mutate(Estimate = ifelse(Estimate < 0, Estimate, NA)), 
              aes(y = Estimate - Std.Error - 0.15, 
                  label = label),
              position = position_dodge(0.9),
              col = "#7b0008", size = 4) +
    xlab(" ") + ylab("Effect size") +
    ylim(-0.6, 0.8) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          strip.text = element_text(size = 8),
          strip.background = element_rect(fill = "grey90", color = NA)))
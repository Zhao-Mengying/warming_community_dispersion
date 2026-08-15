library(lme4)
library(car)
library(MuMIn)
library(tidyverse)


# 1. treatment effects by linear mixed model ------------------------------

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
  # i_formula <- formula(paste(i, '~ warm * precip * clip * year + (1|block)'))
  
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

write.csv(LMM_df_g, 'LMM_effect_size.csv', row.names = F)


# 2. treatment effects in each year ---------------------------------------

# test --
dispbac = data.frame(treat[treat$year == 2013, ],
                     scale(dispersion[treat$year == 2013, ]) )
names(dispbac)

fm1 <- lmer(bac_Sorensen ~ warm * precip * clip + (1|block), data = dispbac)
r.squaredGLMM(fm1)
summary(fm1)
car::Anova(fm1, type = 2)


# run --

unique(treat$year)

LMM_lst <- list()
LMM_df <- data.frame()

for (i in unique(treat$year)) {
  
  # extract data of specific year --
  
  i_treat <- treat %>% filter(year == i)
  
  i_dispersion <- dispersion %>% rownames_to_column(var = 'Name') %>% 
    filter(Name %in% rownames(i_treat)) %>% 
    column_to_rownames(var = 'Name') %>% 
    scale() %>% 
    data.frame()
  
  i_df <- i_treat %>% rownames_to_column(var = 'Name') %>% 
    left_join(i_dispersion %>% rownames_to_column(var = 'Name'), 
              by = 'Name')
  
  # select distance index -> LMM --
  
  for (j in colnames(i_dispersion)) {
    
    ij_formula <- formula(paste(j, '~ warm * precip * clip + (1|block)'))
    
    ij_LMM <- lmer(formula = ij_formula, data = i_df)
    ij_summary <- summary(ij_LMM)
    ij_anova <- car::Anova(ij_LMM, type = 2)
    
    
    # Result --
    # list -
    iTag <- paste0('Y', i)
    LMM_lst[[iTag]][[j]] <- list(model = ij_LMM, 
                                 summary = ij_summary, 
                                 anova2 = ij_anova)
    
    # dataframe -
    ij_df <- coef(ij_summary) %>% data.frame() %>% 
      rownames_to_column(var = 'Var') %>% 
      rename(Std.Error = Std..Error) %>% 
      left_join(
        ij_anova %>% data.frame() %>% 
          rownames_to_column(var = 'Var') %>% 
          select(-Df) %>% 
          rename(P.Chisq = Pr..Chisq.), 
        by = 'Var'
      ) %>% 
      mutate(Year = i, Distance = j) %>% 
      select(Year, Distance, everything())
    
    LMM_df <- bind_rows(LMM_df, ij_df)
    
  }

}

write.csv(LMM_df, 'LMM_every_year.csv')

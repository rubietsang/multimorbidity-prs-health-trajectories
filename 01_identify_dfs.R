# Mixed effects models with natural cubic splines - identify best-fitting dfs

library(tidyverse)
library(lme4)
library(lmerTest)
library(splines)
library(broom.mixed)


# Load data
df_incl <- readRDS("data/df_incl_full.rds")


# For each outcome, transform data to long form and loop through models with increasing dfs
# Depressive symptoms
smfq_long <- df_incl %>%
  filter(smfq_incl == 1) %>%
  select(id, aln, qlet, sex, age_10, age_12, age_13, age_16, age_17, age_18,
         age_21, age_22, age_23, age_25, smfq_10:smfq_25, AAM:pc10) %>%
  pivot_longer(.,
               cols = !c(id, aln, qlet, sex, AAM:pc10),
               names_to = c(".value", "clinic"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(clinic = as.factor(clinic),
         age_cen10 = age - 10,
         ln_smfq = log1p(smfq))

smfq_mods <- map(2:5, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_smfq ~ ns(age_cen10, df = ", .x, ") + sex +
  (1 | id), 
  REML = F, data = smfq_long)")))}, otherwise = NA_real_))
unlist(map(smfq_mods, BIC)) # 5 df


# Body mass index
bmi_long <- df_incl %>%
  filter(bmi_incl == 1) %>%
  select(id, aln, qlet, sex, age_7:age_13, age_15, age_17, age_24,
         bmi_7:bmi_24, AAM:pc10) %>%
  pivot_longer(.,
               cols = !c(id, aln, qlet, sex, AAM:pc10),
               names_to = c(".value", "clinic"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(clinic = as.factor(clinic),
         age_cen7 = age - 7,
         ln_bmi = log(bmi))

bmi_mods <- map(2:5, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_bmi ~ ns(age_cen7, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = bmi_long)")))}, otherwise = NA_real_))
unlist(map(bmi_mods, BIC)) # 5 df


# Fat and lean mass indices
dxa_long <- df_incl %>%
  filter(fmi_incl == 1 | lmi_incl == 1) %>%
  select(id, aln, qlet, sex, age_9, age_11, age_13, age_15, age_17, age_24,
         fmi_9:lmi_24, AAM:pc10) %>%
  pivot_longer(.,
               cols = !c(id, aln, qlet, sex, AAM:pc10),
               names_to = c(".value", "clinic"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(clinic = as.factor(clinic),
         age_cen9 = age - 9,
         ln_fmi = log(fmi),
         ln_lmi = log(lmi))

fmi_mods <- map(2:5, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_fmi ~ ns(age_cen9, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = dxa_long)")))}, otherwise = NA_real_))
unlist(map(fmi_mods, BIC)) # 4 df

lmi_mods <- map(2:5, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_lmi ~ ns(age_cen9, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = dxa_long)")))}, otherwise = NA_real_))
unlist(map(lmi_mods, BIC)) # 4 df


# Systolic and diastolic blood pressure
bp_long <- df_incl %>%
  filter(sbp_incl == 1 | dbp_incl == 1) %>%
  select(id, aln, qlet, sex, age_7, age_9, age_11, age_12, age_13, age_15,
         age_17, age_24, sbp_7:dbp_24, AAM:pc10) %>%
  pivot_longer(.,
               cols = !c(id, aln, qlet, sex, AAM:pc10),
               names_to = c(".value", "clinic"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(clinic = as.factor(clinic),
         age_cen7 = age - 7,
         ln_sbp = log(sbp),
         ln_dbp = log(dbp))

sbp_mods <- map(2:5, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_sbp ~ ns(age_cen7, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = bp_long)")))}, otherwise = NA_real_))
unlist(map(sbp_mods, BIC))  # 4 df

dbp_mods <- map(2:5, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_dbp ~ ns(age_cen7, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = bp_long)")))}, otherwise = NA_real_))
unlist(map(dbp_mods, BIC)) # 4 df


# Lipids
chol_long <- df_incl %>%
  filter(hdl_incl == 1 | nonhdl_incl == 1 | trig_incl == 1) %>%
  select(id, aln, qlet, sex, age_7, age_9, age_15, age_17, age_24,
         hdl_7:trig_24, AAM:pc10) %>%
  pivot_longer(.,
               cols = !c(id, aln, qlet, sex, AAM:pc10),
               names_to = c(".value", "clinic"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(clinic = as.factor(clinic),
         age_cen7 = age - 7,
         ln_hdl = log(hdl),
         ln_nonhdl = log1p(nonhdl),
         ln_trig = log(trig))

hdl_mods <- map(2:4, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_hdl ~ ns(age_cen7, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = chol_long)")))}, otherwise = NA_real_))
unlist(map(hdl_mods, BIC)) # 4 df

nonhdl_mods <- map(2:4, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_nonhdl ~ ns(age_cen7, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = chol_long)")))}, otherwise = NA_real_))
unlist(map(nonhdl_mods, BIC)) # 3 df

trig_mods <- map(2:4, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_trig ~ ns(age_cen7, df = ", .x, ") * sex + (1 | id), 
  REML = F, data = chol_long)")))}, otherwise = NA_real_))
unlist(map(trig_mods, BIC)) # 4 df


# Glucose and insulin
met_long <- df_incl %>%
  filter(gluc_incl == 1 | ins_incl == 1) %>%
  select(id, aln, qlet, sex, age_15, age_17, age_24,
         glucose_7:glucose_24, insulin_15:insulin_24, AAM:pc10) %>%
  pivot_longer(.,
               cols = !c(id, aln, qlet, sex, AAM:pc10),
               names_to = c(".value", "clinic"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(clinic = as.factor(clinic),
         age_cen15 = age - 15,
         ln_gluc = log(glucose),
         ln_ins = log(insulin))

gluc_mods <- map(2:3, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_gluc ~ ns(age_cen15, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = met_long)")))}, otherwise = NA_real_))
unlist(map(gluc_mods, BIC)) # 2 df

ins_mods <- map(2:3, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_ins ~ ns(age_cen15, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = met_long)")))}, otherwise = NA_real_))
unlist(map(ins_mods, BIC)) # 2 df


# C-reactive protein
crp_long <- df_incl %>%
  filter(crp_incl == 1) %>%
  select(id, aln, qlet, sex, age_9, age_15, age_17, age_24,
         crp_9:crp_24, AAM:pc10) %>%
  pivot_longer(.,
               cols = !c(id, aln, qlet, sex, AAM:pc10),
               names_to = c(".value", "clinic"),
               names_pattern = "(.*)_(.*)") %>%
  mutate(clinic = as.factor(clinic),
         age_cen9 = age - 9,
         ln_crp = log(crp))

crp_mods <- map(2:3, possibly(~ {eval(parse(text = paste0(
  "lmer(ln_crp ~ ns(age_cen9, df = ", .x, ") + sex + (1 | id), 
  REML = F, data = crp_long)")))}, otherwise = NA_real_))
unlist(map(crp_mods, BIC)) # 3 df

rm(smfq_mods, bmi_mods, fmi_mods, lmi_mods, sbp_mods, dbp_mods,
   hdl_mods, nonhdl_mods, trig_mods, gluc_mods, ins_mods, crp_mods)
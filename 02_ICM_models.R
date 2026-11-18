# Mixed effects models with natural cubic splines - add PRS effects and PCs

library(tidyverse)
library(lme4)
library(lmerTest)
library(splines)
library(broom.mixed)


# Depressive symptoms
smfq_icm <- lmer(ln_smfq ~ ns(age_cen10, df = 5) +
                   linc_icm + age_cen10:linc_icm + sex +
                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                   (1 | id), REML = F, data = smfq_long)

smfq <- tidy(smfq_icm, exponentiate = T, conf.int = T)[c(7, 19),] %>%
  mutate(outcome = "Depressive symptoms") %>%
  relocate(outcome)


# Body mass index
bmi_icm <- lmer(ln_bmi ~ ns(age_cen7, df = 5) +
                  linc_icm + age_cen7:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = bmi_long)

bmi <- tidy(bmi_icm, exponentiate = T, conf.int = T)[c(7, 19),] %>%
  mutate(outcome = "Body mass index") %>%
  relocate(outcome)


# Fat and lean mass indices
fmi_icm <- lmer(ln_fmi ~ ns(age_cen9, df = 4) +
                  linc_icm + age_cen9:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = dxa_long)

fmi <- tidy(fmi_icm, exponentiate = T, conf.int = T)[c(6, 18),] %>%
  mutate(outcome = "Fat mass index") %>%
  relocate(outcome)

lmi_icm <- lmer(ln_lmi ~ ns(age_cen9, df = 5) +
                  linc_icm + age_cen9:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = dxa_long)

lmi <- tidy(lmi_icm, exponentiate = T, conf.int = T)[c(7, 19),] %>%
  mutate(outcome = "Lean mass index") %>%
  relocate(outcome)


# Systolic and diastolic blood pressure
sbp_icm <- lmer(ln_sbp ~ ns(age_cen7, df = 4) +
                  linc_icm + age_cen7:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = bp_long)

sbp <- tidy(sbp_icm, exponentiate = T, conf.int = T)[c(6, 18),] %>%
  mutate(outcome = "Systolic blood pressure") %>%
  relocate(outcome)

dbp_icm <- lmer(ln_dbp ~ ns(age_cen7, df = 4) +
                  linc_icm + age_cen7:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = bp_long)

dbp <- tidy(dbp_icm, exponentiate = T, conf.int = T)[c(6, 18),] %>%
  mutate(outcome = "Diastolic blood pressure") %>%
  relocate(outcome)


# Lipids
hdl_icm <- lmer(ln_hdl ~ ns(age_cen7, df = 4) +
                  linc_icm + age_cen7:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = chol_long)

hdl <- tidy(hdl_icm, exponentiate = T, conf.int = T)[c(6, 18),] %>%
  mutate(outcome = "High-density lipoprotein") %>%
  relocate(outcome)

nonhdl_icm <- lmer(ln_nonhdl ~ ns(age_cen7, df = 3) +
                     linc_icm + age_cen7:linc_icm + sex +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     (1 | id), REML = F, data = chol_long)

nonhdl <- tidy(nonhdl_icm, exponentiate = T, conf.int = T)[c(5, 17),] %>%
  mutate(outcome = "Non-high-density lipoprotein") %>%
  relocate(outcome)

trig_icm <- lmer(ln_trig ~ ns(age_cen7, df = 4) +
                   linc_icm + age_cen7:linc_icm + sex +
                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                   (1 | id), REML = F, data = chol_long)

trig <- tidy(trig_icm, exponentiate = T, conf.int = T)[c(6, 18),] %>%
  mutate(outcome = "Triglycerides") %>%
  relocate(outcome)


# Glucose and insulin
gluc_icm <- lmer(ln_gluc ~ ns(age_cen15, df = 2) +
                   linc_icm + age_cen15:linc_icm + sex +
                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                   (1 | id), REML = F, data = met_long)

gluc <- tidy(gluc_icm, exponentiate = T, conf.int = T)[c(4, 16),] %>%
  mutate(outcome = "Fasting glucose") %>%
  relocate(outcome)

ins_icm <- lmer(ln_ins ~ ns(age_cen15, df = 2) +
                  linc_icm + age_cen15:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = met_long)

ins <- tidy(ins_icm, exponentiate = T, conf.int = T)[c(4, 16),] %>%
  mutate(outcome = "Fasting insulin") %>%
  relocate(outcome)


# C-reactive protein
crp_icm <- lmer(ln_crp ~ ns(age_cen9, df = 3) +
                  linc_icm + age_cen9:linc_icm + sex +
                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                  (1 | id), REML = F, data = crp_long)

crp <- tidy(crp_icm, exponentiate = T, conf.int = T)[c(5, 17),] %>%
  mutate(outcome = "C-reactive protein") %>%
  relocate(outcome)


# Combined results
icm_combined <- rbind(smfq, bmi, fmi, lmi, sbp, dbp, hdl, nonhdl, trig, gluc, ins, crp) %>%
  select(-c(effect, group)) %>%
  mutate(adj.p = p.adjust(p.value, "BH")) %>%
  relocate(p.value, .before = "adj.p")
write.csv(icm_combined, "results/icm_combined.csv", row.names = F)

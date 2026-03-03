# Associations of ICM-MM PRS with immune proteins

library(tidyverse)
library(haven)
library(broom)
library(ggplot2)
library(ggstats)
library(ggrepel)

df_incl <- readRDS("data/df_incl.rds")
olink <- read_dta("data/Olink_data_clean.dta") %>%
  mutate(across(everything(), as.vector))

df_prot_9 <- inner_join(df_incl, olink, by = c("aln", "qlet")) %>%
  select(c(id, aln, qlet, sex, age_9, mat_ed, gplc_icm, linc_icm,
           pc1:pc10, IL8_F9:Olink_QC_Warning_F9)) %>%
  filter(Olink_QC_Warning_F9 == 1) %>%
  rename_with(~str_remove(., '_F9'))

df_prot_9_trans <- df_prot_9 %>%
  mutate(across(c(IL8:CSF1), ~ qnorm((rank(. ,na.last="keep")-0.5)/sum(!is.na(.)))))

df_prot_24 <- inner_join(df_incl, olink, by = c("aln", "qlet")) %>%
  select(c(id, aln, qlet, sex, age_24, mat_ed, gplc_icm, linc_icm,
           pc1:pc10, IL8_F24:Olink_QC_Warning_F24)) %>%
  filter(Olink_QC_Warning_F24 == 1) %>%
  rename_with(~str_remove(., '_F24'))

df_prot_24_trans <- df_prot_24 %>%
  mutate(across(c(IL8:CSF1), ~ qnorm((rank(. ,na.last="keep")-0.5)/sum(!is.na(.)))))


# Linear regressions for proteins measured at age 9
proteins <- colnames(df_prot_9[19:110])

olink_9 <- map_dfr(
  set_names(proteins),
  \(x) {
    tidy(lm(
      formula = paste0(x, " ~ linc_icm + age_9 + sex +
                       pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10"),
      data = df_prot_9_trans
    ),
    conf.int = TRUE,
    conf.level = 0.95)
  },
  .id = "protein"
) %>%
  filter(term == "linc_icm") %>%
  mutate(clinic = "Age 9",
         adj.p = p.adjust(p.value, "BH"))


# Linear regressions for proteins measured at age 24
olink_24 <- map_dfr(
  set_names(proteins),
  \(x) {
    tidy(lm(
      formula = paste0(x, " ~ linc_icm + age_24 + sex +
                       pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10"),
      data = df_prot_24_trans
    ),
    conf.int = TRUE,
    conf.level = 0.95)
  },
  .id = "protein"
) %>%
  filter(term == "linc_icm") %>%
  mutate(clinic = "Age 24",
         adj.p = p.adjust(p.value, "BH"))


# Combined results
olink_combined <- rbind(olink_9, olink_24) %>%
  select(-term) %>%
  relocate(clinic, .after = protein) %>%
  mutate(clinic = fct_relevel(clinic, "Age 9"),
         protein = ifelse(protein == "TF4EBP1", "4EBP1", protein),
         association = adj.p < 0.05)
write.csv(olink_combined, "results/olink_combined.csv", row.names = F)


# Obtain list of proteins for plot
sig_proteins <- olink_combined %>%
  filter(p.value < 0.05) %>%
  select(protein) %>%
  distinct() %>%
  .$protein


# Plot results and export figure
tiff("results/olink_combined.tiff", units="px", width=2400, height=1500, res=300)
olink_combined %>%
  filter(protein %in% sig_proteins) %>%
  ggplot(aes(x = estimate, y = protein, group = clinic)) +
  geom_stripped_rows() +
  geom_vline(xintercept = 0, color = "#000000") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high,
                      colour = clinic, shape = association,
                      fill = association),
                  size = 0.4, position = position_dodge2(width = 0.8, reverse = TRUE)) +
  scale_y_discrete(limits = rev) +
  scale_color_manual(name = "Clinic", values = c("#FFBE0B", "#C31E23")) +
  scale_shape_manual(name = "Adj. p-value",
                     labels = c(">=0.05", "<0.05"),
                     values = c(21, 16)) +
  scale_fill_manual(name = "Adj. p-value",
                    labels = c(">=0.05", "<0.05"),
                    values = c("#FFFFFF", NA)) +
  labs(x = "Estimate (95% CI)", y = "Protein") +
  theme_bw() +
  theme(text = element_text(size = 10),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())
dev.off()
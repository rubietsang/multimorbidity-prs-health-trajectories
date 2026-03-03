# Plot predicted trajectories for 10th, 50th and 90th percentiles

library(ggplot2)
library(ggeffects)
library(ggpubr)


# Obtain values at 10th, 50th and 90th percentile of ICM-MM PRS
deciles <- as.numeric(quantile(df_incl$linc_icm, probs = c(0.1, 0.5, 0.9), na.rm = TRUE))


# Depressive symptoms
smfq_icm_pred <- predict_response(smfq_icm,
                                  terms = c("age_cen10[all]",
                                            "linc_icm[deciles]"),
                                  ci_level = 0.95,
                                  type = "fixed",
                                  interval = "confidence")

smfq_plot <- smfq_icm_pred %>%
  ggplot(aes(x = (x + 10), y = expm1(predicted),
             ymin = expm1(conf.low), ymax = expm1(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(9, 27, 2)) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "SMFQ score",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))


# Body mass index
bmi_icm_pred <- predict_response(bmi_icm,
                                 terms = c("age_cen7[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

bmi_plot <- bmi_icm_pred %>%
  ggplot(aes(x = (x + 7), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(7, 27, 2)) +
  scale_y_continuous(limits = c(10, 30)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = as.expression(bquote(BMI* " ("*kg/m^2*")")),
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))


# Fat and lean mass indices
fmi_icm_pred <- predict_response(fmi_icm,
                                 terms = c("age_cen9[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

fmi_plot <- fmi_icm_pred %>%
  ggplot(aes(x = (x + 9), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(9, 27, 2)) +
  scale_y_continuous(limits = c(0, 8)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = as.expression(bquote(FMI* " ("*kg/m^2*")")),
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))

lmi_icm_pred <- predict_response(lmi_icm,
                                 terms = c("age_cen9[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

lmi_plot <- lmi_icm_pred %>%
  ggplot(aes(x = (x + 9), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(9, 27, 2)) +
  scale_y_continuous(limits = c(12, 18)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = as.expression(bquote(LMI* " ("*kg/m^2*")")),
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))


# Systolic and diastolic blood pressure
sbp_icm_pred <- predict_response(sbp_icm,
                                 terms = c("age_cen7[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

sbp_plot <- sbp_icm_pred %>%
  ggplot(aes(x = (x + 7), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(6, 26, 2)) +
  scale_y_continuous(limits = c(80, 140)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "SBP (mmHg)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))

dbp_icm_pred <- predict_response(dbp_icm,
                                 terms = c("age_cen7[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

dbp_plot <- dbp_icm_pred %>%
  ggplot(aes(x = (x + 7), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(6, 26, 2)) +
  scale_y_continuous(limits = c(40, 80)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "DBP (mmHg)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))


# Lipids
hdl_icm_pred <- predict_response(hdl_icm,
                                 terms = c("age_cen7[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

hdl_plot <- hdl_icm_pred %>%
  ggplot(aes(x = (x + 7), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(6, 26, 2)) +
  scale_y_continuous(limits = c(0.8, 2), breaks = seq(0.8, 2.0, 0.2)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "HDL (mmol/L)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))

nonhdl_icm_pred <- predict_response(nonhdl_icm,
                                    terms = c("age_cen7[all]",
                                              "linc_icm[deciles]"),
                                    ci_level = 0.95,
                                    type = "fixed",
                                    interval = "confidence")

nonhdl_plot <- nonhdl_icm_pred %>%
  ggplot(aes(x = (x + 7), y = expm1(predicted),
             ymin = expm1(conf.low), ymax = expm1(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(6, 26, 2)) +
  scale_y_continuous(limits = c(1.5, 3.5)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "Non-HDL (mmol/L)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))

trig_icm_pred <- predict_response(trig_icm,
                                  terms = c("age_cen7[all]",
                                            "linc_icm[deciles]"),
                                  ci_level = 0.95,
                                  type = "fixed",
                                  interval = "confidence")

trig_plot <- trig_icm_pred %>%
  ggplot(aes(x = (x + 7), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(6, 26, 2)) +
  scale_y_continuous(limits = c(0.4, 1.2)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "Triglycerides (mmol/L)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))


# Glucose and insulin
gluc_icm_pred <- predict_response(gluc_icm,
                                  terms = c("age_cen15[all]",
                                            "linc_icm[deciles]"),
                                  ci_level = 0.95,
                                  type = "fixed",
                                  interval = "confidence")

gluc_plot <- gluc_icm_pred %>%
  ggplot(aes(x = (x + 15), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(14, 26, 2)) +
  scale_y_continuous(limits = c(4.5, 6)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "Fasting glucose (mmol/L)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))

ins_icm_pred <- predict_response(ins_icm,
                                 terms = c("age_cen15[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

ins_plot <- ins_icm_pred %>%
  ggplot(aes(x = (x + 15), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(14, 26, 2)) +
  scale_y_continuous(limits = c(0, 15)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "Fasting insulin (mU/L)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))


# C-reactive protein
crp_icm_pred <- predict_response(crp_icm,
                                 terms = c("age_cen9[all]",
                                           "linc_icm[deciles]"),
                                 ci_level = 0.95,
                                 type = "fixed",
                                 interval = "confidence")

crp_plot <- crp_icm_pred %>%
  ggplot(aes(x = (x + 9), y = exp(predicted),
             ymin = exp(conf.low), ymax = exp(conf.high))) +
  geom_line(aes(group = group, colour = group)) +
  geom_ribbon(aes(group = group, fill = group), alpha = 0.2) +
  scale_x_continuous(breaks = seq(9, 27, 2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1.2, 0.2)) +
  scale_color_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                     labels = c("10th", "50th", "90th")) +
  scale_fill_manual(values = c("#0D7D87", "#888888", "#C31E23"),
                    labels = c("10th", "50th", "90th")) +
  labs(x = "Age (years)", y = "CRP (mg/L)",
       colour = "ICM-MM PRS centile", fill = "ICM-MM PRS centile") +
  theme_classic() +
  theme(text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.box.spacing = unit(0.1, "cm"))


# Combine plots and export figure
tiff("results/icm_combined.tiff", units="px", width=2400, height=1500, res=300)
ggarrange(smfq_plot, bmi_plot, fmi_plot, lmi_plot,
          sbp_plot, dbp_plot, hdl_plot, nonhdl_plot,
          trig_plot, gluc_plot, ins_plot, crp_plot,
          ncol = 4, nrow = 3,
          align = "hv",
          labels = "auto",
          font.label = list(size = 9),
          legend = "bottom",
          common.legend = TRUE)
dev.off()

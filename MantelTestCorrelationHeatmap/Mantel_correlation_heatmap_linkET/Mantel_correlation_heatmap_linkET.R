# ===================== #
# Clean session & load pkgs
# ===================== #
rm(list = ls())

library(linkET)
library(ggplot2)
library(dplyr)
library(vegan)
library(RColorBrewer)
library(reshape2)   # for long-format export
library(Cairo)      # for cairo_pdf device

# ===================== #
# Read data
# ===================== #
# OTU table (rows = samples, cols = taxa/features)
df <- read.table("otu.txt", header = TRUE, row.names = 1,
                 check.names = FALSE, sep = "\t")
cat("OTU table loaded:\n"); print(head(df))

# Environmental table (rows = samples)
env <- read.table("env.txt", sep = "\t", header = TRUE,
                  row.names = 1, check.names = FALSE)
env <- as.data.frame(env)
cat("Environment table loaded:\n"); print(head(env))

# Align samples by rownames (intersection), keep order of OTU table
common_ids <- intersect(rownames(df), rownames(env))
if (length(common_ids) == 0) stop("No shared sample IDs between OTU and ENV.")
df  <- df[common_ids, , drop = FALSE]
env <- env[common_ids, , drop = FALSE]

# Sanity check
if (nrow(df) != nrow(env)) stop("Sample counts still mismatch after alignment.")

# Ensure numeric ENV columns
num_cols <- sapply(env, is.numeric)
if (!all(num_cols)) {
  warning("Non-numeric ENV columns detected; dropping them: ",
          paste(names(env)[!num_cols], collapse = ", "))
  env <- env[, num_cols, drop = FALSE]
}
if (ncol(env) == 0) stop("No numeric columns left in ENV.")

# ===================== #
# Mantel tests
# ===================== #
df_mantel <- mantel_test(
  df, env,
  spec_select = list(
    "Human-derived Salmonella (developed countries)" = 1,
    "Swine-derived Salmonella (developed countries)"  = 2
  )
) %>%
  mutate(
    # r bins (6 levels)
    df_r = cut(
      r,
      breaks = c(-Inf, 0, 0.2, 0.4, 0.6, 0.8, Inf),
      labels = c("-1–0", "0–0.2", "0.2–0.4", "0.4–0.6", "0.6–0.8", "0.8–1"),
      right = TRUE
    ),
    # p-value bins (4 levels)
    df_p = cut(
      p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
      labels = c("< 0.001", "0.001 - 0.01", "0.01 - 0.05", ">= 0.05"),
      right = TRUE
    )
  )
cat("Mantel test results:\n"); print(df_mantel)

# ===================== #
# Correlation matrix (for plotting & export)
# ===================== #
env_cor <- correlate(env)  # linkET correlation object

# ---- Export CSVs ---- #
write.csv(df_mantel, "mantel_results.csv", row.names = FALSE, fileEncoding = "UTF-8")

env_cor_mat <- as.matrix(env_cor)
env_cor_df  <- as.data.frame(env_cor_mat)
env_cor_df$Var <- rownames(env_cor_df)
env_cor_df <- env_cor_df[, c(ncol(env_cor_df), 1:(ncol(env_cor_df)-1))]
write.csv(env_cor_df, "env_correlation_matrix.csv", row.names = FALSE, fileEncoding = "UTF-8")

env_cor_long <- melt(env_cor_mat, varnames = c("Var1","Var2"), value.name = "r")
write.csv(env_cor_long, "env_correlation_matrix_long.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("Saved CSVs: mantel_results.csv, env_correlation_matrix.csv, env_correlation_matrix_long.csv\n")

# ===================== #
# Plot
# ===================== #
# Color scales for heatmap & Mantel overlays
p_fill <- scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdBu")))
p_size <- scale_size_manual(values = c(0.2, 0.5, 1, 1.5, 2, 3))  # 6 sizes for r bins
p_col  <- scale_colour_manual(values = c(
  "< 0.001"      = "#fc8d59",
  "0.001 - 0.01" = "#feda59",
  "0.01 - 0.05"  = "#7f7f7f",
  ">= 0.05"      = "#e6e6e6"
))

p1 <- qcorrplot(env_cor, type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(
    aes(
      xend = .xend + 1.25,
      yend = .yend + 0.5,
      colour = df_p,
      size  = df_r
    ),
    data = df_mantel,
    curvature = 0.1
  ) +
  geom_diag_label(mapping = aes(y = .y + 0.05), hjust = 0.15) +
  p_fill + p_size + p_col +
  guides(
    size   = guide_legend(
      title = "Mantel's r",
      override.aes = list(colour = "grey35"),
      order = 5
    ),
    colour = guide_legend(
      title = "Mantel's p",
      override.aes = list(size = 6),
      order = 3
    ),
    fill   = guide_colorbar(title = "Pearson's r", order = 3)
  ) +
  theme(axis.text.y = element_blank())

print(p1)

# ===================== #
# Save PDF (Cairo for better fonts/embedding)
# ===================== #
out_pdf <- "MantelTestCorrelationHeatmap_Developing.pdf"
ggsave(out_pdf, plot = p1, width = 10, height = 8, device = cairo_pdf)
cat("Heatmap saved as:", out_pdf, "\n")




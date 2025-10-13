# ===================== #
# 清理环境 & 加载包
# ===================== #
rm(list = ls())

library(linkET)
library(ggplot2)
library(dplyr)
library(vegan)
library(RColorBrewer)
library(reshape2)  # 用于导出相关矩阵的长表

# ===================== #
# 读取数据
# ===================== #
# 读取 OTU
df <- read.table("otu2.txt", header = TRUE, row.names = 1,
                 check.names = FALSE, sep = "\t")
cat("OTU 数据加载成功:\n"); print(head(df))

# 读取 环境
env <- read.table("env2.txt", sep = "\t", header = TRUE,
                  row.names = 1, check.names = FALSE)
env <- as.data.frame(env)
cat("环境数据加载成功:\n"); print(head(env))

# 一致性检查
if (nrow(df) != nrow(env)) {
  stop("OTU 和环境数据样本数不一致！请检查数据格式。")
}

# ===================== #
# Mantel 检验
# ===================== #
df_mantel <- mantel_test(df, env,
                         spec_select = list(
                           "Human" = 1,
                           "Swine" = 2
                         )) %>%
  mutate(
    df_r = cut(r,
               breaks = c(-Inf, 0.5, 0.75, 0.9, Inf),
               labels = c("< 0.5", "0.5 - 0.75", "0.75 - 0.9", ">= 0.9"),
               right = TRUE),
    df_p = cut(p,
               breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
               labels = c("< 0.001", "0.001 - 0.01", "0.01 - 0.05", ">= 0.05"),
               right = TRUE)
  )
cat("Mantel 检验结果:\n"); print(df_mantel)

# ===================== #
# 相关矩阵（用于绘图 & 导出）
# ===================== #
# linkET::correlate 返回 "correlate" 对象，先保留它用于绘图
env_cor <- correlate(env)

# —— 新增：导出为 CSV —— #
# 1) Mantel 检验结果
write.csv(df_mantel, "mantel_results.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 2) 将 correlate 对象转为矩阵 -> data.frame（宽表）
env_cor_mat <- as.matrix(env_cor)         # 关键步骤
env_cor_df  <- as.data.frame(env_cor_mat)
env_cor_df$Var <- rownames(env_cor_df)
env_cor_df <- env_cor_df[, c(ncol(env_cor_df), 1:(ncol(env_cor_df)-1))]
write.csv(env_cor_df, "env_correlation_matrix.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 3) 可选：导出成长表（Var1, Var2, r）
env_cor_long <- melt(env_cor_mat, varnames = c("Var1","Var2"), value.name = "r")
write.csv(env_cor_long, "env_correlation_matrix_long.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("已保存 CSV：mantel_results.csv, env_correlation_matrix.csv, env_correlation_matrix_long.csv\n")

# ===================== #
# 绘图
# ===================== #
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
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdBu"))) +
  scale_size_manual(values = c(0.5, 1, 2, 3)) +
  scale_colour_manual(values = c("#fc8d59", "#feda59", "#7f7f7f", "#e6e6e6")) +
  guides(
    size   = guide_legend(title = "Mantel's r", override.aes = list(colour = "grey35"), order = 2),
    colour = guide_legend(title = "Mantel's p", override.aes = list(size = 3), order = 1),
    fill   = guide_colorbar(title = "Pearson's r", order = 3)
  ) +
  theme(axis.text.y = element_blank())

print(p1)

# 保存 PDF
ggsave("p1.pdf", plot = p1, width = 10, height = 8)
cat("热图已成功保存为 p1.pdf\n")




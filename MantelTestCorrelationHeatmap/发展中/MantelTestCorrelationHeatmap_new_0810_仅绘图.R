# ===================== #
# 绘图脚本：基于 mantel_results 再绘图
# 依赖文件：
#   - mantel_results.csv  (连线/显著性来自这里)
#   - env2.txt            (计算环境相关性热图)
# 输出：
#   - p1_mantel.pdf
#   - p1_mantel.png  (可选)
# ===================== #

rm(list = ls())

# ---- 参数（可按需修改） ----
mantel_csv <- "mantel_results.csv"
env_file   <- "env.txt"
spec_keep  <- c()   # 例如 c("Human") 或 c("Human","Swine")；留空表示全部
save_png   <- TRUE  # 需要 PNG 设为 TRUE

# ---- 依赖包 ----
suppressPackageStartupMessages({
  library(linkET)
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
})

# ---- 读取数据 ----
df_mantel <- read.csv(mantel_csv, check.names = FALSE)

# 如果没有 df_r / df_p（分类列），根据 r/p 自动生成
if (!("df_r" %in% names(df_mantel))) {
  df_mantel <- df_mantel %>%
    mutate(
      # 6 档 r 分层：-1–0, 0–0.2, 0.2–0.4, 0.4–0.6, 0.6–0.8, 0.8–1
      df_r = cut(r,
                 breaks = c(-Inf, 0, 0.2, 0.4, 0.6, 0.8, Inf),
                 labels = c("-1–0", "0–0.2", "0.2–0.4", "0.4–0.6", "0.6–0.8", "0.8–1"),
                 right = TRUE)
    )
}
if (!("df_p" %in% names(df_mantel))) {
  df_mantel <- df_mantel %>%
    mutate(
      df_p = cut(p,
                 breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                 labels = c("< 0.001", "0.001 - 0.01", "0.01 - 0.05", ">= 0.05"),
                 right = TRUE)
    )
}

# 可选：只绘特定 spec（如只关心 Human）
if (length(spec_keep) > 0) {
  df_mantel <- df_mantel %>% filter(spec %in% spec_keep)
}

# ---- 计算环境相关矩阵（底图） ----
env <- read.table(env_file, sep = "\t", header = TRUE, row.names = 1, check.names = FALSE)
env <- as.data.frame(env)

# linkET::correlate 返回 "correlate" 类对象，qcorrplot 可直接使用
env_cor <- correlate(env)

# ---- 绘图 ----
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
  # 6 档 size（可按需整体调大/调小）
  scale_size_manual(values = c(0.2, 0.5, 1, 1.5, 2, 3)) +
  scale_colour_manual(values = c("#fc8d59", "#feda59", "#7f7f7f", "#e6e6e6")) +
  guides(
    # 若想让 r 图例线更粗，可在 override.aes 里加 size，例如 size = 5
    size   = guide_legend(title = "Mantel's r",
                          override.aes = list(colour = "grey35"),
                          order = 2),
    # p 图例线粗细单独控制
    colour = guide_legend(title = "Mantel's p",
                          override.aes = list(size = 6),
                          order = 1),
    fill   = guide_colorbar(title = "Pearson's r", order = 3)
  ) +
  theme(axis.text.y = element_blank())

print(p1)

ggsave("p1_mantel.pdf", plot = p1, width = 10, height = 8)
if (isTRUE(save_png)) {
  ggsave("p1_mantel.png", plot = p1, width = 10, height = 8, dpi = 300)
}
cat("已保存：p1_mantel.pdf", if (isTRUE(save_png)) "、p1_mantel.png" else "", "\n", sep = "")



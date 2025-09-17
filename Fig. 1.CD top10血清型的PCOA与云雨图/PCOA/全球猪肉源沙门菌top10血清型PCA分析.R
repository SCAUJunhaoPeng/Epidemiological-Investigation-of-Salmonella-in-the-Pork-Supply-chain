# 加载必要的R包
library(ggplot2)
library(readxl)
library(dplyr)
library(ggrepel)
library(ggforce)

# ==== 新增：用于将点图层栅格化 & 更稳的PDF设备 ====
library(ggrastr)   # ★ 新增：把散点渲染成位图（减少 AI 打开时的缓存/卡顿）
library(Cairo)     # ★ 新增：cairo_pdf 导出更稳定地嵌入位图

# 读取数据
df <- read_excel("PCA_top10.xlsx", sheet = "PCA_top10")

# 提取PCA所需的数据（去除SampleID和GroupID列）
arg_data <- df %>% select(-SampleID, -GroupID)

# 去除方差为零的列
arg_data_filtered <- arg_data[, apply(arg_data, 2, var) != 0]

# 标准化数据
arg_data_scaled <- scale(arg_data_filtered)

# 进行PCA分析
pca_result <- prcomp(arg_data_scaled)

# 转换PCA结果为数据框
pca_data <- as.data.frame(pca_result$x)

# 将GroupID添加到PCA结果中
pca_data$GroupID <- df$GroupID

# 自定义10组血清型的颜色
my_color <- c(
  "Anatum"        = "#F28E2B",  # 橙色
  "Derby"         = "#EDC949",  # 黄色
  "4,[5],12:i:-"  = "#E15759",  # 深红色
  "Infantis"      = "#76B7B2",  # 青绿色
  "Johannesburg"  = "#4E79A7",  # 深蓝色
  "Typhimurium"   = "#F28D8D",  # 柔和红色
  "London"        = "#9ACFD3",  # 柔和蓝绿色
  "Uganda"        = "#BFD99A",  # 柔和橄榄绿色
  "Eko"           = "#F9D29D",  # 柔和橙色
  "Adelaide"      = "#F3CFC6"   # 柔和粉色
)

# 选择少量样本进行标记（如需）
set.seed(123)
sample_labels <- sample(1:nrow(pca_data), 10)
label_data <- pca_data[sample_labels, ]

# 绘制PCA图（仅将“点”层栅格化，其余保持矢量）
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = GroupID, fill = GroupID)) +
  geom_vline(xintercept = 0, col = "#708090", linetype = 5) +
  geom_hline(yintercept = 0, col = "#708090", linetype = 5) +
  ggforce::geom_mark_ellipse(
    aes(x = PC1, y = PC2, colour = GroupID),
    expand = unit(5, "mm"),
    alpha = 0.25,
    n = 100,
    linewidth = 0.5
  ) +
  ggforce::geom_mark_ellipse(
    data = pca_data[pca_data$GroupID == "4,[5],12:i:-", ],
    aes(x = PC1, y = PC2, colour = GroupID),
    expand = unit(5, "mm"),
    alpha = 0.05,
    n = 100,
    linewidth = 1.0
  ) +
  # ==== 修改：将矢量点改为位图点 ====
ggrastr::geom_point_rast(           # ★ 修改：替换原 geom_point()
  shape = 21, size = 2, aes(fill = GroupID),
  alpha = 0.5,
  raster.dpi = 300                 # ★ 新增：控制位图分辨率（200~600按需调整）
) +
  scale_fill_manual(values = my_color) +
  scale_color_manual(values = my_color) +
  scale_x_continuous(limits = c(-10, 40), breaks = seq(-10, 40, by = 10)) +
  scale_y_continuous(limits = c(-20, 40), breaks = seq(-20, 40, by = 10)) +
  xlab(paste0("PCA1 (", round(summary(pca_result)$importance[2, 1] * 100, 2), "%)")) +
  ylab(paste0("PCA2 (", round(summary(pca_result)$importance[2, 2] * 100, 2), "%)")) +
  labs(fill = "Group", color = "Group") +
  ggtitle("PCA analysis of ARGs in TOP10 Salmonella serotype") +
  theme_bw() +
  theme(
    plot.title = element_text(color = "#000000", hjust = 0.5, size = 20),
    panel.border = element_rect(color = "#000000", linewidth = 1),
    legend.background = element_rect(color = "#FFFFFF", linetype = 1),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15)
  )

print(pca_plot)

# ==== 修改：用 Cairo PDF 导出，嵌入位图散点 ====
ggsave("pca_plot_with_ellipse_bolded.pdf",
       plot = pca_plot,
       device = cairo_pdf,  # ★ 修改：替换 base pdf()，更稳嵌位图
       width = 10, height = 8, units = "in")



# 加载必要的库
library(ggplot2)
library(ggsignif)
library(readxl)
library(gghalves)
library(dplyr)
library(ggrastr)    # ← 新增：用于将散点栅格化
library(Cairo)      # ← 建议：更稳定的 PDF 设备（嵌入栅格层）

# 1. 读取数据
df <- read_excel("Counts.xlsx")

# 2. 计算各血清型的耐药基因均值，并按均值排序
mean_values <- df %>%
  group_by(Serovar) %>%
  summarise(Mean_Counts = mean(Counts, na.rm = TRUE)) %>%
  arrange(Mean_Counts)

# 3. 自定义血清型排序与颜色
mean_values$Serovar <- factor(mean_values$Serovar, levels = mean_values$Serovar)
df$Serovar <- factor(df$Serovar, levels = mean_values$Serovar)

my_color <- c(
  "Anatum"        = "#F28E2B",  # 橙色
  "Derby"         = "#EDC949",  # 黄色
  "4,[5],12:i:-"  = "#E15759",  # 深红色 (突出色)
  "Infantis"      = "#76B7B2",  # 青绿色
  "Johannesburg"  = "#4E79A7",  # 深蓝色
  "Typhimurium"   = "#F28D8D",  # 柔和红色
  "London"        = "#9ACFD3",  # 柔和蓝绿色
  "Uganda"        = "#BFD99A",  # 柔和橄榄绿色
  "Eko"           = "#F9D29D",  # 柔和橙色
  "Adelaide"      = "#F3CFC6"   # 柔和粉色
)

# 4. 显著性检验组合（基于排序后血清型动态生成）
my_comparisons <- list(
  c(levels(df$Serovar)[10], levels(df$Serovar)[9]),
  c(levels(df$Serovar)[10], levels(df$Serovar)[8]),
  c(levels(df$Serovar)[10], levels(df$Serovar)[7])
)

# 绘图：仅将“抖动散点层”栅格化，其余保持矢量
p <- ggplot(df, aes(x = Serovar, y = Counts)) +
  # 半小提琴图（矢量）
  geom_half_violin(aes(fill = Serovar),
                   side = 'r',
                   scale = "width",
                   position = position_nudge(x = 0.2),
                   alpha = 0.8,
                   color = "black",
                   size = 0.2) +
  # ★ 栅格化的抖动散点图（替换原 geom_jitter）
  ggrastr::geom_jitter_rast(
    aes(color = Serovar),
    size = 1.0,
    position = position_jitter(width = 0.1),
    alpha = 0.5,
    raster.dpi = 300   # ← 位图分辨率；可按需调到 200/600
  ) +
  # 箱线图（矢量）
  geom_boxplot(aes(fill = Serovar),
               width = 0.25,
               position = position_nudge(x = 0.15),
               outlier.shape = NA,
               color = "black",
               size = 0.2) +
  # 显著性标记（矢量）
  geom_signif(comparisons = my_comparisons,
              step_increase = 0.1,
              y_position = seq(26, 17, by = -4),
              map_signif_level = TRUE,
              textsize = 4.5,
              tip_length = 0.01,
              vjust = 0.5) +
  # 均值标注（矢量）
  geom_text(data = mean_values,
            aes(x = Serovar, y = Mean_Counts, label = sprintf("%.1f", Mean_Counts)),
            vjust = 1.0, size = 3.5, fontface = "bold", color = "black") +
  # 颜色&图例
  scale_fill_manual(values = my_color, name = "ST", guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = my_color, guide = "none") +
  # 翻转坐标轴
  coord_flip() +
  # Y轴
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  # 主题
  theme_classic(base_size = 16) +
  labs(x = NULL, y = "Statistical analysis of Salmonella ARGs in the TOP 10 serotypes") +
  theme(
    plot.title = element_text(size = 12, face = "bold"),  # 原注释写12，这里与注释一致
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key   = element_rect(fill = NA, color = NA),
    legend.position = "right",
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.text    = element_text(size = 14, face = "plain", color = "black"),
    axis.text.x  = element_text(face = "bold"),
    axis.line    = element_line(size = 0.5, color = "black")
  )

# 显示
print(p)

# ★ 建议用 Cairo PDF 导出（散点位图将被内嵌进 PDF）
ggsave("serovar_ARGs_violin_box_raster_points.pdf",
       plot = p,
       device = cairo_pdf,   # 需要 library(Cairo)
       width = 9, height = 6, units = "in")


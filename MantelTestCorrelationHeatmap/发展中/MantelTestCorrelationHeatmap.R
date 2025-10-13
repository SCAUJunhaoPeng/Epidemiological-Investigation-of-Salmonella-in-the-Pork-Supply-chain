# 清理工作环境中的所有对象
rm(list = ls())

# 加载所需的包
library(linkET)
library(ggplot2)
library(dplyr)
library(vegan)

# 加载 OTU 数据
df <- read.table("otu.txt", header = TRUE, row.names = 1, check.names = FALSE, sep = "\t")
# 检查数据并重新排列行
print("OTU 数据加载成功:")
print(head(df))

# 加载环境数据
env <- read.table("env.txt", sep = "\t", header = TRUE, row.names = 1, check.names = FALSE)
env <- as.data.frame(env)
print("环境数据加载成功:")
print(head(env))

# 确保列数一致
if (nrow(df) != nrow(env)) {
  stop("OTU 和环境数据样本数不一致！请检查数据格式。")
}

# Mantel 检验
df_mantel <- mantel_test(df, env,
                         spec_select = list(
                           "Human" = 1,
                           "Sweine" = 2
                         )) %>%
  mutate(
    df_r = cut(r, breaks = c(-Inf, 0.5, 0.75, 0.9, Inf),  # 根据 r 值进行分段
               labels = c("< 0.5", "0.5 - 0.75", "0.75 - 0.9", ">= 0.9")),
    df_p = cut(p, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),  # 根据 p 值进行分段
               labels = c("< 0.001","0.001 - 0.001", "0.01 - 0.05", ">= 0.05"))
  )

# 打印 Mantel 检验结果
print("Mantel 检验结果:")
print(df_mantel)

# 绘制热图（连线在右上）
p1 <- qcorrplot(correlate(env), type = "lower", diag = FALSE) +  # 计算环境数据相关性
  geom_square() +  # 绘制方块
  geom_couple(  # 添加连线
    aes(
      xend = .xend + 1.25,  # 定义连接线的结束位置
      yend = .yend + 0.5,  # 定义连接线的结束位置
      colour = df_p,  # 连接线颜色基于 p 值
      size = df_r  # 连接线大小基于 r 值
    ),
    data = df_mantel,  # 使用 Mantel 检验的结果数据
    curvature = 0.1  # 连接线的弯曲程度
  ) +
  geom_diag_label(  # 添加对角线标签
    mapping = aes(y = .y + 0.05),  # 设置标签位置
    hjust = 0.15  # 设置标签的水平对齐
  ) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdBu"))) +  # 翻转色阶，负相关为红色，正相关为蓝色 +  # 设置色阶，使用“RdBu”色调
  scale_size_manual(values = c(0.5, 1, 2, 3, 4)) +  # 调整连接线大小
  scale_colour_manual(values = c("#fc8d59", "#feda59", "#7f7f7f", "#e6e6e6")) +  # 为 p 值级别设置橙黄色系，最高显著性为深色，>= 0.05 为浅色
  guides(
    size = guide_legend(  # 设置 r 值大小的图例
      title = "Mantel's r",  # 图例标题
      override.aes = list(colour = "grey35"),  # 覆盖 aes 属性
      order = 2
    ),
    colour = guide_legend(  # 设置 p 值颜色的图例
      title = "Mantel's p",  # 图例标题
      override.aes = list(size = 3),  # 覆盖 aes 属性
      order = 1
    ),
    fill = guide_colorbar(title = "Pearson's r", order = 3)  # 设置 Pearson's r 值的色条
  ) +
  theme(
    axis.text.y = element_blank()  # 删除 y 轴文本
  )
# 显示热图
print(p1)

# 保存热图
ggsave("p1.pdf", plot = p1, width = 10, height = 8)

print("热图已成功保存为 p1.pdf")



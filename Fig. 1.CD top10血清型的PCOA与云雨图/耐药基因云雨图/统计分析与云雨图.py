import pandas as pd
import scipy.stats as stats
import seaborn as sns
import matplotlib.pyplot as plt

# 读取数据
file_path = 'Counts2.xlsx'  # 替换为你的文件路径
df = pd.read_excel(file_path)

# 计算每组均值和计数量
group_stats = df.groupby('Group').agg(
    Mean=('Counts', 'mean'),
    Count=('Counts', 'size')
).reset_index()

# 统计性分析：对每组进行两两比较，计算 p 值并记录显著性差异
grouped = df.groupby('Group')['Counts']
groups = grouped.groups.keys()

result = []
for group1 in groups:
    for group2 in groups:
        if group1 != group2:
            stat, p_value = stats.mannwhitneyu(grouped.get_group(group1), grouped.get_group(group2))
            result.append({
                'Group1': group1,
                'Group2': group2,
                'P-Value': p_value,
                'Significant': 'Yes' if p_value < 0.05 else 'No'
            })

# 合并统计结果和均值、计数量
result_df = pd.DataFrame(result)
group_stats_renamed = group_stats.rename(columns={'Mean': 'Group1_Mean', 'Count': 'Group1_Count'})
result_df = result_df.merge(group_stats_renamed, left_on='Group1', right_on='Group')
group_stats_renamed = group_stats.rename(columns={'Mean': 'Group2_Mean', 'Count': 'Group2_Count'})
result_df = result_df.merge(group_stats_renamed, left_on='Group2', right_on='Group', suffixes=('_1', '_2'))

# 保存结果为 .xlsx 文件
output_path = '全球猪肉源沙门菌ARGs01表（云雨图）result.xlsx'
result_df.to_excel(output_path, index=False, engine='openpyxl')
print(f"统计分析结果已保存为 {output_path}")

# 可视化：云雨箱线图并标注均值
plt.figure(figsize=(10, 6))

# 绘制云雨图（指定 `hue` 为 `x`，禁用图例）
sns.violinplot(x='Group', y='Counts', data=df, inner=None, palette='pastel', hue='Group', legend=False)
sns.boxplot(x='Group', y='Counts', data=df, width=0.2, palette='Set2', hue='Group', legend=False)

# 获取每组的横坐标位置
group_positions = {group: pos for pos, group in enumerate(df['Group'].unique())}

# 标注每组均值
for i, row in group_stats.iterrows():
    x_pos = group_positions[row['Group']]  # 获取组的绘图位置
    plt.text(x_pos, row['Mean'], f'{row["Mean"]:.2f}',
             color='red', ha='center', va='bottom', fontsize=10)

# 图表标题和轴标签
plt.title('Distribution of Resistance Genes by Group')
plt.ylabel('Resistance Gene Counts')
plt.xlabel('Group')

# 保存并显示图像
plot_path = 'resistance_genes01_violinplot_with_means_final.png'
plt.savefig(plot_path)
plt.show()
print(f"云雨箱线图已保存为 {plot_path}")





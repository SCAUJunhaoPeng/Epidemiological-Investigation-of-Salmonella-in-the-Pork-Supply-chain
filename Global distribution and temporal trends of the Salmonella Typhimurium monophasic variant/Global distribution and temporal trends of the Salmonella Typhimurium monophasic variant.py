import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl

# === Key settings: keep editable text (TrueType), do not convert to paths ===
mpl.rcParams['pdf.fonttype'] = 42        # Type 42 (TrueType) → editable in Illustrator
mpl.rcParams['ps.fonttype']  = 42
mpl.rcParams['svg.fonttype'] = 'none'    # keep text as text in SVG export as well
mpl.rcParams['text.usetex']  = False     # avoid TeX text rendering (which outlines text)

# === Font family: prefer common editable fonts; includes CJK fallbacks if installed ===
mpl.rcParams['font.family'] = 'sans-serif'
mpl.rcParams['font.sans-serif'] = [
    'Arial', 'Helvetica', 'DejaVu Sans',
    'Microsoft YaHei', 'SimHei', 'Noto Sans CJK SC', 'PingFang SC', 'Arial Unicode MS'
]
mpl.rcParams['axes.unicode_minus'] = False  # show minus sign properly

# === Global font sizes (tweak as needed) ===
mpl.rcParams['font.size'] = 12
mpl.rcParams['axes.labelsize'] = 14
mpl.rcParams['axes.titlesize'] = 16
mpl.rcParams['xtick.labelsize'] = 12
mpl.rcParams['ytick.labelsize'] = 12
mpl.rcParams['legend.fontsize'] = 12

# -----------------------------
# Read the Excel data
# -----------------------------
INPUT_XLSX = 'Global distribution and temporal trends of the Salmonella Typhimurium monophasic variant.xlsx'  # <- updated input file name

try:
    df = pd.read_excel(INPUT_XLSX)
except FileNotFoundError:
    print("Excel file not found. Please check the file path.")
    raise SystemExit

# Optional cleanup: standardize common typos so labels match the color dictionary
# (e.g., 'Unknow' -> 'Unknown')
if 'Serovar' in df.columns:
    df['Serovar'] = df['Serovar'].replace({'Unknow': 'Unknown'})
else:
    raise KeyError("The input Excel must contain a column named 'Serovar'.")

# -----------------------------
# Define the Top 10 (by label)
# -----------------------------
top_10_serovars = [
    'ST34',
    'ST19',
    'ST2379',
    'ST3224',
    'ST2956',
    'ST4431',
    'ST3478',
    'ST36',
    'ST5301',
    'ST313',
    'Unknown'
]

# Map any non–Top 10 serovars to 'Other'
df['Serovar'] = df['Serovar'].apply(lambda x: x if x in top_10_serovars else 'Other')

# Count how many distinct serovars are grouped into 'Other'
other_serovars = df[~df['Serovar'].isin(top_10_serovars)]['Serovar'].unique()
other_serovar_count = len(other_serovars)

# -----------------------------
# Aggregate by year and serovar
# -----------------------------
if 'Collection date' not in df.columns:
    raise KeyError("The input Excel must contain a column named 'Collection date' (year).")

df_grouped = df.groupby(['Collection date', 'Serovar']).size().unstack(fill_value=0)

# Custom column order with 'Other' last
custom_order = top_10_serovars + ['Other']
valid_order = [s for s in custom_order if s in df_grouped.columns]
df_grouped = df_grouped[valid_order]

# Normalize to percentage per year
df_grouped_percent = df_grouped.div(df_grouped.sum(axis=1), axis=0) * 100

# -----------------------------
# Color dictionary
# -----------------------------
color_dict = {
    'ST34':   '#E15759',
    'ST19':   '#9ACFD3',
    'ST2379': '#F28E2B',
    'ST3224': '#4E79A7',
    'ST2956': '#76B7B2',
    'ST4431': '#4E79A7',
    'ST3478': '#BFD99A',
    'ST36':   '#9ACFD9',
    'ST5301': '#BFD99A',
    'ST313':  '#F9D29D',
    'Unknown':'#F3CFC6',
    'Other':  '#F4F9FE'
}

# -----------------------------
# Outbreak growth point detector (for ST34)
# -----------------------------
def calculate_outbreak_point(data, threshold=1.0):
    """
    Print any years where ST34's year-over-year growth rate >= threshold.
    threshold = 1.0 means >= 100% growth over previous year.
    """
    if 'ST34' not in data.columns:
        print("ST34 not found in data. Skipping outbreak detection.")
        return

    serovar_data = data['ST34']
    years = serovar_data.index.tolist()
    counts = serovar_data.values.tolist()
    outbreak_points = []
    for i in range(1, len(counts)):
        if counts[i - 1] > 0:
            growth_rate = (counts[i] - counts[i - 1]) / counts[i - 1]
            if growth_rate >= threshold:
                outbreak_points.append((years[i], growth_rate))
    if outbreak_points:
        print("Detected outbreak growth points:")
        for year, rate in outbreak_points:
            print(f"Year: {year}, Growth rate: {rate:.2f}")
    else:
        print("No obvious outbreak growth points detected.")

calculate_outbreak_point(df_grouped, threshold=1.0)

# -----------------------------
# Plotting helper
# -----------------------------
def plot_stacked_area_chart(data, title, ylabel, filename):
    fig, ax = plt.subplots(figsize=(12, 6))
    data.plot(kind='area', stacked=True, ax=ax,
              color=[color_dict.get(col, 'lightgreen') for col in data.columns])

    # Larger, bold title/labels (still editable text in exported files)
    ax.set_title(title, fontsize=16, fontweight='bold')
    ax.set_xlabel('Year', fontsize=14, fontweight='bold')
    ax.set_ylabel(ylabel, fontsize=14, fontweight='bold')

    ax.set_xlim(1984, 2023)
    ax.set_xticks(range(1984, 2023, 5))

    # Larger legend text
    leg = ax.legend(title='Serovars', loc='upper left', bbox_to_anchor=(1, 1), frameon=False)
    if leg and leg.get_title():
        leg.get_title().set_fontsize(13)

    plt.tight_layout()
    fig.savefig(filename, bbox_inches='tight')  # keep text as text in PDF/SVG
    plt.close(fig)

# 1)

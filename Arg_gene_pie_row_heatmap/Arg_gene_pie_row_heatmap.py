import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import seaborn as sns
import numpy as np
from matplotlib.colors import LinearSegmentedColormap
from matplotlib.cm import ScalarMappable
from matplotlib import rcParams
from matplotlib.ticker import FuncFormatter

# ---------- Global style (Nature Food–like) ----------
rcParams.update({
    "font.family": "Arial",
    "axes.linewidth": 0.6,
    "axes.edgecolor": "#222222",
    "xtick.major.width": 0.6,
    "ytick.major.width": 0.6,
    "pdf.fonttype": 42,   # keep text editable in PDF
    "ps.fonttype": 42,
    "svg.fonttype": "none"
})

# ---------- IO & preprocessing ----------
def load_and_prepare_data(file_path, sheet_name='Sheet1'):
    df = pd.read_excel(file_path, sheet_name=sheet_name)

    categories = [
        'Human isolates (Developed countries)',
        'Human isolates (Developing countries)',
        'Swine isolates (Developed countries)',
        'Swine isolates (Developing countries)'
    ]
    # Four fixed category colors (adjust if needed)
    category_colors = ['#6A8DBB', '#B9CAE2', '#C85B5B', '#F0B7B0']

    df[categories] = df[categories].fillna(0).astype(float)
    return df, categories, category_colors

# ---------- Soft white→red colormap ----------
def make_nf_red_cmap(colors=None):
    """
    Default: white → soft pink → light red → mid red → deep red.
    Pass a list of hex colors (>=2) to override.
    """
    default_colors = [
        "#FFFFFF", "#F8EEEC", "#F1D7D3", "#E8B8B2",
        "#DD918A", "#CF6B63", "#B72230"
    ]
    colors = colors if (colors and len(colors) >= 2) else default_colors
    return LinearSegmentedColormap.from_list("nf_soft_red", colors)

# ---------- Main plotting ----------
def plot_gene_heatmap(
    df,
    categories,
    category_colors,
    gene_class_dict,
    class_color_dict,
    output_pdf,
    heatmap_colors=None,   # list of hex colors for the white→red scale
    show_value=True        # annotate cell values (raw numbers, no % sign)
):
    df = df.copy()
    df['Class'] = df['Gene'].map(gene_class_dict)
    df.sort_values(by=['Class', 'Gene'], inplace=True, kind='mergesort')

    # Heatmap scale expects percentages 0–100
    custom_cmap = make_nf_red_cmap(heatmap_colors)
    vmin, vmax = 0.0, 100.0

    n_rows = len(df)
    fig = plt.figure(figsize=(12, max(4, n_rows * 0.40)))
    grid = plt.GridSpec(n_rows, 4, width_ratios=[1.3, 2.6, 2.2, 0.15], wspace=0.05, hspace=0.30)

    # Row-wise: (1) gene label, (2) pie chart (4 categories), (3) 1×4 heat row
    for i, (_, row) in enumerate(df.iterrows()):
        # (1) Gene name colored by class
        ax_text = plt.subplot(grid[i, 0])
        gene_color = class_color_dict.get(row['Class'], '#222222')
        ax_text.text(0.5, 0.5, row['Gene'], ha='center', va='center',
                     fontsize=8.5, color=gene_color)
        ax_text.set_axis_off()

        # (2) Pie chart for four categories
        ax_pie = plt.subplot(grid[i, 1])
        pie_data = row[categories].values.astype(float)
        if np.allclose(pie_data.sum(), 0):
            pie_data = np.array([1, 0, 0, 0], dtype=float)  # avoid all-zero error
        ax_pie.pie(
            pie_data,
            colors=category_colors,
            startangle=90,
            radius=1.0,
            wedgeprops=dict(linewidth=0.4, edgecolor="white")
        )
        ax_pie.axis('equal')
        ax_pie.set_axis_off()

        # (3) Single-row heatmap (0–100)
        ax_heat = plt.subplot(grid[i, 2])
        heat_data = row[categories].values.reshape(1, -1).astype(float)
        heat_data = np.clip(heat_data, vmin, vmax)
        annot = np.round(heat_data, 2) if show_value else None

        sns.heatmap(
            heat_data,
            cmap=custom_cmap,
            vmin=vmin, vmax=vmax,
            cbar=False,
            annot=annot,
            fmt='.2f' if show_value else '',
            linewidths=0.4, linecolor='#DDDDDD',
            xticklabels=False, yticklabels=[], ax=ax_heat,
            annot_kws={"fontsize": 7, "color": "#1f1f1f"}
        )
        for spine in ax_heat.spines.values():
            spine.set_visible(True)
            spine.set_linewidth(0.6)
            spine.set_edgecolor("#DDDDDD")
        ax_heat.tick_params(axis='x', length=0)

    # Pie legend (top-left)
    pie_legend_ax = fig.add_axes([0.02, 0.92, 0.35, 0.06])
    pie_legend_ax.axis('off')
    patches_cat = [mpatches.Patch(color=col, label=cat) for col, cat in zip(category_colors, categories)]
    pie_legend_ax.legend(
        handles=patches_cat, title='Category',
        fontsize=8, title_fontsize=8, loc='upper left', frameon=False, ncol=1
    )

    # Class legend (bottom-left)
    gene_legend_ax = fig.add_axes([0.02, 0.02, 0.40, 0.20])
    gene_legend_ax.axis('off')
    patches_cls = [mpatches.Patch(color=col, label=cls) for cls, col in class_color_dict.items()]
    gene_legend_ax.legend(
        handles=patches_cls, title='Antibiotic class',
        fontsize=8, title_fontsize=8, loc='lower left', frameon=False, ncol=2
    )

    # Unified colorbar (right): ticks 0%, 50%, 100%
    cax = plt.subplot(grid[:, 3])
    norm = plt.Normalize(vmin=vmin, vmax=vmax)
    sm = ScalarMappable(cmap=custom_cmap, norm=norm); sm.set_array([])
    cb = plt.colorbar(sm, cax=cax)
    cb.outline.set_linewidth(0.6)
    cb.set_ticks([0, 50, 100])
    cb.ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{int(v)}%"))
    cb.ax.tick_params(labelsize=8)

    plt.tight_layout()
    plt.savefig(output_pdf, format='pdf', bbox_inches='tight')
    plt.show()
    print(f"✅ Generated: {output_pdf}")

# ---------- Entrypoint ----------
if __name__ == "__main__":
    # Input workbook
    file_path = 'Human_Swine_WGS_heatmap.xlsx'

    # Load
    df, categories, category_colors = load_and_prepare_data(file_path)

    # Gene → class mapping
    gene_class_dict = {
        'aac(3)-IId': 'Aminoglycosides', 'aac(3)-IVa': 'Aminoglycosides', "aac(6')-Ib-cr": 'Aminoglycosides',
        'aadA16': 'Aminoglycosides', 'aadA2': 'Aminoglycosides', "ant(3'')-Ia": 'Aminoglycosides',
        "aph(3')-Ia": 'Aminoglycosides', "aph(3'')-Ib": 'Aminoglycosides', 'aph(4)-Ia': 'Aminoglycosides',
        'aph(6)-Id': 'Aminoglycosides', 'blaOXA-1': 'Beta-lactams', 'blaTEM-1B': 'Beta-lactams',
        'catA1': 'Phenicols', 'catA2': 'Phenicols', 'cmlA1': 'Phenicols', 'floR': 'Phenicols',
        'dfrA12': 'Trimethoprim', 'dfrA27': 'Trimethoprim', 'fosA3': 'Fosfomycin',
        'lnu(F)': 'Lincosamides', 'mcr-1.1': 'Polymyxins', 'mcr-3.1': 'Polymyxins', 'mcr-9': 'Polymyxins',
        'mph(A)': 'Macrolides', 'oqxA': 'Quinolones', 'oqxB': 'Quinolones', 'qnrS1': 'Quinolones', 'qnrS2': 'Quinolones',
        'sul1': 'Sulfonamides', 'sul2': 'Sulfonamides', 'sul3': 'Sulfonamides',
        'tet(A)': 'Tetracyclines', 'tet(D)': 'Tetracyclines', 'tet(M)': 'Tetracyclines', 'tet(X)': 'Tetracyclines',
        'ARR-3': 'Rifampin'
    }

    # Class colors
    class_color_dict = {
        'Aminoglycosides': '#3A6AA3', 'Beta-lactams': '#D87A2C', 'Phenicols': '#3B8E4A',
        'Trimethoprim': '#C54B4B', 'Fosfomycin': '#7E67B3', 'Lincosamides': '#8F6E63',
        'Polymyxins': '#C166A1', 'Macrolides': '#6E6E6E', 'Quinolones': '#A5A63A',
        'Sulfonamides': '#1BA6B6', 'Tetracyclines': '#8CAFD8', 'Rifampin': '#E48B8B'
    }

    output_pdf = 'Gene_Combined_Heatmap_ClassColor_NF_percent.pdf'

    custom_heatmap_colors = [
        "#FFFFFF", "#F8EEEC", "#F1D7D3", "#E8B8B2", "#DD918A", "#CF6B63", "#B72230"
    ]

    plot_gene_heatmap(
        df, categories, category_colors,
        gene_class_dict, class_color_dict,
        output_pdf,
        heatmap_colors=custom_heatmap_colors,
        show_value=True
    )


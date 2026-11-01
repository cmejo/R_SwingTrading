#!/usr/bin/env python3
"""
scratch/plot_feature_importance_stacked_bar.py
Generates publication-quality stacked bar time series of ElasticNet feature importance.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick

def main():
    df = pd.read_csv("output/walkforward_feature_importance.csv")
    summary_df = pd.read_csv("output/walkforward_model_epochs_summary.csv")
    
    epochs = summary_df["Epoch"].tolist()
    
    # Pivot features to matrix: Epoch x Feature (Relative Weight %)
    pivot_df = df.pivot(index="Epoch", columns="Feature", values="RelWeightPct").fillna(0.0)
    # Ensure correct chronological order of epochs
    pivot_df = pivot_df.reindex(epochs)
    
    # Also pivot categories: Epoch x Category (Relative Weight %)
    cat_df = df.groupby(["Epoch", "Category"])["RelWeightPct"].sum().unstack(fill_value=0.0)
    cat_df = cat_df.reindex(epochs)
    
    # Order features by overall total importance
    feat_totals = pivot_df.sum().sort_values(ascending=False)
    # Keep top 10 features, group rest into "Other Features"
    top_10_feats = feat_totals.head(10).index.tolist()
    
    plot_pivot = pivot_df[top_10_feats].copy()
    plot_pivot["Other Features"] = pivot_df.drop(columns=top_10_feats).sum(axis=1)
    
    # Color palette
    colors_top10 = [
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#4b0082"
    ]
    
    category_colors = {
        "Microstructure_Demand": "#2b5c8f",
        "Volatility_Squeeze":    "#d95f02",
        "Trend_Quality":         "#7570b3",
        "Macro_Volatility":      "#e7298a",
        "Alpha_RelStrength":     "#1b9e77",
        "Other":                 "#66a61e"
    }

    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(16, 12), gridspec_kw={'height_ratios': [3, 2, 1.2]}, sharex=True)
    plt.subplots_adjust(hspace=0.15)
    
    # -------------------------------------------------------------
    # Plot 1: Top 10 Features Relative Importance Stacked Bar
    # -------------------------------------------------------------
    plot_pivot.plot(kind="bar", stacked=True, ax=ax1, color=colors_top10, width=0.75, edgecolor="black", linewidth=0.5)
    ax1.set_title("Walk-Forward ElasticNet Feature Importance Over Time (10-Year Retraining Epochs)", fontsize=14, fontweight="bold", pad=12)
    ax1.set_ylabel("Share of Model Importance (%)", fontsize=11, fontweight="bold")
    ax1.yaxis.set_major_formatter(mtick.PercentFormatter(100))
    ax1.set_ylim(0, 100)
    ax1.grid(axis="y", linestyle="--", alpha=0.5)
    ax1.legend(loc="upper left", bbox_to_anchor=(1.01, 1.0), title="Predictor Feature", fontsize=9, title_fontsize=10)
    
    # Annotate epochs with zero active features if any
    for idx, row in summary_df.iterrows():
        if row["ActiveFeatures"] == 0:
            ax1.text(idx, 50, "SPARSE INTERCEPT\n(Zero Non-Zero Coefs)", ha="center", va="center",
                     fontsize=7, fontweight="bold", color="darkred", bbox=dict(boxstyle="round,pad=0.2", facecolor="yellow", alpha=0.6))

    # -------------------------------------------------------------
    # Plot 2: Category Breakdown Stacked Bar
    # -------------------------------------------------------------
    ordered_cats = [c for c in category_colors.keys() if c in cat_df.columns]
    cat_df[ordered_cats].plot(kind="bar", stacked=True, ax=ax2,
                              color=[category_colors[c] for c in ordered_cats],
                              width=0.75, edgecolor="black", linewidth=0.5)
    ax2.set_ylabel("Category Share (%)", fontsize=11, fontweight="bold")
    ax2.yaxis.set_major_formatter(mtick.PercentFormatter(100))
    ax2.set_ylim(0, 100)
    ax2.grid(axis="y", linestyle="--", alpha=0.5)
    ax2.legend(loc="upper left", bbox_to_anchor=(1.01, 1.0), title="Feature Category", fontsize=9, title_fontsize=10)

    # -------------------------------------------------------------
    # Plot 3: Active Features Count & Selected Lambda
    # -------------------------------------------------------------
    x_pos = np.arange(len(epochs))
    bars = ax3.bar(x_pos - 0.15, summary_df["ActiveFeatures"], width=0.3, color="#2c7bb6", label="Active Non-Zero Features", edgecolor="black", linewidth=0.5)
    ax3.set_ylabel("Active Features", fontsize=10, fontweight="bold", color="#2c7bb6")
    ax3.set_ylim(0, 26)
    ax3.grid(axis="y", linestyle=":", alpha=0.5)
    
    ax3_twin = ax3.twinx()
    line = ax3_twin.plot(x_pos + 0.15, summary_df["BestLambda"], color="#d7191c", marker="o", linewidth=1.8, markersize=5, label="Optimal Lambda")
    ax3_twin.set_ylabel("Regularization Lambda", fontsize=10, fontweight="bold", color="#d7191c")
    ax3_twin.grid(False)
    
    # Combined legend for panel 3
    lines_labels = [bars, line[0]]
    ax3.legend(lines_labels, [b.get_label() for b in lines_labels], loc="upper left", bbox_to_anchor=(1.01, 1.0), fontsize=9)
    
    ax3.set_xticks(x_pos)
    ax3.set_xticklabels(epochs, rotation=45, ha="right", fontsize=9, fontweight="bold")
    ax3.set_xlabel("Walk-Forward Retraining Epoch", fontsize=11, fontweight="bold")
    
    output_path = "output/walkforward_feature_importance_stacked_bar.png"
    plt.tight_layout()
    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    print(f"[Success] Generated stacked bar time series chart at: {output_path}")

if __name__ == "__main__":
    main()

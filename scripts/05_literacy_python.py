"""Question 5: recompute the band table and redraw the Question 3 figure in Python.
Run from the repository root:  python scripts/05_literacy_python.py
"""
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

person = pd.read_csv("data/pslm_person_subset.csv")
hh = pd.read_csv("data/pslm_hh_subset.csv")

person = person.merge(
    hh[["hhid", "prov_code", "urban_rural", "hhweight"]],
    on="hhid", how="left"
)

# Literacy sample: rural Punjab (variant 0), ages 10-64, literate not missing
lit = person[
    (person["prov_code"] == 2) & (person["urban_rural"] == 1) &
    (person["age"] >= 10) & (person["age"] <= 64) &
    (person["literate"].notna())
].copy()

lit["age_band"] = (lit["age"] // 5) * 5            # 10, 15, ..., 60 (left-closed bands)
lit["sex_label"] = lit["sex"].map({1: "Men", 2: "Women"})
lit["lw"] = lit["literate"] * lit["hhweight"]

g = lit.groupby(["age_band", "sex_label"]).agg(
    n=("literate", "size"), lw=("lw", "sum"), w=("hhweight", "sum")
).reset_index()
g["literacy_rate"] = g["lw"] / g["w"] * 100         # weighted mean x 100
g["age_mid"] = g["age_band"] + 2
g["thin"] = g["n"] < 30

table = g[["age_band", "sex_label", "n", "literacy_rate"]].copy()
table["literacy_rate"] = table["literacy_rate"].round(1)
print(table.to_string(index=False))

wide = g.pivot(index="age_band", columns="sex_label", values="literacy_rate")
wide["gap"] = (wide["Men"] - wide["Women"]).abs()
gb = int(wide["gap"].idxmax())
print("Largest gap band:", gb, "-", gb + 4, round(wide.loc[gb, "gap"], 1), "pp")
print("Women 15-19:", round(wide.loc[15, "Women"], 1))

# Figure (matches the R redesign)
colors = {"Men": "#737373", "Women": "#287A55"}
fig, ax = plt.subplots(figsize=(10, 6))
for sex in ["Men", "Women"]:
    d = g[g["sex_label"] == sex].sort_values("age_mid")
    ax.plot(d["age_mid"], d["literacy_rate"], color=colors[sex], linewidth=2)
    solid, hollow = d[~d["thin"]], d[d["thin"]]
    ax.scatter(solid["age_mid"], solid["literacy_rate"], color=colors[sex], s=40, zorder=3)
    ax.scatter(hollow["age_mid"], hollow["literacy_rate"], facecolors="white",
               edgecolors=colors[sex], s=40, zorder=3)
    last = d.iloc[-1]
    ax.text(last["age_mid"] + 0.8, last["literacy_rate"], sex, color=colors[sex], va="center")

gap_x = gb + 2
ax.plot([gap_x, gap_x], [wide.loc[gb, "Women"], wide.loc[gb, "Men"]], color="#595959", linestyle="--")
ax.text(gap_x, min(95, wide.loc[gb, "Men"] + 18),
        f"Largest gap: {gb}\u2013{gb + 4} ({wide.loc[gb, 'gap']:.1f} pp)",
        ha="center", color="#404040", fontsize=9)

ax.set_xticks(range(12, 63, 5))
ax.set_xticklabels([f"{b}\u2013{b + 4}" for b in range(10, 61, 5)])
ax.set_xlim(9, 68)
ax.set_ylim(0, 100)
ax.set_xlabel("Age band")
ax.set_ylabel("Weighted literacy rate (%)")
ax.set_title("The literacy gap between men and women widens with age in rural Punjab",
             loc="left", fontweight="bold")
for side in ["top", "right"]:
    ax.spines[side].set_visible(False)
fig.text(0.01, 0.01,
         "Source: PSLM person and household data. Rural Punjab residents aged 10\u201364 with reported "
         "literacy status; weighted by household sampling weights.\n"
         "Hollow markers indicate fewer than 30 unweighted observations.",
         fontsize=8, ha="left", va="bottom")
fig.subplots_adjust(bottom=0.17)
plt.savefig("figures/05_literacy_python.svg", format="svg")

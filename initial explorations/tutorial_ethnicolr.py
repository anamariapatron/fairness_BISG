"""
Ethnicolr documentation: https://pypi.org/project/ethnicolr/
"""
import pandas as pd
from ethnicolr import pred_fl_reg_ln_five_cat
from ethnicolr import pred_fl_reg_name

from metrics import run_metrics

# TODO: set path to output directory
OUTPUT = r"C:\Documents"

# TODO: set path to tutorial data
data = pd.read_csv(
    r"tutorial_20231030.csv",
    dtype={"ZCTA": str},
)
data["ZCTA"] = data["ZCTA"].astype(str).str.zfill(5)

last_names = data[["last_name"]].drop_duplicates()
full_names = data[["first_name", "last_name"]].drop_duplicates()

# -----------------------------------------------
#  Full Name Florida Voter Registration model
# -----------------------------------------------
full_name_fl_results = pred_fl_reg_name(
    full_names, "last_name", "first_name", num_iter=100, conf_int=1.0
)

data_full_name_fl = data.merge(
    full_name_fl_results.drop(columns=["race"]),
    how="left",
    on=["last_name", "first_name"],
)

# this model doesn't predict probabilities for the "Other" cohort. Therefore,
# we will exclude it from the metrics
run_metrics(
    data_full_name_fl,
    "race",
    "full_name_fl",
    ["nh_white", "nh_black", "asian", "hispanic"],
    OUTPUT,
)

# -----------------------------------------------
#  Last Name Florida Voter Registration model
# -----------------------------------------------
last_name_fl_5cat_results = pred_fl_reg_ln_five_cat(
    last_names, "last_name", num_iter=100, conf_int=1.0
)

data_last_name_fl_5cat = data.merge(
    last_name_fl_5cat_results.drop(columns=["race"]), how="left", on=["last_name"]
)

run_metrics(
    data_last_name_fl_5cat,
    "race",
    "last_name_fl_5cat",
    ["nh_white", "nh_black", "asian", "hispanic", "other"],
    OUTPUT,
)

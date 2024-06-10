"""
Surgeo documentation: https://surgeo.readthedocs.io/en/dev/
"""
import pandas as pd
import surgeo

from metrics import run_metrics

# TODO: set path to output directory
OUTPUT = r"C:\Documents"

# TODO: set path to tutorial data
data = pd.read_csv(
    r"tutorial_20231030.csv",
    dtype={"ZCTA": str},
)
data["ZCTA"] = data["ZCTA"].astype(str).str.zfill(5)

# ----------------
# BISG
# ----------------
bisg_model = surgeo.SurgeoModel()
bisg_results = bisg_model.get_probabilities(data["last_name"], data["ZCTA"])

bisg_results = bisg_results.rename(columns={"name": "last_name", "zcta5": "ZCTA"})

bisg_results["last_name"] = bisg_results["last_name"].str.capitalize()

bisg_results["ZCTA"] = bisg_results["ZCTA"].astype(str).str.zfill(5)

data_bisg = data.merge(
    bisg_results.drop_duplicates(), how="left", on=["last_name", "ZCTA"]
)

# NOTE: for the purpose of this tutorial, we will drop records
# with missing predicted probabilities
data_bisg_na_records = data_bisg[data_bisg.isna().any(axis=1)]
data_bisg = data_bisg.dropna()

# rename columns to match race cohorts
data_bisg = data_bisg.rename(
    columns={"white": "nh_white", "black": "nh_black", "api": "asian"}
)

# the tutorial data does include the native and multiple cohorts. Therefore,
# we will exclude them from the run_metrics function
run_metrics(
    data_bisg,
    "race",
    "BISG",
    ["nh_white", "nh_black", "asian", "hispanic"],
    OUTPUT,
)

# ----------------
# BIFSG
# ----------------
bifsg_model = surgeo.BIFSGModel()
bifsg_results = bifsg_model.get_probabilities(
    data["first_name"], data["last_name"], data["ZCTA"]
)

bifsg_results = bifsg_results.rename(columns={"surname": "last_name", "zcta5": "ZCTA"})

bifsg_results["last_name"] = bifsg_results["last_name"].str.capitalize()
bifsg_results["first_name"] = bifsg_results["first_name"].str.capitalize()

bifsg_results["ZCTA"] = bifsg_results["ZCTA"].astype(str).str.zfill(5)

# merge bifsg_results on to data using last_name and ZCTA
data_bifsg = data.merge(
    bifsg_results.drop_duplicates(), how="left", on=["first_name", "last_name", "ZCTA"]
).dropna()

# NOTE: for the purpose of this tutorial, we will drop records with
# missing predicted probabilities
data_bifsg_na_records = data_bifsg[data_bifsg.isna().any(axis=1)]
data_bifsg = data_bifsg.dropna()

# rename columns to match race cohorts
data_bifsg = data_bifsg.rename(
    columns={"white": "nh_white", "black": "nh_black", "api": "asian"}
)

# the tutorial data does include the native and multiple cohorts. Therefore,
# we will exclude them from the run_metrics function
run_metrics(
    data_bifsg,
    "race",
    "BIFSG",
    ["nh_white", "nh_black", "asian", "hispanic"],
    OUTPUT,
)

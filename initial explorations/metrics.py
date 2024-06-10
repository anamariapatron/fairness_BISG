"""
Metrics functions
"""
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sklearn.metrics
from sklearn.calibration import calibration_curve
from sklearn.metrics import class_likelihood_ratios
from sklearn.metrics import confusion_matrix
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score

plt.rcParams.update({"figure.max_open_warning": 0})


def get_auc_values(df, cohorts):
    """
    Calculate the AUC of a binary classifier for each cohort.
    Args:
        - df (pd.DataFrame): data frame with columns "{cohort}_ind", each
        containing the predicted values of the binary classifier for that cohort
        - cohorts (list): list of strings giving cohorts to evaluate
    Returns: data frame containing the AUC for each cohort
    """

    auc_metrics = {}
    for cohort in cohorts:
        fpr, tpr, _ = sklearn.metrics.roc_curve(
            y_true=df[f"{cohort}_ind"], y_score=df[cohort], pos_label=1
        )  # positive class is 1; negative class is 0
        auc = sklearn.metrics.auc(fpr, tpr)

        auc_metrics[cohort] = auc

    auc_metrics = pd.DataFrame.from_dict(auc_metrics, orient="index", columns=["auc"])
    return auc_metrics


def get_prob_race_given_race(df, race_col, cohorts):
    """
    Compute the average predicted probability of belonging to a given cohort
    among individuals who self-report belonging to that cohort.
    Args:
        - df (pd.DataFrame): data frame with columns "{cohort}", each containing
        predicted probabilities of belonging to that cohort; and a column
        race_col containing self-reported races
        - race_col (str): column name for self-reported race
        - cohorts (list): list of strings giving cohorts to evaluate
    Returns: data frame containing the average predicted probability of
    belonging to a cohort among self-reported membes of that cohort
    """
    avg_probs = {}
    for cohort in cohorts:
        subset = df[df[race_col] == cohort].copy()

        avg_prob = subset[cohort].mean()

        avg_probs[cohort] = avg_prob

    avg_probs = pd.DataFrame.from_dict(avg_probs, orient="index", columns=["avg prob"])

    return avg_probs


def get_prob_white_given_race(df, race_col, white_prob, cohorts):
    """
    Compute the average predicted probability of belonging to the white cohort
    among individuals who self-report belonging to a particular cohort. For
    members of the white cohort, this should produce the same result as
    get_prob_race_given_race.
    Args:
        - df (pd.DataFrame): data frame with a column race_col containing self-
        reported races; and a column white_prob containing predicted
        probabilities of belonging to the white cohort
        - race_col (str): column name for self-reported race
        - white_prob (str): column name for predicted probability of being white
        - cohorts (list): list of strings giving cohorts to evaluate
    Returns: data frame containing the average predicted probability of
    belonging to the white cohort among self-reported members of each cohort
    """
    avg_white_probs = {}
    for cohort in cohorts:
        subset = df[df[race_col] == cohort].copy()

        avg_white_prob = subset[white_prob].mean()

        avg_white_probs[cohort] = avg_white_prob

    avg_white_probs = pd.DataFrame.from_dict(
        avg_white_probs, orient="index", columns=["avg prob"]
    )
    return avg_white_probs


def plot_histograms(df, cohorts, method, outpath):
    """
    Plot histograms of predicted probabilities for each cohort.
    Args:
        - df (pd.DataFrame): data frame with columns "{cohort}", each containing
        predicted probabilities of belonging to that cohort
        - cohorts (list): list of strings giving cohorts to evaluate
        - method (str): name of method that was used to predict probabilities;
        used for labeling png files
        - outpath (str): file path to directory where png should be saved
    Returns: None, but writes png files
    """
    title_labels = {
        "nh_white": "White",
        "nh_black": "Black",
        "asian": "Asian",
        "native": "Native",
        "multiple": "Multiple",
        "hispanic": "Hispanic",
        "other": "Other",
    }

    for cohort in cohorts:
        try:
            plt.figure(figsize=(8, 6))
            plt.hist(
                df[cohort],
                bins=10,
                alpha=0.5,
                color="#50bee1",
                edgecolor="black",
                linewidth=0.7,
            )
            plt.title(f"{title_labels[cohort]}")
            plt.xlabel("Predicted Probability")
            plt.ylabel("Frequency")
            plt.grid(axis="y", linestyle="--", alpha=0.7)
            plt.savefig(
                os.path.join(
                    outpath, f"{method}/probs_distribution_{method}_{cohort}.png"
                ),
                bbox_inches="tight",
            )
        except KeyError:
            print(f"Cohort {cohort} not found in dataframe when plotting histogram")
            pass


def run_metrics(data, self_rep_race, method, cohorts, outpath):
    """
    Run all metrics on a data frame of predictions produced by a given method.
    Args:
        - data (pd.DataFrame): data frame with columns containing predicted
        probabilities for race cohorts, one of which must be "nh_white"; a
        column self_rep_race containing the self-reported race for each
        individual; and a column "race_pred" containing the predicted race
        cohort for each individual
        - self_rep_race (str): column name for self-reported race
        - method (str): name of method that was used to predict probabilities;
        used for labeling files
        - cohorts (list): list of strings giving names of columns of data
        containing predicted probabilities for each race cohort
        - outpath (path): file path to directory where files should be saved
    Returns: None, but writes files
    """
    os.makedirs(
        os.path.join(outpath, f"{method}"),
        exist_ok=True,
    )
    for cohort in cohorts:
        data[f"{cohort}_ind"] = np.where(data[self_rep_race] == cohort, 1, 0)

    # AUC
    auc_metrics = get_auc_values(
        data,
        cohorts,
    )
    auc_metrics.to_csv(os.path.join(outpath, f"{method}/auc_{method}.csv"))

    # given you are of a specific cohort, what's the
    # avg probability of being assigned that cohort
    prob_race_given_race = get_prob_race_given_race(
        data,
        self_rep_race,
        cohorts,
    )
    prob_race_given_race.to_csv(
        os.path.join(outpath, f"{method}/prob_race_given_race_{method}.csv")
    )

    # given you are of a specific cohort, what's the avg
    # probability of being assigned white
    avg_white_probs = get_prob_white_given_race(
        data,
        self_rep_race,
        "nh_white",
        cohorts,
    )
    avg_white_probs.to_csv(
        os.path.join(outpath, f"{method}/prob_white_given_race_{method}.csv")
    )

    # create a prediction classifier based on highest probability
    data["race_pred"] = data[cohorts].idxmax(axis=1)

    # precision: What proportion of positive identifications was correct?
    precisions = precision_score(
        data[self_rep_race],
        data["race_pred"],
        average=None,
        labels=cohorts,
        zero_division=np.nan,
    )
    precisions = {label: precision for label, precision in zip(cohorts, precisions)}
    pd.DataFrame.from_dict(precisions, orient="index", columns=["precision"]).to_csv(
        os.path.join(outpath, f"{method}/precision_{method}.csv")
    )

    # recall: What proportion of actual positives was identified correctly?
    recalls = recall_score(
        data[self_rep_race],
        data["race_pred"],
        average=None,
        labels=cohorts,
        zero_division=np.nan,
    )
    recalls = {label: recall for label, recall in zip(cohorts, recalls)}
    pd.DataFrame.from_dict(recalls, orient="index", columns=["recall"]).to_csv(
        os.path.join(outpath, f"{method}/recall_{method}.csv")
    )

    # specificity
    spec_metrics = {}
    for cohort in cohorts:
        try:
            data[f"{cohort}_pred_ind"] = np.where(data["race_pred"] == cohort, 1, 0)

            tn, fp, fn, tp = confusion_matrix(
                data[f"{cohort}_ind"], data[f"{cohort}_pred_ind"]
            ).ravel()

            specificity = tn / (tn + fp)
            spec_metrics[cohort] = specificity
        except ValueError:
            spec_metrics[cohort] = np.nan
    pd.DataFrame.from_dict(
        spec_metrics, orient="index", columns=["specificity"]
    ).to_csv(os.path.join(outpath, f"{method}/specificity_{method}.csv"))

    # positive likelihood ratios
    pl_ratios = {}
    for cohort in cohorts:
        plr = class_likelihood_ratios(data[f"{cohort}_ind"], data[f"{cohort}_pred_ind"])

        pl_ratios[cohort] = plr
    pd.DataFrame.from_dict(pl_ratios, orient="index", columns=["LR+", "LR-"]).to_csv(
        os.path.join(outpath, f"{method}/likelihood_ratios_{method}.csv")
    )

    # calibration curve
    for strategy in ["quantile", "uniform"]:
        calibration_values = {}
        for cohort in cohorts:
            prob_true, prob_pred = calibration_curve(
                data[f"{cohort}_ind"],
                data[cohort],
                n_bins=10,
                strategy=strategy,
            )

            calibration_values[cohort] = {
                "prob_true": prob_true,
                "prob_pred": prob_pred,
            }

        # Create the calibration curve plot for each cohort
        # in the same plot with different colors and markers
        plt.figure(figsize=(8, 6))
        plt.plot(
            calibration_values["nh_white"]["prob_pred"],
            calibration_values["nh_white"]["prob_true"],
            marker="o",
            label="White",
            linestyle="-",
        )
        plt.plot(
            calibration_values["nh_black"]["prob_pred"],
            calibration_values["nh_black"]["prob_true"],
            marker="^",
            label="Black",
            linestyle="-",
        )
        plt.plot(
            calibration_values["hispanic"]["prob_pred"],
            calibration_values["hispanic"]["prob_true"],
            marker="s",
            label="Hispanic",
            linestyle="-",
        )
        plt.plot(
            calibration_values["asian"]["prob_pred"],
            calibration_values["asian"]["prob_true"],
            marker="*",
            label="Asian and Pacific Islander",
            linestyle="-",
        )
        if "other" in cohorts:
            plt.plot(
                calibration_values["other"]["prob_pred"],
                calibration_values["other"]["prob_true"],
                marker="x",
                label="Other",
                linestyle="-",
            )
        plt.plot(
            [0, 1], [0, 1], linestyle="--", color="gray", label="Perfectly Calibrated"
        )
        plt.xlabel("Mean Predicted Probability")
        plt.ylabel("Fraction of Positives")
        plt.title("Calibration Curve")
        plt.legend()
        plt.grid()
        plt.savefig(
            os.path.join(
                outpath, f"{method}/calibration_curve_{method}_{strategy}.png"
            ),
            bbox_inches="tight",
        )
        plt.close()

        plot_histograms(
            data,
            cohorts,
            method,
            outpath,
        )
        plt.close("all")

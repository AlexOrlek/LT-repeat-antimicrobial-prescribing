from cohortextractor import Measure, StudyDefinition, patients, codelist
from codelists import *
import pandas as pd


# dictionary of STPs
STPs = pd.read_csv(filepath_or_buffer="input-data/STPs.csv")
dict_stp = {stp: 1 / len(STPs.index) for stp in STPs["stp_id"].tolist()}


study = StudyDefinition(
    index_date = "2022-01-01",
    # configure expectations framework: event dates expected to be distributed between 2022-01-01 and 2022-04-01, with uniform frequency, with events occurring for 90% patients
    default_expectations={
            "date": {"earliest": "index_date", "latest": "index_date + 3 months"},
            "rate": "uniform",
            "incidence": 0.9,
    },
        
    # STUDY POPULATION
    population = patients.satisfying(
        """
        registered AND
        (NOT died) AND
        (age >=18 AND age <=120) AND 
        (sex = 'M' OR sex = 'F')
        """,

        registered = patients.registered_as_of(
            "index_date",
            return_expectations={"incidence": 0.9},
        ),

        died = patients.died_from_any_cause(
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.1}
        ),
    ),

    # VARIABLES
    # demographics
    age = patients.age_as_of(
        "index_date",
        return_expectations = {
            "int": {"distribution": "population_ages"}
        }
    ),

    sex = patients.sex(
        return_expectations = {
            "category": {"ratios": {"M": 0.49, "F": 0.51}}
        }
    ),

    # stp = patients.registered_practice_as_of(
    #     "index_date",
    #     returning = "stp_code",
    #     return_expectations={
    #         "category": {"ratios": dict_stp},
    #     },
    # ),

     # antibiotic prescribing
    amr_6_months = patients.with_these_medications(
        antibacterial_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),

    # amr_6_months_first_match = patients.with_these_medications(
    #     antibacterial_codes,
    #     between = ["index_date - 6 months", "index_date"],
    #     find_first_match_in_period = True,
    #     date_format="YYYY-MM-DD",
    #     returning = "date",
    #     return_expectations = {
    #         "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
    #     },
    # ),

    repeat_amr = patients.satisfying(
        """
        (amr_6_months >= 3)
        """,
    ),

    non_repeat_amr = patients.satisfying(
        """
        (amr_6_months < 3)
        """,
    ),

)

# MEASURES
# proportion of population receiving repeat antibiotic prescriptions
measures = [
    Measure(
        id = "repeat_prescribing",
        numerator = "repeat_amr",
        denominator = "population",
        group_by = "population"
    ),
    Measure(
        id = "non_repeat_prescribing",
        numerator = "non_repeat_amr",
        denominator = "population",
        group_by = "population"
    ),
    # Measure(
    #     id = "prescribing_stp",
    #     numerator = "amr_6_months_binary",
    #     denominator = "population",
    #     group_by = "stp",
    # ),
]

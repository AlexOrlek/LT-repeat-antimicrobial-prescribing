from cohortextractor import StudyDefinition, patients, codelist
from codelists import *
import pandas as pd


# dictionary of STPs
STPs = pd.read_csv(filepath_or_buffer="input-data/STPs.csv")
dict_stp = {stp: 1 / len(STPs.index) for stp in STPs["stp_id"].tolist()}


# create variable for each code in antibacterial codelist
def loop_over_codes(code_list):

    def make_variable(code):
        return {
            f"count_{code}": (
                patients.with_these_clinical_events(
                    codelist([code], system="snomed"),
                    between = ["index_date - 6 months", "index_date"],
                    returning = "number_of_episodes",
                    episode_defined_as = "series of events each <= 28 days apart",
                    return_expectations={
                        "int": {"distribution": "normal", "mean": 2, "stddev": 1},
                        "incidence": 0.2,
                    },
                )
            )
        }

    variables = {}
    for code in code_list:
        variables.update(make_variable(code))
    return variables



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

    region = patients.registered_practice_as_of(
        "index_date",
        returning = "nuts1_region_name",
        return_expectations = {
            "category": {"ratios": {
                "North East": 0.1,
                "North West": 0.1,
                "Yorkshire and the Humber": 0.1,
                "East Midlands": 0.1,
                "West Midlands": 0.1,
                "East of England": 0.1,
                "London": 0.2,
                "South East": 0.2}
            }
        }
    ),

    stp = patients.registered_practice_as_of(
        "index_date",
        returning = "stp_code",
        return_expectations={
            "category": {"ratios": dict_stp},
        },
    ),

    care_home = patients.care_home_status_as_of(
        "index_date",
        categorised_as={
            "CareHome": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing = 'Y'
              AND LocationRequiresNursing = 'N'
            """,
            "NursingHome": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing = 'N'
              AND LocationRequiresNursing = 'Y'
            """,
            "CareOrNursingHome": "IsPotentialCareHome",
            "PrivateHome": "NOT IsPotentialCareHome",
            "": "DEFAULT",
        },
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "CareHome": 0.30,
                    "NursingHome": 0.10,
                    "CareOrNursingHome": 0.10,
                    "PrivateHome": 0.45,
                    "": 0.05,
                },
            },
        },
    ),

    imd = patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844""",
        },
        index_of_multiple_deprivation = patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest = 100,
        ),
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.05,
                    "1": 0.19,
                    "2": 0.19,
                    "3": 0.19,
                    "4": 0.19,
                    "5": 0.19,
                }
            },
        },
    ),

    ethnicity = patients.with_ethnicity_from_sus(
        returning = "group_16",  
        use_most_frequent_code = True,
        return_expectations = {
            "category": {"ratios": {"1": 0.0625,
                    "2": 0.0625,
                    "3": 0.0625,
                    "4": 0.0625,
                    "5": 0.0625,
                    "6": 0.0625,
                    "7": 0.0625,
                    "8": 0.0625,
                    "9": 0.0625,
                    "10": 0.0625,
                    "11": 0.0625,
                    "12": 0.0625,
                    "13": 0.0625,
                    "14": 0.0625,
                    "15": 0.0625,
                    "16": 0.0625,}},
            "incidence": 0.4,
        },
    ),

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

    amr_6_months_first_match = patients.with_these_medications(
        antibacterial_codes,
        between = ["index_date - 6 months", "index_date"],
        find_first_match_in_period = True,
        returning = "date",
        return_expectations = {
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),

    # comorbidities
    has_copd = patients.with_these_clinical_events(
        copd_codes,
        on_or_before = "index_date",
        return_expectations = {"incidence": 0.5}
    ),

    # indications
    has_acne = patients.with_these_clinical_events(
        acne_codes,
        between = ["index_date - 6 months", "index_date"],
        return_expectations = {"incidence": 0.5}
    ),

    #**loop_over_codes(antibacterial_codes),
)

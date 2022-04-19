from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA
import pandas as pd

placeholder_med_codes = codelist_from_csv(
    "codelists/opensafely-asthma-inhaler-steroid-medication.csv",
    system="snomed",
    column="id",
)

copd_codes = codelist_from_csv(
    "codelists/opensafely-current-copd.csv", system="ctv3", column="CTV3ID"
)

STPs = pd.read_csv(filepath_or_buffer="input-data/STPs.csv")
dict_stp = {stp: 1 / len(STPs.index) for stp in STPs["stp_id"].tolist()}


study = StudyDefinition(
    index_date = "2022-01-01"
    # configure expectations framework: event dates expected to be distributed between 2022-01-01 and 2022-04-01, with uniform frequency, with events occurring for 50% patients
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
        (sex = 'M' OR sex = 'F') AND
        (amr_6_months >= 3)
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
        return_expectations = {"incidence": 0.5}  # 50% care home residency
    ),

    # antibiotic prescribing
    amr_6_months = patients.with_these_medications(
        placeholder_med_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_episodes",
        episode_defined_as = "series of events each <= 28 days apart",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),

    # comorbidities
    has_copd = patients.with_these_clinical_events(
        copd_codes,
        between = ["index_date", "index_date + 3 months"]
    )
)

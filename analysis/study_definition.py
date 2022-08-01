from cohortextractor import StudyDefinition, patients, codelist
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

    ethnicity = patients.categorised_as(
            {"Missing": "DEFAULT",
            "British": "eth = '1' OR (NOT eth AND ethnicity_sus = '1')", 
            "Irish": "eth = '2' OR (NOT eth AND ethnicity_sus = '2')", 
            "Any other White background": "eth = '3' OR (NOT eth AND ethnicity_sus = '3')", 
            "White and Black Caribbean": "eth = '4' OR (NOT eth AND ethnicity_sus = '4')",  
            "White and Black African": "eth = '5' OR (NOT eth AND ethnicity_sus = '5')",
            "White and Asian": "eth = '6' OR (NOT eth AND ethnicity_sus = '6')",
            "Any other Mixed background": "eth = '7' OR (NOT eth AND ethnicity_sus = '7')",
            "Indian": "eth = '8' OR (NOT eth AND ethnicity_sus = '8')",
            "Pakistani": "eth = '9' OR (NOT eth AND ethnicity_sus = '9')",
            "Bangladeshi": "eth = '10' OR (NOT eth AND ethnicity_sus = '10')",
            "Any other Asian background": "eth = '11' OR (NOT eth AND ethnicity_sus = '11')",
            "Caribbean": "eth = '12' OR (NOT eth AND ethnicity_sus = '12')",
            "African": "eth = '13' OR (NOT eth AND ethnicity_sus = '13')",
            "Any other Black background": "eth = '14' OR (NOT eth AND ethnicity_sus = '14')",
            "Chinese": "eth = '15' OR (NOT eth AND ethnicity_sus = '15')",
            "Any other ethnic group": "eth = '16' OR (NOT eth AND ethnicity_sus = '16')",
            },
            return_expectations = {
            "category": {"ratios": {"British": 0.0625, "Irish": 0.0625, "Any other White background": 0.0625,
                        "White and Black Caribbean": 0.0625, "White and Black African": 0.0625, "White and Asian": 0.0625, "Any other Mixed background": 0.0625,
                        "Indian": 0.0625, "Pakistani": 0.0625, "Bangladeshi": 0.0625, "Any other Asian background": 0.0625,
                        "Caribbean": 0.0625, "African": 0.0625, "Any other Black background": 0.0625,
                        "Chinese": 0.0625, "Any other ethnic group": 0.0625}},
            "incidence": 0.4,
            },

            ethnicity_sus = patients.with_ethnicity_from_sus(
                returning = "group_16",
                use_most_frequent_code = True,
                return_expectations = {
                    "category": {"ratios": {"1": 0.0625, "2": 0.0625, "3": 0.0625, "4": 0.0625, "5": 0.0625, "6": 0.0625, "7": 0.0625, "8": 0.0625,
                                "9": 0.0625, "10": 0.0625, "11": 0.0625, "12": 0.0625, "13": 0.0625, "14": 0.0625, "15": 0.0625, "16": 0.0625}},
                    "incidence": 0.75,
                },
            ),

            eth = patients.with_these_clinical_events(
                ethnicity_codes,
                returning = "category",
                find_last_match_in_period = True,
                on_or_before = "index_date",
                return_expectations = {
                    "category": {"ratios": {"1": 0.0625, "2": 0.0625, "3": 0.0625, "4": 0.0625, "5": 0.0625, "6": 0.0625, "7": 0.0625, "8": 0.0625,
                                "9": 0.0625, "10": 0.0625, "11": 0.0625, "12": 0.0625, "13": 0.0625, "14": 0.0625, "15": 0.0625, "16": 0.0625}},
                    "incidence": 0.75,
                },
            ),
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
        date_format="YYYY-MM-DD",
        return_expectations = {
            "date": {"earliest": "index_date - 6 months", "latest": "index_date"}
        },
    ),

    aminoglycosides_6_months = patients.with_these_medications(
        aminoglycosides_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    antileprotic_drugs_6_months = patients.with_these_medications(
        antileprotic_drugs_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    antituberculosis_drugs_6_months = patients.with_these_medications(
        antituberculosis_drugs_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    cephalosporins_and_other_beta_lactams_6_months = patients.with_these_medications(
        cephalosporins_and_other_beta_lactams_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    clindamycin_and_lincomycin_6_months = patients.with_these_medications(
        clindamycin_and_lincomycin_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    macrolides_6_months = patients.with_these_medications(
        macrolides_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    metronidazole_tinidazole_and_ornidazole_6_months = patients.with_these_medications(
        metronidazole_tinidazole_and_ornidazole_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    penicillins_6_months = patients.with_these_medications(
        penicillins_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    quinolones_6_months = patients.with_these_medications(
        quinolones_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    some_other_antibacterials_6_months = patients.with_these_medications(
        some_other_antibacterials_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    sulfonamides_and_trimethoprim_6_months = patients.with_these_medications(
        sulfonamides_and_trimethoprim_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    tetracyclines_6_months = patients.with_these_medications(
        tetracyclines_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    urinary_tract_infections_6_months = patients.with_these_medications(
        urinary_tract_infections_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),

    demeclocycline_6_months = patients.with_these_medications(
        demeclocycline_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    doxycycline_6_months = patients.with_these_medications(
        doxycycline_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ), 
    lymecycline_6_months = patients.with_these_medications(
        lymecycline_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    minocycline_6_months = patients.with_these_medications(
        minocycline_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    oxytetracycline_6_months = patients.with_these_medications(
        oxytetracycline_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),
    tetracycline_6_months = patients.with_these_medications(
        tetracycline_codes,
        between = ["index_date - 6 months", "index_date"],
        returning = "number_of_matches_in_period",
        return_expectations = {
            "int": {"distribution": "normal", "mean": 2, "stddev": 1},
            "incidence": 0.2,
        },
    ),


    # clinical events: comorbidities (variables prefixed with "has_" created using on_or_before statement); indications (variables prefixed with "had_" created using between statement) 
    # returns binary_flag (default for with_these_clinical_events variable extractor) depending on whether patient had clinical event in defined period

    # comorbidities
    has_comorbidity_cancer_immunosuppression = patients.with_these_clinical_events(
        cancer_immunosuppression_codes,
        on_or_before = "index_date",
        return_expectations = {"incidence": 0.5}
    ),

    has_comorbidity_copd = patients.with_these_clinical_events(
        copd_codes,
        on_or_before = "index_date",
        return_expectations = {"incidence": 0.5}
    ),

    has_comorbidity_sickle_cell = patients.with_these_clinical_events(
        sickle_cell_codes,
        on_or_before = "index_date",
        return_expectations = {"incidence": 0.5}
    ),

    has_comorbidity_splenectomy = patients.with_these_clinical_events(
        splenectomy_codes,
        on_or_before = "index_date",
        return_expectations = {"incidence": 0.5}
    ),  

    # indications
    has_indication_acne = patients.with_these_clinical_events(
        acne_codes,
        between = ["amr_6_months_first_match - 14 days", "amr_6_months_first_match + 14 days"],
        return_expectations = {"incidence": 0.5}
    ),

    has_indication_copd_infection = patients.with_these_clinical_events(
        copd_infection_codes,
        between = ["amr_6_months_first_match - 14 days", "amr_6_months_first_match + 14 days"],
        return_expectations = {"incidence": 0.5}
    ),

    has_indication_otitis_media = patients.with_these_clinical_events(
        otitis_media_codes,
        between = ["amr_6_months_first_match - 14 days", "amr_6_months_first_match + 14 days"],
        return_expectations = {"incidence": 0.5}
    ),

    has_indication_ssti = patients.with_these_clinical_events(
        ssti_codes,
        between = ["amr_6_months_first_match - 14 days", "amr_6_months_first_match + 14 days"],
        return_expectations = {"incidence": 0.5}
    ),

    has_indication_uti = patients.with_these_clinical_events(
        uti_codes,
        between = ["amr_6_months_first_match - 14 days", "amr_6_months_first_match + 14 days"],
        return_expectations = {"incidence": 0.5}
    ),

)

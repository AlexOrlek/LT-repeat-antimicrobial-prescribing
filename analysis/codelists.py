from cohortextractor import codelist, codelist_from_csv


# define codelists
antibacterial_codes = codelist_from_csv(
    "codelists/opensafely-antibacterials.csv", system="snomed", column="dmd_id",
)

copd_codes = codelist_from_csv(
    "codelists/opensafely-current-copd.csv", system="ctv3", column="CTV3ID"
)

acne_codes = codelist_from_csv(
    "codelists/user-alexorlek-acne.csv", system="snomed", column="code"
)
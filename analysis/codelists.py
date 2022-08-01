from os import system
from cohortextractor import codelist, codelist_from_csv, combine_codelists


# DEFINE CODELISTS

# antibacterial codes
# antibacterial_codes = codelist_from_csv(
#     "codelists/opensafely-antibacterials.csv", system="snomed", column="dmd_id",
# )

aminoglycosides_codes = codelist_from_csv(
    "codelists/user-alexorlek-aminoglycosides-antibacterials.csv", system="snomed", column="dmd_id"
)

antileprotic_drugs_codes = codelist_from_csv(
    "codelists/user-alexorlek-antileprotic-drugs-antibacterials.csv", system="snomed", column="dmd_id"
)

antituberculosis_drugs_codes = codelist_from_csv(
    "codelists/user-alexorlek-antituberculosis-drugs-antibacterials.csv", system="snomed", column="dmd_id"
)

cephalosporins_and_other_beta_lactams_codes = codelist_from_csv(
    "codelists/user-alexorlek-cephalosporins-and-other-beta-lactams-antibacterials.csv", system="snomed", column="dmd_id"
)

clindamycin_and_lincomycin_codes = codelist_from_csv(
    "codelists/user-alexorlek-clindamycin-and-lincomycin-antibacterials.csv", system="snomed", column="dmd_id"
)

macrolides_codes = codelist_from_csv(
    "codelists/user-alexorlek-macrolides-antibacterials.csv", system="snomed", column="dmd_id"
)

metronidazole_tinidazole_and_ornidazole_codes = codelist_from_csv(
    "codelists/user-alexorlek-metronidazole-tinidazole-and-ornidazole-antibacterials.csv", system="snomed", column="dmd_id"
)

penicillins_codes = codelist_from_csv(
    "codelists/user-alexorlek-penicillins-antibacterials.csv", system="snomed", column="dmd_id"
)

quinolones_codes = codelist_from_csv(
    "codelists/user-alexorlek-quinolones-antibacterials.csv", system="snomed", column="dmd_id"
)

some_other_antibacterials_codes = codelist_from_csv(
    "codelists/user-alexorlek-some-other-antibacterials-antibacterials.csv", system="snomed", column="dmd_id"
)

sulfonamides_and_trimethoprim_codes = codelist_from_csv(
    "codelists/user-alexorlek-sulfonamides-and-trimethoprim-antibacterials.csv", system="snomed", column="dmd_id"
)

tetracyclines_codes = codelist_from_csv(
    "codelists/user-alexorlek-tetracyclines-antibacterials.csv", system="snomed", column="dmd_id"
)

urinary_tract_infections_codes = codelist_from_csv(
    "codelists/user-alexorlek-urinary-tract-infections-antibacterials.csv", system="snomed", column="dmd_id"
)

antibacterial_codes = combine_codelists(
    aminoglycosides_codes,
    antileprotic_drugs_codes,
    antituberculosis_drugs_codes,
    cephalosporins_and_other_beta_lactams_codes,
    clindamycin_and_lincomycin_codes,
    macrolides_codes,
    metronidazole_tinidazole_and_ornidazole_codes,
    penicillins_codes,
    quinolones_codes,
    some_other_antibacterials_codes,
    sulfonamides_and_trimethoprim_codes,
    tetracyclines_codes,
    urinary_tract_infections_codes
)

demeclocycline_codes = codelist_from_csv(
    "codelists/user-alexorlek-demeclocycline-tetracyclines.csv", system="snomed", column="dmd_id"
)

doxycycline_codes = codelist_from_csv(
    "codelists/user-alexorlek-doxycycline-tetracyclines.csv", system="snomed", column="dmd_id"
)

lymecycline_codes = codelist_from_csv(
    "codelists/user-alexorlek-lymecycline-tetracyclines.csv", system="snomed", column="dmd_id"
)

minocycline_codes = codelist_from_csv(
    "codelists/user-alexorlek-minocycline-tetracyclines.csv", system="snomed", column="dmd_id"
)

oxytetracycline_codes = codelist_from_csv(
    "codelists/user-alexorlek-oxytetracycline-tetracyclines.csv", system="snomed", column="dmd_id"
)

tetracycline_codes = codelist_from_csv(
    "codelists/user-alexorlek-tetracycline-tetracyclines.csv", system="snomed", column="dmd_id"
)


# demographic codes
ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv", system="ctv3", column="Code", category_column="Grouping_16"
)


# comorbidity codes
cancer_immunosuppression_codes = codelist_from_csv(
    "codelists/user-alexorlek-cancer-immunosuppression.csv", system="snomed", column="code"
)

copd_codes = codelist_from_csv(
    "codelists/opensafely-current-copd.csv", system="ctv3", column="CTV3ID"
)

sickle_cell_codes = codelist_from_csv(
    "codelists/opensafely-sickle-cell-disease-snomed.csv", system="snomed", column="id"
)

splenectomy_codes = codelist_from_csv(
    "codelists/user-alexorlek-splenectomy.csv", system="snomed", column="code"
)


# indication codes
acne_codes = codelist_from_csv(
    "codelists/user-alexorlek-acne.csv", system="snomed", column="code"
)

copd_infection_codes = codelist_from_csv(
    "codelists/opensafely-copd-infection.csv", system="ctv3", column="CTV3ID"
)

otitis_media_codes = codelist_from_csv(
    "codelists/user-alexorlek-otitis-media.csv", system="snomed", column="code"
)

ssti_codes = codelist_from_csv(
    "codelists/user-alexorlek-skin-and-soft-tissue-infections.csv", system="snomed", column="code"
)

uti_codes = codelist_from_csv(
    "codelists/user-alexorlek-urinary-tract-infection.csv", system="snomed", column="code"
)

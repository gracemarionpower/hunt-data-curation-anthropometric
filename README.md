This repository documents the data curation steps carried out on the HUNT Cloud to prepare HUNT anthropometric data for the following projects:

1. Disentangling Early-Life Pathways to Breast Cancer: Investigating the Independent and Interactive Effects of Prepubertal Body Composition and Puberty Timing
2. The Lifecourse GWAS processing pipeline (for requirements, see: https://github.com/MRCIEU/Lifecourse-GWAS/wiki).

The pipeline performs the following tasks:

Generates one individual-level datafile with all YoungHUNT anthropometric data, with year of birth, sex and age at each measurement collected.
Generates a static covariate file containing FID, IID, sex, yob for children, mothers, and fathers. Each individual is treated independently.
Generates  separate long-format time-varying trait files for  up to the 5 anthropometric traits requested throuhg the Lifecourse GWAS pipeline, with columns: FID, IID, value, age (when value measured). Individuals may have multiple repeated measurements, each recorded independently and will be treated in the pipeline as seperate individuals.

Collaborators include: Ben Brumpton and Bjørn Olav Åsvold, NTNU

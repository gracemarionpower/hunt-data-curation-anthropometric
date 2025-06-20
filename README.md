This repository documents the data curation steps carried out on the HUNT Cloud to prepare HUNT anthropometric data for the my trajectories grant and the Lifecourse GWAS processing pipeline. For requirements, see: https://github.com/MRCIEU/Lifecourse-GWAS/wiki.

The pipeline performs the following tasks:

Generates a static covariate file containing FID, IID, sex, yob for children, mothers, and fathers. Each individual is treated independently.
Generates  separate long-format time-varying trait files with columns: FID, IID, value, age (when value measured). Individuals may have multiple repeated measurements, each recorded independently and will be treated in the pipeline as seperate individuals.

Collaborators include: Ben Brumpton and Bjørn Olav Åsvold, NTNU

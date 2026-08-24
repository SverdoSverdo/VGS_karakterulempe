# FILE INFORMATION
Contains code used for the report "Karakterulempen ved å velge realfag: I hvilken grad veier realfagspoengene opp for den?" commissioned by the Norwegian Ministry of Education and Research.

The files StartVals_joint2.rds and StartVals_joint3.rds provide starting values for the more complex models in Year 2 and year 3 that are very computationally demanding.
The starting values matches that of the final parameter-estimates, and so using them reduces the computational time significantly, although the calculation of standard errors will still take some time.


All code was run on R version 4.6.1.
The scripts are numbered in the order they should be run. Each one sources
`00_settings.R`


### `00_settings.R`

Sets the working directory and loads all required packages. It also documents the folder structure 
of your working directory that the other scripts require: 
`scripts/`, `data.temp/`, `results/expected_grades/`, `results/models/` and
`results/plots/`. 

### `fagkode_scraper.R`

Builds the subject-code reference file used throughout the project by scraping
Udir's fagkode listing API for the studiespesialisering (ST) programme, paging
through all results. The output is a table of subject titles, fagkoder and flags
whether the subject provides a final grade (standpunktkarakter).

### `01_cleaning.R`
Cleans the raw grade register: keeps the 2023–2024 cohort, resolves duplicate
pupil × subject rows, and merges in study programme from the course register.
The output is the long-format grade file everything downstream builds on.

### `02_year1.R`
Reshapes the VG1 grades to one row per pupil, with a standpunkt and an exam
column per subject, keeping the most common subjects. Also writes a
descriptives sheet for the trinn.

### `03_year2.R`
The same for VG2, keeping the 20 most common subjects and defining which of
them are mandatory.

### `04_year3.R`
The same for VG3.

### `05_choice_variable.R`
Builds the binary choice items for VG2 and VG3: did the pupil take the subject,
given that their school offered it. Subjects the school never offered are set
to missing rather than counted as a rejected choice.

### `06_year1_analyses.R`
Fits and compares the VG1 measurement models and settles on the one used later,
with item fit checks along the way.

### `07_year2_analyses.R`
Selects the VG2 item set and fits the grade models, the choice model, and
finally the joint model that combines them. Starting values that speeds up estimation
of the joint mode can be used as instructed in the script.

### `08_year3_analyses.R`
The same for VG3. The joint model is larger here, so it is given starting
values from the choice model to help it converge. Starting values that speeds up estimation
of the joint model can be used as instructed in the script.

### `09_expectedgrades.R`
Computes the counterfactual grades: for each pupil, the expected grade in every
subject they did not take, given their observed grades and choices. Also
Calculates the observed grades. These latter calculations are used in 010_model_comparison.R.
The calculations are performed in parallel, and you might want to adjust the number of cores used in the two
separate functions that calculates grades. 

### `010_model_comparisons.R`
Checks whether the joint model reproduces the observed subject means better
than the grade-only model, and writes the comparison for VG2 and VG3.

### `21_descriptives.R`
Builds the descriptive tables for the report: grade counts, mean standpunkt and
exam grades per subject, pupils per study programme, and how much pupils cross
between the two tracks.

### `22_grade_disadvantage.R`
Produces the subject-level results — observed against expected grades, and the
karakterulempe of each realfag subject relative to the SSØ average — and saves
the figures.

### `23_track_comparison.R`
Simulates full three-year courses of study and accumulates the disadvantage in grades and
admission points
across VG1–VG3 for three realfag tracks against an SSØ track. Converts the
result to admission points under the current and the post-2028 bonus systems.

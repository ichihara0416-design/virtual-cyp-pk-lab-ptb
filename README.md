# Virtual CYP-PK Lab — Pentobarbital

This repository contains the **Virtual CYP-PK Lab** and the reproducible analysis code accompanying the manuscript:

**“A literature-informed framework for linking systemic pharmacokinetic changes to pentobarbital-induced hypnosis and its implementation in a Virtual CYP-PK Lab.”**

It includes standalone English and Japanese R/Shiny applications and the reproducible R analysis pipeline used for the literature-informed pentobarbital (PTB) PK/PD framework.

## Repository structure

```text
virtual-cyp-pk-lab-ptb/
├─ Virtual_CYP_PK_Lab_EN.R
├─ Virtual_CYP_PK_Lab_JA.R
├─ README.md
├─ CITATION.cff
├─ LICENSE
├─ .gitignore
└─ Analysis/
   ├─ run_all.R
   ├─ R/
   │  ├─ 00_constants.R
   │  ├─ 01_model_core.R
   │  ├─ 02_reference_calibration.R
   │  ├─ 03_fit_all_PK.R
   │  ├─ 04_behavior_and_residual.R
   │  ├─ 05_external_transportability.R
   │  ├─ 06_variability_and_app.R
   │  ├─ 07_monte_carlo.R
   │  ├─ 08_manuscript_outputs.R
   │  └─ 99_QA.R
   └─ data/
      ├─ evidence_A_B.csv
      ├─ control_variability_benchmarks.csv
      └─ literature_fixed_inputs.csv
```

The analysis creates `Analysis/outputs/`, `Analysis/figures/`, and `Analysis/tables/` when it is run. These generated files are intentionally not stored in the repository.

## Virtual CYP-PK Lab

Two standalone versions are provided:

- `Virtual_CYP_PK_Lab_EN.R` — English version
- `Virtual_CYP_PK_Lab_JA.R` — Japanese version

Both applications are self-contained and use the same fixed virtual-rat bank and model parameterization used for the manuscript implementation.

### Required R packages

- `shiny`
- `deSolve`

The application scripts check for these packages and install them from CRAN if needed.

### Running an application

Open either application file in RStudio and click **Run App**.

Alternatively:

```r
source("Virtual_CYP_PK_Lab_EN.R")
```

or

```r
source("Virtual_CYP_PK_Lab_JA.R")
```

## Reproducing the manuscript analysis

From the repository root, run:

```r
source("Analysis/run_all.R")
```

`run_all.R` changes the working directory to `Analysis/`, executes the analysis scripts in the required order, and generates numerical outputs, manuscript-oriented tables and figures, and QA files.

The analysis sequence is:

1. reference/control calibration
2. behavior-independent systemic PK fitting
3. behavioral calculation and post-PK residual analysis
4. external transportability analysis
5. variability qualification and virtual-rat-bank generation
6. Monte Carlo analysis
7. manuscript-oriented tables and figures
8. QA checks

The Monte Carlo analysis uses fixed random seeds so that the public analysis is reproducible from the same code and inputs.

## Analysis inputs

`Analysis/data/evidence_A_B.csv` contains the quantitative Evidence A and Evidence B inputs used for intervention-specific PK analysis.

`Analysis/data/control_variability_benchmarks.csv` contains literature-derived variability benchmarks used to qualify the prespecified virtual-laboratory variability settings.

`Analysis/data/literature_fixed_inputs.csv` contains fixed literature-derived numerical inputs used for reference/control calibration and external transportability calculations.

## Generated outputs

Running `Analysis/run_all.R` creates:

- `Analysis/outputs/` — numerical outputs and QA files
- `Analysis/figures/` — generated manuscript figures
- `Analysis/tables/` — manuscript-oriented source tables

These folders are excluded from Git version control because they can be regenerated directly from the public code and input data.

## Reproducibility notes

Intervention-specific systemic PK fitting is performed without using literature-reported behavioral outcomes. Behavioral outcomes are introduced only after the systemic PK parameters have been estimated.

Evidence A and Evidence B are analyzed using the same fixed reference/control framework. The Monte Carlo analysis uses a fixed virtual-rat bank and fixed random seeds.

The `99_QA.R` script performs consistency checks on the quantitative evidence set and analysis outputs. A successful complete run terminates only after all QA checks pass.

## Citation

If you use this repository, please cite the associated manuscript and this repository.

Citation metadata for the software are provided in `CITATION.cff`.

## License

This project is released under the **MIT License**, permitting reuse, modification, and redistribution subject to the terms of the license.

## Author

Yoshinori Ichihara  
Division of Pharmacology, Department of Pathophysiological and Therapeutic Science, Faculty of Medicine, Tottori University, Japan

# 🧙 MIMICWizard 
[![Docker Image Version](https://img.shields.io/docker/v/lucasduval/mimicwizard?sort=semver)](https://hub.docker.com/r/lucasduval/mimicwizard) ![GitHub Release Date](https://img.shields.io/github/release-date/biomerieux-open-sources/mimicwizard) ![GitHub contributors](https://img.shields.io/github/contributors/biomerieux-open-sources/mimicwizard) 





**MIMICWizard** is an open-source web application that enables intuitive exploration of the [MIMIC-IV](https://physionet.org/content/mimiciv/) critical care database by clinicians and researchers without programming expertise

![](graphical_abstract.png) 

> [!NOTE] 
> When using this resource, please cite the associated article : (submitted, waiting for review)

## 🧭 Introduction

Large public EHR datasets like MIMIC-IV contain rich, longitudinal patient data but are difficult to navigate for users lacking data science training.

MIMICWizard addresses this gap by providing an interactive dashboard for patient-level data visualization, cohort query and creation, statistical summarization for MIMIC-IV database.

Originally developed for clinicians, MIMICWizard supports hypothesis generation and data exploration in a hospital context.

## 🚀 Features

-   Visual exploration of patient data from MIMIC-IV.
-   Cohort creation and exploration for hypothesis generation
-   Stratification and descriptive statistics on population and subgroups
-   Advanced exploration with search engine including events, ICD and demographics
-   Demo mode with a lightweight dataset.
-   Open-source and community-driven.

## 🖥 Demo

A demo version of the application is available at [https://mimicwizard-demo.lcr.datailor.eu/](https://mimicwizard-demo.lcr.datailor.eu/).

This demo version use [MIMIC-IV demo database](https://physionet.org/content/mimic-iv-demo/2.2/), an openly-available demo of MIMIC-IV containing a subset of 100 patients.

## 🛠 Installation and usage

MIMICWizard is based on R Shiny and interact with a PostgreSQL database. Detailed setup instruction are available on the [dedicated documentation page](https://mimicwizard.readthedocs.io/en/latest/installation/).

The application can be installed via Docker or source, please refer to documentation.

More information about the application and usage are available on the [ReadTheDocs documentation](https://mimicwizard.readthedocs.io/en/latest/).

> [!IMPORTANT] 
> Access to the full MIMIC-IV dataset requires credentialing and completion of a data use agreement. Visit [PhysioNet](https://physionet.org) for more information.

### 📦 Changelog
**v1.1** (2026-07-27)

General :
- Configuration variable need to be exposed as environment variable (global.R configuration does not work anymore)

Patient Explorer :
- fix : grouped events are now correctly displayed on timeline when grouped along individual events
- fix : improve performance when displaying a hadm before a stay

Cohort creation & Cohort explorer :
- feat : cohort can now be edited, copied, exported and imported for reproducibility
- feat : cohort now display underling SQL logic


**v1.0** (2026-04-15)
General
- Ready to deploy
- Bugfixes

**v0.9.1** (2026-04-01)

Event searchbar :
- feat : add unit of measure on value field
- enhancement : error when using event filtering are now more explicit 
- fix : correct error throwing on datetimeevents
- fix : auto-convert comma with point when filtering with numerical value 

Cohort explorer :
- fix : refresh and delete button


**v0.9.0** (2026-03-25)

Patient explorer:
- fix: longitudinal trajectory is now not restricted to a 1 hour timestep

Cohort creation:
- feat : cohort creation can now be based on exclusion criteria, in addition to inclusion criteria
- fix : fetch button now show if ICD only criteria are set

Cohort explorer:
- feat : cohort outcomes data can now be exported in csv format 
- fix : we added some warning when using statistical testing in Clinical Data Desc. tab

**v0.8.1** (2026-03-03)
Cohort explorer :
- feat : cohort parameter exploration now support advanced stratification

Patient explorer
- fix : microbiology events was lacking of time attribute

**v0.8.0** (2025-11-05) Clinical notes and extended data

General
- Support for MIMIC-IV clinical notes (discharge), activate the flag in the global.R file

Patient explorer
- Adding hadm/stay start and end time visualization on timeline
- Add extended data when visualize hospitalization, get access to lab and microbiology result when data is consitent with hospital stay but not linked to a hadm_id (useful for ED lab measurement)
- Bugfixes

**v0.7.1** (2025-06-27) Bugfixes and optimization

**v0.7.0** (2025-06-20) Public release associated with online demo version

**v0.6.4** (2025-03-13) First closed full version beta-testing

## 🧪 Research & Development

This application was developed by the **Laboratoire Commun de Recherche - HCL-bioMérieux** (Lyon, France), a joint research initiative between Edouard Herriot Hospital and bioMérieux.

📫 For questions, suggestions, or collaborations, feel free to open an issue or contact the research team via the GitHub repository.

> [!WARNING] 
> This is not an official product of bioMérieux or Hospices Civils de Lyon. The software is provided "as is" without warranty under GPLv3 license.

### 🤝 Contributing

We welcome contributions from the community! To contribute:

1.  Open a issue about your concerns, problems or feature proposal
2.  Fork the repository and open a Pull Request with your code suggestion

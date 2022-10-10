# Domiciliary care use in Barking & Dagenham

#### Project Status: [completed]

## Project Description

Domiciliary care, or home care, are a range of services that support people while living in their own home. Older people and people with complex needs often move between different care settings, and around 20% of requests for social care support received by Local Authorities in England come from people being discharged from hospital.

This analysis aims to describe the relationship between hospital admissions and domiciliary care use, including
* what proportion of publicly funded domiciliary care packges are associated with discharge from hospital, and
* which groups of hospital patients were at higher risk of requiring domiciliary care after discharge. 

This project is a collaboration between Care City and the Health Foundation. 

## Outputs
The findings have been published in [BMJ Open](https://bmjopen.bmj.com/content/12/9/e061875.abstract).

## Data sources

We used the [Care City Cohort](https://www.carecity.london/component/content/article/95-what-we-do/216-care-city-cohort), a linked dataset of  residents of the London Borough of Barking and Dagenham, which contains household and individual-level data across health services and Barking and Dagenham council. 

## How does it work?

As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on similar datasets.

This [R script](https://github.com/HFAnalyticsLab/domcare_hospital_LBBD/blob/main/analysis_of_care_city_cohort.R) joins data from the local authority social care department (including data on domiciliary care) with data about hospital admissions for local residents. It describes the association between hospital discharge and new domiciliary care packages.

### Requirements

These scripts were written in R.
The following R packages (available on CRAN) are needed: 
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)
* [**data.table**](https://cran.r-project.org/web/packages/data.table/index.html) 
* [**RColorBrewer**](https://cran.r-project.org/web/packages/RColorBrewer/index.html)
* [**stringi**](https://cran.r-project.org/web/packages/stringi/index.html)

## Project team

* Dan Lewer, UCL
* Jenny Shand, Care City and UCLP
* John Craig, Care City
* Shilpi Begum, Care City
* Fiona Grimm, Health Foundation
* Rafi Roganswatson, University Hospitals Sussex

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/domcare_hospital_LBBD/blob/master/LICENSE).

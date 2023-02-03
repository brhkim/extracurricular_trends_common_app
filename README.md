# Replication Code Archive for "Inequality beyond Standardized Tests: Trends in Extracurricular Activity Reporting in College Applications Across Race and Class" research paper by Park et al.

This repository is a public-facing archive of the analytic code used for the paper, "Inequality beyond Standardized Tests: Trends in Extracurricular Activity Reporting in College Applications Across Race and Class," by Julie Park et al. A public working paper draft of this work will be available by mid-Feburary, 2023:

>Inequality related to standardized tests in college admissions has long been a subject of discussion; less is known about inequality in non-standardized components of the college application. We analyzed extracurricular activity descriptions in 5,967,920 applications submitted through the Common Application platform. Using human-crafted keyword dictionaries combined with text-as-data (natural language processing) methodologies, we found that White, Asian American, high-SES, and private school students reported substantially more extracurricular activities overall, more activities with top-level leadership roles, and more activities with distinctive accomplishments (e.g., honors, awards). Disparities decrease when accounting for other applicant demographics, school fixed effects, and standardized test scores. Still, salient differences remain, especially those related to first-generation applicants. Implications and recommendations for college admissions policy and practice are discussed.

Importantly, the data necessary for this project were provided in close collaboration with researchers at The Common Application, Inc., and is not publicly accessible given the inherent risks of personally identifiable information and reidentification within the data. As such, this codebase is provided for transparency and instruction purposes only, given that complete replication is infeasible.

Code in this repository is named in the same sequence it needs to be run. The general flow of the analysis proceeds in the following steps:
1. Gather the requisite data from the Common App data warehouse
2. Create a cleaned dataset of covariates for student data
   + 2b-2d. Conduct exploratory text analyses to inform the creation of a keyword dictionary
   + 2e-2f. Align manually created keyword dictionaries and apply to the data
3. Create a final analytic dataset with all the extracurricular measures and student covariates together
   + 3b. Produce simple descriptive tables to describe the sample
4. Conduct simple descriptive analysis on the extracurricular data results
   + 4b. Run regression analyses as described in the paper
5. Run supplementary analyses to examine the robustness of our dictionary method against human judgment
6. Conduct other related robustness checks

While this codebase cannot be applied "as-is" to any other data, I am happy to provide whatever support I can to other researchers interested in applying similar methods, or encountering similar technical challenges in their own analyses. I also recognize the potential for errors and issues of all kinds, despite my best efforts. Please don't hesitate to reach out!

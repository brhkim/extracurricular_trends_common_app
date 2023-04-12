# Replication Code Archive and Online Appendix for "Inequality beyond Standardized Tests: Trends in Extracurricular Activity Reporting in College Applications Across Race and Class" research paper by Park et al.

This repository is a public-facing archive of the analytic code used for the paper, "Inequality beyond Standardized Tests: Trends in Extracurricular Activity Reporting in College Applications Across Race and Class," by Julie Park et al. This repository also includes our online appendix. A public working paper draft of this work is available on [EdWorkingPapers](https://edworkingpapers.com/ai23-749):

>Inequality related to standardized tests in college admissions has long been a subject of discussion; less is known about inequality in non-standardized components of the college application. We analyzed extracurricular activity descriptions in 5,967,920 applications submitted through the Common Application platform. Using human-crafted keyword dictionaries combined with text-as-data (natural language processing) methods, we found that White, Asian American, high-SES, and private school students reported substantially more activities, more activities with top-level leadership roles, and more activities with distinctive accomplishments (e.g., honors, awards). Black, Latinx, Indigenous, and low-income students reported a similar proportion of activities with top-level leadership positions as other groups, although the absolute number was lower. Gaps also lessened for honors/awards when examining proportions, versus absolute number. Disparities decreased further when accounting for other applicant demographics, school fixed effects, and standardized test scores. However, salient differences related to race and class remain. Findings do not support a return to required standardized testing, nor do they necessarily support ending consideration of activities in admissions. We discuss reducing the number of activities that students report and increasing training for admissions staff as measures to strengthen holistic review.

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

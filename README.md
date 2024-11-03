# AccindexCOVID19
The Acceleration Index is a novel indicator that takes into account diagnostic tests in a unit- and model-free way to track accurately the dynamics of epidemics, hence helping to guide public health policies and to better inform health specialists and the general population.

- This repository contains data and R codes to accompany two papers published in PLoS ONE (Open Access academic journal) and to replicate their main results: 

(1) https://doi.org/10.1371/journal.pone.0252443 in which Acceleration Index is shown to take into account time-varying tests and is applied to French data (aggregate, age groups, departemental and city areas)

(2) https://doi.org/10.1371/journal.pone.0281943 in which how Reproduction Number and its bias due to time-varying tests combine into the Acceleration Index, using French and other countries' data as an illustration

R codes for (1):

R code Figs-Covid-France-departement.2.2.R uses data file sp-pos-quot-dep-2021-04-01-18h20.csv to generate input used in particular for 

Figure 3 available at https://journals.plos.org/plosone/article/figure?id=10.1371/journal.pone.0252443.g003

Figure 4 available at https://journals.plos.org/plosone/article/figure?id=10.1371/journal.pone.0252443.g004

Figure 5 available at https://journals.plos.org/plosone/article/figure?id=10.1371/journal.pone.0252443.g005

Figure 7 available at https://journals.plos.org/plosone/article/figure?id=10.1371/journal.pone.0252443.g007

R codes for (2):

R code acceleration-index.R generates input used in particular for 

Figure 3 available at https://journals.plos.org/plosone/article/figure?id=10.1371/journal.pone.0281943.g003

Figure 4 available at https://journals.plos.org/plosone/article/figure?id=10.1371/journal.pone.0281943.g004

- This repository also contains data and R codes to accompany MedRXiv preprint available at https://www.medrxiv.org/content/10.1101/2020.11.11.20230243v1 and to replicate the main results:

R code curfew-january.R uses data file sp-pos-quot-dep-2021-01-27-19h20.csv to generate input used in particular for the figures in the preprint

- This repository also contains data and R code to generate animated graphs used for dissemination in a festival (see https://festivaljeudeloie.fr/projet/pouvoir-et-vouloir-savoir-en-periode-dincertitude-la-dynamique-dune-pandemie/):

R code Animated-graphs-France.only.R uses data file sp-pos-quot-fra-2021-03-10-17h20.csv to generate daily departemental maps of the Acceleration Index in France

# Topic Modelling
Repository for topic modelling analyses in the project "Markets for Resilience or 'Disaster Capitalism'?".

We apply Structural Topic Models (STM; Roberts et al., 2013, 2014) to the corpus "Studies on Water", a series of
55 documents issued by the OECD (2009-2022). We use the R package *stm* (Roberts
et al. 2019) to fit STMs with different numbers of topics to the corpus and 
compare the models using different diagnostic criteria, such as semantic coherence  and exclusivity. 
We inspect the most probable and exclusive words for the estimated topics. 
We also extract the documents in which topics are most prevalent for qualitative inspection.
Finally, we estimate the effects of document covariates on topic prevalence.

## Structure
The structure of the repository is as follows:
- `notebooks/`: Contains initial jupyter notebooks for exploring the corpus and fitting Latent-Dirichlet-Allocation (LDA) and contextualized topic models (CTM). Only used for exploration but not included in the final analysis.
  - `contextualized-topic-models-body.ipynb`: Applies CTM to the main bodies of the corpus documents.
  - `contextualized-topic-models-summary.ipynb`: Applies CTM to the executive summaries of the corpus documents.
  - `exploratory-analysis.ipynb`: Explores the corpus with word counts and clouds.
  - `standard-lda-body.ipynb`: Applies LDA topic models to the main bodies of the corpus documents.
  - `standard-lda-summary.ipynb`: Applies LDA topic models to the executive summaries of the corpus documents.
- `scrape-corpus/`: Sub module for scraping the corpus text from PDF files.
- `stm-checkpoints/`: Saved STM fits for reloading.
  - Contains files for STMs with 114 (free), 38, 30, 9, and 8 topics.
  - `stm_search_from_38.RData`: Saved model search object for 38 +/- 10 topics.
- `stm-figures/`: Plots of the results from STMs with 30, 9, and 8 topics.
  - `*_diag`: Diagnostics plots.
  - `*_effects_*`: Effect plots for Year, Focus on Finance, or Study Type covariates on topic prevalence.
  - `*_topic_freq`: Topic prevalence distribution plots with top 5 most probable words.
- `stm-key-docs/`: Lists of top 3 documents where each topic is most prevalent for STMs with 30, 9, and 8 topics.
- `stm-topic-keywords/`: Lists of top 10 keywords for each topic according to word probability, FREX, Lift, and Score metrics for STMs with 30, 9, and 8 topics.
- `included_chapters.txt`: List of document chapters included in the STM analysis.
- `structural_topic_models.R` Script for running the STM analysis.
- `studies_on_water_metadata.csv`: Metadata for the corpus documents.

## Getting Started
To download and setup the repository, run:
```
git clone --recurse-submodules https://github.com/disaster-capitalism/topic-modelling.git

```
This will initialize the git submodule `scrape-corpus` and fetch its content.

## Requirements
Running the analyses requires the scraped text from the pdf files of the corpus "Studies on Water" issued by the OECD. The scraped text must be stored in a JSON file. Checkout the instructions on the submodule [page](https://github.com/disaster-capitalism/scrape-corpus) on how to obtain these JSON files.

The STM analysis in written in R (version 4.1.2). The following R packages are required and can be installed by running the R command:

```r

install.packages(
  c('rjson', 'dplyr', 'stringr', 'tidyr', 'ggplot2',
    'quanteda', 'stm', 'stminsights')
)

```

For specific details on package versions and reproducibility, see the `session_info.txt` file.

## Running the Analyses
To reproduce the STM analysis pipeline, execute the R script `structural_topic_models.R`. Within the file two parameters can be set:
- `json`: Relative path to the directory containing the JSON files with the scraped corpus text. The default is within in the submodule, i.e., `scrape-corpus/studies_on_water_scraped`.
- `fit`: Boolean whether to fit the structural topic models. If `FALSE`, saved model objects are loaded from the directory. Defaults to `FALSE`.

## References
OECD (2022). OECD Studies on Water. https://doi.org/10.1787/22245081

Roberts, M. E., Stewart, B. M., Tingley, D., & Airoldi, E. M. (2013, December). The structural topic model and applied social science. In *Advances in Neural Information Processing Systems Workshop on Topic Models: Computation, Application, and Evaluation* (Vol. 4, pp. 1-20).

Roberts, M. E., Stewart, B. M., Tingley, D., Lucas, C., Leder‐Luis, J., Gadarian, S. K., ... & Rand, D. G. (2014). Structural topic models for open‐ended survey responses. *American Journal of Political Science, 58*(4), 1064-1082.

Roberts, M. E., Stewart, B. M., & Tingley, D. (2019). stm: An R Package for Structural Topic Models. *Journal of Statistical Software, 91*(2), 1–40. https://doi.org/10.18637/jss.v091.i02
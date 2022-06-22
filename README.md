# Topic Modelling
Repository for topic modelling analyses in the project "Markets for Resilience or 'Disaster Capitalism'?".

## Setup
To download and setup the repository, run:
```
git clone --recurse-submodules https://github.com/disaster-capitalism/topic-modelling.git

```
This will initialize the git submodule `scrape-corpus` and fetch its content.


## Running the Analyses
Running the analyses requires the scraped text from the pdf files of the corpus "Studies on Water" issued by the OECD. The scraped text must be stored in a json file. Checkout the instructions on the submodule [page](https://github.com/disaster-capitalism/scrape-corpus) on how to obtain these json files.

To reproduce the structural topic modelling analysis pipeline, execute the Rmarkdown file `structural-topic-models.Rmd`. Within the file two parameters can be set:
- `json`: Relative path to the directory containing the json files with the scraped corpus text. The default is within in the submodule, i.e., `/scrape-corpus/studies_on_water_scraped`.
- `fit`: Boolean whether to fit the structural topic models. If `False`, saved model objects are loaded from the directory. Defaults to `False`.

## Output
The analysis pipeline creates several output files:
- `included_chapters.txt`: A text file listing the titles of the chapters that were included in the analyses.
- `stm_final_topic_labels.*`: A text and a csv file containing the top 10 topic keywords for the final topic model.
- `stm_fit_*.RData`: Saved structural topic model objects for reloading.
- `stm_search_from_base.RData`: Saved model search object starting from the base model.
- `structural-topic-models.html`: Html file containing the figures and output of the analyses.

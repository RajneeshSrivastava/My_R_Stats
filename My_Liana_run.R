#tutorial https://saezlab.github.io/liana/articles/liana_tutorial.html
library(tidyverse)
library(magrittr)
library(liana)
#CCC Resources
#liana provides CCC resources obtained and formatted via OmnipathR which are then converted to the appropriate format to each method.

# Resource currently included in OmniPathR (and hence `liana`) include:
#show_resources()
#Note that the different algorithms (or scoring measures) used in sca, natmi, connectome, cellphonedb, cytotalk’s crosstalk scores, and logfc were re-implemented in LIANA. Yet, the original method pipelines can be called via the call_* functions.

##liana wrapper function
#liana takes Seurat and SingleCellExperiment objects as input, containing processed counts and clustered cells.
liana_path <- system.file(package = "liana")
#testdata <-readRDS(file.path(liana_path , "testdata", "input", "testdata.rds"))
#testdata %>% dplyr::glimpse()
##### liana_wrap calls a number of methods and and each method is run with the provided resource(s).
# Run liana
liana_test <- liana_wrap(testdata)

## Liana returns a list of results, each element of which corresponds to a method
liana_test %>% dplyr::glimpse()

#LIANA currently provides a mixture of re-implemented methods and pipelines which externally call specific LR methods. By default, LIANA will call only the internal scoring function, i.e. those that are re-implemented in LIANA.
#One can use LIANA to also run the original methods. For more about the original methods see LIANA++.

#Aggregate and Obiain Consensus Ranks
## liana also provides consensus ranks for the results obtained using different methods. By default, liana will provide mean, median, and aggregate* consensus ranks

# We can aggregate these results into a tibble with consensus ranks
liana_test <- liana_test %>% liana_aggregate()
dplyr::glimpse(liana_test)

#Simple DotPlot
liana_test %>%
  liana_dotplot(source_groups = c("B"),
                target_groups = c("NK", "CD8 T", "B"),
                ntop = 20)

#Frequency Heatmap
liana_trunc <- liana_test %>%
   # only keep interactions concordant between methods
  filter(aggregate_rank <= 0.01) # this can be FDR-corr if n is too high

heat_freq(liana_trunc)

## Frequency Chord diagram
####Install
if(!require("circlize")){
  install.packages("circlize", quiet = TRUE,
                   repos = "http://cran.us.r-project.org")
}
library(circlize)
p <- chord_freq(liana_trunc,
                source_groups = c("CD8 T", "NK", "B"),
                target_groups = c("CD8 T", "NK", "B"))


#### Run any method of choice.
# Load Sce testdata
#sce <- readRDS(file.path(liana_path , "testdata", "input", "testsce.rds"))

# RUN CPDB alone
cpdb_test <- liana_wrap(sce,
                        method = 'cellphonedb',
                        resource = c('CellPhoneDB'),
                        permutation.params = list(nperms=100,
                                                  parallelize=FALSE,
                                                  workers=4),
                        expr_prop=0.05)

# identify interactions of interest
cpdb_int <- cpdb_test %>%
  # only keep interactions with p-val <= 0.05
  filter(pvalue <= 0.05) %>% # this reflects interactions `specificity`
  # then rank according to `magnitude` (lr_mean in this case)
  rank_method(method_name = "cellphonedb",
              mode = "magnitude") %>%
  # keep top 20 interactions (regardless of cell type)
  distinct_at(c("ligand.complex", "receptor.complex")) %>%
  head(20)

# Plot toy results
cpdb_test %>%
  # keep only the interactions of interest
  inner_join(cpdb_int, 
             by = c("ligand.complex", "receptor.complex")) %>%
  # invert size (low p-value/high specificity = larger dot size)
  # + add a small value to avoid Infinity for 0s
  mutate(pvalue = -log10(pvalue + 1e-10)) %>% 
  liana_dotplot(source_groups = c("c"),
                target_groups = c("c", "a", "b"),
                specificity = "pvalue",
                magnitude = "lr.mean",
                show_complex = TRUE,
                size.label = "-log10(p-value)")
                
                
 # Run liana re-implementations with the CellPhoneDB resource
complex_test <- liana_wrap(testdata,
                           method = c('natmi', 'sca', 'logfc'),
                           resource = c('CellPhoneDB'))
                           
complex_test %>% liana_aggregate()

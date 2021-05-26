##----------------------------------------------------------------------------##
# Create the Table with metadata for all datasets

# DatasetProtectionStatus:
# 0 = public datasets
# 1 = internal datasets, password protected
##----------------------------------------------------------------------------##

ds_table <- list( "Wu" = c(Dataset='Wu',
                           Organism='Human',
                           Organ='kidney',
                           CellNumber='4524',
                           CellsNuclei ='nuclei',
                           Comment="Wu et al., Cell Stem Cell 2018",
                           FileName="wu_scExplorer.rds",
                           Access="public"), 
                "Lake" = c(Dataset='Lake',
                           Organism='Human',
                           Organ='kidney',
                           CellNumber='17659',
                           CellsNuclei ='nuclei',
                           Comment="Lake et al., Nature Communications 2019",
                           FileName="lake_scExplorer.rds",
                           Access="CRC 1453 members"))
ds_table <- t(as.data.frame(ds_table))
ds_table <- as.data.frame(ds_table)



setwd("C:/Users/haugs/OneDrive/GenEpi/02_Projects/03_scExplorer/scExplorer/data/")
saveRDS(ds_table, "scExplorer_datasets.RDS")


            
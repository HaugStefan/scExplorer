ds_table <- list( "Wu" = c(Dataset='Wu',
                           Organism='Human',
                           Organ='kidney',
                           CellNumber='4524',
                           CellsNuclei ='nuclei',
                           Comment="Wu et al., Cell Stem Cell 2018",
                           FileName="wu_scExplorer.rds"), 
                "Lake" = c(Dataset='Lake',
                           Organism='Human',
                           Organ='kidney',
                           CellNumber='17659',
                           CellsNuclei ='nuclei',
                           Comment="Lake et al., Nature Communications 2019",
                           FileName="lake_scExplorer.rds"))
ds_table <- t(as.data.frame(ds_table))
ds_table <- as.data.frame(ds_table)



setwd("C:/Users/haugs/OneDrive/GenEpi/02_Projects/NephGen/scExplorer/scExplorer/data")
saveRDS(ds_table, "scExplorer_datasets.RDS")


            
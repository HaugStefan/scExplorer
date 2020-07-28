setwd("C:/Users/haugs/OneDrive/GenEpi/02_Projects/NephGen/scExplorer/scExplorer/data")

wu <- readRDS("wu_scExplorer_old.rds")

wu_new <- c("01 Podocyte", "02 PT(S1)", "03 PT(S2)", "04 PT(S3)", "05 LH(DL)", 
            "06 LH(AL#1)", "07 LH(AL#2)", 
            "08 DCT", "09 CNT", "10 PC", "11 IC-A", 
            "12 IC-B", "13 Mesangium", "14 EC", "15 Macrophage", 
            "16 Undefined#1", "17 Undefined#2")

names(wu_new) <- c("Podocyte", "PT(S1)", "PT(S2)", "PT(S3)", 
                   "LH(DL)", "LH(AL#1)", "LH(AL#2)", "DCT", 
                   "CNT", "PC", "IC-A",  "IC-B", "EC", "Mesangium", 
                    "Macrophage", "Undefined#1", "Undefined#2")

wu_new_list <- c("01 Podocyte", "02 PT(S1)", "03 PT(S2)", "04 PT(S3)", "05 LH(DL)", 
                 "06 LH(AL#1)", "07 LH(AL#2)", 
                 "08 DCT", "09 CNT", "10 PC", "11 IC-A", 
                 "12 IC-B", "13 EC", "14 Mesangium",  "15 Macrophage", 
                 "16 Undefined#1", "17 Undefined#2")

for (cluster in names(wu_new)) {
  levels(wu$cells$cluster)[levels(wu$cells$cluster) == cluster] <- wu_new[[cluster]]
  levels(wu$marker_genes$Cluster)[levels(wu$marker_genes$Cluster) == cluster] <- wu_new[[cluster]]
  wu$cluster_names <- wu_new_list
  
}

saveRDS(wu, "wu_scExplorer.rds")
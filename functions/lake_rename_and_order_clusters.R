setwd("C:/Users/haugs/OneDrive/GenEpi/02_Projects/NephGen/scExplorer/scExplorer/data")

lake <- readRDS("lake_scExplorer_old.rds")

lake_new <- c("01 EPC",
              "02 POD",
              "03 PT-1",
              "04 PT-2",
              "05 PT-3",
              "06 PT-4",
              "07 PT-5",
              "08 DTL",
              "09 ATL-1",
              "10 ATL-2",
              "11 ATL-3",
              "12 TAL-1",
              "13 TAL-2",
              "14 DCT",
              "15 CNT",
              "16 PC-1",
              "17 PC-2",
              "18 PC-3",
              "19 IC-A2",
              "20 IC-A1",
              "21 IC-B",
              "22 EC-1",
              "23 EC-2",
              "24 EC-3",
              "25 EC-4",
              "26 MC",
              "27 vSMC/P",
              "28 INT",
              "29 Unk",
              "30 IMM")

names(lake_new) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

lake_new_list <- c("01 EPC",
              "02 POD",
              "03 PT-1",
              "04 PT-2",
              "05 PT-3",
              "06 PT-4",
              "07 PT-5",
              "08 DTL",
              "09 ATL-1",
              "10 ATL-2",
              "11 ATL-3",
              "12 TAL-1",
              "13 TAL-2",
              "14 DCT",
              "15 CNT",
              "16 PC-1",
              "17 PC-2",
              "18 PC-3",
              "19 IC-A2",
              "20 IC-A1",
              "21 IC-B",
              "22 EC-1",
              "23 EC-2",
              "24 EC-3",
              "25 EC-4",
              "26 MC",
              "27 vSMC/P",
              "28 INT",
              "29 Unk",
              "30 IMM")

for (cluster in names(lake_new)) {
  levels(lake$cells$cluster)[levels(lake$cells$cluster) == cluster] <- lake_new[[cluster]]
  # convert markergene clusternames to factor first (were a list, since old clusternames are only numbers)
  lake$marker_genes$Cluster <- as.factor(lake$marker_genes$Cluster)
  levels(lake$marker_genes$Cluster)[levels(lake$marker_genes$Cluster) == cluster] <- lake_new[[cluster]]
  lake$cluster_names <- lake_new_list
  
}

saveRDS(lake, "lake_scExplorer.rds")
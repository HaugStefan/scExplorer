# ---------------------------------------------------------------------#
# Wu Processing
# ---------------------------------------------------------------------#
# Run "FindAllMarkers" with standard settings
# Run umap with standard settings.

# exportFromSeurat(wu, assay = "RNA", "wu_scExplorer.rds", "wu", "human", 
#                  column_sample = "orig.ident", 
#                  column_cluster = "Cell.Type",
#                  column_nUMI = "nUMI",
#                  column_nGene = "nGene",
#                  add_all_meta_data = FALSE)

# ---------------------------------------------------------------------#
# Lake Processing
# ---------------------------------------------------------------------#

# load lake: load("D:/GenEpi/Data/Lake_Dataset_Adult_Normal_Kidney_snDrop_Lake2019_NCOMM_Seuratv3.Robj")
# mg <- read_excel("D:/GenEpi/Data/Lake_MarkerGenes_edited.xlsx")
# ank.3@misc$marker_genes <- mg
exportFromSeurat(ank.3, assay = "RNA", "lake_scExplorer.rds", "Lake", "Human", 
                 column_sample = "orig.ident", 
                 column_cluster = "cluster_ID",
                 column_nUMI = "nCount_RNA",
                 column_nGene = "nFeature_RNA",
                 add_all_meta_data = FALSE)
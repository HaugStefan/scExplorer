##----------------------------------------------------------------------------##
## Tab: Marker genes.
##----------------------------------------------------------------------------##

tab_marker_genes <- tabItem(
  tabName = "marker_genes",
  cerebroBox(
    title = tagList(
      boxTitle("Marker genes per cluster"),
      #cerebroInfoButton("marker_genes_by_cluster_info")
    ),
    uiOutput("marker_genes_by_cluster_UI")
  )
)

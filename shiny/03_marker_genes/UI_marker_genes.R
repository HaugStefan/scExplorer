##----------------------------------------------------------------------------##
## Tab: Marker genes.
##----------------------------------------------------------------------------##

tab_marker_genes <- tabItem(
  tabName = "marker_genes",
    tagList(
      cerebroBox(
        title = tagList(
            "Marker genes per cluster ",
            #cerebroInfoButton("marker_genes_by_cluster_info")
            textOutput(inline = TRUE, "selected_dataset_short_markergenes")
        ),
        tagList(
          uiOutput("marker_genes_by_cluster_UI")
        )
      )
  )
)

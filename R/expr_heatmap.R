#' expr_heatmap
#'
#' \code{expr_heatmap} returns a heatmap for the supplied matrix
#'
#' @param expr_matrix A numeric matrix of gene expression values
#' @param scale character one of 'scale', 'log10'
#' @param legend_position character (one of 'left'[default], 'right', 'bottom', 'top')
#' @param row_labels character an optional character vector to allow for non-unique row labels
#'
#' @return a ggplot heatmap object
#'
#' @examples
#' expr_heatmap( matrix, legend_position = 'right')
#'
#' @export
expr_heatmap <- function( expr_matrix, scale = NULL,
                          colour_scale = 'viridis',
                          legend_position = 'left', row_labels = NULL ){
  # centre/scale
  if (!is.null(scale)) {
    if (scale == "scale") {
      expr_matrix <- t( scale( t(expr_matrix) ) )
    } else if( scale == "log10" ){
      expr_matrix <- log10(expr_matrix + 1)
    }
  }
  # reshape data for heatmap
  expr_matrix_m <- as.data.frame(expr_matrix) %>%
    tibble::rownames_to_column(., var = 'GeneID') %>%
    tidyr::pivot_longer(., -GeneID,
                        names_to = "Sample", values_to = "Value")

  # reorder levels of gene id and labels if provided
  expr_matrix_m$GeneID <- forcats::as_factor(expr_matrix_m$GeneID)
  expr_matrix_m$GeneID <- forcats::fct_rev(expr_matrix_m$GeneID)
  # order levels of Sample in the order they appear
  expr_matrix_m$Sample <- forcats::as_factor(expr_matrix_m$Sample)

  if ( !is.null(row_labels) ) {
    row_labels <- rev(row_labels)
  }

  Heatmap <- ggplot2::ggplot(data = expr_matrix_m) +
    ggplot2::geom_raster( ggplot2::aes( y = GeneID, x = Sample, fill = Value ) )
  if (colour_scale %in% c('viridis', 'plasma', 'magma', 'inferno') ) {
    Heatmap <- Heatmap + ggplot2::scale_fill_viridis_c(option = colour_scale)
  } else if (colour_scale == 'byr') {
    Heatmap <- Heatmap +
      ggplot2::scale_fill_gradientn( colours = c("blue", "yellow", "red") )
  }

  if ( is.null(row_labels) ) {
    Heatmap <- Heatmap + ggplot2::theme_void() +
      ggplot2::theme( legend.position=legend_position,
                      legend.title = ggplot2::element_text(colour="black" ) )
  } else {
    Heatmap <- Heatmap +
      ggplot2::scale_y_discrete( labels = row_labels ) +
      ggplot2::theme_void() +
      ggplot2::theme( legend.position=legend_position,
                      legend.title = ggplot2::element_text(colour="black" ),
                      axis.text.y = ggplot2::element_text(colour="black" ) )

  }
  return( Heatmap )
}

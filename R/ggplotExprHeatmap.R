#' ggplotExprHeatmap
#'
#' \code{ggplotExprHeatmap} returns a heatmap for the supplied matrix
#'
#' @param exprMatrix A numeric matrix of gene expression values
#' @param logCounts logical should the counts be logged first
#' @param legendPosition character (one of 'left'[default], 'right', 'bottom', 'top')
#'
#' @return a ggplot heatmap object
#'
#' @examples
#' ggplotExprHeatmap( matrix, legendPosition = 'right')
#'
#' @export
ggplotExprHeatmap <- function( exprMatrix, logCounts = FALSE, legendPosition = 'left' ){
  # log counts if required
  if( logCounts ){
    exprMatrix <- log10(exprMatrix + 1)
  }
  # reshape data for heatmap
  exprMatrix.m <- reshape2::melt(exprMatrix)
  colnames(exprMatrix.m) <- c("Gene", "Sample", "Value")
  # reorder levels of gene
  exprMatrix.m$Gene <- factor( exprMatrix.m$Gene,
                               levels = rev(levels(exprMatrix.m$Gene)) )

  Heatmap <- ggplot2::ggplot(data = exprMatrix.m) +
    ggplot2::geom_raster( ggplot2::aes( y = Gene, x = Sample, fill = Value ) ) +
    ggplot2::scale_fill_gradientn( colours = c("blue", "yellow", "red") ) +
    ggplot2::theme_void() +
    ggplot2::theme( legend.position=legendPosition,
                    legend.title = ggplot2::element_text(colour="black" ) )
  return( Heatmap )
}

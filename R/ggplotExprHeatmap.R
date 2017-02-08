#' ggplotExprHeatmap
#' 
#' \code{ggplotExprHeatmap} returns a heatmap for the supplied matrix
#'    
#' @param exprMatrix A numeric matrix of gene expression values
#' @param legendPosition character (one of 'left'[default], 'right', 'bottom', 'top')
#' 
#' @return a ggplot heatmap object
#' 
#' @examples
#' ggplotExprHeatmap( matrix, legendPosition = 'right')
#' 
#' @export
ggplotExprHeatmap <- function( exprMatrix, legendPosition = 'left' ){
  # reshape data for heatmap
  exprMatrix.m <- melt(exprMatrix)
  colnames(exprMatrix.m) <- c("Gene", "Sample", "Value")
  # reorder levels of gene
  exprMatrix.m$Gene <- factor( exprMatrix.m$Gene,
                               levels = rev(levels(exprMatrix.m$Gene)) )
  
  Heatmap <- ggplot(data = exprMatrix.m) + 
    geom_raster( aes( y = Gene, x = Sample, fill = Value ) ) + 
    scale_fill_gradientn( colours = c("blue", "yellow", "red") ) + 
    theme_void() + theme( legend.position=legendPosition,
                          legend.title = element_text(colour="black" ) )
  return( Heatmap )
}

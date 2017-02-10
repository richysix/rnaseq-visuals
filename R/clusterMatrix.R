#' hierarchical clustering with optimal ordering
#'
#' \code{cluster} hierarchically clusters the supplied matrix by column and
#' orders the leaves optimally
#'
#' @param counts Matrix of counts to cluster
#'
#' @return matrix with columns reordered based on clustering
#'
#' @examples
#' cluster( matrix )
#'
#' @export
#'
cluster <- function( counts ){
  # centre and scale numbers
  scaledCounts <- scale(counts)
  distanceMatrix <- as.dist( (1 - cor(scaledCounts))/2 )
  # cluster and reorder correlation matrix
  hClust <- hclust(distanceMatrix)
  ## find optimal ordering of leaves
  #optOrder <- order.optimal(distanceMatrix, hClust$merge)
  ## reorder count martix and return
  #newClust <- hClust
  #newClust$merge <- optOrder$merge
  #newClust$order <- optOrder$order
  # newOrder <- seriate(distanceMatrix, method='OLO')

  # return( counts[ , get_order(newOrder) ] )
  return( counts[ , hClust$order] )
}

#' hierarchical cluster matrix by rows or columns (or both)
#'
#' \code{clusterMatrix} hierarchically clusters the supplied matrix by row or column
#' or both
#'
#' @param counts Matrix of counts to cluster
#' @param byRow logical - whether to cluster the rows of the matrix
#' @param byCol logical - whether to cluster the columns of the matrix
#'
#' @return reordered matrix based on clustering
#'
#' @examples
#' clusterMatrix( matrix )
#'
#' clusterMatrix( matrix, byRow = FALSE, byCol = TRUE )
#'
#' @export
#'
clusterMatrix <- function( counts, byRow = TRUE, byCol = FALSE ){
  # cluster columns
  if( byCol ){
    counts <- cluster( counts )
  }
  # cluster rows
  if( byRow ){
    counts <- t( cluster( t(counts) ) )
  }
  return( counts )
}

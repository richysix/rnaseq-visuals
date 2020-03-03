#' Convert a numeric matrix of features (rows) and conditions (columns) with
#' raw feature counts to transcripts per million.
#'
#'    This function is from https://gist.github.com/slowkow/c6ab0348747f86e2748b#file-counts_to_tpm-r
#'    Convert counts to transcripts per million (TPM).
#'
#'    Lior Pachter. Models for transcript quantification from RNA-Seq.
#'    arXiv:1104.3889v2
#'
#'    Wagner, et al. Measurement of mRNA abundance using RNA-seq data:
#'    RPKM measure is inconsistent among samples. Theory Biosci. 24 July 2012.
#'    doi:10.1007/s12064-012-0162-3
#'
#' @param counts A numeric matrix of raw feature counts i.e.
#'  fragments assigned to each gene.
#' @param featureLength A numeric vector with feature lengths.
#'  This should be the same length as the number of features
#' @param meanFragmentLength A numeric vector with mean fragment lengths.
#'  This should be the same length as the number of samples
#' @return tpm A numeric matrix normalized by library size and feature length
#'
#' @examples
#' set.seed(1785)
#' counts <- matrix(sample(20:200, size = 100), ncol = 5)
#' featureLength <- sample(200:1000, size = 20)
#' meanFragmentLength <- sample(200:300, size = 5)
#' countsToTPM(counts, featureLength, meanFragmentLength)
#'
#' @export
#'
countsToTPM <- function(counts, featureLength, meanFragmentLength) {

  # Ensure valid arguments.
  stopifnot(length(featureLength) == nrow(counts))
  stopifnot(length(meanFragmentLength) == ncol(counts))

  # Compute effective lengths of features in each library.
  effLen <- do.call(cbind, lapply(1:ncol(counts), function(i) {
    featureLength - meanFragmentLength[i] + 1
  }))

  # Exclude genes with length less than the mean fragment length.
  idx <- apply(effLen, 1, function(x) min(x) > 1)
  counts <- counts[idx,]
  effLen <- effLen[idx,]
  featureLength <- featureLength[idx]

  # Process one column at a time.
  tpm <- do.call(cbind, lapply(1:ncol(counts), function(i) {
    rate = log(counts[,i]) - log(effLen[,i])
    denom = log(sum(exp(rate)))
    exp(rate - denom + log(1e6))
  }))

  # Copy the column names from the original matrix.
  colnames(tpm) <- colnames(counts)
  return(tpm)
}

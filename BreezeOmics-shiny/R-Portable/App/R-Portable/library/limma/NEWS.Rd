\name{limmanews}
\title{limma News}
\encoding{UTF-8}


\section{Version 3.54.1}{\itemize{

  \item
  Rename readSampleInfoFromGEO() to sampleInfoFromGEO() keeping legacy
  name as well.

  \item
  Add new argument `p.value` for topGO() and topKEGG().

  \item
  eBayes() now checks whether `fit` is a list object before
  undertaking other tests.

  \item
  Revise voom.Rd to specify that y-values in voom plot are sqrt
  residual standard deviations.

  \item
  New function goanaTrend() to estimate a covariate-dependent trend in
  the probability of differential expression for use in goana() and
  kegga() with `trend=TRUE`. The trend estimated by goanaTrend() is
  squeezed slightly towards constancy to provide stability when the
  number of DE genes is small. The amount of squeezing decreases with
  the number of DE genes.

  goana() and kegga() now call goanaTrend() when the `trend` argument
  is used. When `plot=TRUE` the plot is now created by goanaTrend()
  instead of by barcodeplot().

  \item
  Rename argument `prior.prob` to `null.prob` in goana() and kegga().

  \item
  Update goana() and kegga() code to catch NA gene IDs or covariate
  values.

  \item
  goana() and kegga() no longer give an error when a trend is
  estimated but there are no DE genes.

  \item
  kegga() now uses https instead of http links when reading from rest.kegg.jp.

  \item
  Add new argument `fc` to topTable() and topTableF().
  Change default for `lfc` to NULL instead of 0.

  \item
  lmFit() now checks explicitly for NAs in the design matrix.

  \item
  Update references listed on help pages to use DOIs instead of URLs.

}}


\section{Version 3.52.0}{\itemize{

  \item
  New function readSampleInfoFromGEO().

  \item
  Allow the `trend` argument of eBayes() to specify a general variance
  covariate.

  \item

  More detailed checking and error messages when the `object` input to
  lmFit() is a data.frame.

  \item
  Improve checking for incorrectly specified `contrasts` argument in
  contrasts.fit().

}}


\section{Version 3.50.0}{\itemize{

  \item
  Improve help pages for fry() and barcodeplot().

  \item
  Revise Section 18.1.10 of User's Guide so that coloring of
  X and Y genes is consistent between plots.

  \item
  Fix bug in the MDS class method of plotMDS() (introduced in the previous release) when new `dim.plot`
  values are set.

  \item
  Fix bug in read.idat() when `annotation` contains NA values.

}}


\section{Version 3.48.0}{

  \subsection{New functionality}{\itemize{

    \item
    Explicitly setting `weights=NULL` in a call to lmFit() no longer
    over-rides the `weights` value found in `object`.
    Default settings in lmFit() changed from `ndups=` and `spacing=1`
    to `ndups=NULL` and `spacing=NULL`, although this doesn't change function behavior
    from a user point of view.

    \item
    A number of improvements to duplicateCorrelation() to make the results more robust
    and to make the interface consistent with lmFit().
    duplicateCorrelation() sets `weights` same as lmFit(). Setting
    `weights=NULL` in the function call no longer overwrites weights
    found in `object`.
    duplicateCorrelation() now checks whether the block factor is
    spanned by the design matrix. If so, it returns intrablock
    correlations of zero with a warning. Previously this usage error
    was not specifically trapped and could lead to correlations that
    were or NA or close to 1 depending on floating point errors.
    duplicateCorrelation() now issues a simplified message when design
    is not of full rank and uses message() to do so instead of cat().
    In terms of output, duplicateCorrelation() now bounds the genewise correlations away
    from the upper and lower bounds by 0.01 so that the correlation
    matrix will always be positive-definite.
    There is also a fix to the value returned by duplicateCorrelation() when no blocks or
    duplicates are present.

    \item
    New argument `fc` for treat() so that the fold-change threshold can
	optionally be specified on the fold-change scale rather than as a
	log2-fold-change.

    \item
    plotMDS() no longer calls cmdscale() but instead performs the necessary
    eigenvector computations directly. Proportion of variance explained
    by each dimension is now computed and is optionally added to the dimension labels.
    The `ndim` argument is now
    removed. All eigenvectors are now stored so that plotMDS.MDS does
    not need to recompute them when different dimensions are plotted.

    \item
    New arguments `path` and `bgxpath` for read.idat().
    read.idat() now checks for gzipped IDAT files and, if detected,
    gives an informative error message.
    read.idat() now checks for existence of input files before calling
    illuminaio read functions.

  }}

  \subsection{Other code improvements}{\itemize{

    \item
    The average log-expression column written by write.fit() now has
	column heading "AveExpr" to match the output from topTable().
	The column was previously called "A".
 
  }}

  \subsection{Documentation}{\itemize{

    \item
    Additional documentation for the `design` argument of `lmFit`
    using the term "samples" instead of "arrays" and mentioning that
    the design matrix defaults to `object$design` when that component
    is not NULL.
 
    \item
    duplicateCorrelation() help page revised including new code example.
 
    \item
    Help page for voom() now explains that the design matrix will be
    set from the `group` factor of the DGEList object if available.
 
    \item
    coolmap() help page now clarifies which heatmap.2() arguments are
    reserved and which can be included in the coolmap call.
 
  }}

  \subsection{Bug fixes}{\itemize{
 
    \item
    Fix typo in voom() warning when negative counts are detected.
 
  }}
}


\section{Version 3.46.0}{

  \subsection{New functionality}{\itemize{

    \item
    New function chooseLowessSpan().

    \item
    fitFDist() with a non-NULL `covariate` now fits a smoother trend
    (with fewer spline knots) than before unless there are at least
    30 observations. This also affects squeezeVar() and eBayes() with
    `trend=TRUE`.

    \item
    New argument `output.style` to weightedLowess().

    \item
    write.fit() now outputs a blank column heading for the row names
    so that the number of column names in the delimited output file
    will agree with the number of columns (similar to write.csv in the
    base package).

  }}

  \subsection{Code improvements}{\itemize{

    \item
    Subsetting and contrasts.fit() now work on MArrayLM objects even
    if the cov.coefficients component is absent.

    \item
    volcanoplot() now prompts user to run eBayes() if the object
    doesn't contain p-values.

    \item
    Add checks and more informative error messages to getEAWP() and
    lmFit() when the data object is missing, NULL or has zero rows.

    \item
    Subsetting for RGList, MAList, EListRaw, EList, MArrayLM or
    TestResults objects now accepts arguments other than `i` or `j`
    but ignores them without an error. This is relevant if a user
    adds a `drop` argument by analogy with matrix subsetting. Previously
    that produced an error.

    \item
    Subsetting of TestResults now requires two arguments, same as all
    of the other data object classes listed above. Previously single
    index subsetting (for example object[i]) was allowed.

    \item
    Improved treatment of zero weights by weighted.median().

    \item
    More consistent use of rep.int(), rep_len() and rep() throughout
    the package.

    \item
    Replace NA with NA_real_ where appropriate.

    \item
    Add a message to topTableF() to warn that the function is
    obsolete and will be removed in a future version of limma.
    Remove usages of topTableF() from the User's Guide.

    \item
    Remove obsolete function toptable(), which has been officially 
    deprecated in favor of topTable() since 1 Feb 2018.

  }}

  \subsection{Documentation}{\itemize{

    \item
    Revise and expand the weightedLowess help page.

    \item
    Add more explanation about "heirarchical" method in the details
    section of the decideTests() help page.

    \item
    Add Note to voom help page to emphasise that voom is not an appropriate
    tool to analyse expresssion measures that have been normalized for library size.

    \item
    Revise the help page for plotMDS() to clarify that the function can
    produce PCA plots as well as PCoA plots.

    \item
    Add example to EList-class help page.

    \item
    Edits to the eBayes help page.

    \item
    Edit concept entries for help files (for reference manual index).

  }}

  \subsection{Bug fixes}{\itemize{

    \item
    Revise write.fit() so that column names of the output file are
    correct when `digits=NULL` and the `fit` object contains only one
    coefficient column. Previously the same contrast name was repeated
    as the column name for the coefficients, t-statistics and p.values,
    making them hard to distinguish.
 
    \item
    Bug fix to arrayWeights() when `y` contains NAs, the design matrix
    has several columns and some but not all genes have no residual df.

  }}
}


\section{Version 3.44.0}{

  \subsection{New functionality}{\itemize{

    \item
    Add new argument `package` to changeLog(). changeLog() now works
    for any package rather than only for limma.

  }}

  \subsection{Code improvements}{\itemize{

    \item
    Improved treatment of NA coefficients by contrasts.fit().
    contrasts.fit() has always removed coefficients that were NA because
    of singularities in the design matrix, but any extra NA coefficients
    caused by NA expression values would cause all the stdev.unscaled
    values returned by contrasts.fit() to be NA for those genes.
    contrasts.fit() now returns non-NA coefficients and
    non-NA stdev.unscaled values if all the NA coefficients are
    multiplied by zero contrast multipliers.

    \item
    Speed improvement for contrasts.fit() if some input coefficients are
    not involved (have zero multipliers) in any of the contrasts.

    \item
    contrasts.fit() now preserves the `pivot` component of the fit
    object instead of removing it.

    \item
    voom now checks explicitly for NA or negative counts and gives an
    informative error message.

    \item
    fitFDist() now allows a covariate trend even with fewer than 4
    useable observations. (Useable means positive residual df and
    finite variance and covariate values.)

    \item
    Internal code cleaned up to avoid partial matching of function
    arguments, attributes or list component names. The automatic package
    tests are now run with the warnPartialMatchArgs,
    warnPartialMatchAttr and warnPartialMatchDollar options all set to
    TRUE.

  }}

  \subsection{Documentation}{\itemize{

    \item
    More explanation of `covariates` argument to removeBatchEffects().

  }}

  \subsection{Bug fixes}{\itemize{
 
    \item
    Fix bug in contrasts.fit() when some coefficients are NA due to NA
    expression values and the corresponding contrast multipliers are
    zero. The new code fixes the improvement originally introduced in
    limma 3.35.9. The new code will return non-NA contrasts if the NA
    coefficients all get 0 weight in the contrast. Previous code was
    correct if all genes had the same NA coefficients but could give an
    avoidable NA result if some genes had NA coefficients but the
    first gene did not.

    \item
    Improvements to subsetting MArrayLM objects by column. Previously
    the presence of non-estimable coefficients sometimes caused the
    cov.coefficient matrix to be subsetted incorrectly; that is now
    fixed. Subsetting zero columns is now allowed even when F-statistics
    are present.

    \item
    Fix to classifyTestsF() when one or more of the coefficients have
    zero variance. Previously the zero variance will introduce NAs into
    the correlation matrix and hence cause an error. This is now
    avoided. This also fixes eBayes() when one or more of the
    coefficients are identically zero (usually caused by an all-zero
    contrast).

  }}
}


\section{Version 3.42.0}{

  \subsection{New functionality}{\itemize{

    \item
     New head() and tail() methods for all limma data classes.

    \item
    New unique() and subsetting methods for TestResults objects.
    Previously a
    subsetted TestResults object became an ordinary numeric matrix, but
    now the TestResults class is preserved. Single index subsetting is
    also allowed but produces a numeric vector.

    \item
    New argument `gene.weights` for fry() so as to match the arguments
    and behavior of mroast(). Previously gene weights could be input to
    fry() only through the `index` argument.
    fry's mixed p-values now take account of gene weights.
    Previously gene weights were not used in fry's mixed
    p-value calculation.

    \item
    Add arguments `block`, `correlation` and `weights` to voom() and
    remove the `...` argument.

    \item
    Update the arguments of voomWithQualityWeights() to match the
    revisions to arrayWeights() in limma 3.40.0.
    Add new argument `var.group`.

    \item
    New arguments `xlab`, `ylab` and `main` for plotFB(). The default
    plot title now includes the column name from the data object.
    Default for `pch` increased to 0.3.

    \item
    plotWithHighlights and plotMD.MArrayLM now gives special treatment
    to the case that `status` is a TestResults object.

  }}

  \subsection{Code improvements}{\itemize{

    \item
    roast() and mroast() now use less memory. Memory usage now
    remains bounded regardless of the number of rotations used.
    Both functions are faster than before when \code{approx.zscore=TRUE}.
    A new argument `legacy` is added to allow users to turn these improvements
    off if they wish to reproduce numerical results from limma 3.40.0 exactly.
    The default number of rotations `nrot` is increased  
    from 999 to 1999.
    A bug has been fixed for roast() and mroast() when `index` is a character vector of geneids or
    rownames. Previously this usage failed.

    \item
    Complete rewrite of zscoreT(), which is now somewhat faster and
    offer two new options for the normalizing transformation. The new
    argument `method` indicates which transformation is used when
    `approx=TRUE`. The default approximation method is changed from
    "hill" to "bailey".
    zscoreT() now allows NA values when `approx=TRUE` and works
    correctly for all valid df values.  Previously `approx=TRUE`
    returned NA results for `df` infinite or <= 0.05. A simple robust
    approximation from Wallace (1959) has been introduced to handle
    very large or very small `df` values. Previously `approx=FALSE`
    treated any `df` greater than 10000 as infinite; this threshold is
    now raised to 1e300.

    \item
    tZScore() is slightly faster.

    \item
    All uses of the approx() function now set
    `ties=list("ordered",mean)`, except for fitFDistRobustly()
    which uses `ties=mean`.
    The change was prompted by a change in
    R 3.6.0 whereby using the default for `ties` generates a warning
    message whenever ties are present in `x`. The new code gives the
    same results but is very slightly faster and avoids the warning
    message.
    limma now depends on R >= 3.6.0 because of this change.

  }}

  \subsection{Documentation}{\itemize{

    \item
    Update Yoruba case study in User's Guide to call voom() and
    duplicateCorrelation() twice each instead of once.

    \item
    Update Users Guide: update Rsubread reference, correct remarks
    about prior distribution for 1/sigma^2 in Section 13.2 and rerun
    the Yoruba and Pasilla case studies.

    \item
    Use "RNA-seq" and "ChIP-seq" consistently in the documentation instead of
    "RNA-Seq" and "ChIP-Seq".

  }}

 \subsection{Bug fixes}{\itemize{

   \item
   Fix bug in barcodeplot() when `index` or `index2` are character
   vectors.

  }}
}


\section{Version 3.40.0}{\itemize{

\item
Major rewrite of arrayWeights() to improve speed and stability.
  The arrayWeightsSimple() function has been removed and its
  functionality incorporated into arrayWeights().
A new 'var.group' argument has been added to simplify specification of the 
  variance design matrix.
The weights are now squeezed slightly towards unit and a new argument
  'prior.n' has been added to control the prior weight with which the array weights are squeezed.
arrayWeights() now chooses between the REML and gene-by-gene algorithms automatically by default. REML is chosen
  when there are no prior weights or missing values and gene-by-gene
  is used otherwise.
arrayWeights() now checks for and skips over any genes with zero
  residual variances.

\item
voom() now checks for experiments that have no replication. In this
  case it now returns weights that are all 1, with an informative
  warning but no error.
Two references have been added to the voom help page.

\item
classifyTests.Rd now focuses on nested F-tests.
The function FStat() is now removed. It was a convenience wrapper
  for classifyTestsF() with fstat.only=TRUE but is not used often
  enough to justify itself.
classifyTestsP() is no longer exported as it is not intended to be
  called by users.
The function classifyTestsT() is now removed, having been
  superceded by more sophisticated multiple testing methods.

\item
contrasts.fit() now removes any test statistics or p-values found
  in the fit object. This avoids any potential mis-match between coefficients and
  test statistics if eBayes() has been run on the object previously.

More details have been added to the warning about approximations in the
  contrasts.fit() help page.

\item
The old function ebayes(), which was deprecated a year ago in favor
  of eBayes(), is now removed.

\item
The fitted() and residuals() methods for MArrayLM objects now give
an informative error message if the object contains contrasts
  instead of the original coefficients.

\item
Rewrite Filtering chapter of User's Guide.

\item
Add Research Fields to the biocViews entry in the package
  DESCRIPTION file.
}}


\section{Version 3.38.0}{\itemize{

\item
New function plotExonJunc() to plot results from diffSplice().

\item
New function logsumexp().

\item
New argument hl.col for volcanoplot(), allowing users to specify
  the color for the gene names when highlight > 0.

\item
barcodeplot() no longer assumes that 'statistic'
  has unique names. Previously it returned an error if
  names(statistic) contained any duplicated values.

\item
The colors "blue", "red" and "yellow" used by coolmap() changed to
  "blue2", "red2" and "yellow2" when used in a color panel with white.

\item
goana.Rd now explains more explicitly that p-values are unadjusted
  for multiple testing.

\item
arrayWeights.Rd now mentions minimum dimensions for expression
  object.

\item
More advice on how to choose 'lfc' added to the treat() help page.

\item
Minor bug fix to the mixed p-value from roast() and mroast() when
  set.statistic="floormean".

\item
Bug fix for cumOverlap(), which was under-counting overlaps in some
  cases.

}}


\section{Version 3.36.0}{\itemize{

\item
New arguments 'quote' and 'row.names' for write.fit(). Write.fit()
  now outputs row.names by default, which previously were suppressed.

\item
coolmap() will now accept an arbitrary vector of colors. A new
  preset "whitered" panel is now also supported.

\item
In the past, contrasts.fit() always returned NA contrasts for genes
  with any NA fitted coefficients. contrasts.fit() will now ignore an
  NA coefficient if the contrast multiplier is zero for that
  coefficient.

\item
The treat() default for 'lfc' changed from lfc=0 to lfc=log2(1.2).

\item
kegga() and goana() now check whether a data.frame has been input
  by mistake and generates an error. Previously a data.frame value
  for 'de' was interpreted as a list of gene sets without any error.

\item
voomWithQualityWeights() now returns 'sample.weights' as a column
  of the 'targets' data.frame instead of as a separate vector.

\item
"TestResults" objects now include a 'labels' attribute, defaulting
  to c("Down","NotSig","Up").

\item
Functions ebayes() and toptable(), long ago replaced by eBayes() and
topTable(), are now formally deprecated.

\item
Update the User's Guide case study that analyses Lrp- E. Coli
  samples and profiled by Affymetrix arrays.

\item
Update the Agilent single-channel case study in Section 17.4 of the
  Users' Guide to use the Agilent gIsWellAboveBG detection column.

\item
Comments added to voom.Rd and eBayes.Rd to clarify the relationship
  between limma-trend and voom.

\item
In the kegga help page, clarify default gene ID system used by
kegga() when species="dme".

\item
Update wsva() help page to describe correct number of columns for output.

\item
bug fix for goana() when 'covariate' is specified and some of the
  'universe' genes don't have GO annotation.

\item
Fix to how roast(), mroast() and fry() handle weights. The 'weight' argument
can now be a matrix, or a vector of length nrow(y), or a vector of length ncol(y).
This allows the functions to accept precision weights, or gene weights, or sample weights.

}}


\section{Version 3.34.0}{\itemize{

\item 
read.idat() can now handle idat files in SNP format.

\item
New argument 'sort.by' for topSplice().

\item
diffSplice() now treats any NA geneid as a separate gene with one
  exon. Previously all NA geneids were treated as the same gene.

\item 
Bug fixes for plotSplice() which, in some circumstances, was not
  highlight significant exons.

\item
Default value for 'style' in volcanoplot() changed to "p-value"
  instead of "p". This doesn't change the behavior of the function.

\item
volcanoplot() didn't work with MArrayLM objects produced by
  treat(). Now fixed.

\item 
The gene.info.file argument of alias2SymbolUsingNCBI() will now
  optionally accept a data.frame instead of a file name.

\item
alias2SymbolUsingNCBI() now always produces a data.frame.
  Previously a single-column data.frame was returned as a vector.

\item
Bug fixes to camera() and cameraPR() when 'index' contains character row names.

\item
New argument 'restrict.universe' for the default kegga() method.
  The default is now not to the restrict the universe to genes with
  KEGG annotation.

\item
goana.default() code is rewritten to more closely match
  kegga.default. Slight speed improvement. Prior probabilities are now
  computed using the restricted universe with GO annotation instead of
  the whole universe.

\item
Bug fix for kegga() when trend=TRUE or a prior.prob or covariate is
  specified. Previously the Down p-values were substantially too small
  and the Up p-values were too large.

\item
plotWithHighlights() checks whether 'status' is a factor and
  converts to character.

\item
Bug fixes for beadCountWeights(). Default is now set correct for
  'design' and the function now works correctly when 'y' is an EList
  object contain bead standard errors but not standard deviations.

}}


\section{Version 3.32.0}{\itemize{

\item
New function cameraPR(), which implemented a pre-ranked version
of camera().

\item
New function alias2SymbolUsingNCBI(), which converts gene aliases
  or synonyms into official gene symbols using an NCBI gene-info
  file.

\item
New function wsva() for weighted surrogate variable analysis.

\item
New function coolmap(). This is essentially a wrapper for the
  heatmap.2() function in the ggplots package, but with sensible
  default settings for genomic log-expression data.

\item
decideTests() is now an S3 generic function with a default method
  and a method for MArrayLM objects.  decideTests() now selects all
  null hypotheses as rejected if p.value=1.

\item
length() methods removed all limma data objects (objects of class
  EList, EListRaw, RGList, MAList or MArrayLM). length(x) will now
  return the number of list components in the object rather than the
  number of elements in the expression matrix.

\item
New argument 'style' for volcanoplot(). The default is now to use
  -log10(p-value) for the y-axis instead of the B-statistic.

\item
New argument 'xlab' for barcodeplot().

\item
New argument 'col' for plotSA().  plotSA() now longer plots a
  lowess curve trend, but if appropriate both high and low outlier
  variances are highlighted in a different color.

\item
Argument 'replace.weights' removed from voomWithQualityWeights().
  The function now always produces an EList, similar to voom(). The
  default behavior of the function is unchanged.

\item
barcodeplot() now ranks statistics from low to high, instead of
  from high to low, following the usual style of axes in R plots.
  This means that left and right are now interchanged.

\item
plotSA() now plots quarter-root variances instead of
  log2(variances).

\item
Default for 'legend' argument of plotWithHighlights() changed from
  "topleft" to "topright".

\item
fitFDist() now estimates the scale by mean(x) when df2 is estimated
  to be Inf.  This will make the results from eBayes() less
  conservative than before when df.prior=Inf.

\item
plotSA() now indicates, by way of an open plotting symbol, any
  points that have low robust df.prior values.

\item
Clearer error message from fitFDistRobustly() when some variances
  are zero.

\item
C functions are now registered using R_registerRoutines.

\item
Bug fix for contrastAsCoef() when there is more than one contrast.
  Previously the coefficients for the transformed design matrix were
  correct only for the first contrast.

\item
Bug fix for kegga() when the universe is explicitly specified.

\item
Bug fix for fitFDistRobustly() when there is an extreme outlier.
  Previously floating point underflow for the outlier p-value could
  cause an error.

\item
Bug fix to mroast(), which was ignoring 'geneid' argument.

\item
Bug fix to printHead() for arrays with 1 column.
}}


\section{Version 3.30.0}{\itemize{

\item
  New function cumOverlap() to analyse the overlap between two
  ordered lists.

\item
  New function detectionPValues() to compute detection p-values from
  negative control probes.

\item
  New function fitmixture() to estimate genewise fold changes and
  expression values from mixture experiments.  Previously this
  funtion was only available as part of the illumina package available
  from http://bioinf.wehi.edu.au/illumina

\item
  New function logcosh() to compute log(cosh(x)) accurately without
  floating underflow or overflow.

\item
  The default settings for the 'inter.gene.cor' and 'use.neg.cor'
  arguments of camera() have been changed.  camera() now uses by
  default a preset value for the inter-gene correlation. This has the
  effect that it tends to rank highly co-regulated,
  biologically-interpretable more highly than before.

\item
  New flexibility for the roast() and mroast() functions, similar to
  that previously implemented for fry().  The index vector for each
  gene set can now be a data.frame, allowing each gene set to have
  its own set of gene weights.  The indices can now optionally be a
  vector of gene names instead of a vector of indices.  roast() and
  mroast() now support the robust empirical Bayes option of
  squeezeVar().  roast() and mroast() can now accept, via '...', any
  argument that would be normally passed to lmFit() or eBayes().

\item
  Slight change to the standardize="posterior.sd" method for fry().

\item
  goana(), alias2Symbol() and alias2SymbolTable() now work for any species for which an Entrez Gene based
  organism package exists.

\item
  The 'species' argument of kegga() can now accept any Bioconductor
  species abbreviation.

\item
  topGO() now breaks ties by the number of genes in the GO Term and
  by the name of the Term if the p-values are equal. (This is the
  same behavior as topKEGG.)

\item
  New argument 'plot' for plotMDS(), to optionally allow an MDS object
  to be returned without making a plot.

\item
  New arguments 'annotation' and 'verbose' for read.idat().
  The first of these to allows users to read
  any required columns from the manifest file.

\item
  New arguments 'pch.y' and 'pch.z' for plotRLDF.

\item
  Unnecessary argument 'design' removed from fitted.MArrayLM.

\item
  normalizeBetweenArrays() now checks whether the input 'object' is a
  data.frame, and converts to a matrix if possible.

\item
  duplicateCorrelation() now expands weights using expandAsWeights(),
  making it consistent with lmFit().

\item
  The lowess line drawn by plotSA() is now more robust with respect
  to NA variances.

\item
  More informative error message from voom() when there is only one
  row of data.

\item
  More informative error message from getEAWP() and lmFit() when
  'object' is a non-normalized data object.

\item
  Update Phipson et al (2016) reference for robust empirical Bayes.

\item
  Bug fix to fitFDistRobustly(), which in some circumstances was
  trying to save extra (undocumented) results that had not been
  computed.

\item
  Bug fixes for fry() with robust=TRUE or when 'index' has NULL names.

\item
  Bug fix to propTrueNull() when method="hist" and all the p-values
  are less than 1/nbins.

\item
  Bug fix to alias2Symbol() with expand.symbol=TRUE.

}}


\section{Version 3.28.0}{\itemize{

\item
  Improved capabilities and performance for fry().  fry() has two
  new arguments, 'geneid' and 'standardize'.  The index argument of
  fry() can now be a list of data.frames containing identifiers and
  weights for each set.  The options introduced by the standardize
  argument allows fry() to be more statistically powerful when genes have unequal
  variances.  fry() now accepts any arguments that would be suitable
  for lmFit() or eBayes().
  The 'sort' argument of fry() is now the same as for mroast().

\item
  roast(), mroast(), fry() and camera() now require the 'design'
  matrix to be set.  Previously the design matrix defaulted to a
  single intercept column.

\item
  Two changes to barcodeplot():  new argument 'alpha' to set
  semitransparency of positional bars in some circumstances;
  the default value for 'quantiles' is increased slightly.

\item
  kegga() has several new arguments and now supports any species
  supported by KEGG.  kegga() can now accept annotation as a
  data.frame, meaning that it can work from user-supplied annotation.

\item
  New functions getGeneKEGGLinks() and getKEGGPathwayNames() to get
  pathway annotation from the rest.kegg website.

\item
  topKEGG() now breaks tied p-values by number of genes in pathway and
  by name of pathway.

\item
  goana() now supports species="Pt" (chimpanzee).

\item
  plotRLDF() now produces more complete output and the output is
  more completely documented.  It can now be used as a complete LDF
  analysis for classification based on training data.  The argument
  'main' has been removed as an explicit argument because it and
  other plotting arguments are better passed using the ... facility.
  New arguments 'ndim' and 'plot' have been added.  The first allows
  all possible discriminant functions to be completed even if only
  two are plotted.  The second allows dicriminant functions to be
  computed without a plot.
  
  plotRLDF() also now uses squeezeVar() to estimate how by how much the
  within-group covariance matrix is moderated.  It has new arguments
  'trend' and 'robust' as options to the empirical Bayes estimation
  step.  It also has new argument 'var.prior' to allow the prior
  variances to be optionally supplied.  Argument 'df.prior' can now be
  a vector of genewise values.

\item
  New argument 'save.plot' for voom().

\item
  diffSplice() now returns genewise residual variances.

\item
  removeExt() has new 'sep' argument.

\item
  Slightly improved algorithm for producing the df2.shrunk values
  returned by fitFDistRobustly().  fitFDistRobustly() now returns
  tail.p.value, prob.outlier and df2.outlier as well as previously
  returned values.  The minimum df.prior value returned by
  eBayes(fit, robust=TRUE) may be slightly smaller than previously.

\item
  tmixture.vector() now handles unequal df in a better way.  This
  will be seen in slightly changed B-statistics from topTable when
  the residual or prior df are not all equal.

\item
  If a targets data.frame is provided to read.maimages() through the
  'files' argument, then read.maimages() now overwrites its rownames
  on output to agree with the column names of the RGList object.

\item
  More explicit handling of namespaces.
  Functions needed from the grDevices, graphics, stats and utils
  packages are now imported explicitly into the NAMESPACE, avoiding any possible masking by other packages.
  goana(), alias2Symbol(), alias2SymbolTable() now load the name spaces of GO.db
  the relevant organism package org.XX.eg.db instead of loading the
  packages.
  This keeps the user search path cleaner.

\item
  Various minor bug fixes and improvements to error messages.

}}


\section{Version 3.26.0}{\itemize{

\item
  New functions kegga() and topKEGG() to conduct KEGG pathway over-representation analyses.
  kegga() is an S3 generic function with a method for MArrayLM fitted model objects as well a default method.
  It uses the KEGGREST package to access up-to-date KEGG pathways using an internet connection.

\item
  goana() with trend now chooses the span of the tricube smoother
  based on the number of differentially expressed genes.  Larger
  spans give smoother trends and are used for smaller numbers of
  genes.  Arguments 'species' and 'plot' are no longer part of
  goana.MArrayLM() and are instead passed if present in a call to goana.default().

\item
  topGO() has a new argument 'truncate.term' to limit number of
  characters appearing in a column of the output data.frame.

\item
  Improved power for the romer() function.  The empirical Bayes
  shrinkage of the residuals, removed on 4 April, has been
  re-instated in a corrected form.  It can optionally be turned off
  using the 'shrink.resid' argument.

\item
  camera() has a new argument inter.gene.cor.  This allows a preset
  inter gene correlation to be set for each test test, resulting in
  potentially less conservative tests.
  camera() no longer gives NA results when a gene has a residual
  variance exactly zero.

\item
  mroast() now sorts significant gene sets by proportion of genes
  changing as well as by p-value and set size.  When p-values are 
  equal, sets with a higher proportion of changing genes will now
  rank higher in ordered results.
  The contrast argument of roast() can now optionally be a character
  string giving the name of a column of the design matrix.

\item
  fry() now produces "mixed" (non-directional) p-values and FDRs as
  well as directional p-values and FDRs.
  Bug fix for fry() when the gene set has a small number of genes.

\item
  camera(), fry(), mroast() and romer() now give more informative
  error messages when the index list is empty.

\item
  Speedup for ids2indices().

\item
  tricubeMovingAverage() has new argument 'power' and old argument
  'full.length' is removed.
  tricubeMovingAverage() now checks whether span is correctly
  specified.  It now gives identical results for any span>=1 or any
  span<=0.

\item
  lmFit() and friends now always produce an output component 'rank'
  that returns the column rank of the design matrix.  Previously it
  depended on the options used whether this component was given.

\item
  lmFit() now assumes that if the 'object' of expression values is a simple vector, that it represents
  a single gene rather than a single sample.

\item
  lmFit() now always treats infinite expression values as NA.  In the past,
  this was done somewhat inconsistently.

\item
  Checking for NA coefficients in lmFit() is now done more
  efficiently, meaning that lmFit() is now faster on large datasets.

\item
  More informative error message from lmscFit() when correlation is
  NA.

\item
  getEAWP() now works on all eSet objects, not just ExpressionSet,
  provided there is an data element called "exprs".
  This allows all the linear modelling functions to handle general eSet objects.

\item
  plotMD() can now accept arguments 'array' or 'coef' as appropriate
  as synonyms for 'column'.  This may help users transitioning from
  plotMA ot plotMD.

\item
  Arguments hi.pch, hi.col and hi.cex of plotWithHighlights() renamed
  to hl.pch, hl.col and hl.cex.  ('hl' is short for 'highlight'.)

\item
  New 'tolerance' argument for read.idat() to allow manifest and idat
  to differ by a certain number of probes

\item
  The 'genes' argument of controlStatus() now accepts EListRaw or
  Elist objects.

\item
  All data classes (EListRaw, EList, RGList, MAList, MArrayLM) now 
  require two arguments for subsetting.  This restores the behavior
  in limma version 3.19.10 and earlier.  In limma 3.19.11 through
  3.25.13, subsetting by one argument y[i] was equivalent to y[i,].
  This will now give an error message.

\item
  The dimnames<- method for EListRaw objects now sets rownames for
  the background matrix Eb as well as for the foreground matrix E.

\item
  avearrays() now gives a more informative error message if input
  argument 'x' is not a matrix.

\item
  auROC() now allows for tied stat values.

\item
  Bug fix to write.fit() when method="global" and fit contains more
  than one column.
}}


\section{Version 3.24.0}{\itemize{
\item
Limma review article (Ritchie et al, Nucleic Acids Research, 2015) is now published, and becomes the primary citation for limma.
References in User's Guide, help files and CITATION have been updated to refer to this article.

\item
New function plotMD() for mean-difference plots.  This will take
  over the functionality of the plotMA() function.

\item
mdplot() has new arguments 'columns' and 'main'.

\item
Arguments 'pch', 'cex' and 'col' of plotWithHighlights() renamed to
  'hi.pch', 'hi.cex' and 'hi.col'.  Improved help page for
  plotWithHighlights().

\item
plot() methods removed for EList, MAList and MArrayLM objects.  Use plotMD() instead.

\item
New function fry() provides a fast version of mroast()
  when there is little heteroscedasticity between genes.

\item 
Minor change to mroast() output so that FDR values are never
  smaller than p-values, even when mid-p-values are used.
 
\item
camera(), roast(), mroast() and romer() now give a warning if
  any arguments found in ... are not used.

\item
romer() is now an S3 generic function.
Speed improvements for romer() when the "mean" or "floormean" set
  statistics are used.
The empirical Bayes shrinkage of the residuals, introduced 5 October 2009, has been removed --
this means that romer() results will revert to being somewhat more conservative.

\item
topGO() can now sort gene ontology results by several p-value
  columns at once, using the minimum of the columns.  The default is
  now to sort results using all the p-value columns found in the
  results instead of just by the first column.

\item
goana() has new arguments 'plot' and 'covariate'.
The plot argument allows users to view any estimated trend in the genewise significance values.
The help files goana.Rd and goana.MArrayLM.Rd are consolidated into one file.

\item
plotDensities() has a new argument 'legend', allowing user to
  reposition the legend.

\item
voomWithQualityWeights() has a new argument 'col', allowing the barplot of 
  sample weights to be colour-coded.

\item
Improvements and bug fixes to plotMDS().
plotMDS now stores 'axislabel', depending on the method used to
  compute distances, and uses this to make more informative axis labels.
plotMDS now traps problems when the distances are all zero or if
  one or more of the dimensions are degenerate.
Also, a bug when gene.selection="common" and top=nrow(x) has been fixed.

\item
The arguments for predFCm() have been renamed.  Old argument
  'VarRel' replaced by logical argument 'var.indep.of.fc'.  Old
  argument 'prop' replaced by 'all.de'.  New argument
  'prop.true.null.method' passes through method to propTrueNull().

\item
vennDiagram() now fills circles with specified colors.

\item
plotMA() now has a method for EListRaw objects.

\item
voomWithQualityWeights() now stores the sample weights in the output EList object.
  The values stored in $weights are unchanged, they are still the combined 
  observation and sample weights.

\item
nec() and neqc() now remove the background Eb values if they exist in the
  EListRaw object.

\item
Acknowledgements edited in User's Guide.

\item
fix URLs for a number of references in help pages.

\item
Improvements to lowess C code.

\item
When loading suggested packages within function calls, usages of
  require() have been converted to requireNamespace() wherever
  possible.  Functions from suggested packages are called using `::'.

\item
topTable() now gives an information error message if eBayes() or
  treat() have not already been run on the fitted model object.

\item
genas() now gives an error message when applied to fitted model for
  which the coefficient correlations are gene-specific.

\item
Fix a bug in the MAList method for plotDensities that was introduced with
  the use of NextMethod() in version 3.21.16.
}}


\section{Version 3.22.0}{\itemize{
\item
New functions goana() and topGO() provide gene ontology analyses of differentially genes from a linear model fit.
The tests include the ability to adjust for gene length or abundance biases in differential expression detection, similar to the goseq package.

\item
Improvements to diffSplice.
diffSplice() now calculates Simes adjusted p-values for gene level inferences, in addition to the exon level t-tests and gene level F-tests.
topSplice() now has three ranking methods ("simes", "F" or "t"), with "simes" now becoming the default.
diffSplice() also has a new argument 'robust' giving access to robust empirical Bayes variance moderation.

\item
New function plotExons() to plot log-fold-changes by exon for a given gene.

\item
New function voomWithQualityWeights() allows users to estimate sample quality weights or allow for heteroscedasticity between treatment groups when doing an RNA-seq analysis.

\item
Improvement to arrayQualightyWeights().
It now has a new argument 'var.design' which allows users to model variability by treatment group or other covariates.

\item
Improved plotting for voomaByGroup().

\item
barcodeplot() can now plot different weights for different genes in the set.

\item
Improvements to roast() and mroast().
The directional (up and down) tests done by roast() now use both the original rotations and their opposite signs, effectively doubling the number of effective rotations for no additional computational cost.
The two-sided tests are now done explicitly by rotation instead of doubling the smallest one-sided p-value.
The two-sided p-value is now called "UpOrDown" in the roast() output.
Both functions now use a fast approximation to convert t-statistics into z-scores,
making the functions much faster when the number of rotations or the number of genes is large.
The contrast argument can now optionally be a character string giving a column name of the design matrix.

\item
zscoreT() can optionally use a fast approximation instead of the slower exact calculation.

\item
symbols2indices() renamed to ids2indices().

\item
Improvements to removeBatchEffect().
It can now take into account weights and other arguments that will affect the linear model fit.
It can now accept any arguments that would be acceptable for lmFit().
The behavior of removeBatchEffect() with design supplied has also changed so that it is now consistent with that of lmFit() when modelling batches as additive effects.
Previously batch adjustments were made only within the treatment levels defined by the design matrix.

\item
New function plotWithHighlights(), which is now used as the low-level function for plotMA() and plot() methods for limma data objects.

\item
The definition of the M and A axes for an MA-plot of single channel
  data is changed slightly.  Previously the A-axis was the average of
  all arrays in the dataset -- this has been definition since MA-plots
  were introduced for single channel data in April 2003.  Now an
  artificial array is formed by averaging all arrays other than the
  one to be plotted.  Then a mean-difference plot is formed from the
  specified array and the artificial array.  This change ensures the 
  specified and artificial arrays are computed from independent data,
  and ensures the MA-plot will reduce to a correct mean-difference
  plot when there are just two arrays in the dataset.

\item
plotMDS() can now optionally plot samples using symbols instead of
  text labels.
It no longer has a 'col' argument, which instead is handled by ....

\item
vennDiagram() now supports circles of different colors for any
  number of circles.  Previously this was supported only up to three
  sets.  

\item
getEAWP() will now find a weights matrix in an ExpressionSet object
  if it exists.

\item
update to helpMethods().

\item
Substantial updates to the two RNA-seq case studies in the User's Guide.
In both cases, the short read data has been realigned and resummarized.

\item
Improvements to many Rd files.
Many keyword entries have been revised.
Many usage and example lines been reformated to avoid over long lines.

\item
biocViews keywords updated.

\item
Subsetting columns of a MArrayLM object no longer subsets the design matrix.

\item
Bug fix for read.maimages: default value for 'quote' was not being
  set correctly for source="agilent.mean" or source="agilent.median".

\item
Bug fix to topTableF() and topTable().
The ordering of Amean values was sometimes incorrect when sorting by F-statistic and a lfc or p.value filter had been set.

\item
Bug fix to read.ilmn() when sep=",".
}}


\section{Version 3.20.0}{\itemize{

\item
New functions diffSplice(), topSplice() and plotSplice() provide functionality to analyse differential splicing using exon-level expression data from either microarrays or RNA-seq.

\item
New Pasilla case study added to User's Guide, demonstrating differential splicing analysis of RNA-seq data.

\item
new function weightedLowess() which fits a lowess curve with prior weights.
Unlike previous implementations of lowess or loess, the weights are used in calculating which neighbouring points to include in each local regression as well as in the local regression itself.

\item
weightedLoess() now becomes the default method used by loessFit() to fit the loess curve when there are weights.
The previous locfit and loess() methods are offered as options.

\item
linear model fit functions lm.series(), mrlm.series() and gls.series() no longer drop the dimensions of the components of the fitted object when there is just coefficient or just one gene.
Previously this was done inconsistently in some cases but not others.
Now the matrix components always keep dimensions.

\item
The functions lmFit(), eBayes() and tmixture.vector() now work even when there is just one gene (one row of data).

\item
New function subsetListOfArrays(), which is used to simplify the subsetting code for RGList, MAList, EList, EListRaw and MArrayLM objects.

\item
new function tricubeMovingAverage() for smoothing a time series.

\item
barcodeplot() has a new option to add enrichment worms to the plot, making use of tricubeMovingAverage().

\item
New plot() methods for RGList, MAList, EList and MArrayLM class objects.  In each case, this produces a similar result to plotMA().
When using plot() or plotMA() on an MArrayLM object, the column is now specified by the 'coef' argument instead of by 'array'.

\item
plotMA3by2() now works on single channel data objects as well as on MAList objects.

\item
New function read.idat() to read files from Illumina expression beadarrays in IDAT format.

\item
The ctrlpath argument of read.ilmn() now defaults to the same as path for regular probes.
This means that only one path setting is required if the regular and control probe profiles are in the same directory.

\item
read.ilmn() now sets the same probe IDs as rownames for both the expression matrix E and the annotation data.frame genes, providing that the probe IDs are unique.

\item
beadCountWeights() can now work with either probe-wise standard errors or probe-wise standard deviations.

\item
treat() has new arguments robust and winsor.tail.p which are passed through to robust empirical Bayes estimation.

\item
topTreat() now includes ... argument which is passed to topTable().

\item
topTable() with confint=TRUE now produces confidence intervals based on the t-distribution instead of on the normal distribution.
It also now accepts a numeric value for the confint argument to specify a confidence level other the default of 0.95.

\item
topTable() will now work on an MArrayLM fit object that is missing the lods component, for example as produced by treat().

\item
roast() and mroast() now permit array weights and observation weights to both be specified.

\item
camera(), roast() and mroast() now use getEAWP() to interpret the data object.  This means that they now work on any class of data object that lmFit() will.

\item
romer() now uses propTrueNull(method="lfdr") instead of convest().
This makes it substantially faster when the number of genes is large.

\item
genas() now uses fit$df.total from the MArrayLM object.
This prevents df.total from exceeding the total pooled residual df for the dataset.
The genas() results will change slightly for datasets for which df.prior was very lage.

\item
plotDensities() is now an S3 generic function with methods for RGList, MAList, EListRaw and EList objects.

\item
plotFB is now an S3 generic function with methods for RGList and EList data objects.

\item
New topic help pages 10GeneSetTests.Rd and 11RNAseq.Rd.
The page 10Other.Rd is deleted.
All topic help pages are now listed under 'See also' in the package introduction page accessible by ?limma.

\item
avereps() was never intended to be applied to RGList or EListRaw objects.
It now gives an error when applied to these objects instead of returning a matrix of questionable value.

\item
Bug fix: fitFDistRobustly() was failing when there were missing values or zero df values and covariate was NULL.

\item
Bug fix: vennDiagram() wasn't passing extra arguments (...) to plot() when the number of sets was greater than 3.

\item
Bug fix to topTreat().  Rownames were incorrectly ordered when p<1.

\item
bug fix to genas(), which was not handling vector df.prior correctly when the fit object was generated using robust=TRUE.

\item
bug fix to squeezeVar().
Previously there was an error when robust=TRUE and trend=FALSE and some of the estimated df.prior were infinite.

\item
bug fix to topTable() and topTableF() when sorting by F-statistic combined with p-value or lfc cutoffs.
}}


\section{Version 3.18.0}{\itemize{

\item
new function beadCountWeights() to estimate quantitative weights
from the bead counts for each probe for Illumina BeadArrays.

\item
New function contrastAsCoef(), which reforms a design matrix so
that one or more specified contrasts become coefficients.  This
function is called by roast().

\item
plotMA() is now an S3 generic function.

\item
The legend argument to plotMA() can now take a character value
giving the position to place the legend.  

\item
toptable(), topTable() and topTreat() now preserve the rownames of
the fit object, unless the fit object has duplicated names, in
which case the rownames are copied to the ID column.  Empty
rownames are replaced with 1:nrow(fit).

\item 
read.ilmn() no longer adds the probe IDs to the gene annotation
data.frame, leaving them instead as rownames of the expression
matrix.  It longer creates a targets file since the sample names
are already preserved as column names of the expression matrix.

\item
loessFit() now uses the locfit.raw in the locfit package when
weights are provided instead of loess in the stats package.  The
function now runs very efficiently even on very long data vectors.
The output results will change slightly when weights are provided.

\item
voom() now outputs lib.size as a column of targets instead of as a
separate component.

\item
cbind for EList and EListRaw objects now recognizes a design matrix
if it is present.

\item
plotMDS() now checks explicitly that there are at least 3 samples
to plot.

\item
normexp.fit.detection.p() now tolerates some non-monotonicity in
the detection p-pvalues as a function of expression.

\item
fitFDistRobustly() now uses a smoother for the smallest df.prior
values.  This may result in smaller tail values than before when
a group of input x values appear to be outliers but the largest
value is not individually a stand-out value.

\item
New merge methods for EList and EListRaw objects.

\item
topTable() and treat() now give more informative error messages
when the argument fit is not a valid MArrayLM object.

\item
roast() now calls mroast() if the index vector is a list.
Bug fix to mroast(), which had been ignoring the weights.

\item
Updates to genas() function.
argument chooseMethod renamed to subset and option "n" renamed to "all". 
Function now returns NA results and a message when no genes satisfy
the criterion for inclusion in the analysis.
Some editing of help page and streamlining of code.

\item
Roles of contributors now specified in author field of DESCRIPTION
file using standard codes.

\item
Additions and updates to references in the help pages.
Removed defunct Berkeley Press links to published Smyth (2004)
article in several Rd files.  Replacing with link to Preprint.
Added link to Phipson (2013) thesis in two Rd files.
Add Majewski et al reference to genas.Rd.
Add Phipson et al and Sartor et al references to squeezeVar.Rd.
Add Phipson et al reference to eBayes.Rd.
Update lmscFit and voom references.

\item
Update mammmary stem cell case study in User's Guide.  As well
as reflecting changes to read.ilmn() and topTable(), this now
demonstrates how to find signature genes for particular cell type.

\item
documentation about rownames and column names and the use of
rownames(fit) and colnames(fit) added to lmFit.Rd.

\item
improvements to help pages for data classes.

\item
Edits to normalizeBetweenArrays help page (i) to further clarify
which normalization methods are available for single-channel data
and which are available for two-color data and (ii) to give a cross
citation to the neqc() function for Illumina BeadChips.

\item
Edits to voomaByGroup help page.

\item
duplicateCorrelation() now uses the weights matrix when block is
set.  Previously the weights were ignored when block was used.

\item
Bug fix to subsetting for MArrayLM objects: the df.total component
was not being subsetted.

\item
bug fix to eBayes(robust=TRUE) when some of the df.prior values are
infinite.

\item
Bug fix to ebayes(), which was not passing the 'robust' argument
correctly on to squeezeVar().

\item
Bug fix to fitFDistRobustly(), which affected the estimated scale
when df2 is estimated to be Inf.
}}


\section{Version 3.16.0}{\itemize{

\item
New section in User's Guide on time course experiments with many time points.
The RNA-seq case study in User's Guide has also been revised.

\item
Improvements to various help pages including read.maimages.Rd, squeezeVar.Rd, fitFDist.Rd, trigammaInverse.Rd, normalizeRobustSpline.Rd, genas.Rd and roast.Rd.
Previously the meaning of source="agilent" was mis-stated in read.maimages.Rd.

\item
New robust method for estimating the empirical Bayes prior, called
by specifying robust=TRUE in the call to eBayes().  When this is
TRUE the output df.prior is now a vector instead of a scalar.

\item  
New function fitFDistRobustly() estimates the parameters of a
scaled F-distribution robustly using Winsorized values.  Outlier
observations receive smaller values for df.prior than non-outliers.
This permits robust methods for squeezeVar(), ebayes() and eBayes(),
all of which now have a new argument wins.tail.p to specify the
tail proportions for Winsorizing.

\item
fitFDist() now permits infinite values for the covariate.
It also gracefully handles cases where the covariate takes only a small number of distinct values.
Similarly for eBayes() and squeezeVar() that call fitFDist().  

\item
All the functions that perform gene set tests have been revised to make the input and output formats more consistent.

roast(), mroast() and camera() are now S3 generic functions, with methods for EList and MAList objects.

The order of arguments has been changed for roast(), mroast() and camera() so that the first argument is now y.

All functions that perform gene sets now use the argument 'index' to specify which genes are included in the test set.
Previously this argument was called 'iset' for roast() and romer() and 'indices' for camera().

camera() and mroast() now produce a data.frames.
Instead of separate up and down p-value columns, there is now a two-sided p-value and a column indicating direction of change.  
There are new columns giving FDR values and the number of genes in each set.
There is a new argument 'sort' to indicate whether output results should be sorted by p-value.

mroast() has a new argument 'weights' for observational weights, to bring it into line with roast(),

\item
vennDiagram() can now plot up to five sets (previously limited to three).

\item
genas() now optionally draws a plot in which ellipses are used to represent the technical and biological components of correlation.
It also now has the ability to automatically select which probes are used for the correlation analysis, and a new argument controls the method used for this selection.

\item
New options for the method argument of propTrueNull().

\item
New functions vooma() and voomaByGroup() for computing precision weights based on a mean-variance trend.
vooma() is similar to voom() but for microarray data instead of RNA-Seq.
voomaByGroup() allows different groups to have systematically different variances.

\item
New function predFCm() to compute predictive (shrunk) log fold changes.

\item
New function fitGammaIntercept() for estimating the intercept of a gamma glm with an offset.
Used by genas().

\item
New function zscoreHyper() for computing z-score equivalents of deviates from a hypergeometric distribution.

\item
New function qqf() for qq-plots relative to an F-distribution.

\item
normalizeWithinArrays() with method="robustspline" now longer requires the layout argument to be set.
The layout argument for normalizeRobustSpline() now defaults to a single print-tip group.

\item
fitFDist() now coerces degrees of freedom df1 below 1e-15 to zero.

\item
Due to changes in R, loessFit() no longer makes direct calls to foreign language code in the stats package, and instead calls R functions.
Unfortunately, this makes loessFit() about 25-30\% slower than previously when weights are used.

\item
Bug fix to read.maimages(), which was not accepting source="agilent.mean".

\item
Bug fix for contrasts.fit() when the covariance matrix of the coefficients (cov.coefficients) is not found in the fitted model object.
This situation doesn't arise using any of the standard limma analysis pipelines.

\item
Bug fix to lmscFit() when the residual df = 1.

\item
Bug fix to readTargets() to avoid warning message when targets$Label is used to set row names but targets$Label contains duplicated entries.

}}


\section{Version 3.14.0}{\itemize{

\item
limma license upgraded to GPL (>=2) instead of LGPL to match R itself.

\item
Many updates to the User's Guide. 
Sections have been added on reading single channel Agilent and Illumina data.
The chapter on experimental designs has been split into three chapters on single-channel, common reference and
two-color designs respectively.  The material on the fixed effect approach
  to technical replication has been deleted.
There are new sections on nested interactions for factorial designs
and on multi-level designs.

\item
The links to the Apoa1, Weaver and Bob1 datasets in the User's Guide have been updated
to help users download the data themselves if they wish to repeat the case study analyses.

\item
The help page for camera() now cites published paper Wu and Smyth (NAR, 2012).
In view of the results of this paper, the claim is no longer made on help page for geneSetTest()
  that genes might be treated as independent when the experimental
  units are genetically identical mice.

\item
Minor edits to CITATION file.

\item
New function propTrueNull() for fast estimation of the proportion of true null hypotheses from a vector of p-values.

\item
New function zscore() to compute z-score equivalents for deviates from any continuous distribution.
Includes the functionality of the older functions zscoreGamma() and zscoreT() as special cases.

\item
roast() now accepts observation level weights, through a new
  argument 'weights'.

\item
loessFit() now applies minimum and maximum bounds by default to avoid zero or
  infinite weights.  Equal weights are now treated as if the weights were NULL, even all
  zero weights, so that the lowess code is called instead of the loess code.

\item
When there are no weights, loessFit() now extracts residuals
directly from the C code output instead of computing in R.

\item
  fitFDist() now permits missing values for x or zero values for df1
  even when there is a covariate.  This means that squeezeVar() and
  eBayes() now work with trends even when not all the data values
  are informative.

\item
New argument 'file' for convest(), implementing edits contributed
  by Marcus Davy.  Arguments doplot and dereport renamed to 'plot'
  and 'report'.

\item
Two improvements for plotMDS().
It now coerces labels to be character, and now makes extra room on the plot when the text labels are wide.

\item
plotMDS() no longer gives an error when the requested number of top
  genes is greater than the total number of rows of data.

\item
Code speed-up for alias2SymbolTable()

\item
any(duplicated()) replaced by anyDuplicated() in several functions.

\item
Fix to voom() so that it computes weights correctly even when the
  design matrix is not of full rank.

\item
Bug fix for roast() when the fitted model has only one coefficient.

}}


\section{Version 3.12.0}{\itemize{

\item
read.maimages() with source="agilent" now reads median foreground
estimates instead of mean foreground.  New option source=
"agilent.mean" preserves earlier meaning of source="agilent".

\item
Agilent single-channel case study added to User's Guide.

\item
removeBatchEffect() now corrects for continuous covariates
as well as qualitative factors.

\item
new function camera() performs competitive
gene set tests while adjusting for inter-gene correlation.

\item
new function interGeneCorrelation() estimates the average
intergene correlation for a set of genes.

\item
columns in output from roast() have been re-ordered.

\item
arguments 'selected' and 'selected2' renamed to 'index'
and 'index2' in functions barcodeplot(), geneSetTest()
and wilcoxGST().

\item
default labels for barcodeplot() are now somewhat more
explicit.

\item
new function rankSumTestWithCorrelation extends the
Wilcoxon-Mann-Whitney test to allow for correlation between
cases in one of the groups.  geneSetTest() now calls this
function instead of wilcox.test, with a consequence improvement
in speed.

\item
The lfc (log-fold-change) cutoff argument of topTable() is now
applied to the minimum absolute logFC when ranking by F-statistic.
Previously lfc was only used when ranking by t-statistic.

\item
new methods "fast" and "affy" for normalizeCyclicLoess(),
with "fast" becoming the default method.
New argument 'cyclic.method' for normalizeBetweenArrays() gives
access to the different cyclic loess methods.

\item
There were problems with using the argument gene.weights in
mroast().  This argument is now permitted to be of the same
length as the number of probes in the data set.  It is then
automatically subsetted for each gene set.

\item
mroast() now uses mid-p-values by default when adjusting for multiple testing.

\item
neqc(), nec() and normexp.fit.control() now give user-friendly error messages when no negative control probes or no regular probes are found.

}}

\section{Version 3.10.0}{\itemize{

\item
New function voom() allows RNA-Seq experiments to be analysed using the standard limma pipeline.
An RNA-Seq case study is added to User's Guide.

\item
treat(), roast() and mroast() can now estimate and work with a trend on the prior variance, bringing them into line with eBayes().

\item
barcodeplot() and barcodeplot2() merged into one function.

\item
removeBatchEffect() can now correct for two batch factors.

\item
plotMDS is now an S3 generic function.  This allows MDS plots to be redrawn with new labels without needing to repeat the distance or scaling calculations.
New S4 class "MDS" to hold the multidimensional scaling information output from plotMDS.

\item
getEAWP() now gets probe annotation from the expression rownames of an EList object, if no other probe annotation is available.

\item
topRomer() now ranks gene sets by secondary columns as well the primary criterion specified, to give a more meaningful ranking when the p-values are tied.

\item
wilcoxGST() now accepts signed or unsigned test statistics.
Change to p-value calculation in geneSetTest() when rank.only=FALSE to avoid zero p-values and follow recommendation of Phipson and Smyth (SAGMB, 2010).

\item
plotMA() now recognizes ElistRaw and EList objects appropriately.

\item
Default span for normalizeCyclicLoess increased from 0.4 to 0.7.
Speed improved when weights=NULL.

\item
Weaver case study (Section 11.5) in User's Guide is updated and rewritten.
Data classes ElistRaw and Elist now described in the quick start section of the User's Guide.  Other minor updates to User's Guide.

\item
Bug fix for normalizeBetweenArrays() when object is an EListRaw and method="cyclicloess".  Previously this function was applying cyclicloess to the raw intensities, then logging.  Now it logs first, then applies cyclicloess.

\item
Bug fix to avereps() for EList objects x when x$other is not empty.

}}

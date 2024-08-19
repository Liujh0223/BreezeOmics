\name{edgeRnews}
\title{edgeR News}
\encoding{UTF-8}

\section{Version 3.40.0}{\itemize{
\item
New argument 'hairpinBeforeBarcode' for processAmplicons().
The revised function can process reads where the hairpins/sgRNAs/sample index sequences are in variable positions within each read.
When 'plotPositions=TRUE' a density plot of the  match positions is created to allow the user to assess whether they occur in the expected positions.

\item
Update C++ BLAS calls to account for USE_FC_LEN_T setting in R 4.3.0.

\item
Bug fix to R_compute_apl.cpp to make sure GLM working weights are zero when fitted mu=0.
}}


\section{Version 3.38.0}{\itemize{
\item
New argument 'keep.EList' for voomLmFit() to store the normalized log2-CPM values and voom weights.
}}


\section{Version 3.36.0}{\itemize{
\item
diffSpliceDGE() now returns p-value=1 instead of NA when an exon has all zero counts.

\item
Improve error message from readDGE() when there are repeated gene/tag names.
}}


\section{Version 3.34.0}{\itemize{
\item
New function featureCounts2DGEList() that converts results from Rsubread::featureCounts() to DGELists.

\item
plotMDS.DGEList (the DGEList method of plotMDS) now displays the percentage variance explained by each dimension, and a new argument 'var.explained' is provided to make that optional.
It no longer calls stats::cmdscale() internally and the 'ndim' argument is removed.
The "bcv" method is scheduled to be deprecated in a future release of edgeR.

\item
read10X() now counts the number of comment lines in mtx files and skips those lines when reading in the data.

\item
Fix a bug in voomLmFit() whereby zeros were sometimes incorrectly identified due to floating point errors.
}}


\section{Version 3.32.0}{\itemize{
\item
cpm.default() and rpkm.default() now accept offset.

\item
scaleOffset() now accepts CompressedMatrix offset and accounts for norm.factors.

\item
Revise the lowess trend fitting in voomLmFit() to downweight genes with exact zeros and hence fewer df to estimate the variance.

\item
Add as.data.frame method for DGEList class.

\item
Change default choice for refColumn in calcNormFactors() with method="TMMwsp". The new method chooses the column with the largest sum of sqrt-counts.

\item
processAmplicons() can now accommodate data from newer screens that use a staggered primer design.

\item
Fixed a bug that diffSpliceDGE() accept more than one coef. It now gives a warning if more than one coef or contrast is supplied. It only uses the first.
}}


\section{Version 3.30.2}{\itemize{
\item
New function voomLmFit() that combines the limma voom-lmFit pipeline with loss of residual df due to zero counts as for glmQLFit().
The new function is more robust to zero counts than running voom() and lmFit() separately.
The new function allows sample quality weights and intra-block correlations to be estimated it incorporates the functionality of duplicateCorrelation() and voomWithQualityWeights() as well.

\item
New function SE2DGEList() to convert a SummarizedExperiment object into a DGEList object.

\item
S3 methods for SummarizedExperiment objects are added to the following functions: 
aveLogCPM(), calcNormFactors(), cpm(), cpmByGroup(), estimateDisp(), filterByExpr(), glmFit(), glmQLFit(), plotMD(), plotMDS(), predFC(), rowsum(), rpkm(), rpkmByGroup() and sumTechReps().

\item
New cpm and rpkm methods for DGEGLM and DGELRT objects.

\item
New function effectiveLibSizes() to extract normalized library sizes from an edgeR data object or fitted model object.

\item
Add as.data.frame methods for DGEExact and DGELRT objects and remove the 'optional' argument from as.data.frame.TopTags().

\item
readBismark2DGE() now forces 'files' to be character vector.

\item
Add warning messages when filterByExpr() is used without specifying group or design.

\item
Add warning message when calcNormFactors() is applied to DGEList object containing an offset matrix.

\item
Rewrite User's Guide Section 3.5 on Multilevel Experiments so that the code is valid regardless of the number of subjects in each disease group.
}}


\section{Version 3.28.0}{\itemize{
\item
Add head() and tail() methods for edgeR classes.

\item
Remove the 'mixed.df' argument and add a 'locfit.mixed' option to 'trend.method' in estimateDisp() and WLEB().

\item
Add two new arguments 'large.n' and 'min.prop' to filterByExpr() to allow users to change parameters previously hard-wired.

\item
Remove 'values' and 'col' arguments to plotMD.DGELRT() and plotMD.ExactTest() as no longer needed because of changes to plotWithHighlights().

\item
roast.DGEList() and mroast.DGEList() now pass the 'nrot' argument to roast.default().

\item
Rename dglmStdResid() to plotMeanVar2().

\item
getDispersions() is no longer exported.

\item
Estimated dispersions are now numeric even if NA.

\item
Bug fix to goana.DGELRT() and kegga.DGELRT() when the LRT was on more than 1 df.
}}


\section{Version 3.26.0}{\itemize{
\item
read10X() now automatically detects files names from latest CellRanger version.

\item
glmTreat() now checks whether 'contrast' is a matrix with multiple columns and uses first column.

\item
The TMMwzp method has been renamed to TMMwsp, but calls to method="TMMwzp" will still be respected.
calcNormFactors(object) now returns a named vector when 'object' is a matrix, with colnames(objects) as the names.

\item
New 'random' method for zscoreNBinom().

\item
Add arguments 'log' and 'prior.count' to cpmByGroup() and rpkmByGroup().

\item
Bug fix to filterByExpr().
}}


\section{Version 3.24.0}{\itemize{
\item
New functions catchKallisto() and catchSalmon() to read outputs from kallisto and Salmon
and to compute overdispersion factors for each transcript from bootstrap samples.

\item
New function readBismark2DGE() to read coverage files created by Bismark for BS-seq methylation data.

\item
New method 'TMMwzp' for calcNormFactors() to better handle samples with large proportions of zero counts.

\item
The default value for prior.count increased from 0.25 to 2 in cpm() and rpkm().
The new value is more generally useful and agrees with the default values in aveLogCPM() and with the DGEList method for plotMDS().

\item
zscoreNBinom() now supports non-integer q values.

\item
The scaleOffset() S3 methods for DGEList and default objects are now registered in the NAMESPACE.
Previously the functions were exported but not registered as S3 methods.

\item
The rowsum() method for DGEList objects (rowsum.DGEList) now automatically removes gene annotation columns that are not group-level.

\item
More specific error messages from DGEList() when invalid (NA, negative or infinite) count values are detected.

\item
Bug fix to glmfit.default() when lib.size is specified.

\item
Bug fix to column name returned by decideTestsDGE().
}}


\section{Version 3.22.0}{\itemize{
\item
New function read10X() to read 10X Genomics files.

\item
New function nearestTSS() to find the nearest transcriptional start site (TSS) for given genomic loci.

\item
New function nearestReftoX() to find the element of a reference table that is closest to each element of an incoming vector.

\item
New function modelMatrixMeth() to construct design matrices for analysis of methylation data.

\item
New function filterByExpr() to filter low expression genes or features.

\item
New rowsum method for DGEList objects.

\item
nbinomUnitDeviance() now respects vectors.

\item
DGEList() takes 'group' from 'samples' only if samples has a column called group.

\item
decideTestsDGE() now includes a 'label' attribute, which allows more information row.names for the summary results table from decideTestsDGE() or decideTests().

\item
Design now defaults to y$design for all the gene set tests.

\item
More intuitive error messages from glmFit() when the arguments are not conformal.

\item
Update User's Guide to cite the Chen et al (2017) methylation workflow.

\item
Change glmTreat() default to lfc=log2(1.2).

\item
Fix incorrect implementation of weights in adjustedProfileLik().

\item
Bug fix to glmLRT() when there is just one gene but multiple contrasts.

\item
Bug fix to cpmByGroup().
}}


\section{Version 3.20.0}{\itemize{
\item
DGEList() sets genes and counts to have same row.names.

\item
topTags() preserves row.names.

\item
estimateDisp() uses 'y$design' if it exists.

\item
estimateDisp() doesn't use average log-CPM in the prior.df calculation if 'trend.method' is 'none'.

\item
estimateDisp() doesn't return trended.dispersion if 'trend.method' is 'none'. 

\item
Design matrix defaults to 'y$design' before 'y$samples$group' in all the gene set testing functions.

\item
New arg 'group' for mglmOneWay(). Results in slight speed improvement for glmFit().

\item
'design' arg for predFC() is now compulsory.

\item
Switched 'coef.start' back to a vector in mglmOneGroup().

\item
New functions cpmByGroup() and rpkmByGroup().

\item
Renamed arg 'x' to 'y' in cpm() and rpkm().

\item
Restored null dispersion check in glmFit().

\item
Removed 'offset' arg from glmQLFit() to be consistent with glmFit().

\item
Exported CompressedMatrix subset operator.

\item
Refactored C++ code with greater C++11 support to use Rcpp.

\item
Streamlined input dimension checks in C++ code.

\item
Supported zero-row input to addPriorCounts() C++ code.

\item
Added cbind and rbind S3 methods for DGEList objects.

\item
Added 'Dims' as part of the compressedMatrix class.

\item
Added common methods for the compressedMatrix class.

\item
Register S3 methods for compressedMatrix.

\item
Added a case study of differential methylation analysis to the user's guide.
}}


\section{Version 3.18.0}{\itemize{
\item
roast.DGEList(), mroast.DGEList(), fry.DGEList() and camera.DGEList() now have explicit arguments instead of passing arguments with ... to the default method.

\item
New function scaleOffset() to ensure scale of offsets are consistent with library sizes.

\item
Added decideTests() S3 methods for DGEExact and DGELRT objects. It now works for F-tests with multiple contrasts.

\item
Report log-fold changes for redundant contrasts in F-tests with multiple contrasts.

\item
Modified plotMD() S3 method for DGELRT and DGEExact objects. It now automatically uses decideTests() and highlights the DE genes on the MD plot.

\item 
New argument 'plot' in plotMDS.DGEList().

\item
Removed S3 length methods for data objects.

\item
gini() now support NA values and avoids integer overflow.
}}


\section{Version 3.16.0}{\itemize{
\item 
estimateDisp() now respects weights in calculating the APLs.

\item 
Added design matrix to the output of estimateDisp().

\item 
glmFit() constructs design matrix, if design=NULL, from y$samples$group.

\item 
New argument 'null' in glmTreat(), and a change in how p-values are calculated by default.

\item 
Modified the default 'main' in plotMD().

\item 
Created a new S3 class, compressedMatrix, to store offsets and weights efficiently.

\item 
Added the makeCompressedMatrix() function to make a compressedMatrix object.

\item 
Switched storage of offsets in DGEGLM objects to use the compressedMatrix class.

\item 
Added the addPriorCount() function for adding prior counts.

\item 
Modified spliceVariants() calculation of the average log-CPM.

\item 
Migrated some internal calculations and checks to C++ for greater efficiency.
}}


\section{Version 3.14.0}{\itemize{
\item
estimateDisp(), estimateCommonDisp(), estimateTrendedDisp(), estimateTagwiseDisp(), splitIntoGroups() and equalizeLibSizes() are now S3 generic functions.

\item
The default method of estimateGLMTrendedDisp() and estimateGLMTagwiseDisp() now only return dispersion estimates instead of a list.

\item
The DGEList method of estimateDisp(), estimateCommonDisp() and estimateGLMCommonDisp() now use the common dispersion estimate to compute AveLogCPM and store it in the output.

\item
Add fry method for DGEList objects.

\item
Import R core packages explicitly.

\item
New function gini() to compute Gini coefficients.

\item
New argument poisson.bound for glmQLFTest().
If TRUE (default), the p-value returned by glmQLFTest() will never be less than what would be obtained for a likelihood ratio test with NB dispersion equal to zero.

\item
New argument samples for DGEList(). It takes a data frame containing information for each sample.

\item
glmFit() now protects against zero library sizes and infinite offset values.

\item
glmQLFit.default() now avoids passing a NULL design to .residDF().

\item
cpm.default() now outputs a matrix of the same dimensions as the input even when the input has 0 row or 0 column.

\item
DGEList() pops up a warning message when zero lib.size is detected.

\item
Bug fix to calcNormFactors(method="TMM") when two libraries have identical counts but the lib.sizes have been set unequal.

\item
Add a CRISPR-Cas9 screen case study to the users' guide and rename Nigerian case study to Yoruba.
}}


\section{Version 3.12.0}{\itemize{
\item
New argument tagwise for estimateDisp(), allowing users to optionally skip estimation of tagwise dispersions, estimating common and trended dispersions only.

\item
estimateTrendedDisp() has more stable performance and does not return negative trended dispersion estimates.

\item
New plotMD methods for DGEList, DGEGLM, DGEExact and DGELRT objects to make a mean-difference plot (aka MA plot).

\item
readDGE() now recognizes HTSeq style meta genes.

\item
Remove the F-test in glmLRT().

\item
New argument contrast for diffSpliceDGE(), allowing users to specify the testing contrast. 

\item
glmTreat() returns both logFC and unshrunk.logFC in the output table.

\item
New method implemented in glmTreat() to increase the power of the test.

\item
New kegga methods for DGEExact and DGELRT objects to perform KEGG pathway analysis of differentially expressed genes using Entrez Gene IDs.

\item
New dimnames<- methods for DGEExact and DGELRT objects.

\item
glmFit() and glmQLFit() will now accept a matrix of dispersion values, i.e., a potentially different dispersion for each observation.

\item
Bug fix to dimnames<- method for DGEGLM objects.

\item
User's Guide updated. Three old case studies are replaced by two new comprehensive case studies.
}}


\section{Version 3.10.0}{\itemize{
\item
An DGEList method for romer() has been added, allowing access to rotation gene set enrichment analysis.

\item
New function dropEmptyLevels() to remove unused levels from a factor.

\item
New argument p.value for topTags(), allowing users to apply a p-value or FDR cutoff for the results.

\item
New argument prior.count for aveLogCPM().

\item
New argument pch for the plotMDS method for DGEList objects.
Old argument col is now removed, but can be passed using ....
Various other improvements to the plotMDS method for DGEList objects, better labelling of the axes and protection against degenerate dimensions.

\item
treatDGE() is renamed as glmTreat().
It can now optionally work with either likelihood ratio tests or with quasi-likelihood F-tests.

\item
glmQLFit() is now an S3 generic function.

\item
glmQLFit() now breaks the output component s2.fit into three separate components: df.prior, var.post and var.prior.

\item
estimateDisp() now protects against fitted values of zeros, giving more accurate dispersion estimates.

\item
DGEList() now gives a message rather than an error when the count matrix has non-unique column names.

\item
Minor corrections to User's Guide.

\item
requireNamespace() is now used internally instead of require() to access functions in suggested packages.
}}


\section{Version 3.8.0}{\itemize{

\item
New goana() methods for DGEExact and DGELRT objects to perform Gene Ontology analysis of differentially expressed genes using Entrez Gene IDs.

\item
New functions diffSpliceDGE(), topSpliceDGE() and plotSpliceDGE() for detecting differential exon usage and displaying results.

\item
New function treatDGE() that tests for DE relative to a specified log2-FC threshold.

\item
glmQLFTest() is split into three functions: glmQLFit() for fitting quasi-likelihood GLMs, glmQLFTest() for performing quasi-likelihood F-tests and plotQLDisp() for plotting quasi-likelihood dispersions.

\item
processHairpinReads() renamed to processAmplicons() and allows for paired end data.

\item
glmFit() now stores unshrunk.coefficients from prior.count=0 as well as shrunk coefficients.

\item
estimateDisp() now has a min.row.sum argument to protect against all zero counts.

\item 
APL calculations in estimateDisp() are hot-started using fitted values from previous dispersions, to avoid discontinuous APL landscapes. 

\item
adjustedProfileLik() is modified to accept starting coefficients. glmFit() now passes starting coefficients to mglmOneGroup().

\item
calcNormFactors() is now a S3 generic function.

\item
The SAGE datasets from Zhang et al (1997) are no longer included with the edgeR package.
}}


\section{Version 3.6.0}{\itemize{

\item
Improved treatment of fractional counts.
Previously the classic edgeR pipeline permitted fractional counts but the glm pipeline did not.
edgeR now permits fractional counts throughout.

\item
All glm-based functions in edgeR now accept quantitative observation-level weights.
The glm fitting function mglmLS() and mglmSimple() are retired, and all glm fitting is now done by either mglmLevenberg() or mglmOneWay().

\item
New capabilities for robust estimation allowing for observation-level outliers.
In particular, the new function estimateGLMRobustDisp() computes a robust dispersion estimate for each gene.

\item
More careful calculation of residual df in the presence of exactly zero fitted values for glmQLFTest() and estimateDisp().
The new code allows for deflation of residual df for more complex experimental designs.

\item
New function processHairpinReads() for analyzing data from shRNA-seq screens.

\item
New function sumTechReps() to collapse counts over technical replicate libraries.

\item
New functions nbinomDeviance() and nbinomUnitDeviance.
Old function deviances.function() removed.

\item
New function validDGEList().

\item
rpkm() is now a generic function, and it now tries to find the gene lengths automatically if available from the annotation information in a DGEList object.

\item
Subsetting a DGEList object now has the option of resetting to the library sizes to the new column sums.
Internally, the subsetting code for DGEList, DGEExact, DGEGLM, DGELRT and TopTags data objects has been simplified using the new utility function subsetListOfArrays in the limma package.

\item
To strengthen the interface and to strengthen the object-orientated nature of the functions, the DGEList methods for estimateDisp(), estimateGLMCommonDisp(), estimateGLMTrendedDisp() and estimateGLMTagwiseDisp no longer accept offset, weights or AveLogCPM as arguments.
These quantities are now always taken from the DGEList object.

\item
The User's Guide has new sections on read alignment, producing a table of counts, and on how to translate scientific questions into contrasts when using a glm.

\item
camera.DGEList(), roast.DGEList() and mroast.DGEList() now include ... argument.

\item
The main computation of exactTestByDeviance() now implemented in C++ code.

\item
The big.count argument has been removed from functions exactTestByDeviance() and exactTestBySmallP().

\item
New default value for offset in dispCoxReid.

\item
More tolerant error checking for dispersion value when computing aveLogCPM().

\item
aveLogCPM() now returns a value even when all the counts are zero.

\item
The functions is.fullrank and nonEstimable are now imported from limma.
}}


\section{Version 3.4.0}{\itemize{

\item
estimateDisp() now creates the design matrix correctly when the design matrix is not given as an argument and there is only one group.  Previously this case gave an error.

\item
plotMDS.DGEList now gives a friendly error message when there are fewer than 3 data columns.

\item
Updates to DGEList() so that arguments lib.size, group and norm.factors are now set to their defaults in the function definition rather than set to NULL.
However NULL is still accepted as a possible value for these arguments in the function call, in which case the default value is used as if the argument was missing.

\item
Refinement to cutWithMinN() to make the bin numbers more equal in the worst case.
Also a bug fix so that cutWithMinN() does not fail even when there are many repeated x values.

\item
Refinement to computation for nbins in dispBinTrend.  Now changes more smoothly with the number of genes.  trace argument is retired.

\item
Updates to help pages for the data classes.

\item
Fixes to calcNormFactors with method="TMM" so that it takes account of lib.size and refCol if these are preset.

\item
Bug fix to glmQLFTest when plot=TRUE but abundance.trend=FALSE.

\item
predFC() with design=NULL now uses normalization factors correctly.
However this use of predFC() to compute counts per million is being phased out in favour of cpm().
}}

\section{Version 3.2.0}{\itemize{

\item
The User's Guide has a new section on between and within subject designs and a new case study on RNA-seq profiling of unrelated Nigerian individuals.
Section 2.9 (item 2) now gives a code example of how to pre-specify the dispersion value.

\item
New functions estimateDisp() and WLEB() to automate the estimation
of common, trended and tagwise dispersions.
The function estimateDisp() provides a simpler alternative pipeline and in principle replaces all the other dispersion estimation functions, for both glms and for classic edgeR.
It can also incorporate automatic estimation of the prior degrees of freedom, and can do this in a robust fashion.

\item
glmLRT() now permits the contrast argument to be a matrix with multiple columns, making the treatment of this argument analogous to that of the coef argument.

\item
glmLRT() now has a new F-test option.
This option takes into account the uncertainty with which the dispersion is estimated and is more conservative than the default chi-square test.

\item
glmQLFTest() has a number of important improvements.
It now has a simpler alternative calling sequence: it can take either a fitted model object as before, or it can take a DGEList object and design matrix and do the model fit itself.
If provided with a fitted model object, it now checks whether the dispersion is of a suitable type (common or trended).
It now optionally produces a plot of the raw and shrunk residual mean deviances versus AveLogCPM.
It now has the option of robustifying the empirical Bayes step.
It now has a more careful calculation of residual df that takes special account of cases where all replicates in a group are identically zero.

\item
The gene set test functions roast(), mroast() and camera() now have methods defined for DGEList data objects.
This facilitates gene set testing and pathway analysis of expression profiles within edgeR.

\item
The default method of plotMDS() for DGEList objects has changed.
The new default forms log-counts-per-million and computes Euclidean distances.
The old method based on BCV-distances is available by setting method="BCV".
The annotation of the plot axes has been improved so that the distance method used is apparent from the plot.

\item
The argument prior.count.total used for shrinking log-fold-changes has been changed to prior.count in various functions throughout the package, and now refers to the average prior.count per observation rather than the total prior count across a transcript.
The treatment of prior.counts has also been changed very slightly in cpm() when log=TRUE.

\item
New function aveLogCPM() to compute the average log count per million for each transcript across all libraries.
This is now used by all functions in the package to set AveLogCPM, which is now the standard measure of abundance. 
The value for AveLogCPM is now computed just once, and not updated when the dispersion is estimated or when a linear model is fitted.
glmFit() now preserves the AveLogCPM vector found in the DGEList object rather than recomputing it.
The use of the old abundance measure is being phased out.

\item
The glm dispersion estimation functions are now much faster.

\item
New function rpkm() to compute reads per kilobase per million (RPKM).

\item
New option method="none" for calcNormFactors().

\item
The default span used by dispBinTrend() has been reduced.

\item
Various improvements to internal C++ code.

\item
Functions binCMLDispersion() and bin.dispersion() have been removed as obsolete.

\item
Bug fix to subsetting for DGEGLM objects.

\item
Bug fix to plotMDS.DGEList to make consistent use of norm.factors.
}}


\section{Version 3.0.0}{\itemize{

\item
New chapter in the User's Guide covering a number of common types of experimental designs, including multiple groups, multiple factors and additive models.
New sections in the User's Guide on clustering and on making tables of read counts.
Many other updates to the User's Guide and to the help pages.

\item
New function edgeRUsersGuide() to open the User's Guide in a pdf viewer.

\item
Many functions have made faster by rewriting the core computations in C++.
This includes adjustedProfileLik(), mglmLevenberg(), maximizeInterpolant() and goodTuring().

\item
New argument verbose for estimateCommonDisp() and estimateGLMCommonDisp().

\item
The trended dispersion methods based on binning and interpolation have been rewritten to give more stable results when the number of genes is not large.

\item
The amount by which the tagwise dispersion estimates are squeezed towards the global value is now specified in estimateTagwiseDisp(), estimateGLMTagwiseDisp() and dispCoxReidInterpolateTagwise() by specifying the prior degrees of freedom prior.df instead of the prior number of samples prior.n.

\item
The weighted likelihood empirical Bayes code has been simplified or developed in a number of ways.
The old functions weightedComLik() and weightedComLikMA() are now removed as no longer required.

\item
The functions estimateSmoothing() and approx.expected.info() have been removed as no longer recommended.

\item
The span used by estimateGLMTagwiseDisp() is now chosen by default as a decreasing function of the number of tags in the dataset.

\item
New method "loess" for the trend argument of estimateTagwiseDisp, with "tricube" now treated as a synonym.

\item
New functions loessByCol() and locfitByCol() for smoothing columns of matrix by non-robust loess curves.
These functions are used in the weighted likelihood empirical Bayes procedures to compute local common likelihood.

\item
glmFit now shrinks the estimated fold-changes towards zero.
The default shrinkage is as for exactTest().

\item
predFC output is now on the natural log scale instead of log2.

\item
mglmLevenberg() is now the default glm fitting algorithm, avoiding the occasional errors that occurred previously with mglmLS().

\item
The arguments of glmLRT() and glmQLFTest() have been simplified so that the argument y, previously the first argument of glmLRT, is no longer required.

\item
glmQLFTest() now ensures that no p-value is smaller than what would be obtained by treating the likelihood ratio test statistic as chisquare.

\item
glmQLFTest() now treats tags with all zero counts in replicate arrays as having zero residual df.

\item
gof() now optionally produces a qq-plot of the genewise goodness of fit statistics.

\item
Argument null.hypothesis removed from equalizeLibSizes().

\item
DGEList no longer outputs a component called all.zeros.

\item
goodTuring() no longer produces a plot.
Instead there is a new function goodTuringPlot() for plotting log-probability versus log-frequency.
goodTuring() has a new argument 'conf' giving the confidence factor for the linear regression approximation.

\item
Added plot.it argument to maPlot().
}}


\section{Version 2.6.0}{\itemize{

\item
edgeR now depends on limma.

\item
Considerable work on the User's Guide.
New case study added on Pathogen inoculated arabidopsis illustrating a two group comparison with batch effects.
All the other case studies have been updated and streamlined.
New section explaining why adjustments for GC content and mappability are not necessary in a differential expression context.

\item
New and more intuitive column headings for topTags() output.
'logFC' is now the first column.
Log-concentration is now replaced by log-counts-per-million ('logCPM').
'PValue'  replaces 'P.Value'.
These column headings are now inserted in the table of results by
exactTest() and glmLRT() instead of being modified by the show method for the TopTags object generated by topTags().
This means that the column names will be correct even when users access the fitted model objects
directly instead of using the show method.

\item
plotSmear() and plotMeanVar() now use logCPM instead of logConc.

\item
New function glmQLFTest() provides quasi-likelihood hypothesis testing using F-tests, as an alternative to likelihood ratio tests using the chisquare distribution.

\item
New functions normalizeChIPtoInput() and calcNormOffsetsforChIP()
for normalization of ChIP-Seq counts relative to input control.

\item
New capabilities for formal shrinkage of the logFC.
exactTest() now incorporates formal shrinkage of the logFC, controlled by argument 'prior.count.total'.
predFC() provides similar shrinkage capability for glms.

\item
estimateCommonDisp() and estimateGLMCommonDisp() now set the dispersion to NA when there is no replication,
instead of setting the dispersion to zero.
This means that users will need to set a dispersion value explicitly to use functions further down the analysis pipeline.

\item
New function estimateTrendedDisp() analogous to estimateGLMTrendedDisp() but for classic edgeR.

\item
The algorithms implemented in estimateTagwiseDisp() now uses fewer grid points but interpolates, similar to estimateGLMTagwiseDisp().

\item
The power trend fitted by dispCoxReidPowerTrend() now includes a positive asymptote.
This greatly improves the fit on real data sets.
This now becomes the default method for estimateGLMTrendedDisp() when the number of genes is less than 200.

\item
New user-friendly function plotBCV() displays estimated dispersions.  

\item
New argument target.size for thinCounts().

\item
New utility functions getDispersion() and zscoreNBinom().

\item
dimnames() methods for DGEExact, DGELRT and TopTags classes.

\item
Function pooledVar() removed as no longer necessary.

\item
Minor fixes to various functions to ensure correct results in special cases.
}}


\section{Version 2.4.0}{\itemize{

\item New function spliceVariants() for detecting alternative exon usage
      from exon-level count data.

\item A choice of rejection regions is now implemented for exactTest(),
      and the default is changed from one based on small probabilities
      to one based on doubling the smaller of the tail probabilities.
      This gives better results than the original conditional test when
      the dispersion is large (especially > 1).  A Beta distribution
      approximation to the tail probability is also implemented when
      the counts are large, making exactTest() much faster and
      less memory hungry.

\item estimateTagwiseDisp() now includes an abundance trend on the
      dispersions by default.
      
\item exactTest() now uses tagwise.dispersion by default if found in the
      object.
    
\item estimateCRDisp() is removed. It is now replaced by
      estimateGLMCommonDisp(), estimateGLMTrendedDisp() and
      estimateGLMTagwiseDisp().
    
\item Changes to glmFit() so that it automatically detects dispersion
      estimates if in data object. It uses tagwise if available, then
      trended, then common.

\item Add getPriorN() to calculate the weight given to the common
      parameter likelihood in order to smooth (or stabilize) the
      dispersion estimates. Used as default for estimateTagwiseDisp and
      estimateGLMTagwiseDisp().
    
\item New function cutWithMinN() used in binning methods.
    
\item glmFit() now S3 generic function, and glmFit() has new method
      argument specifying fitting algorithm.
    
\item DGEGLM objects now subsettable.
    
\item plotMDS.dge() is retired, instead a DGEList method is now defined for 
      plotMDS() in the limma package.  One advantage is that the plot can
      be repeated with different graphical parameters without recomputing
      the distances.  The MDS method is also now much faster.
    
\item Add as.data.frame method for TopTags objects.
    
\item New function cpm() to calculate counts per million conveniently. 
    
\item Adding args to dispCoxReidInterpolateTagwise() to give more access to 
      tuning parameters.
    
\item estimateGLMTagwiseDisp() now uses trended.dispersion by default if 
      trended.dispersion is found.
      
\item Change to glmLRT() to ensure character coefficient argument will work.
    
\item Change to maPlot() so that any really extreme logFCs are brought back 
      to a more reasonable scale.
      
\item estimateGLMCommonDisp() now returns NA when there are no residual
      df rather than returning dispersion of zero.

\item The trend computation of the local common likelihood in
      dispCoxReidInterpolateTagwise() is now based on moving averages
      rather than lowess.

\item Changes to binGLMDispersion() to allow trended dispersion for data
      sets with small numbers of genes, but with extra warnings.

\item dispDeviance() and dispPearson() now give graceful estimates and
      messages when the dispersion is outside the specified interval.

\item Bug fix to mglmOneWay(), which was confusing parametrizations when
      the design matrix included negative values.

\item mglmOneWay() (and hence glmFit) no longer produces NA coefficients
      when some of the fitted values were exactly zero.

\item Changes to offset behaviour in estimateGLMCommonDisp(),
      estimateGLMTrendedDisp() and estimateGLMTagwiseDisp() to fix bug.
      Changes to several other functions on the way to fixing bugs
      when computing dispersions in data sets with genes that have all
      zero counts.

\item Bug fix to mglmSimple() with matrix offset.
    
\item Bug fix to adjustedProfLik() when there are fitted values exactly
      at zero for one or more groups.
}}


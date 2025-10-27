READmediterranean.txt

Code for reproducing analyses of the Mediterranean data, for a manuscript being
 submitted (the manuscript is not publically available yet; this is currently
 just for reviewers). If accepted this will be updated with a proper citation. 

Some code could be used as a template for other analyses. 

Each folder is for a different scenario, including sensitivity tests (each
described below; this does include some of our working notes and ideas). Each
folder has an med-analysis-*.Rmd file to do the detailed analyses, and then a
med-summary-*.Rmd to make a shorter summary pdf. The pdf's automatically do the
analyses and create results figures and tables, but we did not interpret results
in each one (so some of the manual text was not updated, we just wrote it in the
Supp Material for the relevant runs). The pdf's are not
pushed to Github (to not bloat the repository), but are available upon request. 

Main results mentioned in the manuscript are 
mediterranean-analysis-4/ - the base assumptions
mediterranean-analysis-12/ - Sensitivity Example A
mediterranean-analysis-15/ - Sensitivity Example B

Details of each scenario are:

mediterranean-analysis-3/
*************************
 med-analysis.Rmd - renaming, functionalising and adapting earlier med...-2/zabala-data-analysis.Rmd,
 med-summary.Rmd from - renaming and adapting med...-2/zabala-summary.Rmd.
 Doing what we discussed in the 8/5/25 discussion.

mediterranean-analysis-4/
*************************
 Rerunning with corrected length resolutions (0.01, 0.1, 1 mm), as explained at
 the start of med-analysis-4.Rmd. Originally figured out in MLEbins vignette but now doing
 here first.
 xmin is the min `bin_min` value above the histogram (1-g bins) determined minimum
 med-analysis-length-resolution.Rmd - explains all that.
 med-analysis-4.Rmd - redoing the analysis.
 med-summary-4.Rmd - summarising results and doing aggregate plots, based on
  code from May 2025.

 Rerunning from scratch before doing analysis-5. Took 50 minutes to run.

Going to work through the assumptions, starting assuming the baseline xmin, then
the FG xmin, then the NTR xmin (based on Table on p84 of analysis-4). Will need a few .Rmd files, but gradually
whittle down the analyses in each as won't need everything. Then doing min(xmin)
for each group, which actually makes more sense.

Simplify 4 to use as 5 as a template (can shorten the initial part), and then
  use for 6,7,8.

Decided as went along to just rerun all results (even though some will be
  duplicated analyses) - easiest to just edit a few lines and then wait for them to
  run. Will also allow confirmation, and automatically build the tables and
  summary plots when done. And .pdfs will all look similar. Not updating text
  for results (as indicated by TODOs in there).

So continuing based on 4, but, based on those results, trying
  different approaches for xmin as a sensitivity, to see how results change with
  xmin assumption.


mediterranean-analysis-5/
*************************
  baseline xmin for each group (make notes in .Rmd) . Obtain automatically from version 4 results.

  In 4 we did the histograms (1-g width) and used xmin as the minimum bin_min
  within the modal histogram bin.

  Fits generally look worse when xmin not determined by that data set (as
  expected). Chondr NTR is almost uniform (b = 0).

mediterranean-analysis-6/
*************************
  use 5 as template
  fg xmin for each.

  full: baseline fit not great, ntr not too bad, b still changes in the expected
  direction
  crust: baseline fit poor, ntr a bit better, spectra still steepening with
   fishing
  actin: baseline fit good (since data are declining), ntr not so good, spectra
   steepening with fishing
  chondr: baseline not bad, ntr also not too bad. b values actually go in the expected
   direction.
  ceph small: only change slightly since xmins from analysis-4 are all fairly
   close
  ceph large: fits not great.


mediterranean-analysis-7/
*************************
  use 5 or 6 as template.
  ntr xmin for each

mediterranean-analysis-8/
*************************
  do a global xmin of the value obtained for the global community, namely
  1.01...  Use for all fits for consistency.
  Lots of the fits look very poor, so not recommending.

mediterranean-analysis-9/
*************************
  decide on base case of the above, and do sensitivity to xmax; just set xmax
  to baseline value (didn't mean to do this, see 10). So far 4 is still looking most
  sensible, as some of the other fits are not good (since fitting a declining
  function to an increasing then decreasing data set).
    So xmin still determined individually for each group.

  Accidentally set xmax to baseline xmax (and hadn't taken out low outlier
  (small crustacea in NTR) so the baseline and crustacea NTR need to be redone),
  so keep results, but then do 10 (and now doing 11 and 12 anyway):

mediterranean-analysis-10/
*************************
  as for 9 but with just setting xmax
  within each group to be the max of the xmax from analysis-4.
  So allowing for the
  possibility of, for example, a huge crustacean if we've seen one once. Be good
  for understanding sensitivity to xmax.
  So xmin still determined individually for each group.
  Looking at properly now:
 also hadn't taken out low outlier (small crustacea in NTR) so the
baseline and crustacea NTR need to be redone, but doing 11 and 12 now anyway.

mediterranean-analysis-11/
*************************
  redo 4 but with min(xmin) for each group. Make sure to take out small
  crustacea outlier in NTR (and leave that code in now, since always needed -
 actually don't need to add in here as are fixing xmin) so can use 10 as
 template, commenting out xmax stuff since will use it in 12.
 Results may well already be spread out between .pdfs, but simpler to rerun all
 in one go.

Fits are just not as good, because sometimes fitting a decreasing distribution
to unimodal data.

mediterranean-analysis-12/
*************************
  xmin as min(xmin) for each group (run 11),
  then with xmax to  be max(xmax) within each group.
 has run, done summary.
Again, fits are just not as good.

So conclude to stick with version 4 (as per Robinson et al), and say we tried 11 and 12
but fits to data not satisfactory. The aggregated, restricted and normalised
method overcomes some of the issues. Can show crustacea results in main text
maybe as that is the focus of the manuscript, and will show some real data.

Taking med-summary-4.Rmd, copying .tex over to
size-spectra-applications\zabala\supp-material\ and using as template for
supp-size-spectra.tex. Copied the figures over for simplicity.

Juliana chat (3/10/25) led to new runs:

mediterranean-analysis-13/
*************************

Juliana suggested an xmin for each group, but based on looking for gaps in data
(see a) above).
 copying -11 to here and editing. Also separate xmax for each.

	      	Range
		x_min	x_max
Community	0.2	1000
Crustacea	0.2	200
Actinopterygii	0.2	1000
Chondrichthyes	2	500
Ceph small	1	10
Ceph large	10	1000

I think the fits are just too poor to use, since fitting to sparse data at the
low end.

mediterranean-analysis-14/
*************************
not done yet, as 13 were so poor:
- do the same, but for crustacea baseline and ceph small also: reduce xmax
to keep continuous

mediterranean-analysis-15/
*************************

Doing as med-4, but for crustacea baseline and ceph small also (maybe ceph large
ntr also): reduce xmax to
keep continuous. There are some outliers, even on log scale, that do drive xmax.

Have to look at those manually in med-4, and determine values.
So load in med-4 results in analysis-15, and look at the tail values. See if
there's any easy way to automate a rule.

Total bin counts of 21 removed. So no difference to full fits (since not
affecting xmax values).

Compared results to med-4, check that only the intended ones have
changed. Rerunning all plots, tables and saving, just not the calculations
(cacheing wasn't properly updating everything).
  - baseline full - n is 15 less in med-15, b unchanged
  - ntr full - n is 7 less in med-15, b unchanged
  - baseline crust - n is 7 less, b -1.28 to -1.15
  - baseline cephsmall - n is 7 less, b -3.88 to -1.95
  - ntr cephlarge - n is 7 less, b -3.63 to  -3.20


magick convertion from .pdf to .jpeg to go into Word, trying
 magick -density 300 figure4.pdf -quality 100 figure4.jpeg



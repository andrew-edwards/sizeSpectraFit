mediterranean-analysis-1/ - first attempts based on Juliana's code, doing
  individual body masses and MLE, but that doesn't take into account standardisation
  that gives non-integer counts.

mediterranean-analysis-2/ - pdf as discussed in our chat on 8/5/25. Figuring out
  functionalisation and plotting and tables using MLEbins, with
  different xmin and xmax approaches. Had excluded <100 mm, and
  had kept in all fish but not going to now.
  Have confirmed that the running of meditteranean-data.Rmd (renamed from
  data-raw/zabala-data.Rmd) has not change the data .rda files, so this code
  should still produce the same results (8/5/25, check the commit dates of .rda
  files if looking to fully reproduce the results). Not committing; if want
  early version then get first version of
  mediterranean-analysis-3/med-analysis.Rmd and med_summary.Rmd, as they are
  copied into that directory before editing.

mediterranean-analysis-3/
 med-analysis.Rmd - renaming and adapting med...-2/zabala-data-analysis.Rmd,
 med-summary.Rmd from - renaming and adapting med...-2/zabala-summary.Rmd.
 Doing what we discussed in the 8/5/25 discussion.

 saving the .pdfs with new filenames in these:
 - xmin-still-integer/ - results saved (with <100 mm not excluded but big fish still
   included)  while still having xmin be the min of the
   integer bin, before changing to min bin_min above the min of the integer
   bin. Want to keep this to compare the changed version to see if it makes a
   difference.

 - xmin-not-integer/ above run but setting xmin to be the min bin_min above the
   histogram determined version. Should note that when describing histograms.
   NOT COMPARED wiht previous yet, copying over and running the no-large-fish
   fun.

mediterranean-analysis-4/
 Rerunning with corrected length resolutions (0.01, 0.1, 1 mm), as explained at
 the start of med-analysis.Rmd. Originally figured out in MLEbins vignette but now doing
 here first.
 xmin is the min `bin_min` value above the histogram (1-g bins) determined minimum
 med-analysis-length-resolution.Rmd - explains all that.
 med-analysis-4.Rmd - redoing the analysis.
 med-summary-4.Rmd - summarising results and doing aggregate plots, found some
 code from May 2025.

 Rerunning from scratch before doing analysis-5. Took 50 minutes to run.

--
From chatting with Juliana on 19/8/25, decided to (see below for explicit plan):

Crustacea - fix xmin across strata, using each of the three values that come
out. Makes sense for crustacea. May as well do same assumption for full
community, since xmin values are the same as those for crustacea.
Also xmax, maybe biggest of each.

Actin (small fish) - try both xmin.

Crust and fish appear much better in NTR (from being out on the water), so 'suspicious' of results.

Sharks like disturbed grounds so can find in FG. Try both xmin, not the big one
(will probably do for completeness).

Cephalopod small and large - also change xmin. Expect maybe doing worse as not
commercial species, just there, and not that many.

She mentioned that xmin = 0.2 worked well for her in the past analysis, sounds
low, could try a global xmin of something low?

--

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
  for results (as indicated by TO DO in there).

So continuing based on 4, but, based on those results, trying
  different approaches for xmin as a sensitivity, to see how results change with
  xmin assumption.


mediterranean-analysis-5/
  baseline xmin for each group (make notes in .Rmd) . Obtain automatically from version 4 results.

  In 4 we did the histograms (1-g width) and used xmin as the minimum bin_min
  within the modal histogram bin.

  Fits generally look worse when xmin not determined by that data set (as
  expected). Chondr NTR is almost uniform (b = 0).

mediterranean-analysis-6/
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
  use 5 or 6 as template.
  ntr xmin for each

mediterranean-analysis-8/
  do a global xmin of the value obtained for the global community, namely
  1.01...  Use for all fits for consistency.
  Lots of the fits look very poor, so not recommending.

mediterranean-analysis-9/
  decide on base case of the above, and do sensitivity to xmax; just set xmax
  to baseline value (didn't mean to do this, see 10). So far 4 is still looking most
  sensible, as some of the other fits are not good (since fitting a declining
  function to an increasing then decreasing data set).
    So xmin still determined individually for each group.

  Accidentally set xmax to baseline xmax (and hadn't taken out low outlier
  (small crustacea in NTR) so the baseline and crustacea NTR need to be redone),
  so keep results, but then do 10 (and now doing 11 and 12 anyway):

mediterranean-analysis-10/
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
  redo 4 but with min(xmin) for each group. Make sure to take out small
  crustacea outlier in NTR (and leave that code in now, since always needed -
 actually don't need to add in here as are fixing xmin) so can use 10 as
 template, commenting out xmax stuff since will use it in 12.
 Results may well already be spread out between .pdfs, but simpler to rerun all
 in one go.

 has run, done summary.
Fits are just not as good, because sometimes fitting a decreasing distribution
to unimodal data.

mediterranean-analysis-12/
  xmin as min(xmin) for each group (run 11),
  then with xmax to  be max(xmax) within each group.
 has run, done summary.
Again, fits are just not as good.

So conclude to stick with 4 (as per Robinson et al), can say we tried 11 and 12
but fits to data not satisfactory. The aggregated, restricted and normalised
method overcomes some of the issues. Can show crustacea results in main text
maybe as that is the focus of the manuscript, and will show some real data.

Taking med-summary-4.Rmd, copying .tex over to
size-spectra-applications\zabala\supp-material\ and using as template for
supp-size-spectra.tex. Copied the figures over for simplicity.

HERE - now just writing on .tex in size-spectra-applications/



Juliana 21/8/25 (based on up to 10):

Thanks a lot for all the documents!

I agree with you that setting the xmin as we did for version 4 is best. I would discard versions 5, 6, 7 and 8. I am conflicted between versions 9 and 10 since having the highest xmax also changes the results quite a bit when its only a couple of individuals with that size.

I'm leaning more to version 4, but I don't know how to make the results between groups (baseline, fg, ntr) comparable, if they all have different xmin. I know it fits the model best if we start with the highest bin but it means we're excluding a good portion of the sample to fit the model, when we know by the histograms that those individuals had a good chance of being caught since they're larger than 12mm (net size). Especially in the case of the sharks (chondrichthyes), which would have a value closer to 0 in the NTR as the sizes seem to be 'levelling' - a good indicator of an area in transition to recovery. Perhaps if we use smaller bin we can incorporate more of the data, for example 0.5g bins. Do you think this makes sense? Could you tell me a bit about how the comparison between groups can be done this way?

I've talked to a couple cephalopod experts here and the variability in results could be due to the fact that cephalopods reproduce once a year and then die, so the protected area wouldn't have much of an effect on them. Its normal that they're divided in small and big groups, its just due to the species types that live in the area. Its also normal to find more of the smaller species.

Thank you so much again!

Juliana

Juliana felt before: ceph and chondrichthytes seemed to pull the fit a lot for full
community, and the sharks can move
around a lot. Also, although the sampling was the same for all species groups, the main interest is in seeing how
crustaceans changed (especially as they do not move as much).


[and see other emails maybe]

mediterranean-analysis-10/


further ideas?
  Probably want to fix xmin for some maybe? The chondr one is strange.

magick <filename>.pdf <filename>.png   to make .png's I included into the doc file.


2/10/25 and just before from Juliana:

I think the current graphs are very nice and my supervisors think we should
include the multi-graph graph, as it gives a good idea of where the final graph
come from. We also wonder if we could try the code with a set xmin for all
groups and also with the minimum xmin, as with the histogram frequency
distribution method we seem to leave some data out.

then:

Not quite, sorry I didn't explain it well before, I meant the following:

a) a fixed xmin across the three strata for that group. For example, xmin crustacea = 0.2 for baseline, fg and ntr. We choose the xmin based on the smallest 'continuous' value we have for each strata. For example, for crustacea we have the following small values: 0.001, 0.002, 0.20, 0.22, 0.22, 0.23... We would choose 0.20 as the xmin as there's a gap between 0.002 and 0.20 and then its continuous values.

This is a little tricky to automate (as it sounds a little subjective), so let's
discuss it. [Or start looking at in mediterranean-analysis-12/ below]. She now
sent me values to use (see below).

b) xmin = the min(xmin) for each group and each strata. So, no modifications, just the minimum value for each group and strata.

This is what is done in med-summary-11.pdf (which I realise I didn't send
you before I don't think). The xmin for each group is the minimum of the three
xmin determined in analysis-4 for each group. See Table 1 on page 22 for the values.


Juliana chat (3/10/25) (see details below as have to do them):
new runs:

mediterranean-analysis-13/

Juliana suggested an xmin for each group, but based on looking for gaps in data
(see a) above).
 copying -11 to here and editing.
D use xmin from Juliana's visual suggestions (where it starts to be continuous),
saved in 'Ranges table.xls'.
D xmax independently within each strata/group combination
D then copy summary-4 and use

Text for justification is:

The objective of the table is to show x_min and x_max values for each taxonomic
group (the range). Going back to the December emails, we were discussing if to
have ranges at all, where we decided to keep the 12mm x_min (selecting
everything above 12mm for the whole data set) since the sampling net is
12x12mm. So far, we were going to discuss if we should have no set value for
x_max or if we should put a limit to it too. These 'limits' are the values in
the attached document for x_max, as we have a couple groups (especially in
Actinopterygii) where we have around 5 organisms with > 1000g, which pulls the
curve a lot. 5 organisms out of a few thousands are a very small portion and can
be considered outliers, so that is why I suggest the x_max values on the table
(e.g. 1000 for Actinopterygii). Let me know what you think about this so we
input the agreed upon range options in the function and then fill in the b
values on the table accordingly.  Values are:

	      	Range
		x_min	x_max
Community	0.2	1000
Crustacea	0.2	200
Actinopterygii	0.2	1000
Chondrichthyes	2	500
Ceph small	1	10
Ceph large	10	1000

I think the fits are just too poor to use, since fitting to sparse data at the
low end. TODO email Juliana

mediterranean-analysis-14/
not doing yet:
- do the same, but for crustacea baseline and ceph small also: reduce xmax
to keep continuous

mediterranean-analysis-15/
next:
D do as med-4, but for crustacea baseline and ceph small also (maybe ceph large
ntr also): reduce xmax to
keep continuous. There are some outliers, even on log scale, that do drive xmax.
D copy analysis-13 as a template

Have to look at those manually in med-4, and determine values.

D - So load in med-4 results in analysis-15, and look at the tail values. See if
there's any easy way to automate a rule.

Total bin counts of 21 removed. So no difference to full fits (since not
affecting xmax values).

Next:
D rerunning med-15 since cache messed up and I can't rerun summary either to
update and check figures. So check these again:

D compare results to med-4, check that only the intended ones have
changed. Rerunning all plots, tables and saving, just not the calculations
(cacheing wasn't properly updating everything).
   D baseline full - n is 15 less in med-15, b unchanged
   D ntr full - n is 7 less in med-15, b unchanged
   D baseline crust - n is 7 less, b -1.28 to -1.15
   D baseline cephsmall - n is 7 less, b -3.88 to -1.95
   D ntr cephlarge - n is 7 less, b -3.63 to  -3.20

D check crustacea ntr when recalculated, unchanged as expected
D rebuild med-summary-15.pdf

Then, just edit supp info

Juliana chat (3/10/25):
D me add to methods, why weights not lengths?
D finish editing the supp material
 D read through (double check fig numbers from new version)
 D - print and read her latest version
D rerunning med-15 analysis and summary (see above)

D redo summary4 plots and copy over, check they worked
D check edits
D spell check in Supp

D caption mention the resolution (wasn't sure what that meant)

D dotted and dashed lines in aggregated plots
D send pdf, jpeg, jpg, or tiff, figures to her for the .doc. Maybe just do .tiff
for the Word and send the .pdf's. 

D do a latexdiff, maybe not bother as so much has changed
D check about the summary plot being in main text; if not then add back into
   that section and check text again.


magick - see READpbs

- get github into usable format, and add something to the README


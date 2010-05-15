########### UTILITY FUNCTIONS for generating results text ############

strip_newlines = function(str) {
    return(gsub("\n", "", str))
}

start_results = function(str, ...) {
    formatted_string = get_formatted_string(str, ...)
    results <<- formatted_string
    all_results <<- paste(all_results, formatted_string, sep="")
}

get_formatted_string = function(str, ...)
    if (length(list(...)) > 0) {
        formatted_string = sprintf(str, ...)
    } else {
        formatted_string = str
    }
    return(formatted_string)
}

append_to_results = function(str, ...) {
    formatted_string = get_formatted_string(str, ...)
    results <<- paste(results, formatted_string, sep="")
    all_results <<- paste(all_results, formatted_string, sep="")
}

count_unique_items = function(column) {
    return(dim(table(column)))
}

count_true_items = function(column) {
    return(sum(column==TRUE))
}

count_false_items = function(column) {
    return(sum(column!=TRUE))
}

########### RESULTS TEXT ############

all_results = ""
start_results("\nOchsner et al.{Ochsner} identified ")
setwd("/Users/hpiwowar/Documents/Projects/Thesis/Aim2b/DISCO/revision4")

dat.read = read.csv("rawdata.csv", header=TRUE, sep=",")

number_of_data_producing_publications_found_by_ochsner = 
    count_unique_items(dat.read$pmid)
append_to_results(number_of_data_producing_publications_found_by_ochsner) 
append_to_results(" studies that generated gene expression microarray data.")
results

start_results("  Ochsner’s search found ")
number_of_datasets_found_by_ochsner = 
    count_true_items(dat.read$ochsner_found_any_data)
append_to_results(number_of_datasets_found_by_ochsner)
append_to_results(" publicly-available datasets, ")
append_to_results("representing ")
number_of_data_sharing_publications_found_by_ochsner = 
    count_unique_items(dat.read$pmid[dat.read$ochsner_found_any_data == TRUE])
append_to_results(number_of_data_sharing_publications_found_by_ochsner)
append_to_results(" distinct studies")
append_to_results(" (or %.1f%% of all %d data-producing studies; some studies have more than one associated dataset).", 
                100*number_of_data_sharing_publications_found_by_ochsner / number_of_data_producing_publications_found_by_ochsner, 
                number_of_data_producing_publications_found_by_ochsner)
results

start_results("  Of the datasets, ")
number_of_datasets_found_in_geo_by_ochsner = count_true_items(dat.read$ochsner_found_geo_data)
number_of_data_producing_publications_found_by_ochsner_that_shared_data = count_true_items(dat.read$ochsner_found_any_data)
append_to_results("%d (%.1f%%)", 
                number_of_datasets_found_in_geo_by_ochsner, 
                100*number_of_datasets_found_in_geo_by_ochsner / number_of_data_producing_publications_found_by_ochsner_that_shared_data)
append_to_results(" were found in the Gene Expression Omnibus (GEO) database, ")
number_of_datasets_found_in_arrayexpress_by_ochsner = count_true_items(dat.read$ochsner_found_arrayexpress_data)
append_to_results("%d (%.1f%%)", 
                number_of_datasets_found_in_arrayexpress_by_ochsner, 
                100*number_of_datasets_found_in_arrayexpress_by_ochsner /  number_of_data_producing_publications_found_by_ochsner_that_shared_data)               
append_to_results(" in ArrayExpress, ")
number_of_datasets_found_on_journal_sites_by_ochsner = count_true_items(dat.read$ochsner_journal)
append_to_results("%d (%.1f%%)", 
                number_of_datasets_found_on_journal_sites_by_ochsner, 
                100*number_of_datasets_found_on_journal_sites_by_ochsner /  number_of_data_producing_publications_found_by_ochsner_that_shared_data)               
append_to_results(" hosted on journal websites, and ")
number_of_datasets_found_elsewhere_by_ochsner = count_true_items(dat.read$ochsner_other)
append_to_results("%d (%.1f%%)", 
                number_of_datasets_found_elsewhere_by_ochsner, 
                100*number_of_datasets_found_elsewhere_by_ochsner /  number_of_data_producing_publications_found_by_ochsner_that_shared_data)               
append_to_results(" on lab websites and smaller online data repositories ")
append_to_results("(some datasets were found in more than one location).")
results

start_results("  Combined, GEO and ArrayExpress housed ")
number_of_datasets_found_in_geo_or_arrayexpress_by_ochsner = 
    count_true_items(dat.read$ochsner_found_geo_data | dat.read$ochsner_found_arrayexpress_data)
append_to_results("%d (%.1f%%)", 
                number_of_datasets_found_in_geo_or_arrayexpress_by_ochsner, 
                100*number_of_datasets_found_in_geo_or_arrayexpress_by_ochsner /  number_of_data_producing_publications_found_by_ochsner_that_shared_data)               
append_to_results(" of the manually-located datasets.")
results

start_results("\nNext, we ran automated queries.  We queried the GEO and ArrayExpress databases with the PubMed IDs of the ")
append_to_results(number_of_data_producing_publications_found_by_ochsner)
append_to_results(" data-producing studies.")
results


start_results("  The automated queries returned ")
number_of_oschnser_pmid_link_to_geo_or_arrayexpress = count_true_items(dat.read$centralized_link_to_ochsner_pmid & (dat.read$in_geo | dat.read$in_arrayexpress))
append_to_results(number_of_oschnser_pmid_link_to_geo_or_arrayexpress)
append_to_results(" datasets in total, with ")
number_of_oschnser_pmid_link_to_geo = count_true_items(dat.read$centralized_link_to_ochsner_pmid & dat.read$in_geo)
append_to_results(number_of_oschnser_pmid_link_to_geo)
append_to_results(" datasets from GEO and ")
number_of_oschnser_pmid_link_to_arrayexpress = count_true_items(dat.read$centralized_link_to_ochsner_pmid & dat.read$in_arrayexpress)
append_to_results(number_of_oschnser_pmid_link_to_arrayexpress)
append_to_results(" datasets from ArrayExpress, including ")
number_of_oschnser_pmid_link_to_geo_AND_arrayexpress = count_true_items(dat.read$centralized_link_to_ochsner_pmid & dat.read$in_geo & dat.read$in_arrayexpress)
append_to_results(number_of_oschnser_pmid_link_to_geo_AND_arrayexpress)
append_to_results(" datasets in both databases (ArrayExpress has a policy of importing selected GEO submissions).")
results

start_results("\nWe compared the two retrieval sets: the automated search found ")
number_datasets_found_by_automated_not_ochsner_geo_arrayexpress = 
    count_true_items(dat.read$centralized_link_to_ochsner_pmid & !dat.read$ochsner_found_any_data)
append_to_results(number_datasets_found_by_automated_not_ochsner_geo_arrayexpress)
append_to_results(" datasets overlooked by the manual search, ")
append_to_results("while the manual search found ")
number_datasets_found_by_ochsner_geo_arrayexpress_not_automated = 
    count_true_items((dat.read$ochsner_found_geo_data | dat.read$ochsner_found_arrayexpress_data) & !dat.read$centralized_link_to_ochsner_pmid)
append_to_results(number_datasets_found_by_ochsner_geo_arrayexpress_not_automated)    
append_to_results(" datasets in GEO and ArrayExpress that were not found automatically")
append_to_results(" (plus ")
number_datasets_ochsner_not_geo_arrayexpress_or_automated = 
    count_true_items((dat.read$ochsner_journal | dat.read$ochsner_other) & !dat.read$centralized_link_to_ochsner_pmid)
append_to_results(number_datasets_ochsner_not_geo_arrayexpress_or_automated)        
append_to_results(" datasets in other internet locations, as described above).")
results

start_results("  A union of the manual and automated searches includes ")
number_of_datasets_found_via_manual_or_automated = count_true_items(dat.read$data_available)
append_to_results(number_of_datasets_found_via_manual_or_automated)
append_to_results(" distinct datasets")
append_to_results("; we consider this our reference set of 'publicly-available datasets' in the analysis below.")
#append_to_results("; a breakdown by repository location is given in Table 1.")
results

start_results("Figure 1 shows the relative recall of the search strategies.")
append_to_results("  As illustrated in the first bar of Figure A, the manual search retrieved ")
append_to_results("%.1f%%", 
                100*number_of_datasets_found_by_ochsner / number_of_datasets_found_via_manual_or_automated)               
append_to_results(" of the publicly-available datasets.")

append_to_results("  The second bar highlights the datasets available in either GEO or ArrayExpress: ")
number_datasets_found_geo_arrayexpress_union = 
    count_true_items(dat.read$ochsner_found_geo_data | dat.read$ochsner_found_arrayexpress_data | dat.read$centralized_link_to_ochsner_pmid)
append_to_results("%.1f%%", 
                100*number_datasets_found_geo_arrayexpress_union / number_of_datasets_found_via_manual_or_automated)               
append_to_results(" of the total.")

append_to_results("  Automated searches retrieve ")
append_to_results("%.1f%%", 
                100*number_of_oschnser_pmid_link_to_geo_or_arrayexpress / number_datasets_found_geo_arrayexpress_union)               
append_to_results(" of the datasets housed in GEO and/or ArrayExpress, or ")
append_to_results("%.1f%%", 
                100*number_of_oschnser_pmid_link_to_geo_or_arrayexpress / number_of_datasets_found_via_manual_or_automated)               
append_to_results(" of datasets housed anywhere.")

append_to_results("  Finally, the last two bars depict on the databases individually.  Automated searches of just GEO and just ArrayExpress retrieve ")
append_to_results("%.1f%%", 
                100*number_of_oschnser_pmid_link_to_geo / number_of_datasets_found_via_manual_or_automated)               
append_to_results(" and ")
append_to_results("%.1f%%", 
                100*number_of_oschnser_pmid_link_to_arrayexpress / number_of_datasets_found_via_manual_or_automated)               
append_to_results(" of all available datasets, respectively.")
results

start_results("\n\n\n<<TABLE 1 goes here!>>\n\n\n")
cat(all_results)


start_results("Next, we looked at univariate patterns to see if the datasets retrieved automatically ")
append_to_results("differed from those that were retrieved manually.  ")
append_to_results(strip_newlines("
The odds that a dataset was about cancer, performed on an Affymetrix platform, 
involved humans, or involved cultured cells were not significantly associated 
with whether the dataset was retrievable through automated mechanisms  (p>0.3). 
These odds ratios and their confidence intervals are depicted in Figure 2. 
Similarly, in ANOVA analysis, the species was not significantly different 
between datasets found automatically and those not found automatically (p=0.7).
"))



library(Hmisc,T)
library(Design,T)
library(sciplot)
library(rmeta)

dat.raw = dat.read[which(dat.read$data_available == '1'),]
dim(dat.raw)
names(dat.raw) = gsub("_", ".", names(dat.raw))
names(dat.raw) = gsub(" ", ".", names(dat.raw))
names(dat.raw)

dat = dat.raw
levels(dat$pubmed.journal)[which(levels(dat$pubmed.journal) == "Proc Natl Acad Sci U S A")] = "PNAS"
#dat$impact.factor = as.numeric(levels(dat.raw$impact.factor)[dat.raw$impact.factor])

# Create some helpful summary columns
dat$all = dat$pmid > ""
dat$in.arrayexpress.or.geo = dat$in.arrayexpress | dat$in.geo
dat$centralized.arrayexpress = dat$in.arrayexpress & dat$centralized.link.to.ochsner.pmid
dat$centralized.geo = dat$in.geo & dat$centralized.link.to.ochsner.pmid
dat$centralized.arrayexpress.or.geo = dat$in.arrayexpress.or.geo & dat$centralized.link.to.ochsner.pmid

# Consolidate the species levels
levels(dat$species)[table(dat$species) < .03*length(dat$species)] = "Other"

# Consolidate the array design levels
affy = grep("Affymetrix", levels(dat$array.design))
levels(dat$array.design)[-affy] = "Other"
affy = grep("Affymetrix", levels(dat$array.design))
levels(dat$array.design)[affy] = "Affymetrix"


# Verify the p-values for these are all above 0.3
fisher.test(table(dat$centralized.link.to.ochsner.pmid, dat$array.design))
fisher.test(table(dat$centralized.link.to.ochsner.pmid, dat$pubmed.is.cancer))
fisher.test(table(dat$centralized.link.to.ochsner.pmid, dat$pubmed.is.cultured.cells))
fisher.test(table(dat$centralized.link.to.ochsner.pmid, dat$pubmed.is.humans))
fisher.test(table(dat$centralized.link.to.ochsner.pmid, dat$species))

# Verify this is not statistically significant
anova(lrm(centralized.link.to.ochsner.pmid ~ species, dat=dat))

# Verify this is not statistically significant
fisher.test(table(dat$centralized.link.to.ochsner.pmid, dat$discipline.journal))

# Verify this is not statistically significant
lrm(centralized.link.to.ochsner.pmid ~ pubmed.journal, dat=dat)
anova(lrm(centralized.link.to.ochsner.pmid ~ pubmed.journal, dat=dat))

results

#######
###<< INSERT FIGURE 2 HERE>>

start_results("\n")
# Verify p-value not significant
wilcox.test(dat$pubmed.number.times.cited.in.pmc ~ dat$centralized.link.to.ochsner.pmid)
wilcox.test(dat$number.samples ~ dat$centralized.link.to.ochsner.pmid)

append_to_results(strip_newlines("We did find some differences.  "))
wilcox.test(dat$impact.factor ~ dat$centralized.link.to.ochsner.pmid)


append_to_results(strip_newlines("Histograms of the distribution of 
citations (Figure 3), impact factors (Figure 4), and sample size (Figure 5) 
illustrate the different distributions for studies with database citation links 
to those without.  Searching centralized links in GEO and ArrayExpress can find "))
table_impact_factor = table(dat$centralized.link.to.ochsner.pmid, cut(dat$impact.factor, c(0, 10, 20, 100)))
prop_table_impact_factor = prop.table(table_impact_factor, 2)
append_to_results("%.1f%%", 100*prop_table_impact_factor[2, 3])
append_to_results(" of datasets associated with articles published in journals with impact factors greater than 20, but only ")
append_to_results("%.1f%%", 100*prop_table_impact_factor[2, 2])
append_to_results(" of those with impact factors from 10-20, and only ")
append_to_results("%.1f%%", 100*prop_table_impact_factor[2, 1])
append_to_results(" of the datasets for papers published with impact factors less than 10.")
results

start_results("\n")
append_to_results("Journal data sharing policy and journal scope were strongly associated with journal impact factor ")
# These are highly significant
wilcox.test(impact.factor ~ journal.policy.requires, data=dat)
wilcox.test(impact.factor ~ discipline.journal, data=dat)

# Some of these are borderline significant
wilcox.test(impact.factor ~ centralized.link.to.ochsner.pmid, data=dat)
wilcox.test(impact.factor ~ centralized.link.to.ochsner.pmid, data=dat, subset=journal.policy.requires==1)
wilcox.test(impact.factor ~ centralized.link.to.ochsner.pmid, data=dat, subset=journal.policy.requires==0)
wilcox.test(impact.factor ~ centralized.link.to.ochsner.pmid, data=dat, subset=discipline.journal==1)
wilcox.test(impact.factor ~ centralized.link.to.ochsner.pmid, data=dat, subset=discipline.journal==0)

append_to_results(", but stratifying by these features only slightly reduced the association between impact factor and recall. ")
results


start_results("\n")
append_to_results("The ability to retrieve online datasets through PubMed identifiers differed across the twenty journals in our sample, as illustrated in Figure 3, although this difference was not statistically significant in an ANOVA test. ")

# Verify this is not statistically significant
lrm(centralized.link.to.ochsner.pmid ~ pubmed.journal, dat=dat)
anova(lrm(centralized.link.to.ochsner.pmid ~ pubmed.journal, dat=dat))

results

start_results("\n")
append_to_results("Similarly, our automated search found ")
table_citations = table(dat$centralized.link.to.ochsner.pmid, cut(dat$pubmed.number.times.cited.in.pmc, c(-1, 0, 5, 10000)))
prop_table_citations = prop.table(table_citations, 2)
prop_table_citations
append_to_results("%.1f%%", 100*prop_table_citations[2, 3])
append_to_results(" of datasets whose associated articles had more than 5 citations, ")
append_to_results("%.1f%%", 100*prop_table_citations[2, 2])
append_to_results(" with 0-5 citations, and only ")
append_to_results("%.1f%%", 100*prop_table_citations[2, 1])
append_to_results(" with 0 citations.")

append_to_results("\nOur automated queries retrieved ")
table_sample_size = table(dat$centralized.link.to.ochsner.pmid, cut(dat$number.samples, c(0, 10, 100, 10000)))
prop_table_sample_size = prop.table(table_sample_size, 2)
prop_table_sample_size
append_to_results("%.1f%%", 100*prop_table_sample_size[2, 3])
append_to_results(" of datasets with more than 100 datapoints, ")
append_to_results("%.1f%%", 100*prop_table_sample_size[2, 2])
append_to_results(" with between 10 and 100, and  ")
append_to_results("%.1f%%", 100*prop_table_sample_size[2, 1]) 
append_to_results(" of datasets with fewer than 10 datapoints.")
results

start_results("\n")
append_to_results(strip_newlines("A journal policy requiring an accession number in published articles 
may increase the number of citation links in centralized database ("))
table_journal_policy = table(dat$centralized.link.to.ochsner.pmid, dat$journal.policy.requires)
prop_table_journal_policy = prop.table(table_journal_policy, 2)
append_to_results("%.1f%%", 100*prop_table_journal_policy[2, 2])
append_to_results(strip_newlines(" of datasets can be found through citation links for journals that 
require an accession number, vs. "))
append_to_results("%.1f%%", 100*prop_table_journal_policy[2, 1])
append_to_results(strip_newlines(" of datasets in other journals), but this 
difference was not statistically significant.  "))

# Verify this is not statistically significant
fisher.test(table(dat$centralized.link.to.ochsner.pmid, dat$journal.policy.requires))

results

start_results("\n")
append_to_results(strip_newlines("Multivariate regressions did not reveal any significant relationships between 
dataset retrievability and impact factor, number of citations, sample size, 
or journal policy."))

# Verify the p-value is not significant  with permutations of 
f = lrm(formula = centralized.link.to.ochsner.pmid 
    ~ rcs(log(impact.factor), 4)  
    + journal.policy.requires  
    #+ rcs(log(number.samples), 3) 
    #+ rcs(log(1 + pubmed.number.times.cited.in.pmc), 3)
    , dat=dat, x=T, y=T)
f
anova(f)

results
cat(all_results)


########### END OF RESULTS TEXT

##### FIGURE 1 #########


plot_on_same_graph = function() {
    par(new=TRUE)
}

plot_on_new_graph = function() {
    par(new=FALSE)
}

plot_ci_bar = function(x, bar_number=1, max_number_bars=1, bar_name="", col="black") {
    y_max = 1
    bar_position = bar_number*2 - 1
    x_spaces = max_number_bars*2
    no_factors = rep(TRUE, length(x))
    plot_on_same_graph()
    bargraph.CI(x.factor = no_factors, 
                response = x, 
                xlim=c(0, x_spaces), 
                ylim=c(0, y_max), 
                names.arg=c(bar_name), 
                col=col, 
                space=bar_position)
}

greys = grey(0:8 / 8)
light_grey = greys[8]
medium_grey = greys[4]
dark_grey = greys[2]

plot_on_new_graph()

plot_ci_bar(dat$all, 1, 4, "", light_grey)
plot_ci_bar(dat$all, 2, 4, "", light_grey)
plot_ci_bar(dat$all, 3, 4, "", light_grey)

plot_ci_bar(dat$in.arrayexpress.or.geo, 1, 4, "GEO and/or\nArrayExpress", medium_grey)
plot_ci_bar(dat$in.geo, 2, 4, "GEO", medium_grey)
plot_ci_bar(dat$in.arrayexpress, 3, 4, "ArrayExpress", medium_grey)

#plot_ci_bar(dat$ochsner.found.any.data, 1, 4, "", medium_grey)

plot_ci_bar(dat$centralized.arrayexpress.or.geo, 1, 4, "", dark_grey)
plot_ci_bar(dat$centralized.geo, 2, 4, "", dark_grey)
plot_ci_bar(dat$centralized.arrayexpress, 3, 4, "", dark_grey)

# Here I enlarge the figure window so the legend will fit without overlapping the graph

legend("right", c("\nDatasets not at this location\n", "\nDatasets found only\nby Ochsner search\n", "Datasets found by\nsearch of databases\nfor PubMed IDs"), 
    fill=c(light_grey, medium_grey, dark_grey), bty='n')
    
title(main=NULL, ylab="Proportion of article-related publicly-available datasets")
#title(main="Proportion of article-related publicly-available datasets\n retrievable by various search strategies")

         
##### end FIGURE 1 #########



#################         
##### Figure 2

hist_back_back_wrapper = function(histdata, splitdata, breaks=NULL, 
    title="Histogram", ylabels="", xlabels="", 
    log="n", sigfigs=0){
    plotdata = histdata
    if (log=="y") {
        plotdata = log(histdata)
    }
    sp = split(plotdata, splitdata)                    
    out = histbackback(sp, probability=TRUE, brks=breaks)
    print(out)
    frequency_maximum = max(c(out$left, out$right))
    frequency_plot_limit = frequency_maximum * 1.1
    print(frequency_plot_limit)
    out = histbackback(sp, probability=TRUE, 
                         main = title, brks=breaks, 
                         xlim=c(-frequency_plot_limit, frequency_plot_limit), 
                         xlab=xlabels,
                         axes=FALSE)
    print(out)
    if (log=="y") {
        breaklabels = round(exp(out$breaks), sigfigs)
    } else {
        breaklabels = out$breaks
    }
    title(ylab=ylabels, xlab=xlabels)
    xpoints = c(-frequency_plot_limit/2, 0, frequency_plot_limit/2)
    print(xpoints)
    axis(2, at=0:(length(breaklabels)-1), labels=breaklabels)
    axis(1, at=xpoints, labels=round(abs(xpoints)/sum(out$left), 2))
    
    #! just adding color
    barplot(-out$left, col=medium_grey , horiz=TRUE, space=0, add=TRUE, axes=FALSE) 
    barplot(out$right, col=light_grey, horiz=TRUE, space=0, add=TRUE, axes=FALSE) 
    return(out)
}

hist_above_wrapper = function(histdata, splitdata, breaks=NULL, 
    title=c("", ""), ylabels="", xlabels="", 
    use_log="n", sigfigs=0){
    plotdata = histdata
    if (use_log=="y") {
        plotdata = log(histdata)
    }
    sp = split(plotdata, splitdata)                    
    h = hist(plotdata, freq=FALSE, axes=FALSE) 
    print(h)

    par(mfrow=c(2, 1))
    hist_min = 0.9*min(plotdata)
    hist_max = 1.1*max(plotdata)
    h_1 = hist(sp$`1`, breaks=h$breaks, main=title[1], col=medium_grey, xlim=c(hist_min, hist_max), axes=FALSE, xlab="", ylab="") 
    
    if (use_log=="y") {
        breaklabels = round(exp(h$breaks), sigfigs)
    } else {
        breaklabels = round(h$breaks, sigfigs)
    }
    print(breaklabels)
    title(ylab=ylabels) # , xlab=xlabels)
    axis(1, at=h$breaks, labels=breaklabels)
    freq_label = c(0, round(max(h_1$counts)/sum(h_1$counts), 2))
    axis(2, at=c(0, max(h_1$counts)), labels=freq_label)
    
    h_0 = hist(sp$`0`, breaks=h$breaks, main=title[2], col=medium_grey, xlim=c(hist_min, hist_max), axes=FALSE, xlab="", ylab="") 
    title(ylab=ylabels, xlab=xlabels)
    axis(1, at=h$breaks, labels=breaklabels)
    freq_label = c(0, round(max(h_0$counts)/sum(h_0$counts), 2))
    axis(2, at=c(0, max(h_0$counts)), labels=freq_label)

}

quartz()
hist_above_wrapper(dat$impact.factor, 
    dat$centralized.link.to.ochsner.pmid, 
    seq(0.5, 4, by=0.25), 
    c(paste("Histogram of Impact Factor for datasets\nFound by query of databases for PubMed IDs (n=",count_true_items(dat$centralized.link.to.ochsner.pmid),")", sep=""), 
      paste("Not found by query of databases (n=",count_false_items(dat$centralized.link.to.ochsner.pmid),")", sep="")), 
    "Proportion of datasets",
    "Impact factor (log scale)", 
    use_log="y", sigfigs=0)



quartz()
hist_above_wrapper(dat$pubmed.number.times.cited.in.pmc+.5, 
    dat$centralized.link.to.ochsner.pmid, 
    NULL, #seq(-5, 5, by=.4), 
    c(paste("Histogram of Received Citations for datasets\nFound by query of databases for PubMed IDs (n=",count_true_items(dat$centralized.link.to.ochsner.pmid),")", sep=""), 
      paste("Not found by query of databases (n=",count_false_items(dat$centralized.link.to.ochsner.pmid),")", sep="")), 
    "Proportion of datasets",
    "Number of citations by articles in PubMed Central (log scale)",
    use_log="y", sigfigs=0)
    
quartz()
hist_above_wrapper(dat$number.samples[!is.na(dat$number.samples)], 
    dat$centralized.link.to.ochsner.pmid, 
    seq(0, 8, by=1), 
    c(paste("Histogram of Dataset Size for datasets\nFound by query of databases for PubMed IDs (n=",count_true_items(dat$centralized.link.to.ochsner.pmid[!is.na(dat$number.samples)]),")", sep=""), 
      paste("Not found by query of databases (n=",count_false_items(dat$centralized.link.to.ochsner.pmid[!is.na(dat$number.samples)]),")", sep="")), 
    "Proportion of datasets",
    "Number of samples in microarray study (log scale)",
    use_log="y", sigfigs=0)
    
    
#################         
##### Figure 3


library(sciplot)

quartz()
par(omi=c(1, 0, 0, 2))
bargraph.CI(x.factor = pubmed.journal, 
            response = in.centralized.db, 
            group = journal.policy.requires,
            data = dat, 
            space=c(-1, .5),
            ylim=c(0,1),
            cex=0.8,
            cex.names=1,
            xlab="", ylab="Proportion of online datasets", 
            names.arg = paste(levels(dat$pubmed.journal), " (", table(dat$pubmed.journal), ")", sep=""),
            main="Datasets published online",
            las=2, 
            err.width=0.05,
            col=grey(0.7), 
            err.col=grey(.5))

bargraph.CI(x.factor = pubmed.journal, 
            response = centralized.link.to.ochsner.pmid, 
            group = journal.policy.requires,
            data = dat, 
            space=c(-1, .5),
            ylim=c(0,1),
            cex=0.8,
            cex.names=1,
            xlab="", 
            ylab="", #"Proportion of datasets found by query", 
            names.arg = paste(levels(dat$pubmed.journal), " (", table(dat$pubmed.journal), ")", sep=""),
            #main="Fraction of all datasets found by query",
            las=2, 
            err.width=0.05,
            add=TRUE, 
            col=grey(0.3), 
            err.col=grey(0))

par(xpd=NA)
my.usr = par('usr')
right.x = my.usr[2] - 1.25
mid.y = (my.usr[3] + my.usr[4])/2
legend(right.x, mid.y, xjust=0, yjust=0, 
    c("\nDatasets deposited\nin GEO or AE\n", "\nDatasets with PubMed ids\nin GEO or AE entries\n"),
    fill=c(grey(0.7), grey(0.3)), bty='n', cex=0.9)

##################### end of Figures


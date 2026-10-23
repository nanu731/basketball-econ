#!/usr/bin/env Rscript
# Private scale-only benchmark: no acquisition, refitting, model selection or UI change.
source('R/core.R')

check_gaps <- function(gap) {
  if (!is.numeric(gap) || !is.null(dim(gap)) || any(!is.finite(gap))) stop('Whole cohort requires finite numeric gaps; no records may be dropped')
}

gap_scales <- function(gap) {
  check_gaps(gap)
  if (!length(gap)) return(c(rmse=NA_real_, sd=NA_real_, median_abs=NA_real_, mad_normal=NA_real_))
  magnitude <- max(abs(gap))
  scaled <- if(magnitude>0)gap/magnitude else gap
  c(rmse=magnitude*sqrt(mean(scaled^2)), sd=if(length(gap)>1L)magnitude*stats::sd(scaled)else NA_real_,
    median_abs=stats::median(abs(gap)),
    mad_normal=magnitude*(1.4826*stats::median(abs(scaled-stats::median(scaled)))))
}

gap_candidates <- function(gap) {
  scale <- gap_scales(gap)
  divide <- function(denominator, centered=FALSE) {
    if (!is.finite(denominator) || denominator<=0) return(rep(NA_real_,length(gap)))
    (gap-if(centered)mean(gap)else 0)/denominator
  }
  data.frame(gap=gap, rmse=divide(scale['rmse']), sd=divide(scale['sd']),
    centered_z=divide(scale['sd'],TRUE), median_abs=divide(scale['median_abs']),
    mad_normal=divide(scale['mad_normal']),
    percentile=if(length(gap))100*(rank(gap,ties.method='average')-.5)/length(gap)else numeric())
}

salary_groups <- function(salary, gap) {
  check_gaps(gap)
  if(length(salary)!=length(gap)||any(!is.finite(salary))||any(salary<0))stop('Invalid salary cohort')
  group <- pmin(5L, floor((rank(salary,ties.method='average')-1)*5/length(salary))+1L)
  result <- lapply(sort(unique(group)),function(g){
    at<-group==g;r<-gap[at]
    data.frame(group=g,n=sum(at),salary_min=min(salary[at]),salary_max=max(salary[at]),
      mean_gap=mean(r),rmse=sqrt(mean(r^2)),sd=if(length(r)>1L)stats::sd(r)else NA_real_,
      median_abs=median(abs(r)))
  })
  list(group=group, summary=dplyr::bind_rows(result))
}

cohort_benchmark <- function(gap,salary) {
  scales <- gap_scales(gap);scores<-gap_candidates(gap)
  loo<-vapply(seq_along(gap),function(i)gap_scales(gap[-i]),numeric(4))
  sensitivity<-vapply(seq_along(scales),function(j){
    if(!is.finite(scales[j])||scales[j]<=0||any(!is.finite(loo[j,])))return(NA_real_)
    max(abs(loo[j,]/scales[j]-1))
  },numeric(1))
  groups<-salary_groups(salary,gap);group_scale<-groups$summary$rmse[match(groups$group,groups$summary$group)]
  conditional<-ifelse(is.finite(group_scale)&group_scale>0,gap/group_scale,NA_real_)
  ratio<-function(x)if(all(is.finite(x))&&min(x)>0)max(x)/min(x)else NA_real_
  row<-data.frame(n=length(gap),mean_gap=mean(gap),median_gap=median(gap),
    rmse=scales['rmse'],sd=scales['sd'],median_abs=scales['median_abs'],mad_normal=scales['mad_normal'],
    loo_rmse=sensitivity[1],loo_sd=sensitivity[2],loo_median_abs=sensitivity[3],loo_mad_normal=sensitivity[4],
    median_abs_to_rmse=scales['median_abs']/scales['rmse'],mad_to_rmse=scales['mad_normal']/scales['rmse'],
    sd_to_rmse=scales['sd']/scales['rmse'],maximum_abs_rmse=max(abs(scores$rmse)),
    maximum_abs_median=max(abs(scores$median_abs)),maximum_abs_mad=max(abs(scores$mad_normal)),
    centered_zero_shift=if(is.finite(scales['sd'])&&scales['sd']>0)-mean(gap)/scales['sd']else NA_real_,
    group_rmse_ratio=ratio(groups$summary$rmse),group_variance_ratio=ratio(groups$summary$sd^2),
    salary_group_rank_correlation=if(all(is.finite(conditional)))stats::cor(gap,conditional,method='spearman')else NA_real_)
  list(summary=row,scores=cbind(scores,salary_group=groups$group,salary_group_scale=group_scale,
    salary_group_relative_gap=conditional),groups=groups$summary)
}

self_test <- function() {
  close<-function(x,y)stopifnot(isTRUE(all.equal(x,y,tolerance=1e-12,check.attributes=FALSE)))
  a<-gap_candidates(c(-2,0,2));close(a$rmse,c(-sqrt(1.5),0,sqrt(1.5)));close(a$sd,c(-1,0,1))
  close(a$percentile,c(100/6,50,500/6));close(gap_candidates(1)$percentile,50);close(gap_candidates(c(1,1,1))$percentile,rep(50,3))
  stopifnot(nrow(gap_candidates(numeric()))==0L,all(is.na(gap_candidates(c(0,0))$rmse)),
    all(is.na(gap_candidates(c(2,2))$sd)),all(is.na(gap_candidates(c(-1,0,0,0,1))$median_abs)))
  bad<-tryCatch({gap_candidates(c(1,NA_real_));FALSE},error=function(e)TRUE);stopifnot(bad)
  r<-c(-4,-1,0,2,7);x<-gap_candidates(r);y<-gap_candidates(r*1000)
  for(k in names(x)[-1]){
    close(x[[k]],y[[k]])
    close(x[[k]],gap_candidates(r*1e200)[[k]])
    close(x[[k]],gap_candidates(r*1e-200)[[k]])
    close(x[[k]][c(5,1,3,2,4)],gap_candidates(r[c(5,1,3,2,4)])[[k]])
  }
  for(k in c('rmse','sd','median_abs','mad_normal'))stopifnot(all(sign(x[[k]])==sign(r)),all(diff(x[[k]])>0))
  # Literal filter example: full-cohort gaps -2,0,2 have these stored scores.
  close(a$rmse[c(1,3)],c(-sqrt(1.5),sqrt(1.5)))
  stopifnot(!isTRUE(all.equal(a$rmse[c(1,3)],gap_candidates(c(-2,2))$rmse)))
  stopifnot(gap_candidates(c(1,2,9))$centered_z[1]<0)
  g<-salary_groups(c(1,1,1,2,3),c(-1,0,1,2,3));stopifnot(length(unique(g$group[1:3]))==1L)
  z<-cohort_benchmark(c(-2,0,2),c(1,2,3));close(z$summary$loo_rmse,sqrt(1.5)-1)
  cat('PASS scale/rank worked example; zero/ties/missing; sign/order; unit/filter invariance; centering distinction; salary ties; leave-one scale\n')
}

args<-commandArgs(trailingOnly=TRUE)
if(any(!grepl('^(--self-test|--index=.+)$',args)))stop('Use --self-test or --index=<current local index>')
self_test()
if('--self-test'%in%args)quit(status=0)
index_arg<-args[startsWith(args,'--index=')]
if(length(index_arg)>1L)stop('One index only')
index_path<-if(length(index_arg))substring(index_arg,9L)else'outputs/private/browser-v1/moneyball-v1-31960f8380b3ee1b1d5c/index.json'
index<-jsonlite::fromJSON(index_path,simplifyVector=FALSE)
if(!identical(index$schemaVersion,'moneyball.index.v1'))stop('Invalid index schema')
if(!identical(index$dataVersion,'moneyball-v1-31960f8380b3ee1b1d5c')||length(index$views)!=45L)stop('Expected the pinned current 45-view export')
cohorts<-groups<-players<-list();provenance<-list(index=list(path=index_path,sha256=unname(tools::sha256sum(index_path))))
for(entry in index$views){
  id<-entry$id
  if(!identical(entry$url,paste0('/data/moneyball/v1/views/',id,'.json'))||!grepl('^[0-9]{4}-(1|3|5)$',id))stop('Invalid local view reference')
  path<-file.path(dirname(index_path),'views',paste0(id,'.json'))
  bundle<-jsonlite::fromJSON(path);view<-bundle$views
  if(bundle$schemaVersion!='moneyball.v1'||bundle$dataVersion!=index$dataVersion||nrow(view)!=1L||view$id!=id)stop('View identity/version mismatch')
  p<-view$players[[1]]
  if(anyDuplicated(p$id)||nrow(p)!=view$coverage$matched)stop('Invalid cohort identities/count')
  provenance[[id]]<-list(path=path,sha256=unname(tools::sha256sum(path)))
  for(metric in c('lebron','war')){
    gap<-p$residual[[metric]]
    if(any(abs(gap-(p[[metric]]-p$fitted[[metric]]))>1e-6*pmax(1,abs(gap))))stop('Gap differs from visible reference')
    result<-cohort_benchmark(gap,p$salaryCapShare)
    for(candidate in c('rmse','sd','median_abs','mad_normal')){
      z<-result$scores[[candidate]]
      stopifnot(all(is.finite(z)),all(sign(z)==sign(gap)),all(diff(z[order(gap)])>=0))
    }
    if(any(abs(result$scores$percentile-p$valuePercentile[[metric]])>1e-8))stop('Exported percentiles disagree with canonical gaps')
    r<-result$summary;r$id<-id;r$metric<-metric;r$end_season<-entry$endSeason;r$window<-entry$window
    cohorts[[length(cohorts)+1L]]<-r
    r<-result$groups;r$id<-id;r$metric<-metric;groups[[length(groups)+1L]]<-r
    r<-result$scores;r$id<-id;r$metric<-metric;r$nba_id<-p$id;r$name<-p$name
    players[[length(players)+1L]]<-r
  }
}
cohorts<-dplyr::bind_rows(cohorts);groups<-dplyr::bind_rows(groups);players<-dplyr::bind_rows(players)
stopifnot(nrow(cohorts)==90L,all(cohorts$n>0))
for(metric in c('lebron','war'))for(window in c(1L,3L,5L)){
 at<-cohorts$metric==metric&cohorts$window==window
 stopifnot(all(is.finite(cohorts$rmse[at])))
}
set.seed(20261027L)
normal<-rnorm(1000);salary<-seq(.01,.5,length.out=1000)
synthetic<-list(normal=normal,skew=rexp(1000)-1,heavy_tail=rt(1000,df=3),
  contaminated=c(normal[-1000],50),heteroscedastic=rnorm(1000)*(.5+4*salary))
synthetic_rows<-lapply(names(synthetic),function(name){r<-cohort_benchmark(synthetic[[name]],salary)$summary;r$scenario<-name;r$origin<-'synthetic_only';r})
synthetic_rows<-dplyr::bind_rows(synthetic_rows)
comparison<-data.frame(candidate=c('rmse','sd','median_abs','mad_normal','percentile'),
  common_score_change_after_contamination=vapply(c('rmse','sd','median_abs','mad_normal','percentile'),function(k){
    original<-gap_candidates(normal)[[k]][-1000];changed<-gap_candidates(synthetic$contaminated)[[k]][-1000]
    max(abs(changed-original))
  },numeric(1)))
prefix<-'outputs/private/gap-score-benchmark-31960f8380b3ee1b1d5c'
utils::write.csv(cohorts,paste0(prefix,'-cohorts.csv'),row.names=FALSE,na='')
utils::write.csv(groups,paste0(prefix,'-salary-groups.csv'),row.names=FALSE,na='')
utils::write.csv(players,paste0(prefix,'-players.csv'),row.names=FALSE,na='')
utils::write.csv(synthetic_rows,paste0(prefix,'-synthetic.csv'),row.names=FALSE,na='')
utils::write.csv(comparison,paste0(prefix,'-contamination.csv'),row.names=FALSE,na='')
provenance$data_version<-index$dataVersion;provenance$seed<-20261027L;provenance$R<-R.version.string
provenance$untested<-c('Classical leverage residual: no individual cached h in current export/flat diagnostics; large analysis object not loaded',
 'Nested CV, conditional quantiles, conformal and new conditional variance fits: different claims, no demonstrated need for descriptive scaling')
writeLines(jsonlite::toJSON(provenance,auto_unbox=TRUE,pretty=TRUE),paste0(prefix,'-provenance.json'))
cat('PASS',nrow(cohorts),'observed cohorts;',nrow(players),'player/metric entries; private output prefix:',prefix,'\n')

# Standalone offline diagnostics tests. All observations here are SYNTHETIC.
for (file in c('core','caps','teams','windows','models','bootstrap','analysis','sensitivity')) {
  source(paste0('R/',file,'.R'))
}
source('tests/fixtures/analysis-fixtures.R')
sensitivity_tests <- 0L
check_sensitivity <- function(label, expression) {
  force(expression)
  sensitivity_tests <<- sensitivity_tests + 1L
  cat(sprintf('PASS %02d %s\n',sensitivity_tests,label))
}
expect_sensitivity_error <- function(expression, pattern) {
  e <- tryCatch({force(expression);NULL},error=identity)
  stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e)))
}

check_sensitivity('minutes weighting changes only the rate of the fixed complete cohort', {
  dataset <- synthetic_window_dataset()
  dataset$players$provider_possessions[1:2] <- c(100,900)
  main <- make_player_window(dataset,'2025-26',2L,prespecified_exposure_rules()$main)
  minutes <- make_minutes_weighted_window(main)
  stopifnot(identical(main$points$nba_id,'1'),abs(main$points$lebron-5.6)<1e-12,
    abs(minutes$points$lebron-5)<1e-12,minutes$points$war==6,
    abs(minutes$points$salary_cap_share-.15)<1e-12,
    identical(main$points[setdiff(names(main$points),'lebron')],minutes$points[setdiff(names(minutes$points),'lebron')]),
    identical(main$active_player_seasons,minutes$active_player_seasons))
})

# The annual outcome is 1000*x plus a season offset: 0, 10, then 20.
# Holding out the middle season predicts it exactly; endpoint-season bias is +/-15.
synthetic_sensitivity_panel <- function(seasons=c('2023-24','2024-25','2025-26'),n=30L) {
  rows <- dplyr::bind_rows(lapply(seq_along(seasons),function(k) {
    x <- seq(.01,.30,length.out=n)
    tibble::tibble(season=seasons[k],nba_id=seq_len(n),player_name=paste0('TEST_FIXTURE_',seq_len(n)),
      played=TRUE,minutes=1000,games=40L,provider_possessions=2000,
      salary_cap_share=x,salary_reported_usd=x*154647000,
      lebron=(1000*x+10*(k-1))/100,war=1000*x+10*(k-1),team_count=1L)
  }))
  list(players=rows,teams=tibble::tibble(season=rows$season,nba_id=rows$nba_id,team='SAS'),
    data_origin='test_fixture',source_provenance=list(),publication_status='unreviewed_do_not_publish')
}

check_sensitivity('held-out seasons score native annual targets with grouped training-only selection', {
  panel <- synthetic_sensitivity_panel()
  result <- out_of_season_robustness(panel,config=methods_config())
  stopifnot(result$available,identical(result$sentinel_seasons,c('2023-24','2024-25','2025-26')))
  middle <- result$results[['2024-25:war']]
  stopifnot(middle$available,middle$training_observations==60L,middle$training_players==30L,
    middle$mse<1e-14,abs(result$results[['2023-24:war']]$mse-225)<1e-7,
    abs(result$results[['2025-26:war']]$mse-225)<1e-7,
    abs(middle$calibration$slope-1)<1e-8,abs(middle$calibration$intercept)<1e-8,
    !'2024-25'%in%middle$training_seasons,grepl('not a future forecast',middle$interpretation),
    identical(middle$partitions[[1]]$fold_map,result$results[['2024-25:lebron']]$partitions[[1]]$fold_map))
  for (partition in middle$partitions) {
    by_player <- split(partition$predictions$fold,partition$predictions$nba_id)
    stopifnot(all(vapply(by_player,function(x)length(unique(x))==1L,logical(1))),
      all(partition$scores$scored_observations[partition$scores$available]==60L))
  }
  changed <- panel
  changed$players$war[changed$players$season=='2024-25'] <- changed$players$war[changed$players$season=='2024-25']+900
  changed_result <- out_of_season_robustness(changed,config=methods_config())$results[['2024-25:war']]
  stopifnot(identical(middle$predictions$predicted,changed_result$predictions$predicted),
    identical(middle$partitions,changed_result$partitions))
})

check_sensitivity('curve comparisons use common supported salaries and the same reference dollars', {
  a <- synthetic_relationship_points('linear',160L)
  a$salary_cap_share <- seq(.10,.30,length.out=160L)
  a$war <- 1+2*a$salary_cap_share; a$lebron <- a$war
  b <- a; b$salary_cap_share <- seq(.15,.25,length.out=160L)
  b$war <- 1+3*b$salary_cap_share; b$lebron <- b$war
  make_analysis <- function(points) list(available=TRUE,window=list(coverage=list()),
    models=list(war=relationship_curve(fit_salary_relationship(points,'war'),100000000),
      lebron=relationship_curve(fit_salary_relationship(points,'lebron'),100000000)))
  primary <- make_analysis(a); alternative <- make_analysis(b)
  comparison <- compare_relationship_sensitivity(primary,alternative)$comparisons$war
  stopifnot(!is.null(comparison),comparison$common_supported_grid_points>0,
    all(comparison$grid$x>=.15 & comparison$grid$x<=.25))
  at <- which.min(abs(comparison$grid$x-.20))
  stopifnot(abs(comparison$grid$curve_difference[at]-.20)<.002,
    all(abs(comparison$grid$contrast_difference[comparison$grid$common_supported]-.01)<1e-10))
  alternative$models$war$reference_cap_usd <- 154647000
  expect_sensitivity_error(compare_relationship_sensitivity(primary,alternative),'same reference cap')
  unavailable <- compare_relationship_sensitivity(list(available=FALSE,reason='no_main'),alternative)
  stopifnot(!unavailable$available)
})

check_sensitivity('support variants change one setting and reuse identical fitted contrasts', {
  points <- synthetic_relationship_points('linear',160L)
  points$salary_cap_share <- seq(.10,.30,length.out=160L);points$war<-2*points$salary_cap_share
  curve <- relationship_curve(fit_salary_relationship(points,'war'),100000000)
  original <- curve
  support <- support_sensitivity(curve)
  stopifnot(nrow(support)==5L,identical(curve,original),all(support$common_contrast_max_abs_difference[is.finite(support$common_contrast_max_abs_difference)]==0),
    is.na(support$common_contrast_max_abs_difference[support$setting=='radius_01']),
    support$supported_points[support$setting=='radius_01']==0L,
    all((support$radius!=.02)+(support$minimum_players!=20)<=1L))
  for (details in support$details) {
    stopifnot(all(is.na(details$estimate[!details$supported])),
      all(abs(details$estimate[details$supported]-.02)<1e-10))
  }
})

check_sensitivity('sentinels require contiguous shared years and use the lower middle held-out season', {
  panel <- synthetic_sensitivity_panel(sprintf('%d-%02d',2020:2025,(2021:2026)%%100L))
  plan <- sensitivity_sentinel_windows(panel)
  stopifnot(identical(plan$id,c('2026-1','2026-3','2026-5','2021-1')),all(plan$available))
  transport <- out_of_season_robustness(panel)
  stopifnot(identical(transport$sentinel_seasons,c('2020-21','2022-23','2025-26')))
  gap <- panel;gap$players<-gap$players[gap$players$season!='2024-25',]
  plan_gap <- sensitivity_sentinel_windows(gap)
  stopifnot(!plan_gap$available[plan_gap$id=='2026-3'],!plan_gap$available[plan_gap$id=='2026-5'])
})

check_sensitivity('orchestration fits every prespecified population and retains full bootstrap evidence', {
  panel <- synthetic_sensitivity_panel('2025-26',40L)
  panel$players$minutes <- c(rep(100,10),rep(300,10),rep(1000,20))
  panel$players$games <- c(rep(2L,10),rep(6L,10),rep(40L,20))
  result <- run_prespecified_sensitivity(panel,cache_dir=tempfile('synthetic-sensitivity-bootstrap-'))
  view <- result$windows[['2026-1']]
  stopifnot(result$publication_status=='private_diagnostics_only',nrow(result$coverage_all_windows)==3L,
    view$analyses$main$available,view$analyses$broad$available,!view$analyses$strict$available,
    nrow(view$analyses$main$window$points)==30L,nrow(view$analyses$broad$window$points)==40L,
    view$evidence$main$exposure$included_minutes==23000,
    view$evidence$main$coverage$omitted_known_signed_war>0,
    view$analyses$main$models$war$bootstrap$planned_draws>=499L,
    view$analyses$broad$models$war$bootstrap$planned_draws>=499L,
    !result$out_of_season$available)
  for (analysis in view$analyses[c('main','broad')]) {
    for (outcome in c('lebron','war')) for (partition in analysis$models[[outcome]]$fit$partitions) {
      stopifnot(identical(partition$fold_map,view$fold_maps[[if(identical(analysis,view$analyses$main)) 'main' else 'broad']][[as.character(partition$seed)]]))
    }
  }
  pilot <- view$analyses$main;pilot$pilot_only<-TRUE
  expect_sensitivity_error(run_window_sensitivity(panel,pilot),'timing pilot')
  filtered <- view$analyses$main;filtered$window$points<-filtered$window$points[-1,]
  expect_sensitivity_error(run_window_sensitivity(panel,filtered),'fixed main population')
  empty <- panel;empty$players$salary_cap_share[]<-NA_real_
  no_fit <- run_prespecified_sensitivity(empty,analysis_runner=function(...)stop('Forbidden fit'))
  stopifnot(!no_fit$available,no_fit$reason=='no_shared_completed_seasons')
})

check_sensitivity('minutes sensitivity preserves salary WAR IDs and every planned player fold', {
  panel <- synthetic_sensitivity_panel()
  panel$players$provider_possessions[panel$players$season=='2025-26'] <- 8000
  window <- make_player_window(panel,'2025-26',3L,prespecified_exposure_rules()$main)
  cache <- tempfile('synthetic-rate-sensitivity-')
  main <- analyze_player_window(window,sentinel=TRUE,cache_dir=cache,influence=FALSE)
  result <- run_window_sensitivity(panel,main,cache_dir=cache)
  variant <- result$analyses$minutes_weighted
  stopifnot(result$alternatives$minutes_weighted$fixed_population,
    abs(result$alternatives$minutes_weighted$point_outcome_difference$rms-.05)<1e-12,
    identical(result$fold_maps$main,result$fold_maps$minutes_weighted),
    identical(main$models$war,variant$models$war),
    identical(main$window$points$nba_id,variant$window$points$nba_id),
    identical(main$window$points$salary_cap_share,variant$window$points$salary_cap_share),
    identical(main$window$points$war,variant$window$points$war))
  tampered <- main
  tampered$models$lebron$fit$partitions[[1]]$fold_map$fold[1] <- 99L
  expect_sensitivity_error(run_window_sensitivity(panel,tampered),'fold map')
  wrong_target <- main;wrong_target$models$war$fit$data$y <- wrong_target$models$war$fit$data$y+1
  expect_sensitivity_error(run_window_sensitivity(panel,wrong_target),'fixed window inputs')
  no_bootstrap <- main;no_bootstrap$models$war$bootstrap<-NULL
  expect_sensitivity_error(run_window_sensitivity(panel,no_bootstrap),'499 planned')
  broken <- window;broken$active_player_seasons$minutes[]<-0
  expect_sensitivity_error(make_minutes_weighted_window(broken),'minutes weights')
})

check_sensitivity('transport prerequisites and identity errors cannot produce diagnostic fits', {
  small <- out_of_season_robustness(synthetic_sensitivity_panel(n=10L))
  stopifnot(small$available,all(vapply(small$results,function(x)!x$available && x$reason=='fewer_than_25_training_players',logical(1))))
  short <- out_of_season_robustness(synthetic_sensitivity_panel(c('2024-25','2025-26')))
  stopifnot(!short$available,short$reason=='fewer_than_three_shared_completed_seasons')
  duplicate <- synthetic_sensitivity_panel();duplicate$players<-dplyr::bind_rows(duplicate$players,duplicate$players[1,])
  expect_sensitivity_error(out_of_season_robustness(duplicate),'distinct identified player-seasons')
})

check_sensitivity('annual exposure cannot replace prescribed shared-season sentinels', {
  panel <- synthetic_sensitivity_panel(c('2022-23','2023-24','2024-25','2025-26'))
  panel$players$minutes[panel$players$season=='2022-23'] <- 100
  result <- out_of_season_robustness(panel)
  stopifnot(result$available,
    identical(result$sentinel_seasons,c('2022-23','2023-24','2025-26')),
    identical(result$coverage$included_player_seasons,c(0L,30L,30L,30L)))
  for (outcome in c('lebron','war')) {
    earliest <- result$results[[paste('2022-23',outcome,sep=':')]]
    stopifnot(!earliest$available,earliest$reason=='no_eligible_held_out_player_seasons',
      earliest$test_players==0L,earliest$coverage$source_player_seasons==30L,
      earliest$coverage$shared_player_seasons==30L,
      earliest$coverage$included_player_seasons==0L,
      earliest$coverage$excluded_player_seasons==30L,
      is.null(earliest$predictions),is.null(earliest$partitions),
      result$results[[paste('2023-24',outcome,sep=':')]]$available,
      result$results[[paste('2025-26',outcome,sep=':')]]$available)
  }
  # A source season without any comparable salary is not a shared season.
  missing <- synthetic_sensitivity_panel('2021-22')$players
  missing$salary_cap_share[] <- NA_real_
  panel$players <- dplyr::bind_rows(missing,panel$players)
  stopifnot(identical(out_of_season_robustness(panel)$sentinel_seasons,result$sentinel_seasons))
})

cat(sprintf('\n%d standalone synthetic sensitivity tests passed.\n',sensitivity_tests))

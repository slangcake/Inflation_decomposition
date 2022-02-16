#' ---
#' title: Inflation model comparisons
#' ---
#+ fig.width=6, fig.height=6, dpi=50

#' Compares results from different configurations of the inflation components models

for(i in 1:length(gather)){
  names(gather)[i] %>% print()
  gather[[i]]$weights %>% print(.,digits=3)
}

for(i in 1:length(gather)){
  names(gather)[i] %>% print()
  gather[[i]]$catgrs %>% print()
}

for(i in 1:length(gather)){
  names(gather)[i] %>% print()
  j <- gather[[i]]$headline
  j %>% print()
  j <- gather[[i]]$lmkt
  j %>% print()
  j <- gather[[i]]$xr
  j %>% print()
}



if(!'pacman' %in% .packages()){
  require("pacman")
}
if(!'rblm' %in% .packages()){
  library("rblm")
}



##### ISSUE : Pb loading utils when using the modified code
###### I put them all in the other notebook

wplot <- function(Wm) {
  dd = melt(Wm,c('l','k'))
  ggplot(dd,aes(x=factor(l),color=factor(k),group=factor(k),y=value)) + geom_line() + theme_bw()
}


model.connectiveness <- function (model, all = FALSE) 
{
  EV = rep(0, model$nk)
  pk1 = rdim(model$pk1, model$nf, model$nf, model$nk)
  dd_post = data.table(melt(pk1, c("j1", "j2", "k")))
  pp = model$NNm/sum(model$NNm)
  dd_post <- dd_post[, `:=`(pr_j1j2, pp[j1, j2]), list(j1, 
                                                       j2)]
  dd_post <- dd_post[, `:=`(pr_j1j2k, pr_j1j2 * value)]
  for (kk in 1:model$nk) {
    A1 = acast(dd_post[k == kk, list(pr = pr_j1j2k/sum(pr_j1j2k), 
                                     j2, j1)], j1 ~ j2, value.var = "pr")
    A2 = acast(dd_post[k == kk, list(pr = pr_j1j2k/sum(pr_j1j2k), 
                                     j2, j1)], j2 ~ j1, value.var = "pr")
    A = 0.5 * A1 + 0.5 * A2
    D = diag(rowSums(A)^(-0.5))
    L = diag(model$nf) - D %*% A %*% D
    EV[kk] = sort(eigen(L)$values)[2]
  }
  if (all == TRUE) 
    return(EV)
  return(min(abs(EV)))
}

# Include silhouette score to find best cluster number --------------------



grouping.classify_wsil <- function (measures, ksupp = ceiling((1:(60)^(1/1.3))^1.3), nstart = 1000, 
                               iter.max = 200, stop = FALSE, verbose = 1, cval = 1) 
{
  N = measures$N
  flog.info("clustering T=%f, Nw=%i , measure=%s", measures$tsize, 
            measures$Nw, measures$measure)
  ksupp = setdiff(ksupp, 1)
  rk = data.frame()
  rk_all = list()
  silhouette_all = list()
  ##### Here I keep only the long process
  for (k in ksupp) {
      if (k > nrow(measures$M)) 
        next
      if (k < 2) 
        next
      kres = kmeansW(measures$M, centers = k, weight = measures$W, 
                     nstart = nstart, iter.max = iter.max)
      silhouette_all[[k]] <- silhouette(kres$cluster, dist(measures$M))
      rk = rbind(rk, data.frame(k = k, Q = sum(kres$withinss/N), silhouette = mean(silhouette_all[[k]][,3])))
      flog.info("k=%i WSS=%f nstart=%i nfrims=%i", k, 
                sum(kres$withinss), nstart, N)
      rk_all[[k]] <- kres$cluster
      if ((stop == TRUE) & (sum(kres$withinss/N) < cval * 
                            measures$discrepency)) 
        break
    }
  
  rk$cval = cval
  rk$tsize = measures$tsize
  rk$vh = measures$discrepency
  rk$Nw = ncol(measures$M)
  rk$measure = measures$measure
  if (any(rk$Q < cval * measures$discrepency)) {
    best_k = min(subset(rk, Q < cval * measures$discrepency)$k)
  }
  else {
    change <- c()
    for(i in 2:length(rk$k)){
      change <- c(change, (rk$silhouette[[i-1]]-rk$silhouette[[i]]))
    }
    if(any(change < 0)){
      best_k = which(change <0)[[1]]+1
    }
    else if(which(rk$silhouette == min(rk$silhouette)) != length(rk$silhouette)){
      best_rk = rk$k[[which(rk$silhouette == min(rk$silhouette))]]
    }
    else{
    best_k = max(ksupp)
    }
  }
  print("grouping done")
  return(list(summary = rk, all = rk_all, best_k = best_k, 
              best_cluster = rk_all[[best_k]], discrepency = measures$discrepency, 
              measures = measures, silhouette = silhouette_all ))
}


# return type distribution for workers ------------------------------------
#m2.get.pk_unc <- function (model, supersample=1) 
#{
#  dpk1 = m2.get.pk1(model)
#  pk_m = acast(dpk1[, sum(pr_j1j2k), list(j1, k)], j1 ~ k, 
#               value.var = "V1")
#  NNs = model$NNs * round(1/supersample)
#  NNm = model$NNm
#  share_s = sum(NNs)/(sum(NNm) + sum(NNs))
#  pk_unc = share_s * rdim(model$pk0, model$nf, 
#                          model$nk) + (1 - share_s) * pk_m
#  pk_unc
#}



# 1. Modify function for movers and stayers to retrieve p_i(K=k) f --------



# # 1.1 Movers ------------------------------------------------------------


m2.mixt.movers_wdist <- function (jdatae, model, ctrl) 
{
  start.time <- Sys.time()
  tic <- tic.new()
  dprior = ctrl$dprior
  model0 = ctrl$model0
  taum = ctrl$tau
  nk = model$nk
  nf = model$nf
  A1 = model$A1
  S1 = model$S1
  A2 = model$A2
  S2 = model$S2
  pk1 = model$pk1
  Y1m = jdatae$y1
  Y2m = jdatae$y2
  J1m = jdatae$j1
  J2m = jdatae$j2
  JJm = J1m + nf * (J2m - 1)
  Nm = jdatae[, .N]
  CS1 = cons.pad(cons.get(ctrl$cstr_type[1], ctrl$cstr_val[1], 
                          nk, nf), nk * nf * 0, nk * nf * 1)
  CS2 = cons.pad(cons.get(ctrl$cstr_type[2], ctrl$cstr_val[2], 
                          nk, nf), nk * nf * 1, 0)
  CS = cons.bind(CS1, CS2)
  if (ctrl$fixb == T) {
    CS2 = cons.fixb(nk, nf, 2)
    CS = cons.bind(CS2, CS)
  }
  if (ctrl$model_var == T) {
    CSw = cons.none(nk, nf * 2)
  }
  else {
    CS1 = cons.pad(cons.mono_k(nk, nf), nk * nf * 0, nk * 
                     nf * 3)
    CS2 = cons.pad(cons.mono_k(nk, nf), nk * nf * 1, nk * 
                     nf * 2)
    CSw = cons.bind(CS1, CS2)
    CSw$meq = length(CSw$H)
  }
  Dkj1f = diag(nf) %x% rep(1, nf) %x% diag(nk)
  Dkj2f = rep(1, nf) %x% diag(nf) %x% diag(nk)
  XX = rbind(cbind(Dkj1f, 0 * Dkj2f), cbind(0 * Dkj1f, Dkj2f))
  lik_old = -Inf
  lik = -Inf
  lik_best = -Inf
  liks = 0
  likm = 0
  lpt1 = array(0, c(Nm, nk))
  lpt2 = array(0, c(Nm, nk))
  lp = array(0, c(Nm, nk))
  tic("prep")
  stop = F
  for (step in 1:ctrl$maxiter) {
    model1 = list(nk = nk, nf = nk, A1 = A1, A2 = A2, S1 = S1, 
                  S2 = S2, pk1 = pk1, dprior = dprior)
    if (is.na(taum[1]) | (step > 1)) {
      for (l1 in 1:nf) for (l2 in 1:nf) {
        I = which((J1m == l1) & (J2m == l2))
        ll = l1 + nf * (l2 - 1)
        if (length(I) == 0) 
          next
        for (k in 1:nk) {
          lpt1[I, k] = lognormpdf(Y1m[I], A1[l1, k], 
                                  S1[l1, k])
          lpt2[I, k] = lognormpdf(Y2m[I], A2[l2, k], 
                                  S2[l2, k])
          lp[I, k] = log(pk1[ll, k]) + lpt1[I, k] + 
            lpt2[I, k]
        }
      }
      liks = sum(logRowSumExp(lp))
      taum = exp(lp - spread(logRowSumExp(lp), 2, nk))
      lik_prior = (dprior - 1) * sum(log(pk1))
      lik = liks + lik_prior
    }
    else {
      cat("skiping first max step, using supplied posterior probabilities\n")
    }
    tic("estep")
    if (stop) 
      break
    rwm = c(t(taum + ctrl$posterior_reg))
    if (ctrl$fixm == F) {
      DYY = array(0, c(nk, nf, nf, 2))
      WWT = array(1e-07, c(nk, nf, nf, 2))
      for (l1 in 1:nf) for (l2 in 1:nf) {
        I = which((J1m == l1) & (J2m == l2))
        if (length(I) == 0) 
          next
        for (k in 1:nk) {
          ww = sum(taum[I, k] + ctrl$posterior_reg)
          DYY[k, l2, l1, 1] = sum(Y1m[I] * (taum[I, 
                                                 k] + ctrl$posterior_reg))/ww
          DYY[k, l2, l1, 2] = sum(Y2m[I] * (taum[I, 
                                                 k] + ctrl$posterior_reg))/ww
          WWT[k, l2, l1, 1] = ww/pmax(ctrl$sd_floor, 
                                      S1[l1, k]^2)
          WWT[k, l2, l1, 2] = ww/pmax(ctrl$sd_floor, 
                                      S2[l2, k]^2)
        }
      }
      WWT = WWT/sum(WWT)
      fit = slm.wfitc(XX, as.numeric(DYY), as.numeric(WWT), 
                      CS)$solution
      is = 1
      A1 = t(rdim(fit[is:(is + nk * nf - 1)], c(nk, nf)))
      is = is + nk * nf
      A2 = t(rdim(fit[is:(is + nk * nf - 1)], c(nk, nf)))
      is = is + nk * nf
      DYY_bar = array(0, c(nk, nf, nf, 2))
      DYY_bar[] = XX %*% fit
      DYYV = array(0, c(nk, nf, nf, 2))
      for (l1 in 1:nf) for (l2 in 1:nf) {
        I = which((J1m == l1) & (J2m == l2))
        if (length(I) == 0) 
          next
        for (k in 1:nk) {
          ww = sum(taum[I, k] + ctrl$posterior_reg)
          DYYV[k, l2, l1, 1] = sum((Y1m[I] - DYY_bar[k, 
                                                     l2, l1, 1])^2 * (taum[I, k] + ctrl$posterior_reg))/ww
          DYYV[k, l2, l1, 2] = sum((Y2m[I] - DYY_bar[k, 
                                                     l2, l1, 2])^2 * (taum[I, k] + ctrl$posterior_reg))/ww
        }
      }
      fitv = slm.wfitc(XX, as.numeric(DYYV), as.numeric(WWT), 
                       CSw)$solution
      is = 1
      S1 = sqrt(t(rdim(fitv[is:(is + nk * nf - 1)], nk, 
                       nf)))
      is = is + nk * nf
      S2 = sqrt(t(rdim(fitv[is:(is + nk * nf - 1)], nk, 
                       nf)))
      is = is + nk * nf
      S1[S1 < ctrl$sd_floor] = ctrl$sd_floor
      S2[S2 < ctrl$sd_floor] = ctrl$sd_floor
    }
    tic("mstep-ols")
    for (l1 in 1:nf) for (l2 in 1:nf) {
      jj = l1 + nf * (l2 - 1)
      I = which(JJm == jj)
      if (length(I) > 1) {
        pk1[jj, ] = colSums(taum[I, ])
      }
      else if (length(I) == 0) {
        pk1[jj, ] = 1/nk
      }
      else {
        pk1[jj, ] = taum[I, ]
      }
      pk1[jj, ] = (pk1[jj, ] + dprior - 1)/(sum(pk1[jj, 
      ] + dprior - 1))
    }
    tic("mstep-pks")
    if ((!any(is.na(model0))) & ((step%%ctrl$nplot) == (ctrl$nplot - 
                                                        1))) {
      I1 = order(colSums(A1))
      I2 = order(colSums(model0$A1))
      rr = addmom(A2[, I1], model0$A2[, I2], "A2")
      rr = addmom(A1[, I1], model0$A1[, I2], "A1", rr)
      rr = addmom(S2[, I1], model0$S2[, I2], "S2", rr, 
                  type = "var")
      rr = addmom(S1[, I1], model0$S1[, I2], "S1", rr, 
                  type = "var")
      rr = addmom(pk1, model0$pk1, "pk1", rr, type = "pr")
      print(ggplot(rr, aes(x = val2, y = val1, color = type)) + 
              geom_point() + facet_wrap(~name, scale = "free") + 
              theme_bw() + geom_abline(linetype = 2))
    }
    else {
      if ((step%%ctrl$nplot) == (ctrl$nplot - 1)) {
        wplot(A1)
      }
    }
    dlik = (lik - lik_old)/abs(lik_old)
    lik_old = lik
    lik_best = pmax(lik_best, lik)
    if ((step%%ctrl$ncat) == 0) 
      flog.info("[%3i][%s] lik=%4.4f dlik=%4.4e liks=%4.4e likm=%4.4e", 
                step, ctrl$textapp, lik, dlik, liks, likm)
    if (step > 10) 
      if (abs(dlik) < ctrl$tol) 
        break
    tic("loop-wrap")
  }
  flog.info("[%3i][%s][final] lik=%4.4f dlik=%4.4e liks=%4.4e likm=%4.4e", 
            step, ctrl$textapp, lik, dlik, liks, likm)
  model$A1 = A1
  model$S1 = S1
  model$A2 = A2
  model$S2 = S2
  model$pk1 = pk1
  model$NNm = acast(jdatae[, .N, list(j1, j2)], j1 ~ j2, fill = 0, 
                    value.var = "N")
  model$likm = lik
  model$dist = as.data.table(cbind(jdatae$wid, taum))
  colnames(model$dist) <- c('wid', paste0('k_', 1:nk) )
  model$dist <- model$dist %>%
    .[, (paste0('k_', 1:nk)):=lapply(.SD, as.numeric), .SDcols =paste0('k_', 1:nk) ]%>%
    .[, max_k := do.call(pmax, .SD), .SD = paste0('k_', 1:nk) ] %>%
    .[, max_k := names(.SD)[max.col(.SD)], .SDcols =paste0('k_', 1:nk) ]
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  return(list(tic = tic(), model = model, lik = lik, step = step, 
              dlik = dlik, time.taken = time.taken, ctrl = ctrl, liks = liks, 
              likm = likm))
}


# # 1.2 Stayers ----------------------------------------------------------

m2.mixt.stayers_wdist <- function (sdata, model, ctrl) 
{
  nk = model$nk
  nf = model$nf
  Y1 = sdata$y1
  J1 = sdata$j1
  X = sdata$x
  nx = length(unique(X))
  N = length(Y1)
  Wmu = t(model$A1)
  Wsg = t(model$S1)
  J1x = X + nx * (J1 - 1)
  J1s <- Matrix(0, nrow = N, ncol = nf * nx, sparse = TRUE)
  II = 1:N + N * (J1x - 1)
  J1s[II] = 1
  tot_count = t(spread(Matrix::colSums(J1s), 2, nk))
  empty_cells = (tot_count[1, ] == 0)
  PI = rdim(model$pk0, nf * nx, nk)
  PI_old = PI
  lik_old = Inf
  iter_start = 1
  for (count in iter_start:ctrl$maxiter) {
    norm1 = (dnorm(spread(Y1, 2, nk), t(Wmu[, J1]), t(Wsg[, 
                                                         J1]))+1)
    
    tau = PI[J1x, ] * norm1
    tsum = Matrix::rowSums(tau)
    tau = tau/spread(tsum, 2, nk)
    lik = -sum(log(tsum))
    PI = t.default(as.array(t(tau) %*% J1s/tot_count))
    PI[empty_cells, ] = array(1/nk, c(sum(empty_cells), 
                                      nk))
    dPI = abs(PI - PI_old)
    max_change = max(dPI)
    mean_change = mean(dPI)
    PI_old = PI
    if (!is.finite(lik)) {
      status = -5
      break
    }
    prg = (lik_old - lik)/lik
    lik_old = lik
    if ((count%%ctrl$ncat) == (ctrl$ncat - 1)) {
      flog.info("[%3i][%s] lik=%4.4e inc=%4.4e max-pchg=%4.4e mean-pchg=%4.4e", 
                count, ctrl$textapp, lik, prg, max_change, mean_change)
      flush.console()
    }
    if (max_change < ctrl$tol) {
      status = 1
      msg = "converged"
      break
    }
  }
  model$pk0 = rdim(PI, nx, nf, nk)
  model$liks = lik
  model$NNs = sdata[, .N, j1][order(j1)][, N]
  model$dist = as.data.table(cbind(sdata$wid, tau))
  colnames(model$dist) <- c('wid', paste0('k_', 1:nk) )
  model$dist <- model$dist %>%
    .[, (paste0('k_', 1:nk)):=lapply(.SD, as.numeric), .SDcols =paste0('k_', 1:nk) ]%>%
    .[, max_k := do.call(pmax, .SD), .SD = paste0('k_', 1:nk) ] %>%
    .[, max_k := names(.SD)[max.col(.SD)], .SDcols =paste0('k_', 1:nk) ]
  
  return(model)
}



# 2. Modify global function so that it returns the stats ------------------

m2.mixt.estimate.all_wdist <- function (sim, nk = 6, ctrl, cl = NA) 
{
  start.time <- Sys.time()
  sdata = sim$sdata
  jdata = sim$jdata
  mm = mean(sdata$y1)
  ms = 2 * sd(sdata$y1)
  if (!("x" %in% names(sdata))) {
    flog.info("creating an x column in sdata and set it to 1")
    sdata$x = 1
  }
  else if (length(unique(sdata$x)) >= 50) {
    stop("likely too many values in the x column of sdata")
  }
  nf = max(sdata$j1)
  model_start = m2.mixt.new(nk, nf)
  res_para = m2.mixt.movers_wdist(jdata, model_start, ctrl = em.control(ctrl, 
                                                                  cstr_type = "para", textapp = "para0", fixb = F))
  if (!any(is.na(cl))) {
    flog.info("cluster -- exporting objects to nodes")
    clusterExport(cl, c("res_para", "jdata", "ctrl"), environment())
    mylapply <- function(...) parLapply(cl, ...)
    nnodes = length(cl)
  }
  else {
    mylapply <- function(...) lapply(...)
    nnodes = 1
  }
  flog.info("starting repetitions with %i nodes", nnodes)
  rr = mylapply(1:ctrl$est_rep, function(i) {
    res_mixt = list()
    tryCatch({
      res_para$model$A1 = spread(sort(rnorm(nk)) * ms + 
                                   mm, 1, nf)
      res_para$model$A2 = res_para$model$A1
      res_para_fixm = m2.mixt.movers_wdist(jdata, res_para$model, 
                                     ctrl = em.control(ctrl, cstr_type = "para", 
                                                       textapp = sprintf("paraf (%i/%i)", i, ctrl$est_rep), 
                                                       fixm = T, fixb = F))
      res_para_new = m2.mixt.movers_wdist(jdata, res_para_fixm$model, 
                                    ctrl = em.control(ctrl, textapp = sprintf("para1 (%i/%i)", 
                                                                              i, ctrl$est_rep), cstr_type = "para", fixm = F, 
                                                      fixb = F))
      res_mixt = m2.mixt.movers_wdist(jdata, res_para_new$model, 
                                ctrl = em.control(ctrl, textapp = sprintf("move1 (%i/%i)", 
                                                                          i, ctrl$est_rep)))
      res_mixt$connectedness = model.connectiveness(res_mixt$model)
      res_mixt$rep_id = i
    }, error = function(e) {
      catf("error in rep %i!\n", i)
      print(e)
    })
    flog.info("done with reptitions %i/%i", i, ctrl$est_rep)
    res_mixt
  })
  save(rr, ctrl, file = paste(ctrl$file_backup_prefix, "data", 
                              sep = "."))
  rrd = ldply(rr, function(r) {
    data.frame(lik_mixt = r$model$likm, connectedness = r$connectedness, 
               i = r$rep_id)
  })
  rrd = data.table(rrd)
  rrd[, `:=`(sel, -1)]
  rrd.sub = rrd[order(-lik_mixt)][1:ctrl$est_nbest]
  rrd[i %in% rrd.sub$i, `:=`(sel, 0)]
  Ibest = rrd.sub[order(-connectedness)][1, i]
  res_mixt = rr[[Ibest]]
  rrd[i == Ibest, `:=`(sel, 1)]
  if (ctrl$sdata_subredraw == TRUE) {
    sim$sdata[, `:=`(sample, rank(runif(.N))/.N <= ctrl$sdata_subsample), 
              j1]
    flog.info("drawing %f from the stayers", ctrl$sdata_subsample)
  }
  res_mixt$model = m2.mixt.stayers_wdist(sim$sdata[sample == 1], 
                                   res_mixt$model, ctrl = em.control(ctrl, textapp = "stayers"))
  res_mixt$second_stage_reps = rrd
  res_mixt$second_stage_reps_all = rr
  NNm = res_mixt$model$NNm
  NNs = res_mixt$model$NNs/ctrl$sdata_subsample
  NNm[!is.finite(NNm)] = 0
  NNs[!is.finite(NNs)] = 0
  share_s = sum(NNs)/(sum(NNm) + sum(NNs))
  share_m = sum(NNm)/(sum(NNm) + sum(NNs))
  NNs = round(NNs * ctrl$vdec_sim_size * share_s/sum(NNs))
  NNm = round(NNm * ctrl$vdec_sim_size * share_m/sum(NNm))
  #sdata.sim = m2.mixt.simulate.stayers(res_mixt$model, NNs)
  #jdata.sim = m2.mixt.simulate.movers(res_mixt$model, NNm)
  #sdata.sim = rbind(sdata.sim[, list(j1, k, y1)], jdata.sim[, 
  #                                                          list(j1, k, y1)])
  #vdec = lin.proj(sdata.sim, y_col = "y1", k_col = "k", j_col = "j1")
  #res_mixt$vdec = vdec
  res_mixt$ctrl = ctrl
  end.time <- Sys.time()
  res_mixt$time.taken <- end.time - start.time
  return(res_mixt)
}


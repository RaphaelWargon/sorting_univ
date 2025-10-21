# ------------    CONSTRAINTS ---------------

# imposese that a_k1_l1 - a_k2_l1 = a_k1_l2 - a_k2_l2 for all k1,k2,l1,l2
cons.lin_add <- function(nk,nf) {
  LL = array(0,c(nf-1,nf))
  for (l in 1:(nf-1)) { LL[l,l]=1; LL[l,l+1]=-1}
  KK = array(0,c(nk-1,nk))
  for (k in 1:(nk-1)) { KK[k,k]=1; KK[k,k+1]=-1}
  C1     = kronecker(LL,KK)
  H1      = rep(0,dim(C1)[1])
  meq     = dim(C1)[1]
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}

cons.lin <- function(nk,nf,gap=0) {
  KK = array(0,c(nk-1,nk))
  for (k in 1:(nk-1)) { KK[k,k]=1; KK[k,k+1]=-1}
  
  LL = array(0,c(nf-1,nf))
  for (l in 1:(nf-1)) { LL[l,l]=1; LL[l,l+1]=-1}
  C1    = kronecker(LL,KK)
  H1    = rep(0,dim(C1)[1])
  meq   = dim(C1)[1]
  
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}


cons.akm <- function(nk,nf,gap=0) {
  KK = array(0,c(nk-1,nk)) 
  LL = array(0,c(nf-1,nf))
  for (k in 1:(nk-1)) { KK[k,k]=1; KK[k,k+1]=-1}
  for (l in 1:(nf-1)) { LL[l,l]=1; LL[l,l+1]=-1}
  
  C1      = kronecker(LL,KK)
  H1      = rep(0,dim(C1)[1])
  meq     = dim(C1)[1]
  
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}


cons.akmmono <- function(nk,nf,gap=0) {
  KK = array(0,c(nk-1,nk))
  for (k in 1:(nk-1)) { KK[k,k]=1; KK[k,k+1]=-1}
  C1     = -kronecker(diag(nf),KK)
  H1     = rep(gap,nf*(nk-1))
  meq    = 0
  
  LL = array(0,c(nf-1,nf))
  for (l in 1:(nf-1)) { LL[l,l]=1; LL[l,l+1]=-1}
  C1b     = kronecker(LL,KK)
  C1      = rbind(C1b,C1)
  H1      = c(rep(0,dim(C1b)[1]),H1)
  meq     = dim(C1b)[1]
  
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}

cons.mono_k <- function(nk,nf,gap=0) {
  KK = array(0,c(nk-1,nk))
  for (k in 1:(nk-1)) { KK[k,k]=1; KK[k,k+1]=-1}
  C1     = -kronecker(diag(nf),KK)
  H1     = rep(gap,nf*(nk-1))
  meq    = 0
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}

cons.fixb <- function(nk,nf,nt=4) {
  CC = cons.mono_k(nk,nf)$C
  MM = array( 0,c(nt-1,nt))
  for (i in 1:(nt-1)) {
    MM[i,i]=1
    MM[i,i+1]=-1
  }
  CC  = kronecker(MM,CC)
  H1  = rep(0, (nt-1)*(nk-1)*nf )
  meq = (nt-1)*(nk-1)*nf 
  return(list(C=CC,H=H1,meq=meq,nk=nk,nf=nf))
}

cons.biggerthan <-function(nk,nf,gap=0) {
  CC = diag(nk*nf)
  H1 = rep(gap,nk*nf)
  meq=0
  return(list(C=CC,H=H1,meq=meq,nk=nk,nf=nf))
}

cons.lin_para <- function(nk,nf) {
  LL = array(0,c(nf-1,nf))
  for (l in 1:(nf-1)) { LL[l,l]=1; LL[l,l+1]=-1}
  C1     = -kronecker(LL,diag(nk))
  H1     = rep(0,(nf-1)*nk)
  meq    = (nf-1)*nk
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}

cons.none <- function(nk,nf) {
  meq=0
  C1 = matrix(0,1,nk*nf)
  H1 = c(0)
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}

cons.bind <- function(c1,c2) {
  
  if (c1$meq==0) {
    I11 = c(); I12 = 1:length(c1$H);
  } else if (c1$meq==length(c1$H)) {
    I11 = 1:length(c1$H); I12 = c();
  } else {
    I11 = 1:c1$meq; I12 = (c1$meq+1):length(c1$H)
  }
  if (c2$meq==0) {
    I21 = c(); I22 = 1:length(c2$H);
  } else if (c2$meq==length(c2$H)) {
    I21 = 1:length(c2$H); I22 = c();
  } else {
    I21 = 1:c2$meq; I22 = (c2$meq+1):length(c2$H)
  }
  
  c1$C = rbind(c1$C[I11,],c2$C[I21,],c1$C[I12,],c2$C[I22,])
  c1$H = c(c1$H[I11],c2$H[I21],c1$H[I12],c2$H[I22])
  c1$meq = c1$meq + c2$meq
  return(c1)
}

# add right padding
cons.pad <- function(c1,l,r) {
  c1$C = cbind(matrix(0,dim(c1$C)[1],l),c1$C,matrix(0,dim(c1$C)[1],r))
  return(c1)
}

cons.get <-function(name,val,nk,nf) {
  if (name=="none") {
    return(cons.none(nk,nf))
  } else if (name=="mono_k") {
    return(cons.mono_k(nk,nf,val))
  } else if (name=="para") {
    return(cons.lin_para(nk,nf))
  } else if (name=="akm") {
    return(cons.akm(nk,nf,val))
  } else if (name=="akmmono") {
    return(cons.akmmono(nk,nf,val))
  } else if (name=="lin") {
    return(cons.lin(nk,nf))
  } else {
    error("unkown constraint")
  }
}

cons.sum <- function(nk,nf) {
  CC = kronecker(diag(nf),t(rep(1,nk)))
  C1     = kronecker(diag(nf),t(rep(1,nk)))
  H1     = rep(0,nf)
  meq    = nf
  return(list(C=C1,H=H1,meq=meq,nk=nk,nf=nf))
}


#' Generate a linear projection decomposition for the model
#' with continuous worker hetergoneity
#'
#' @export
lin.proja <- function(sdata,y_col="y",k_col="k",j_col="j",do_interacted_reg=1) {
  rr = list()
  
  sdata2 = copy(data.table(sdata))
  sdata2[,y_imp := get(y_col)]
  sdata2[,k_imp := get(k_col)]
  sdata2[,j     := get(j_col)]
  
  fit = lm(y_imp ~ k_imp + factor(j),sdata2)
  sdata2$res = residuals(fit)
  pred = predict(fit,type = "terms")
  sdata2$k_hat = pred[,1]
  sdata2$l_hat = pred[,2]
  
  rr$cc = sdata2[,cov.wt( data.frame(y_imp,k_hat,l_hat,res))$cov]
  rr$rsq1 = summary(fit)$r.squared
  
  if (do_interacted_reg==1) {
    fit2 = lm(y_imp ~ 0+  k_imp:factor(j) + factor(j),sdata2)
    rr$rsq2 = 1-mean(resid(fit2)^2)/var(sdata2$y_imp)
  } else {
    rr$rsq2=NA
  }
  
  get.stats <- function(cc) {
    r=list()
    den = cc[2,2] + cc[3,3] + 2 * cc[2,3]
    r$cor_kl = round(cc[2,3]/sqrt(cc[2,2]*cc[3,3]),4)
    r$cov_kl = 2*round(cc[2,3]/den,4)
    r$var_k  = round(cc[2,2]/den,4)
    r$var_l  = round(cc[3,3]/den,4)
    r$rsq    = round((cc[1,1] - cc[4,4])/cc[1,1],4)
    return(r)
  }
  
  rr$stats = get.stats(rr$cc)
  print(data.frame(rr$stats))
  rr$NNs = sdata[,.N,j1][order(j1)][,N]
  
  return(rr)
}



#' @export
lin.proj <- function(sdata,y_col="y",k_col="k",j_col="j",usex=FALSE,do.unc=TRUE) {
  rr = list()
  
  sdata2 = copy(data.table(sdata))
  sdata2[,y_imp := get(y_col)]
  sdata2[,k_imp := get(k_col)]
  sdata2[,j     := get(j_col)]
  
  fit = lm(y_imp ~ factor(k_imp) + factor(j),sdata2)
  sdata2$res = residuals(fit)
  pred = predict(fit,type = "terms")
  sdata2$k_hat = pred[,1]
  sdata2$l_hat = pred[,2]
  
  rr$cc = sdata2[,cov.wt( data.frame(y_imp,k_hat,l_hat,res))$cov]
  rr$rsq1 = summary(fit)$r.squared
  
  if (do.unc) {
    fit2 = lm(y_imp ~factor(k_imp) * factor(j),sdata2)
    rr$rsq2 = summary(fit2)$r.squared
  }
  
  get.stats <- function(cc) {
    r=list()
    den = cc[2,2] + cc[3,3] + 2 * cc[2,3]
    r$cor_kl = round(cc[2,3]/sqrt(cc[2,2]*cc[3,3]),4)
    r$cov_kl = 2*round(cc[2,3]/den,4)
    r$var_k  = round(cc[2,2]/den,4)
    r$var_l  = round(cc[3,3]/den,4)
    r$rsq    = round((cc[1,1] - cc[4,4])/cc[1,1],4)
    return(r)
  }
  
  rr$stats = get.stats(rr$cc)
  rr$NNs = sdata2[,.N,j][order(j)][,N]
  print(data.frame(rr$stats))
  
  return(rr)
}

#' Computes the linear projection using X
#' @export
lin.projx <- function(sdata,y_col="y",k_col="k",j_col="j") {
  rr = list()
  
  sdata2 = copy(data.table(sdata))
  sdata2[,y_imp := get(y_col)]
  sdata2[,k_imp := get(k_col)]
  sdata2[,j     := get(j_col)]
  
  fit = lm(y_imp ~ factor(k_imp) + factor(j) +factor(x),sdata2)
  sdata2$res = residuals(fit)
  pred = predict(fit,type = "terms")
  sdata2$k_hat = pred[,1]
  sdata2$l_hat = pred[,2]
  sdata2$x_hat = pred[,3]
  
  rr$cc = sdata2[,cov.wt( data.frame(y_imp,k_hat,l_hat,res,x_hat))$cov]
  
  fit2 = lm(y_imp ~factor(k_imp) * factor(j),sdata2)
  
  rr$rsq1 = summary(fit)$r.squared
  rr$rsq2 = summary(fit2)$r.squared
  
  get.stats <- function(cc) {
    r=list()
    den = cc[2,2] + cc[3,3] + 2 * cc[2,3]
    r$cor_kl = round(cc[2,3]/sqrt(cc[2,2]*cc[3,3]),4)
    r$cov_kl = 2*round(cc[2,3]/den,4)
    r$var_k  = round(cc[2,2]/den,4)
    r$var_l  = round(cc[3,3]/den,4)
    r$rsq    = round((cc[1,1] - cc[4,4])/cc[1,1],4)
    return(r)
  }
  
  rr$stats = get.stats(rr$cc)
  print(data.frame(rr$stats))
  
  return(rr)
}

#' Computes the linear projection using X
#' @export
lin.projax <- function(sdata,y_col="y",k_col="k",j_col="j") {
  rr = list()
  
  sdata2 = copy(data.table(sdata))
  sdata2[,y_imp := get(y_col)]
  sdata2[,k_imp := get(k_col)]
  sdata2[,j     := get(j_col)]
  
  fit = lm(y_imp ~ k_imp + factor(j) +factor(x),sdata2)
  sdata2$res = residuals(fit)
  pred = predict(fit,type = "terms")
  sdata2$k_hat = pred[,1]
  sdata2$l_hat = pred[,2]
  sdata2$x_hat = pred[,3]
  
  rr$cc = sdata2[,cov.wt( data.frame(y_imp,k_hat,l_hat,res,x_hat))$cov]
  rr$rsq1 = summary(fit)$r.squared
  
  get.stats <- function(cc) {
    r=list()
    den = cc[2,2] + cc[3,3] + 2 * cc[2,3]
    r$cor_kl = round(cc[2,3]/sqrt(cc[2,2]*cc[3,3]),4)
    r$cov_kl = 2*round(cc[2,3]/den,4)
    r$var_k  = round(cc[2,2]/den,4)
    r$var_l  = round(cc[3,3]/den,4)
    r$rsq    = round((cc[1,1] - cc[4,4])/cc[1,1],4)
    return(r)
  }
  
  rr$stats = get.stats(rr$cc)
  print(data.frame(rr$stats))
  
  return(rr)
}

#' Create a control structure for running EM algorithms
#'
#' @param nplot how often to plot wages of comparaison to model0
#' @param ncat how often to log information
#' @param maxiter maximum number of iterations
#' @param model_var whether to allow for flexible variance (default is TRUE)
#' @param est_Amb TBD
#' @param cstr_type defines the type of constraints on the means for estimation
#' @param cstr_val TBD
#' @param tol tolerance for stopping the EM
#' @param tau posterior likelihood to use instead of computing them using initial parameters
#' @param model0 model to compare estimation to when plotting
#' @param presp=1e-9 TBD
#' @param rel_weight setting a particular weight for stayers versus movers (default is 1)
#' @param est_rho  vector of TRUE/FALSE that states which rho should be estimated
#' @param rho_in_diff whether to estimate rho in level of in differences
#' @param dprior    Dirichlet prior for proportions (default 1.01)
#' @param nfirms_to_update=10, number firms to update in probabilistic approach
#' @param proba_include what terms to include in the liklihood for probabilistic approach (default to = c(1,1,1,1))
#' @param check_lik whether to check the likelihood at each updating parameter
#' @param stochastic whether to use stochastic EM instead of straight EM
#' @param fixb   when TRUE, imposes fixed interactions in different time periods
#' @param fixm when TRUE, levels are not updated, only variances and proportions
#' @param deps=1e-50 TBD
#' @param file_backup_prefix TBD
#' @param sd_floor= floor imposed on all standard deviations (default is 1e-10)
#' @param posterior_reg term added to posterior probablities (this is to deal with numerical issues, default is 1e-9)
#' @param textapp  text to show in logging
#' @param sdata_subsample share of the stayers to use in estimation
#' @param sdata_subredraw whether to redraw the subsample of stayers
#' @param vdec_sim_size  size to use in simulation
#' @param stayer_weight weight attributed to stayers in joint estimation
#' @param est_rep= number of starting values for EM
#' @param est_nbest number of best starting values to select from using connectedness
#'
#' @export
em.control <- function(ctrl=NULL,...) {
  args = list(...)
  if (is.null(ctrl)) {
    ctrl = list(
      nplot=5,
      ncat=25,
      maxiter=2000,
      model_var=TRUE,
      est_Amb=TRUE,
      cstr_type = "none",
      cstr_val = 0,
      tol=1e-9,
      tau=NA,
      model0=NA,
      presp=1e-9,
      rel_weight = 1,
      est_rho=FALSE,       # vector of TRUE/FALSE that states which rho should be estimated
      rho_in_diff=FALSE,   # whether to estimate rho in level of in differences
      dprior = 1.01,       # Dirichlet prior for proportions
      nfirms_to_update=10, # number firms to update in probabilistic approach
      proba_include = c(1,1,1,1), # what terms to include in the liklihood for probabilistic approach
      check_lik=FALSE,
      stochastic=0,
      fixb=FALSE,          # when TRUE, imposes fixed interactions in different time periods
      fixm=FALSE,          # when TRUE, levels are not updated, only variances and proportions
      deps=1e-50,
      file_backup_prefix = "estimation-bu-tmp",
      sd_floor=1e-10,      # floor imposed on all standard deviations
      posterior_reg=1e-9,  # term added to posterior probablities (this is to deal with numerical issues)
      textapp="",          # text to show in logs
      sdata_subsample=1.0, # share of the stayers to use in estimation
      sdata_subredraw=TRUE,# whether to redraw the subsample of stayers
      vdec_sim_size=1e6,        # size to use in simulation
      stayer_weight=1,     # weight attributed to stayers in joint estimation
      est_rep=10,          # number of starting values for EM
      est_nbest=5)         # number of best starting values to select from using connectedness
  }
  ctrl[names(args)]  = args[names(args)]
  
  # for the following argument, if only one, we repeat them
  for (nn in c('cstr_type','cstr_val','est_rho','est_Amb')) {
    if (length(ctrl[[nn]])==1) ctrl[[nn]]=rep(ctrl[[nn]],6);
  }
  
  return(ctrl)
}


#' functions for em
#' @export
lognormpdf <- function(Y,mu=0,sigma=1)   -0.5 * (  (Y-mu) / sigma )^2   - 0.5 * log(2.0*pi) - log(sigma)

#' logsumexp function
#' @export
logsumexp <- function(v) {
  vm = max(v)
  log(sum(exp(v-vm))) + vm
}

#' logsumexp function by Row
#' @export
logRowSumExp <- function(M) {
  if (is.null(dim(M))) {return(M)}
  vms = apply(M,1,max)
  log(rowSums(exp(M-spread(vms,2,dim(M)[2])))) + vms
}
catf <- function(...) cat(sprintf(...))

#' this is a utility function to generate
#' multidimensional arrays - like the spread function in fortran
#' @export
spread <- function (A, loc, dims) {
  if (!(is.array(A))) {
    A = array(A, dim = c(length(A)))
  }
  adims = dim(A)
  l = length(loc)
  if (max(loc) > length(dim(A)) + l) {
    stop("incorrect dimensions in spread")
  }
  sdim = c(dim(A), dims)
  edim = c()
  oi = 1
  ni = length(dim(A)) + 1
  for (i in c(1:(length(dim(A)) + l))) {
    if (i %in% loc) {
      edim = c(edim, ni)
      ni = ni + 1
    }
    else {
      edim = c(edim, oi)
      oi = oi + 1
    }
  }
  return(aperm(array(A, dim = sdim), edim))
}

hist2 <- function(Y1,Y2,wsup) {
  n = length(wsup)
  H = array(0,c(n,n))
  for (i in 1:n) for (j in 1:n) {
    H[i,j] = sum(  (Y1 < wsup[i]) & (Y2 < wsup[j]) )
  }
  H[n,n] = length(Y1)
  H = H/H[n,n]
  return(H)
}

hist1 <- function(Y1,wsup) {
  n = length(wsup)
  H = array(0,c(n))
  for (i in 1:n) {
    H[i] = sum(  (Y1 < wsup[i]) )
  }
  H[n] = length(Y1)
  H = H/H[n]
  return(H)
}

# allow to use 2 different supports
hist2d <- function(Y1,Y2,wsup) {
  n = length(wsup)
  H = array(0,c(n,n))
  for (i in 1:n) for (j in 1:n) {
    H[i,j] = sum(  (Y1 < wsup[i]) & (Y1 >= wsup[i-1]) & (Y2 < wsup[j]) & (Y1 >= wsup[i-1])  )
  }
  H[n,n] = length(Y1)
  H = H/H[n,n]
  return(H)
}

# smoothed histogram
hist2s <- function(Y1,Y2,wsup,h) {
  n = length(wsup)
  H = array(0,c(n,n))
  for (i in 1:n) for (j in 1:n) {
    H[i,j] = sum(  pnorm( (wsup[i] - Y1 )/h ) *  pnorm( (wsup[j] - Y2 )/h ) )
  }
  H = H/H[n,n]
  return(H)
}

#' @export
rdim <- function(A,...) {
  dd <- c(...);
  if (length(dd)==1) {
    dim(A)<-dd[[1]]
  } else {
    dim(A) <- dd
  }
  return(A)
}

tic.new <- function() {
  t = Sys.time()
  tt = list(all.start=t,last.time=t,loop=0,timers=list())
  
  tic.toc <- function(name="") {
    t = Sys.time()
    if (name=="") {
      return(tt)
    }
    
    if (name %in% names(tt$timers)) {
      tm = tt$timers[[name]]
      tm$count = tm$count+1
      tm$total = tm$total + t - tt$last.time
      tt$timers[[name]] = tm
    } else {
      tm = list(count=1,total=t - tt$last.time)
      tt$timers[[name]] = tm
    }
    tt$last.time=t;
    tt <<- tt
  }
  
  return(tic.toc)
}

#' order cluster by increasing wage
#' @export
cluster.order <- function(sim) {
  sim$sdata = sim$sdata[!is.na(j1)]
  I = sim$sdata[,mean(y1),j1][order(j1)][,rank(V1)]
  sim$sdata[,j1:=I[j1]][,j2:=I[j2]]
  sim$jdata[,j1:=I[j1]][,j2:=I[j2]]
  return(sim)
}

mvar <- function(x) {
  if (length(x)<=1) return(0);
  return(var(x))
}
mcov <- function(x,y) {
  if (length(x)<=1) return(0);
  return(cov(x,y))
}

lm.wfitc <- function(XX,YY,rw,C1,C0,meq) {
  
  S = apply(abs(XX),2,max)
  XX2 = XX*spread(1/S,1,dim(XX)[1])
  C12 = C1*spread(1/S,1,dim(C1)[1])
  
  XXw      = diag(rw) %*% XX2
  Dq       = t(XXw) %*% XX2
  dq       = t(YY %*% XXw)
  
  # do quadprod
  fit      = solve.QP(Dq,dq,t(C12),C0,meq)
  
  # rescale
  fit$solution = fit$solution/S
  
  return(fit)
}

# fits a weighted ols with non-negative constraints
lm.wfitnn <- function(XX,YY,rw,floor = 0) {
  
  n = dim(XX)[2]
  XX2 = XX
  #   S = apply(abs(XX),2,max)
  #   XX2 = XX*spread(1/S,1,dim(XX)[1])
  #   C12 = C1*spread(1/S,1,dim(C1)[1])
  
  XXw      = diag(rw) %*% XX2
  Dq       = t(XXw) %*% XX2
  dq       = t(YY %*% XXw)
  C1       = diag(n)
  C0       = rep(floor,n)
  
  #fit      = qprog(Dq,dq,C1,C0)
  #fit$solution = as.numeric(fit$thetahat)
  fit      = solve.QP(Dq,dq,C1,C0)
  return(fit)
}

# fits a linear problem with weights under constraints
slm.wfitc <- function(XX,YY,rw,CS,scaling=0) {
  nk = CS$nk
  nf = CS$nf
  YY = as.numeric(YY)
  XX = as.matrix.csr(XX)
  # to make sure the problem is positive semi definite, we add
  # the equality constraints to the XX matrix! nice, no?
  
  if (CS$meq>0) {
    XXb = rbind(XX,  as.matrix.csr(CS$C[1:CS$meq,]))
    YYb = c(YY,CS$H[1:CS$meq])
    rwb  = c(rw,rep(1,CS$meq))
  } else {
    XXb = XX
    YYb = YY
    rwb = rw
  }
  
  t2 = as(dim(XXb)[1],"matrix.diag.csr")
  t2@ra = rwb
  XXw = t2 %*% XXb
  Dq       = SparseM:::as.matrix(SparseM:::t(XXw) %*% XXb)
  dq       = SparseM:::t(YYb %*% XXw)
  
  # scaling
  #
  if (scaling>0) {
    sc <- norm(Dq,"2")^scaling
  } else {
    sc=1
  }
  
  # do quadprod
  tryCatch({
    fit      = solve.QP(Dq/sc,dq/sc,t(CS$C)/sc,CS$H/sc,CS$meq)
  }, error = function(err) {
    browser()
  })
  
  return(fit)
}

#' Computes graph connectedness among the movers
#' within each type and returns the smalless value
#' @export
model.connectiveness <- function(model,all=FALSE) {
  EV = rep(0,model$nk)
  pk1 = rdim(model$pk1,model$nf,model$nf,model$nk)
  dd_post = data.table(melt(pk1,c('j1','j2','k')))
  pp = model$NNm/sum(model$NNm)
  dd_post <- dd_post[, pr_j1j2 := pp[j1,j2],list(j1,j2)  ]
  dd_post <- dd_post[, pr_j1j2k := pr_j1j2*value]
  
  for (kk in 1:model$nk) {
    # compute adjency matrix
    A1 = acast(dd_post[k==kk, list(pr=pr_j1j2k/sum(pr_j1j2k),j2,j1)],j1~j2,value.var = "pr")
    A2 = acast(dd_post[k==kk, list(pr=pr_j1j2k/sum(pr_j1j2k),j2,j1)],j2~j1,value.var = "pr")
    # construct Laplacian
    A = 0.5*A1 + 0.5*A2
    D = diag( rowSums(A)^(-0.5) )
    L = diag(model$nf) - D%*%A%*%D
    EV[kk] = sort(eigen(L)$values)[2]
    #print(eigen(L)$values)
  }
  if (all==TRUE) return(EV);
  return(min(abs(EV)))
}

#' plots the wages of a model
#' @export
m2.mixt.wplot <- function(Wm) {
  dd = melt(Wm,c('l','k'))
  ggplot(dd,aes(x=factor(l),color=factor(k),group=factor(k),y=value)) + geom_line() + theme_bw()
}

#' @export
mplot <- function(M) {
  mm = melt(M,c('i','j'))
  #mm$i = factor(mm$i)
  #mm$j = factor(mm$j)
  mm = mm[mm$value>0,]
  ggplot(mm,aes(x=j,y=i,fill=value)) + geom_tile() + theme_bw() + scale_y_reverse()
}

#' plots the proportions of a model
#' @export
m2.mixt.pplot <- function(pk0) {
  dd = as.data.table(melt(pk0,c('l','k')))
  dd$l = ((1:nrow(dd)-1) %/%dim(pk0)[2])+1
  ggplot(dd,aes(x=factor(l),y=value,fill=factor(k))) + geom_bar(position="stack",stat = "identity") + theme_bw()
}

#' creates a matrix and fill it using a data.table
#' in contrast to acast, it creates all rows and cols (even if there is not data)
#' @export
mcast <- function(dd,val,row,col,dim,fill=NA) {
  
  M  = array(fill,dim)
  ri = dd[, get(row)]
  ci = dd[, get(col)]
  vv = dd[, get(val)]
  for (i in 1:max(ri)) {
    I = which(ri==i)
    M[i,ci[I]] = vv[I]
  }
  return(M)
}

#' creates a vector and fill it using a data.table
#' in contrast to acast, it creates all elements
#' @export
vcast <- function(dd,val,index,size,fill=NA) {
  V    = rep(fill,size)
  I    = dd[, get(index)]
  v    = dd[, get(val)]
  V[I] = v
  return(V)
}

#' Weighted mean
#' @export
wt.mean <- function(x,w) {
  w = w/sum(w)
  m1 = sum(x*w)
  return(m1)
}

#' Weighted variance
#' @export
wt.var <- function(x,w) {
  w = w/sum(w)
  m1 = sum(x*w)
  v1 = sum((x-m1)^2*w)
  return(v1)
}

#' Weighted covariance
#' @export
wt.cov <- function(x,y,w) {
  w = w/sum(w)
  m1 = sum(x*w)
  v1 = sum((x-m1)^2*w)
  m2 = sum(y*w)
  v2 = sum((y-m2)^2*w)
  cc = sum( (y-m2)*(x-m1)*w)
  return(cc)
}

#' Sparse colSums
#' @export
sColSums <- function(M) {
  return(as.numeric((rep(1,dim(M)[1]) %*% M)))
}

#' Sparse rowSums
#' @export
sRowSums <- function(M) {
  return(as.numeric(M %*% rep(1,dim(M)[2])))
}


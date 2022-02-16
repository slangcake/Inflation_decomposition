#----------------------------------------------------------
#Inflation RB June 2021
#----------------------------------------------------------

# Replicating Mahedy & Shapiro - splitting inflation into various characteristics
# https://www.frbsf.org/economic-research/files/el2017-35.pdf?mod=article_inline

#Detail on constructing CPI indices in Australia

# https://www.abs.gov.au/AUSSTATS/abs@.Nsf/39433889d406eeb9ca2570610019e9a5/e16115198d024042ca25768e002c82a0!OpenDocument

#---------------------------------
#Preamble
library('oxgraphs')
ox_setup()
require('car')
require('rlist')

this_path <- dirname(getActiveDocumentContext()$path)
setwd(this_path)
#----------------------------------
#User inputs
#Name a desktop path - makes importing db files much quicker
desktop_path <- 'C:/Users/Sean Langcake/Desktop/Tmp files'

oe_db <- 'C:/OEF/Feb22/AusFeb22v2(1).db'

#Model specifications to test - select multiple, and at least one

#Unemployment gap (linear and/or non-linear)
gp_opts <- 'lin'    #c('lin','nonlin')
  
#Import price proxy (GEM series)
tp_opts <- 'PMG'    #c('XR','PM','PMG')

#Significance level for hypothesis tests    
sig_opts <- 0.1     #c(0.05,0.1)
      
#Inclusion of inflation expectations series
ie_opts <- 'ie'      #c(ie,no_ie)

#-------------------------------
#Functions

#Make a dataframe that neatly shows which category things fall into
ct_cleanup <- function(categories){
  
  ct <- t(categories) %>% as.data.frame()
  
  for(j in 1:nrow(ct)){
    ct[j,which(ct[j,]==1)] <- row.names(ct)[j]
  }
  
  for(k in 1:ncol(ct)){
    ct[,k] <- ct[order(ct[,k]),k] 
    ct[,k] <- ct[(nrow(ct):1),k]
  }
  row.names(ct) <- NULL
  
  if(all(ct[50,]=='0')){ct <- ct[1:49,]}
  if(all(ct[45,]=='0')){ct <- ct[1:44,]}
  if(all(ct[40,]=='0')){ct <- ct[1:39,]}
  if(all(ct[35,]=='0')){ct <- ct[1:34,]}
  if(all(ct[30,]=='0')){ct <- ct[1:29,]}
  
  return(ct)
}

#Function to run Phillips curve estimation
pc_est <- function(n,data){
  
  #Add dependent variable (and lags) to regressors dataframe
  l <- data[c('Dates',n)] %>% filter(.,Dates<=max_date,Dates>=data$Dates[1])
  
  l['dx'] <- l[n]/lag(l[n])*100-100
  
  l2 <- mutate(l,dx1=lag(dx,1),
               dx2=lag(dx,2),
               dx3=lag(dx,3)) %>% select(dx,dx1,dx2,dx3)
  
  tmp <- cbind(l2,select(r2,-Dates)) %>% drop_na()
  
  #Drop GST dummies if history is too short
  if(sum(tmp$d_gst1)==0){tmp <- select(tmp,-d_gst1)}
  
  res <- lm(dx~.,data=tmp)
  return(res)
}

#Function to calculate relevant p values from joint hypothesis tests
hypotheses <- function(n,p,r){
  
  z <- p[[n]]$coefficients[,4]
  
  y <- linearHypothesis(results[[n]],c('dx1=0','dx2=0','dx3=0'))
  
  x <- linearHypothesis(results[[n]],c('dpm1=0','dpm2=0','dpm3=0'))
  
  list('L_mkt' = z[names(z)=='ugap'] %>% unname(),
       'XR' = x$`Pr(>F)`[2],
       'Persistence' = y$`Pr(>F)`[2])
}

#Function that categorises components
categorise_results <- function(n,hyp,res,sig_level=0.1){
  
  cat <- rep(0,4)
  
  #Labour market sensitivity
  if(hyp_results[[n]]$L_mkt<sig_level){
    tmp <- results[[n]]$coefficients
    if(tmp[which(names(tmp)=='ugap')]<0){
      cat[1] <- 1
    }
  }
  
  #XR sensitive
  if(hyp_results[[n]]$XR<sig_level && cat[1]!=1){
    tmp <- results[[n]]$coefficients
    if((tmp[which(names(tmp)=='dpm1')]+tmp[which(names(tmp)=='dpm2')]+tmp[which(names(tmp)=='dpm3')])<0){
      cat[2] <- 1
    }
  }
  
  #Persistent Acyclical
  
  if(hyp_results[[n]]$Persistence<sig_level && cat[1]!=1 && cat[2]!=1){
    tmp <- results[[n]]$coefficients
    if((tmp[which(names(tmp)=='dx1')]+tmp[which(names(tmp)=='dx2')]+tmp[which(names(tmp)=='dx3')])>0){
      cat[3] <- 1
    }
  }
  
  #Transitory acyclical
  if(cat[1]!=1 && cat[2]!=1 && cat[3]!=1){
    cat[4] <- 1
  }
  return(cat)
}

#Function to scale effective weights so they equal 1
scale_eff_weights <- function(x,r){
  y <- mapply('*',x[,-1]/100,categories[rownames(categories)==r,]) 
  y2 <- y/rowSums(y)
  return(cbind(x['Dates'],y2))
}
#----------------------------------------------
#Load data
#----------------------------------------------

#Component indices
h_codes <- c('auspcfm','auspcfh','auspfic','auspfbr','auspfsw','auspfci','auspfci1','auspcf1',
                          'auspcf2','auspcf3','auspcf4','auspfo','auspfcs','auspcff','auspcfv','auspcftr',
                          'auspcftf','auspcfg','auspffs','auspfsa','auspcfo','auspcfsc','auspcfbn','auspcbc',
                          'auspcbj','auspcb7','auspcb8','auspcb9','auspcbt','auspcaj','auspcav','auspcak',
                          'auspckm','auspckw','auspckc','auspcaa','auspcar','auspchr','auspche','auspchg',
                          'auspchw','auspchs','auspchc','auspchm','auspcgh','auspcgcf','auspcgt','auspcgj',
                          'auspcgs','auspcgg','auspcge','auspcg1','auspcot','auspcg3','auspcoc','auspcoh',
                          'auspcohs','auspcmph','auspcmq','auspc861','auspc86c','auspctm','auspctf','auspctg',
                          'auspcta','auspcto','auspctu','auspccx','auspcct','auspccae','auspccam','auspcrbk',
                          'auspcrnm','auspcrda','auspcria','auspcres','auspcrgt','auspcrsp','auspcros','auspcrps',
                          'auspcrvp','auspce1','auspce2','auspce3','auspcodl','auspcoz','auspcois')

index_sa <- data_import("Haver",hvr_start="1993",hvr_end="2030",hvr_codes = h_codes) %>% l2w()

#NSA contributions
contrib_nsa <- data_import("Haver",hvr_start="1993",hvr_end="2030",
                    hvr_codes= c('aunpcfmt','aunpcfht','aunpfict','aunpfbrt','aunpfswt','aunpfcit','aunpfc1t',
                                 'aunpcf1t','aunpcf2t','aunpcf3t','aunpcf4t','aunpfot','aunpfcst','aunpcfft',
                                 'aunpcfvt','aunpcfrt','auncftft','aunpcfgt','aunpffst','aunpfsat','aunpcfot',
                                 'aunpcfst','aunpc10t','aunpcbct','aunpcbjt','aunpcb7t','aunpcb8t','aunpcb9t',
                                 'aunpcbtt','aunpcajt','aunpcavt','aunpcakt','aunpckmt','aunpckwt','aunpckct',
                                 'aunpcaat','aunpcart','aunchrt','aunchet','aunchgt','aunchwt','aunchst','aunchct',
                                 'aunchmt','aunpcght','aunpcgft','aunpcgtt','aunpcgjt','aunpcgst','aunpcggt',
                                 'aunpcget','aunpcg1t','aunpcott','aunpcg3t','aunpcoct','aunpcoht','aunpcost',
                                 'auncmht','aunpcmqt','aunpc61t','aunpc6ct','aunpctmt','aunpctft','aunpctgt',
                                 'aunpctat','aunpctot','aunpctut','aunpccxt','aunpcctt','aunpccat','aunccamt',
                                 'auncrbkt','aunprnmt','auncrdat','aunpriat','aunprest','auncrgtt','auncrspt',
                                 'aunprost','aunprpst','aunpcrvt','aunpce1t','aunpce2t','aunpce3t','auncodlt',
                                 'aunpcozt','auncoist')) %>% l2w()

clean_names <- c('Dates','Milk','Cheese','Oth_dairy','Bread','Cakes','Bfast_cer','Oth_cer','Beef','Pork','Poultry',
                 'Lamb','Oth_meat','Fish','Fruit','Vegetables','Rest_meals','Takeout','Eggs','Jams','Condiments',
                 'Fats','Snacks','Oth_food','Coffee','Soft_drinks','Spirits','Wine','Beer','Tobacco','Cloth_men',
                 'Cloth_women','Cloth_children','Footwear_men','Footwear_women','Footwear_children','Accessories',
                 'Cloth_ser','Rents','Electr','Gas','Water_sewer','House_purch','Prop_rates','Oth_house','Furniture',
                 'Carpets','House_textiles','Maj_hh_apps','Small_hh_apps','HH_utensils','HH_tools',
                 'HH_cleaning','Toiletries','Oth_hh_items','Child_care','Hairdressing','Oth_hh_ser','Pharma',
                 'Therap_apps','Med_hosp_ser','Dental_ser','Mv','Auto_fuel','Mv_ser','Mv_parts','Oth_motor',
                 'Urb_trans','Postal_ser','Telecommunication','AV_equip','AV_ser','Books','Newspapers','Dom_travel',
                 'Intl_travel','Sports_eqip','Games','Sports_part','Oth_rec_ser','Pets','Vet_services',
                 'Prim_educ','Sec_educ','Tert_educ','Deposit_loan','Oth_fin_services','Insurance')

names(index_sa) <- clean_names
names(contrib_nsa) <- clean_names

#Total CPI measures
index_tot <- data_import("Haver",hvr_start="1993",hvr_end="2030",hvr_codes=c('AUNPC','AUSPC')) %>% l2w()

ratio_contrib_to_nsa <- function(x){x/index_tot$aunpc*100}

#Calculate effective weights
eff_weight <- cbind(contrib_nsa['Dates'],
                        apply(contrib_nsa[,-1],2,ratio_contrib_to_nsa))

#Data prior to 2011 - from ABS website
eff_2005 <- data_import('Excel',xl_file = 'pts_contrib 2005-2011.xls',xl_sheet='R_input') %>% l2w()

eff_2005 <- eff_2005[,clean_names]

#Splice into eff_weight dataframe
eff_weight[which(eff_weight$Dates==eff_2005$Dates[1]):which(eff_weight$Dates==eff_2005$Dates[nrow(eff_2005)]),] <- 
  eff_2005

#Load in data from GEM for estimation
setwd(desktop_path)
gem <- oxoedb(oe_db)
setwd(this_path)

#Inflation expectations estimate - may need to run this code to update
ie <- readRDS('S:/Economics/Subscriptions/Australia Macro Service/Charts/Inflation expectations/IE.rds') %>% 
  select(.,Dates,`Inflation expectations estimate`)
#--------------------------------------------------------------
#Estimation
#--------------------------------------------------------------

r <- select(gem[[1]] %>% l2w(),Dates,UP,NAIRU,PM,RX_RBA,PMG)

r$PM <- 1/r$PM*100 #Invert PM so it has the same relationship with inflation as TWI
r$PMG <- 1/r$PMG*100

r$ie <- NA

r$ie[r$Dates %in% ie$Dates] <- ie$`Inflation expectations estimate`

#Date to cut the GEM data
max_date <- index_sa[,1][max(which(!is.na(index_sa[,2])))]

gather <- list()

#Different specifications to loop through
for(gp_c in gp_opts){ #c('lin','nonlin')

  for(tp_c in tp_opts){ #c('XR','PM','PMG')

    for(sig_c in sig_opts){ #c(0.05,0.1)
      
      for(ie_c in ie_opts){ #c(ie,no_ie)


#Build a dataframe of regressors
r2 <- filter(r,Dates<=max_date,Dates>=index_sa$Dates[1]) 

if(gp_c=='lin'){
  r2 <- r2 %>%  mutate(.,ugap=(UP-NAIRU) %>% lag(.,1))
}else{
  r2 <- r2 %>%  mutate(.,ugap=(UP-NAIRU)/UP %>% lag(.,1))}

if(tp_c=='XR'){
  r2 <- r2 %>% mutate(.,dpm=RX_RBA/lag(RX_RBA,1)*100-100)
}else if(tp_c=='PM'){
  r2 <- r2 %>% mutate(.,dpm=PM/lag(PM,1)*100-100)
}else{
  r2 <- r2 %>% mutate(.,dpm=PMG/lag(PMG,1)*100-100)}

if(ie_c=='ie'){
  r2 <- r2 %>%  mutate(.,dpm1=lag(dpm,1),dpm2=lag(dpm,2),dpm3=lag(dpm,3)) %>% 
    select(.,Dates,ugap,dpm1,dpm2,dpm3,ie)
}else{
r2 <- r2 %>%  mutate(.,dpm1=lag(dpm,1),dpm2=lag(dpm,2),dpm3=lag(dpm,3)) %>% 
          select(.,Dates,ugap,dpm1,dpm2,dpm3)}

#Dummies
r2['d_gst1'] <- 0
r2['d_cvd1'] <- 0
r2['d_cvd2'] <- 0

r2['d_gst1'][r2['Dates']=='2000-09-01'] <- 1
r2['d_cvd1'][r2['Dates']=='2020-06-01'] <- 1
r2['d_cvd2'][r2['Dates']=='2020-09-01'] <- 1

#Run the estimation
results <- lapply(clean_names[-1],pc_est,index_sa)
names(results) <- clean_names[-1]

#Generate coefficient p values
pvals <- lapply(clean_names[-1],function(x,results) summary(results[[x]]),results)
names(pvals) <- clean_names[-1]

#Joint hypothesis testing
hyp_results <- lapply(clean_names[-1],hypotheses,pvals,results)
names(hyp_results) <- clean_names[-1]

#Dataframe that gives component categories
categories <- lapply(clean_names[-1],categorise_results,hyp_results,results,
                     sig_level=sig_c) %>% 
                     list.cbind() %>% as.data.frame()
names(categories) <- clean_names[-1]
rownames(categories) <- c('L_mkt','XR','Persistent','Transitory')

cats <- ct_cleanup(categories)

#Summary of the category weights
z <- eff_weight[year(eff_weight$Dates)==2019,-1] %>% colMeans()
category_weights <- mapply('*',categories,z) %>% rowSums

category_weights_detail <- mapply('*',categories,z)
category_weights_detail[2,]
category_weights_detail[3,]
category_weights_detail[4,]


names(category_weights) <- c('L_mkt','XR','Persistent','Transitory')
names(category_weights_detail) <- c('L_mkt','XR','Persistent','Transitory')
#--------------------------------------------------
# Calculating inflation series

ew <- eff_weight %>% drop_na()

# Scale effective weights in each category
ew_L_mkt <- scale_eff_weights(ew,'L_mkt')
ew_XR <- scale_eff_weights(ew,'XR')
ew_Persistent <- scale_eff_weights(ew,'Persistent')
ew_Transitory <- scale_eff_weights(ew,'Transitory')

# Year-ended inflation in each component
yy <- index_sa %>% w2l %>% growth(.,p=4) %>% l2w()

#Some assumptions made for missing data
yy$Deposit_loan[is.na(yy$Deposit_loan)] <- 0.3
yy$Oth_fin_services[is.na(yy$Oth_fin_services)] <- 1.2

yy <- yy[yy$Dates %in% ew$Dates,]

#Store created series in dataframe g
g <- ew['Dates']

g$L_mkt <- rowSums(ew_L_mkt[,-1]*yy[,-1])
g$XR <- rowSums(ew_XR[,-1]*yy[,-1])
g$Persistent <- rowSums(ew_Persistent[,-1]*yy[,-1])
g$Transitory <- rowSums(ew_Transitory[,-1]*yy[,-1])

#Charts

icchart <- ox_line_graph(g %>% w2l(),'Inflation components','% y/y',c(2002,2021),c(-4,10,2),
                         var_order = c("Persistent","Transitory","L_mkt","XR"),
                         leg = c('Persistent','Transitory','Labour sensitive','Exchange rate sensitive'),
                         leg_pos = c(0.4,0.9))
icchart



#LMKT chart

d <- cbind(g[c('Dates','L_mkt')],
      select(gem[[1]] %>% l2w(),Dates,ERI) %>% filter(.,Dates %in% g$Dates) %>% select(.,-Dates)) %>% w2l() %>% 
  growth(.,p=4,ivars='ERI')
lichart <- ox_line_graph(d,'Labour sensitive inflation','% y/y',c(2005,2022),c(0,8,2),leg = c("Labour sensitive", "Wages"))
lichart



#XR chart
d <- cbind(g[c('Dates','XR')],
           select(gem[[1]] %>% l2w(),Dates,PM,PMG,RX_RBA) %>% filter(.,Dates %in% g$Dates) %>% 
          select(.,-Dates)) %>% mutate(.,TWI=1/RX_RBA*100) %>% w2l() %>% growth(.,p=4,ivars=c('TWI','PM','PMG')) %>%
          trail_avg(.,ivars=c('TWI','PM','PMG')) %>% l2w() %>% drop_na()
  
d$TWI <- ((d$TWI-mean(d$TWI))/sd(d$TWI))*sd(d$XR)+mean(d$XR)
d$PM <- ((d$PM-mean(d$PM))/sd(d$PM))*sd(d$XR)+mean(d$XR)
d$PMG <- ((d$PMG-mean(d$PMG))/sd(d$PMG))*sd(d$XR)+mean(d$XR)

if(tp_c=='XR'){
xchart <- ox_line_graph(select(d,Dates,XR,TWI) %>% w2l(),'XR inflation','% y/y',c(2005,2021),c(-6,8,2))
}else if(tp_c=='PM'){
xchart <- ox_line_graph(select(d,Dates,XR,PM) %>% w2l(),'XR inflation','% y/y',c(2005,2021),c(-6,8,2))
}else{
xchart <- ox_line_graph(select(d,Dates,XR,PMG) %>% w2l(),'Exchange rate inflation','% y/y',c(2000,2021),c(-6,6,2),
                        leg = c("Exchange rate sensitive","Import prices"),x_seq = 2)
}

xchart


#Gather results and charts in a list - useful to compare different specifications
gather_name <- paste0(gp_c,tp_c,sig_c,ie_c)
tmp <- list(weights=category_weights,catgrs=cats,headline=icchart,lmkt=lichart,xr=xchart,res=results,p=pvals)
gather[[gather_name]] <- tmp

    }
  }
  }
}

#Render results
render('Inflation model results.R')
# shell.exec('Inflation-model-results.html')

#--------------------------------------------
#RB charts
g$Persistent[g$Dates=='2021-06-01'] <- NA

g$Persistent <- na.approx(g$Persistent)


ox_line_graph(g %>% w2l(),'Inflation components','% y/y',c(2002,2021),c(-4,10,2),
                         var_order = c("Persistent","Transitory","L_mkt","XR"),
                         leg = c('Acyclical - persistent','Acyclical - transitory','Procyclical','Exchange rate sensitive'),
                         leg_pos = c(0.4,0.9))

ggsave("Inflation_components.png",width=8.1,height=6.5)

ox_line_graph(g %>% w2l(),'Inflation components','% y/y',c(2010,2022),c(-4,10,2),
              var_order = c("Persistent","Transitory","L_mkt","XR"),
              leg = c('Acyclical - persistent','Acyclical - transitory','Procyclical','Exchange rate sensitive'),
              leg_pos = c(0.4,0.9))
ox_save('Inf_comp_short')

#LMKT chart

d <- cbind(g[c('Dates','L_mkt')],
           select(gem[[1]] %>% l2w(),Dates,ERI) %>% filter(.,Dates %in% g$Dates) %>% select(.,-Dates)) %>% w2l() %>% 
  growth(.,p=4,ivars='ERI')
ox_line_graph(d,'Procyclical inflation','% y/y',c(2005,2021),c(0,8,2),leg = c("Procyclical inflation", "WPI"),no_forc=1)

ggsave("Procyclical.png",width = 8.1, height = 6.5)


#XR chart
d <- cbind(g[c('Dates','XR')],
           select(gem[[1]] %>% l2w(),Dates,PM,PMG,RX_RBA) %>% filter(.,Dates %in% g$Dates) %>% 
             select(.,-Dates)) %>% mutate(.,TWI=1/RX_RBA*100) %>% w2l() %>% growth(.,p=4,ivars=c('TWI','PM','PMG')) %>%
  trail_avg(.,ivars=c('TWI','PM','PMG')) %>% l2w() %>% drop_na()

d$PMG <- ((d$PMG-mean(d$PMG))/sd(d$PMG))*sd(d$XR)+mean(d$XR)

ox_line_graph(select(d,Dates,XR,PMG) %>% w2l(),'Exchange rate sensitive inflation','% y/y',c(2000,2021),c(-6,6,2),
              leg = c("Exchange rate sensitive inflation","Import prices (rescaled)"),x_seq = 2,no_forc=1)

ox_save('XR')
# Persistent chart
j <- select(g,Dates,Persistent) %>% w2l()

k <- data_import('Haver',hvr_codes = 'AUNPCHU', hvr_start = 2000, hvr_end=2030) %>% growth(.,p=4) %>% 
  filter(.,Dates %in% j$Dates)

l <- select(yy,Dates, Insurance, Prop_rates, Telecommunication, Electr, Water_sewer) %>% w2l() %>% filter(.,Dates %in% j$Dates)



contribs <- cbind(g['Dates'],ew_Persistent[,-1]*yy[,-1])

l <- select(contribs,Dates, Insurance, Prop_rates, Telecommunication, Electr, Water_sewer) %>%
  w2l() %>% filter(.,Dates %in% j$Dates)

m <- rbind(j,l)

ox_dated_bar_line_graph(m,'Acyclical - persistent inflation','% y/y', c(2005,2025),c(-4,12,4),
                        colours = c(6,5,4,3,2,1),line_ser = 'Persistent',leg=c('Insurance',
                        'Property rates','Telecommunications','Electricity','Water & sewerage','Total'),leg_col=2)

# m1 <- mean(j$value)

# ox_line_graph(m,"Persistent inflation", "% y/y",c(2005,2025),c(-10,20,2),no_leg=0,
                         # hlines = m1, hlinestyle = 2,event = "Mean",eventdate = c('1/9/2005'),
              # event_ypos = m1+1,eventlinestyle = 0)

ggsave("persistent.png",width=8.1,height=6.5)

# Transitory chart

d <- cbind(g[c('Dates','Transitory')],yy['Auto_fuel'])
           # select(ew,Auto_fuel))
d$Auto_fuel <- ((d$Auto_fuel-mean(d$Auto_fuel))/sd(d$Auto_fuel))*sd(d$Transitory)+mean(d$Transitory)


ox_line_graph(d %>% w2l(),"Acyclical - transitory inflation","% y/y",c(2005,2025),c(-3,9,3),
                         leg = c("Acyclical - transitory inflation","Fuel prices (rescaled)"),leg_pos = c(0.5,0.9))

ggsave("transitory.png",width=8.1,height=6.5)

# Inflation
a <- select(gem[[1]] %>% l2w(),Dates,CPI, CPICORE) %>% w2l() %>% growth(.,p=4)
h_end <- gem[[2]]
ox_line_graph(a,"Inflation","% y/y",c(2005,2025),c(-1,6,1),leg=c('Headline inflation','Core inflation'),
                     hlines = c(2,3), hlinestyle = 2,eventdate = "1/1/2012",eventlinestyle = 0,event_ypos = 3.7,event = "RBA inflation target")

ggsave("cpi.png",width=8.1,height=6.5)

b <- select(gem[[1]] %>% l2w(),Dates, ERI, UP, NAIRU, CPI) %>% w2l() %>% growth(.,p=4,ivars=c('ERI','CPI')) %>% l2w()

b$UGAP <- b$NAIRU-b$UP

ox_line_graph(b %>% select(.,Dates, UGAP, ERI, CPI) %>% w2l(),
              'Labour market indicators & inflation','% y/y',c(2005,2026),c(-4,6,2),leg_pos=c(0.02,0.15),
              rhs_var='UGAP',rh_units='ppts',leg=c('Unemployment gap (inverted, RHS)','Wages','Headline inflation'))
ox_save('UGAP')

## management cost - biodiversity model
library(countrycode)
library(dplyr)
library(rvest)
library(reshape2)
library(mgcv)
library(visreg)
library(ggplot2)


dam<-read.csv('./data/damag.ex.or.csv')
mg<-read.csv('./data/manag.ex.or.csv')
dam$code<-countrycode(dam$Official_country,'country.name', 'iso3c')
mg$code<-countrycode(mg$Official_country,'country.name', 'iso3c')

sr<-read_html('https://rainforests.mongabay.com/03highest_biodiversity.htm')
sr_tab<-html_table(sr)[[1]]
sr_tab<-as.data.frame(sr_tab)
for (i in 2:7)
{
  sr_tab[,i]<-gsub(",", "",sr_tab[,i])
  
  sr_tab[,i]<-as.numeric(sr_tab[,i])
}
sr_tab$code<-countrycode(sr_tab$Country, 'country.name', 'iso3c')
sr_tab$tot_sr<-rowSums(sr_tab[,2:7], na.rm=T)

dam$TenYear<-dam$Impact_year%/%10*10
mg$TenYear<-mg$Impact_year%/%10*10
n_refs_dam<-dam%>%group_by(TenYear, code)%>%summarize_at("Reference_ID", n_distinct)
n_refs_mg<-mg%>%group_by(TenYear, code)%>%summarize_at("Reference_ID", n_distinct)
dam_sum<-dam%>%group_by(Species, TenYear, code)%>%summarise_at("cost.bil", sum) # not sure why a sum doesnt work in this line but i back out a sum from mean and length
dam_sum<-subset(dam_sum, is.na(dam_sum$code)==F)

mg_sum<-mg%>%group_by(Species, TenYear, code, Management_type)%>%summarise_at("cost.bil", sum) # not sure why a sum doesnt work in this line but i back out a sum from mean and length
mg_sum<-subset(mg_sum, is.na(mg_sum$code)==F)
mg_sum2<-dcast(mg_sum, cost.bil+Species+code+TenYear~Management_type,mean, na.rm=T)
combined<-merge(dam_sum, mg_sum2, by=c("Species", "code", "TenYear"), all=T)

combined<-combined[,c(1:3,4,6:9)]
colnames(combined)[c(4)]<-c( "damage_cost")

##add additional predictors
combined<-merge(combined, sr_tab, 'code', all.x=T)
socioeco_dat<-readRDS('./data/soc_econ_country.rds') # From Sardain, Leung et al. Nature Sustainability, GDP and population are already log transformed
socioeco_dat<-subset(socioeco_dat, yr==2014)
socioeco_dat<-subset(socioeco_dat, IHS.i%in%countrycode(unique(combined$code), 'iso3c', 'country.name'))
socioeco_dat<-subset(socioeco_dat, IHS.j%in%countrycode(unique(combined$code), 'iso3c', 'country.name'))
socioeco_dat$code<-socioeco_dat$IHS.j
trade_historical<-readRDS('./data/trade_historical_averaged.RDS')
trade<-readRDS('./data/trade_averaged.RDS')
trade<-colSums(trade)
trade<-cbind(trade, names(trade))
trade<-as.data.frame(trade)
colnames(trade)<-c('imports', 'code')
trade$imports<-as.numeric(trade$imports)
trade_historical<-cbind(colSums(trade_historical), (trade$code)[1:92])
colnames(trade_historical)<-c('imports_historical', 'code')
trade_historical<-as.data.frame(trade_historical)
trade_historical$imports<-as.numeric(trade_historical$imports)


##calculate mean lag per country

stwist<-read.table('./data/sTwist_database.csv', header=T)

colnames(stwist)[3]<-'Species'
colnames(stwist)[1]<-"Official_country"
stwist$eventDate<-gsub(";.*","",stwist$eventDate )
stwist$eventDate<-as.numeric(stwist$eventDate) # use minimum first record date

#Assign iso3c codes#
stwist$code<-countrycode(stwist$Official_country, 'country.name', 'iso3c')
stwist$TenYear<-signif(stwist$eventDate, digits=3)
stwist<-subset(stwist, eventDate>1500)#use post-colonial invasions
domesticated<-c('Felis catus', 'Canis lupus', 'Ovis aries', 'Camelus dromedarius','Sus scrofa','Equus caballus','Equus asinus', 'Mustela furo','Capra hircus')
for (dom in domesticated)
{
  stwist<-stwist[-grep(dom, stwist$scientificName),]
}
stwist_cont<-aggregate(locationID~code+Species+TenYear,data=stwist,FUN=length)
stwist_match<-subset(stwist_cont, Species%in%combined$Species)
stwist_cont<-stwist_cont %>% group_by(code, TenYear)%>%summarize_at('Species', length)
stwist_cont<-stwist_cont%>%group_by(code)%>%mutate(cum_species=cumsum(Species))

combined$mgmt_year<-0

for (i in 1:nrow(combined))
{
  relevant<-subset(combined, Species==combined$Species[i]&(combined$`Pre-invasion management`!=0|combined$`Post-invasion management`!=0))
  combined$mgmt_year[i]<-min(relevant$TenYear, na.rm=T)
}
combined$mgmt_year[which(is.infinite(combined$mgmt_year))]<-2017

##matched data
stwist_merge<-merge(combined, stwist_match, c('code', 'Species'), all.x=T)
stwist_merge$mgmt_delay<-stwist_merge$TenYear.x-stwist_merge$mgmt_year # time since management began (can be negative)
stwist_sum<-stwist_merge%>%group_by(code, TenYear.x)%>%summarize_at("mgmt_delay",mean, na.rm=T)                    

combined2<-merge(combined, stwist_cont, by=c("code", "TenYear"), all.x=T)
colnames(stwist_sum)[2]<-"TenYear"
combined3<-merge(combined2, stwist_sum, by=c("code", "TenYear"), all.x=T)
combined4<-combined3%>%group_by(code, TenYear)
combined4$`Post-invasion management`[which(is.nan(combined4$`Post-invasion management`))]<-0
combined4$`Pre-invasion management`[which(is.nan(combined4$`Pre-invasion management`))]<-0
combined4$`Knowledge funding`[which(is.nan(combined4$`Knowledge funding`))]<-0
combined4$`Mixed`[which(is.nan(combined4$`Mixed`))]<-0
combined4<-combined4%>%summarize_if(is.numeric, c(mean,sum), na.rm=T)
combined4$damage_cost<-combined4$damage_cost_fn2
combined4$Knowledge<-combined4$`Knowledge funding_fn2`
combined4$pre_inv<-combined4$`Pre-invasion management_fn2`
combined4$post_inv<-combined4$`Post-invasion management_fn2`
combined4$Knowledge[which(is.na(combined4$Knowledge)==T)]<-0
combined4$pre_inv[which(is.na(combined4$pre_inv)==T)]<-0
combined4$post_inv[which(is.na(combined4$post_inv)==T)]<-0
combined4$mgmt_delay_fn1[which(is.na(combined4$mgmt_delay_fn1))]<-combined4$TenYear[which(is.na(combined4$mgmt_delay_fn1))]-2017 # if management hadn't happened by 2017, take the delay from that year

combined4<-merge(combined4, trade, by="code", all.x=T)
combined4<-merge(combined4, trade_historical, by="code", all.x=T)
colnames(combined4)[39:40]<-c("imports", "imports_historical")
socioeco_dat$code<-countrycode(socioeco_dat$code, 'country.name', 'iso3c')
socioeco_dat<-socioeco_dat[,c(1,3,5,7,16,17)]
socioeco_dat<-unique.data.frame(socioeco_dat)
combined4<-merge(combined4, socioeco_dat, by="code", all.x=T)
combined4$imports[which(is.na(combined4$imports))]<-0
combined4$imports_historical[which(is.na(combined4$imports_historical))]<-0
combined4$imports_historical<-as.numeric(combined4$imports_historical)
combined4$GDP.j[which(is.na(combined4$GDP.j)==T)]<-mean(combined4$GDP.j, na.rm=T)
combined4$Pop.j[which(is.na(combined4$Pop.j)==T)]<-mean(combined4$Pop.j, na.rm=T)
combined4$mgmt_delay_fn1[which(is.infinite(combined4$mgmt_delay_fn1))]<-combined4$TenYear[which(is.infinite(combined4$mgmt_delay_fn1))]-2017
combined4$tot_sr_fn1[which(is.na(combined4$tot_sr_fn1))]<-0
n_refs_dam$damage_ref<-n_refs_dam$Reference_ID
n_refs_dam<-n_refs_dam[,-(3)]
n_refs_mg$mg_ref<-n_refs_mg$Reference_ID
n_refs_mg<-n_refs_mg[,-(3)]
combined4<-merge(combined4, n_refs_dam, all.x=T)
combined4<-merge(combined4, n_refs_mg, all.x=T)
combined4$mg_ref[which(is.na(combined4$mg_ref))]<-0
combined4$cum_species_fn1[which(is.na(combined4$cum_species_fn1))]<-0

combined4<-combined4[which(combined4$damage_cost>0),]
combined4$GDP.j<-exp(combined4$GDP.j)
combined4$Pop.j<-exp(combined4$Pop.j)

m<-gam(log(damage_cost+1)~log(Knowledge+1)+log(pre_inv+1)+log(post_inv+1)+log(cum_species_fn1+1)+log(tot_sr_fn1+1)+s(TenYear, k=5)+log(imports+1)+log(imports_historical+1)+log(GDP.j+1)+log(Pop.j+1)+s(mgmt_delay_fn1,k=5)+log(damage_ref+1), select=T, method="GCV.Cp", data=combined4)
(sum_m<-summary(m))
write.csv(sum_m$p.table,file="damage_results.csv")
write.csv(sum_m$s.table,file="damage_results_smooth.csv")

combined4$mgmt_spend<-combined4$pre_inv+combined4$post_inv



m2<-gam(log(mgmt_spend+1)~log(pre_inv+1)+log(Knowledge+1)+log(damage_cost+1)+log(cum_species_fn1+1)+log(imports+1)+log(imports_historical+1)+log(GDP.j+1)+log(Pop.j+1)+s(mgmt_delay_fn1, k=5)+log(tot_sr_fn1+1)+log(mg_ref+1)+s(TenYear, k=5), select=T, method="GCV.Cp", data=combined4)
(sum_m2<-summary(m2))

write.csv(sum_m2$p.table,file="mg_results.csv")
write.csv(sum_m2$s.table,file="mg_results_smooth.csv")


###plotting

#GAM
gam.check(m)
plot(m, all.terms = FALSE, ylab="TenYear", rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m)[1], residuals=TRUE)
plot(m, all.terms = FALSE, ylab="TenYear", rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m)[1], residuals=TRUE)

T4 <-visreg(m,  scale='response',"TenYear", line.par = list(col = 'black'), plot=TRUE, data=combined4,xlab="Decade", ylab="log(Damage cost+1)")
T5 <-visreg(m, scale='response', "damage_ref", line.par = list(col = 'black'), plot=TRUE,xlab="Number of damage cost references", ylab="log(Damage cost+1)")
T6 <-visreg(m,scale='response',"post_inv", line.par = list(col = 'black'), plot=TRUE,xlab="Post-invasion management spending", ylab="log(Damage cost+1)")
T7 <-visreg(m, scale='response', "Pop.j", line.par = list(col = 'black'), plot=TRUE,xlab="Population", ylab="log(Damage cost+1)")


##management plotting
gam.check(m2)
plot(m2, all.terms = FALSE, ylab="TenYear", rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m)[1], residuals=TRUE)
plot(m2, all.terms = FALSE, ylab="TenYear", rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, shift = coef(m)[1], residuals=TRUE)

T8 <-visreg(m2,  scale='response',"TenYear", line.par = list(col = 'black'), plot=TRUE, data=combined4,xlab="Decade", ylab="log(Management cost+1)")
T9 <-visreg(m2, scale='response', "mg_ref", line.par = list(col = 'black'), plot=TRUE,xlab="Number of management cost references", ylab="log(Management cost+1)")
T10 <-visreg(m2,scale='response',"pre_inv", line.par = list(col = 'black'), plot=TRUE,xlab="Pre-invasion management spending", ylab="log(Management cost+1)")
T11 <-visreg(m, scale='response', "Pop.j", line.par = list(col = 'black'), plot=TRUE,xlab="Damage cost spending", ylab="log(Management cost+1)")

hist(combined4$mgmt_delay_fn1, xlab="Mean management delay across countries and decades", main=NULL)
abline(v=0, col='red', lty=2)
plot(combined4$mgmt_delay_fn1~combined4$TenYear, xlab='Time', ylab="Management delay (years)", pch=19, col='darkgrey')
abline(h=0, col='red', lty=2)



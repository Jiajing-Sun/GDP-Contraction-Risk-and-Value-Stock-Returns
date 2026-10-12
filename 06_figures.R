library(ggplot2)
root <- normalizePath('.')
out <- file.path(root,'figures')
ragg::agg_png(file.path(out,'survey_timeline.png'),width=8,height=4.7,units='in',res=250,background='white')
par(mar=c(.3,.3,.3,.3),family='serif',cex=1)
plot.new();plot.window(xlim=c(0,100),ylim=c(0,100),xaxs='i',yaxs='i')
text(0,97,'A. Same forecast year and next-calendar-quarter returns',adj=c(0,1),font=2,cex=1.15)
arrows(2,69,99,69,length=.075,lwd=.8)
xx<-c(12,38,67)
segments(xx,67,xx,72,lwd=.7)
text(xx,78,c('February 9, 2024\nPrevious survey','May 6 / May 10, 2024\nDeadline / release','June 30\nQuarter-end'),cex=.95)
rect(76,65,99,88,lwd=.8)
text(87.5,79,'July–September\nThird-quarter returns',cex=.9)
text(4,60,'Both surveys forecast annual GDP growth in 2025',adj=c(0,1),cex=.88)
text(4,51,'Quarter-end controls precede returns; factors and industries are contemporaneous.',adj=c(0,1),cex=.78)
text(0,42,'B. Trading-day windows around publication',adj=c(0,0),font=2,cex=1.15)
text(0,34,'Day 0 is the release date or the first trading day thereafter.',adj=c(0,0),cex=.95)
labs<-c('Before release\n[−63, −2]','From release\n[0, 1]','After release\n[2, 21]','After release\n[22, 63]','After release\n[64, 126]','After release\n[127, 252]')
for(i in 1:6){x<-1+(i-1)*16.5;rect(x,12,x+15.5,28,lwd=.7);text(x+7.75,20,labs[i],cex=.88)}
text(0,3,'Schematic spacing. Calendar-quarter outcomes and trading-day windows are separate constructions.',adj=c(0,0),cex=.83)
dev.off()

m<-read.csv('results/model_estimates.csv')
pick<-function(f,label){d<-m[m$family==f & m$label==label,];stopifnot(nrow(d)==1);d}
z<-rbind(pick('characteristic_OP_paired_family','GV_OP_low_minus_high'),pick('weighting','small_VW'),pick('weighting','big_VW'),pick('europe_profitability_paired','S1_contrast'),pick('europe_profitability_paired','S2_contrast'))
groups<-c('US: all stocks','US: small stocks','US: large stocks','Europe: small stocks','Europe: large stocks')
d<-data.frame(group=factor(groups,levels=rev(groups)),estimate=z$coef,lo=z$ci95_low,hi=z$ci95_high)
write.csv(d,'results/figure2_data.csv',row.names=FALSE)
p<-ggplot(d,aes(estimate,group))+geom_vline(xintercept=0,linewidth=.3)+
 geom_errorbar(aes(xmin=lo,xmax=hi),orientation='y',width=.2,linewidth=.3)+geom_point(size=1.7)+
 scale_x_continuous(limits=c(-15,30),breaks=c(-10,0,10,20,30))+
 labs(x='Slope: return percentage points per 10 probability percentage points',y=NULL)+
 theme_classic(base_size=11,base_family='serif')+
 theme(axis.line.y=element_blank(),axis.ticks.y=element_blank(),axis.text.y=element_text(margin=margin(r=14)),axis.title.x=element_text(margin=margin(t=12)),plot.margin=margin(12,16,8,8))
ggsave(file.path(out,'us_europe_contrasts.png'),p,width=8.6,height=3.7,dpi=250,device=ragg::agg_png)

u<-read.csv('data/us_panel.csv')
d<-data.frame(quarter=u$quarter,rx_pp=10*resid(lm(matched_x~matched_mu,u)),rc=resid(lm(all_VW~matched_mu,u)))
stopifnot(nrow(d)==102)
b<-sum(d$rx_pp*d$rc)/sum(d$rx_pp^2)
write.csv(d,'results/figure3_data.csv',row.names=FALSE)
sel<-d[d$quarter %in% c('2008Q4','2020Q2','2001Q4','2022Q4','2025Q2'),]
sel$tx<-sel$rx_pp+.35;sel$ty<-sel$rc+2.2
lower <- sel$quarter %in% c('2008Q4','2020Q2')
sel$ty[lower] <- sel$rc[lower]-3.5
sel$tx[sel$quarter=='2025Q2']<-sel$rx_pp[sel$quarter=='2025Q2']-.8
sel$ty[sel$quarter=='2025Q2']<-sel$rc[sel$quarter=='2025Q2']-4
p<-ggplot(d,aes(rx_pp,rc))+geom_hline(yintercept=0,colour='grey75',linewidth=.3)+geom_vline(xintercept=0,colour='grey75',linewidth=.3)+
 geom_point(colour='grey50',alpha=.75,size=1.6)+geom_abline(intercept=0,slope=b,colour='#224C63',linewidth=.55)+
 geom_point(data=sel,colour='#224C63',size=1.8)+geom_text(data=sel,aes(x=tx,y=ty,label=quarter),hjust=0,size=3.2,colour='#224C63',family='serif')+
 labs(x='Probability revision after mean-growth control (percentage points)',y='Return contrast C after mean-growth control\n(percentage points)')+
 coord_cartesian(xlim=c(-7,33))+
 theme_minimal(base_size=11,base_family='serif')+
 theme(panel.grid.minor=element_blank(),axis.title.x=element_text(margin=margin(t=12)),axis.title.y=element_text(margin=margin(r=10)),plot.margin=margin(10,18,8,8))
ggsave(file.path(out,'conditional_return_relation.png'),p,width=8.6,height=5,dpi=250,device=ragg::agg_png)
cat('Three figures generated in R from reconstructed dates, outcomes and fitted estimates.\n')

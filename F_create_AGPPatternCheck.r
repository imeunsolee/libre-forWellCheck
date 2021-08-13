
##########################################
## create_AGPPatternCheck
## ---------------------------------------
## input 
## . data : 상위함수에서 가져옴
## . unit.glucose : 리브레 혈당 측정 단위 'mg.dl' 'mmol.mol'
## . Target : Target
## . TargetGoal : [[1]] 분석기간 [[2]] 분석일수 [[3]] CGM활성화비율 [[4]] eA1c %cv [5]][1]] TBR_lev2.Cut TBR_lev1.Cut TAR_lev1.Cut TAR_lev2.Cut [[5]][[2]] TBR_lev2.Goal TBR_lev1.Goal TAR_lev1.Goal TAR_lev2.Goal
## . mod : 1=단기간분석, 2=2+기간비교분석
## ---------------------------------------

create_AGPPatternCheck = function( data, unit.glucose='mg.dl', Target='T2DM', TargetGoal=TargetGoal, TIRstat=TIRstat, memberKey=memberKey, createdtime=createdtime, mod=mod ) {

	### step0 =============================================================================##

	## 분석환경셋팅
#	if ( mod==2 ) {
#		subP = sort(unique(data$sub))
#	} else {
		subP = 1 ## p=1일때만 분석하도록 /210714
#	}
	outFileNames = c()
	errCode.sub = c()

	## common theme ##

	subTitle.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0,vjust=0.5,col='#000000',
					fontsize=14.5,fontface='plain')
		)
	)
	remark.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0,vjust=0.5,col='#808285',
					fontsize=13.5,fontface='plain')
		)
	)

#	out.timeBlockAUC = vector('list',length(subP))
#	out.timeBlockVAR = vector('list',length(subP))


	### step1 =============================================================================##
	## 각 분석기간별 24h-AGP 그래프
	out.AGP = vector('list',length(subP))
#	break.tmp = GlucoseCut
#	break.tmp[3:4] = GlucoseCut[3:4] -1
	break.tmp = TargetGoal[[5]][[1]]
	
	## 15분단위 통계량 계산 -- 
	#. q0.05, q0.95, q0.25, q0.75, q0.5
	HMScut.15M = strptime(format(seq.POSIXt(strptime('00:00:00',format='%H:%M:%S'),strptime('23:59:59',format='%H:%M:%S'),by='15 min'),format='%H:%M:%S'),format='%H:%M:%S',tz='GMT')
	data$timef = as.POSIXct(strptime(data$time,format='%H:%M:%S'),tz='GMT')
#	data$timeCut.15M = NA
	data$HMSIdx.15M = NA
	for ( i in 1:nrow(data) ) data$HMSIdx.15M[i] = sum(round(data$timef[i],unit='min')>=HMScut.15M)
#	data$timef = NULL

	## 6시간단위 인덱스 --	
	HMScut.6H = as.POSIXct(strptime(c('04:00:00','10:00:00','16:00:00','22:00:00'),format='%H:%M:%S'),tz='GMT')
	data$HMSIdx.6H = NA
	for ( i in 1:nrow(data) ) data$HMSIdx.6H[i] = sum(round(data$timef[i],unit='min')>=HMScut.6H)+1 #1 : 야간 2 : 오전 3: 오후 4: 저녁 
	data$HMSIdx.6H = ifelse(data$HMSIdx.6H>length(HMScut.6H),(data$HMSIdx.6H-4),data$HMSIdx.6H) 
#	data$timef = NULL

	for ( p in subP ) {

		out.AGP[[p]] = vector('list',length=2)
		data.tmp = data[which(data$sub==p & data$log==1),]
		tmp = tapply(data.tmp$glucose,data.tmp$HMSIdx.15M,function(x) {quantile(x,probs=c(0.05,0.25,0.5,0.75,0.95))})

		out.AGP[[p]][[1]] = data.frame(matrix(unlist(tmp),nrow=length(tmp),ncol=5,byrow=T))
		RMIdx = setdiff(c(1:length(HMScut.15M)),as.numeric(as.vector(names(tmp))))

		if ( length(RMIdx)==0 ) {
			out.AGP[[p]][[1]]$time = as.POSIXct(HMScut.15M,format='%H:%M:%S',tz='GMT')

		} else if( length(RMIdx)>0 ) {
			out.AGP[[p]][[1]]$time = as.POSIXct(HMScut.15M[-RMIdx],format='%H:%M:%S',tz='GMT')
			addRows = data.frame(matrix(NA,nrow=length(RMIdx),ncol=5,byrow=T))
			addRows$time = as.POSIXct(HMScut.15M[RMIdx],format='%H:%M:%S',tz='GMT')
			out.AGP[[p]][[1]] = rbind(out.AGP[[p]][[1]],addRows)
		}
		colnames(out.AGP[[p]][[1]]) = c('Q05','Q25','Q50','Q75','Q95','time')
		out.AGP[[p]][[1]] = out.AGP[[p]][[1]][order(out.AGP[[p]][[1]]$time),]


		## smooth line -- 

		out.AGP[[p]][[1]]$index = 1:nrow(out.AGP[[p]][[1]])
		loessMod.Q05 = loess(Q05~index,data=out.AGP[[p]][[1]],span=0.1)
		out.AGP[[p]][[1]]$Q05.s = predict(loessMod.Q05,out.AGP[[p]][[1]]$index)
		loessMod.Q25 = loess(Q25~index,data=out.AGP[[p]][[1]],span=0.1)
		out.AGP[[p]][[1]]$Q25.s = predict(loessMod.Q25,out.AGP[[p]][[1]]$index)
		loessMod.Q50 = loess(Q50~index,data=out.AGP[[p]][[1]],span=0.1)
		out.AGP[[p]][[1]]$Q50.s = predict(loessMod.Q50,out.AGP[[p]][[1]]$index)
		loessMod.Q75 = loess(Q75~index,data=out.AGP[[p]][[1]],span=0.1)
		out.AGP[[p]][[1]]$Q75.s = predict(loessMod.Q75,out.AGP[[p]][[1]]$index)
		loessMod.Q95 = loess(Q95~index,data=out.AGP[[p]][[1]],span=0.1)
		out.AGP[[p]][[1]]$Q95.s = predict(loessMod.Q95,out.AGP[[p]][[1]]$index)

		addNextLine = out.AGP[[p]][[1]][1,]
		addNextLine$time = as.POSIXct(strptime(paste(Sys.Date()+1,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'))
		addNextLine$index = 97
		out.AGP[[p]][[1]] = rbind(out.AGP[[p]][[1]],addNextLine)

#		ymin.Target = GlucoseCut[2]
#		ymax.Target = (GlucoseCut[3]-1)
		ymin.Target = TargetGoal[[5]][[1]][2]
		ymax.Target = TargetGoal[[5]][[1]][3]
		ylim.tmp = ifelse(max(out.AGP[[p]][[1]]$Q95.s,na.rm=T)>350,ifelse(max(out.AGP[[p]][[1]]$Q95.s,na.rm=T)>400,ifelse(max(out.AGP[[p]][[1]]$Q95.s,na.rm=T)>450,500,450),400),350)

		out.AGP[[p]][[2]] = ggplot(out.AGP[[p]][[1]])+
			geom_vline(xintercept=as.POSIXct(strptime(c('03:00','06:00','09:00','15:00','18:00','21:00'),format='%H:%M'),tz='GMT'),color='#bcbec0',lty=2,size=0.2)+
			geom_vline(xintercept=as.POSIXct(strptime('12:00',format='%H:%M'),tz='GMT'),color='#bcbec0',size=0.3)+
			# Q05-95
			geom_line(aes(x=time,y=Q05.s),color='#a2b5d4',lty=2,size=0.5)+
			geom_line(aes(x=time,y=Q95.s),color='#a2b5d4',lty=2,size=0.5)+
			geom_ribbon(aes(x=time,ymin=Q05.s,ymax=Q95.s),fill='#dfe3ed',alpha=0.9)+
			# Q25-75
			geom_ribbon(aes(x=time,ymin=Q25.s,ymax=Q75.s),fill='#a2b5d4')+
			# median
			geom_line(aes(x=time,y=Q50.s),color='#1f50ce',size=0.8)+

			scale_y_continuous(name='',limits=c(0,ylim.tmp),breaks=setdiff(c(break.tmp,350,ylim.tmp),c(ymin.Target,ymax.Target)))+
			scale_x_datetime(name='',date_labels='%H:%M',
				breaks=seq.POSIXt(strptime(paste(Sys.Date(),'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),
					strptime(paste(Sys.Date()+1,'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT'),by='3 hours'))+
			annotate('text',x=as.POSIXct(strptime(paste(Sys.Date(),'00:00:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT')),y=(-5),label='00:00',parse=T)+
			geom_hline(yintercept=c(ymin.Target,ymax.Target),color='#40ac49')+
			coord_cartesian(ylim=c(0,ylim.tmp),xlim=range(out.AGP[[p]][[1]]$time),expand=F,clip='off')+
			theme(panel.background=element_rect(fill=NA,color=NA), panel.border=element_rect(colour='#231f20',fill=NA,size=0.4),
				axis.text.y=element_text(color='#231f20',size=8,face='bold'), axis.ticks.y=element_line(size=0.3),
				axis.text.x=element_text(color=c('#231f20',rep('#808285',3),'#231f20',rep('#808285',3),'#231f20'),size=8,hjust=0.5), axis.ticks.x=element_blank(),
				text=element_text(family='NotoSansCJKkrR'))

		## last value line ---
		lastIdx = ifelse(all(c(96,1) %in% setdiff(c(1:96),RMIdx)), 97, max(setdiff(c(1:96),RMIdx)))
		last_value = out.AGP[[p]][[1]][lastIdx,]
		last_date = strptime(paste(Sys.Date()+1,'0:30:00'),format='%Y-%m-%d %H:%M:%S',tz='GMT')

		Q25.ytmp = ifelse(last_value$Q25.s>(last_value$Q05.s+20),last_value$Q25.s,(last_value$Q05.s+20))
		Q50.ytmp = ifelse(last_value$Q50.s>(Q25.ytmp+20),last_value$Q50.s,(Q25.ytmp+20))
		Q75.ytmp = ifelse(last_value$Q75.s>(Q50.ytmp+20),last_value$Q75.s,(Q50.ytmp+20))
		Q95.ytmp = ifelse(last_value$Q95.s>(Q75.ytmp+20),last_value$Q95.s,(Q75.ytmp+20))

		out.AGP[[p]][[2]] = out.AGP[[p]][[2]]+theme(plot.margin=unit(c(1,1.5,0,0.5),'cm'))
		out.AGP[[p]][[2]] = out.AGP[[p]][[2]]+							
			annotation_custom(grob=textGrob(expression('95%'),gp=gpar(col='#bcbec0',fontsize=8,fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q95.ytmp,ymax=Q95.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lty=2,size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q95.s,ymax=Q95.ytmp)+
			annotation_custom(grob=textGrob(expression('75%'),gp=gpar(col='#a2b5d4',fontsize=8,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q75.ytmp,ymax=Q75.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q75.s,ymax=Q75.ytmp)+
			annotation_custom(grob=textGrob(expression('50%'),gp=gpar(col='#1f50ce',fontsize=8,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q50.ytmp,ymax=Q50.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#1f50ce',size=1)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q50.s,ymax=Q50.ytmp)+
			annotation_custom(grob=textGrob(expression('25%'),gp=gpar(col='#a2b5d4',fontsize=8,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q25.ytmp,ymax=Q25.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q25.s,ymax=Q25.ytmp)+
			annotation_custom(grob=textGrob(expression('5%'),gp=gpar(col='#bcbec0',fontsize=8,fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lty=2,size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+

			annotation_custom(grob=textGrob(ymin.Target,gp=gpar(col='#231f20',fontsize=8,fontface='plain',fontfamily='NotoSansCJKkrB')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),ymin=ymin.Target,ymax=ymin.Target)+ 
			annotation_custom(grob=textGrob(ymax.Target,gp=gpar(col='#231f20',fontsize=8,fontface='plain',fontfamily='NotoSansCJKkrB')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),ymin=ymax.Target,ymax=ymax.Target)+ 
			annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=0.8)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-45*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymin.Target,ymax=ymin.Target)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=0.8)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-45*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymax.Target,ymax=ymax.Target) 

	}


	### step2 =============================================================================##
	## 혈당 패턴
	out.PTRN = vector('list',3)
	out.PTRN.detail = vector('list',3)

	## 항목1. 저혈당에피소드 ---
	TBRCut = TargetGoal[[5]][[1]][2]
#	TBRCut = 80 #  test / todo / rm 
	TBR1.num = 1
#	data$glucose[1] = 75 # test / todo / rm 
	data$TBR1Idx = NA 

	for ( i in 1:nrow(data) ) {
		if ( !is.na(data$glucose[i]) ) {
			if ( data$glucose[i] < TBRCut ) {

				TBR1.num = ifelse( i>1 && data$TBR1Idx[i-1]==0, TBR1.num+1, ifelse( i>1 && as.numeric(data$dateandtime[i] - round(data$dateandtime[i-1],unit='min'))>20, TBR1.num+1, TBR1.num) )
				data$TBR1Idx[i] = TBR1.num

			} else {
				data$TBR1Idx[i] = 0
			}
		} else {
			next 
		}
	}

	## 저혈당 에피소드 Summary ##
	library(sqldf) # todo / -> 위치이동 F_Libre_x_DW_report_onW library에 추가하기 
	TBRevent = sqldf('select sub, TBR1Idx, min(dateandtime) as time_strt, max(dateandtime) as time_end, (max(dateandtime)-min(dateandtime))/60 as duration 
	from data where TBR1Idx>0 group by TBR1Idx')
	TBRevent$time_strt = as.POSIXct(TBRevent$time_strt,origin='1970-01-01',tz='GMT')
	TBRevent$time_end = as.POSIXct(TBRevent$time_end,origin='1970-01-01',tz='GMT')
#	data$TBR1dur = TBRevent[match(data$TBR1Idx,TBRevent$TBR1Idx),]$duration

#	TBR1Stat = sqldf('select sub, TBR1Idx, min(dateandtime) as time_strt, max(dateandtime) as time_end, (max(dateandtime)-min(dateandtime))/60 as duration, avg(glucose) as glucose_avg, min(glucose) as glucose_min from data where TBR1Idx>0 amd TBR1dur>=15 group by TBR1Idx')

	## 저혈당 에피소드 테이블 ##
	ptrn.Hypo = ifelse( dim(TBRevent)[1] > 0, 1, 0 ) # 1 yes 0 no
	out.PTRN[[1]] = data.frame(c1=c('저혈당에피소드',NA,NA))
    out.PTRN.detail[[1]] = data.frame(c1=c('','','',''))

	if ( ptrn.Hypo[1]==1 ) {

		out.PTRN[[1]][2,1] = paste('총 ',length(TBRevent[which(TBRevent$sub==1),]$TBR1Idx),'회 평균지속시간 ',round(mean(TBRevent[which(TBRevent$sub==1),]$duration),0),'분',sep='')
		ptrn.Hypo[2] = ifelse( sum(TIRstat[grepl('TBR',names(TIRstat))],na.rm=T) >= TargetGoal[[5]][[2]][2], 1, 0 ) # 1 bad 0 soso 
		out.PTRN[[1]][3,1] = '5백분위수 선이 낮은 혈당 목표 아래로 낮아지지 않도록 관리합니다.'#'10분위수 선이 매우 낮은 혈당 목표 아래로 낮아지지 않도록 관리합니다.'

	} else {

		out.PTRN[[1]][2,1] = '총 0회 관측'
		ptrn.Hypo[2] = NA 
		out.PTRN[[1]][3,1] = '5백분위수 선이 낮은 혈당 목표 아래로 낮아지지 않도록 관리합니다.'#'10분위수 선이 매우 낮은 혈당 목표 아래로 낮아지지 않도록 관리합니다.'

	}


	## 항목2. 롤러코스터패턴 ---
	# : 하루내 혈당패턴이 들쭉날쭉한 경우 = intra-days, within days variation이 크다 = 하루내 혈당변동이 크다 => flatter한 혈당변동을 목표로 조절해야함 # 
	x.hhmm = tapply(data[which(data$sub==1 & data$log==1),]$glucose,data[which(data$sub==1 & data$log==1),]$HMSIdx.15M,mean,na.rm=T)
	sd.hhmm = sd(x.hhmm,na.rm=T)
	ptrn.RollerCoaster = ifelse( sd.hhmm > 26, 1, 0 ) # 1 yes 0 no 
	out.PTRN[[2]] = data.frame(c1=c('24시간패턴 #intra-day'))
	out.PTRN.detail[[2]] = data.frame(c1=c('','','',''))

	if ( ptrn.RollerCoaster[1]==1 ) {

		out.PTRN[[2]][2,1] = '롤러코스터 패턴' 
		out.PTRN[[2]][3,1] = '24시간 혈당패턴이 높은 변동성을 보입니다. flatter한 혈당패턴이 되도록 관리해야합니다.'

		## 혈당변동을 일으키는 시간 구간 ---
		coef.w = rep(NA,96)
		p=1 ## 
		for ( i in 1:96 ) {
			timepoint = i:(i+1)
			timepoint = ifelse( timepoint<1, timepoint+96, ifelse( timepoint>96, timepoint-96, timepoint ))
			x.tmp = out.AGP[[p]][[1]][which(out.AGP[[p]][[1]]$index %in% timepoint),]$Q50
			t = c(1:length(timepoint))
			coef.w[i] = coef(lm(x.tmp~t))['t']
		}
		x.hhmm.sub = vector('list',4) # 4개 HMSIdx.6H
		sd.hhmm.sub = vector('list',4)
		for ( b in 1:4 ) {
			x.hhmm.sub[[b]] = tapply(data[which(data$sub==p & data$log==1 & data$HMSIdx.6H==b),]$glucose,data[which(data$sub==p & data$log==1 & data$HMSIdx.6H==b),]$HMSIdx.15M,mean)
			sd.hhmm.sub[[b]] = sd(x.hhmm.sub[[b]])
		}
		maxvar.intraday = max(unlist(sd.hhmm.sub))
		maxvar.intraday.Idx = which(sd.hhmm.sub==maxvar.intraday)
		
		if ( coef.w[which.max(coef.w)]>0 ) {
			faster.set = which(coef.w/15>=3)
			if ( length(faster.set)>0 ) {
				faster.time = format(HMScut.15M[faster.set],'%H시%M분')
				faster.adv = '급격한 '
			} else {
				faster.time = format(HSMcut.15M[which.max(coef.w)],'%H시%M분')
				faster.adv = ''
			}
			faster.time = paste(faster.time,collapse=',')
			out.PTRN.detail[[2]][3,1] = paste(faster.time,faster.adv,'혈당상승패턴')
		} else {
			out.PTRN.detail[[2]][3,1] = ''
		}

		if ( coef.w[which.min(coef.w)]<0 ) {
			faster.set = which(coef.w/15<=(-1))
			if ( length(faster.set)>0 ) {
				faster.time = format(HMScut.15M[faster.set],'%H시%M분')
				faster.adv = '급격한 '
			} else {
				faster.time = format(HMScut.15M[which.min(coef.w)],'%H시%M분')
				faster.adv = ''
			}
			faster.time = paste(faster.time,collapse=',')
			out.PTRN.detail[[2]][4,1] = paste(faster.time,faster.adv,'혈당하강패턴')
		} else {
			out.PTRN.detail[[2]][4,1] = ''
		}

		out.PTRN.detail[[2]][1,1] = '24시간패턴 점검'
		out.PTRN.detail[[2]][2,1] = paste(paste(c('야간','오전','오후','저녁')[maxvar.intraday.Idx],collapse=','),'시간 혈당변동패턴',sep='')

	} else {
		out.PTRN[[2]][2,1] = '롤러코스터 패턴이 없습니다.'
		out.PTRN[[2]][3,1] = '24시간 혈당패턴이 안정적인 변동성을 보입니다. flatter한 혈당패턴을 유지하도록 관리해야합니다.'
	}


	## 항목3. 풍선패턴 --- ## out.PTRN.detail[[3]] / todo 210719 
	# : inter-day 혈당변동
	sd.dm = sd(tapply(data[which(data$sub==1 & data$log==1),]$glucose,data[which(data$sub==1 & data$log==1),]$date,mean,na.rm=T))
	x.b.hhmm = tapply(data[which(data$sub==1 & data$log==1),]$glucose,data[which(data$sub==1 & data$log==1),]$HMSIdx.15M,sd,na.rm=T)
	sd.b.hhmm = mean(x.b.hhmm,na.rm=T)

	out.AGP[[p]][[1]]$IQR = out.AGP[[p]][[1]]$Q75 - out.AGP[[p]][[1]]$Q25
	out.AGP[[p]][[1]]$IDR = out.AGP[[p]][[1]]$Q95 - out.AGP[[p]][[1]]$Q05
	out.AGP[[p]][[1]]$diffIQRIDR = (out.AGP[[p]][[1]]$IDR - out.AGP[[p]][[1]]$IQR)

	ptrn.Ballooning = ifelse( sd.b.hhmm > 26, 2, ifelse( sd(x.b.hhmm,na.rm=T) > sd.b.hhmm/3, 1, 0 ) )
	out.PTRN[[3]] = data.frame(c1=c('일일간혈당패턴 #day-to-day #inter-day'))
	if ( ptrn.Ballooning>0 ) {
		out.PTRN[[3]][2,1] = '풍선 패턴'
		if ( ptrn.Ballooning==1 ) {
			out.PTRN[[3]][3,1] = '일일 혈당패턴이 높은 변동성을 보입니다. 풍선처럼 부푼 25-75백분위수 면적의 패턴이 줄어들도록 관리해야합니다.'
		} else if ( ptrn.Ballooning==2 ) {
			out.PTRN[[3]][3,1] = '일일 혈당패턴이 높은 변동성을 보입니다. 풍선처럼 부풀어오르는 패턴이 줄어들도록 관리해야합니다.'
		}

		### (detail 3-1) interVar이 가장 큰 시간 ---
		maxIQR.timeidx = which(out.AGP[[p]][[1]][1:96,]$IQR==max(out.AGP[[p]][[1]]$IQR,na.rm=T))
#		maxIQR.timepoint = format(HMScut.15M[maxIQR.timeidx],'%H시%M분')
		### (detail 3-2) interVar이 가장 큰 시간구간 ---
		### (detail 3-3) 혈당변동상승구간 --- 
		### (detail 3-4) 혈당변동상승구간 ---  


	} else {
		out.PTRN[[3]][2,1] = '풍선 패턴이 없습니다.'
		out.PTRN[[3]][3,1] = '일일 혈당패턴이 안정적인 변동성을 보입니다.'
	}


    ## 출력 테스트 ## 
    ptrn.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
        core = list(bg_params=list(fill=c('#F7F8F9','#FFF6F9')[rep(c(ptrn.Hypo[2],ptrn.Hypo[2],ptrn.Hypo[2]),each=3)+1],col=NA),
                    fg_params=list(hjust=0,x=0.01,vjust=c(0,0,0.7),
                        col=c(c('#000000',c('#000000','#FF2525')[ptrn.Hypo[2]+1],'#808285'),c('#000000',c('#000000','#FF2525')[ptrn.Hypo[2]+1],'#808285'),c('#000000'c('#000000','#FF2525')[ptrn.Hypo[2]+1],'#808285')),
                        fontsize=c(13.5,14,11),fontface=c('plain','bold','plain'))
        )
    )
    ptrn.theme_forWeb$core$fg_params$fontfamily = rep(c('NotoSansCJKkrR','NotoSansCJKkrB','NotoSansCJKkrR'),3)
    tab.test = tableGrob(rbind(out.PTRN[[1]],out.PTRN[[1]],out.PTRN[[1]]),theme=ptrn.theme_forWeb,cols=NULL,rows=NULL,
        widths=unit(360,'points'),heights=unit(rep(c(23,30,30),3),'points'))


	### step3 =============================================================================##
	## 출력

    LibreReport_AGP_forWeb = try(grid.arrange(grobs=list(tableGrob('24시간 연속 혈당 프로필 (AGP)',theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heighs=unit(26,'points')))),silent=T)


		## timeblock 정의 --
		HMScut.3B = strptime(paste(Sys.Date(),c('00:00:00','06:00:00','12:00:00')),format='%Y-%m-%d %H:%M:%S',tz='GMT')
		data$timeCut.3B = NA
		for( i in 1:nrow(data)) data$timeCut.3B[i] = sum(round(data$timef[i],unit='min')>=HMScut.3B)

		sd.t = sd(data[which(data$sub==p & data$log==1),]$glucose)


		## 항목1. 하루내 혈당변동성 ---
		# pattern : rollerCoaster 
		# : 하루내의 혈당패턴이 들쭉날쭉한 경우 : intra-days, within days, => 하루내 혈당변동이 크다 => flatter한 혈당변동을 목표로 조절해야한다.
		# : sd.hhmm 
		x.hhmm = tapply(data[which(data$sub==p & data$log==1),]$glucose,data[which(data$sub==p & data$log==1),]$timeCut.15M,mean)
		sd.hhmm = sd(x.hhmm)
#		avg.hhmm = mean(x.hhmm)
		# sd.w = mean(tapply(data[which(data$sub==p & data$log==1),]$glucose,data[which(data$sub==p & data$log==1),]$date,sd))
		# if (sd.hhmm > sd.w)
		# if (sd.hhmm > avg.hhmm/3 )
		# if (sd.hhmm > 26 )  : 정상적인 혈당변동성의 sd는 10-26이내이어야함에 따라 / ref. : in ternational consensus use of continuous glucose monitoring (diabetsjournals.org)
		rollerCoaster = vector('list',2)

		if( sd.hhmm > 26 ) {
			rollerCoaster[[1]] = 1
			rollerCoaster[[2]] = paste('롤러코스터패턴','24시간 혈당패턴이 높은 변동성을 보입니다. flatter한 24시간혈당패턴이 되도록 관리해주세요.',sep='') 
		} else {
			rollerCoaster[[1]] = 0
			rollerCoaster[[2]] = paste('24시간 혈당패턴이 낮은 변동성을 보입니다. 지금처럼 안정적인 24시간혈당 변동 패턴을 유지해주세요.',sep='')
		}

		## 항목2. 하루내 혈당변동을 일으키는 시간구간 ---

		rollerCoaster.sub = vector('list',3)
		# : 24시간혈당패턴선을 기준으로 기울기가 가장 높거나 작은 시간은?
	#	fit.tmp = loess(Q05~index,data=out.AGP[[p]][[1]],span=0.1)
		# : 15m 단위로 쪼개서 기울기 검색
		coef.w = rep(NA,96)
		for( i in 1:96 ) {
			timepoint = i:(i+1)
			timepoint = ifelse( timepoint<1, timepoint+96, ifelse( timepoint>96, timepoint-96, timepoint))
			x.tmp = out.AGP[[p]][[1]][which(out.AGP[[p]][[1]]$index %in% timepoint),]$Q50
			t = c(1:length(timepoint))
			coef.w[i] = coef(lm(x.tmp~t))['t']
		}


		if( rollerCoaster[[1]]==1 ) {
				
			x.hhmm.sub = vector('list',3)
			sd.hhmm.sub = vector('list',3)
			for( b in 1:3 ) {
				x.hhmm.sub[[b]] = tapply(data[which(data$sub==p & data$log==1 & data$timeCut.3B==b),]$glucose,data[which(data$sub==p & data$log==1 & data$timeCut.3B==b),]$timeCut.15M,mean)
				sd.hhmm.sub[[b]] = sd(x.hhmm.sub[[b]])
			}
			rollerCoaster.sub[[1]] = paste(c('야간시간','오전시간','활동시간')[which.max(sd.hhmm.sub)],'에 혈당패턴의 변동이 가장 크게 나타납니다.',sep='') # todo / 다 유사할 가능성도 고려해야하지않을까? 

			if( coef.w[which.max(coef.w)]>0 ) {
				faster.set = which(coef.w/15>=3)
				if( length(faster.set)>0 ) {
					faster.time = format(HMScut.15M[faster.set],'%H시%M분')
					faster.tmp = '급격한 '
				} else {
					faster.time = format(HMScut.15M[which.max(coef.w)],'%H시%M분')
					faster.tmp = ''
				}
	#			faster.tmp = ifelse(length(faster.set)>0, '급격한 ','')
				faster.time = paste(faster.time,collapse=',')
				rollerCoaster.sub[[2]] = paste(faster.time, faster.tmp,'혈당 상승 패턴을 보입니다.', sep='')
			} else {
				rollerCoaster.sub[[2]] = NA
			}

			if( coef.w[which.min(coef.w)]<0 ) {
				faster.set = which(coef.w/15<=(-1))
				if( length(faster.set)>0 ) {
					faster.time = format(HMScut.15M[faster.set],'%H시%M분')
					faster.tmp = '급격한 '
				} else {
					faster.time = format(HMScut.15M[which.max(coef.w)],'%H시%M분')
					faster.tmp = ''
				}
				faster.time = paste(faster.time,collapse=',')
				rollerCoaster.sub[[3]] = paste(faster.time,' 경 ', faster.tmp,'혈당 하강 패턴을 보입니다.', sep='')
			} else {
				rollerCoaster.sub[[3]] = NA 
			}

		}

# 		if( rollerCoaster[[1]]==0 ) { 
			# todo / 롤러코스터패턴이 아닐때에도 혈당상승하강패턴이나... 등등 알려줘야할 정보가 있을지 
#		}

		## 항목2. patterns of High/ Low 
#		patternsPeak = vector('list',2)
#		names(patternsPeak) = c('High','Low')


		## 항목3. Inter-day 혈당변동성 ---
		# pattern : ballooning
		# sd.b.hhmm 이 가장 클 때엔, IDR 이 큰 시점일 수 있음 (루틴한 패턴이 아닌 특별한 이벤트에 의한 경우가 있을 수 있음)
		sd.dm = sd(tapply(data[which(data$sub==p & data$log==1),]$glucose,data[which(data$sub==p & data$log==1),]$date,mean))
		x.b.hhmm = tapply(data[which(data$sub==p & data$log==1),]$glucose,data[which(data$sub==p & data$log==1),]$timeCut.15M,sd)
		sd.b.hhmm = mean(x.b.hhmm)		
 		
		out.AGP[[p]][[1]]$IQR = out.AGP[[p]][[1]]$Q75-out.AGP[[p]][[1]]$Q25
		out.AGP[[p]][[1]]$IDR = out.AGP[[p]][[1]]$Q95-out.AGP[[p]][[1]]$Q05
		out.AGP[[p]][[1]]$diffIQRIDR = (out.AGP[[p]][[1]]$IDR - out.AGP[[p]][[1]]$IQR)

		ballooning = vector('list',2)
# 		if( sd.b.hhnn > 26 ) # 이건 iqr 자체가 다 큰지 
#		if ( sd(x.b.hhmm) > sd.b.hhmm/3 ) # todo / sd > mean/3 이면 변동성이 크다는 것이 일반적인 것인지 확인 필요함 (이건 iqr이 작았다컸다라는 의미고)
		if ( sd(x.b.hhmm) > sd.b.hhmm/3 ) {
			ballooning[[1]] = 1
			ballooning[[2]] = paste('풍선패턴 ','일일 혈당 패턴이 높은 변동성을 보입니다. 풍선처럼 부풀어오르는 패턴이 줄어들도록 관리해주세요.',sep='') # 특정시간대에 혈당변동성이 높은 경우 (IQR기준)
		} else if( sd.b.hhmm > 26 ) {
			ballooning[[1]] = 2
			ballooning[[2]] = paste('풍선패턴 ','일일 혈당 패턴이 높은 변동성을 보입니다. 풍선처럼 부푼 푸른색 면적의 패턴이 줄어들도록 관리해주세요.',sep='') # 하루 종일 혈당 변동성이 높은 경우 (IQR기준) # todo / 멘트변경필요
		} else {
			ballooning[[1]] = 0 
			ballooning[[2]] = paste('칭찬코멘트주기',sep='') # todo
		}


		## 항목4. day-to-day간 혈당변동을 일으키는 시간구간 ---
		ballooning.sub = vector('list',4)
		# 1: (IQR) interVar 이 가장 큰 timepoint
		# 2: (IQR) interVar 이 큰 timeblock
		# 3: (IQR) 혈당변동상승 구간
		# 4: (IDR) interVar 이 가장 큰 timepoint (less IQR)
		
		if( ballooning[[1]]==1 ) {
			
			### 1: (IQR) interVar 이 가장 큰 timepoint
#			ballooning.sub[[1]] = format(HMScut.15M[which.max(out.AGP[[p]][[1]][1:96,]$IQR)],'%H시%M분')
			maxIQR.time = which.max(out.AGP[[p]][[1]][1:96,]$IQR)
			ballooning.sub[[1]] = format(HMScut.15M[maxIQR.time],'%H시%M분')

			### 3: (IQR) 혈당변동상승 구간
			coef.sub = rep(NA,96)
			for( i in 1:96 ) {
				timepoint = (i-1):(i+1) # 30min단위
				t = c(1:length(timepoint))
				timepoint = ifelse( timepoint<1, timepoint+96, ifelse( timepoint>96, timepoint-96, timepoint))
				x.sub = out.AGP[[p]][[1]][which(out.AGP[[p]][[1]]$index%in%timepoint),]$IQR
				coef.sub[i] = coef(lm(x.sub~t))['t']
			}

#			ballooning.strt = format(HMScut.15M[max(which(coef.sub[1:(maxIQR.time-1)]<0))+1],'%H시%M분') #혈당변동상승 시작구간
#			ballooning.end = format(HMScut.15M[c((maxIQR.time+1):96)[which(coef.sub[(maxIQR.time+1):96]>0)[1]]],'%H시%M분') #혈당변동상승 마지막구간 
#			ballooning.sub[[3]] = paste(ballooning.strt,'-',ballooning.end,' 사이 일일 혈당 패턴의 변화가 크게 나타납니다. ','일상패턴 중 혈당변화를 발생시키는 습관이 있는지 되짚어보세요.', sep='')			
			ballooning.strt = max(which(coef.sub[1:(maxIQR.time-1)]<0))+1 #혈당변동상승 시작구간
			ballooning.end = c((maxIQR.time+1):96)[which(coef.sub[(maxIQR.time+1):96]>0)[1]] #혈당변동상승 마지막구간 
			ballooning.sub[[3]] = paste(format(HMScut.15M[ballooning.strt],'%H시%M분'),'-',format(HMScut.15M[ballooning.end],'%H시%M분'),' 사이 일일 혈당 패턴의 변화가 크게 나타납니다. ','일상패턴 중 혈당변화를 발생시키는 습관이 있는지 되짚어보세요.', sep='')

			### 4: (IDR) interVar 이 가장 큰 timepoint
			lessIQR.time = which(out.AGP[[p]][[1]]$IQR < mean(out.AGP[[p]][[1]]$IQR))
			maxIDR.time = lessIQR.time[which.max(out.AGP[[p]][[1]][lessIQR.time,]$IDR)]
			ballooning.sub[[4]] = paste(format(HMScut.15M[maxIDR.time],'%H시%M분'),'경 일상적이지 않은 불규칙하거나 특별한 이벤트에 의해 혈당 변화가 크게 나타납니다. ','분석기간 중 특별한 일이 있었는지 되짚어보세요.', sep='') 


		} 

		if( ballooning[[1]]==2 ) {
			
			### 1: (IQR) interVar 이 가장 큰 timepoint
#			ballooning.sub[[1]] = format(HMScut.15M[which.max(out.AGP[[p]][[1]][1:96,]$IQR)],'%H시%M분')
			maxIQR.time = which.max(out.AGP[[p]][[1]][1:96,]$IQR)
			ballooning.sub[[1]] = format(HMScut.15M[maxIQR.time],'%H시%M분')

			### 2: (IQR) interVar 이 큰 timeblock
			x.b.hhmm.sub = vector('list',3)
			sd.b.hhmm.sub = vector('list',3)
			for( b in 1:3 ) {
				x.b.hhmm.sub[[b]] = tapply(data[which(data$sub==p & data$log==1 & data$timeCut.3B==b),]$glucose,data[which(data$sub==p & data$log==1 & data$timeCut.3B==b),]$timeCut.15M,sd)
				sd.b.hhmm.sub[[b]] = mean(x.b.hhmm.sub[[b]])
			}
			ballooning.sub[[2]] = paste(c('야간시간','오전시간','활동시간')[which.max(sd.b.hhmm.sub)],'에 일일 혈당 변동이 큰 편입니다.',sep='') # todo / 다 유사할 가능성 고려  

			### 4: (IDR) interVar 이 가장 큰 timepoint
			lessIQR.time = which(out.AGP[[p]][[1]]$IQR < mean(out.AGP[[p]][[1]]$IQR))
			maxIDR.time = lessIQR.time[which.max(out.AGP[[p]][[1]][lessIQR.time,]$IDR)]
			ballooning.sub[[4]] = paste(format(HMScut.15M[maxIDR.time],'%H시%M분'),'경 일상적이지 않은 불규칙하거나 특별한 이벤트에 의해 혈당 변화가 크게 나타납니다. ','분석기간 중 특별한 일이 있었는지 되짚어보세요.', sep='') 


		} else if ( ballooning[[1]]==0 ) {
	
			### 4: (IDR) interVar 이 가장 큰 timepoint
			lessIQR.time = which(out.AGP[[p]][[1]]$IQR < mean(out.AGP[[p]][[1]]$IQR))
			maxIDR.time = lessIQR.time[which.max(out.AGP[[p]][[1]][lessIQR.time,]$IDR)]
			ballooning.sub[[4]] = paste(format(HMScut.15M[maxIDR.time],'%H시%M분'),'경 일상적이지 않은 불규칙하거나 특별한 이벤트에 의해 혈당 변화가 크게 나타납니다. ','분석기간 중 특별한 일이 있었는지 되짚어보세요.', sep='') 
		
			ballooning.sub[[4]] = paste(c('야간시간','오전시간','활동시간')[which.max(sd.b.hhmm.sub)],'에 일일 혈당 변동이 큰 편입니다.',sep='') # todo / 다 유사할 가능성 고려  

		}

	}

	
	### step3 =============================================================================##
	## 각 분석기간별 24h-AGP 그래프 에 추가 

	for( p in subP ) {

		## maxIQR.time 라인 추가 
		maxIQR.set = data.frame(time = rep(out.AGP[[p]][[1]][out.AGP[[p]][[1]]$index==maxIQR.time,]$time,2),
								glucose = as.numeric(as.vector(out.AGP[[p]][[1]][out.AGP[[p]][[1]]$index==maxIQR.time,c('Q25.s','Q75.s')])))

		out.AGP[[p]][[2]] = out.AGP[[p]][[2]] + 
							annotate('segment',x=maxIQR.set$time[1],xend=maxIQR.set$time[1],y=maxIQR.set$glucose[1],yend=maxIQR.set$glucose[2], alpha=0.6,colour='#ff5454',size=1.3)+
							annotate('text',x=maxIQR.set$time[1],y=maxIQR.set$glucose[1]-3,label='more variability',col='#333333',fontsize=8.5,fontface='bold',fontfamily='KRfontname',hjust='left')
							
		
		## maxIDR.time 라인 추가 
		maxIDR.set = data.frame(time = rep(out.AGP[[p]][[1]][out.AGP[[p]][[1]]$index==maxIDR.time,]$time,2),
								glucose = as.numeric(as.vector(out.AGP[[p]][[1]][out.AGP[[p]][[1]]$index==maxIDR.time,c('Q05.s','Q95.s')])))
		out.AGP[[p]][[2]] = out.AGP[[p]][[2]] + 
							annotate('segment',x=maxIDR.set$time[1],xend=maxIDR.set$time[1],y=maxIDR.set$glucose[1],yend=maxIDR.set$glucose[2], alpha=0.6,colour='#ff5454',size=1.3)+
							annotate('text',x=maxIDR.set$time[1],y=maxIDR.set$glucose[1]-3,label='more variability',col='#333333',fontsize=8.5,fontface='bold',fontfamily='KRfontname',hjust='left')

		## ballooning.sub[[3]]
#		out.AGP[[p]][[2]] = out.AGP[[p]][[2]] + 
#							annotate('rect',xmin=out.AGP[[p]][[1]][out.AGP[[p]][[1]]$index==ballooning.strt,]$time,xmax=out.AGP[[p]][[1]][out.AGP[[p]][[1]]$index==ballooning.end,]$time,ymin=0,ymax=ylim.tmp,color='#ff5454',fill=NA,lty=2,lwd=1.6)

		## 

	}


        ## TimeBlock 별 분석 --- 

		data.tmp = data[which(data$sub==p & data$log==1),]

		## timeblock ---
		HMScut.3B = strptime(paste(Sys.Date(),c('00:00:00','06:00:00','12:00:00')),format='%Y-%m-%d %H:%M:%S',tz='GMT')
		data.tmp$timef = as.POSIXct(strptime(data.tmp$time,format='%H:%M:%S'),tz='GMT')
		data.tmp$timeCut.3B = NA 
		for( i in 1:nrow(data.tmp) ) data.tmp$timeCut.3B[i] = sum(round(data.tmp$timef[i],unit='min')>=HMScut.3B)

		
        out.timeBlockAUC[[p]] = AUCcalculation(x=data.tmp$glucose,group=data.tmp$timeCut.3B,cut=TAR_lev1.Cut)
        names(out.timeBlockAUC[[p]]) = c('sleep','wakeup','active')
        out.timeBlockVAR[[p]] = glycemicVariability(x=data.tmp$glucose,group=data.tmp$timeCut.3B,method='%cv')$GV
        names(out.timeBlockVAR[[p]]) = c('sleep','wakeup','active')

        timeBlock_stat = vector('list',3)
        timeBlock_stat.comment = vector('list',3)

        timeBlock_stat[[1]] = AUCcalculation(x=data.tmp$glucose,group=data.tmp$timeCut.3B,cut=TAR_lev1.Cut)
        timeBlock_stat[[2]] = glycemicVariability(x=data.tmp$glucose,group=data.tmp$timeCut.3B,method='%cv')
#        names(timeBlock_stat[[1]]) = names(timeBlock_stat[[2]][[1]]) = c('sleep','wakeup','active')

        if( max(timeBlock_stat[[1]])>0 ) {
            timeBlock_stat.comment[[1]] = paste('시간구간 중 ', c('취침시간','기상시간','활동시간')[which.max(timeBlock_stat[[1]])],'에 ',
             '혈당높음 수준에 해당되는 혈당곡선 밑 면적이 가장 넓습니다.',sep='')
        } else {
            timeBlock_stat.comment[[1]] = '연속 혈당이 모두 혈당높음 수준 미만으로 유지되었습니다.'
        } 
        if( max(timeBlock_stat[[2]]$GV)> timeBlock_stat[[2]]$GV.assesment ) {
            timeBlock_stat.comment[[2]] = paste('시간구간 중 ', c('취침시간','기상시간','활동시간')[which.max(timeBlock_stat[[2]])],'에 ',
            '혈당 변동성이 가장 높으나, 이는 안정적인 수준의 변동성이라고 볼 수 있습니다.',sep='')
        } else {
            timeBlock_stat.comment[[2]] = paste('시간구간 중 ', c('취침시간','기상시간','활동시간')[which.max(timeBlock_stat[[1]])],'에 ',
            '혈당 변동성이 가장 높으며, 이는 권장되는 변동성 수준보다 높아 혈당을 안정화시키기 위한 관리가 필요합니다.',sep='')
        }

        timeBlock_stat.comment

        timeBlock_stat[[3]] = tapply(data.tmp$glucose,data.tmp$timeCut.3B, mean, na.rm=T)

        timeBlock_stat[[4]] = vector('numeric',length(HMScut.3B)) 
        for( j in 1:length(HMScut.3B) ) {
            x.tmp = tapply(data.tmp[which(data.tmp$timeCut.3B==j),]$glucose, data.tmp[which(data.tmp$timeCut.3B==j),]$timeCut.15M, median, na.rm=T)
#            timeBlock_stat[[4]][j] = sd(x.tmp)
            print(paste('--',j,'--'))
            print(sd(x.tmp))
            print(sd(x.tmp)/mean(x.tmp)*100)
        }



    }
	## function list ##
	AUCcalculation = function(x, group, cut) {

		x = ifelse(x<cut,cut,x)
		areaSum = tapply(x,group,function(x){ sum(x-cut) })
		groupN = tapply(x,group,function(x){ sum(!is.na(x)) })
		areaAvg = areaSum/groupN

		max.group = names(areaAvg)[which.max(areaAvg)]

		return( areaAvg ) 
	}

	glycemicVariability = function(x, group, method) {

		if(method=='%cv'){
			GV = tapply(x, group, function(x){ sd(x)/mean(x) *100 }) 
			GV.assesment = 36
		} else if(method=='sd'){
			GV = tapply(x, group, function(x){ sd(x) })
			GV.assesment = tapply(x, group, function(x){ mean(x)/3 })
		}
		return(list(GV=GV,GV.assesment=GV.assesment))
	}

}
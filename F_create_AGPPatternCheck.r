
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

		out.AGP[[p]] = vector('list',length=4)
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

		## for Web
		out.AGP[[p]][[3]] = out.AGP[[p]][[2]]+							
			annotation_custom(grob=textGrob(expression('95%'),gp=gpar(col='#bcbec0',fontsize=12,fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q95.ytmp,ymax=Q95.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lty=2,size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q95.s,ymax=Q95.ytmp)+
			annotation_custom(grob=textGrob(expression('75%'),gp=gpar(col='#a2b5d4',fontsize=12,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q75.ytmp,ymax=Q75.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q75.s,ymax=Q75.ytmp)+
			annotation_custom(grob=textGrob(expression('50%'),gp=gpar(col='#1f50ce',fontsize=12,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q50.ytmp,ymax=Q50.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#1f50ce',size=1)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q50.s,ymax=Q50.ytmp)+
			annotation_custom(grob=textGrob(expression('25%'),gp=gpar(col='#a2b5d4',fontsize=12,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q25.ytmp,ymax=Q25.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q25.s,ymax=Q25.ytmp)+
			annotation_custom(grob=textGrob(expression('5%'),gp=gpar(col='#bcbec0',fontsize=12,fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lty=2,size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+

			annotation_custom(grob=textGrob(ymin.Target,gp=gpar(col='#231f20',fontsize=13,fontface='plain',fontfamily='NotoSansCJKkrB')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),ymin=ymin.Target,ymax=ymin.Target)+ 
			annotation_custom(grob=textGrob(ymax.Target,gp=gpar(col='#231f20',fontsize=13,fontface='plain',fontfamily='NotoSansCJKkrB')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),ymin=ymax.Target,ymax=ymax.Target)+ 
			annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=0.8)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-45*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymin.Target,ymax=ymin.Target)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=0.8)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-45*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymax.Target,ymax=ymax.Target)


		## for App

#		out.AGP[[p]][[3]]$theme$text$size # null 로 나옴 
		out.AGP[[p]][[4]] = out.AGP[[p]][[2]]+							
			annotation_custom(grob=textGrob(expression('95%'),gp=gpar(col='#bcbec0',fontsize=18,fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q95.ytmp,ymax=Q95.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lty=2,size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q95.s,ymax=Q95.ytmp)+
			annotation_custom(grob=textGrob(expression('75%'),gp=gpar(col='#a2b5d4',fontsize=18,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q75.ytmp,ymax=Q75.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q75.s,ymax=Q75.ytmp)+
			annotation_custom(grob=textGrob(expression('50%'),gp=gpar(col='#1f50ce',fontsize=18,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q50.ytmp,ymax=Q50.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#1f50ce',size=1)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q50.s,ymax=Q50.ytmp)+
			annotation_custom(grob=textGrob(expression('25%'),gp=gpar(col='#a2b5d4',fontsize=18,fontface='bold',fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=Q25.ytmp,ymax=Q25.ytmp)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#a2b5d4',size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q25.s,ymax=Q25.ytmp)+
			annotation_custom(grob=textGrob(expression('5%'),gp=gpar(col='#bcbec0',fontsize=18,fontfamily='NotoSansCJKkrR')),xmin=last_date+20*60,xmax=last_date+20*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#bcbec0',lty=2,size=0.8)),xmin=last_date-30*60,xmax=last_date-10*60,ymin=last_value$Q05.s,ymax=last_value$Q05.s)+

			annotation_custom(grob=textGrob(ymin.Target,gp=gpar(col='#231f20',fontsize=20,fontface='plain',fontfamily='NotoSansCJKkrB')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),ymin=ymin.Target,ymax=ymin.Target)+ 
			annotation_custom(grob=textGrob(ymax.Target,gp=gpar(col='#231f20',fontsize=20,fontface='plain',fontfamily='NotoSansCJKkrB')),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),xmax=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-75*60),ymin=ymax.Target,ymax=ymax.Target)+ 
			annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=0.8)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-45*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymin.Target,ymax=ymin.Target)+
			annotation_custom(grob=linesGrob(gp=gpar(col='#40ac49',lwd=0.8)),xmin=(as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT')-45*60),xmax=as.POSIXct(strptime('00:00',format='%H:%M'),tz='GMT'),ymin=ymax.Target,ymax=ymax.Target) 

	}


	### step2 =============================================================================##
	## 혈당 패턴
	step2.YN=F
	if ( step2.YN==F ) { # 임시 # 

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
                        col=c(c('#000000',c('#000000','#FF2525')[ptrn.Hypo[2]+1],'#808285'),c('#000000',c('#000000','#FF2525')[ptrn.Hypo[2]+1],'#808285'),c('#000000',c('#000000','#FF2525')[ptrn.Hypo[2]+1],'#808285')),
                        fontsize=c(13.5,14,11),fontface=c('plain','bold','plain'))
        )
    )
    ptrn.theme_forWeb$core$fg_params$fontfamily = rep(c('NotoSansCJKkrR','NotoSansCJKkrB','NotoSansCJKkrR'),3)
    tab.test = tableGrob(rbind(out.PTRN[[1]],out.PTRN[[1]],out.PTRN[[1]]),theme=ptrn.theme_forWeb,cols=NULL,rows=NULL,
        widths=unit(360,'points'),heights=unit(rep(c(23,30,30),3),'points'))


	} ## 임시 



	### output =============================================================================##

	AGPPlot.Remark1_forWeb = data.frame(c1='■',c2='25~75백분위수 면적은 일상적인 생활패턴에 의해 나타납니다.')
	AGPPlot.Remark2_forWeb = data.frame(c1='■',c2='5~95백분위수 면적은 어느 특별한 이벤트에 의해 나타납니다.')
	
	remark1.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=c(0.3,0),vjust=0.5,col=c('#a2b5d4','#808285'),
					fontsize=12,fontface='plain')
		)
	)
	remark2.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=c(0.3,0),vjust=0.5,col=c('#dfe3ed','#808285'),
					fontsize=12,fontface='plain')
		)
	)

	CairoPNG(filename=paste(memberKey,createdtime,'Web_AGP.png',sep='_'),family='NotoSansCJKkrR',scale=1/0.32,bg='white',width=1168,height=1100,unit='px',dpi=96)

	LibreReport_AGP_forWeb = try(grid.arrange(grobs=list(tableGrob('24시간 연속 혈당 프로필 (AGP)',theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(26,'points')),
		tableGrob('AGP는 보고서 기간의 혈당 값을 요약한 것으로, 중앙값(50%) 및 기타 백분위수가 하루에 발생한 것처럼 함께 표시됩니다.',theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(26,'points')),
		tableGrob(AGPPlot.Remark1_forWeb,theme=remark1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(24,852),'points'),heights=unit(21,'points')),
		tableGrob(AGPPlot.Remark2_forWeb,theme=remark2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(24,852),'points'),heights=unit(21,'points')),
		out.AGP[[1]][[3]]),
		nrow=5,ncol=2,layout_matrix=rbind(c(1,1),c(2,2),c(3,3),c(4,4),c(5,5)),
		widths=unit(c(24,852),'points'),heights=unit(c(26,26,21,21,276),'points')),silent=T)

	dev.off()

	if ( class(LibreReport_AGP_forWeb)[1]!='try-error' ) {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_AGP.png',sep='_'))
	} else {
		errCode.sub = c(errCode.sub,'Errr_todo')
	}


	Result = vector('list',2)
	names(Result) = c('outFileNames','errCode.sub')

	if ( is.null(outFileNames) ) outFileNames = NA 
	if ( is.null(errCode.sub) ) errCode.sub = NA   

	Result[[1]] = outFileNames
	Result[[2]] = errCode.sub

	return( Result )

}
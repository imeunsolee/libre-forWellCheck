
##########################################
## create_GlucoseStat
## ---------------------------------------
## input 
## . data : 상위함수에서 가져옴
## . mod : 1=단기간분석, 2=2+기간비교분석 (초)
## . unit.glucose : 리브레 혈당 측정 단위 'mg.dl' 'mmol.mol'
## . GlucoseCut : vector('numeric',4)  c('너무낮음기준값;0미만', '낮음기준값;0미만','높음기준값;0이상','너무높음기준값;0이상')
## . Target='T2DM' option : T2DM T1DM T1DMinPr T2DMinPr GDM HighRiskDM preDM
## . GlucoseCut.goal=c(0.01,0.04,0.7,0.25,0.05) 
## ---------------------------------------

create_GlucoseStat = function( data, unit.glucose='mg.dl', Target='T2DM', memberKey=memberKey, createdtime=createdtime, mod=mod, TIR.detail.opt=3,
	TIR.Goal=NULL,TBR_lev2.Cut=NULL,TBR_lev2.Goal=NULL,TBR_lev1.Cut=NULL,TBR_lev1.Goal=NULL,TAR_lev1.Cut=NULL,TAR_lev1.Goal=NULL,TAR_lev2.Cut=NULL,TAR_lev2.Goal=NULL) {

	### output 
	##1 통계분석 - 평균혈당, GMI or eA1c, CV or other variation stat., std of glucose, (1기간, 2기간..)
	##2 범위내시간 - 평균혈당, GMI or eA1c, CV or other variation stat., std of glucose, (1기간, 2기간..)
	##(3) 과거결과비교

	# 1기간 vs. 2기간의 평균혈당 변화량, 변화차이검정, 
	# 1기간 vs. 2기간의 eA1c 변화량, 변화차이검정
	# 2기간으로부터 이전 3개월간의 모든 데이터를 활용한 eA1c 계산

	### step0 =============================================================================##
	## 분석환경셋팅
#	subP = sort(unique(data$sub)) # mod==1이면 무조건 1로 설정해야하는지는, AGPdata에서 mod==1이면 sub = 1 만 배정하는지 여부에 따라 설정

	if ( mod==2 ) {
		subP = sort(unique(data$sub))
	} else {
		subP = 1
	}
	outFileNames = c()
	errCode.sub = c()

	## common theme ##

	subTitle.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill='#000000',col=NA),
					fg_params=list(hjust=0,x=0,vjust=0.5,col='#ffffff',
					fontsize=14.5,fontface='plain')
		)
	)
	remark.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0,vjust=0.5,col='#808285',
					fontsize=13.5,fontface='plain')
		)
	)

	subTitle.theme_forApp = ttheme_minimal(base_family='NotoSansCJKkrB', 
		core = list(bg_params=list(fill='#000000',col=NA),
					fg_params=list(hjust=0,x=0,col='#ffffff',
					fontsize=25,fontface='bold'))
	)
	remark.theme_forApp = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0,vjust=0.5,col='#808285',
					fontsize=17,fontface='plain'))
	)	


	### step1 =============================================================================##
	## out.Goal : 기본정보 ## todo / 후순위
	out.Goal = vector('list',5)

	## 항목1. 분석기간 ---
	out.Goal[[1]] = range(data[which(data$sub==1),]$date)
	out.Goal[[2]] = length(seq.Date(out.Goal[[1]][1],out.Goal[[1]][2],by=1))

	## 항목2. CGM활성화비율 ---
	# 1-2초씩 엇갈리는 경우가 있어 dateandtime 분단위로 조정
	AllTimeCut = as.POSIXct(seq.POSIXt(min(data[which(data$sub==1),]$dateandtime),max(data[which(data$sub==1),]$dateandtime),by='15 min'),format='%Y-%m-%d %H:%M:%S')
	data.tmp = data[which(data$sub==1),]
	CGMactive = rep(NA,length(AllTimeCut))
#	HMScut.6H = c('00:00:00','06:00:00','12:00:00','18:00:00') # todo / 시간확정 (210708)
#	HMSIdx.6H = rep(NA,length(AllTimeCut))
	for ( i in 1:length(AllTimeCut) ) {
		if( i != length(AllTimeCut) ) {
			CGMactive[i] = ifelse(any(AllTimeCut[i]<= round(data.tmp$dateandtime,unit='min') & AllTimeCut[i+1]> round(data.tmp$dateandtime,unit='min')),1,0)
#			HMSIdx.6H[i] = ifelse(format(AllTimeCut[i],'%H:%M:%S')<HMScut.6H[2],0,1)+ifelse(format(AllTimeCut[i],'%H:%M:%S')<HMScut.6H[3],0,1)+ifelse(format(AllTimeCut[i],'%H:%M:%S')<HMScut.6H[4],0,1)+1
		} else {
			CGMactive[i] = ifelse(any(AllTimeCut[i]<= round(data.tmp$dateandtime,unit='min')),1,0)
#			HMSIdx.6H[i] = ifelse(format(AllTimeCut[i],'%H:%M:%S')<HMScut.6H[2],0,1)+ifelse(format(AllTimeCut[i],'%H:%M:%S')<HMScut.6H[3],0,1)+ifelse(format(AllTimeCut[i],'%H:%M:%S')<HMScut.6H[4],0,1)+1
		}
	}
	out.Goal[[3]] = sum(CGMactive)/length(CGMactive)*100
	
	HMScut.6H = as.POSIXct(strptime(c('04:00:00','10:00:00','16:00:00','22:00:00'),format='%H:%M:%S'),tz='GMT')
	data$HMSIdx.6H = NA
	data$timef = as.POSIXct(strptime(data$time,format='%H:%M:%S'),tz='GMT')
	for ( i in 1:nrow(data) ) data$HMSIdx.6H[i] = sum(round(data$timef[i],unit='min')>=HMScut.6H)+1 #1 : 야간 2 : 오전 3: 오후 4: 저녁 
	data$HMSIdx.6H = ifelse(data$HMSIdx.6H>length(HMScut.6H),(data$HMSIdx.6H-4),data$HMSIdx.6H) 
	data$timef = NULL

	## 항목3. 혈당목표 ---
	out.Goal[[4]] = vector('numeric',2)
	out.Goal[[5]] = vector('list',2)

	if ( Target=='HighRiskDM' ){
		Target.text = '당뇨고위험군'
		out.Goal[[4]][1] = 7.5
		out.Goal[[4]][2] = 36

		PPG.L = 100; PPG.U = 180
		TIR.Goal = 50; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = NA
		TAR_lev2.Goal = 10; TAR_lev1.Goal = 50; TBR_lev1.Goal = 1; TBR_lev2.Goal = NA

	} else if ( Target=='T1DM' ) {
		Target.text = '1형 당뇨'
		out.Goal[[4]][1] = 7
		out.Goal[[4]][2] = 36

		PPG.L = 100; PPG.U = 180
		TIR.Goal = 70; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 5; TAR_lev1.Goal = 25; TBR_lev1.Goal = 4; TBR_lev2.Goal = 1	

	} else if ( Target=='T2DM') {
		Target.text = '2형 당뇨'
		out.Goal[[4]][1] = 6.5
		out.Goal[[4]][2] = 36

		PPG.L = 100; PPG.U = 180
		TIR.Goal = 70; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 5; TAR_lev1.Goal = 25; TBR_lev1.Goal = 4; TBR_lev2.Goal = 1	

	} else if ( Target %in% c('T1GDM')) {
		Target.text = '1혈 당뇨 임산부'            
		out.Goal[[4]][1] = 6
		out.Goal[[4]][2] = 36

		PPG.L = 100; PPG.U = 120
		TIR.Goal = 70; 
		TAR_lev2.Cut = NA; TAR_lev1.Cut = 140; TBR_lev1.Cut = 63; TBR_lev2.Cut = 54
		TAR_lev2.Goal = NA; TAR_lev1.Goal = 25; TBR_lev1.Goal = 4; TBR_lev2.Goal = 1	

	} else if ( Target %in% c('T2GDM')) {
		Target.text = '2형 당뇨 임산부'         
		out.Goal[[4]][1] = 6
		out.Goal[[4]][2] = 36

		PPG.L = 100; PPG.U = 120
		TIR.Goal = 85; 
		TAR_lev2.Cut = NA; TAR_lev1.Cut = 140; TBR_lev1.Cut = 63; TBR_lev2.Cut = 54
		TAR_lev2.Goal = NA; TAR_lev1.Goal = 10; TBR_lev1.Goal = 4; TBR_lev2.Goal = 0	

	} else if ( Target %in% c('GDM')) {            
		Target.text = '임신성 당뇨'
		out.Goal[[4]][1] = 6
		out.Goal[[4]][2] = 36

		PPG.L = 100; PPG.U = 120
		TIR.Goal = 85; 
		TAR_lev2.Cut = NA; TAR_lev1.Cut = 140; TBR_lev1.Cut = 63; TBR_lev2.Cut = 54
		TAR_lev2.Goal = NA; TAR_lev1.Goal = 10; TBR_lev1.Goal = 4; TBR_lev2.Goal = 0	

	} else if ( Target=='preDM' ) {
		Target = '당뇨전단계'
		out.Goal[[4]][1] = 5.601
		out.Goal[[4]][2] = 33

		PPG.L = 100; PPG.U = 140
		TIR.Goal = 90; 
		TAR_lev2.Cut = 250; TAR_lev1.Cut = 180; TBR_lev1.Cut = 70; TBR_lev2.Cut = 54
		TAR_lev2.Goal = 0; TAR_lev1.Goal = 6; TBR_lev1.Goal = 4; TBR_lev2.Goal = 0	

	}

	GlucoseCut = rep(NA,4)
	GlucoseCut.goal = rep(NA,5)

	GlucoseCut[1] = TBR_lev2.Cut#ifelse(!is.null(TBR_lev2.Cut),TBR_lev2.Cut,TBR_lev2.Cut.tmp) 
	GlucoseCut[2] = TBR_lev1.Cut#ifelse(!is.null(TBR_lev1.Cut),TBR_lev1.Cut,TBR_lev1.Cut.tmp) 
	GlucoseCut[3] = TAR_lev1.Cut+1#ifelse(!is.null(TAR_lev1.Cut),TAR_lev1.Cut,TAR_lev1.Cut.tmp)+1 
	GlucoseCut[4] = TAR_lev2.Cut+1#ifelse(!is.null(TAR_lev2.Cut),TAR_lev2.Cut,TAR_lev2.Cut.tmp)+1
	GlucoseCut.goal[1] = TBR_lev2.Goal/100#ifelse(!is.null(TBR_lev2.Goal),TBR_lev2.Goal,TBR_lev2.Goal.tmp)/100
	GlucoseCut.goal[2] = TBR_lev1.Goal/100#ifelse(!is.null(TBR_lev1.Goal),TBR_lev1.Goal,TBR_lev1.Goal.tmp)/100
	GlucoseCut.goal[3] = TIR.Goal/100#ifelse(!is.null(TIR.Goal),TIR.Goal,TIR.Goal.tmp)/100
	GlucoseCut.goal[4] = TAR_lev1.Goal/100#ifelse(!is.null(TAR_lev1.Goal),TAR_lev1.Goal,TAR_lev1.Goal.tmp)/100
	GlucoseCut.goal[5] = TAR_lev2.Goal/100#ifelse(!is.null(TAR_lev2.Goal),TAR_lev2.Goal,TAR_lev2.Goal.tmp)/100

	out.Goal[[5]][[1]] = c(GlucoseCut[1], GlucoseCut[2], GlucoseCut[3]-1, GlucoseCut[4]-1)
	out.Goal[[5]][[2]] = c(GlucoseCut.goal[1], GlucoseCut.goal[2], GlucoseCut.goal[3], GlucoseCut.goal[4], GlucoseCut.goal[5])*100

	## TIR 그래프용 라벨 생성 
	lab.add = c()
	for ( i in 1:5 ) {
		if ( !is.na(GlucoseCut[i]) ) {
			if ( GlucoseCut[i]==min(GlucoseCut,na.rm=T) ) {
				lab.add[i] = paste('＜',GlucoseCut[i],' mg/dL',sep='')
			} else {
				lab.add[i] = paste(GlucoseCut[i-1],'-',(GlucoseCut[i]-1),'mg/dL')
			}			
		} else if ( is.na(GlucoseCut[i]) ) {
			if ( (i-1) %in% c(0,which(is.na(GlucoseCut))) ) {
				lab.add[i] = NA 
			}
			if ( GlucoseCut[i-1]==max(GlucoseCut,na.rm=T) ) {
				lab.add[i] = paste('＞',(GlucoseCut[i-1]-1),' mg/dL',sep='')
			}
			
		}
	}


	### step2 =============================================================================##
	## out.STAT : 혈당통계 ##
	out.STAT = vector('list',length(subP))

	for ( p in subP ) {

		## 항목1. 평균혈당 ---
		out.STAT[[p]][1] = mean(data[which(data$sub==p & data$log==1),]$glucose,na.rm=T)

		## 항목2. 혈당관리표시 GMI or 예측당화혈색소 eA1c ---
		eA1ceqn = function(x,unit.glucose) {
			if(unit.glucose=='mg.dl') {
				y = (x+46.7)/28.7 
			} else if(unit.glucose=='mmol.mol'){
				y = (x+2.59)/1.59 
			}
			return(y)
		}
		GMIeqn = function(x,unit.glucose) {
			if(unit.glucose=='mg.dl'){
				y = 3.31 + 0.02392*x 
			} else if(unit.glucose=='mmol.mol'){
				y = 12.71 + 4.70587*x
			}
			return(y)
		}
		out.STAT[[p]][2] = GMIeqn(x=mean(data[which(data$sub==p & data$log==1),]$glucose,na.rm=T),unit.glucose=unit.glucose)#GMI
#		out.STAT[[p]][2] = eA1ceqn(x=mean(data[which(data$sub==p & data$log==1),]$glucose,na.rm=T),unit.glucose=unit.glucose)#eA1c

		## 항목3. 혈당변동성 - 혈당변동계수 %CV ---
		glycemicVariability = function(x,method) {
			if(method=='%cv'){
				GV = sd(x)/mean(x) *100 
				GV.assesment = 36
			} else if(method=='sd'){
				GV = sd(x)
				GV.assesment = mean(x)/3
			}
			return(list(GV=GV,GV.assesment=GV.assesment))
		}
		out.STAT[[p]][3] = glycemicVariability(x=data[which(data$sub==p & data$log==1),]$glucose,method='%cv')$GV

		## 항목4. 혈당변동성 - 혈당표준편차  ---
		out.STAT[[p]][4] = glycemicVariability(x=data[which(data$sub==p & data$log==1),]$glucose,method='sd')$GV

	}


	## 혈당통계 결과 출력 ## 
	statTab_avg_a = data.frame(matrix('',nrow=2,ncol=6))
	statTab_avg_b = data.frame(matrix('',nrow=1,ncol=3))
	statTab_gmi_a = data.frame(matrix('',nrow=2,ncol=6))
	statTab_gmi_b = data.frame(matrix('',nrow=1,ncol=3))
	statTab_cv_a = data.frame(matrix('',nrow=2,ncol=6))
	statTab_cv_b = data.frame(matrix('',nrow=1,ncol=3))
	statTab_sd_a = data.frame(matrix('',nrow=2,ncol=6))
	statTab_sd_b = data.frame(matrix('',nrow=1,ncol=3))


	statTab_avg_a[1,2] = '평균 혈당'
	statTab_gmi_a[1,2] = '혈당관리표시'
	statTab_gmi_a[2,2] = 'GMI(단일)'
	statTab_cv_a[1,2] = '혈당변동계수'
	statTab_cv_a[2,2] = 'cv%'
	statTab_sd_a[1,2] = '혈당표준편차'

	statTab_avg_a[,4] = c('이번',round(out.STAT[[1]][1],0))
	statTab_gmi_a[,4] = c('이번',round(out.STAT[[1]][2],1))
	statTab_cv_a[,4] = c('이번',round(out.STAT[[1]][3],1))
	statTab_sd_a[,4] = c('이번',round(out.STAT[[1]][4],0))
	
	statTab_avg_a[2,5] = ' mg/dL'
	statTab_gmi_a[2,5] = ' %'
	statTab_cv_a[2,5] = ' %'
	statTab_sd_a[2,5] = ' mg/dL'

	
	## 목표값 달성 여부 체크 ## 
	statTab_avg_b[2] = '88-116 이내 권장합니다.'
	statTab_sd_b[2] = '10-26 이내 권장합니다.'
	if ( out.STAT[[1]][2]>=out.Goal[[4]][1] ) {
		statTab_gmi.bgcol = '#FFF6F9'
		statTab_gmi_a.ftcol = '#FF2525'
		statTab_gmi_b.ftcol = '#122747'
		statTab_gmi_b[2] = paste(out.Goal[[4]][1],'% 미만으로 줄여야합니다.',sep='')
	} else {
		statTab_gmi.bgcol = '#F7F8F9'
		statTab_gmi_a.ftcol = '#122747'
		statTab_gmi_b.ftcol = '#808285'
		statTab_gmi_b[2] = paste(out.Goal[[4]][1],'% 미만 달성했습니다.',sep='')
	}
	if ( out.STAT[[1]][3]>out.Goal[[4]][2] ) {
		statTab_cv.bgcol = '#FFF6F9'
		statTab_cv_a.ftcol = '#FF2525'
		statTab_cv_b.ftcol = '#122747'
		statTab_cv_b[2] = paste(out.Goal[[4]][2],'% 이하로 줄여야합니다.',sep='')
	} else {
		statTab_cv.bgcol = '#F7F8F9'
		statTab_cv_a.ftcol = '#122747'
		statTab_cv_b.ftcol = '#808285'
		statTab_cv_b[2] = paste(out.Goal[[4]][2],'% 이하 달성했습니다.',sep='')
	}


	## 과거 내역 비교 ## 
	if ( mod==2 ) {
		
		STAT.subTitle.remark = paste('과거 분석기간: ',format(min(data$date[data$sub==2]),'%Y년 %m월 %d일'),'-',format(max(data$date[data$sub==2]),'%Y년 %m월 %d일'),sep='')
		statTab_avg_a[,3] = c('과거',round(out.STAT[[2]][1],0))
		statTab_gmi_a[,3] = c('과거',round(out.STAT[[2]][2],1))
		statTab_cv_a[,3] = c('과거',round(out.STAT[[2]][3],1))
		statTab_sd_a[,3] = c('과거',round(out.STAT[[2]][4],0))

		statTab_avg_a[1,6] = '비교'
		statTab_avg_a[2,6] = ifelse( out.STAT[[1]][1]<out.STAT[[2]][1],
									paste('▼ ',round(out.STAT[[2]][1]-out.STAT[[1]][1],0),sep=''),
									paste('▲ ',round(out.STAT[[1]][1]-out.STAT[[2]][1],0),sep=''))

		statTab_gmi_a[1,6] = '비교'
		statTab_gmi_a[2,6] = ifelse( out.STAT[[1]][2]<out.STAT[[2]][2],
									paste('▼ ',round(out.STAT[[2]][2]-out.STAT[[1]][2],1),sep=''),
									paste('▲ ',round(out.STAT[[1]][2]-out.STAT[[2]][2],1),sep=''))

		statTab_ea1c_a = data.frame(matrix('',nrow=2,ncol=1))
		statTab_ea1c_b = data.frame(matrix('',nrow=1,ncol=2))
		statTab_ea1c_a[1,1] = '예측당화혈색소(3개월누적)'
		statTab_ea1c_a[2,1] = format(out.Goal[[1]][2],'%Y년 %m월 %d일 기준')
		eA1c.range = sort(seq(max(data[which(data$sub==1),]$date), by='-89 day', length=2)) # 90일전 
		statTab_ea1c_b[1,1] = round(eA1ceqn(x=mean(data[which(data$date>=eA1c.range[1] & data$date<=eA1c.range[2] & data$log==1),]$glucose,na.rm=T),unit.glucose=unit.glucose),1)
		if ( statTab_ea1c_b[1,1] >= out.Goal[[4]][1] ) {
			statTab_ea1c.bgcol = '#FFF6F9'
			statTab_ea1c.ftcol = '#FF2525'
		} else {
			statTab_ea1c.bgcol = '#F7F8F9'
			statTab_ea1c.ftcol = '#122747'			
		}
		statTab_ea1c_b[1,2] = ' %'

	} else {

		STAT.subTitle.remark = ''

		statTab_ea1c_a = data.frame(matrix('',nrow=2,ncol=1))
		statTab_ea1c_b = data.frame(matrix('',nrow=1,ncol=2))
		statTab_ea1c_a[1,1] = '예측당화혈색소(3개월누적)'
		statTab_ea1c_a[2,1] = format(out.Goal[[1]][2],'%Y년 %m월 %d일 기준')
		statTab_ea1c_b[1,1] = round(eA1ceqn(x=mean(data[which(data$sub==1 & data$log==1),]$glucose,na.rm=T),unit.glucose=unit.glucose),1)

		if ( statTab_ea1c_b[1,1] >= out.Goal[[4]][1] ) {
			statTab_ea1c.bgcol = '#FFF6F9'
			statTab_ea1c.ftcol = '#FF2525'
		} else {
			statTab_ea1c.bgcol = '#F7F8F9'
			statTab_ea1c.ftcol = '#122747'			
		}
		statTab_ea1c_b[1,2] = ' %'

	}

	## 출력 ##
	## TABLE binding : 혈당통계표 ## 
	# theme # 
	statTab_a.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill='#F7F8F9',col=NA),
					fg_params=list(hjust=rep(c(0,0,0,0,0,0),each=2),x=rep(c(0.01,0.01,0.01,0.01,0.01,0.01),each=2),vjust=rep(c(0.5,0.8),6),col='#122747',
					fontsize=c(c(1,1),c(13.5,13),c(12,14),c(12,16),c(12,10),c(12,14))))
	)
	statTab_a.theme_forWeb$core$fg_params$fontfamily = rep('NotoSansCJKkrR',12)
	statTab_a.theme_forWeb$core$fg_params$fontfamily[c(3,4,8,12)] = 'NotoSansCJKkrB'
	statTab_a.theme_forWeb$core$fg_params$col = rep('#122747',12)

	statTab_b.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill='#F7F8F9',col=NA),
					fg_params=list(hjust=0,x=0,vjust=0.1,col='#808285',fontsize=11,fontface='plain'))
	)

	statTab_a.theme_forApp = statTab_a.theme_forWeb
	statTab_a.theme_forApp$core$fg_params$fontsize = c(1,1,23,21,19,23,19,25,19,15,19,23)
	statTab_b.theme_forApp = statTab_b.theme_forWeb
	statTab_b.theme_forApp$core$fg_params$fontsize = 15

	# TABLE # 
	statTab_a.W_forWeb = c(5,133,60,60,80,65)
	statTab_a.W_forApp = c(10,170,85,70,90,87)
	statTab_a.H_forWeb = c(33,26)
	statTab_a.H_forApp = c(45,42)

	statTab_b.W_forWeb = c(138,200,65)
	statTab_b.H_forWeb = 26
	statTab_b.W_forApp = c(180,245,87)
	statTab_b.H_forApp = 42
	
	out.Web.statTab_avg_a = tableGrob(statTab_avg_a,theme=statTab_a.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forWeb,'points'),heights=unit(statTab_a.H_forWeb,'points'))

	out.App.statTab_avg_a = tableGrob(statTab_avg_a,theme=statTab_a.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forApp,'points'),heights=unit(statTab_a.H_forApp,'points'))

	out.Web.statTab_avg_b = tableGrob(statTab_avg_b,theme=statTab_b.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forWeb,'points'),heights=unit(statTab_b.H_forWeb,'points'))

	out.App.statTab_avg_b = tableGrob(statTab_avg_b,theme=statTab_b.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forApp,'points'),heights=unit(statTab_b.H_forApp,'points'))

	statTab_a.theme_forWeb$core$bg_params$fill = statTab_gmi.bgcol
	statTab_a.theme_forWeb$core$fg_params$col[8] = statTab_gmi_a.ftcol
	out.Web.statTab_gmi_a = tableGrob(statTab_gmi_a,theme=statTab_a.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forWeb,'points'),heights=unit(statTab_a.H_forWeb,'points'))

	statTab_a.theme_forApp$core$bg_params$fill = statTab_gmi.bgcol
	statTab_a.theme_forApp$core$fg_params$col[8] = statTab_gmi_a.ftcol
	out.App.statTab_gmi_a = tableGrob(statTab_gmi_a,theme=statTab_a.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forApp,'points'),heights=unit(statTab_a.H_forApp,'points'))

	statTab_b.theme_forWeb$core$bg_params$fill = statTab_gmi.bgcol
	statTab_b.theme_forWeb$core$fg_params$col = statTab_gmi_b.ftcol
	out.Web.statTab_gmi_b = tableGrob(statTab_gmi_b,theme=statTab_b.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forWeb,'points'),heights=unit(statTab_b.H_forWeb,'points'))

	statTab_b.theme_forApp$core$bg_params$fill = statTab_gmi.bgcol
	statTab_b.theme_forApp$core$fg_params$col = statTab_gmi_b.ftcol
	out.App.statTab_gmi_b = tableGrob(statTab_gmi_b,theme=statTab_b.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forApp,'points'),heights=unit(statTab_b.H_forApp,'points'))

	statTab_a.theme_forWeb$core$bg_params$fill = statTab_cv.bgcol
	statTab_a.theme_forWeb$core$fg_params$col[8] = statTab_cv_a.ftcol
	out.Web.statTab_cv_a = tableGrob(statTab_cv_a,theme=statTab_a.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forWeb,'points'),heights=unit(statTab_a.H_forWeb,'points'))

	statTab_a.theme_forApp$core$bg_params$fill = statTab_cv.bgcol
	statTab_a.theme_forApp$core$fg_params$col[8] = statTab_cv_a.ftcol
	out.App.statTab_cv_a = tableGrob(statTab_cv_a,theme=statTab_a.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forApp,'points'),heights=unit(statTab_a.H_forApp,'points'))

	statTab_b.theme_forWeb$core$bg_params$fill = statTab_cv.bgcol
	statTab_b.theme_forWeb$core$fg_params$col = statTab_cv_b.ftcol
	out.Web.statTab_cv_b = tableGrob(statTab_cv_b,theme=statTab_b.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forWeb,'points'),heights=unit(statTab_b.H_forWeb,'points'))

	statTab_b.theme_forApp$core$bg_params$fill = statTab_cv.bgcol
	statTab_b.theme_forApp$core$fg_params$col = statTab_cv_b.ftcol
	out.App.statTab_cv_b = tableGrob(statTab_cv_b,theme=statTab_b.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forApp,'points'),heights=unit(statTab_b.H_forApp,'points'))

	statTab_a.theme_forWeb$core$bg_params$fill = '#F7F8F9'
	statTab_a.theme_forWeb$core$fg_params$col[8] = '#122747'
	out.Web.statTab_sd_a = tableGrob(statTab_sd_a,theme=statTab_a.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forWeb,'points'),heights=unit(statTab_a.H_forWeb,'points'))

	statTab_a.theme_forApp$core$bg_params$fill = '#F7F8F9'
	statTab_a.theme_forApp$core$fg_params$col[8] = '#122747'
	out.App.statTab_sd_a = tableGrob(statTab_sd_a,theme=statTab_a.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_a.W_forApp,'points'),heights=unit(statTab_a.H_forApp,'points'))

	statTab_b.theme_forWeb$core$bg_params$fill = '#F7F8F9'
	statTab_b.theme_forWeb$core$fg_params$col = '#808285'
	out.Web.statTab_sd_b = tableGrob(statTab_sd_b,theme=statTab_b.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forWeb,'points'),heights=unit(statTab_b.H_forWeb,'points'))

	statTab_b.theme_forApp$core$bg_params$fill = '#F7F8F9'
	statTab_b.theme_forApp$core$fg_params$col = '#808285'
	out.App.statTab_sd_b = tableGrob(statTab_sd_b,theme=statTab_b.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(statTab_b.W_forApp,'points'),heights=unit(statTab_b.H_forApp,'points'))

	# ADD LINE # 
	out.Web.statTab_avg_a = gtable_add_grob(out.Web.statTab_avg_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)	
	out.Web.statTab_gmi_a = gtable_add_grob(out.Web.statTab_gmi_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)
	out.Web.statTab_cv_a = gtable_add_grob(out.Web.statTab_cv_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)
	out.Web.statTab_sd_a = gtable_add_grob(out.Web.statTab_sd_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)		
	out.Web.statTab_sd_b = gtable_add_grob(out.Web.statTab_sd_b,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=3,z=3)

	out.App.statTab_avg_a = gtable_add_grob(out.App.statTab_avg_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)	
	out.App.statTab_gmi_a = gtable_add_grob(out.App.statTab_gmi_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)
	out.App.statTab_cv_a = gtable_add_grob(out.App.statTab_cv_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)
	out.App.statTab_sd_a = gtable_add_grob(out.App.statTab_sd_a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=6)		
	out.App.statTab_sd_b = gtable_add_grob(out.App.statTab_sd_b,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(lwd=5,col='#FFFFFF')),t=1,l=1,r=3,z=3)


	## TABLE : eA1c표 ## 
	# theme # 
	statTab_ea1c_a.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=statTab_ea1c.bgcol,col=NA), 
					fg_params=list(hjust=0,x=0.05,vjust=c(0.5,0),col=c('#122747','#808285'),
					fontsize=c(13.5,12),fontface='plain'))
	)
	statTab_ea1c_a.theme_forWeb$core$fg_params$fontfamily = c('NotoSansCJKkrB','NotoSansCJKkrR')
	statTab_ea1c_b.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=statTab_ea1c.bgcol,col=NA),
					fg_params=list(hjust=0,x=c(0.2,0.01),vjust=0,col=statTab_ea1c.ftcol,
					fontsize=c(16,10),fontface='plain'))
	)
	statTab_ea1c_b.theme_forWeb$core$fg_params$fontfamily = c('NotoSansCJKkrB','NotoSansCJKkrR')


	## STAT TABLE ## 
	out.Web.statTab = grid.arrange(grobs=list(
		out.Web.statTab_avg_a,out.Web.statTab_avg_b,out.Web.statTab_gmi_a,out.Web.statTab_gmi_b,out.Web.statTab_cv_a,out.Web.statTab_cv_b,out.Web.statTab_sd_a,out.Web.statTab_sd_b),
		nrow=8,ncol=1,layout_matrix(rbind(1,2,3,4,5,6,7,8)),widths=unit(sum(statTab_a.W_forWeb),'points'),heights=unit(rep(c(sum(statTab_a.H_forWeb),sum(statTab_b.H_forWeb)),4),'points'))

	out.App.statTab = grid.arrange(grobs=list(
		out.App.statTab_avg_a,out.App.statTab_avg_b,out.App.statTab_gmi_a,out.App.statTab_gmi_b,out.App.statTab_cv_a,out.App.statTab_cv_b,out.App.statTab_sd_a,out.App.statTab_sd_b),
		nrow=8,ncol=1,layout_matrix(rbind(1,2,3,4,5,6,7,8)),widths=unit(sum(statTab_a.W_forApp),'points'),heights=unit(rep(c(sum(statTab_a.H_forApp),sum(statTab_b.H_forApp)),4),'points'))


	out.Web.statTab_ea1c = grid.arrange(grobs=list(
		tableGrob(statTab_ea1c_a,theme=statTab_ea1c_a.theme_forWeb,cols=NULL,rows=NULL,widths=unit(258,'points'),heights=unit(statTab_a.H_forWeb,'points')),
		tableGrob(statTab_ea1c_b,theme=statTab_ea1c_b.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(58,200),'points'),heights=unit(statTab_b.H_forWeb,'points'))),
		nrow=2,ncol=1,layout_matirx(rbind(1,2)),widths=unit(258,'points'),heights=unit(c(sum(statTab_a.H_forWeb),sum(statTab_b.H_forWeb)),'points'))

	out.Web.statTab_ea1c = gtable_add_grob(out.Web.statTab_ea1c,grobs=rectGrob(gp=gpar(fill=NA,col=statTab_ea1c.ftcol,lwd=2)),t=1,b=2,l=1,r=1)

	## rounded corner ## 
	grob.tmp = out.Web.statTab_ea1c$grobs[[3]]
	rgrob.tmp = roundrectGrob(x=grob.tmp$x,y=grob.tmp$y,width=grob.tmp$width,height=grob.tmp$height,r=unit(0.1,'snpc'),just=grob.tmp$just,name=grob.tmp$name,gp=grob.tmp$gp,vp=grob.tmp$vp)
	out.Web.statTab_ea1c$grobs[[3]] = rgrob.tmp

	statTab_ea1c_a.theme_forApp = statTab_ea1c_a.theme_forWeb
	statTab_ea1c_a.theme_forApp$core$fg_params$fontsize = c(23,15)
	statTab_ea1c_a.theme_forApp$core$fg_params$x = 0.05
	statTab_ea1c_b.theme_forApp = statTab_ea1c_b.theme_forWeb
	statTab_ea1c_b.theme_forApp$core$fg_params$fontsize = c(25,15)
	statTab_ea1c_b.theme_forApp$core$fg_params$x = c(0.8,0.01)
	statTab_ea1c_b.theme_forApp$core$fg_params$vjust = 0.5
	statTab_ea1c_b.theme_forApp$core$fg_params$hjust = c(0.8,0)

	out.App.statTab_ea1c = grid.arrange(grobs=list(
		tableGrob(statTab_ea1c_a,theme=statTab_ea1c_a.theme_forApp,cols=NULL,rows=NULL,widths=unit(412,'points'),heights=unit(statTab_a.H_forApp,'points')),
		tableGrob(statTab_ea1c_b,theme=statTab_ea1c_b.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(50,37),'points'),heights=unit(sum(statTab_a.H_forApp),'points'))),
		nrow=1,ncol=2,layout_matirx(cbind(1,2)),widths=unit(c(412,87),'points'),heights=unit(sum(statTab_a.H_forApp),'points'))

	out.App.statTab_ea1c = gtable_add_grob(out.App.statTab_ea1c,grobs=rectGrob(gp=gpar(fill=NA,col=statTab_ea1c.ftcol,lwd=2)),t=1,b=1,l=1,r=2)

	grob.tmp = out.App.statTab_ea1c$grobs[[3]]
	rgrob.tmp = roundrectGrob(x=grob.tmp$x,y=grob.tmp$y,width=grob.tmp$width,height=grob.tmp$height,r=unit(0.1,'snpc'),just=grob.tmp$just,name=grob.tmp$name,gp=grob.tmp$gp,vp=grob.tmp$vp)
	out.App.statTab_ea1c$grobs[[3]] = rgrob.tmp


	### step3 =============================================================================##
	## out.TIR : 범위내시간 TIR ##
	out.TIR = vector('list',2)
	out.TIR[[1]] = vector('list',length(subP))

	for ( p in subP ) {

		out.TIR[[1]][[p]] = vector('numeric',5)
		data.tmp = data[which(data$sub==p & data$log==1),]
		out.TIR[[1]][[p]][5] = ifelse(!is.na(GlucoseCut[1]),sum(data.tmp$glucose<GlucoseCut[1]),NA)
		out.TIR[[1]][[p]][4] = ifelse(!is.na(GlucoseCut[1]) & !is.na(GlucoseCut[2]), 
								sum(data.tmp$glucose>=GlucoseCut[1] & data.tmp$glucose<GlucoseCut[2]),
								ifelse(is.na(GlucoseCut[1]) & !is.na(GlucoseCut[2]), sum(data.tmp$glucose<GlucoseCut[2]),NA))
		out.TIR[[1]][[p]][3] = ifelse(!is.na(GlucoseCut[2]) & !is.na(GlucoseCut[3]),
								sum(data.tmp$glucose>=GlucoseCut[2] & data.tmp$glucose<GlucoseCut[3]),NA)
		out.TIR[[1]][[p]][2] = ifelse(!is.na(GlucoseCut[3]) & !is.na(GlucoseCut[4]),
								sum(data.tmp$glucose>=GlucoseCut[3] & data.tmp$glucose<GlucoseCut[4]),
								ifelse(!is.na(GlucoseCut[3]) & is.na(GlucoseCut[4]), sum(data.tmp$glucose>=GlucoseCut[3]),NA))
		out.TIR[[1]][[p]][1] = ifelse(!is.na(GlucoseCut[4]), sum(data.tmp$glucose>=GlucoseCut[4]), NA)
		names(out.TIR[[1]][[p]]) = c('TAR2','TAR1','TIR','TBR1','TBR2')

		out.TIR[[1]][[p]] = 100*out.TIR[[1]][[p]]/sum(out.TIR[[1]][[p]],na.rm=T)

	}

	break.tmp = GlucoseCut
	break.tmp[3:4] = GlucoseCut[3:4] - 1

	TIR.source = data.frame(x=1,gr=factor(names(out.TIR[[1]][[1]])),levels=names(out.TIR[[1]][[1]]),y=out.TIR[[1]][[1]])

	if ( max(subP)==2 ) {
		TIR.source = rbind(TIR.source,data.frame(x=2,gr=factor(names(out.TIR[[1]][[2]])),levels=names(out.TIR[[1]][[2]]),y=out.TIR[[1]][[2]]))
	}
	TIR.source$levels = ifelse(TIR.source$gr=='TBR2',5,ifelse(TIR.source$gr=='TBR1',4,ifelse(TIR.source$gr=='TIR',3,ifelse(TIR.source$gr=='TAR1',2,1))))

	## TIR 전체 그래프 ## 
	TIR.source$yprime = (TIR.source$y+4)
	TIR.source$xlab = ifelse(TIR.source$x==1,'이번 결과','과거')

	out.TIR[[2]] = ggplot(TIR.source, aes(x=xlab,y=yprime,fill=factor(TIR.source$levels),color=factor(x)))+
		scale_fill_manual(values=c('5'='#b6202e','4'='#d71920','3'='#40ac49','2'='#fff100','1'='#fcb813'))+
		scale_color_manual(values=c('1'='#122747','2'=NA))+
		geom_chicklet(width=(0.8/TIR.source$x),position='stack')+
		geom_text(aes(label=paste(round(y,0),'%',sep='')),vjust=1.1,color='#122747',position='stack',size=4)+
		geom_hline(yintercept=0,color='#122747',size=0.3)+
		theme(panel.background=element_rect(fill=NA), legend.position='none',
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0,0.3,0.3,-1,unit='cm'))


	## TIR 전체 테이블 ##

	tirTab = data.frame(matrix(NA,nrow=15,ncol=3))
	tirTab.lab = vector('character',15)
	tirTab.lab[which(c(1:15)%%3==1)] = ifelse(grepl('TAR',names(out.TIR[[1]][[1]])),'높은 고혈당',ifelse(grepl('TBR',names(out.TIR[[1]][[1]])),'낮은 저혈당',ifelse(grepl('TIR',names(out.TIR[[1]][[1]])),'목표범위','')))
	tirTab.lab[which(c(1:15)%%3==1)] = ifelse(grepl('2',names(out.TIR[[1]][[1]])),paste('매우',tirTab.lab[which(c(1:15)%%3==1)]),gsub('높은 |낮은 ','',tirTab.lab[which(c(1:15)%%3==1)]))

	tirTab.lab[which(c(1:15)%%3==2)] = rev(lab.add)
	tirTab[,2] = tirTab.lab
	tirTab[,1] = rep(c('■','',''),5)
	tirTab[which(c(1:15)%%3==1),3] = paste(round(out.TIR[[1]][[1]],0),' %',sep='')
	tirTab[which(c(1:15)%%3==2)[c(1,2,4,5)],3] = paste(rev(out.Goal[[5]][[2]])[c(1,2,4,5)],'%미만',sep='')
	tirTab[which(c(1:15)%%3==2)[3],3] = paste(rev(out.Goal[[5]][[2]])[3],'%초과',sep='')
	tirTab[which(c(1:15)%%3==0),3] = ''


	tirTab.GoalYN = rep(NA,5) #1 yes 2 no
	tirTab.row = c()
	tirTab.symb.col = c()

	if( !is.na(TAR_lev2.Goal) ) {
		tirTab.GoalYN[names(out.TIR[[1]][[1]])=='TAR2'] = ifelse(out.TIR[[1]][[1]][names(out.TIR[[1]][[1]])=='TAR2'] >= TAR_lev2.Goal && TAR_lev2.Goal!=0,2,1)
		tirTab.row = c(tirTab.row,c(1,2,3))
		tirTab.symb.col = c(tirTab.symb.col,'#fcb813')	
	}
	if( !is.na(TAR_lev1.Goal) ) {
		tirTab.GoalYN[names(out.TIR[[1]][[1]])=='TAR1'] = ifelse(sum(out.TIR[[1]][[1]][grepl('TAR',names(out.TIR[[1]][[1]]))],na.rm=T) >= TAR_lev1.Goal,2,1)
		tirTab.row = c(tirTab.row,c(4,5,6))
		tirTab.symb.col = c(tirTab.symb.col,'#fff100')	
	}
	if( !is.na(TIR.Goal) ) {
		tirTab.GoalYN[names(out.TIR[[1]][[1]])=='TIR'] = ifelse(out.TIR[[1]][[1]][names(out.TIR[[1]][[1]])=='TIR'] < TIR.Goal,2,1)
		tirTab.row = c(tirTab.row,c(7,8,9))	
		tirTab.symb.col = c(tirTab.symb.col,'#40ac49')	
	}
	if( !is.na(TBR_lev1.Goal) ) {
		tirTab.GoalYN[names(out.TIR[[1]][[1]])=='TBR1'] = ifelse(sum(out.TIR[[1]][[1]][grepl('TBR',names(out.TIR[[1]][[1]]))],na.rm=T) >= TBR_lev1.Goal,2,1)
		tirTab.row = c(tirTab.row,c(10,11,12))	
		tirTab.symb.col = c(tirTab.symb.col,'#d71920')	
	}
	if( !is.na(TBR_lev2.Goal) ) {
		tirTab.GoalYN[names(out.TIR[[1]][[1]])=='TBR2'] = ifelse(out.TIR[[1]][[1]][names(out.TIR[[1]][[1]])=='TBR2'] >= TBR_lev2.Goal && TBR_lev2.Goal!=0,2,1)
		tirTab.row = c(tirTab.row,c(13,14,15))	
		tirTab.symb.col = c(tirTab.symb.col,'#b6202e')	
	}

#	tirTab.bgcol = rep(ifelse(tirTab.GoalYN==2,'#FFF6F9','#F7F8F9'),each=3)
	tirTab.bgcol = rep(ifelse(tirTab.GoalYN==2,'#FFF6F9','#FFFFFF'),each=3) ##FFF6F9
	tirTab.ftcol = vector('character',15)
	tirTab.ftcol[which(c(1:15)%%3==1)] = ifelse(tirTab.GoalYN==2,'#FF2525','#122747')
	tirTab.ftcol[which(c(1:15)%%3==2)] = ifelse(!is.na(tirTab.GoalYN),'#122747',NA)
	tirTab.ftcol[which(c(1:15)%%3==0)] = '#122747' 

	tirTab.theme_forWeb = ttheme_minimal(base_family='NotoSansCJKkrR',
#		core = list(bg_params=list(fill=tirTab.bgcol[!is.na(tirTab.bgcol)],col=NA),
		core = list(bg_params=list(fill=c(rep('#FFFFFF',length(tirTab.row)),rep(tirTab.bgcol[!is.na(tirTab.bgcol)],2)),col=NA),
					fg_params=list(hjust=rep(0,length(tirTab.row)*3),x=0.01,vjust=0.5,col=c(rep(tirTab.symb.col,each=3),rep('#122747',length(tirTab.row)),tirTab.ftcol[!is.na(tirTab.ftcol)]),
					fontsize=c(rep(14,length(tirTab.row)),rep(c(13,12,12),length(tirTab.row)/3),rep(c(14,11,11),length(tirTab.row)/3)))
		)
	)
	tirTab.theme_forWeb$core$fg_params$fontfamily = c(rep('NotoSansCJKkrR',length(tirTab.row)*2),rep(c('NotoSansCJKkrB','NotoSansCJKkrR','NotoSansCJKkrR'),length(tirTab.row)/3))

	tirTab = tirTab[tirTab.row,]
	tirTab.H_forWeb = rep(22,length(tirTab.row))
	tirTab.W_forWeb = c(20,132,70)
	out.Web.tirTab = tableGrob(tirTab,theme=tirTab.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(tirTab.W_forWeb,'points'),heights=unit(tirTab.H_forWeb,'points'))

	tirTab.theme_forApp = tirTab.theme_forWeb
	tirTab.theme_forApp$core$fg_params$fontsize = c(rep(20,length(tirTab.row)),rep(c(21,17,17),length(tirTab.row)/3),rep(c(25,15,15),length(tirTab.row)/3))

	tirTab.H_forApp = rep(28,length(tirTab.row))
	tirTab.W_forApp = c(28,155,83)
	out.App.tirTab = tableGrob(tirTab,theme=tirTab.theme_forApp,cols=NULL,rows=NULL,
		widths=unit(tirTab.W_forApp,'points'),heights=unit(tirTab.H_forApp,'points'))

	## TIR 상세 그래프 ##
	# HMSIdx.6H : 1 야간 2 오전 3 오후 4 저녁 
	out.TIR.detail = vector('list',2)
	out.TIR.detail[[1]] = matrix(NA,nrow=5,ncol=length(HMScut.6H))

	data.tmp = data[which(data$sub==1 & data$log==1),]
#	mat.TIR.detail = matrix(NA,nrow=5,ncol=4)
	
	if ( !is.na(GlucoseCut[1]) ) {
		out.TIR.detail[[1]][5,] = tapply(data.tmp$glucose<GlucoseCut[1],data.tmp$HMSIdx.6H,sum,na.rm=T)
	}
	if ( !is.na(GlucoseCut[1]) & !is.na(GlucoseCut[2]) ) {
		out.TIR.detail[[1]][4,] = tapply(data.tmp$glucose>=GlucoseCut[1] & data.tmp$glucose<GlucoseCut[2],data.tmp$HMSIdx.6H,sum,na.rm=T)
	} else if ( is.na(GlucoseCut[1]) & !is.na(GlucoseCut[2]) ) {
		out.TIR.detail[[1]][4,] = tapply(data.tmp$glucose<GlucoseCut[2],data.tmp$HMSIdx.6H,sum,na.rm=T)
	}
	if ( !is.na(GlucoseCut[2]) & !is.na(GlucoseCut[3]) ) {
		out.TIR.detail[[1]][3,] = tapply(data.tmp$glucose>=GlucoseCut[2] & data.tmp$glucose<GlucoseCut[3],data.tmp$HMSIdx.6H,sum,na.rm=T)
	}
	if ( !is.na(GlucoseCut[3]) & !is.na(GlucoseCut[4]) ) {
		out.TIR.detail[[1]][2,] = tapply(data.tmp$glucose>=GlucoseCut[3] & data.tmp$glucose<GlucoseCut[4],data.tmp$HMSIdx.6H,sum,na.rm=T)
	} else if ( !is.na(GlucoseCut[3]) & is.na(GlucoseCut[4]) ) {
		out.TIR.detail[[1]][2,]= tapply(data.tmp$glucose>=GlucoseCut[3],data.tmp$HMSIdx.6H,sum,na.rm=T)
	}
	if ( !is.na(GlucoseCut[4]) ) {
		out.TIR.detail[[1]][1,] = tapply(data.tmp$glucose>=GlucoseCut[4],data.tmp$HMSIdx.6H,sum)
	}

	TIR.detail.source = data.frame(gr=rep(c('TAR2','TAR1','TIR','TBR1','TBR2'),4),block=rep(c(1:length(HMScut.6H)),each=5),y=NA,time=NA)#unlist(out.TIR.detail[[1]]))	
	
	if ( TIR.detail.opt==1 ) {
		## 전체시간 중 차지하는 비중
		for ( b in 1:length(HMScut.6H) ) {
		#	out.TIR.detail[[1]][[b]] = 100*mat.TIR.detail[,b]/sum(mat.TIR.detail,na.rm=T)
			TIR.detail.source[which(TIR.detail.source$block==b),]$y = 100*out.TIR.detail[[1]][,b]/sum(out.TIR.detail[[1]],na.rm=T)
			TIR.detail.source[which(TIR.detail.source$block==b),]$time = out.TIR.detail[[1]][,b]
		} 
	} else if ( TIR.detail.opt==2 ) {
		## 시간block별로 <- 처음에 한버전 . 난 이게 맞는것 같아!! ############################################################################### 
		for ( b in 1:length(HMScut.6H) ) {
		#	out.TIR.detail[[1]][[b]] = 100*out.TIR.detail[[1]][,b]/sum(out.TIR.detail[[1]][,b],na.rm=T)
			TIR.detail.source[which(TIR.detail.source$block==b),]$y = 100*out.TIR.detail[[1]][,b]/sum(out.TIR.detail[[1]][,b],na.rm=T)
			TIR.detail.source[which(TIR.detail.source$block==b),]$time = out.TIR.detail[[1]][,b]#*15/60
		} 
	} else if ( TIR.detail.opt==3 ) {
		## 혈당level별로 
		for ( b in 1:length(HMScut.6H) ) {
		#	out.TIR.detail[[1]][[b]] = 100*mat.TIR.detail[,b]/apply(mat.TIR.detail,1,sum,na.rm=T)
			TIR.detail.source[which(TIR.detail.source$block==b),]$y = 100*out.TIR.detail[[1]][,b]/apply(out.TIR.detail[[1]],1,sum,na.rm=T)
			TIR.detail.source[which(TIR.detail.source$block==b),]$time = out.TIR.detail[[1]][,b]
		}
	}


	TIR.detail.source$colYN = 0
	TIR.detail.source$xlab = ifelse(TIR.detail.source$block==1,'야간',ifelse(TIR.detail.source$block==2,'오전',ifelse(TIR.detail.source$block==3,'오후','저녁')))
	TIR.detail.source$timelab = ifelse(TIR.detail.source$time>60,paste(TIR.detail.source$time%/%60,'시간',TIR.detail.source$time%%60,'분',sep=''),paste(TIR.detail.source$time,'분',sep=''))

	out.TIR.detail[[2]] = vector('list',5)
	names(out.TIR.detail[[2]]) = c('TAR2','TAR1','TIR','TBR1','TBR2')

	if ( !is.na(TAR_lev2.Goal) && any(!is.na(TIR.detail.source[which(TIR.detail.source$gr=='TAR2'),]$y)) ) {
		TIR.detail.source$colYN[which(TIR.detail.source$gr=='TAR2')[which.max(TIR.detail.source[which(TIR.detail.source$gr=='TAR2'),]$y)]]=1 #=> 0 일 경우 에 대한 이슈 처리 필요 
		ylim.tmp = c(0,max(TIR.detail.source[which(TIR.detail.source$gr=='TAR2'),]$y))
		ylim.tmp[2] = ifelse(max(ylim.tmp)==0, 1, round(ylim.tmp[2]*1.1,0))
		out.TIR.detail[[2]][[1]] = ggplot(TIR.detail.source[which(TIR.detail.source$gr=='TAR2'),],aes(x=xlab,y=y,fill=factor(colYN)))+
			scale_fill_manual(values=c('1'='#fcb813','0'='#FFD85D'))+
			scale_y_continuous(limits=ylim.tmp)+
			geom_chicklet(width=0.5)+
#			geom_text(aes(label=paste(round(y,0),'%',sep='')),vjust=-1,color='#122747',size=3.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#122747',size=3.5)+
			geom_hline(yintercept=0,color='#122747',size=0.3)+
			labs(title='매우 높은 고혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#122747',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[1]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='매우 높은 고혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			plot.subtitle=element_text(size=13,color='#808285',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))
	}

	if ( !is.na(TAR_lev1.Goal) && any(!is.na(TIR.detail.source[which(TIR.detail.source$gr=='TAR1'),]$y)) ) {
		TIR.detail.source$colYN[which(TIR.detail.source$gr=='TAR1')[which.max(TIR.detail.source[which(TIR.detail.source$gr=='TAR1'),]$y)]]=1
		ylim.tmp = c(0,max(TIR.detail.source[which(TIR.detail.source$gr=='TAR1'),]$y))
		ylim.tmp[2] = ifelse(max(ylim.tmp)==0, 1, round(ylim.tmp[2]*1.1,0))
		out.TIR.detail[[2]][[2]] = ggplot(TIR.detail.source[which(TIR.detail.source$gr=='TAR1'),],aes(x=xlab,y=y,fill=factor(colYN)))+
			scale_fill_manual(values=c('1'='#fff100','0'='#FFFF7D'))+
			scale_y_continuous(limits=ylim.tmp)+
			geom_chicklet(width=0.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#122747',size=3.5)+
			geom_hline(yintercept=0,color='#122747',size=0.3)+
			labs(title='고혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#122747',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[2]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='고혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			plot.subtitle=element_text(size=13,color='#808285',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))
	}
	
	if ( !is.na(TBR_lev1.Goal) && any(!is.na(TIR.detail.source[which(TIR.detail.source$gr=='TBR1'),]$y)) ) {
		TIR.detail.source$colYN[which(TIR.detail.source$gr=='TBR1')[which.max(TIR.detail.source[which(TIR.detail.source$gr=='TBR1'),]$y)]]=1
		ylim.tmp = c(0,max(TIR.detail.source[which(TIR.detail.source$gr=='TBR1'),]$y))
		ylim.tmp[2] = ifelse(max(ylim.tmp)==0, 1, round(ylim.tmp[2]*1.1,0))
		out.TIR.detail[[2]][[3]] = ggplot(TIR.detail.source[which(TIR.detail.source$gr=='TBR1'),],aes(x=xlab,y=y,fill=factor(colYN)))+
			scale_fill_manual(values=c('1'='#D71920','0'='#F07C82'))+
			scale_y_continuous(limits=ylim.tmp)+
			geom_chicklet(width=0.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#122747',size=3.5)+
			geom_hline(yintercept=0,color='#122747',size=0.3)+
			labs(title='저혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#122747',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[3]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='저혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			plot.subtitle=element_text(size=13,color='#808285',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))
	}

	if ( !is.null(TBR_lev2.Goal) && any(!is.na(TIR.detail.source[which(TIR.detail.source$gr=='TBR2'),]$y)) ) {
		TIR.detail.source$colYN[which(TIR.detail.source$gr=='TBR2')[which.max(TIR.detail.source[which(TIR.detail.source$gr=='TBR2'),]$y)]]=1
		ylim.tmp = c(0,max(TIR.detail.source[which(TIR.detail.source$gr=='TBR2'),]$y))
		ylim.tmp[2] = ifelse(max(ylim.tmp)==0, 1, round(ylim.tmp[2]*1.1,0))
		out.TIR.detail[[2]][[4]] = ggplot(TIR.detail.source[which(TIR.detail.source$gr=='TBR2'),],aes(x=xlab,y=y,fill=factor(colYN)))+
			scale_fill_manual(values=c('1'='#B6202E','0'='#E25C69'))+
			scale_y_continuous(limits=ylim.tmp)+
			geom_chicklet(width=0.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#122747',size=3.5)+
			geom_hline(yintercept=0,color='#122747',size=0.3)+
			labs(title='매우 낮은 저혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#122747',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[4]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='매우 낮은 저혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#122747',family='NotoSansCJKkrB'),
			plot.subtitle=element_text(size=13,color='#808285',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))
	}

	out.TIR.detail[[3]] = grid.arrange(grobs=list(out.TIR.detail[[2]][[1]],out.TIR.detail[[2]][[2]],
		out.TIR.detail[[2]][[3]],out.TIR.detail[[2]][[4]]),
		nrow=2,ncol=2,layout_matrix=rbind(c(1,2),c(3,4)),
		widths=unit(c(209,209),'points'),heights=unit(c(165,165),'points'))


	### output =============================================================================##

	## forApp ==== ##

	Stat_forApp.H = c(50,28,10,sum(c(statTab_a.H_forApp,statTab_b.H_forApp))*4,20,sum(c(statTab_a.H_forApp,statTab_b.H_forApp)))

	CairoPNG(filename=paste(memberKey,createdtime,'App_Stat.png',sep='_'),family='NotoSansCJKkrR',scale=1/0.32,bg='white',width=720,height=((sum(Stat_forApp.H)+30)/0.75),unit='px',dpi=96)
	
	LibreReport_Stat_forApp = try(grid.arrange(grobs=list(
		tableGrob(data.frame('','연속혈당 통계분석'),theme=subTitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(10,502),'points'),heights=unit(50,'points')), #1
		tableGrob(data.frame('',STAT.subTitle.remark),theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(10,502),'points'),heights=unit(28,'points')), #2
		out.App.statTab,out.App.statTab_ea1c), #3 #4
		nrow=6,ncol=1,layout_matrix=rbind(1,2,NA,3,NA,4),
		widths=unit(512,'points'),heights=unit(Stat_forApp.H,'points')),silent=T)

	dev.off()

	if ( class(LibreReport_Stat_forApp)[1]!='try-error' ) {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_Stat.png',sep='_'))
	} else {
		errCode.sub = c(errCode.sub,'Errr_201')
	}


	## forApp ==== ##

	TIR_forApp.H = c(50,28,10,sum(tirTab.H_forApp))

	CairoPNG(filename=paste(memberKey,createdtime,'App_TIR.png',sep='_'),family='NotoSansCJKkrR',scale=1/0.32,bg='white',width=720,height=((sum(TIR_forApp.H)+30)/0.75),unit='px',dpi=96)

	LibreReport_TIR_forApp = try(grid.arrange(grobs=list(
		tableGrob(data.frame('','범위 내 시간'),theme=subTitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(10,502),'points'),heights=unit(50,'points')), #1
		tableGrob(data.frame('',STAT.subTitle.remark),theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(10,502),'points'),heights=unit(28,'points')), #2
		out.TIR[[2]],out.App.tirTab),
		nrow=4,ncol=2,layout_matrix=rbind(c(1,1),c(2,2),NA,c(3,4)),
		widths=unit(c(512-sum(tirTab.W_forApp),sum(tirTab.W_forApp)),'points'),heights=unit(TIR_forApp.H,'points')),silent=T)

	dev.off()

	if ( class(LibreReport_TIR_forApp)[1]!='try-error' ) {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_TIR.png',sep='_'))
	} else {
		errCode.sub = c(errCode.sub,'Errr_202')
	}


	## forWeb ==== ##

	StatandTIR_forWeb.H = c(26,26,10,85,85*3,22,26,26,10,sum(tirTab.H_forWeb),(330-sum(tirTab.H_forWeb)))

	CairoPNG(filename=paste(memberKey,createdtime,'Web_StatandTIR.png',sep='_'),family='NotoSansCJKkrR',scale=1/0.32,bg='white',width=1168,height=(sum(StatandTIR_forWeb.H)+30)/0.75,unit='px',dpi=96)
	
	layout.mat = matrix(NA,nrow=11,ncol=7)
	layout.mat[1,] = 1
	layout.mat[2,] = 2
	layout.mat[4:5,2:3] = 3
	layout.mat[4,6] = 4
	layout.mat[7,] = c(5,5,5,NA,6,6,6)
	layout.mat[8,5:7] = 7
	layout.mat[10,] = c(NA,8,9,NA,10,10,NA)
	layout.mat[11,] = c(NA,NA,NA,NA,10,10,NA)

	LibreReport_StatandTIR_forWeb = try(grid.arrange(grobs=list(
		tableGrob(data.frame('','연속혈당 통계분석'),theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(5,871),'points'),heights=unit(26,'points')), #1 
		tableGrob(data.frame('',STAT.subTitle.remark),theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(5,871),'points'),heights=unit(26,'points')), #2
		out.Web.statTab, #3
		out.Web.statTab_ea1c, #4
		tableGrob(data.frame('','범위 내 시간 (Time in Ranges)'),theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(5,413),'points'),heights=unit(26,'points')), #5
		tableGrob(data.frame('','범위내시간> 시간대별 상세보기'),theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(5,433),'points'),heights=unit(26,'points')), #6
		tableGrob(data.frame('','야간: 22-04시 오전: 04-10시 오후: 10-16시 저녁: 16-22시'),theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(5,433),'points'),heights=unit(26,'points')), #7
		out.TIR[[2]], #8 
		out.Web.tirTab, #9
		out.TIR.detail[[3]]), #10
		nrow=11,ncol=7,layout_matrix=layout.mat,
		widths=unit(c(15,181,222,20,50,368,20),'points'),heights=unit(c(26,26,10,85,85*3,20,26,26,10,sum(tirTab.H_forWeb),(330-sum(tirTab.H_forWeb))),'points')),silent=T)

	dev.off() 
	
	if ( class(LibreReport_StatandTIR_forWeb)[1]!='try-error' ) {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_StatandTIR.png',sep='_'))
	} else {
		errCode.sub = c(errCode.sub,'Errr_203')
	}


	## 방법 1 ## 
	Result = vector('list',4)
	names(Result) = c('outFileNames','errCode.sub','TargetGoal','TIRstat')

	if ( is.null(outFileNames) ) outFileNames = NA 
	if ( is.null(errCode.sub) ) errCode.sub = NA 
	
	Result[[1]] = outFileNames
	Result[[2]] = errCode.sub
	Result[[3]] = out.Goal
	Result[[4]] = out.TIR[[1]][[1]]
	
	## 방법 2 ##
#	Result = vector('list',2)
#	Result[[1]] = list( out.tab1_forWeb, out.tab2_forWeb, out.TIR, out.tab3_forWeb, out.TIR.detail )
#	names(Result[[1]]) = c('out.tab1_forWeb', 'out.tab2_forWeb', 'out.TIR', 'out.tab3_forWeb', 'out.TIR.detail')
#	Result[[2]] = errCode.sub

	return( Result )

}

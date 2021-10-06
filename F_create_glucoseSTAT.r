
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

	subTitle.theme_forApp = ttheme_minimal(base_family='NotoSansCJKkrB', 
		core = list(bg_params=list(fill='#000000',col=NA),
					fg_params=list(hjust=0,x=0.01,col='#ffffff',
					fontsize=25,fontface='bold'))
	)
	remark.theme_forApp = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#808285',
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
	tab1_row1a = data.frame(matrix(NA,nrow=2,ncol=5))
	tab1_row1b = data.frame(matrix(NA,nrow=1,ncol=2))
	tab1_row2a = data.frame(matrix(NA,nrow=2,ncol=5))
	tab1_row2b = data.frame(matrix(NA,nrow=1,ncol=2))
	tab1_row3a = data.frame(matrix(NA,nrow=2,ncol=5))
	tab1_row3b = data.frame(matrix(NA,nrow=1,ncol=2))
	tab1_row4a = data.frame(matrix(NA,nrow=2,ncol=5))
	tab1_row4b = data.frame(matrix(NA,nrow=1,ncol=2))

	tab2_colname = data.frame(matrix(NA,nrow=1,ncol=1))
	tab2_row1 = data.frame(matrix(NA,nrow=2,ncol=2))
	tab2_row2 = data.frame(matrix(NA,nrow=2,ncol=2))

	tab1_row1a[,1] = ''
	tab1_row1a[1,2] = '평균 혈당'
	tab1_row1a[2,2] = ''
	tab1_row2a[,2] = ''
	tab1_row2a[1,2] = '혈당관리표시'
	tab1_row2a[2,2] = 'GMI(단일)'
	tab1_row3a[,1] = '' 
	tab1_row3a[1,2] = '혈당변동계수'
	tab1_row3a[2,2] = 'cv%'
	tab1_row4a[,1] = ''
	tab1_row4a[1,2] = '혈당표준편차'
	tab1_row4a[2,2] = ''

	tab1_row1a[,4] = c('이번',round(out.STAT[[1]][1],0))
	tab1_row2a[,4] = c('이번',round(out.STAT[[1]][2],1))
	tab1_row3a[,4] = c('이번',round(out.STAT[[1]][3],1))
	tab1_row4a[,4] = c('이번',round(out.STAT[[1]][4],0))
	
	tab1_row1a[2,5] = ' mg/dL'
	tab1_row2a[2,5] = ' %'
	tab1_row3a[2,5] = ' %'
	tab1_row4a[2,5] = ' mg/dL'	
	tab1_row1a[1,5] = ''
	tab1_row2a[1,5] = ''
	tab1_row3a[1,5] = ''
	tab1_row4a[1,5] = ''

	
	## 목표값 달성 여부 체크 ## 
	tab1_row1b[1,] = c('','88-116 이내 권장합니다.')
	tab1_row4b[1,] = c('','10-26 이내 권장합니다.')
	if ( out.STAT[[1]][2]>=out.Goal[[4]][1] ) {
		tab1_row2a.bgcol = '#FFF6F9'
		tab1_row2a.ftcol = '#FF2525'
		tab1_row2b.ftcol = '#000000'
		tab1_row2b[1,] = c('',paste(out.Goal[[4]][1],'% 미만으로 줄여야합니다.',sep=''))
	} else {
		tab1_row2a.bgcol = '#F7F8F9'
		tab1_row2a.ftcol = '#000000'
		tab1_row2b.ftcol = '#808285'
		tab1_row2b[1,] = c('',paste(out.Goal[[4]][1],'% 미만 달성했습니다.',sep=''))
	}
	if ( out.STAT[[1]][3]>out.Goal[[4]][2] ) {
		tab1_row3a.bgcol = '#FFF6F9'
		tab1_row3a.ftcol = '#FF2525'
		tab1_row3b.ftcol = '#000000'
		tab1_row3b[1,] = c('',paste(out.Goal[[4]][2],'% 이하로 줄여야합니다.',sep=''))
	} else {
		tab1_row3a.bgcol = '#F7F8F9'
		tab1_row3a.ftcol = '#000000'
		tab1_row3b.ftcol = '#808285'
		tab1_row3b[1,] = c('',paste(out.Goal[[4]][2],'% 이하 달성했습니다.',sep=''))
	}


	## 과거 내역 비교 ## 
	if ( mod==2 ) {
		
		Stat.subTitle.remark = paste('과거 분석기간: ',format(min(data$date[data$sub==2]),'%Y년 %m월 %d일'),'-',format(max(data$date[data$sub==2]),'%Y년 %m월 %d일'),sep='')
		tab1_row1a[,3] = c('과거',round(out.STAT[[2]][1],0))
		tab1_row2a[,3] = c('과거',round(out.STAT[[2]][2],1))
		tab1_row3a[,3] = c('과거',round(out.STAT[[2]][3],1))
		tab1_row4a[,3] = c('과거',round(out.STAT[[2]][4],0))

		tab2_colname[1] = '이전 분석기간에 비해,'
		tab2_row1[1,1] = ifelse( out.STAT[[1]][1]<out.STAT[[2]][1],
									paste('▼ ',round(out.STAT[[2]][1]-out.STAT[[1]][1],0),sep=''),
									paste('▲ ',round(out.STAT[[1]][1]-out.STAT[[1]][1],0),sep=''))
		tab2_row1[1,2] = ' mg/dL'
		tab2_row1[2,] = ''

		tab2_row2[1,1] = ifelse( out.STAT[[1]][2]<out.STAT[[2]][2],
									paste('▼ ',round(out.STAT[[2]][2]-out.STAT[[1]][2],1),sep=''),
									paste('▲ ',round(out.STAT[[1]][2]-out.STAT[[2]][2],1),sep=''))
		tab2_row2[1,2] = ' %P'
		tab2_row2[2,] = ''

		tab3_row1a = data.frame(matrix(NA,nrow=2,ncol=1))
		tab3_row1b = data.frame(matrix(NA,nrow=1,ncol=2))
		tab3_row1a[1,1] = '예측당화혈색소(3개월누적)'
		tab3_row1a[2,1] = format(out.Goal[[1]][2],'%Y년 %m월 %d일 기준')
		eA1c.range = sort(seq(max(data[which(data$sub==1),]$date), by='-89 day', length=2)) # 90일전 
		tab3_row1b[1,1] = round(eA1ceqn(x=mean(data[which(data$date>=eA1c.range[1] & data$date<=eA1c.range[2] & data$log==1),]$glucose,na.rm=T),unit.glucose=unit.glucose),1)
		if ( tab3_row1b[1,1] >= out.Goal[[4]][1] ) {
			tab3_row1a.bgcol = '#FFF6F9'
			tab3_row1a.ftcol = '#FF2525'
		} else {
			tab3_row1a.bgcol = '#F7F8F9'
			tab3_row1a.ftcol = '#000000'			
		}
		tab3_row1b[1,2] = ' %'

	} else {

		Stat.subTitle.remark = ''

		tab1_row1a[,3] = ''
		tab1_row2a[,3] = ''
		tab1_row3a[,3] = ''
		tab1_row4a[,3] = ''

		tab2_colname[1] = ''
		tab2_row1[1,] = ''
		tab2_row1[2,] = ''
		tab2_row2[1,] = '' 
		tab2_row2[2,] = ''
		tab3_row1a = data.frame(matrix(NA,nrow=2,ncol=1))
		tab3_row1b = data.frame(matrix(NA,nrow=1,ncol=2))
		tab3_row1a[1,] = ''
		tab3_row1a[2,] = ''
		tab3_row1b[1,] = ''

		tab3_row1a.bgcol = NA 
		tab3_row1a.ftcol = NA 

	}

	## 출력 ##
	## TABLE1 : 혈당통계표 ## 
	# theme # 
	tab1_row_a.theme = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill='#F7F8F9',col=NA),
					fg_params=list(hjust=rep(c(0,0,0,0,0),each=2),x=rep(c(0.01,0.01,0.01,0.01,0.01),each=2),vjust=rep(c(0.5,0.8),5),col='#000000',
					fontsize=c(c(1,1),c(13.5,13),c(12,14),c(12,14),c(12,10))))
	)
	tab1_row_a.theme$core$fg_params$fontfamily = rep('NotoSansCJKkrR',10)
	tab1_row_a.theme$core$fg_params$fontfamily[c(3,4,8)] = 'NotoSansCJKkrB'
	tab1_row_a.theme$core$fg_params$col = rep('#000000',10)

	tab1_row_b.theme = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill='#F7F8F9',col=NA),
					fg_params=list(hjust=0,x=0,vjust=0,col='#808285',fontsize=11,fontface='plain'))
	)

	# TABLE # 
	out.tab1_row1a = tableGrob(tab1_row1a,theme=tab1_row_a.theme,cols=NULL,rows=NULL,
		widths=unit(c(5,132,77,57,132),'points'),heights=unit(c(33,26),'points'))

	out.tab1_row1b = tableGrob(tab1_row1b,theme=tab1_row_b.theme,cols=NULL,rows=NULL,
		widths=unit(c(214,189),'points'),heights=unit(26,'points'))

	tab1_row_a.theme$core$bg_params$fill = tab1_row2a.bgcol
	tab1_row_a.theme$core$fg_params$col[8] = tab1_row2a.ftcol
	out.tab1_row2a = tableGrob(tab1_row2a,theme=tab1_row_a.theme,cols=NULL,rows=NULL,
		widths=unit(c(5,132,77,57,132),'points'),heights=unit(c(33,26),'points'))

	tab1_row_b.theme$core$bg_params$fill = tab1_row2a.bgcol
	tab1_row_b.theme$core$fg_params$col = tab1_row2b.ftcol
	out.tab1_row2b = tableGrob(tab1_row2b,theme=tab1_row_b.theme,cols=NULL,rows=NULL,
		widths=unit(c(214,189),'points'),heights=unit(26,'points'))

	tab1_row_a.theme$core$bg_params$fill = tab1_row3a.bgcol
	tab1_row_a.theme$core$fg_params$col[8] = tab1_row3a.ftcol
	out.tab1_row3a = tableGrob(tab1_row3a,theme=tab1_row_a.theme,cols=NULL,rows=NULL,
		widths=unit(c(5,132,77,57,132),'points'),heights=unit(c(33,26),'points'))

	tab1_row_b.theme$core$bg_params$fill = tab1_row3a.bgcol
	tab1_row_b.theme$core$fg_params$col = tab1_row3b.ftcol
	out.tab1_row3b = tableGrob(tab1_row3b,theme=tab1_row_b.theme,cols=NULL,rows=NULL,
		widths=unit(c(214,189),'points'),heights=unit(26,'points'))

	tab1_row_a.theme$core$bg_params$fill = '#F7F8F9'
	tab1_row_a.theme$core$fg_params$col[8] = '#000000'
	out.tab1_row4a = tableGrob(tab1_row4a,theme=tab1_row_a.theme,cols=NULL,rows=NULL,
		widths=unit(c(5,132,77,57,132),'points'),heights=unit(c(33,26),'points'))

	tab1_row_b.theme$core$bg_params$fill = '#F7F8F9'
	tab1_row_b.theme$core$fg_params$col = '#808285'
	out.tab1_row4b = tableGrob(tab1_row4b,theme=tab1_row_b.theme,cols=NULL,rows=NULL,
		widths=unit(c(214,189),'points'),heights=unit(26,'points'))

	# ADD LINE # 
	out.tab1_row1a = gtable_add_grob(out.tab1_row1a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=1)),t=1,l=1,r=5)
	
	out.tab1_row2a = gtable_add_grob(out.tab1_row2a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=1)),t=1,l=1,r=5)

	out.tab1_row3a = gtable_add_grob(out.tab1_row3a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=1)),t=1,l=1,r=5)

	out.tab1_row4a = gtable_add_grob(out.tab1_row4a,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc'),gp=gpar(lwd=1)),t=1,l=1,r=5)
		
	out.tab1_row4b = gtable_add_grob(out.tab1_row4b,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(lwd=1)),t=1,l=1,r=2,z=3)

	## TABLE2 : 비교표 ## 
	# theme # 
	tab2_colname.theme = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#000000',
					fontsize=12))
	)	
	tab2_row.theme = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#000000',
					fontsize=c(14,10,10,10)))
	)
	tab2_row.theme$core$fg_params$fontfamily = c('NotoSansCJKkrB','NotoSansCJKkrR','NotoSansCJKkrR','NotoSansCJKkrR')

	## TABLE3 : eA1c표 ## 
	# theme # 
	tab3_row_a.theme = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=tab3_row1a.bgcol,col=NA), 
					fg_params=list(hjust=0,x=0.05,vjust=c(0.5,0),col=c('#000000','#808285'),
					fontsize=c(13.5,12),fontface='plain'))
	)
	tab3_row_a.theme$core$fg_params$fontfamily = c('NotoSansCJKkrB','NotoSansCJKkrR')
	tab3_row_b.theme = ttheme_minimal(base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=tab3_row1a.bgcol,col=NA),
					fg_params=list(hjust=0,x=c(0.2,0.01),vjust=0.5,col=tab3_row1a.ftcol,
					fontsize=c(14,10),fontface='plain'))
	)
	tab3_row_b.theme$core$fg_params$fontfamily = c('NotoSansCJKkrB','NotoSansCJKkrR')


	## STAT TABLE ## 
	out.tab1_forWeb = grid.arrange(grobs=list(
		out.tab1_row1a,out.tab1_row1b,out.tab1_row2a,out.tab1_row2b,out.tab1_row3a,out.tab1_row3b,out.tab1_row4a,out.tab1_row4b),
		nrow=8,ncol=1,layout_matrix(rbind(1,2,3,4,5,6,7,8)),widths=unit(403,'points'),heights=unit(rep(c(59,26),4),'points'))

	out.tab2_forWeb = grid.arrange(grobs=list(
		tableGrob(tab2_colname,theme=tab2_colname.theme,cols=NULL,rows=NULL,widths=unit(140,'points'),heights=unit(33,'points')),
		tableGrob(tab2_row1,theme=tab2_row.theme,cols=NULL,rows=NULL,widths=unit(c(40,100),'points'),heights=unit(c(26,26),'points')),
		tableGrob(tab2_colname,theme=tab2_colname.theme,cols=NULL,rows=NULL,widths=unit(140,'points'),heights=unit(33,'points')),
		tableGrob(tab2_row2,theme=tab2_row.theme,cols=NULL,rows=NULL,widths=unit(c(40,100),'points'),heights=unit(c(26,26),'points'))),
		nrow=4,ncol=1,layout_matrix(rbind(1,2,3,4)),widths=unit(140,'points'),heights=unit(rep(c(26,52),2),'points'))

	out.tab3_forWeb = grid.arrange(grobs=list(
		tableGrob(tab3_row1a,theme=tab3_row_a.theme,cols=NULL,rows=NULL,widths=unit(258,'points'),heights=unit(c(33,26),'points')),
		tableGrob(tab3_row1b,theme=tab3_row_b.theme,cols=NULL,rows=NULL,widths=unit(c(58,200),'points'),heights=unit(26,'points'))),
		nrow=2,ncol=1,layout_matirx(rbind(1,2)),widths=unit(258,'points'),heights=unit(c(59,26),'points'))

	out.tab3_forWeb = gtable_add_grob(out.tab3_forWeb,grobs=rectGrob(gp=gpar(fill=NA,col=tab3_row1a.ftcol)),t=1,b=2,l=1,r=1)


	## todo app ## doing 211006
	LibreReport_Stat_forApp = try(grid.arrange(grobs=list(
		tableGrob('연속혈당 통계분석',theme=subTitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
		tableGrob(Stat.subTitle.remark,theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(28,'points')),
		out.tab1_forWeb,out.tab2_forWeb),
		nrow=5,ncol=3,layout_matrix=rbind(c(1,1,1),c(2,2,2),NA,c(3,NA,4),c(3,NA,NA)),
		widths=unit(c(519,36,321),'points'),heights=unit(c(50,28,10,78,sum(rep(78,3))),'points')),silent=T)


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
		scale_color_manual(values=c('1'='#000000','2'=NA))+
#		scale_x_discrete(name='',breaks=c(1,2),labels=c('과거','이번 결과'))+
		geom_bar(stat='identity',width=(0.8/TIR.source$x),position='stack')+
		geom_text(aes(label=paste(round(y,0),'%',sep='')),vjust=1.1,color='#000000',position='stack',size=4)+
		geom_hline(yintercept=0,color='#000000',size=0.3)+
		theme(panel.background=element_rect(fill=NA), legend.position='none',
		axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=13,color='#000000',family='NotoSansCJKkrR'),
		axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
		plot.margin=margin(0,0.3,0.3,0.3,unit='cm'))


	## TIR 전체 테이블 ##

	tab4 = data.frame(matrix(NA,nrow=15,ncol=3))
	tab4.lab = vector('character',15)
	tab4.lab[which(c(1:15)%%3==1)] = ifelse(grepl('TAR',names(out.TIR[[1]][[1]])),'높은 고혈당',ifelse(grepl('TBR',names(out.TIR[[1]][[1]])),'낮은 저혈당',ifelse(grepl('TIR',names(out.TIR[[1]][[1]])),'목표범위','')))
	tab4.lab[which(c(1:15)%%3==1)] = ifelse(grepl('2',names(out.TIR[[1]][[1]])),paste('매우',tab4.lab[which(c(1:15)%%3==1)]),gsub('높은 |낮은 ','',tab4.lab[which(c(1:15)%%3==1)]))

	tab4.lab[which(c(1:15)%%3==2)] = rev(lab.add)
	tab4[,2] = tab4.lab
	tab4[,1] = rep(c('■','',''),5)
	tab4[which(c(1:15)%%3==1),3] = paste(round(out.TIR[[1]][[1]],0),' %',sep='')
	tab4[which(c(1:15)%%3==2)[c(1,2,4,5)],3] = paste(rev(out.Goal[[5]][[2]])[c(1,2,4,5)],'%미만',sep='')
	tab4[which(c(1:15)%%3==2)[3],3] = paste(rev(out.Goal[[5]][[2]])[3],'%초과',sep='')
	tab4[which(c(1:15)%%3==0),3] = ''


	tab4.GoalYN = rep(NA,5) #1 yes 2 no
	tab4.row = c()
	tab4.symb.col = c()

	if( !is.na(TAR_lev2.Goal) ) {
		tab4.GoalYN[names(out.TIR[[1]][[1]])=='TAR2'] = ifelse(out.TIR[[1]][[1]][names(out.TIR[[1]][[1]])=='TAR2'] >= TAR_lev2.Goal && TAR_lev2.Goal!=0,2,1)
		tab4.row = c(tab4.row,c(1,2,3))
		tab4.symb.col = c(tab4.symb.col,'#fcb813')	
	}
	if( !is.na(TAR_lev1.Goal) ) {
		tab4.GoalYN[names(out.TIR[[1]][[1]])=='TAR1'] = ifelse(sum(out.TIR[[1]][[1]][grepl('TAR',names(out.TIR[[1]][[1]]))],na.rm=T) >= TAR_lev1.Goal,2,1)
		tab4.row = c(tab4.row,c(4,5,6))
		tab4.symb.col = c(tab4.symb.col,'#fff100')	
	}
	if( !is.na(TIR.Goal) ) {
		tab4.GoalYN[names(out.TIR[[1]][[1]])=='TIR'] = ifelse(out.TIR[[1]][[1]][names(out.TIR[[1]][[1]])=='TIR'] < TIR.Goal,2,1)
		tab4.row = c(tab4.row,c(7,8,9))	
		tab4.symb.col = c(tab4.symb.col,'#b6202e')	
	}
	if( !is.na(TBR_lev1.Goal) ) {
		tab4.GoalYN[names(out.TIR[[1]][[1]])=='TBR1'] = ifelse(sum(out.TIR[[1]][[1]][grepl('TBR',names(out.TIR[[1]][[1]]))],na.rm=T) >= TBR_lev1.Goal,2,1)
		tab4.row = c(tab4.row,c(10,11,12))	
		tab4.symb.col = c(tab4.symb.col,'#d71920')	
	}
	if( !is.na(TBR_lev2.Goal) ) {
		tab4.GoalYN[names(out.TIR[[1]][[1]])=='TBR2'] = ifelse(out.TIR[[1]][[1]][names(out.TIR[[1]][[1]])=='TBR2'] >= TBR_lev2.Goal && TBR_lev2.Goal!=0,2,1)
		tab4.row = c(tab4.row,c(13,14,15))	
		tab4.symb.col = c(tab4.symb.col,'#40ac49')	
	}

#	tab4.bgcol = rep(ifelse(tab4.GoalYN==2,'#FFF6F9','#F7F8F9'),each=3)
	tab4.bgcol = rep(ifelse(tab4.GoalYN==2,'#FFF6F9',NA),each=3)
	tab4.ftcol = vector('character',15)
	tab4.ftcol[which(c(1:15)%%3==1)] = ifelse(tab4.GoalYN==2,'#FF2525','#000000')
	tab4.ftcol[which(c(1:15)%%3==2)] = ifelse(!is.na(tab4.GoalYN),'#000000',NA)
	tab4.ftcol[which(c(1:15)%%3==0)] = '#000000' 

	tab4.theme = ttheme_minimal(base_family='NotoSansCJKkrR',
#		core = list(bg_params=list(fill=tab4.bgcol[!is.na(tab4.bgcol)],col=NA),
		core = list(bg_params=list(fill=c(rep(NA,length(tab4.row)),rep(tab4.bgcol[!is.na(tab4.bgcol)],2)),col=NA),
					fg_params=list(hjust=rep(0,length(tab4.row)*3),x=0.01,vjust=0.5,col=c(rep(tab4.symb.col,each=3),rep('#000000',length(tab4.row)),tab4.ftcol[!is.na(tab4.ftcol)]),
					fontsize=c(rep(14,length(tab4.row)),rep(c(13,12,12),length(tab4.row)/3),rep(c(14,11,11),length(tab4.row)/3)))
		)
	)
	tab4.theme$core$fg_params$fontfamily = c(rep('NotoSansCJKkrR',length(tab4.row)*2),rep(c('NotoSansCJKkrB','NotoSansCJKkrR','NotoSansCJKkrR'),length(tab4.row)/3))

	tab4 = tab4[tab4.row,]
	tab4.height = rep(22,length(tab4.row))
	out.tab4_forWeb = tableGrob(tab4,theme=tab4.theme,cols=NULL,rows=NULL,
		widths=unit(c(20,132,70),'points'),heights=unit(tab4.height,'points'))


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

#	TIR.detail.source$y = ifelse( is.na(TIR.detail.source$y), 0, TIR.detail.source$y )

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
			geom_bar(stat='identity',width=0.5)+
#			geom_text(aes(label=paste(round(y,0),'%',sep='')),vjust=-1,color='#000000',size=3.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#000000',size=3.5)+
			geom_hline(yintercept=0,color='#000000',size=0.3)+
			labs(title='매우 높은 고혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#000000',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[1]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='매우 높은 고혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
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
			geom_bar(stat='identity',width=0.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#000000',size=3.5)+
			geom_hline(yintercept=0,color='#000000',size=0.3)+
			labs(title='고혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#000000',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[2]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='고혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
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
			geom_bar(stat='identity',width=0.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#000000',size=3.5)+
			geom_hline(yintercept=0,color='#000000',size=0.3)+
			labs(title='저혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#000000',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[3]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='저혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
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
			geom_bar(stat='identity',width=0.5)+
			geom_text(aes(x=xlab,y=0,label=paste(round(y,0),'%\n(',timelab,')',sep='')),vjust=-0.5,color='#000000',size=3.5)+
			geom_hline(yintercept=0,color='#000000',size=0.3)+
			labs(title='매우 낮은 저혈당')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(size=10,color='#000000',family='NotoSansCJKkrR'),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))

	} else {
		out.TIR.detail[[2]][[4]] = ggplot()+
			scale_x_continuous(limits=c(0,4))+
			scale_y_continuous(limits=c(0,1))+
			labs(title='매우 낮은 저혈당',subtitle='해당사항 없음')+
			theme(panel.background=element_rect(fill=NA),legend.position='none',plot.title=element_text(size=13,color='#000000',family='NotoSansCJKkrB'),
			plot.subtitle=element_text(size=13,color='#808285',family='NotoSansCJKkrB'),
			axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),
			axis.ticks.y=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),
			plot.margin=margin(0.7,0.3,0.3,0.3,unit='cm'))
	}

	out.TIR.detail[[3]] = grid.arrange(grobs=list(out.TIR.detail[[2]][[1]],out.TIR.detail[[2]][[2]],
		out.TIR.detail[[2]][[3]],out.TIR.detail[[2]][[4]]),
		nrow=2,ncol=2,layout_matrix=rbind(c(1,2),c(3,4)),
		widths=unit(c(209,209),'points'),heights=unit(c(165,165),'points'))
	
#	LibreReport_TIR_forWeb = grid.arrange(grobs=list(tableGrob('범위 내 시간 (Time in Ranges)',theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(360,'points'),heights=unit(26,'points')),
#		tableGrob('범위내시간> 시간대별 상세보기',theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(480,'points'),heights=unit(26,'points')),
#		tableGrob('야간: 22-04시 오전: 04-10시 오후: 10-16시 저녁: 16-22시',theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(480,'points'),heights=unit(26,'points')),
#		out.TIR[[2]],out.tab3_forWeb,out.TIR.detail[[3]]),
#		nrow=5,ncol=4,layout_matrix=rbind(c(1,1,1,2),c(NA,NA,NA,3),NA,c(4,5,NA,6),c(NA,NA,NA,6)),
#		widths=unit(c(160,200,36,480),'points'),heights=unit(c(26,26,10,sum(tab3.height),(360-sum(tab3.height))),'points'))

	### output =============================================================================##

	CairoPNG(filename=paste(memberKey,createdtime,'Web_StatandTIR.png',sep='_'),family='NotoSansCJKkrR',scale=1/0.32,bg='white',width=1168,height=1100,unit='px',dpi=96)
	
	layout.mat = matrix(NA,nrow=11,ncol=8)
	layout.mat[1,] = 1
	layout.mat[2,] = 2
	layout.mat[4:6,2:3] = 3
	layout.mat[4:5,5] = 4
	layout.mat[4,7] = 5 
	layout.mat[8,] = c(6,6,6,NA,7,7,7,7)
	layout.mat[9,5:8] = 8
	layout.mat[11,] = c(NA,9,10,NA,11,11,11,NA)

	LibreReport_StatandTIR_forWeb = try(grid.arrange(grobs=list(
		tableGrob('연속혈당 통계분석',theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(26,'points')), #1 
		tableGrob(Stat.subTitle.remark,theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(26,'points')), #2
		out.tab1_forWeb, #3
		out.tab2_forWeb, #4
		out.tab3_forWeb, #5
		tableGrob('범위 내 시간 (Time in Ranges)',theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(418,'points'),heights=unit(26,'points')), #6
		tableGrob('범위내시간> 시간대별 상세보기',theme=subTitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(438,'points'),heights=unit(26,'points')), #7
		tableGrob('야간: 22-04시 오전: 04-10시 오후: 10-16시 저녁: 16-22시',theme=remark.theme_forWeb,cols=NULL,rows=NULL,widths=unit(438,'points'),heights=unit(26,'points')), #8
		out.TIR[[2]], #9 
		out.tab4_forWeb, #10
		out.TIR.detail[[3]]), #11
		nrow=11,ncol=8,layout_matrix=layout.mat,
		widths=unit(c(15,181,222,20,140,20,258,20),'points'),heights=unit(c(26,26,10,85,85,85*2, 20, 26,26,10,330),'points')),silent=T)

	dev.off() 
	
	if ( class(LibreReport_StatandTIR_forWeb)[1]!='try-error' ) {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_StatandTIR.png',sep='_'))
	} else {
		errCode.sub = c(errCode.sub,'Errr_201')
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

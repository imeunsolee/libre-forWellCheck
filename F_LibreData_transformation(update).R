
##########################################
## LibreData_transformation 
## ---------------------------------------
## input 
## . inFileName : 'NAME_glucose_YYYYMMDD.xlsx' 
## . FinalDate : 리브레 종료일  YYYY-MM-DD 의 형태
## . mod : 1=1기간 분석 2 = 2기간 이상 분석 
## .... ver 2.0 에서는 mod = 2 에서 최대 2개 기간만 출력하도록 제한해둠 #### 
## ---------------------------------------
## ver 2.0 ( 210713 ) 

LibreData_transformation = function( inFileName, FinalDate ) {
	errCode.sub = c()

	### step0 =============================================================================##
	## 데이터 불러오기    

	lines = readLines(inFileName,encoding='UTF-8')[-c(1:2)]
	data = matrix(NA, nrow=(length(lines)-1),ncol=19)
	for ( i in 2:length(lines) ) {

		lines.tmp = strsplit(lines[[i]],split="\"")
		if( grepl('FreeStyle LibreLink',lines.tmp[[1]][1]) ) {
			if( length(lines.tmp[[1]])==1 ) {
				line1 = unlist(strsplit(lines.tmp[[1]],split=','))
				if( length(line1)==18 ) line1[19] = ''

			} else { 

				line1 = c()
				for ( j in 1:length(lines.tmp[[1]]) ) {
					if ( j==2 ) {
						line1.tmp = unlist(lines.tmp[[1]][j])
					} else {
						line1.tmp = unlist(strsplit(lines.tmp[[1]][j],split=','))
					}
					line1 = c(line1,line1.tmp)
				}
				if ( length(line1)==14 ) next
			}
			data[(i-1),] = line1

		} else {

			addtext = ifelse(is.na(unlist(lines.tmp[[1]][1])),'',unlist(lines.tmp[[1]][1]))
			line1[14] = paste(line1[14],addtext,sep=' ')
			if ( length(unlist(lines.tmp))<=1 ) {
				next
			}
			line1 = c(line1,unlist(strsplit(lines.tmp[[1]][2],split=',')))
			if ( length(line1)==18 ) line1 = line1[19] = ''
			data[(i-2),] = line1

		}
	}
	colnames(data) = unlist(strsplit(lines[[1]],split=','))
	data = as.data.frame(data)
	data = data[!is.na(data[,1]),]

	data$dateandtime = as.POSIXct(data[,3],tz='GMT')
	data$date = as.Date(data$dateandtime)
	data$time = format(data$dateandtime,format='%H:%M:%S')


	### step1 =============================================================================##
	## 분석기간 분류 
	subNum = 0
	End.s1 = T
	Datelist = as.character(unique(as.Date(data[which(data[,4]%in%c(0,1)),]$date)))

	date.tmp = c(as.Date(FinalDate,'%Y-%m-%d')-13, as.Date(FinalDate,'%Y-%m-%d'))
	data$sub = NA
	ndays = length(Datelist[Datelist>=date.tmp[1] & Datelist<=date.tmp[2]])

	while ( 1 ) {

		subNum = ( subNum+1 )

		## part1 ## 
        while ( End.s1==T ) {

            if ( ndays==14 ) {
                data[which(data$date>=date.tmp[1] & data$date<=date.tmp[2]),]$sub = subNum

            } else if ( ndays > 0 & ndays < 14 ) {

                FinalDate = max(as.Date(data$date[intersect(which(data$date>=date.tmp[1] & data$date<=date.tmp[2]),which(data[,4]%in%c(0,1)))]))
    #			FinalDate = max(as.Date(data$date[which(data$date>=date.tmp[1] & data$date<=date.tmp[2])]))
                date.tmp1 = c(as.Date(FinalDate,'%Y-%m-%d')-13, as.Date(FinalDate,'%Y-%m-%d'))
    #			ndays.tmp1 = length(unique(as.Date(data$date[intersect(which(data$date>=date.tmp1[1] & data$date<=date.tmp1[2]),which(data[,4]%in%c(0,1)))]))) 
                ndays.tmp1 = length(Datelist[Datelist>=date.tmp1[1] & Datelist<=date.tmp1[2]]) 

                FinalDate = min(as.Date(data$date[which(data$date>=date.tmp[1] & data$date<=date.tmp[2])]))+13
                date.tmp2 = c(as.Date(FinalDate,'%Y-%m-%d')-13, as.Date(FinalDate,'%Y-%m-%d'))
    #			ndays.tmp2 = length(unique(as.Date(data$date[intersect(which(data$date>=date.tmp2[1] & data$date<=date.tmp2[2]),which(data[,4]%in%c(0,1)))]))) 
                ndays.tmp2 = length(Datelist[Datelist>=date.tmp2[1] & Datelist<=date.tmp2[2]])

                if ( ndays.tmp1 > ndays.tmp2 ) {
                    data[which(data$date>=date.tmp1[1] & data$date<=date.tmp1[2]),]$sub = subNum
    #				cat('[경고] 설정된 분석시작일을 포함한 이전 기간으로 분석기간이 변경되었습니다.\n')
    #				cat(paste(' --- 변경된 마지막 분석일:', date.tmp1[2],'\n'))
                    errCode.sub = c(errCode.sub, 'Warn_101') 

                } else if ( ndays.tmp1 <= ndays.tmp2 ) {
                    data[which(data$date>=date.tmp2[1] & data$date<=date.tmp2[2]),]$sub = subNum
    #				cat('[경고] 설정된 분석종료일을 포함한 이후 기간으로 분석기간이 변경되었습니다.\n')
    #				cat(paste(' --- 변경된 마지막 분석일:', date.tmp2[2],'\n'))
                    errCode.sub = c(errCode.sub, 'Warn_102')  

                } else {
                    break
                }

            } else if ( ndays==0 ) {

                FinalDate = max(data[which(data[,4]%in%c(0,1)),]$date)
                date.tmp = c(as.Date(FinalDate,'%Y-%m-%d')-13, as.Date(FinalDate,'%Y-%m-%d'))
                data[which(data$date>=date.tmp[1] & data$date<=date.tmp[2]),]$sub = subNum
    #			cat('[경고] 설정된 분석종료일 이후의 기간으로 분석기간이 변경되었습니다.\n')
    #			cat(paste(' --- 변경된 마지막 분석일:', date.tmp[2],'\n'))
                errCode.sub = c(errCode.sub, 'Warn_103') 

            } else {
    #			cat('[에러] 리브레 데이터가 없어 분석을 종료합니다.\n')
                errCode.sub = c(errCode.sub, 'Errr_101')
                break
            }

            End.s1 = F 

        }

#		break ## 웰체크다이어트프로젝트용 ## / todo ( 확인해보기 )

		## part2 ## 

		## 과거 데이터 자동 탐색 ###
		Datelist = setdiff(as.character(Datelist), unique(as.character(data[!is.na(data$sub),]$date)))
		if ( length(Datelist)==0 ) {
			break
		} else {
			FinalDate = max(Datelist[Datelist<min(data[!is.na(data$sub),]$date,na.rm=T)],na.rm=T)
			
			if ( length(Datelist)==0 ) {
				break
			} else if ( is.na(FinalDate) ) {
				break 
			} else {
				date.tmp = c(as.Date(FinalDate,'%Y-%m-%d')-13, as.Date(FinalDate,'%Y-%m-%d'))
				date.max = NULL
				End.s2 = T 
			}
		}


		while ( End.s2==T ) {

			Datelist.tmp = Datelist[Datelist>=date.tmp[1] & Datelist<=date.tmp[2]]
			ndays = length(Datelist.tmp)
			if ( length(Datelist[Datelist>=date.max[1] & Datelist<=date.max[2]]) < ndays ) {
				# max date 저장, max date보다 길면 update
				date.max = date.tmp
				if ( ndays==14 ) {
					break
				}
			}
			if ( range(Datelist)[1] > (date.max[1]-1) ) {
				End.s2 = F
				break
			}
			date.tmp = ( date.tmp - 1 )
		}
		date.tmp = date.max
		ndays = length(Datelist[Datelist>=date.tmp[1] & Datelist<=date.tmp[2]])
		if ( ndays<10 ) { # 7일 미만으로 바꿀까.. 고민
            End.s1 = F 
            break
        } else {
            End.s1 = T 
        }

	}

	mod = ifelse( max(subNum)==1, 1 , 2)


	### step2 =============================================================================##
	## 기록유형 분류 (자동,스캐닝,식사 등)  

	### 기록유형 = 0 -> 자동 혈당값 
	data$log = ifelse(data[,4]==0,1,0)

	## S2-1. 동일시점기록 요약 

	### 자동 혈당 ---
	data_type0 = data[which(data[,4]==0),c(1:3,20:24,5)]
	colnames(data_type0)[9]='glucose'

	## 스캔 혈당 ---
	data_type1 = data[which(data[,4]==1),c(1:3,20:24,6)]
	colnames(data_type1)[9]='glucose'

	## 음식 기록 ---
	# 아침, 점심, 저녁 분류할 필요 있음 
	data_type5 = data[which(data[,4]==5),c(1:3,20:24,9)]
	colnames(data_type5)[9]='event_eat'

	## 운동 기록 ---
	data_type6_1 = data[which(data[,4]==6),c(1:3,20:24,14)]
	data_type6_1 = data_type6_1[which(data_type6_1[,9]=='운동'),]## 메모로 '운동'이라고 기록한 경우도 포함시켜야하지 않을까 ? ## data_type6_1[grepl('운동',data_type6_1[,9][[1]]),]
	if(dim(data_type6_1)[1]!=0){
		data_type6_1[,9]=1
	}
	colnames(data_type6_1)[9]='event_exercise'

	## 모든 메모 ---
	data_type6_2 = data[which(data[,4]==6),c(1:3,20:24,14)]
	if(dim(data_type6_2[which(data_type6_2[,9]!=''),])[1]!=0){
		data_type6_2 = data_type6_2[which(data_type6_2[,9]!=''),]
	}
	colnames(data_type6_2)[9]='memo'

	## S2-2. merge

	data_type1to5 = merge(data_type1[,-c(1:3)],data_type5[,-c(1:3)],by=c('dateandtime','date','time','sub','log'),all=T)
	data_type6 = merge(data_type6_1[,-c(1:3)],data_type6_2[,-c(1:3)],by=c('dateandtime','date','time','sub','log'),all=T)
	data_type1to6 = merge(data_type1to5,data_type6,by=c('dateandtime','date','time','sub','log'),all=T)

	data_type0to6 = merge(data_type0[,-c(1:3)],data_type1to6,by=c('dateandtime','date','time','sub','log','glucose'),all=T)
	AGPdata = data.frame(data_type0to6[!is.na(data_type0to6$dateandtime),])
	AGPdata$glucose = as.numeric(as.vector(AGPdata$glucose))


	### output =============================================================================##
#	out.sub = 1 # ver1.0 #
	if ( mod !=1 ) {
		out.sub = c(1,2) # ver2.0 # //최대 2개 기간까지만 출력하도록 제한 ## 
	} else {
		out.sub = 1 
	}

	if ( any(AGPdata$sub==1,na.rm=T)==F ) {
#		cat('[경고] 리브레 데이터 변환 완료하였으나, 분석기간에 해당되는 데이터가 없습니다.\n')
		errCode.sub = c(errCode.sub, 'Warn_104')
	}

	return(list(AGPdata = AGPdata[which(AGPdata$sub%in%out.sub),], errCode.sub = errCode.sub, mod=mod))


}
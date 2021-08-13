##########################################
## Libre_x_DW_report
## ---------------------------------------
## input 
## . Target : 당뇨유형 T2DM T1DM T1GDM T2GDM GDM HighRiskDM preDM
## . FinalDate : YYYY-MM-DD
## . inFileName : 리브레뷰에서 다운받은 혈당 데이터 파일명 (홍길동_glucose_2020-8-5.csv) 
## . method : 식후혈당분석방법 TargetValue IncValue
## . mod : 1: 단일기간분석, 2: 과거내역있을 경우 자동 분석 
## ---------------------------------------

Libre_x_DW_report_onW = function( Target, FinalDate, inFileName, method, path0, memberKey ){
# Libre_x_DW_report_forApp = function( CodeFileName, inFileName, imageFileName=NULL, method ){


	### step0 =============================================================================##
	### Install Package
#	strt.time = Sys.time()
	needto_packages = c('ggplot2','gridExtra','grid','gtable','readxl','jsonlite','Cairo','stringr','jsonlite','png','sysfonts','showtextdb','showtext') # 
	for(i in 1:length(needto_packages)){
		if(!needto_packages[i] %in% installed.packages()){
			install.packages( needto_packages[i] , repos='http://cran.r-project.org')
		}
	}

	for(i in 1:length(needto_packages)) {
		for(j in 1:length(.libPaths())) {
			libPath.tmp = .libPaths()[j]
			libPath.err = try(library(needto_packages[i],lib.loc=libPath.tmp,character.only=T),silent=T)
			if(class(libPath.err)!='try-error') {
				next
			}
		}
	}

	# font 설정 # 
#	ttf_import(paths='C:/리브레/Auto30_소스/Font/')
#	loadfonts(quiet=T)
#	fontBoldtype = 'NotoSansCJKkrB'
#	fontRglrtype = 'NotoSansCJKkrR'
	font_add('NotoSansCJKkrB','NanumBarunGothicBold.ttf')
	font_add('NotoSansCJKkrR','NanumBarunGothic.ttf')
	showtext_auto()

	errCode = c()
	outFileNames = c()
	createdtime = as.integer(Sys.time(),format='%Y-%m-%d %H:%M:%S.%OS')

	### step1 =============================================================================##
	### 리브레 데이터 변환 
#	AGPdata = try(LibreData_transformation(inFileName,FinalDate,mod=mod),silent=T)
	AGPdata = try(LibreData_transformation(inFileName,FinalDate),silent=T)
	# 에러 확인용 ***************************************************************************
	if ( class(AGPdata)=='try-error' ) {
#		cat('[에러] 알 수 없는 이유로 리브레 데이터 변환이 중지되었습니다.\n')
		errCode = c(errCode,'Errr_109')
		break
	} else {
		errCode = c(errCode,AGPdata$errCode.sub)
		mod = AGPdata$mod
		AGPdata = AGPdata$AGPdata
	}


	### step2 =============================================================================##
	### 연속혈당 통계분석 및 범위내 시간 
	setwd(path0) ## 이미지 파일 경로 설정 

	### 연속혈당 통계분석 및 범위내시간 
	section1.tmp = try(create_GlucoseStat(data=AGPdata,Target=Target,memberKey=memberKey,createdtime=createdtime,mod=mod), silent=T)
	# 에러 확인용 ****************************************************************************
	if ( class(section1.tmp)=='try-error' ) {
#		cat('[에러] 알 수 없는 이유로 리브레 연속혈당 분석이 종료되었습니다.\n')
		errCode = c(errCode,'Errr_209')
		break
	} else {
		outFileNames = c(outFileNames,section1.tmp$outFileNames)
		errCode = c(errCode,section1.tmp$errCode.sub)
		TargetGoal = section1.tmp$TargetGoal
		TIRstat = section1.tmp$TIRstat
	}

####################### 여기까지 완료 210714 

	### step3 =============================================================================##
	### 24시간 혈당패턴 확인
	data = AGPdata; unit.glucose='mg.dl'

	section2.tmp = try(create_AGPPatternCheck(data=AGPdata,Target=Target,TargetGoal=TargetGoal,TIRstat=TIRstat,memberKey=memberKey,createdtime=createdtime,mod=mod), silent=T)
	# 에러 확인용 ****************************************************************************
	if ( class(section2.tmp)=='try-error' ) {
		errCode = c(errCode,'Errr_309')
		break
	} else {
		outFileNames = c(outFileNames,section2.tmp$outFileNames)
		errCode = c(errCode,section1.tmp$errCode.sub)
	}

	
	### step999 =============================================================================##
	### out
#	print(paste('-------- running time: ',round(difftime(Sys.time(),strt.time,units='sec')),' sec --------',sep=''))
	out.Result.tmp = list(errCode=errCode[!is.na(errCode)], outFileNames_Web=outFileNames[grep('Web_',outFileNames)], outFileNames_App=outFileNames[grep('App_',outFileNames)] )
	out.Result = toJSON(out.Result.tmp)
	return( result = out.Result )


	## 1page SubTitle +++++++++++++
	page1.subtitle_11 = data.frame(x='혈당 통계 및 목표값') 
	page1.subtitle_12 = data.frame(x='범위 내 시간') 
	page1.subtitle_21 = data.frame(x='24시간 연속 혈당 프로필(AGP)')  
	page1.subtitle_31 = data.frame(x='일일 혈당 프로필')

	page1.subtitle.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrB',
		core=list(bg_params=list(fill='#000000',col=NA),
			fg_params=list(hjust=0,x=0.01,col='#ffffff',fontsize=13.5,fontface='bold'))
	)
	page1.subtitle.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill='#000000',col=NA),
					fg_params=list(hjust=0,x=0.01,col='#ffffff',fontsize=25,fontface='bold'))
	)

	## 1page Remark +++++++++++++
	page1.remark_21_forApp = paste(strwrap(data.frame(x='AGP는 보고서 기간의 혈당 값을 요약한 것으로, 중앙값(50%) 및 기타 백분위수가 하루에 발생한 것처럼 함께 표시됩니다.'),width=63),collapse='\n')
	page1.remark_21_forWeb = data.frame(x='AGP는 보고서 기간의 혈당 값을 요약한 것으로, 중앙값(50%) 및 기타 백분위수가 하루에 발생한 것처럼 함께 표시됩니다.')
#	page1.remark_21 = data.frame(x='AGP는 보고서 기간의 혈당 값을 요약한 것으로, 중앙값(50%) 및 \n기타 백분위수가 하루에 발생한 것처럼 함께 표시됩니다.')
#	page1.remark_22 = data.frame(c1='■',c2='25~75백분위수 면적은 일상적인 생활패턴에 의한 혈당패턴으로 해석됩니다.')
	page1.remark_22 = data.frame(c1='■',c2='25~75백분위수 면적은 일상적인 생활패턴에 의해 나타납니다.')
#	page1.remark_23 = data.frame(c1='■',c2='5~95백분위수 면적은 어느 특별한 이벤트에 의한 혈당패턴으로 해석됩니다.')
	page1.remark_23 = data.frame(c1='■',c2='5~95백분위수 면적은 어느 특별한 이벤트에 의해 나타납니다.')
	
	page1.remark_31 = data.frame(x='각 일일 프로필은 자정에서 자정까지의 기간을 나타내며 왼쪽 상단 모서리에 날짜가 표시됩니다.')

	## forWeb ==== ## 
	page1.remark21.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_parapms=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#767e89',fontsize=12,fontface='plain'))
	)
	page1.remark22.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_parapms=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=c(0.3,0),vjust=0.5,col=c('#a2b5d4','#767e89'),fontsize=12,fontface='plain'))
	)
	page1.remark23.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_parapms=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=c(0.3,0),vjust=0.5,col=c('#dfe3ed','#767e89'),fontsize=12,fontface='plain'))
	)
	## forApp ==== ## 
	page1.remark21.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_parapms=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#767e89',fontsize=17,fontface='plain'))
	)
	page1.remark22.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_parapms=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=c(0.03,0),vjust=0.5,col=c('#a2b5d4','#767e89'),fontsize=17,fontface='plain'))
	)
	page1.remark23.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_parapms=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=c(0.03,0),vjust=0.5,col=c('#dfe3ed','#767e89'),fontsize=17,fontface='plain'))
	)

	## 1page. Table format +++++++++++++ 

	page1.Tab_111a_forApp = tableGrob(page1.tmp$out.Tab_11_source1a,theme=page1.tmp$out.Tab_11_source1a.theme,cols=NULL,rows=NULL,
		widths=unit(c(341,171),'points'),heights=unit(c(7.5,38.4,38.4,7.5),'points'))
	page1.Tab_111a_forApp.f = gtable_add_grob(page1.Tab_111a_forApp,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=4,b=4,l=1,r=2)

	page1.Tab_11_source1a.theme_forWeb = page1.tmp$out.Tab_11_source1a.theme
	page1.Tab_11_source1a.theme_forWeb$core$fg_params$fontsize = c(1,12,12,1)
	page1.Tab_111a_forWeb = tableGrob(page1.tmp$out.Tab_11_source1a,theme=page1.Tab_11_source1a.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(2,28,28,2),'points'))
	page1.Tab_111a_forWeb.f = gtable_add_grob(page1.Tab_111a_forWeb,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=4,b=4,l=1,r=2)

	page1.Tab_111b_forApp = tableGrob(page1.tmp$out.Tab_11_source1b,theme=page1.tmp$out.Tab_11_source1b.theme,cols=NULL,rows=NULL,
		widths=unit(512,'points'),heights=unit(45.5,'points'))

	page1.Tab_11_source1b.theme_forWeb = page1.tmp$out.Tab_11_source1b.theme
	page1.Tab_11_source1b.theme_forWeb$core$fg_params$fontsize = 12
	page1.Tab_111b_forWeb = tableGrob(page1.tmp$out.Tab_11_source1b,theme=page1.Tab_11_source1b.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(483,'points'),heights=unit(28.5,'points'))

	page1.Tab_111c_forApp = tableGrob(page1.tmp$out.Tab_11_source1c,theme=page1.tmp$out.Tab_11_source1c.theme,cols=NULL,rows=NULL,
		widths=unit(c(256,256),'points'),heights=unit(c(39.5,rep(28.5,5)),'points'))
	page1.Tab_111c_forApp.f = gtable_add_grob(page1.Tab_111c_forApp,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=1,b=1,l=1,r=2)
	page1.Tab_111c_forApp.f = gtable_add_grob(page1.Tab_111c_forApp.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=6,b=6,l=1,r=2) 

	page1.Tab_11_source1c.theme_forWeb = page1.tmp$out.Tab_11_source1c.theme
	page1.Tab_11_source1c.theme_forWeb$core$fg_params$fontsize = rep(12,6)
	page1.Tab_111c_forWeb = tableGrob(page1.tmp$out.Tab_11_source1c,theme=page1.Tab_11_source1c.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(21,rep(19.2,5)),'points'))
	page1.Tab_111c_forWeb.f = gtable_add_grob(page1.Tab_111c_forWeb,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=1,b=1,l=1,r=2)
	page1.Tab_111c_forWeb.f = gtable_add_grob(page1.Tab_111c_forWeb.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc'),gp=gpar(col='#4d5262')),t=6,b=6,l=1,r=2)  

	page1.Tab_112_forApp = tableGrob(page1.tmp$out.Tab_11_source2,theme=page1.tmp$out.Tab_11_source2.theme,cols=NULL,rows=NULL,
		widths=unit(c(512),'points'),heights=unit(c(38.4),'points'))

	page1.Tab_11_source2.theme_forWeb = page1.tmp$out.Tab_11_source2.theme
	page1.Tab_11_source2.theme_forWeb$core$fg_params$fontsize = 12
	page1.Tab_112_forWeb = tableGrob(page1.tmp$out.Tab_11_source2,theme=page1.Tab_11_source2.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(483),'points'),heights=unit(c(19),'points'))

	page1.Tab_113_forApp = tableGrob(page1.tmp$out.Tab_11_source3,theme=page1.tmp$out.Tab_11_source3.theme,cols=NULL,rows=NULL,
		widths=unit(c(256,256),'points'),heights=unit(c(2.8,5.7,rep(38.4,2)),'points'))
	page1.Tab_113_forApp.f = gtable_add_grob(page1.Tab_113_forApp,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=1,b=1,l=1,r=2)

	page1.Tab_11_source3.theme_forWeb = page1.tmp$out.Tab_11_source3.theme
	page1.Tab_11_source3.theme_forWeb$core$fg_params$fontsize = c(1,1,13,13)
	page1.Tab_113_forWeb = tableGrob(page1.tmp$out.Tab_11_source3,theme=page1.Tab_11_source3.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(2,6,rep(26,2)),'points'))
	page1.Tab_113_forWeb.f = gtable_add_grob(page1.Tab_113_forWeb,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=1,b=1,l=1,r=2)

	page1.Tab_114_forApp = tableGrob(page1.tmp$out.Tab_11_source4,theme=page1.tmp$out.Tab_11_source4.theme,cols=NULL,rows=NULL,
		widths=unit(c(512),'points'),heights=unit(c(37),'points'))

	page1.Tab_11_source4.theme_forWeb = page1.tmp$out.Tab_11_source4.theme
	page1.Tab_11_source4.theme_forWeb$core$fg_params$fontsize = 12
	page1.Tab_114_forWeb = tableGrob(page1.tmp$out.Tab_11_source4,theme=page1.Tab_11_source4.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(483),'points'),heights=unit(c(21),'points'))

	page1.Tab_115_forApp = tableGrob(page1.tmp$out.Tab_11_source5,theme=page1.tmp$out.Tab_11_source5.theme,cols=NULL,rows=NULL,
		widths=unit(c(256,256),'points'),heights=unit(c(37),'points'))

	page1.Tab_11_source5.theme_forWeb = page1.tmp$out.Tab_11_source5.theme
	page1.Tab_11_source5.theme_forWeb$core$fg_params$fontsize = 13
	page1.Tab_115_forWeb = tableGrob(page1.tmp$out.Tab_11_source5,theme=page1.Tab_11_source5.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(300,183),'points'),heights=unit(c(26),'points'))

	page1.Tab_116_forApp = tableGrob(page1.tmp$out.Tab_11_source6,theme=page1.tmp$out.Tab_11_source6.theme,cols=NULL,rows=NULL,
		widths=unit(c(512),'points'),heights=unit(rep(28.5,2),'points'))

	page1.Tab_11_source6.theme_forWeb = page1.tmp$out.Tab_11_source6.theme
	page1.Tab_11_source6.theme_forWeb$core$fg_params$fontsize = c(12,12)
	page1.Tab_116_forWeb = tableGrob(page1.tmp$out.Tab_11_source6,theme=page1.Tab_11_source6.theme_forWeb,cols=NULL,rows=NULL,
		widths=unit(c(483),'points'),heights=unit(rep(21,2),'points'))


	## 1page. Graph format +++++++++++++ 

	page1.tmp$out.Plot_21_forWeb$theme$axis.text.y$size = 12 
	page1.tmp$out.Plot_21_forWeb$theme$axis.text.x$size = 12 
	
	#### LibreReport_Stat - overall - print out - ####################

	setwd(path0)
	createdtime = as.integer(Sys.time(),format='%Y-%m-%d_%H:%M:%S.%OS')

	## forApp ==== ##
 	CairoPNG(filename=paste(memberKey,createdtime,'App_Stat.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=832, units='px',dpi=96)
	LibreReport_Stat_forApp = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_11,theme=page1.subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
									page1.Tab_111a_forApp.f,page1.Tab_111b_forApp,page1.Tab_111c_forApp.f,
									page1.Tab_112_forApp,page1.Tab_113_forApp.f,page1.Tab_114_forApp,page1.Tab_115_forApp,page1.Tab_116_forApp), 
						nrow=9, ncol=1, layout_matrix=rbind(c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9)),
						widths=unit(c(512),'points'),heights=unit(c(50,91.8,45.5,182,38.4,85.3,37,37,57),'points')),silent=T)
	dev.off()

	if(class(LibreReport_Stat_forApp)[1]!='try-error') {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_Stat.png',sep='_'))
	}

	## forWeb ==== ##
#	CairoPNG(filename='Libre_x_DW_report_item1_W.png',family='NotoSansCJKkrR', family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=832, units='px',dpi=96)
#	item1_forWeb = grid.arrange(grobs=list(tableGrob(page1.subtitle_11,theme=page1.subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(780,'points'),heights=unit(50,'points')),
#									page1.Tab_111a_forWeb.f,page1.Tab_111b_forWeb,page1.Tab_111c_forWeb.f,
#									page1.Tab_112_forWeb,page1.Tab_113_forWeb.f,page1.Tab_114_forWeb,page1.Tab_115_forWeb,page1.Tab_116_forWeb), 
#						nrow=9, ncol=1, layout_matrix=rbind(c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9)),
#						widths=unit(c(780),'points'),heights=unit(c(50,91.8,45.5,182,38.4,85.3,37,37,57),'points')) 
#	dev.off()


	#### LibreReport_TIR - print out - ####################

	## forApp ==== ##
 	CairoPNG(filename=paste(memberKey,createdtime,'App_TIR.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=832, units='px',dpi=96)
	LibreReport_TIR_forApp = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_12,theme=page1.subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
									page1.tmp$out.Plot_12), 
						nrow=3, ncol=1, layout_matrix=rbind(c(1),NA,c(2)),
						widths=unit(c(512),'points'),heights=unit(c(50,62,512),'points')),silent=T)
	dev.off()

	if(class(LibreReport_TIR_forApp)[1]!='try-error') {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_TIR.png',sep='_'))
	}

	## forWeb ==== ##
#	CairoPNG(filename='Libre_x_DW_report_item2_W.png',family='NotoSansCJKkrR', family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=760, height=832, units='px',dpi=96)
#	item2_forWeb = grid.arrange(grobs=list(tableGrob(page1.subtitle_12,theme=page1.subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(570,'points'),heights=unit(50,'points')),
#									page1.tmp$out.Plot_12), 
#						nrow=3, ncol=1, layout_matrix=rbind(c(1),NA,c(2)),
#						widths=unit(c(570),'points'),heights=unit(c(50,62,512),'points')) # auto30 cm to points
#	dev.off()

	## forWeb ==== ##
 	CairoPNG(filename=paste(memberKey,createdtime,'Web_StatandTIR.png',sep='_'), family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=544, units='px',dpi=96)
	LibreReport_StatandTIR_forWeb = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_11,theme=page1.subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(483,'points'),heights=unit(28.5,'points')),
										tableGrob(page1.subtitle_12,theme=page1.subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(357,'points'),heights=unit(28.5,'points')),
										page1.Tab_111a_forWeb.f,page1.Tab_111b_forWeb,page1.Tab_111c_forWeb.f,page1.Tab_112_forWeb,page1.Tab_113_forWeb.f,page1.Tab_114_forWeb,page1.Tab_115_forWeb,page1.Tab_116_forWeb,
										page1.tmp$out.Plot_12_forWeb),
								nrow=10, ncol=3, layout_matrix=rbind(c(1,NA,2),NA,c(3,NA,11),c(4,NA,11),c(5,NA,11),c(6,NA,11),c(7,NA,11),c(8,NA,11),c(9,NA,11),c(10,NA,11)),
								widths=unit(c(483,36,357),'points'),heights=unit(c(28.5,6,60,28.5,117,19,60,21,26,42),'points')),silent=T)
	dev.off()

	if(class(LibreReport_StatandTIR_forWeb)[1]!='try-error') {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_StatandTIR.png',sep='_'))
	}

	#### item3 - 24HR AGP graph - print out - ####################
 
	## forApp ==== ##
 #	CairoPNG(filename='LibreReport_AGP_A.png', family='NotoSansCJKkrR', family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=1112, units='px',dpi=96)
 	CairoPNG(filename=paste(memberKey,createdtime,'App_AGP.png',sep='_'), family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=1112, units='px',dpi=96)
	LibreReport_AGP_forApp = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_21,theme=page1.subtitle.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),#1
									tableGrob(page1.remark_21_forApp,theme=page1.remark21.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(58,'points')),#2
									tableGrob(page1.remark_22,theme=page1.remark22.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(24,488),'points'),heights=unit(27,'points')),#3
									tableGrob(page1.remark_23,theme=page1.remark23.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(24,488),'points'),heights=unit(27,'points')),#4
									page1.tmp$out.Plot_21_forApp), #5
						nrow=6, ncol=1, layout_matrix=rbind(c(1),NA,c(2),c(3),c(4),c(5)),
						widths=unit(c(512),'points'),heights=unit(c(50,6,58,27,27,666),'points')),silent=T)

	dev.off()
	
	if(class(LibreReport_AGP_forApp)[1]!='try-error') {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_AGP.png',sep='_'))
	}

	## forWeb ==== ##
 	CairoPNG(filename=paste(memberKey,createdtime,'Web_AGP.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=496, units='px',dpi=96)
	LibreReport_AGP_forWeb = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_21,theme=page1.subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
									tableGrob(page1.remark_21_forWeb,theme=page1.remark21.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(21.5,'points')),#2
									tableGrob(page1.remark_22,theme=page1.remark22.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(24,852),'points'),heights=unit(21,'points')),#3
									tableGrob(page1.remark_23,theme=page1.remark23.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(24,852),'points'),heights=unit(21,'points')),#4
									page1.tmp$out.Plot_21_forWeb), #5
						nrow=6, ncol=3, layout_matrix=rbind(c(1,1,1),NA,c(2,2,2),c(3,3,3),c(4,4,4),c(NA,5,NA)),
						widths=unit(c(25,826,25),'points'),heights=unit(c(28.5,4,21.5,21,21,276),'points')),silent=T)

	dev.off()

	if(class(LibreReport_AGP_forWeb)[1]!='try-error') {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_AGP.png',sep='_'))
	}

	#### item4 - daytoday graph - print out - ####################

	## forWeb ==== ## 
 	CairoPNG(filename=paste(memberKey,createdtime,'Web_DailyGP.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=404, units='px',dpi=96)
	LibreReport_DailyGP_forWeb = try(grid.arrange(grobs=list(tableGrob(page1.subtitle_31,theme=page1.subtitle.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#1
									tableGrob(page1.remark_31,theme=page1.remark21.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(21.5,'points')),#2
									page1.tmp$out.Plot_31[[1]],page1.tmp$out.Plot_31[[2]]), #3,4
						nrow=7, ncol=1, layout_matrix=rbind(c(1),NA,c(2),NA,c(3),c(4),NA),
						widths=unit(c(876),'points'),heights=unit(c(28.5,6,21.5,3,120,120,4),'points')),silent=T) 

	dev.off()

	if(class(LibreReport_DailyGP_forWeb)[1]!='try-error') {
		outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DailyGP.png',sep='_'))
	}

	### step3 =============================================================================##
	### 리브레 검사결과지 2-3page

	page2.timelogo1 = data.frame(c1='오전',c2='04:00-10:00')
	page2.timelogo2 = data.frame(c1='정오',c2='10:00-16:00')
	page2.timelogo3 = data.frame(c1='저녁',c2='16:00-22:00')

	page2.timelogo.theme_forWeb = ttheme_minimal( base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
		core=list(bg_params=list(fill=NA,col=NA),
				fg_params=list(hjust=c(1,0),x=c(0.9,0.01),vjust=0.5,col=c('#122747'),fontsize=c(12,10),fontface=c('plain','plain')))
	)  
	page2.timelogo.theme_forApp = ttheme_minimal( base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
		core=list(bg_params=list(fill=NA,col=NA),
				fg_params=list(hjust=0,x=c(0,0.01),vjust=0.5,col=c('#122747'),fontsize=c(22,20),fontface=c('plain','plain')))
	)

	#### breakfast
	page2.sub1 = GlucosePattern_clustering_onW(daysAZ=unique(AGPdata$date),data=AGPdata,mealtime=1,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut)#,timelogo=timelogo1)
	#### lunch 
	page2.sub2 = GlucosePattern_clustering_onW(daysAZ=unique(AGPdata$date),data=AGPdata,mealtime=2,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut)#,timelogo=timelogo2)
	#### dinner 
	page2.sub3 = GlucosePattern_clustering_onW(daysAZ=unique(AGPdata$date),data=AGPdata,mealtime=3,method=method,PPG.L=PPG.L,PPG.U=PPG.U,Target=Target,IncCut=IncCut)#,timelogo=timelogo3)

	errCode = c(errCode,unique(c(page2.sub1$errCode.sub,page2.sub2$errCode.sub,page2.sub3$errCode.sub)))

	## 2page Title +++++++++++++ 	
	page2.title2 = t(data.frame(r1='혈당 패턴에 따른 식사 리스트',r2='분석 기간 동안 식사에 의한 혈당 변동 패턴을 분석하여 보여줍니다.'))
	## forWeb ==== ## 
	page2.title2.theme_forWeb = ttheme_minimal( base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
		core = list(bg_paramse=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col=c('#122747','#4d5262'),fontsize=c(13.5,12),fontface=c('plain','plain')))
	)
	## forApp ==== ## 
	page2.title2.theme_forApp = ttheme_minimal( base_family=c('NotoSansCJKkrB','NotoSansCJKkrR'),
		core = list(bg_paramse=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=c(0.5,0),col=c('#122747','#4d5262'),fontsize=c(25,17),fontface=c('plain','plain')))
	)

	## 2page SubTitle +++++++++++++
	page2.subtitle1 = data.frame('권장되는 식사 리스트')
	page2.subtitle2 = data.frame('주의가 필요한 식사 리스트')
	page2.subtitle3 = data.frame('피해야 할 식사 리스트')

	## forWeb ==== ## 
	page2.subtitle1.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrB',
		core=list(bg_params=list(fill='#00b050',col=NA),
				fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#ffffff',fontsize=13.5,fontface='plain'))
	)
	page2.subtitle2.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrB',
		core=list(bg_params=list(fill='#ff6600',col=NA),
				fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#ffffff',fontsize=13.5,fontface='plain'))
	)
	page2.subtitle3.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrB',
		core=list(bg_params=list(fill='#ff0000',col=NA),
				fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#ffffff',fontsize=13.5,fontface='plain'))
	)

	## forApp ==== ## 
	page2.subtitle1.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrB',
		core=list(bg_params=list(fill='#00b050',col=NA),
				fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#ffffff',fontsize=25,fontface='plain'))
	)
	page2.subtitle2.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrB',
		core=list(bg_params=list(fill='#ff6600',col=NA),
				fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#ffffff',fontsize=25,fontface='plain'))
	)
	page2.subtitle3.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrB',
		core=list(bg_params=list(fill='#ff0000',col=NA),
				fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#ffffff',fontsize=25,fontface='plain'))
	)


	## 2page TabTitle +++++++++++++

	if(method=='TargetValue') {

#		page2.Tabtitle1 = paste(strwrap(data.frame(x=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL에서 벗어나지 않은 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n')
#		page2.Tabtitle2 = paste(strwrap(data.frame(x=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL보다 상승하나, 식후 3시간 내에 식전 혈당으로 회복되었던 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n')
		page2.Tabtitle1 = data.frame(x=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 에서 벗어나지 않은 식사 기록을 보여줍니다.',sep=''))
		page2.Tabtitle2 = data.frame(x=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 보다 상승하나, 식후 3시간 내에 식전 혈당으로 회복되었던 식사 기록을 보여줍니다.',sep=''))
#temp_0219		page2.Tabtitle3 = paste(strwrap(data.frame(c1='고혈당 유지',c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL보다 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n') 
		page2.Tabtitle3l = data.frame(c1='고혈당 유지') 
#		page2.Tabtitle3r = paste(strwrap(data.frame(c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL보다 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep='')),width=43),collapse='\n') 
		page2.Tabtitle3r = data.frame(c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 보다 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep='')) 
#temp_2019		page2.Tabtitle4 = paste(strwrap(data.frame(c1='혈당 스파이크',c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL보다 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n')
		page2.Tabtitle4l = data.frame(c1='혈당 스파이크')
#		page2.Tabtitle4r = paste(strwrap(data.frame(c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL보다 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep='')),width=43),collapse='\n')
		page2.Tabtitle4r = data.frame(c2=paste('식후 혈당이 ',PPG.L,'-',PPG.U,' mg/dL 보다 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep=''))

	} else if (method=='IncValue') {

#		page2.Tabtitle1 = paste(strwrap(data.frame(x=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 이내로 상승된 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n')
#		page2.Tabtitle2 = paste(strwrap(data.frame(x=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL보다 높게 상승하나, 식후 3시간 내에 식전 혈당으로 회복되었던 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n')
		page2.Tabtitle1 = data.frame(x=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 이내로 상승된 식사 기록을 보여줍니다.',sep=''))
		page2.Tabtitle2 = data.frame(x=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 보다 높게 상승하나, 식후 3시간 내에 식전 혈당으로 회복되었던 식사 기록을 보여줍니다.',sep=''))
##temp_0219		page2.Tabtitle3 = paste(strwrap(data.frame(c1='고혈당 유지',c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL보다 높게 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n')
		page2.Tabtitle3l = data.frame(c1='고혈당 유지')
#		page2.Tabtitle3r = paste(strwrap(data.frame(c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL보다 높게 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep='')),width=45),collapse='\n')
		page2.Tabtitle3r = data.frame(c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 보다 높게 상승한 후, 식후 3시간 내에 식전 혈당으로 회복되지 못한 식사 기록을 보여줍니다.',sep=''))
#temp_0219		page2.Tabtitle4 = paste(strwrap(data.frame(c1='혈당 스파이크',c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL보다 높게 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep='')),width=63),collapse='\n')
		page2.Tabtitle4l = data.frame(c1='혈당 스파이크')
#		page2.Tabtitle4r = paste(strwrap(data.frame(c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL보다 높게 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep='')),width=45),collapse='\n')
		page2.Tabtitle4r = data.frame(c2=paste('식후 혈당의 최고값이 식전 혈당 대비 ', IncCut,' mg/dL 보다 높게 상승한 후, 식전 혈당보다 낮은 수준으로 하락 후 재상승된 식사 기록을 보여줍니다.',sep=''))
	}
	page2.Tabtitle1.A = paste(strwrap(page2.Tabtitle1,width=63),collapse='\n')
	page2.Tabtitle2.A = paste(strwrap(page2.Tabtitle2,width=63),collapse='\n')
	page2.Tabtitle3r.A = paste(strwrap(page2.Tabtitle3r,width=45),collapse='\n')
	page2.Tabtitle4r.A = paste(strwrap(page2.Tabtitle4r,width=45),collapse='\n')
	page2.Tabtitle1.W = page2.Tabtitle1
	page2.Tabtitle2.W = page2.Tabtitle2
	page2.Tabtitle3r.W = page2.Tabtitle3r
	page2.Tabtitle4r.W = page2.Tabtitle4r
	

	## forWeb ==== ##
	page2.Tabtitle1.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=12,fontface='plain'))
	)
	page2.Tabtitle3l.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.02,vjust=0.5,col='#122747',fontsize=12,fontface='plain'))
	)
	page2.Tabtitle3r.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=11,fontface='plain'))
	)

	## forApp ==== ## 
	page2.Tabtitle1.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=17,fontface='plain'))
	)

	page2.Tabtitle3l.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#122747',fontsize=23,fontface='plain'))
	)
	page2.Tabtitle3r.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=NA,col=NA),
					fg_params=list(hjust=0,x=0.01,vjust=0.5,col='#4d5262',fontsize=17,fontface='plain'))
	)

	## 식후혈당 권장 평균 상승폭 ##  	
	
#	page2.out.Tab_11_sub1 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub1$avgInc_Total),round(page2.sub1$avgInc_Total),'-'),c3='mg/dL')
#	page2.out.Tab_11_sub2 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub2$avgInc_Total),round(page2.sub2$avgInc_Total),'-'),c3='mg/dL')
#	page2.out.Tab_11_sub3 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub3$avgInc_Total),round(page2.sub3$avgInc_Total),'-'),c3='mg/dL')
	page2.out.Tab_11_sub1 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub1$avgInc_byPtrn[1]),round(page2.sub1$avgInc_byPtrn[1]),'-'),c3='mg/dL')
	page2.out.Tab_11_sub2 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub2$avgInc_byPtrn[1]),round(page2.sub2$avgInc_byPtrn[1]),'-'),c3='mg/dL')
	page2.out.Tab_11_sub3 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub3$avgInc_byPtrn[1]),round(page2.sub3$avgInc_byPtrn[1]),'-'),c3='mg/dL')

	## forWeb ==== ## 
	page2.out.Tab_11_sub.theme_forWeb = ttheme_minimal( base_family=c('NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrR'), 
		core = list(bg_params=list(fill='#e3e3e3',col=NA),#col='#122747'),
					fg_params=list(hjust=c(-0.1,0,-0.1),x=c(0,0.1,0),vjust=c(0,0,0),y=c(0.35,0.33,0.35),col=c('#122747','#0096ff','#122747'),
									fontsize=c(13,13.5,10),fontface=c('plain','plain','plain')))
	)
	## forApp ==== ## 
	page2.out.Tab_11_sub.theme_forApp = ttheme_minimal( base_family=c('NotoSansCJKkrB','NotoSansCJKkrB','NotoSansCJKkrR'),  
		core = list(bg_params=list(fill='#e3e3e3',col=NA),#col='#122747'),
					fg_params=list(hjust=c(-0.1,0,-0.1),x=c(0,0.1,0),vjust=c(0,0,0),y=c(0.35,0.33,0.35),col=c('#122747','#0096ff','#122747'),
									fontsize=c(21,23,17),fontface=c('plain','plain','plain')))
	)
	page2.out.Tab_11_sub_null.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill=c('#e3e3e3'),col=NA),
					fg_params=list(hjust=0,x=0.02,vjust=0.5,col=c('#122747'),fontsize=c(24),fontface='plain'))
	)

	good_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당을 안정화 시키는 식사 기록이 없었어요 ㅠㅠ',sep='') }

	if(!is.na(page2.sub1$avgInc_byPtrn[1])) {
		page2.out.Tab_11_sub1.Af = tableGrob(page2.out.Tab_11_sub1,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_11_sub1.Af = tableGrob(good_null_msg(time=1),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(77,'points'))
	}
	if(!is.na(page2.sub2$avgInc_byPtrn[1])) {
		page2.out.Tab_11_sub2.Af = tableGrob(page2.out.Tab_11_sub2,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_11_sub2.Af = tableGrob(good_null_msg(time=2),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(77,'points'))
	}
	if(!is.na(page2.sub3$avgInc_byPtrn[1])) {
		page2.out.Tab_11_sub3.Af = tableGrob(page2.out.Tab_11_sub3,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_11_sub3.Af = tableGrob(good_null_msg(time=3),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(77,'points'))
	}
	
	page2.out.Tab_11_sub1.Wf = tableGrob(page2.out.Tab_11_sub1,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_11_sub2.Wf = tableGrob(page2.out.Tab_11_sub2,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_11_sub3.Wf = tableGrob(page2.out.Tab_11_sub3,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))


	## 식후혈당 주의,지양1,지양2 식사 평균 상승폭 ##  	

	page2.out.Tab_21_sub1 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub1$avgInc_byPtrn[2]),round(page2.sub1$avgInc_byPtrn[2]),'-'),c3='mg/dL')
	page2.out.Tab_21_sub2 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub2$avgInc_byPtrn[2]),round(page2.sub2$avgInc_byPtrn[2]),'-'),c3='mg/dL')
	page2.out.Tab_21_sub3 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub3$avgInc_byPtrn[2]),round(page2.sub3$avgInc_byPtrn[2]),'-'),c3='mg/dL')

	page2.out.Tab_31_sub1 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub1$avgInc_byPtrn[3]),round(page2.sub1$avgInc_byPtrn[3]),'-'),c3='mg/dL')
	page2.out.Tab_31_sub2 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub2$avgInc_byPtrn[3]),round(page2.sub2$avgInc_byPtrn[3]),'-'),c3='mg/dL')
	page2.out.Tab_31_sub3 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub3$avgInc_byPtrn[3]),round(page2.sub3$avgInc_byPtrn[3]),'-'),c3='mg/dL')

	page2.out.Tab_32_sub1 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub1$avgInc_byPtrn[4]),round(page2.sub1$avgInc_byPtrn[4]),'-'),c3='mg/dL')
	page2.out.Tab_32_sub2 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub2$avgInc_byPtrn[4]),round(page2.sub2$avgInc_byPtrn[4]),'-'),c3='mg/dL')
	page2.out.Tab_32_sub3 = data.frame(c1='식후 혈당의 평균 상승폭',c2=ifelse(!is.na(page2.sub3$avgInc_byPtrn[4]),round(page2.sub3$avgInc_byPtrn[4]),'-'),c3='mg/dL')

	page2.out.Tab_21_sub1.Wf = tableGrob(page2.out.Tab_21_sub1,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_21_sub2.Wf = tableGrob(page2.out.Tab_21_sub2,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_21_sub3.Wf = tableGrob(page2.out.Tab_21_sub3,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))

	page2.out.Tab_31_sub1.Wf = tableGrob(page2.out.Tab_31_sub1,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_31_sub2.Wf = tableGrob(page2.out.Tab_31_sub2,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_31_sub3.Wf = tableGrob(page2.out.Tab_31_sub3,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))

	page2.out.Tab_32_sub1.Wf = tableGrob(page2.out.Tab_32_sub1,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_32_sub2.Wf = tableGrob(page2.out.Tab_32_sub2,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))
	page2.out.Tab_32_sub3.Wf = tableGrob(page2.out.Tab_32_sub3,theme=page2.out.Tab_11_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(168,56,56),'points'),heights=unit(28,'points'))

	warn_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당을 높인 식사 기록이 없었어요 ^_^',sep='') }

	if(!is.na(page2.sub1$avgInc_byPtrn[2])) {
		page2.out.Tab_21_sub1.Af = tableGrob(page2.out.Tab_21_sub1,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_21_sub1.Af = tableGrob(warn_null_msg(time=1),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}
	if(!is.na(page2.sub2$avgInc_byPtrn[2])) {
		page2.out.Tab_21_sub2.Af = tableGrob(page2.out.Tab_21_sub2,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_21_sub2.Af = tableGrob(warn_null_msg(time=2),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}
	if(!is.na(page2.sub3$avgInc_byPtrn[2])) {
		page2.out.Tab_21_sub3.Af = tableGrob(page2.out.Tab_21_sub3,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_21_sub3.Af = tableGrob(warn_null_msg(time=3),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}

	badh_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n고혈당이 지속된 식사 기록이 없었어요 ^_^',sep='') }

	if(!is.na(page2.sub1$avgInc_byPtrn[3])) {
		page2.out.Tab_31_sub1.Af = tableGrob(page2.out.Tab_31_sub1,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_31_sub1.Af = tableGrob(badh_null_msg(time=1),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}
	if(!is.na(page2.sub2$avgInc_byPtrn[3])) {
		page2.out.Tab_31_sub2.Af = tableGrob(page2.out.Tab_31_sub2,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_31_sub2.Af = tableGrob(badh_null_msg(time=2),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}
	if(!is.na(page2.sub3$avgInc_byPtrn[3])) {
		page2.out.Tab_31_sub3.Af = tableGrob(page2.out.Tab_31_sub3,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_31_sub3.Af = tableGrob(badh_null_msg(time=3),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}

	bads_null_msg = function(time) { paste('분석기간 ', c('오전','정오','저녁')[time],' 식사 중\n혈당스파이크를 일으킨 식사 기록이 없었어요 ^_^',sep='') }

	if(!is.na(page2.sub1$avgInc_byPtrn[4])) {
		page2.out.Tab_32_sub1.Af = tableGrob(page2.out.Tab_32_sub1,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_32_sub1.Af = tableGrob(bads_null_msg(time=1),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}
	if(!is.na(page2.sub2$avgInc_byPtrn[4])) {
		page2.out.Tab_32_sub2.Af = tableGrob(page2.out.Tab_32_sub2,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_32_sub2.Af = tableGrob(bads_null_msg(time=2),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}
	if(!is.na(page2.sub3$avgInc_byPtrn[4])) {
		page2.out.Tab_32_sub3.Af = tableGrob(page2.out.Tab_32_sub3,theme=page2.out.Tab_11_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(307,102.5,102.5),'points'),heights=unit(50,'points'))
	} else {
		page2.out.Tab_32_sub3.Af = tableGrob(bads_null_msg(time=3),theme=page2.out.Tab_11_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(77),'points'))
	}

	page2.out.Tab_1_sub.theme_forWeb = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=c(NA,'#e3e3e3'),col=NA),#col='#122747'),
					fg_params=list(hjust=0,x=0.02,vjust=0.5,col='#122747',fontsize=13,fontface='plain'))
	)
	page2.out.Tab_1_sub.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrR',
		core = list(bg_params=list(fill=c(NA,'#e3e3e3'),col=NA),#col='#122747'),
					fg_params=list(hjust=0,x=0.02,vjust=0.5,col='#122747',fontsize=20,fontface='plain'))
	)
	out.Tab_spaceitem.Wf = tableGrob(data.frame(''),theme=ttheme_minimal(core=list(bg_params=list(fill=NA,col=NA))),cols=NULL,rows=NULL,widths=unit(18,'points'),heights=unit(1,'points'))
	out.Tab_nullitem.Wf = tableGrob(data.frame(''),theme=ttheme_minimal(core=list(bg_params=list(fill=NA,col=NA))),cols=NULL,rows=NULL,widths=unit(280,'points'),heights=unit(1,'points'))

	page2.out.Tab_1_sub_null.theme_forApp = ttheme_minimal( base_family='NotoSansCJKkrB',
		core = list(bg_params=list(fill=c(NA),col=NA),
					fg_params=list(hjust=0,x=0.02,vjust=0.5,col=c('#767e89'),fontsize=c(20),fontface='plain'))
	)

	tabHtFt.W = function(nline){
		return( 14*nline+5*(nline-1)+20 )
	}
	tabHtFt.A = function(nline){
		return( 21*nline+5*(nline-1)+20 )
	}

	cheer_msg1 = data.frame(x='웰체크는 언제나 응원합니다.\n함께 건강을 잘 관리해 보아요!')

	if( dim(page2.sub1$out_value1)[1]==1 && grepl('해당 없음',page2.sub1$out_value1[1,2]) ) {
		item5.nrows=NULL;item5.nrowsW=NULL
		out.Tab_item5.Af = tableGrob(cheer_msg1,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item5.Wf=out.Tab_nullitem.Wf
	} else {
		item5.nrows = str_count(page2.sub1$out_value1[,2],'\n')+1
#		item5.nrows = sqrt(2)^item5.nrows#ifelse(item5.nrows>=1,1+item5.nrows/2,1)
		out.Tab_item5.Af = tableGrob(page2.sub1$out_value1[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item5.nrows),'points'))
# temp_0308 
		for(m in 1:length(page2.sub1$out_value1[,2])) page2.sub1$out_value1[m,2] = paste(strwrap(gsub('\n',' ',page2.sub1$out_value1[m,2]),width=32),collapse='\n')
		item5.nrowsW = str_count(page2.sub1$out_value1[,2],'\n')+1
#		item5.nrowsW = sqrt(2)^item5.nrowsW#ifelse(item5.nrowsW>=1,1+item5.nrowsW/2,1)
		out.Tab_item5.Wf = tableGrob(page2.sub1$out_value1[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item5.nrowsW),'points'))
#		for(i in 1:length(item5.nrows)) out.Tab_item5.f = gtable_add_grob(out.Tab_item5.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item5.f = gtable_add_grob(out.Tab_item5.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item5.nrows)-1,b=length(item5.nrows),l=1,r=2)
		if(length(item5.nrows)==1) {
			out.Tab_item5.Af$grobs[which(out.Tab_item5.Af$layout$t==1 & out.Tab_item5.Af$layout$l==2 & out.Tab_item5.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item5.Wf$grobs[which(out.Tab_item5.Wf$layout$t==1 & out.Tab_item5.Wf$layout$l==2 & out.Tab_item5.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		} 
	}

	if( dim(page2.sub2$out_value1)[1]==1 && grepl('해당 없음',page2.sub2$out_value1[1,2]) ) {
		item6.nrows=NULL;item6.nrowsW=NULL
		out.Tab_item6.Af = tableGrob(cheer_msg1,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item6.Wf=out.Tab_nullitem.Wf
	} else {
		item6.nrows = str_count(page2.sub2$out_value1[,2],'\n')+1
#		item6.nrows = sqrt(2)^item6.nrows#ifelse(item6.nrows>=1,1+item6.nrows/2,1)
		out.Tab_item6.Af = tableGrob(page2.sub2$out_value1[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item6.nrows),'points'))
# temp_0308
		for(m in 1:length(page2.sub2$out_value1[,2])) page2.sub2$out_value1[m,2] = paste(strwrap(gsub('\n',' ',page2.sub2$out_value1[m,2]),width=32),collapse='\n')
		item6.nrowsW = str_count(page2.sub2$out_value1[,2],'\n')+1
#		item6.nrowsW = sqrt(2)^item6.nrowsW#ifelse(item6.nrowsW>=1,1+item6.nrowsW/2,1)
		out.Tab_item6.Wf = tableGrob(page2.sub2$out_value1[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item6.nrowsW),'points'))
#		for(i in 1:length(item6.nrows)) out.Tab_item6.f = gtable_add_grob(out.Tab_item6.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item6.f = gtable_add_grob(out.Tab_item6.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item6.nrows)-1,b=length(item6.nrows),l=1,r=2)
		if(length(item6.nrows)==1) {
			out.Tab_item6.Af$grobs[which(out.Tab_item6.Af$layout$t==1 & out.Tab_item6.Af$layout$l==2 & out.Tab_item6.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item6.Wf$grobs[which(out.Tab_item6.Wf$layout$t==1 & out.Tab_item6.Wf$layout$l==2 & out.Tab_item6.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}

	if( dim(page2.sub3$out_value1)[1]==1 && grepl('해당 없음',page2.sub3$out_value1[1,2]) ) {
		item7.nrows=NULL;item7.nrowsW=NULL
		out.Tab_item7.Af = tableGrob(cheer_msg1,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item7.Wf=out.Tab_nullitem.Wf
	} else {
		item7.nrows = str_count(page2.sub3$out_value1[,2],'\n')+1
#		item7.nrows = sqrt(2)^item7.nrows#ifelse(item7.nrows>=1,1+item7.nrows/2,1)
		out.Tab_item7.Af = tableGrob(page2.sub3$out_value1[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item7.nrows),'points'))
# temp_0308 
		for(m in 1:length(page2.sub3$out_value1[,2])) page2.sub3$out_value1[m,2] = paste(strwrap(gsub('\n',' ',page2.sub3$out_value1[m,2]),width=32),collapse='\n')
		item7.nrowsW = str_count(page2.sub3$out_value1[,2],'\n')+1
#		item7.nrowsW = sqrt(2)^item7.nrowsW#ifelse(item7.nrowsW>=1,1+item7.nrowsW/2,1)
		out.Tab_item7.Wf = tableGrob(page2.sub3$out_value1[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item7.nrowsW),'points'))
#		for(i in 1:length(item7.nrows)) out.Tab_item7.f = gtable_add_grob(out.Tab_item7.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item7.f = gtable_add_grob(out.Tab_item7.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item7.nrows)-1,b=length(item7.nrows),l=1,r=2)
		if(length(item7.nrows)==1) {
			out.Tab_item7.Af$grobs[which(out.Tab_item7.Af$layout$t==1 & out.Tab_item7.Af$layout$l==2 & out.Tab_item7.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item7.Wf$grobs[which(out.Tab_item7.Wf$layout$t==1 & out.Tab_item7.Wf$layout$l==2 & out.Tab_item7.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}

	cheer_msg2 = data.frame(x='앞으로도 웰체크와 함께\n식후 혈당을 잘 관리해 보아요!')

	if( dim(page2.sub1$out_value2)[1]==1 && grepl('해당 없음',page2.sub1$out_value2[1,2]) ) {
		item8.nrows=NULL;item8.nrowsW=NULL
		out.Tab_item8.Af = tableGrob(cheer_msg2,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item8.Wf=out.Tab_nullitem.Wf
	} else {
		item8.nrows = str_count(page2.sub1$out_value2[,2],'\n')+1
#		item8.nrows = sqrt(2)^item8.nrows#ifelse(item8.nrows>=1,1+item8.nrows/2,1)
		out.Tab_item8.Af = tableGrob(page2.sub1$out_value2[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item8.nrows),'points'))

		for(m in 1:length(page2.sub1$out_value2[,2])) page2.sub1$out_value2[m,2] = paste(strwrap(gsub('\n',' ',page2.sub1$out_value2[m,2]),width=32),collapse='\n')
		item8.nrowsW = str_count(page2.sub1$out_value2[,2],'\n')+1
#		item8.nrowsW = sqrt(2)^item8.nrowsW#ifelse(item8.nrowsW>=1,1+item8.nrowsW/2,1)
		out.Tab_item8.Wf = tableGrob(page2.sub1$out_value2[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item8.nrowsW),'points'))
#		for(i in 1:length(item8.nrows)) out.Tab_item8.f = gtable_add_grob(out.Tab_item8.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item8.f = gtable_add_grob(out.Tab_item8.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item8.nrows)-1,b=length(item8.nrows),l=1,r=2)
		if(length(item8.nrows)==1) { 
			out.Tab_item8.Af$grobs[which(out.Tab_item8.Af$layout$t==1 & out.Tab_item8.Af$layout$l==2 & out.Tab_item8.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item8.Wf$grobs[which(out.Tab_item8.Wf$layout$t==1 & out.Tab_item8.Wf$layout$l==2 & out.Tab_item8.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}

	if( dim(page2.sub2$out_value2)[1]==1 && grepl('해당 없음',page2.sub2$out_value2[1,2]) ) {
		item9.nrows=NULL;item9.nrowsW=NULL
		out.Tab_item9.Af = tableGrob(cheer_msg2,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item9.Wf=out.Tab_nullitem.Wf
	} else {
		item9.nrows = str_count(page2.sub2$out_value2[,2],'\n')+1
#		item9.nrows = sqrt(2)^item9.nrows#ifelse(item9.nrows>=1,1+item9.nrows/2,1)
		out.Tab_item9.Af = tableGrob(page2.sub2$out_value2[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item9.nrows),'points'))

		for(m in 1:length(page2.sub2$out_value2[,2])) page2.sub2$out_value2[m,2] = paste(strwrap(gsub('\n',' ',page2.sub2$out_value2[m,2]),width=32),collapse='\n')
		item9.nrowsW = str_count(page2.sub2$out_value2[,2],'\n')+1
#		item9.nrowsW = sqrt(2)^item9.nrowsW#ifelse(item9.nrowsW>=1,1+item9.nrowsW/2,1)
		out.Tab_item9.Wf = tableGrob(page2.sub2$out_value2[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item9.nrowsW),'points'))
#		for(i in 1:length(item9.nrows)) out.Tab_item9.f = gtable_add_grob(out.Tab_item9.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item9.f = gtable_add_grob(out.Tab_item9.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item9.nrows),b=length(item9.nrows),l=1,r=2)
		if(length(item9.nrows)==1) {
			out.Tab_item9.Af$grobs[which(out.Tab_item9.Af$layout$t==1 & out.Tab_item9.Af$layout$l==2 & out.Tab_item9.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item9.Wf$grobs[which(out.Tab_item9.Wf$layout$t==1 & out.Tab_item9.Wf$layout$l==2 & out.Tab_item9.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}

	if( dim(page2.sub3$out_value2)[1]==1 && grepl('해당 없음',page2.sub3$out_value2[1,2]) ) {
		item10.nrows=NULL;item10.nrowsW=NULL
		out.Tab_item10.Af = tableGrob(cheer_msg2,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item10.Wf=out.Tab_nullitem.Wf
	} else {
		item10.nrows = str_count(page2.sub3$out_value2[,2],'\n')+1
#		item10.nrows = sqrt(2)^item10.nrows#ifelse(item10.nrows>=1,1+item10.nrows/2,1)
		out.Tab_item10.Af = tableGrob(page2.sub3$out_value2[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item10.nrows),'points'))

		for(m in 1:length(page2.sub3$out_value2[,2])) page2.sub3$out_value2[m,2] = paste(strwrap(gsub('\n',' ',page2.sub3$out_value2[m,2]),width=32),collapse='\n')
		item10.nrowsW = str_count(page2.sub3$out_value2[,2],'\n')+1
#		item10.nrowsW = sqrt(2)^item10.nrowsW#ifelse(item10.nrowsW>=1,1+item10.nrowsW/2,1)
		out.Tab_item10.Wf = tableGrob(page2.sub3$out_value2[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item10.nrowsW),'points'))
#		for(i in 1:length(item10.nrows)) out.Tab_item10.f = gtable_add_grob(out.Tab_item10.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item10.f = gtable_add_grob(out.Tab_item10.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item10.nrows),b=length(item10.nrows),l=1,r=2)
		if(length(item10.nrows)==1) { 
			out.Tab_item10.Af$grobs[which(out.Tab_item10.Af$layout$t==1 & out.Tab_item10.Af$layout$l==2 & out.Tab_item10.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item10.Wf$grobs[which(out.Tab_item10.Wf$layout$t==1 & out.Tab_item10.Wf$layout$l==2 & out.Tab_item10.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}
	
	if( dim(page2.sub1$out_value3)[1]==1 && grepl('해당 없음',page2.sub1$out_value3[1,2]) ) {
		item11.nrows=NULL;item11.nrowsW=NULL
		out.Tab_item11.Af = tableGrob(cheer_msg2,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item11.Wf=out.Tab_nullitem.Wf
	} else {
		item11.nrows = str_count(page2.sub1$out_value3[,2],'\n')+1
#		item11.nrows = sqrt(2)^item11.nrows#ifelse(item11.nrows>=1,1+item11.nrows/2,1)
		out.Tab_item11.Af = tableGrob(page2.sub1$out_value3[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item11.nrows),'points'))

		for(m in 1:length(page2.sub1$out_value3[,2])) page2.sub1$out_value3[m,2] = paste(strwrap(gsub('\n',' ',page2.sub1$out_value3[m,2]),width=32),collapse='\n')
		item11.nrowsW = str_count(page2.sub1$out_value3[,2],'\n')+1
#		item11.nrowsW = sqrt(2)^item11.nrowsW#ifelse(item11.nrowsW>=1,1+item11.nrowsW/2,1)
		out.Tab_item11.Wf = tableGrob(page2.sub1$out_value3[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item11.nrowsW),'points'))
#		for(i in 1:length(item11.nrows)) out.Tab_item11.f = gtable_add_grob(out.Tab_item11.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item11.f = gtable_add_grob(out.Tab_item11.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item11.nrows),b=length(item11.nrows),l=1,r=2)
		if(length(item11.nrows)==1) {
			out.Tab_item11.Af$grobs[which(out.Tab_item11.Af$layout$t==1 & out.Tab_item11.Af$layout$l==2 & out.Tab_item11.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item11.Wf$grobs[which(out.Tab_item11.Wf$layout$t==1 & out.Tab_item11.Wf$layout$l==2 & out.Tab_item11.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}

	if( dim(page2.sub2$out_value3)[1]==1 && grepl('해당 없음',page2.sub2$out_value3[1,2]) ) {
		item12.nrows=NULL;item12.nrowsW=NULL
		out.Tab_item12.Af = tableGrob(cheer_msg2,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item12.Wf=out.Tab_nullitem.Wf
	} else {
		item12.nrows = str_count(page2.sub2$out_value3[,2],'\n')+1
#		item12.nrows = sqrt(2)^item12.nrows#ifelse(item12.nrows>=1,1+item12.nrows/2,1)
		out.Tab_item12.Af = tableGrob(page2.sub2$out_value3[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item12.nrows),'points'))

		for(m in 1:length(page2.sub2$out_value3[,2])) page2.sub2$out_value3[m,2] = paste(strwrap(gsub('\n',' ',page2.sub2$out_value3[m,2]),width=32),collapse='\n')
		item12.nrowsW = str_count(page2.sub2$out_value3[,2],'\n')+1
#		item12.nrowsW = sqrt(2)^item12.nrowsW#ifelse(item12.nrowsW>=1,1+item12.nrowsW/2,1)
		out.Tab_item12.Wf = tableGrob(page2.sub2$out_value3[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item12.nrowsW),'points'))
#		for(i in 1:length(item12.nrows)) out.Tab_item12.f = gtable_add_grob(out.Tab_item12.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item12.f = gtable_add_grob(out.Tab_item12.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item12.nrows),b=length(item12.nrows),l=1,r=2)
		if(length(item12.nrows)==1) {
			out.Tab_item12.Af$grobs[which(out.Tab_item12.Af$layout$t==1 & out.Tab_item12.Af$layout$l==2 & out.Tab_item12.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item12.Wf$grobs[which(out.Tab_item12.Wf$layout$t==1 & out.Tab_item12.Wf$layout$l==2 & out.Tab_item12.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}

	if( dim(page2.sub3$out_value3)[1]==1 && grepl('해당 없음',page2.sub3$out_value3[1,2]) ) {
		item13.nrows=NULL;item13.nrowsW=NULL
		out.Tab_item13.Af = tableGrob(cheer_msg2,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item13.Wf=out.Tab_nullitem.Wf
	} else {
		item13.nrows = str_count(page2.sub3$out_value3[,2],'\n')+1
#		item13.nrows = sqrt(2)^item13.nrows#ifelse(item13.nrows>=1,1+item13.nrows/2,1)	
		out.Tab_item13.Af = tableGrob(page2.sub3$out_value3[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item13.nrows),'points'))

		for(m in 1:length(page2.sub3$out_value3[,2])) page2.sub3$out_value3[m,2] = paste(strwrap(gsub('\n',' ',page2.sub3$out_value3[m,2]),width=32),collapse='\n')
		item13.nrowsW = str_count(page2.sub3$out_value3[,2],'\n')+1
#		item13.nrowsW = sqrt(2)^item13.nrowsW#ifelse(item13.nrowsW>=1,1+item13.nrowsW/2,1)	
		out.Tab_item13.Wf = tableGrob(page2.sub3$out_value3[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item13.nrowsW),'points'))
#		for(i in 1:length(item13.nrows)) out.Tab_item13.f = gtable_add_grob(out.Tab_item13.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item13.f = gtable_add_grob(out.Tab_item13.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item13.nrows),b=length(item13.nrows),l=1,r=2)
		if(length(item13.nrows)==1) {
			out.Tab_item13.Af$grobs[which(out.Tab_item13.Af$layout$t==1 & out.Tab_item13.Af$layout$l==2 & out.Tab_item13.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item13.Wf$grobs[which(out.Tab_item13.Wf$layout$t==1 & out.Tab_item13.Wf$layout$l==2 & out.Tab_item13.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}
	
	bads_detl_msg = data.frame(x='혈당 스파이크란\n혈당이 급격히 올라갔다 내려가는 증상을 말해요')

	if( dim(page2.sub1$out_value4)[1]==1 && grepl('해당 없음',page2.sub1$out_value4[1,2]) ) {
		item14.nrows=NULL;item14.nrowsW=NULL
		out.Tab_item14.Af = tableGrob(bads_detl_msg,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points')) 
		out.Tab_item14.Wf=out.Tab_nullitem.Wf
	} else {
		item14.nrows = str_count(page2.sub1$out_value4[,2],'\n')+1
#		item14.nrows = sqrt(2)^item13.nrowsW#ifelse(item13.nrowsW>=1,1+item13.nrowsW/2,1)
		out.Tab_item14.Af = tableGrob(page2.sub1$out_value4[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item14.nrows),'points'))

		for(m in 1:length(page2.sub1$out_value4[,2])) page2.sub1$out_value4[m,2] = paste(strwrap(gsub('\n',' ',page2.sub1$out_value4[m,2]),width=32),collapse='\n')
		item14.nrowsW = str_count(page2.sub1$out_value4[,2],'\n')+1
#		item14.nrowsW = sqrt(2)^item14.nrowsW#ifelse(item14.nrowsW>=1,1+item14.nrowsW/2,1)
		out.Tab_item14.Wf = tableGrob(page2.sub1$out_value4[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item14.nrowsW),'points'))
#		for(i in 1:length(item14.nrows)) out.Tab_item14.f = gtable_add_grob(out.Tab_item14.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item14.f = gtable_add_grob(out.Tab_item14.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item14.nrows),b=length(item14.nrows),l=1,r=2)
		if(length(item14.nrows)==1) {
			out.Tab_item14.Af$grobs[which(out.Tab_item14.Af$layout$t==1 & out.Tab_item14.Af$layout$l==2 & out.Tab_item14.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item14.Wf$grobs[which(out.Tab_item14.Wf$layout$t==1 & out.Tab_item14.Wf$layout$l==2 & out.Tab_item14.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}	

	if( dim(page2.sub2$out_value4)[1]==1 && grepl('해당 없음',page2.sub2$out_value4[1,2]) ) {
		item15.nrows=NULL;item15.nrowsW=NULL
		out.Tab_item15.Af = tableGrob(bads_detl_msg,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points'))
		out.Tab_item15.Wf=out.Tab_nullitem.Wf
	} else {
		item15.nrows = str_count(page2.sub2$out_value4[,2],'\n')+1
#		item15.nrows = sqrt(2)^item15.nrows#ifelse(item15.nrows>=1,1+item15.nrows/2,1)
		out.Tab_item15.Af = tableGrob(page2.sub2$out_value4[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item15.nrows),'points'))

		for(m in 1:length(page2.sub2$out_value4[,2])) page2.sub2$out_value4[m,2] = paste(strwrap(gsub('\n',' ',page2.sub2$out_value4[m,2]),width=32),collapse='\n')
		item15.nrowsW = str_count(page2.sub2$out_value4[,2],'\n')+1
#		item15.nrowsW = sqrt(2)^item15.nrowsW#ifelse(item15.nrowsW>=1,1+item15.nrowsW/2,1)
		out.Tab_item15.Wf = tableGrob(page2.sub2$out_value4[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item15.nrowsW),'points'))
#		for(i in 1:length(item15.nrows)) out.Tab_item15.f = gtable_add_grob(out.Tab_item15.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item15.f = gtable_add_grob(out.Tab_item15.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item15.nrows),b=length(item15.nrows),l=1,r=2)
		if(length(item15.nrows)==1) {
			out.Tab_item15.Af$grobs[which(out.Tab_item15.Af$layout$t==1 & out.Tab_item15.Af$layout$l==2 & out.Tab_item15.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item15.Wf$grobs[which(out.Tab_item15.Wf$layout$t==1 & out.Tab_item15.Wf$layout$l==2 & out.Tab_item15.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}
	
	if( dim(page2.sub3$out_value4)[1]==1 && grepl('해당 없음',page2.sub3$out_value4[1,2]) ) {
		item16.nrows=NULL;item16.nrowsW=NULL
		out.Tab_item16.Af = tableGrob(bads_detl_msg,theme=page2.out.Tab_1_sub_null.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(512),'points'),heights=unit(c(67),'points'))
		out.Tab_item16.Wf=out.Tab_nullitem.Wf
	} else {
		item16.nrows = str_count(page2.sub3$out_value4[,2],'\n')+1
#		item16.nrows = sqrt(2)^item16.nrows#ifelse(item16.nrows>=1,1+item16.nrows/2,1)
		out.Tab_item16.Af = tableGrob(page2.sub3$out_value4[,1:2],theme=page2.out.Tab_1_sub.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(171,341),'points'),heights=unit(tabHtFt.A(item16.nrows),'points'))

		for(m in 1:length(page2.sub3$out_value4[,2])) page2.sub3$out_value4[m,2] = paste(strwrap(gsub('\n',' ',page2.sub3$out_value4[m,2]),width=32),collapse='\n')
		item16.nrowsW = str_count(page2.sub3$out_value4[,2],'\n')+1
#		item16.nrowsW = sqrt(2)^item16.nrowsW#ifelse(item16.nrowsW>=1,1+item16.nrowsW/2,1)
		out.Tab_item16.Wf = tableGrob(page2.sub3$out_value4[,1:2],theme=page2.out.Tab_1_sub.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(93,187),'points'),heights=unit(tabHtFt.W(item16.nrowsW),'points'))
#		for(i in 1:length(item16.nrows)) out.Tab_item16.f = gtable_add_grob(out.Tab_item16.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(1,'npc'),x1=unit(1,'npc'),y1=unit(1,'npc')),t=i,b=i,l=1,r=2)
#		out.Tab_item16.f = gtable_add_grob(out.Tab_item16.f,grobs=segmentsGrob(x0=unit(0,'npc'),y0=unit(0,'npc'),x1=unit(1,'npc'),y1=unit(0,'npc')),t=length(item16.nrows),b=length(item16.nrows),l=1,r=2)
		if(length(item16.nrows)==1) {
			out.Tab_item16.Af$grobs[which(out.Tab_item16.Af$layout$t==1 & out.Tab_item16.Af$layout$l==2 & out.Tab_item16.Af$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
			out.Tab_item16.Wf$grobs[which(out.Tab_item16.Wf$layout$t==1 & out.Tab_item16.Wf$layout$l==2 & out.Tab_item16.Wf$layout$name=='core-bg')][[1]][['gp']][['fill']] = NA
		}
	}

	# legend # 
	# todo [] 이렇게하지말고 고정으로 하고싶은데.. temp_0306
	page2.Plot1_legend.tmp1 = ifelse(!is.null(page2.sub1$out.Plot1_legend),dim(page2.sub1$out.Plot1_legend)[1],0)
	page2.Plot1_legend.tmp2 = ifelse(!is.null(page2.sub2$out.Plot1_legend),dim(page2.sub2$out.Plot1_legend)[1],0)
	page2.Plot1_legend.tmp3 = ifelse(!is.null(page2.sub3$out.Plot1_legend),dim(page2.sub3$out.Plot1_legend)[1],0)
	page2.Plot1_legend.tmp = which.max(c(page2.Plot1_legend.tmp1,page2.Plot1_legend.tmp2,page2.Plot1_legend.tmp3))

	if(page2.Plot1_legend.tmp==1){
		page2.Plot1_legend = page2.sub1$out.Plot1_legend
	} else if(page2.Plot1_legend.tmp==2){
		page2.Plot1_legend = page2.sub2$out.Plot1_legend
	} else if(page2.Plot1_legend.tmp==3){
		page2.Plot1_legend = page2.sub3$out.Plot1_legend
	}

	if(is.null(page2.Plot1_legend)) {
#		page2.Plot1_legend = ggplot()+theme(panel.background=element_blank(),panel.border=element_blank())
		page2.Plot1_legend = ggplot()+theme(panel.background=element_rect(fill=NA,color=NA),panel.border=element_blank())
	}

	out.msgPlt_bad = ggplot() + annotation_custom(rasterGrob(bad_emoji)) + theme(panel.background=element_rect(fill=NA,color=NA))
	out.msgPlt_good = ggplot() + annotation_custom(rasterGrob(good_emoji)) + theme(panel.background=element_rect(fill=NA,color=NA))

	#### item5 - Dietary Plan (good, morning) - print out - ####################

	if( !is.null(item5.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanGood1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(576,tabHtFt.A(item5.nrows)))/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle1.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(c(25),'points')),
										page2.sub1$out.Plot1_forApp,
										page2.out.Tab_11_sub1.Af,                                    
										out.Tab_item5.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(NA,4,NA,NA),c(NA,NA,5,NA),c(6,6,6,6),c(7,7,7,7),c(8,8,8,8)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,55,4,25,25,285,50,sum(tabHtFt.A(item5.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanGood1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanGood1.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanGood1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=621/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle1.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
										out.msgPlt_bad,
										page2.out.Tab_11_sub1.Af,                                    
										out.Tab_item5.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(4,4,4,4),c(5,5,5,5),c(6,6,6,6)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,55,5,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanGood1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanGood1.png',sep='_'))		
	}


	#### item6 - Dietary Plan (good, noon) - print out - ####################

	if( !is.null(item6.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanGood2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(576,tabHtFt.A(item6.nrows)))/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle1.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(c(25),'points')),
										page2.sub2$out.Plot1_forApp,
										page2.out.Tab_11_sub2.Af,                                    
										out.Tab_item6.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(NA,4,NA,NA),c(NA,NA,5,NA),c(6,6,6,6),c(7,7,7,7),c(8,8,8,8)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,55,4,25,25,285,50,sum(tabHtFt.A(item6.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanGood2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanGood2.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanGood2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=621/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle1.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
										out.msgPlt_bad,
										page2.out.Tab_11_sub2.Af,                                    
										out.Tab_item6.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(4,4,4,4),c(5,5,5,5),c(6,6,6,6)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,55,5,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanGood2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanGood2.png',sep='_'))		
	}

	#### item7 - Dietary Plan (good, evening) - print out - ####################

	if( !is.null(item7.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanGood3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(576,tabHtFt.A(item7.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanGood3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle1.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(c(25),'points')),
										page2.sub3$out.Plot1_forApp,
										page2.out.Tab_11_sub3.Af,                                    
										out.Tab_item7.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(NA,4,NA,NA),c(NA,NA,5,NA),c(6,6,6,6),c(7,7,7,7),c(8,8,8,8)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,55,4,25,25,285,50,sum(tabHtFt.A(item7.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanGood3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanGood3.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanGood3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=621/0.75, units='px',dpi=96)

		LibreReport_DietPlanGood3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle1.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(55),'points')),
										out.msgPlt_bad,
										page2.out.Tab_11_sub3.Af,                                    
										out.Tab_item7.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(4,4,4,4),c(5,5,5,5),c(6,6,6,6)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,55,5,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanGood3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanGood3.png',sep='_'))		
	}

	## forWeb ==== ##
	if( !is.null(item5.nrowsW) | !is.null(item6.nrowsW) | !is.null(item7.nrowsW) ) {
		tmpL = max(length(item5.nrowsW),length(item6.nrowsW),length(item7.nrowsW))
		tmpH = matrix(NA,nrow=tmpL,ncol=3); 
		if(!is.null(item5.nrowsW)) tmpH[1:length(item5.nrowsW),1]=item5.nrowsW; 
		if(!is.null(item6.nrowsW)) tmpH[1:length(item6.nrowsW),2]=item6.nrowsW; 
		if(!is.null(item7.nrowsW)) tmpH[1:length(item7.nrowsW),3]=item7.nrowsW
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T))) #sum(13*item7.nrowsW)+17
		
		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanGood.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(474,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle1.W,theme=page2.Tabtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
										page2.Plot1_legend,#4
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#5
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#7
										page2.sub1$out.Plot1_forWeb,page2.sub2$out.Plot1_forWeb,page2.sub3$out.Plot1_forWeb, # 8,9,10
										page2.out.Tab_11_sub1.Wf,page2.out.Tab_11_sub2.Wf,page2.out.Tab_11_sub3.Wf, # 11,12,13                    
										gtable_combine(out.Tab_item5.Wf,out.Tab_spaceitem.Wf,out.Tab_item6.Wf,out.Tab_spaceitem.Wf,out.Tab_item7.Wf,along=1)), # 14,15,16
				nrow=9, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,NA,NA,NA),c(5,NA,6,NA,7),c(8,NA,9,NA,10),c(11,NA,12,NA,13),c(14,14,14,14,14)),
				widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,4,25,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanGood_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanGood.png',sep='_'))
	} else {
		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanGood.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=453/0.75, units='px',dpi=96)
		LibreReport_DietPlanGood_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle1,theme=page2.subtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle1.W,theme=page2.Tabtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#4
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#5
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										page2.sub1$out.Plot1_forWeb,page2.sub2$out.Plot1_forWeb,page2.sub3$out.Plot1_forWeb, # 7,8,9
										textGrob('분석기간 중 혈당을 안정화 시키는 식사 기록이 없습니다.',
											gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), # 10
				nrow=7, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,5,NA,6),c(7,NA,8,NA,9),c(10,10,10,10,10)),
				widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,6,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanGood_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanGood.png',sep='_'))	
	}


	#### item8 - Dietary Plan (warning, morning) - print out - ####################
	
	if( !is.null(item8.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanWarn1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(597,tabHtFt.A(item8.nrows)))/0.75, units='px',dpi=96)
		LibreReport_DietPlanWarn1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle2.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(76,'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub1$out.Plot2_forApp,
										page2.out.Tab_21_sub1.Af, # 
										out.Tab_item8.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(NA,4,NA,NA),c(NA,NA,5,NA),c(6,6,6,6),c(7,7,7,7),c(8,8,8,8)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,76,4,25,25,285,50,sum(tabHtFt.A(item8.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanWarn1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanWarn1.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanWarn1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=642/0.75, units='px',dpi=96)
		LibreReport_DietPlanWarn1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle2.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(76,'points')),
										out.msgPlt_good,
										page2.out.Tab_21_sub1.Af, # 
										out.Tab_item8.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(4,4,4,4),c(5,5,5,5),c(6,6,6,6)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,76,5,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanWarn1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanWarn1.png',sep='_'))		
	}

	#### item9 - Dietary Plan (warning, noon) - print out - ####################

	if( !is.null(item9.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanWarn2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(597,tabHtFt.A(item9.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanWarn2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle2.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(76,'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub2$out.Plot2_forApp,
										page2.out.Tab_21_sub2.Af, # 
										out.Tab_item9.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(NA,4,NA,NA),c(NA,NA,5,NA),c(6,6,6,6),c(7,7,7,7),c(8,8,8,8)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,76,4,25,25,285,50,sum(tabHtFt.A(item9.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanWarn2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanWarn2.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanWarn2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=642/0.75, units='px',dpi=96)

		LibreReport_DietPlanWarn2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle2.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(76,'points')),
										out.msgPlt_good,
										page2.out.Tab_21_sub2.Af, # 
										out.Tab_item9.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(4,4,4,4),c(5,5,5,5),c(6,6,6,6)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,76,5,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanWarn2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanWarn2.png',sep='_'))	
	}

	#### item10 - Dietary Plan (warning, evening) - print out - ####################

	if( !is.null(item10.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanWarn3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(597,tabHtFt.A(item10.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanWarn3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle2.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(76,'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub3$out.Plot2_forApp,
										page2.out.Tab_21_sub3.Af, # 
										out.Tab_item10.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(NA,4,NA,NA),c(NA,NA,5,NA),c(6,6,6,6),c(7,7,7,7),c(8,8,8,8)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,76,4,25,25,285,50,sum(tabHtFt.A(item10.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanWarn3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanWarn3.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanWarn3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=642/0.75, units='px',dpi=96)

		LibreReport_DietPlanWarn3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle2.A,theme=page2.Tabtitle1.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(76,'points')),
										out.msgPlt_good,
										page2.out.Tab_21_sub3.Af, # 
										out.Tab_item10.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),NA,c(4,4,4,4),c(5,5,5,5),c(6,6,6,6)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,76,5,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanWarn3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanWarn3.png',sep='_'))		
	}

	## forWeb ==== ##
	if( !is.null(item8.nrowsW) | !is.null(item9.nrowsW) | !is.null(item10.nrowsW) ) {
		tmpL = max(length(item8.nrowsW),length(item9.nrowsW),length(item10.nrowsW))
		tmpH = matrix(NA,nrow=tmpL,ncol=3);
		if(!is.null(item8.nrowsW)) tmpH[1:length(item8.nrowsW),1]=item8.nrowsW; 
		if(!is.null(item9.nrowsW)) tmpH[1:length(item9.nrowsW),2]=item9.nrowsW; 
		if(!is.null(item10.nrowsW)) tmpH[1:length(item10.nrowsW),3]=item10.nrowsW
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T))) #sum(13*item7.nrowsW)+17

		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanWarn.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(474,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanWarn_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle2.W,theme=page2.Tabtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
										page2.Plot1_legend,#4
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#5
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#7
										page2.sub1$out.Plot2_forWeb,page2.sub2$out.Plot2_forWeb,page2.sub3$out.Plot2_forWeb, # 8,9,10
										page2.out.Tab_21_sub1.Wf,page2.out.Tab_21_sub2.Wf,page2.out.Tab_21_sub3.Wf, # 11,12,13
										gtable_combine(out.Tab_item8.Wf,out.Tab_spaceitem.Wf,out.Tab_item9.Wf,out.Tab_spaceitem.Wf,out.Tab_item10.Wf,along=1)), # 14,15,16
			nrow=9, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,NA,NA,NA),c(5,NA,6,NA,7),c(8,NA,9,NA,10),c(11,NA,12,NA,13),c(14,14,14,14,14)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,4,25,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanWarn_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanWarn.png',sep='_'))
	} else {
		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanWarn.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=453/0.75, units='px',dpi=96)
		LibreReport_DietPlanWarn_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle2,theme=page2.subtitle2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle2.W,theme=page2.Tabtitle1.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(21.5),'points')),#3
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#4
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#5
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										page2.sub1$out.Plot2_forWeb,page2.sub2$out.Plot2_forWeb,page2.sub3$out.Plot2_forWeb, # 7,8,9
										textGrob('분석기간 중 혈당을 높인 식사기록이 없습니다.',
											gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), #10
			nrow=7, ncol=5, layout_matrix=rbind(c(1,1,1,1,1),c(2,2,2,2,2),c(3,3,3,3,3),NA,c(4,NA,5,NA,6),c(7,NA,8,NA,9),c(10,10,10,10,10)),
			widths=unit(c(280,18,280,18,280),'points'),heights=unit(c(57,28.5,21.5,6,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanWarn_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanWarn.png',sep='_'))		
	}

	#### item11 - Dietary Plan (bad1, morning) - print out - ####################

	if( !is.null(item11.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadH1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(627,tabHtFt.A(item11.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadH1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle3r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub1$out.Plot3_forApp,
										page2.out.Tab_31_sub1.Af, # 
										out.Tab_item11.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(NA,5,NA,NA),c(NA,NA,6,NA),c(7,7,7,7),c(8,8,8,8),c(9,9,9,9)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,25,25,285,50,sum(tabHtFt.A(item11.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadH1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadH1.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadH1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=672/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadH1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle3r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										out.msgPlt_good,
										page2.out.Tab_31_sub1.Af, # 
										out.Tab_item11.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,6,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadH1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadH1.png',sep='_'))
	}

	#### item12 - Dietary Plan (bad1, noon) - print out - ####################

	if( !is.null(item12.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadH2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(627,tabHtFt.A(item12.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadH2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle3r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub2$out.Plot3_forApp,
										page2.out.Tab_31_sub2.Af, # 
										out.Tab_item12.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(NA,5,NA,NA),c(NA,NA,6,NA),c(7,7,7,7),c(8,8,8,8),c(9,9,9,9)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,25,25,285,50,sum(tabHtFt.A(item12.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadH2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadH2.png',sep='_'))
	}  else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadH2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=672/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadH2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle3r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										out.msgPlt_good,
										page2.out.Tab_31_sub2.Af, # 
										out.Tab_item12.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,6,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadH2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadH2.png',sep='_'))
	}

	#### item13 - Dietary Plan (bad1, evening) - print out - ####################

	if( !is.null(item13.nrows) ) {		
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadH3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(627,tabHtFt.A(item13.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadH3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle3r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),									
										page2.Plot1_legend,
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub3$out.Plot3_forApp,
										page2.out.Tab_31_sub3.Af, #
										out.Tab_item13.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(NA,5,NA,NA),c(NA,NA,6,NA),c(7,7,7,7),c(8,8,8,8),c(9,9,9,9)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,25,25,285,50,sum(tabHtFt.A(item13.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadH3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadH3.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadH3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=672/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadH3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle3r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										out.msgPlt_good,							
										page2.out.Tab_31_sub3.Af, #
										out.Tab_item13.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,6,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadH3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadH3.png',sep='_'))
	}


	## forWeb ==== ##
	if( !is.null(item11.nrowsW) | !is.null(item12.nrowsW) | !is.null(item13.nrowsW) ) {
		tmpL = max(length(item11.nrowsW),length(item12.nrowsW),length(item13.nrowsW))
		tmpH = matrix(NA,nrow=tmpL,ncol=3);
		if(!is.null(item11.nrowsW)) tmpH[1:length(item11.nrowsW),1]=item11.nrowsW; 
		if(!is.null(item12.nrowsW)) tmpH[1:length(item12.nrowsW),2]=item12.nrowsW; 
		if(!is.null(item13.nrowsW)) tmpH[1:length(item13.nrowsW),3]=item13.nrowsW
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T))) #sum(13*item7.nrowsW)+17
		
		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanBadH.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(484,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadH_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
										tableGrob(page2.Tabtitle3r.W,theme=page2.Tabtitle3r.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
										page2.Plot1_legend,#5
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#7
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#8
										page2.sub1$out.Plot3_forWeb,page2.sub2$out.Plot3_forWeb,page2.sub3$out.Plot3_forWeb, # 9,10,11
										page2.out.Tab_31_sub1.Wf,page2.out.Tab_31_sub2.Wf,page2.out.Tab_31_sub3.Wf, # 12,13,14
										gtable_combine(out.Tab_item11.Wf,out.Tab_spaceitem.Wf,out.Tab_item12.Wf,out.Tab_spaceitem.Wf,out.Tab_item13.Wf,along=1)), # 15,16,17 
			nrow=9, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,NA,NA,NA),c(6,6,NA,7,NA,8),c(9,9,NA,10,NA,11),c(12,12,NA,13,NA,14),c(15,15,15,15,15,15)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,4,25,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadH_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanBadH.png',sep='_'))
	} else {
		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanBadH.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=462/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadH_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle3l,theme=page2.Tabtitle3l.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
										tableGrob(page2.Tabtitle3r.W,theme=page2.Tabtitle3r.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#5
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#7
										page2.sub1$out.Plot3_forWeb,page2.sub2$out.Plot3_forWeb,page2.sub3$out.Plot3_forWeb, # 8,9,10
										textGrob('분석기간 중 고혈당이 지속된 식사기록이 없습니다.',
											gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), # 11
			nrow=7, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,6,NA,7),c(8,8,NA,9,NA,10),c(11,11,11,11,11,11)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,5,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadH_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanBadH.png',sep='_'))
	}


	#### item14 - Dietary Plan (bad2, morning) - print out - ####################

	if( !is.null(item14.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadS1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(627,tabHtFt.A(item14.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadS1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle4r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),									
										page2.Plot1_legend,
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub1$out.Plot4_forApp,
										page2.out.Tab_32_sub1.Af, #
										out.Tab_item14.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(NA,5,NA,NA),c(NA,NA,6,NA),c(7,7,7,7),c(8,8,8,8),c(9,9,9,9)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,25,25,285,50,sum(tabHtFt.A(item14.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadS1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadS1.png',sep='_'))
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadS1.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=672/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadS1_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle4r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),	
										out.msgPlt_good,								
										page2.out.Tab_32_sub1.Af, #
										out.Tab_item14.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,6,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadS1_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadS1.png',sep='_'))
	}

	#### item15 - Dietary Plan (bad2, noon) - print out - ####################

	if( !is.null(item15.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadS2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(627,tabHtFt.A(item15.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadS2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle4r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										page2.Plot1_legend,
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub2$out.Plot4_forApp,
										page2.out.Tab_32_sub2.Af, #
										out.Tab_item15.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(NA,5,NA,NA),c(NA,NA,6,NA),c(7,7,7,7),c(8,8,8,8),c(9,9,9,9)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,25,25,285,50,sum(tabHtFt.A(item15.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadS2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadS2.png',sep='_')) 
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadS2.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=672/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadS2_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle4r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										out.msgPlt_good,
										page2.out.Tab_32_sub2.Af, #
										out.Tab_item15.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,6,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadS2_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadS2.png',sep='_')) 
	}

	#### item16 - Dietary Plan (bad2, evening) - print out - ####################

	if( !is.null(item16.nrows) ) {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadS3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=sum(c(627,tabHtFt.A(item16.nrows)))/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadS3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle4r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),									
										page2.Plot1_legend,
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forApp,cols=NULL,rows=NULL,widths=unit(c(60,140),'points'),heights=unit(25,'points')),
										page2.sub3$out.Plot4_forApp,
										page2.out.Tab_32_sub3.Af, # 
										out.Tab_item16.Af),
			nrow=9, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(NA,5,NA,NA),c(NA,NA,6,NA),c(7,7,7,7),c(8,8,8,8),c(9,9,9,9)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,5,25,25,285,50,sum(tabHtFt.A(item16.nrows))),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadS3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadS3.png',sep='_')) 
	} else {
		## forApp ==== ##
		CairoPNG(filename=paste(memberKey,createdtime,'App_DietPlanBadS3.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=720, height=672/0.75, units='px',dpi=96)

		LibreReport_DietPlanBadS3_forApp = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(c(50,32),'points')),
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forApp,cols=NULL,rows=NULL,widths=unit(512,'points'),heights=unit(50,'points')),
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forApp,cols=NULL,rows=NULL,widths=unit(156,'points'),heights=unit(105,'points')),
										tableGrob(page2.Tabtitle4r.A,theme=page2.Tabtitle3r.theme_forApp,cols=NULL,rows=NULL,widths=unit(356,'points'),heights=unit(105,'points')),
										out.msgPlt_good,
										page2.out.Tab_32_sub3.Af, # 
										out.Tab_item16.Af),
			nrow=7, ncol=4, layout_matrix=rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4),NA,c(5,5,5,5),c(6,6,6,6),c(7,7,7,7)),
			widths=unit(c(15,141,200,156),'points'),heights=unit(c(82,50,105,6,285,77,67),'points')),silent=T)
		dev.off()

		if( all(class(LibreReport_DietPlanBadS3_forApp)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'App_DietPlanBadS3.png',sep='_')) 
	}


	## forWeb ==== ##
	if( !is.null(item14.nrowsW) | !is.null(item15.nrowsW) | !is.null(item16.nrowsW) ) {
		tmpL = max(length(item14.nrowsW),length(item15.nrowsW),length(item16.nrowsW))
		tmpH = matrix(NA,nrow=tmpL,ncol=3); 
		if(!is.null(item14.nrowsW)) tmpH[1:length(item14.nrowsW),1]=item14.nrowsW; 
		if(!is.null(item15.nrowsW)) tmpH[1:length(item15.nrowsW),2]=item15.nrowsW; 
		if(!is.null(item16.nrowsW)) tmpH[1:length(item16.nrowsW),3]=item16.nrowsW
		tmpH = sum(tabHtFt.W(apply(tmpH,1,max,na.rm=T))) #sum(13*item7.nrowsW)+17
    
		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanBadS.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=sum(c(484,tmpH))/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadS_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
										tableGrob(page2.Tabtitle4r.W,theme=page2.Tabtitle3r.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
										page2.Plot1_legend,#5
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#7
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#8
										page2.sub1$out.Plot4_forWeb,page2.sub2$out.Plot4_forWeb,page2.sub3$out.Plot4_forWeb, # 9,10,11 
										page2.out.Tab_32_sub1.Wf,page2.out.Tab_32_sub2.Wf,page2.out.Tab_32_sub3.Wf, # 12,13,14
										gtable_combine(out.Tab_item14.Wf,out.Tab_spaceitem.Wf,out.Tab_item15.Wf,out.Tab_spaceitem.Wf,out.Tab_item16.Wf,along=1)), # 15,16,17
			nrow=9, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,NA,NA,NA),c(6,6,NA,7,NA,8),c(9,9,NA,10,NA,11),c(12,12,NA,13,NA,14),c(15,15,15,15,15,15)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,4,25,25,285,28,tmpH),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadS_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanBadS.png',sep='_'))
	} else {

		CairoPNG(filename=paste(memberKey,createdtime,'Web_DietPlanBadS.png',sep='_'),family='NotoSansCJKkrR', scale=1/0.32, bg='white', width=1168, height=462/0.75, units='px',dpi=96)
		LibreReport_DietPlanBadS_forWeb = try(grid.arrange(grobs=list(tableGrob(page2.title2,theme=page2.title2.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(c(28.5,28.5),'points')),#1
										tableGrob(page2.subtitle3,theme=page2.subtitle3.theme_forWeb,cols=NULL,rows=NULL,widths=unit(876,'points'),heights=unit(28.5,'points')),#2
										tableGrob(page2.Tabtitle4l,theme=page2.Tabtitle3l.theme_forWeb,cols=NULL,rows=NULL,widths=unit(84,'points'),heights=unit(c(31.5),'points')),#3
										tableGrob(page2.Tabtitle4r.W,theme=page2.Tabtitle3r.theme_forWeb,cols=NULL,rows=NULL,widths=unit(792,'points'),heights=unit(31.5,'points')),#4
										tableGrob(page2.timelogo1,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#5
										tableGrob(page2.timelogo2,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#6
										tableGrob(page2.timelogo3,theme=page2.timelogo.theme_forWeb,cols=NULL,rows=NULL,widths=unit(c(84,196),'points'),heights=unit(c(25),'points')),#7
										page2.sub1$out.Plot4_forWeb,page2.sub2$out.Plot4_forWeb,page2.sub3$out.Plot4_forWeb, # 8,9,10
										textGrob('분석기간 중 혈당 스파이크를 일으킨 식사기록이 없습니다.',
											gp=gpar(fontsize=12,fontface='plain',col='#333333',fontfamily='NotoSansCJKkrB'),hjust=0.5,vjust=0.5)), #11
			nrow=7, ncol=6, layout_matrix=rbind(c(1,1,1,1,1,1),c(2,2,2,2,2,2),c(3,4,4,4,4,4),NA,c(5,5,NA,6,NA,7),c(8,8,NA,9,NA,10),c(11,11,11,11,11,11)),
			widths=unit(c(84,196,18,280,18,280),'points'),heights=unit(c(57,28.5,31.5,5,25,285,30),'points')),silent=T)
		dev.off()
		
		if( all(class(LibreReport_DietPlanBadS_forWeb)!='try-error') ) outFileNames = c(outFileNames,paste(memberKey,createdtime,'Web_DietPlanBadS.png',sep='_'))
	}

	
#	print(paste('-------- running time: ',round(difftime(Sys.time(),strt.time,units='sec')),' sec --------',sep=''))
	out.Result.tmp = list(errCode=errCode, outFileNames_Web=outFileNames[grep('Web_',outFileNames)], outFileNames_App=outFileNames[grep('App_',outFileNames)] )
	out.Result = toJSON(out.Result.tmp)
	return( result = out.Result )

}
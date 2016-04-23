;;;管线探测CAD-数据库系统2014-07-08
;;;

;;;全局变量 gl_[大写名称]
(setq gl_DEBUG T) ;调试标志
(setq gl_INFO_LINE_PATH	 "H:\\INFO_LINE\\" ;程序路径
      gl_BLOCKLIB_PATH	 (strcat gl_INFO_LINE_PATH "BlockLib\\") ;符号图库路径
      gl_PPROJECT_PATH	 (strcat gl_INFO_LINE_PATH "project\\") ;项目路径，存放数据库和图形、项目配置文件

      gl_TableColorList	 nil ;主类型，颜色
      gl_SymbolBlockList nil ;管线特征点和附属物图标
      gl_UserSymbolList	 nil ;用户自定义图标,兼容V0.0.1,不再使用
	  gl_FeaturesList nil	;所有类型的特征点表((J (Sym1 起讫点) (..)....) (W () ()..)....)
      gl_SubList	 nil ; 子类型-主类型列表((subname mainname 中文名)....)

      gl_AllDBPointList	 nil ;所有数据库中的点名，检查重名
	  gl_MapNameList	nil	 ;图中所有管线点的名称，保证名称唯一性；
	  gl_ExpNameList nil	;所有物探点号
      gl_PointFieldList	 nil ;点字段名称列表((Map_No.图号)...)
	  gl_LineFieldList	 nil ;管线段字段名称列表

      gl_Material_List	 (list "砼" "UPVC" "PVC" "PE" "铜" "钢" "光纤" "铸铁" "铜/光" "铝" "镀锌" "砖" "塑料" "玻璃钢" "陶瓷")
	  gl_JG_Material (list "无" "砼" "铸铁" "钢" "塑料" )
	  gl_JG_Shape (list "无" "圆" "方")
	  ;;0-直埋\n1-矩形管沟\n2-圆形管沟\n3-拱形管沟\n4-人防\n5-管块\n6-套管\n7-小通道(d<1m)\n8-地上(架空)\n9-井内
	  gl_BuryWay (list "0-直埋" "1-矩形管沟" "2-圆形管沟" "3-拱形管沟" "4-人防" "5-管块" "6-套管" "7-小通道(d<1m)" "8-地上(架空)" "9-井内")
	  gl_Line_Style (list "0-非空管" "1-空管" "2-井内连线" "3-垂直管线段" "4-架空(过河)" "5-非开挖(顶管)" "6-井内管道")
	  gl_JS_Type (list "无" "100-圆柱状" "101-圆柱(偏心)" "102-圆柱(上窄下宽)" "103-圆柱+方型井室" "104-圆柱+方形井室" "105-长方体" "200-雨篦(立面)+井" "201-雨篦(旁边)+井" "202-雨篦(平面并排)")
	  gl_Well_Memo	(list "无" "废弃" "独立井" "淤塞")
      gl_ErrorList	 nil ;记录错误 (("Handle" "ErrorInfo")....)
	  gl_ErrorInfoString "" ;现在在对话框底部的错误信息
	  gl_EditInfo	""		;编辑时显示提示信息
      gl_Time		 0 ;记录运行时间
	  gl_DataBaseTime nil	;记录数据库保存时间
) ;_ End_setq
(setq gl_MIN 0.0001) ;零值
(setq gl_AppName "INFO_LINE") ;xrecord 的字典,对象lata-put key
(setq gl_Version "5.0.0")

;;图中所有块的名称列表,用于导入数据,加快导入速度
(setq gl_BlockNameList nil)

(regapp gl_AppName)
;;图形比例尺(1:1000输入1；1:500输入2；1:2000输入0.5
(if (Xrecord-Get gl_AppName "SCALE")
		(setq gl_MAP_SCALE (atof (car (Xrecord-Get gl_AppName "SCALE"))))
    (progn
		(Xrecord-Rebuild gl_AppName "SCALE" "2")
		(setq gl_MAP_SCALE 2.0)
    ) ;_ end_progn
) ;_ end_if

(LineInfo_GetSupportPath)

;;;----------------------读写参数配置文件---------------------------------------------

 ;*********************************************************************************************
 ;函数定义:ReadColorConfig()
 ;功能：读取参数配置文件中的主类型，颜色，中文名称、管道顶底深度
 ;参数：filepath
 ;返回：点表  eg:((J,5,给水,0,0,255))
 ;创建时间：2014/07/09   12:16
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun ReadColorConfig (path / b colorindex file g hname msm pos r str tablecolorlist typename)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ End_if
    (setq TableColorList nil)
    (if	(setq file (open path "r"))
		(progn
			(while (setq str (read-line file))
				(if (= str "[TableColorConfig]")
					(while (/= "[/TableColorConfig]" (setq str (read-line file)))
						(if (/= "//" (substr str 1 2));;注释行
							(setq pos	     (vl-string-search "," str)
								  typename	     (substr str 1 pos)

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  colorindex     (atoi (substr str 1 pos))

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  Hname	     (substr str 1 pos)

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  R		     (atoi (substr str 1 pos))

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  G		     (atoi (substr str 1 pos))

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  B		     (atoi (substr str 1 pos))

								  msm	     (atoi (substr str (+ 2 pos))) ;埋深码 1 管顶深， 3 管底深（排水，污水，雨水）
								  TableColorList (cons
										 (list typename colorindex Hname (list R G B) msm)
										 TableColorList
										 ) ;_ End_append
							) ;setq
						)
					) ;while
				) ;if
			) ;while
			(close file)
		) ;progn
	;;else
	(prompt (strcat "\nError！打开文件失败：" path))
    ) ;if
    (reverse TableColorList)
) ;_ End_defun

;*********************************************************************************************
;函数定义:ReadTypedFeatureList()
;功能：读取参数配置文件中的特征点FeatureName内容
;参数：
;返回：((J (Sym1 起讫点) (...)) (W (Sym1 起讫点))...) 类别 符号名 中文名
;创建时间：2014/12/16   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun ReadTypedFeatureList( path / alllist bend blockname cname featurelist file  maintype 
							newlist pos str outlist)
	(if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq FeatureList nil
		AllList nil
		newlist nil
		bEnd nil)
    (if	(setq file (open path "r"))
		(progn
			(while (and (not bEnd)(setq str (read-line file)))
				(if (= str "[FeatureName]")
					(progn
						(while (/= "[/FeatureName]" (setq str (read-line file)))
							(if (/= "//" (substr str 1 2));;注释行
								(progn
									(setq pos	 (vl-string-search "," str)
										  maintype (substr str 1 pos) ;first letter is the class name;

										  str	 (substr str (+ 2 pos))
										  pos	 (vl-string-search "," str)
										  blockname	 (substr str 1 pos)
										  blockname	 (strcat blockname ".dwg")
										  cname	 (substr str (+ 2 pos))
									) ;_ end_setq
									(if (setq FeatureList (assoc maintype AllList))
										(setq newlist (cons (list blockname cname) (cdr FeatureList))
											AllList (subst (cons maintype newlist) FeatureList AllList)
										)
										;;else
										(setq AllList (cons (cons maintype (list (list blockname cname))) AllList ))
									)
								)
							)
						) ;while
						(setq bEnd T)
					)
				) ;if
			) ;while
			(close file)
		) ;progn
		;;else
		(prompt (strcat "\nError！打开文件失败：" path))
    ) ;if
    ;;反转顺序
	(setq outlist nil)
	(foreach e AllList
		(setq FeatureList (cdr e)
			FeatureList (reverse FeatureList)
			outlist (cons (cons (car e) FeatureList) outlist)
		)
	)
	outlist
)
 ;*********************************************************************************************
 ;函数定义:ReadSymbolConfig()
 ;功能：读取参数配置文件中点的图块配置
 ;参数：filepath，默认系统目录
 ;返回：点表  eg:(("J21" "sym1.dwg"  "阀门井")...)
 ;创建时间：2014/07/09   21:10
 ;修改时间：2014/07/11   21:00 修改了config.ini，[BlockName] 类别+S_code识别符号块
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun ReadSymbolConfig	(path / blocklist blockname file cname pos str symbolname)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq BlockList nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (setq str (read-line file))
		(if (= str "[BlockName]")
		    (while (/= "[/BlockName]" (setq str (read-line file)))
				(if (/= "//" (substr str 1 2));;注释行
					(setq pos	 (vl-string-search "," str)
						  symbolname (substr str 1 pos) ;first letter is the class name;

						  str	 (substr str (+ 2 pos))
						  pos	 (vl-string-search "," str)
						  blockname	 (substr str 1 pos)
						  blockname	 (strcat blockname ".dwg")
						  cname	 (substr str (+ 2 pos))

						  BlockList	 (cons (list symbolname blockname cname) BlockList)
					) ;_ end_setq
				)
		    ) ;while
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError！打开文件失败：" path))
    ) ;if
    (reverse BlockList)
) ;_ end_defun
;;;读取子类型-主类型列表((subname mainname 中文名)....)
(defun ReadSubsymbolConfig (path / blocklist cname file mainname pos str subname)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq BlockList nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (setq str (read-line file))
		(if (= str "[Sub_Type]")
		    (while (/= "[/Sub_Type]" (setq str (read-line file)))
				(if (/= "//" (substr str 1 2));;注释行
					(setq
						pos	      (vl-string-search "," str)
						subname   (substr str 1 pos)

						str	      (substr str (+ 2 pos))
						pos	      (vl-string-search "," str)
						mainname  (substr str 1 pos)
						cname     (substr str (+ 2 pos))

						BlockList (cons (list subname mainname cname) BlockList)
					) ;_ end_setq
				)
		    ) ;while
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError！打开文件失败：" path))
    ) ;if
    (reverse BlockList)
) ;_ end_defun
;;根据类型“J”得到subsidlist
(defun getsubsidlist-fromtype  (typestr / e symstr subsidlist typename) ;"J"
	(setq subsidlist (list "无")
		typename (substr typestr 1 1) )
	(if (= nil gl_SymbolBlockList)
		(setq gl_SymbolBlockList (ReadSymbolConfig nil))
		)
	(foreach e  gl_SymbolBlockList
		(if (/= typestr nil) ;根据点类型，设置subsid内容
			(progn
				(setq symstr  (substr (car e) 1 1)
					  ) ;第一个字符为类别
				(if (= typename symstr)
					(setq subsidlist (append subsidlist (list (nth 2 e))))
					)
				) ;progn
			) ;if
		) ;foreach
	subsidlist
) ;defun

;;根据类型“J”得到subsidlist
(defun getfeaturelist-fromtype  (typestr / e FeatureList outlist) ;"J"
	(if (= nil gl_FeaturesList)
		(setq gl_FeaturesList (ReadTypedFeatureList nil))
		)
	(setq outlist nil)
	(setq FeatureList (cdr (assoc typestr gl_FeaturesList))
		FeatureList (reverse FeatureList))
	(foreach e FeatureList
		(setq outlist (cons (cadr e) outlist))
	)
	outlist
) ;defun

;;;根据字符串获取主类型
(defun GetTypeFromPointName (strName / typename)
    (if	(null gl_SubList)
	(setq gl_SubList (ReadSubsymbolConfig nil))
    ) ;if
    ;;使用子类型判断类型
    (setq strName  (vl-string-trim "0123456789-_+." strName) ;去掉数字序号
	  typename (cadr (assoc strName gl_SubList))
    ) ;_ end_setq
    ;;使用第一个字符判断类型
    (if	(= nil typename)
	(setq typename (cadr (assoc (substr strName 1 1) gl_SubList)))
    ) ;_ end_if
	
	 ;;使用第三个说明判断类型
    (if	(= nil typename)
	(setq typename (cadr (assoc2  strName  gl_SubList 2)))
    ) ;_ end_if
	
    (if	(= nil typename)
	(setq typename "B") ;默认不明
    ) ;_ end_if
    typename
) ;defun

;;从特征或者附属物 获取符号的文件名
;;sstr-附属物或特征点名称,maintype-主类型名
(defun GetSymbolFileName (featurestr subsidstr maintype / bsubsid bfeature e e1 i scode outstr 
							len FeatureList)
    (if	(= nil gl_SymbolBlockList)
		(setq gl_SymbolBlockList (ReadSymbolConfig nil))
    ) ;_ end_if
	(if	(= nil gl_FeaturesList)
		(setq gl_FeaturesList (ReadTypedFeatureList nil))
    ) ;_ end_if
	(setq FeatureList (cdr (assoc maintype gl_FeaturesList)))

    (setq bfeature   nil
	  bsubsid    nil
	  outstr "Sym1.dwg"
	  i	     0
	  len	     (length gl_SymbolBlockList)
    ) ;_ end_setq
    (repeat len
		(setq e	 (nth i gl_SymbolBlockList)
			  e1 (strcase (car e))
		) ;_ end_setq
		(if (= maintype (substr e1 1 1))
			(if	(= subsidstr (caddr e))
				(setq outstr (cadr e)
					  bsubsid T
				) ;_ end_setq
			) ;_ end_if
		)
		(setq i (1+ i))
    ) ;_ end_repeat
	(if (not bsubsid)
		(foreach e FeatureList
			(if (= featurestr (cadr e))
				(setq outstr (car e)
				) ;_ end_setq
			) ;if
		)
	)
    outstr
) ;_ end_defun

;;版本更新，3.3->3.4
;;把所有块和线段的ldata组成一个gl_App属性
(defun OneAllLdata( / AllPointSet AllLineSet i ent count)
	(setq AllPointSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
		AllLineSet (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName))))
		i 0
		count 0
		)
	(repeat (sslength AllLineSet)
		(setq ent (ssname AllLineSet i ))
		(if (setq listdata (vlax-ldata-list ent))
			(progn
				(foreach e listdata
					(vlax-ldata-delete ent (car e))
				)
				(setq listdata (subst (cons "Version" gl_Version) (assoc gl_AppName listdata) listdata))
				(vlax-ldata-put ent gl_AppName listdata)
			)
		)
		(setq i (1+ i))
	)	
	(setq count i 
		i 0)
	(repeat (sslength AllPointSet)
		(setq ent (ssname AllPointSet i ))
		(if (setq listdata (vlax-ldata-list ent))
			(progn
				(foreach e listdata
					(vlax-ldata-delete ent (car e))
				)
				(setq listdata (subst (cons "Version" gl_Version) (assoc gl_AppName listdata) listdata))
				(vlax-ldata-put ent gl_AppName listdata)
			)
		)
		(setq i (1+ i))
	)
	(setq count (+ count i))
)

;;*********************************************************************************************
 ;函数定义:UpdateVerion()
 ;功能：更新旧版本的图到新版本,由于gl_AppName 的修改;
 ;参数：
 ;返回：
 ;创建时间：2014/12/01   15:40
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun C:UpdateVersion  (/ allset ename i j ldata oldappname typename mapname maintype ldata1 
							ldata2 depth mdate)
    (print "正在更新图形版本...")
    (setq AllSet     (ssget "X")
	  oldAppName "INFO_LINE0.0.1"
	  i	     0
	  j	     0
    ) ;_ end_setq



    (repeat (sslength AllSet)
	(setq ename    (ssname AllSet i)
	      typename (cdr (assoc 0 (entget ename)))
	      ldata1   (vlax-ldata-get ename oldAppName)
	      ldata2   (vlax-ldata-get ename gl_AppName)
	      ldata    nil
	      version  (ldata-get ename "Version")
	) ;_ end_setq 
	(if (or ldata1 ldata2)
		    ;;设置AppID 用于ssget
		    (setxdata ename gl_AppName (list (cons 1000 gl_Version)))
		) ;_ end_if
	(if (/= gl_Version Version)
	    (progn
		
			;;0.01版
			(if (listp ldata1)
				(if	(> (length ldata1) 0)
					(setq ldata ldata1)
				) ;_ end_if
			) ;_ end_if
			;;1.0-3.2
			(if (and (listp ldata2) (/= version nil))
				(if	(> (length ldata2) 0)
					(setq ldata ldata2)
				) ;_ end_if
				;;else new version
			) ;_ end_if
			;;修改Mdata为Sur_Date
			(if (= version "3.4.0")
				(progn 
					(setq ldata nil)
					(ldata-put ename "Version" gl_Version)
					(setq tname (cdr (assoc 0 (entget ename))))
					(cond
						((= tname "INSERT") 
							(progn
								(ldata-delete ename "Sur_Date")
								(ldata-delete ename "Mdate")
							))
						((= tname "LINE") 
							(progn
								(ldata-put ename "Sur_Date" "")
								(if (setq mdate (ldata-get ename "Mdate"))
									(progn
										(ldata-put ename "Sur_Date" mdate)
										(ldata-put ename "Mdate" "")
									)
								)
							))	
					)
					(ldata-delete ename gl_AppName)
					
					(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
	      		(if (= version "4.0.0")
			  (setq ldata nil)
			  )
			(if (= ldata2 "3.3.0")
				(progn 
					(setq ldata nil)
					(if (setq listdata (vlax-ldata-list ename))
						(progn
						(foreach e listdata
							(vlax-ldata-delete ename (car e))
						) ;_ end_foreach
						(setq listdata (subst (cons "Version" gl_Version)
									  (assoc gl_AppName listdata)
									  listdata
								   ) ;_ end_subst
						) ;_ end_setq
						(vlax-ldata-put ename gl_AppName listdata)
						) ;_ end_progn
					) ;_ end_if
					(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
			(if (= ldata2 "3.0.0")
				;;3.2.0增加了这两个字段
				(progn
				(if (= "LINE" typename)
					(vlax-ldata-put ename "Road_Name" "")
				) ;_ end_if
				(if (= "INSERT" typename)
					(vlax-ldata-put ename "Point_Size" "")
				) ;_ end_if
				(vlax-ldata-put ename gl_AppName "3.3.0")

				(setq ldata nil)
				(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
			(if ldata
				;;0.0.1版 2.0版
				(progn
				(vlax-ldata-delete ename oldAppName)
				(vlax-ldata-put ename gl_AppName ldata)
				(setq maintype (substr (car ldata) 1 1))
				;;
				(if (= "LINE" typename)
					(progn
					;;通用属性
					(InitLineAttribs ename)
					(if (setq data (nth 1 ldata))
						(vlax-ldata-put ename "Start_Point" data) ;起点编号
					) ;_ end_if
					(if (setq data (nth 2 ldata))
						(vlax-ldata-put ename "End_Point" data) ;终点编号
					) ;_ end_if
					(if (setq data (nth 5 ldata))
						(vlax-ldata-put ename "Material" data) ;材质
					) ;_ end_if

					(vlax-ldata-put ename "Main_Type" maintype) ;主类型,13种,单个字符
					;;(vlax-ldata-put ename "Sub_Type" "")		;子类型,1-3个字符
					(if (setq data (nth 20 ldata))
						(vlax-ldata-put ename "Use_Status" data) ;0-废弃;1-使用;2-施工中
					) ;_ end_if

					;;属性
					(if (setq data (nth 7 ldata))
						(vlax-ldata-put ename "D_S" data) ;管径或尺寸DN100圆管,或者100*100方沟 mm
					) ;_ end_if
					;;适用于:非电信电力管道
					(if (setq data (nth 15 ldata))
						(progn (vlax-ldata-put ename "Cab_Count" data) ;管线数:电力或电信								
						   (vlax-ldata-put ename "Hole_Count" data) ;孔数:电力或电信
						) ;_ end_progn
					) ;_ end_if
					(if (setq data (nth 18 ldata))
						(vlax-ldata-put ename "Hole_Used" data) ;使用孔数:电力或电信
					) ;_ end_if
					(if (setq data (nth 17 ldata))
						(vlax-ldata-put ename "Pressure" data) ;压力:
					) ;_ end_if
					(if (setq data (nth 16 ldata))
						(vlax-ldata-put ename "Voltage" data) ;电压;kV V
					) ;_ end_if
					(if (setq data (nth 25 ldata))
						(vlax-ldata-put ename "Flowdirect" data) ;流向:使用与排水和给水,或许石油燃气热水
					) ;_ end_if
					;;流向 0 start-end ,1 end-start-end

					;;标记
					(vlax-ldata-put ename "Text_Pos" (list 0 0 0 0)) ;点号的标记位置,4个参数(x y z angle)
					(vlax-ldata-put ename "M_Text" "") ;标记文字内容;

					;;project info
					(if (setq data (nth 24 ldata))
						(vlax-ldata-put ename "Sur_Date" data) ;调查时间
					) ;_ end_if
					(if (setq data (nth 22 ldata))
						(vlax-ldata-put ename "SProject" data) ;子项目名称
					) ;_ end_if
					(if (setq data (nth 23 ldata))
						(vlax-ldata-put ename "Project" data) ;大项目名称
					) ;_ end_if
					(if (setq data (nth 19 ldata))
						(vlax-ldata-put ename "Location" data) ;调查位置
					) ;_ end_if
					(if (setq data (nth 21 ldata))
						(vlax-ldata-put ename "Unit" data) ;调查单位
					) ;_ end_if
					) ;_ end_progn
				) ;_ end_if

				(if (= "INSERT" typename)
					(progn
					(setq depth (vlax-ldata-get ename "DEPTH"))
					(InitPointAttribs ename)
					(setq mapname (GetLPointMapNo ename))
					(vlax-ldata-put ename "Map_No" (car mapname)) ;物探点号
					(vlax-ldata-put ename "Exp_No" (car mapname)) ;物探点号
					;;Depth

					(if depth
						(if	(listp depth)
						(vlax-ldata-put ename "Depth" (car depth))
						(vlax-ldata-put ename "Depth" depth)
						) ;_ end_if
					) ;深度

					(if (setq data (nth 7 ldata))
						(vlax-ldata-put ename "Feature" data) ;特征点
					) ;_ end_if
					(if (setq data (nth 8 ldata))
						(vlax-ldata-put ename "Subsid" data) ;附属物
					) ;_ end_if

					(vlax-ldata-put ename "Main_Type" maintype) ;主类型,13种,单个字符
					;;(vlax-ldata-put ename "Sub_Type" "")		;子类型,1-3个字符
					(vlax-ldata-put ename "Use_Status" 1) ;0-废弃;1-使用;2-施工中

					(vlax-ldata-put ename "Text_Pos" (list 0 0 0 0)) ;点号的标记位置,4个参数(x y z angle)
					(vlax-ldata-put ename "Mark_Angle" 0) ;图块的标准角度

					;;project info
					(if (setq data (nth 11 ldata))
						(vlax-ldata-put ename "Sur_Date" data) ;调查时间
					) ;_ end_if
					(if (setq data (nth 12 ldata))
						(vlax-ldata-put ename "SProject" data) ;子项目名称
					) ;_ end_if
					(if (setq data (nth 13 ldata))
						(vlax-ldata-put ename "Project" data) ;大项目名称
					) ;_ end_if
					(vlax-ldata-put ename "Location" "") ;调查位置
					(if (setq data (nth 9 ldata))
						(vlax-ldata-put ename "Unit" data) ;调查单位
					) ;_ end_if
					) ;_ end_progn
				) ;_ end_if
				(vlax-ldata-put ename gl_AppName "3.3.0")
				(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
	    ) ;_ end_progn
	)    
	    (setq i (1+ i))
	) ;_ end_repeat
	;;2015-3-9更新：所有ldata放在一个表中
	;;以上步骤更新到3.3.0,下面更新到3.4
	;;(setq ldata2 (vlax-ldata-get gl_AppName))
;;;	(if (= ldata2 "3.3.0")
;;;		(setq j (OneAllLdata))
;;;	)

	(prompt	(strcat	"\n修改了"
			(rtos j 2 0)
			"个对象的兼容性.\n当前版本为:"
			gl_AppName
			"-"
			gl_Version
			"."
		) ;_ end_strcat
	) ;_ end_prompt
    (princ)
) ;_ end_defun



 ;*********************************************************************************************
 ;函数定义:ReadProjectInfo (path)
 ;功能：读取项目信息
 ;参数：配置文件路径
 ;返回：(("Project"."项目名称") ()...)
 ;创建时间：2014/12/19   13:40
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun ReadProjectInfo (path / file info pos projectinfolist str typename)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ End_if
    (setq ProjectInfoList nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (setq str (read-line file))
		(if (= str "[ProjectInfo]")
		    (while (/= "[/ProjectInfo]" (setq str (read-line file)))
				(if (/= "//" (substr str 1 2));;注释行
					(setq pos	      (vl-string-search "," str)
						  typename	      (substr str 1 pos)
						  info	      (substr str (+ 2 pos))
						  ProjectInfoList (cons
								  (cons typename info)
								  ProjectInfoList
								  ) ;_ End_append
					) ;setq
				)
		    ) ;while
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError！打开文件失败：" path))
    ) ;if

    ProjectInfoList
) ;_ end_defun
;;把项目信息写入配置文件
;;参数:项目信息点对表
(defun WriteProjectInfo	(projectinfolist path / tmpfile tmpfilestr e file newdata olddata oldinfolist str strname)
    (if	(= nil path)
	(setq path	 (strcat gl_BLOCKLIB_PATH "config.ini")
	      tmpfilestr (strcat gl_BLOCKLIB_PATH "lineinfotmp.dat")
	) ;_ end_setq
    ) ;_ End_if
    (setq oldInfoList (ReadProjectInfo path))
    (vl-file-delete tmpfilestr)

    (if	(and (/= nil projectinfolist) (setq file (open path "r")) (setq tmpfile (open tmpfilestr "w")))
	(progn
	    (while (setq str (read-line file))
		(write-line str tmpfile)
		(if (= str "[ProjectInfo]")
		    (progn
			(foreach e oldInfoList
			    (setq strname (car e)
				  olddata (cdr e)
				  newdata (cdr (assoc strname projectinfolist))
			    ) ;_ end_setq
			    (if	(= olddata newdata)
				(write-line (strcat strname "," olddata) tmpfile)
				(write-line (strcat strname "," newdata) tmpfile)
			    ) ;_ end_if
			) ;_ end_foreach
			(while (/= "[/ProjectInfo]" (setq str (read-line file)))
			) ;_ end_while
			(write-line str tmpfile)
		    ) ;_ end_progn
		) ;if
	    ) ;while
	    (prompt "\n项目信息写入配置文件.")
	    (close file)
	    (close tmpfile)
	    (vl-file-delete path)
	    (vl-file-rename tmpfilestr path)
	) ;progn
	;;else
	(prompt (strcat "\nFile_Error！写入项目信息失败!" path))
    ) ;if
    (princ)
) ;_ end_defun

;;设置图形比例尺
;;显示当前比例尺，若修改比例尺，则修改所有块对象的尺度
(defun C:SetNewScale (/ scale)
    (setq scale (Xrecord-Get gl_AppName "SCALE"))
    (if	scale
	(setq gl_MAP_SCALE (atof (car scale)))
    ) ;_ end_if

    (prompt (strcat "\n当前比例尺为1:" (rtos (/ 1000.0 gl_MAP_SCALE) 2 0) ".\n"))
    (setq scale (Fun_InPutValue 1000 "USERR2" "\n输入新的图形比例尺(1:1000输入1000；1:500输入500；)：" 2))
    (if	(= 'REAL (type scale))
	(progn
	    (setq gl_MAP_SCALE (/ 1000.0 scale))
	    (Xrecord-Rebuild gl_AppName "SCALE" (rtos gl_MAP_SCALE 2 4))
	    (SetBlockScale (/ 1.0 gl_MAP_SCALE))
	) ;progn
    ) ;_ end_if
) ;_ end_defun
;;设置管线点图块比例
(defun SetBlockScale (newscale / AllPointBlockSet i j ent obj)
    (setq AllPointBlockSet (ssget "X" (list (cons 0 "INSERT")))
	  i		   0
	  j		   0
    ) ;_ end_setq
    (if	AllPointBlockSet
	(repeat	(sslength AllPointBlockSet)
	    (setq ent (ssname AllPointBlockSet i)
		  obj (vlax-ename->vla-object ent)
	    ) ;_ e_setq
	    (if	(vlax-ldata-get obj gl_AppName)
		(progn
		    ;;修改点号属性
;;;                    (if (= :vlax-true (vla-get-HasAttributes obj))
;;;                        (progn
;;;                            (setq attrib_objs (vlax-safearray->list
;;;                                                  (vlax-variant-value (vla-GetAttributes obj))
;;;                                              ) ;_ e_vlax-safearray->list
;;;                            ) ;_ e_setq
;;;                            (if (> (length attrib_objs) 0)
;;;                                (progn
;;;                                    (setq attrib_obj (car attrib_objs))
;;;;;;						(vla-put-TextString attrib_obj strP);内容
;;;                                    (vla-put-Height attrib_obj (/ 2 newscale)) ;字高
;;;				  (vla-put-ScaleFactor attrib_obj 0.8)
;;;;;;				                (if ts (vla-put-stylename attrib_obj (vla-get-name ts)))
;;;                                ) ;progn
;;;                            ) ;if
;;;                        ) ;progn
;;;                    ) ;if
		    (vla-put-XScaleFactor obj newscale)
		    (vla-put-YScaleFactor obj newscale)
		    (vla-put-ZScaleFactor obj newscale)
		    (setq j (1+ j))
		) ;progn
	    ) ;if
	    (setq i (1+ i))
	) ;repeat
    ) ;if
    (prompt (strcat "\n修改了" (rtos j 2 0) "个图块的比例尺."))
) ;defun

;;读取图元信息
;;显示图元属性
(defun C:LineInfo_GetObjInfo (/ e ldatas typename attribstr)
    (setq dbstr	nil
	  mstr nil
    ) ;_ End_setq
    (if	(setq e (car (entsel "\n选择图元：")))
	(if (setq ldatas(vlax-ldata-get e gl_AppName))
	    (progn
		(setq typename	(cdr (assoc 0 (entget e)))
		      attribstr	"\n管线属性:"
		) ;_ end_setq
		(if	(and gl_DEBUG e)
			(princ (vlax-ldata-list e))
		) ;_ end_if
		(if (= "INSERT" typename)
		    (progn
				(if (not gl_PointFieldList)
					(setq gl_PointFieldList (ReadPointFieldsInfo nil))
				) ;_ end_if
				
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Map_No" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Map_No" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Exp_No" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Exp_No" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Depth" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Depth" ldatas))
					(setq attribstr (strcat attribstr (rtos (cdr data) 2 4)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Point_Size" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Point_Size" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Feature" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Feature" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Subsid" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Subsid" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Project" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Project" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "SProject" gl_PointFieldList)) ":"))
				(if (setq data (assoc "SProject" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Unit" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Unit" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Sur_Date" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Sur_Date" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
		    ) ;_ end_progn
		) ;_ end_if

		(if (= "LINE" typename)
		    (progn
				(if (not gl_LineFieldList)
					(setq gl_LineFieldList (ReadLineFieldsInfo nil))
				) ;_ end_if
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Start_Point" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Start_Point" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "End_Point" gl_LineFieldList)) ":"))
				(if (setq data (assoc "End_Point" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "D_S" gl_LineFieldList)) ":"))
				(if (setq data (assoc "D_S" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Material" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Material" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Pressure" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Pressure" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Voltage" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Voltage" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Cab_Count" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Cab_Count" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Hole_Count" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Hole_Count" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Hole_Used" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Hole_Used" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Flowdirect" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Flowdirect" ldatas))
					(setq attribstr (strcat attribstr (rtos (cdr data) 2 0)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Road_Name" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Road_Name" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Project" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Project" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "SProject" gl_LineFieldList)) ":"))
				(if (setq data (assoc "SProject" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Unit" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Unit" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Sur_Date" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Sur_Date" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
		    ) ;_ end_progn
		) ;_ end_if
		(princ attribstr)
	    ) ;_ end_progn
	    (prompt "\n图元不含管线属性信息。")
	) ;_ end_if
    ) ;if
    
	(textpage)
    (princ)
) ;defun

;;读取点的字段信息
;;返回:点对表 ((Map_No.图号)...)
(defun ReadPointFieldsInfo (path / fieldslist file str pos name1 name2 bEnd)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq fieldslist nil
		bEnd nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (and (not bEnd)(setq str (read-line file)))
		(if (= str "[Point_Attributes]")
			(progn
				(while (/= "[/Point_Attributes]" (setq str (read-line file)))
					(if (/= "//" (substr str 1 2));;注释行
						(setq pos	 (vl-string-search "," str)
							  name1	 (substr str 1 pos) ;first letter is the field name;
							  str	 (substr str (+ 2 pos))
							  pos	 (vl-string-search "," str)
							  name2	 (substr str 1 pos) ;field name (Chinese)

							  fieldslist (cons (cons name1 name2) fieldslist)
						) ;_ end_setq
					)
				) ;while
				(setq bEnd T)
			)
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError！打开文件失败：" path))
    ) ;if
    fieldslist
) ;_ end_defun

;;读取线段的字段信息
;;返回:点对表 ((Start_Point.起点编号)...)
(defun ReadLineFieldsInfo (path / fieldslist file str pos name1 name2 bEnd)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq fieldslist nil
		bEnd nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (and (not bEnd)(setq str (read-line file)))
		(if (= str "[Line_Attributes]")
			(progn
				(while (/= "[/Line_Attributes]" (setq str (read-line file)))
					(if (/= "//" (substr str 1 2));;注释行
						(setq pos	 (vl-string-search "," str)
							  name1	 (substr str 1 pos) ;first letter is the field name;
							  str	 (substr str (+ 2 pos))
							  pos	 (vl-string-search "," str)
							  name2	 (substr str 1 pos) ;field name (Chinese)

							  fieldslist (cons (cons name1 name2) fieldslist)
						) ;_ end_setq
					)
				) ;while
				(setq bEnd T)
			)
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError！打开文件失败：" path))
    ) ;if
    fieldslist
) ;_ end_defun
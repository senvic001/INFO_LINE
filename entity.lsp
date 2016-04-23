;;;管线点线，实体相关文件
;*********************************************************************************************
;函数定义:InitPointAttribs()
;功能：初始化属性参数写入到字段，包括关键字
;参数：pename：点实体名称（限定为点块）
;返回：ename
;创建时间：2014/12/01   12:40
;修改时间：2015/3/9	114:40
;修改数据为一个ldata：(Info_Line (("Map_No"."JS001")  ("Depth".0)......))
;创建人：沈雄君
;*********************************************************************************************
(defun InitPointAttribs(pename  / mapno datalist)
	;;gl_AppName
	; (if (not (vlax-ldata-get pename gl_AppName))
		; (vlax-ldata-put pename gl_AppName gl_Version)	;?!需要修改
	; )
	
	(setq datalist nil)
	(if (setq mapno (car mapno))
		(setq  mapno (cons  "Map_No" mapno))
		(setq  mapno (cons  "Map_No" ""))
	)
	(setq datalist (cons mapno datalist)
		datalist (cons (cons "Version" gl_Version) datalist)
		datalist (cons (cons "Exp_No" "") datalist)
		;datalist (cons (cons "Depth" 0) datalist)
		datalist (cons (cons "Feature" "") datalist)
		datalist (cons (cons "Subsid" "") datalist)
		datalist (cons (cons "Main_Type" "") datalist)
		datalist (cons (cons "Sub_Type" "") datalist)
		datalist (cons (cons "Use_Status" 0) datalist)
		
		datalist (cons (cons "Point_Size" "") datalist)
		datalist (cons (cons "Well_Deep" "") datalist)
		datalist (cons (cons "Text_Pos" "") datalist)
		datalist (cons (cons "Mark_Angle" "") datalist)
		
		datalist (cons (cons "Sur_Date" "") datalist)
		;;datalist (cons (cons "Mdate" "") datalist)
		datalist (cons (cons "SProject" "") datalist)
		datalist (cons (cons "Location" "") datalist)
		datalist (cons (cons "Unit" "") datalist)
		datalist (cons (cons "Project" "") datalist)
		
		datalist (cons (cons "Edge" '()) datalist)
		datalist (cons (cons "Status_DB" 0) datalist)
		datalist (cons (cons "Status_Modify" 0) datalist)
	)
	(vlax-ldata-put pename gl_AppName datalist)
	; ;;Map_No
	; (setq mapno (GetLPointMapNo pename))
	; (if (setq mapno (car mapno))
		; (vlax-ldata-put pename "Map_No" mapno)
		; (vlax-ldata-put pename "Map_No" "")
	; )
	; (vlax-ldata-put pename "Exp_No" "")			;物探点号
	; ;;Depth
	; (vlax-ldata-put pename "Depth" 0)			;深度
	; (vlax-ldata-put pename "Feature" "")			;特征点
	; (vlax-ldata-put pename "Subsid" "")			;附属物
	; (vlax-ldata-put pename "Main_Type" "")		;主类型,13种,单个字符
	; ;(vlax-ldata-put pename "Sub_Type" "")		;子类型,1-3个字符
	; (vlax-ldata-put pename "Use_Status" 0)		;0-废弃;1-使用;2-施工中
	; (vlax-ldata-put pename "Point_Size" "")		;井盖规格,圆形为直径,矩形为X*Y
	
	; (vlax-ldata-put pename "Text_Pos" (list 0 0 0 0))	;点号的标记位置,4个参数(x y z angle)
	; ;(vlax-ldata-put pename "Mark_Angle" 0)		;图块的标准角度
	
	; ;;project info
	; (vlax-ldata-put pename "Sur_Date" "")			;调查时间
	; (vlax-ldata-put pename "SProject" "")		;子项目名称
	; (vlax-ldata-put pename "Project" "")			;大项目名称
	; (vlax-ldata-put pename "Location" "")		;调查位置
	; (vlax-ldata-put pename "Unit" "")			;调查单位
	
	; ;;runtime info
	; (vlax-ldata-put pename "Edge" '())				;点所在的边,对象的句柄((p1 p2 e1)....)
	; (vlax-ldata-put pename "Status_DB" 0)		;数据库状态 0-不在DB ;1-在db中
	; (vlax-ldata-put pename "Status_Modify" 0)	;编辑状态 0-未修改,1-已修改
	
	pename
)
;;得到所有管线点的图上点号mapnamelist
(defun GetMapNamesList( / allp i  pname entp)
	(setq gl_MapNameList nil)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq i 0)
			(repeat (sslength allp)
				(setq entp (ssname allp i))
				(if (setq pname (ldata-get entp "Map_No"))
					(setq gl_MapNameList (cons pname gl_MapNameList))
				)
				(setq i (1+ i))
			)
		)
	)
	;;去掉重复的点号
	(setq gl_MapNameList (ListRemoveSameElement gl_MapNameList))
)
(defun GetExpNamesList( / allp i  pname entp)
	(setq gl_ExpNameList nil)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq i 0)
			(repeat (sslength allp)
				(setq entp (ssname allp i))
				(if (setq pname (ldata-get entp "Exp_No"))
					(setq gl_ExpNameList (cons pname gl_ExpNameList))
				)
				(setq i (1+ i))
			)
		)
	)
	gl_ExpNameList
)

;;判断点号是否重复,
;;参数:val 点名
;;返回:nil or 重复的实体名称列表
(defun HasSameMapName (val  )
	; (setq result nil)
	(if (not gl_MapNameList)
		(setq gl_MapNameList (GetMapNamesList))
	)
	; (if (vl-position val gl_MapNameList)
		; (setq result T)
	; )
	(vl-position val gl_MapNameList)
)
;;判断点名是否唯一
(defun isUniqueMapNo (val / result)
	(setq result T)
	(if (not gl_MapNameList)
		(setq gl_MapNameList (GetMapNamesList))
	)
	(if (member val (cdr (member val gl_MapNameList)))
		(setq result nil)
	)
	result
)
(defun HasSameExpName (val )
	; (setq result nil)
	(if (not gl_ExpNameList)
		(setq gl_ExpNameList (GetExpNamesList))
	)
	; (if (vl-position val gl_ExpNameList)
		; (setq result T)
	; )
	(vl-position val gl_ExpNameList)
)
;;判断物探点号是否唯一
(defun isUniqueExpNo (val / result)
	(setq result T)
	(if (not gl_ExpNameList)
		(setq gl_ExpNameList (GetExpNamesList))
	)
	(if (member val (cdr (member val gl_ExpNameList)))
		(setq result nil)
	)
	result
)
;;根据现有点号,生成下一个唯一点号
;;参数:前一个点号 or nil，maintype 点号前缀 ,NameType :"EXP" "MAP" 两种
;;返回:新的唯一点号
(defun CreateNewPointName (preName maintype NameType / newname prestr prestr0 pcount bfound allp np i pname )
	(if (= nil maintype ) (setq maintype "J"))
	(if (= nil preName) 
		(setq prestr maintype
			pcount 101
			newname (strcat maintype "101"))
		;else 
		(setq preName (strcase preName)
			prestr0 (vl-string-left-trim "0123456789-+._" preName);点号前缀-子类型	
			prestr (vl-string-right-trim "0123456789-_+." prestr0)
			pcount (1+ (atoi (vl-string-left-trim "ABCDEFGHIJKLMNOPQRSTUVWXYZ_ -" preName)))
			newname (strcat prestr (rtos pcount 2 0))
		)
	)
	(setq bfound nil)
	(if (= NameType "MAP")
		(progn
			(if (not gl_MapNameList)
				(setq gl_MapNameList (GetMapNamesList))
			)
			(while (vl-position newname gl_MapNameList)
				(setq pcount (1+ pcount)
					newname (strcat prestr (rtos pcount 2 0))
				)	
			)
		)	
	)	
	(if (= NameType "EXP")
		(progn
			(if (not gl_ExpNameList)
				(setq gl_ExpNameList (GetExpNamesList))
			)
			(while (vl-position newname gl_ExpNameList)
				(setq pcount (1+ pcount)
					newname (strcat prestr (rtos pcount 2 0))
				)	
			)
		)	
	)
	newname
)

;;设置和获取管线点的点号属性
;;参数：entp 点实体名称；attrstr 属性名称
;;返回:nil or text
(defun GetAttrib-PointName(entp attrstr / eobj attrib_objs e text)
	(setq text nil)
	(if (vlax-ldata-get entp gl_AppName)
		(progn
			(setq eobj (vlax-ename->vla-object entp)
			)
			;;获取点号文字属性
			(if (= :vlax-true (vla-get-HasAttributes eobj))
				(progn
					(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes eobj))))
					(foreach e attrib_objs
						(if (= attrstr (vla-get-tagstring e))
							(setq text (vla-get-TextString e));内容
						)
					)
				 );progn
			);if
		)
	)
	text
)
;;设置和获取管线点的点号属性
;;参数：entp 点实体名称；attrstr 属性名称;text 属性文字内容
;;返回nil or T
(defun SetAttrib-PointName(entp attrstr text / eobj attrib_objs e ret)
	(setq ret nil)
	(if (vlax-ldata-get entp gl_AppName)
		(progn
			(setq eobj (vlax-ename->vla-object entp)
			)
			;;获取点号文字属性
			(if (= :vlax-true (vla-get-HasAttributes eobj))
				(progn
					(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes eobj))))
					(foreach e attrib_objs
						(if (= attrstr (vla-get-tagstring e))
							(progn
								(vla-put-TextString e text);内容
								(setq ret T)
							)	
						)
					)
				 );progn
			);if
		)
	)
	ret
)
;;;设置实体属性
;;;PutEntityAttribs
;;;entname-实体名称,AttribsList-属性列表(("Map_No" "JS101")()....)
(defun PutEntityAttribs(entname AttribsList /  attrib_obj attrib_objs e ename eobj i mapno mname )
	(foreach e AttribsList
		(if (cdr e)
			(ldata-put entname (car e) (cdr e))
		)
	)
	(setq mname (cdr (assoc "Map_No" AttribsList))
		ename (cdr (assoc "Exp_No" AttribsList)))
	(if (or mname ename)
		(progn
			;;读取属性中的Map_No
			(setq eobj (vlax-ename->vla-object entname))
			(if (= :vlax-true (vla-get-HasAttributes eobj))
				(progn
					(setq attrib_objs (vlax-safearray->list
										  (vlax-variant-value (vla-GetAttributes eobj))
									  ) ;_ end_vlax-safearray->list
					) ;_ end_setq
					(setq i 0)
					(if (> (length attrib_objs) 0)
						(while (and (< i (length attrib_objs)) (= "点号" (vla-get-tagstring (nth i attrib_objs))))
							(setq attrib_obj (nth i attrib_objs)
								  mapno     (vla-Get-TextString attrib_obj) ;内容
							) ;_ end_setq
							(if (and (/= nil mname) (/= mapno mname))
								(vla-Put-TextString attrib_obj mname)
								(if (and (/= nil ename) (= nil mname) (/= mapno ename))
									(vla-Put-TextString attrib_obj ename))
							)
							(setq i (1+ i))
						)
					) ;if
				) ;progn
			) ;if
		)
	)
	entname
)

;*********************************************************************************************
;函数定义:GetLPointMapNo()
;功能：从属性块中获取管线点的点号,以及从ldata Map_No获取点号，则返回两个名称。
;(block_Map_No ldata-getMap_No)
;参数：pename：点实体名称（限定为点块）
;返回：MapNolist or nil
;创建时间：2014/12/01   13:20
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun GetLPointMapNo(pename /  attrib_obj attrib_objs eobj i Map_No Map_No2 result)

	;;修改点号属性
	(if (not (setq eobj (vlax-ename->vla-object pename)))
		(progn
			(prompt "\n非有效实体。")
			(exit)
		)	
	)
	(setq result nil)
	
	;;读取ldata中的点号
	(if (setq Map_No2 (ldata-get pename "Map_No"))
		(setq result (list Map_No2))
		(setq result (list nil))
	)
	;;读取属性中的Map_No
	(if (= :vlax-true (vla-get-HasAttributes eobj))
		(progn
			(setq attrib_objs (vlax-safearray->list
								  (vlax-variant-value (vla-GetAttributes eobj))
							  ) ;_ end_vlax-safearray->list
			) ;_ end_setq
			(setq i 0)
			(if (> (length attrib_objs) 0)
				(while (and (< i (length attrib_objs)) (= "点号" (vla-get-tagstring (nth i attrib_objs))))
					(setq attrib_obj (nth i attrib_objs)
						  Map_No     (vla-Get-TextString attrib_obj) ;内容
						  result (cons Map_No result)
						  i (1+ i)
					) ;_ end_setq
				)
				
			) ;if
		) ;progn
	) ;if
	(if (= nil result) (setq result (cons  nil result)))
	
	;;
	result
)


;*********************************************************************************************
;函数定义:InitLineAttribs()
;功能：初始化属性参数写入到字段，包括关键字
;参数：lename：线实体名称（限定为直线段）
;返回：lename
;创建时间：2014/12/01   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun InitLineAttribs(lename / datalist)
	;;gl_AppName
	; (if (not (vlax-ldata-get lename gl_AppName))
		; (vlax-ldata-put lename gl_AppName gl_Version)
	; )
	(setq datalist nil)
	
	(setq datalist (cons (cons "Version" gl_Version) datalist)
		
		datalist (cons (cons "Start_Point" "") datalist)
		datalist (cons (cons "End_Point" "") datalist)
		datalist (cons (cons "Material" "") datalist)
		datalist (cons (cons "Main_Type" "") datalist)
		datalist (cons (cons "Sub_Type" "") datalist)
		datalist (cons (cons "Use_Status" 0) datalist)
		
		datalist (cons (cons "D_S" "") datalist)
		datalist (cons (cons "Cab_Count" "") datalist)
		datalist (cons (cons "Hole_Count" "") datalist)
		datalist (cons (cons "Hole_Used" "") datalist)
		datalist (cons (cons "Pressure" "") datalist)
		datalist (cons (cons "Voltage" "") datalist)
		datalist (cons (cons "Road_Name" "") datalist)
		datalist (cons (cons "Flowdirect" 0) datalist)
		
		datalist (cons (cons "P_Material" "") datalist)
		datalist (cons (cons "P_D_S" "") datalist)
		
		datalist (cons (cons "Remark" "") datalist)
		datalist (cons (cons "Property" "") datalist)
		;datalist (cons (cons "BuryWay" "0") datalist)
		
		datalist (cons (cons "Text_Pos"  (list 0 0 0 0 )) datalist)
		datalist (cons (cons "M_Text" "") datalist)
		datalist (cons (cons "Label" "") datalist)
		
		datalist (cons (cons "Sur_Date" "") datalist)
		datalist (cons (cons "Mdate" "") datalist)
		datalist (cons (cons "SProject" "") datalist)
		datalist (cons (cons "Location" "") datalist)
		datalist (cons (cons "Unit" "") datalist)
		datalist (cons (cons "Project" "") datalist)
		
		datalist (cons (cons "Edge" '()) datalist)
		datalist (cons (cons "Status_DB" 0) datalist)
		datalist (cons (cons "Status_Modify" 0) datalist)
	)
	(vlax-ldata-put lename gl_AppName datalist)
	; ;;通用属性
	; (vlax-ldata-put lename "Start_Point"  "")			;起点编号
	; (vlax-ldata-put lename "End_Point" "")			;终点编号
	; (vlax-ldata-put lename "Material" "")			;材质
	; (vlax-ldata-put lename "Main_Type" "")		;主类型,13种,单个字符
	; (vlax-ldata-put lename "Sub_Type" "")		;子类型,1-3个字符
	; (vlax-ldata-put lename "Use_Status" 0)		;0-废弃;1-使用;2-施工中
	
	; ;;属性
	; (vlax-ldata-put lename "D_S" "")				;管径或尺寸DN100圆管,或者100*100方沟 mm
													; ;适用于:非电信电力管道
	; (vlax-ldata-put lename "Cab_Count" "")		;管线数:电力或电信
	; (vlax-ldata-put lename "Hole_Count" "")		;孔数:电力或电信
	; (vlax-ldata-put lename "Hole_Used" "")		;使用孔数:电力或电信
	; (vlax-ldata-put lename "Pressure" "")		;压力:
	; (vlax-ldata-put lename "Voltage" "")			;电压;kV V
	; (vlax-ldata-put lename "Road_Name" "")		;所在道路名称
	; (vlax-ldata-put lename "Flowdirect" 0)		;流向:使用与排水和给水,或许石油燃气热水
													; ;流向 0 start-end ,1 end-start-end
	; (vlax-ldata-put lename "P_Material" "")			;保护套材质												
	; (vlax-ldata-put lename "P_D_S" "")			;保护套规格

	; (vlax-ldata-put lename "Remark" "")			;备注
	; (vlax-ldata-put lename "Property " "")			;权属单位	
	; (vlax-ldata-put lename "BuryWay " "")			;埋设方式	
													
	; ;;标记
	; (vlax-ldata-put lename "Text_Pos" (list 0 0 0 0 ))	;点号的标记位置,4个参数(x y z angle)
	; ;(vlax-ldata-put lename "M_Text" "")			;标记文字内容;
	; (vlax-ldata-put lename "Label" "") ;存放标记的对象句柄
	
	; ;;project info
	; (vlax-ldata-put lename "Sur_Date" "")			;调查时间
	; (vlax-ldata-put lename "SProject" "")	;子项目名称
	; (vlax-ldata-put lename "Project" "")	;大项目名称
	; (vlax-ldata-put lename "Location" "")		;调查位置
	; (vlax-ldata-put lename "Unit" "")			;调查单位
	
	; ;;runtime info
	; (vlax-ldata-put lename "Edge" '())				;点所在的边,对象的句柄((p1 p2 e1))
	; (vlax-ldata-put lename "Status_DB" 0)			;数据库状态 0-不在DB ;1-在db中
	; (vlax-ldata-put lename "Status_Modify" 0)		;编辑状态 0-未修改,1-已修改,2-被删除，3-
	lename
)
;;设置实体的属性，使之bylayer
(defun SetDefaultLayer(ent maintype)
	(AddLayer maintype (strcat maintype "_Line"))
	(vla-put-layer (vlax-ename->vla-object ent) (strcat maintype "_Line"))
)
;*********************************************************************************************
;函数定义:AddNewPoint(p1 pname maintype featurestr subsidstr attribslist)
;功能：加入一个新建的管线点
;参数：p1-(x y z)坐标,maintype-主类型名称,attribslist-属性表
;		pname 点名字符串，mapname,featurestr-特征字符串 subsidstr-附属物)
;返回：管线点实体名
;创建时间：2014/12/05   10:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
 (defun AddNewPoint(p1 pname maintype featurestr subsidstr attribslist / attrib_objs e entp 
		layerobj newblock_ref symbolpath ts sstr symname  app Doc Mspace expname mapname)
	(setq layerobj (AddLayer maintype (strcat maintype "_Point")))
	
	(setq ts (Addfont 'acstyle))
   ;;
   ;;从图库创建图块
   (setq app	 (vlax-get-acad-object)
		  Doc	 (vla-get-Activedocument app)
		  Mspace (vla-get-modelspace Doc)
		  ) ;_ end setq
   (setq symname  (GetSymbolFileName featurestr subsidstr maintype)
		blockname (vl-filename-base symname)
		symbolpath (strcat gl_BLOCKLIB_PATH symname)) ;;; (vla-item blocks blockname)
		
	(if	(vl-position blockname gl_BlockNameList)
		(setq newblock_ref  (vla-InsertBlock   Mspace	(vlax-3D-point p1)  blockname  1 1 1 0))
		(setq newblock_ref  (vla-InsertBlock   Mspace	(vlax-3D-point p1)  symbolpath  1 1 1 0)
			gl_BlockNameList (cons blockname gl_BlockNameList)
		)
	)
   ;(setq newblock_ref  (vla-InsertBlock   Mspace	(vlax-3D-point p1)  symbolpath  1 1 1 0))
   ;;修改点号文字属性
	(if (= :vlax-true (vla-get-HasAttributes newblock_ref))
		(progn
			(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
			(foreach e attrib_objs
				(if (= "点号" (vla-get-tagstring e))
					(progn 
						(vla-put-TextString e pname);内容
						(vla-put-Height e 2);字高
						(vla-put-scalefactor e 0.8);宽度因子
						(if ts (vla-put-stylename e (vla-get-name ts)))
					)
				)
			)
		 );progn
    );if
	
	;;修改管线点属性
	(setq entp (vlax-vla-object->ename newblock_ref))
	(InitPointAttribs entp)
	(PutEntityAttribs entp attribslist)
	
	(vla-put-XScaleFactor newblock_ref (/ 1.0 gl_MAP_SCALE))
	(vla-put-YScaleFactor newblock_ref (/ 1.0 gl_MAP_SCALE))
    	(vla-put-ZScaleFactor newblock_ref (/ 1.0 gl_MAP_SCALE))
	(vla-put-layer newblock_ref (strcat maintype "_Point"))
    
	;;设置AppID 用于ssget
	(setxdata entp gl_AppName (list (cons 1000 gl_Version))) 
	
	;;把名字加入点表，When creating newpoint ,mapname and expname have the same name.
	;;else ,only change the property of name
	; (if (not gl_MapNameList)
		; (setq gl_MapNameList (GetMapNamesList))
	; )
	; (setq gl_MapNameList (cons pname gl_MapNameList))
	
	;;加入索引
	(if gl_PointSpaceIndex
		(setq gl_PointSpaceIndex (PutEntityIndex gl_PointSpaceIndex entp (car p1) (cadr p1)))
	)
    entp
 )
 
 ;;删除管线对象(点对象和线对象),同时从索引中删除
 (defun DelEntity(ename)
	(if (= 'ENAME (type ename))
		(progn
			(setq elist (entget ename) 
				tname (cdr (assoc 0 elist)))
			(cond 
				((= tname "LINE") 
					(if gl_LineSpaceIndex
						(progn
							(setq p10 (cdr (assoc 10 elist))
								p11 (cdr (assoc 11 elist))
							)
							(setq gl_LineSpaceIndex (DelEntityIndex gl_LineSpaceIndex ename (car p10) (cadr p10)))
							(setq gl_LineSpaceIndex (DelEntityIndex gl_LineSpaceIndex ename (car p11) (cadr p11)))
						)
					)
				)
				((= tname "INSERT")
					(if gl_PointSpaceIndex 
						(progn
							(setq p10 (cdr (assoc 10 elist)))
							(setq gl_PointSpaceIndex (DelEntityIndex gl_PointSpaceIndex ename (car p10) (cadr p10)))
						)
					)
				)
			)
			(vla-delete (vlax-ename->vla-object ename))	
		)
	)
 )
 
;*********************************************************************************************
;函数定义:AddNewLine(p10 p11 maintype attribslist)
;功能：加入一个新建的管线段
;参数：p10 p11 -线段坐标,maintype-主类型名称,attribslist-属性表
;返回：管线点实体名
;创建时间：2014/12/05   10:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
 (defun AddNewLine(p10 p11 maintype attribslist / lineobj entl d_s)
	(AddLayer maintype (strcat maintype "_Line"))

	(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
                                      	 (vlax-3D-point p10)
                                      	 (vlax-3D-point p11)))
	;;修改管线段属性
	(setq entl (vlax-vla-object->ename lineobj))
	(InitLineAttribs entl)
	(PutEntityAttribs entl attribslist)
	(vla-put-layer lineobj (strcat maintype "_Line"))

    (UpdateLabel entl (ldata-get entl "Start_Deep") (ldata-get entl "End_Deep"))
	;;设置AppID 用于ssget
	(setxdata entl gl_AppName (list (cons 1000 gl_Version))) 
	
	;;D_S作为线宽
	(if (setq d_s (ldata-get entl "D_S"))
		(if (> (strlen d_s) 0)
			(vla-put-LineWeight lineobj (GetLnWtFromDS (atoi d_s)))
		)
	)
	;;加入索引
	(if gl_LineSpaceIndex
		(progn
			(setq gl_LineSpaceIndex (PutEntityIndex gl_LineSpaceIndex entl (car p10) (cadr p10)))
			(setq gl_LineSpaceIndex (PutEntityIndex gl_LineSpaceIndex entl (car p11) (cadr p11)))
		)
	)
    entl
 )
 
 ;;根据管径获取线宽
 ;;d_s 为数字,单位mm
 (defun GetLnWtFromDS (d_s / lineweight )
	(setq lineweight acLnWtByLayer )
	(if (or (= 'INT (type d_s)) (= 'REAL (type d_s)))
		(cond 
			((and (>= d_s 190.0) (< d_s 225.0)) (setq lineweight acLnWt020))
			((and (>= d_s 225.0) (< d_s 275.0)) (setq lineweight acLnWt025))
			((and (>= d_s 275.0) (< d_s 325.0)) (setq lineweight acLnWt030))
			((and (>= d_s 325.0) (< d_s 375.0)) (setq lineweight acLnWt035))
			((and (>= d_s 375.0) (< d_s 450.0)) (setq lineweight acLnWt040))
			((and (>= d_s 450.0) (< d_s 515.0)) (setq lineweight acLnWt050))
			((and (>= d_s 515.0) (< d_s 565.0)) (setq lineweight acLnWt053))
			((and (>= d_s 565.0) (< d_s 650.0)) (setq lineweight acLnWt060))
			((and (>= d_s 650.0) (< d_s 750.0)) (setq lineweight acLnWt070))
			((and (>= d_s 750.0) (< d_s 850.0)) (setq lineweight acLnWt080))
			((and (>= d_s 850.0) (< d_s 950.0)) (setq lineweight acLnWt090))
			((and (>= d_s 950.0) (< d_s 1030.0)) (setq lineweight acLnWt100))
			((and (>= d_s 1030.0) (< d_s 1130.0)) (setq lineweight acLnWt106))
			((and (>= d_s 1130.0) (< d_s 1300.0)) (setq lineweight acLnWt120))
			((and (>= d_s 1300.0) (< d_s 1490.0)) (setq lineweight acLnWt140))
			((and (>= d_s 1490.0) (< d_s 1790.0)) (setq lineweight acLnWt158))
			((and (>= d_s 25.0) (< d_s 70.0)) (setq lineweight acLnWt005))
			((and (>= d_s 70.0) (< d_s 110.0)) (setq lineweight acLnWt009))
			((and (>= d_s 110.0) (< d_s 140.0)) (setq lineweight acLnWt013))
			((and (>= d_s 140.0) (< d_s 165.0)) (setq lineweight acLnWt015))
			((and (>= d_s 165.0) (< d_s 190.0)) (setq lineweight acLnWt018))
			((and (>= d_s 1790.0) (< d_s 2055.0)) (setq lineweight acLnWt200))
			((< d_s 25) (setq lineweight acLnWt000))
			((>= d_s 2055.0) (setq lineweight acLnWt211))
		)
	)
	lineweight
 )
 
 ;;获取线段在Z轴投影长度和坡度
 ;;如果带流向，与流向一致，则为正；否则为负值
 ;;如果长度小于gl_MIN，则坡度为nil
 (defun GetLineProjectdLength_Slope (entl / delth entlist  length_l lx p10 p11 podu)
	(setq entlist (entget entl)
		p10(cdr (assoc 10 entlist))
		p11 (cdr (assoc 11 entlist))
		deltH (- (last p10) (last p11))
	  
		p10 (list (car p10) (cadr p10)) 
		p11 (list (car p11) (cadr p11))
		length_l (distance p10 p11)
	)
	;;length=0
	(if (>= length_l gl_MIN)
		(setq podu (abs (/ deltH length_l)))
		(setq podu nil)
	)
	
	;;流向
	(if (and podu (setq lx (ldata-get entl "Flowdirect")))
		(if (or (and (= lx 1) (< deltH 0)) (and (= lx 2) (> deltH 0))) 
			(setq podu (- 0 podu))
		)
	)
	
	(list length_l podu)
 )
 
 ;;得到线段的起点和终点高程(d1 d2)
 (defun GetDeeps(entl / d1 d2 edge endp entlist name1 name2 outdata p1 p10 p11 p2 startp)
	(setq edge (ldata-get entl "Edge")
		outdata (list 99999 99999))
	(setq d1 (ldata-get entl "Start_Deep")
		 d2 (ldata-get entl "End_Deep")
	) 	
	(if (and  d1 d2)
		(setq outdata (list d1 d2))
		;;兼容3.0版，把深度写在井上
		; (progn
			; (if edge
				; (progn
					; (setq p1 (handent (car edge))
						; p2 (handent (cadr edge)))
					; (if (and  p1 p2)
						; (progn
							; (setq name1 (ldata-get p1 "Map_No")
								; d1 (ldata-get p1 "Depth")
								; name2 (ldata-get p2 "Map_No")
								; d2 (ldata-get p2 "Depth")
								; startp (ldata-get entl "Start_Point")
								; endp (ldata-get entl "End_Point")
							; )
							; (if (= name1 startp) 
								; (setq outdata (list d1 d2))
								; (setq outdata (list d2 d1))
							; )
						; )
						; (progn
							; (ldata-delete entl "Edge")
							; (setq edge nil)
						; )
					; )
				; )
			; )
			; (if (not edge)	;;无边
				; (progn
					; (setq entlist (entget entl)
						; p10 (cdr (assoc 10 entlist))
						; p11 (cdr (assoc 11 entlist))
						; p1 (GetConnectEntity (car p10) (cadr p10) "INSERT")
						; p2 (GetConnectEntity (car p11) (cadr p11) "INSERT")
					; )
					; (if (and p1 p2)
						; (progn
							; (if (and (<= 1 (length p1)) (<= 1 (length p2)))
								; (progn
									; (setq p1 (car p1)
										; p2 (car p2)
										; name1 (ldata-get p1 "Map_No")
										; d1 (ldata-get p1 "Depth")
										
										; name2 (ldata-get p2 "Map_No")
										; d2 (ldata-get p2 "Depth")
										; startp (ldata-get entl "Start_Point")
										; endp (ldata-get entl "End_Point")
									; )
									
									; ;;针对雨篦的特殊处理（兼容性考虑：3.4之前深度放在点上）
									; (if (and d1 d2)
										; (progn
											; ;;雨水篦和污水篦
											; (if (setq subsidstr (ldata-get p2 "Subsid"))
												; (if (or (= subsidstr "雨水篦") (= subsidstr "污水篦"))
													; (if (> d2 gl_MIN)
														; (if (> (/ d1 d2) 2)
															; (setq d1 (- d2 0.06))
														; )
													; )	
												; )
											; )
											; (if (setq subsidstr (ldata-get p1 "Subsid"))
												; (if (or (= subsidstr "雨水篦") (= subsidstr "污水篦"))
													; (if (> d1 gl_MIN)
														; (if (> (/ d2 d1) 2)
															; (setq d2 (- d1 0.06))
														; )
													; )	
												; )
											; )
										; )
									; )
									
									; (if (= name1 startp) 
										; (setq outdata (list d1 d2))
										; (setq outdata (list d2 d1))
									; )
								; )	
							; )	
						; )	
					; )
				; )
			; )
		; )
	)
	; (if (or (= nil (car outdata)) (= nil (cadr outdata)))
		; (progn
			; (prompt "\n直线段上不含高程属性，用99999代替高程。")
			; (if (= nil (car outdata))
			; (setq outdata (list 99999 (cadr outdata))))
			; (if (= nil (cadr outdata))
			; (setq outdata (list (car outdata) 99999)))
		; )
	; )
	outdata
 )
 

;*********************************************************************************************
;函数定义:GetTypedLineList()
;功能：对所有线段按照管线类别分类，类别由管线对象的Main_Type属性确定。
;参数：
;返回：((J (l1 l2 l3)) (W (w1 w2..))...) 类别 图元名称列表
;创建时间：2014/12/16   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun GetTypedLineList( / AllLineList i LinesList typelist newlist)
	(setq AllLineSet (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName))))
		i 0
		LinesList nil 
		typelist nil)
	(if AllLineSet
		(repeat (sslength AllLineSet)
			(setq ent (ssname AllLineSet i))
			(if (vlax-ldata-get ent gl_AppName)
				(progn
					(setq tname  (ldata-get ent "Main_Type"))
					(if (setq typelist (assoc tname LinesList))
						(setq newlist (cons ent (cadr typelist))
							LinesList (subst (cons tname (list newlist)) typelist LinesList)
						)
						;;else
						(setq newlist (cons tname (list (list ent)))
							LinesList (cons newlist LinesList )
						)
					)
				)
			)
			(setq i (1+ i))
		)
	)
	LinesList
)
;*********************************************************************************************
;函数定义:GetTypedPointList()
;功能：对所有线点按照管线类别分类，类别由管线对象的Main_Type属性确定。
;参数：
;返回：((J.(l1 l2 l3)) (W.(w1 w2..))...) 类别.图元名称列表
;创建时间：2014/12/16   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun GetTypedPointList( / AllPointList i PointssList typelist newlist)
	(setq AllPointSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
		i 0
		PointssList nil 
		typelist nil
		newlist nil)
	(if AllPointSet
		(repeat (sslength AllPointSet)
			(setq ent (ssname AllPointSet i))
			(if (vlax-ldata-get ent gl_AppName)
				(progn
					(setq tname  (ldata-get ent "Main_Type"))
					(if (setq typelist (assoc tname PointssList))
						(setq newlist (cons ent (cadr typelist))
							PointssList (subst (cons tname (list newlist)) typelist PointssList)
						)
						;;else
						(setq newlist (cons tname (list (list ent)))
							PointssList (cons newlist PointssList)
						)
					)
				)
			)
			(setq i (1+ i))
		)
	)
	PointssList
)

;*********************************************************************************************
;函数定义:C:StatisticAllEntity()
;;;功能:统计管线资料，在控制台输出。
;;;参数:无
;;;返回:无
;;;创建时间：2015/1/15   12:40
;;;修改时间：
;;;创建人：沈雄君
;*********************************************************************************************
(defun C:StatisticAllEntity( / alllinelist allpointlist e el ep  imingxian len lenall linelist
				nall nline nmingall npoint nyibiall pointlist subsidstr tname typelist typestr 
				entlist p10 p11 len0 i len ent allpl)
	(setq AllLineList (GetTypedLineList)
		AllPointList (GetTypedPointList)
	)
	(if (not gl_TableColorList)
		(setq gl_TableColorList (ReadColorConfig nil))
	)
	(prompt "\n管线点统计结果:")
	;;管线段分类,检查,并写入关联的边
	(prompt "\n\t类别\t点数\t明显管线点\t隐蔽管线点")
	(setq nAll 0
		nmingAll 0
		nYibiAll 0)
	(foreach e AllPointList
		(setq tname (car e)
			pointlist (cadr e)
			npoint (length pointlist)
			typestr nil
			imingxian 0
			nAll (+ nAll npoint)
		)
		(setq typelist (assoc tname gl_TableColorList)
			typestr (strcat (car typelist) "-" (caddr typelist))
		)
		(if (> npoint 0)
			(progn
				(foreach ep pointlist
					;;判断附属物,区分隐蔽和明显管线点
					(if (setq subsidstr (ldata-get ep "Subsid"))
						(if (and (/= "无" subsidstr) (> (strlen subsidstr) 0))
							(setq imingxian (1+ imingxian))
						)
					)
				)
				(setq nmingAll (+ nmingAll imingxian)
					nYibiAll (+ nYibiAll (- npoint imingxian))
				)
				(prompt (strcat "\n\t" typestr "\t" (rtos npoint 2 0) "\t" (rtos imingxian 2 0) "\t" (rtos (- npoint imingxian) 2 0)))
			)
		)
	)
	(prompt (strcat "\n管线点总数:" (rtos nAll 2 0) "\t明显管线点总数为:" (rtos nmingAll 2 0) "\t隐蔽管线点总数为:" (rtos nYibiAll 2 0) "."))
	
	;;统计各个类别线段长度
	(if (= gl_TableColorList nil)
		(setq gl_TableColorList (ReadColorConfig nil))
	) ;if
	(prompt "\n管线段统计结果:")
	(prompt "\n\t类别\t条数\t管线段长度")
	(setq lenAll 0
		nAll 0) ;总长度
	(foreach e AllLineList
		(setq tname (car e)
			linelist (cadr e)
			nline (length linelist)
			len 0
			typestr nil
			nAll (+ nAll nline)
		)
		(setq typelist (assoc tname gl_TableColorList)
			typestr (strcat (car typelist) "-" (caddr typelist))
		)
	
		(if (> nline 0)
			(progn
				(foreach el linelist
					;;管线长度为平面长度
					(setq entlist (entget el)
						p10 (cdr (assoc 10 entlist))
						p11 (cdr (assoc 11 entlist))
						len0 (distance (list (car p10) (cadr p10)) (list (car p11) (cadr p11)))
						len (+ len len0)
					)
					;;(setq len (+ len (vla-get-length (vlax-ename->vla-object el))))
				)
				(setq lenAll (+ lenAll len))
				(prompt (strcat "\n\t" typestr "\t" (rtos nline 2 0) "\t" (rtos len 2 3) ))
			)
		)
	)
	(prompt (strcat "\n管线段条数:" (rtos nAll 2 0) ",   总长度:" (rtos lenAll 2 3) "m."))
	
	;;统计边界线长度
	(setq i 0 len 0)
	(if	(setq allpl (ssget "X" (list (cons 0 "POLYLINE") (list -3 (list gl_AppName)))))
		(progn
				(repeat	(sslength allpl)
				(setq ent (ssname allpl i)
					i  (1+ i)
				)
				(setq len (+ len (vla-get-length (vlax-ename->vla-object ent))))
			) ;_ end_repeat
			(prompt (strcat "\n边界条数:" (rtos i 2 0) ",   总长度:" (rtos len 2 3) "m."))
		)
	) ;_ end_if
	(princ)
)


;;输出横剖面表
(defun C:HSectionTable	(/		  aclayer  app		blockname		  cabcount cabused	d1		 d2		  data	   datalist	depthstr dist	  dist1	   dist2	doc		 ds		  dstr	   e1
						 e2		  ent1	   ent2		entlist	 index	  interps  layers	ldata	 len	  lineobj  mater	materialstr		  mspace   ndata	newarray newblock objblocks
						 p0		  p1	   p2		pt1		 pt2	  plineobj pmax		pmin	 press	  sxstr	   tabname	textpos	 tslayer  typestr  vol		x0		 xnum	  xrefset  y0
						 y01	  txth	   dimz		deeps)
	(prompt "\n绘制管线横剖面表：")
	(setq pt1	  (getpoint "\n选择横剖面起点：")
		  pt2	  (getpoint "\n选择横剖面终点：")
		  textpos (getpoint "\n选择剖面文字插入点：")
		  ) ;_ End_setq
	(if	(or (= nil pt1) (= nil pt2) (= nil textpos))
		(progn
			(prompt "\n取消选择坐标，退出。")
			(exit)
			) ;_ End_progn
		) ;_ End_if

	;;1创建多段线,Z=0
	(setq app	 (vlax-get-acad-object)
		  Doc	 (vla-get-Activedocument app)
		  Mspace (vla-get-modelspace Doc)
		  ) ;_ End_setq
	;;加入图层
	(setq Layers  (vla-get-layers Doc)
		  AcLayer (vla-get-activelayer Doc)
		  TSlayer (vla-add Layers "Text_Section")
		  ) ;_ End_setq
	(vla-put-activelayer Doc (vla-item Layers "Text_Section"))

	(setq newarray (vlax-make-safearray
					   vlax-vbDouble
					   (cons 0 3)
					   ) ;_ End_vlax-make-safearray
		  ) ;_ End_setq
	(vlax-safearray-fill newarray
						 (list (car pt1) (cadr pt1) (car pt2) (cadr pt2))
						 ) ;_ End_vlax-safearray-fill
	(setq plineobj (vla-addlightweightpolyline Mspace
											   (vlax-make-variant newarray)
											   ) ;_ End_vla-addlightweightpolyline
		  ent1	   (vlax-vla-object->ename plineobj)
		  ) ;_ End_setq

	;;2选择所有直线
	(vla-GetBoundingBox plineobj 'pmin 'pmax)
	(vla-zoomwindow app pmin pmax)
	(setq pmin (vlax-safearray->list pmin)
		  pmax (vlax-safearray->list pmax)
		  pmin (list (nth 0 pmin) (nth 1 pmin))
		  pmax (list (nth 0 pmax) (nth 1 pmax))
		  ) ;_ end setq

	(setq xrefset (ssget "C"
						 pmin
						 pmax
						 (list (cons 0 "LINE"))
						 ) ;_ End_ssget
		  ) ;_ end setq


	(if	(or (= nil xrefset) (= 0 (sslength xrefset)))
		(progn
			(prompt "\n未选择到线段，退出。")
			(vla-delete (vlax-ename->vla-object ent1))
			(exit)
			) ;_ End_progn
		) ;_ End_if

	(setq xnum	   (sslength xrefset)
		  index	   0
		  datalist nil
		  ) ;_ end setq

	;;3计算交点
	(repeat	xnum
		(setq ent2 (ssname xrefset index)
			  ) ;_ End_setq
		(if	(setq interps (m_IntersectWith ent1 ent2))
			(progn
				(setq p0	   (car interps) ;直线段只有唯一的交点
					  dist	   (vlax-curve-getDistAtPoint plineobj
														  (list (car p0) (cadr p0) 0)
														  ) ;_ End_vlax-curve-getDistAtPoint
					  datalist (cons (list dist ent2 p0) datalist) ;位置，图元名，交点坐标；
					  ) ;_ End_setq
				) ;_ End_progn
			) ;_ End_if
		(setq index (1+ index))
		) ;_ End_repeat
	
	;;4根据距离排序
	(setq datalist (vl-sort
					   datalist
					   (function
						   (lambda (e1 e2)
							   (< (car e1)
								  (car e2)
								  ) ;_ End_<
							   ) ;_ End_lambda
						   ) ;_ End_function
					   ) ;_ End_vl-sort
		  ) ;_ End_setq

	;;5输出文字内容到CAD:类型，材质，管径（截面），条数，孔数，电压（属性），深度
;;;    (if datalist
;;;        (setq num (length datalist))
;;;    ) ;_ End_if
	;;文字输出，创建一个块
	(setq dimz (getvar "DIMZIN"))
	(setvar "DIMZIN" 1)

	(setq ts (AddFont 'acts))
	(setq objBlocks (vla-get-Blocks Doc))
	(setq BlockName (strcat "DB_" (rtos (* 100000000 (ZL-RAND)) 2 0)))

	;;创建块,块名随机
	(while (not
			   (setq newblock (vla-Add objBlocks (vlax-3D-point 0 0 0) BlockName))
			   ) ;_ End_not
		(setq BlockName (strcat "DB_" (rtos (* 100000000 (ZL-RAND)) 2 0)))
		) ;_ End_while


	(setq index	0
		  ndata	0
		  ) ;_ End_setq
	(repeat	(length datalist)
		(setq data	  (nth index datalist)
			  ent2	  (nth 1 data)
			  p0	  (nth 2 data)
			  entlist (entget ent2)
			  p1	  (cdr (assoc 10 entlist))
			  p2	  (cdr (assoc 11 entlist))
			  index	  (1+ index)
			  ) ;_ End_setq
		(if	 (vlax-ldata-get ent2 gl_AppName)
			(progn
				(setq deeps	   (GetDeeps ent2)
					  d1	   (car deeps)
					  d2	   (cadr deeps)
					  DS	   (ldata-get ent2 "D_S")
					  Press	   (ldata-get ent2 "Pressure")
					  Vol	   (ldata-get ent2 "Voltage")
					  Mater	   (ldata-get ent2 "Material")
					  CabCount (ldata-get ent2 "Hole_Count")
					  Cabused  (ldata-get ent2 "Hole_Used") ;Hole_count

					  typestr  (ldata-get ent2 "Main_Type")
					  ) ;_ End_setq

				;;depth
				(setq lineobj  (vlax-ename->vla-object ent2)
					  dist1	   (vlax-curve-getDistAtPoint lineobj p0)
					  len	   (vla-get-length lineobj)
					  dist2	   (- len dist1)
					  depthstr (rtos (/ (+ (* d2 dist1) (* d1 dist2)) len) 2 2)
					  ) ;_ End_setq
				;;类别
				(if	(= gl_TableColorList nil)
					(setq gl_TableColorList (ReadColorConfig nil))
					) ;if

				;;材质
				;;(setq materialstr (nth (atoi Mater) gl_Material_List))
				(setq materialstr Mater)
				;;size DS
				(setq Dstr DS)
				;;属性
				(setq sxstr "")
				(if	(or (= typestr "L") (= typestr "D")) ;电力
					(setq sxstr Vol)
					) ;_ End_if
				(if	(= typestr "Q") ;燃气
					(setq sxstr Press)
					) ;_ End_if
				(setq typestr (nth 2 (assoc typestr gl_TableColorList)))

				;;(setq outstr (strcat "  typestr    "))

				(if	newblock
					(progn
						(setq x0   0.0
							  y0   (+ (* ndata 4.5) (/ 4.5 2.0))
							  txth 3
							  ) ;_ End_setq

						(setq txtobj (vla-addtext newblock typestr (vlax-3D-point 4 y0 0) txth))
						(vla-put-alignment txtobj acAlignmentMiddle)
						(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 4 y0 0))

						(setq txtobj (vla-addtext newblock materialstr (vlax-3D-point 12 y0 0) txth))
						(vla-put-alignment txtobj acAlignmentMiddle)
						(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 12 y0 0))

						(setq txtobj (vla-addtext newblock Dstr (vlax-3D-point 22 y0 0) txth))
						(vla-put-alignment txtobj acAlignmentMiddle)
						(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 22 y0 0))

						(setq txtobj (vla-addtext newblock CabCount (vlax-3D-point 32.5 y0 0) txth))
						(vla-put-alignment txtobj acAlignmentMiddle)
						(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 32.5 y0 0))

						(setq txtobj (vla-addtext newblock Cabused (vlax-3D-point 41.5 y0 0) txth))
						(vla-put-alignment txtobj acAlignmentMiddle)
						(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 41.5 y0 0))

						(setq txtobj (vla-addtext newblock sxstr (vlax-3D-point 51 y0 0) txth))
						(vla-put-alignment txtobj acAlignmentMiddle)
						(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 51 y0 0))

						(setq txtobj (vla-addtext newblock depthstr (vlax-3D-point 60 y0 0) txth))
						(vla-put-alignment txtobj acAlignmentMiddle)
						(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 60 y0 0))

						(vla-addline newblock
									 (vlax-3D-point 0 (- y0 2.25) 0)
									 (vlax-3D-point 64 (- y0 2.25) 0)
									 ) ;_ End_vla-addline

						) ;_ End_progn
					) ;_ End_progn
				(setq ndata (1+ ndata))

				) ;_ End_progn
			;;else
			;;(prompt "\n部分线段未包含属性，退出。\n部分线段未包含属性，退出。")
			) ;_ End_if

		) ;_ End_repeat

	(setvar "DIMZIN" dimz)
	;;写入表格头部
	(if	newblock
		(progn
			(setq x0   0.0
				  y0   (+ (* ndata 4.5) 2.25)
				  txth 3
				  ) ;_ End_setq

			(setq txtobj (vla-addtext newblock "类型" (vlax-3D-point 4 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 4 y0 0))

			(setq txtobj (vla-addtext newblock "材质" (vlax-3D-point 12 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 12 y0 0))

			(setq txtobj (vla-addtext newblock "管径" (vlax-3D-point 22 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 22 y0 0))

			(setq txtobj (vla-addtext newblock "总孔数" (vlax-3D-point 32.5 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 32.5 y0 0))

			(setq txtobj (vla-addtext newblock "已用孔" (vlax-3D-point 41.5 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 41.5 y0 0))

			(setq txtobj (vla-addtext newblock "属性" (vlax-3D-point 51 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 51 y0 0))

			(setq txtobj (vla-addtext newblock "深度" (vlax-3D-point 60 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 60 y0 0))

			(vla-addline newblock
						 (vlax-3D-point 0 (- y0 2.25) 0)
						 (vlax-3D-point 64 (- y0 2.25) 0)
						 ) ;_ End_vla-addline

			(setq y0 (+ y0 2.25))
			(vla-addline newblock
						 (vlax-3D-point 0 y0 0)
						 (vlax-3D-point 64 y0 0)
						 ) ;top
			(vla-addline newblock
						 (vlax-3D-point 0 0 0)
						 (vlax-3D-point 0 y0 0)
						 ) ;left
			(setq x0 (+ 8 x0))
			(vla-addline newblock
						 (vlax-3D-point x0 0 0)
						 (vlax-3D-point x0 y0 0)
						 ) ;grid1
			(setq x0 (+ x0 8))
			(vla-addline newblock
						 (vlax-3D-point x0 0 0)
						 (vlax-3D-point x0 y0 0)
						 ) ;grid2
			(setq x0 (+ x0 12))
			(vla-addline newblock
						 (vlax-3D-point x0 0 0)
						 (vlax-3D-point x0 y0 0)
						 ) ;grid3
			(setq x0 (+ x0 9))
			(vla-addline newblock
						 (vlax-3D-point x0 0 0)
						 (vlax-3D-point x0 y0 0)
						 ) ;grid4
			(setq x0 (+ x0 9))
			(vla-addline newblock
						 (vlax-3D-point x0 0 0)
						 (vlax-3D-point x0 y0 0)
						 ) ;grid5
			(setq x0 (+ x0 10))
			(vla-addline newblock
						 (vlax-3D-point x0 0 0)
						 (vlax-3D-point x0 y0 0)
						 ) ;grid6
			(setq x0 (+ x0 8))
			(vla-addline newblock
						 (vlax-3D-point x0 0 0)
						 (vlax-3D-point x0 y0 0)
						 ) ;grid7
			) ;_ End_progn
		) ;_ End_if

	;;插入块InsertBlock(InsertionPoint, Name, Xscale, Yscale, ZScale, Rotation [, Password])
	(setq scale (/ 1.0 gl_MAP_SCALE))
	(vla-insertblock Mspace
					 (vlax-3D-point textpos)
					 BlockName
					 scale
					 scale
					 scale
					 0
					 ) ;_ End_vla-insertblock
	(vla-delete (vlax-ename->vla-object ent1))
	(setq newarray (vlax-make-safearray
					   vlax-vbDouble
					   (cons 0 5)
					   ) ;_ End_vlax-make-safearray
		  ) ;_ End_setq
	(vlax-safearray-fill newarray
						 (list (car pt1) (cadr pt1) (car pt2) (cadr pt2) (car textpos) (cadr textpos))
						 ) ;_ End_vlax-safearray-fill
	(setq plineobj (vla-addlightweightpolyline Mspace
											   (vlax-make-variant newarray)
											   ) ;_ End_vla-addlightweightpolyline
		  ent1	   (vlax-vla-object->ename plineobj)
		  ) ;_ End_setq

	;;恢复当前图层
	(vla-put-activelayer Doc AcLayer)
	(vla-ZoomPrevious app)

	) ;_ End_defun



;*********************************************************************************************
;函数定义:C:Import_Coordinates()
;功能：从测量文件导入点,并分层
;参数：
;返回：
;创建时间：2014/12/26   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:Import_Coordinates( / attribslist borderflag bordergroup borderpoints datalist depth e 
	e1 e2 firstname firstpoint index i iborder maintype nborder path path0 pipepoints
		pname points pt s1 s2 tmplist vectors)
	(prompt "\n导入管线点坐标文件,文件格式为文本.\n文件数据格式为:点号,北坐标,东坐标,高程.
				\n自动划分图层(根据点号前缀划分图层).")
	; (setq  division (Fun_InPutString "1" "USERS1" "选择划分方式(1-管线类别,2-点号前缀):"))
	; (if (or (/= division "1") (/= division "2"))
		; (exit)
	; )
	(setq borderFlag (getstring "\n管线点的边界点以中心点号+边界标识符+顺序号表示，如DL101-1。\n请输入管线边界标志："))
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.csv")
		i 0
	)
	(setq gl_PointSpaceIndex (GetInsertRoot T))
	(if (setq path (getfiled "选择坐标文件" path0 "csv;txt;dat" 4))
		(if (setq datalist (ReadCoordiantesFile path))
			(progn
				;;1.点分为普通管线点和边界点，以'-'为标志。V5.7
				(setq borderPoints nil	;;边界点表
					pipePoints nil		;;管线点表
				)
				(if borderFlag
					(foreach e datalist
						(if (vl-string-position (ascii borderFlag) (car e))
							(setq borderPoints (cons e borderPoints))
							(setq pipePoints (cons e pipePoints))
						)
					)
					(setq borderPoints nil
						pipePoints datalist
					)
				)
				
				;;2.加入边界点。至少3个点组成闭合多段线；
				(if (and borderPoints borderFlag (> (setq nBorder (length borderPoints)) 2))
					(progn
						;;2.1按照点号从小到大顺序排列
						(setq borderPoints (vl-sort borderPoints 
											(function (lambda (s1 s2) (< (car s1) (car s2)))))
						)
						;;2.2按照"-"前面的点号分组
						(setq iBorder 0)
						(while borderPoints
							(setq borderGroup nil		;;1个边界
								firstPoint (car borderPoints)
								firstName (car firstPoint)
								borderGroup (cons firstPoint borderGroup)
								firstName (substr firstName 1 (vl-string-position (ascii borderFlag) firstName))
								borderPoints (cdr borderPoints)
							)
							(while (and borderPoints (vl-string-search firstName (car (car borderPoints))))
								(setq borderGroup (cons (car borderPoints) borderGroup)
									borderPoints (cdr borderPoints)
								)
							)
							;;绘制边界
							(if (> (length bordergroup) 2)
								(progn
									(setq vectors nil
										 maintype (GetTypeFromPointName firstName)
										 i 0
									)
									(repeat (length borderGroup)
										(setq vectors (append vectors (cdr(nth i borderGroup)))
											i (1+ i)
										)
									)
									(AddBorder vectors maintype depth firstName  T)
									(setq iBorder (1+ iBorder))
								)
							)
						)
						(prompt (strcat "\n导入了" (rtos nBorder 2 0) "个边界点，" (rtos  iBorder 2 0) "个边界多段线。"))
					)
				)
				;;3.加入普通管线点，首先找到最边界的点，以免多次更新索引。
				;;3.1排序，查找范围点
				(setq borderGroup nil
					tmpList (vl-sort pipePoints (function (lambda (e1 e2) (< (cadr e1) (cadr e2)))))	;;x
					borderGroup (cons (car tmpList) borderGroup)
					borderGroup (cons (last tmpList) borderGroup)
					tmpList (vl-sort pipePoints (function (lambda (e1 e2) (< (caddr e1) (caddr e2)))))	;;y
					borderGroup (cons (car tmpList) borderGroup)
					borderGroup (cons (last tmpList) borderGroup)
					borderGroup (ListRemoveSameElement borderGroup)
				)
				(foreach e borderGroup
					(setq pipePoints (vl-remove e pipePoints))
				)
				;;把边界点放在前面，首先加入
				(setq pipePoints (append borderGroup pipePoints))
				(foreach e pipePoints
					(setq pname (car e)
						pt (cdr e)
					)
					;;是否存在已知点
					(if (setq points (SearchPoint gl_PointSpaceIndex (car pt) (cadr pt)))
						;;已有相同位置点
						(prompt (strcat "\n点号" pname "与图中点位置重合，未导入！"))
						(if (not(HasSameMapName pname))
							(progn
								(setq 	maintype (GetTypeFromPointName pname)
									attribslist (list (cons "Exp_No" pname) 
													  (cons "Map_No" pname) 
													  (cons "Main_Type" maintype)
													  (cons "Feature" "起讫点"))
								)
								;;判断点是否超出索引范围
								;;！！！！代码耦合
								(if (IsOutRange gl_PointSpaceIndex (car pt) (cadr pt))
									(setq gl_PointSpaceIndex nil)	;;AddNewPoint 不更新索引
								)
								(AddNewPoint pt pname maintype "起讫点" "" attribslist)
								(setq gl_MapNameList (cons pname gl_MapNameList))
								;;更新索引
								(if (not gl_PointSpaceIndex )(setq gl_PointSpaceIndex (GetInsertRoot T)))
								(setq i (1+ i))
							)
							(prompt (strcat "\n点号" pname "重名，未导入！"))
						)
					)
				)
				(prompt (strcat "\n文件中共有" (rtos (length datalist) 2 0) "个点，导入" (rtos i 2 0) "个管线点."))
			)
		)
	)
	(princ)
)

;*********************************************************************************************
;函数定义:ReadCoordiantesFile( path)
;功能：从测量文件导读入点号 x y z,文件数据格式为:点号,北坐标,东坐标,高程,.....
;参数：路径
;返回：list ((no x y z)...)
;创建时间：2014/12/26   23:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun ReadCoordiantesFile (path / filep icount datalist str pos x y z)
	(if (not (setq filep (open path "r")))
		(progn
			 (prompt (strcat "\n打开文件" path "失败。退出！"))
			 (exit)
		) ;_ End_progn
	) ;if
	(setq icount 0
		datalist nil)
	(while (setq str (read-line filep))
		(if (setq pos (vl-string-search "," str))  
			(progn
				(setq pname(substr str 1 pos)
					  str (substr str (+ 2 pos))
					  pos (vl-string-search "," str)
					  y  (atof (substr str 1 pos))
					  str  (substr str (+ 2 pos))
					  pos (vl-string-search "," str)
					  x  (atof (substr str 1 pos))
					  str  (substr str (+ 2 pos))
					  pos (vl-string-search "," str)
					  z  (atof (substr str 1 pos))
				)
				;;非标题行
				(if (/= 0.0 y)
					(setq datalist (cons (list pname x y z) datalist))
				)
			)
		)	  
	)
	datalist
)

;*********************************************************************************************
;函数定义:C:CorrectionDraft( )
;功能：校正草图坐标,并生成点对照表,统计导入点,未导入点,缺少坐标点
;前置条件:所有点都为点块,且为管线点,包含点属性.
;参数：路径
;返回：list ((no x y z)...)
;创建时间：2014/12/26   23:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:CorrectionDraft(/ allpointset data datalist e eobj errorlist filep filepath i path path0 pname pos newpos oldpos mapname )
	(prompt "\n导入测量坐标成果,校正草图坐标,并生成点对照表,统计导入点,未导入点,缺少坐标点.
			\n测量成果为文本文件,数据格式为:点号,北坐标,东坐标,高程.
			\n导入管线点坐标之前，请用(错误处理->检查连接错误)建立连接关系.")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.csv")
	)
	(if (setq path (getfiled "选择测量成果文件" path0 "csv;txt;dat" 4))
		(if (setq datalist (ReadCoordiantesFile path))
			(progn
				;;1.移动管线点
				(setq AllPointSet (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName))))
					i 0 j 0
					errorlist nil ;;无测量坐标列表
				)
				(repeat (sslength AllPointSet)
					(setq e (ssname AllPointSet i)
						eobj (vlax-ename->vla-object e)
						pname (ldata-get e "Exp_No")	;;与物探点号一致，
                                              	mapname (ldata-get e "Map_No")
					)
					(cond
						;;Exp_No
						((setq data (assoc pname datalist))
						(progn
							;(setq datalist (vl-remove data datalist))
							(setq newpos (cdr data)
								oldpos (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint eobj))))
							(if (not (equal newpos oldpos))
								(progn
									(MoveOnePoint2 oldpos newpos e)
									(setq j (1+ j))
								)	
							)
							; (vla-move eobj (vla-get-InsertionPoint eobj) (vlax-3D-point (cdr data)))
							; (setq datalist (vl-remove data datalist)
								  ; j (1+ j))
						))
						;;Map_No
						((setq data (assoc mapname datalist))
						(progn
							;(setq datalist (vl-remove data datalist))
							(setq newpos (cdr data)
								oldpos (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint eobj))))
							(if (not (equal newpos oldpos))
								(progn
									(MoveOnePoint2 oldpos newpos e)
									(setq j (1+ j))
								)	
							)
						))
						;;No Name
						(T (setq errorlist (cons (cons pname (cdr (assoc 10 (entget e)))) errorlist)))
					)
					(setq i (1+ i))
				)
				(prompt (strcat "\n校正了" (rtos j 2 0) "个管线点."
								"\n\t有" (rtos (length errorlist) 2 0) "个管线点无对应的测量点."
								"\n\t有" (rtos (length datalist) 2 0) "个测量点无对应的管线点."))
				;;output to a file
				(setq filePath   "c:\\line_info_CorrectionError.csv")
				;;打开点文件
				 (vl-file-delete filePath)
				 (if (not (setq filep (open filePath "w")))
					 (progn
						 (Prompt (strcat "\n打开文件" filePath "失败。退出！"))
						 (exit)
					 ) ;_ End_progn
				 ) ;if
				 (write-line (strcat "有" (rtos (length errorlist) 2 0) "个管线点无对应测量点.") filep)
				(foreach e errorlist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),管线点无对应测量点" )
								filep)
				)
				(write-line (strcat "有" (rtos (length datalist) 2 0) "个测量点无对应的管线点.") filep)
				(foreach e datalist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),测量点无对应的管线点" )
								filep)
				)
				(close filep)
				(GetLineRoot T)
				(GetInsertRoot T)
				(startapp "EXPLORER.EXE" filePath)
			)
		)
	)
	(princ)
)

;*********************************************************************************************
;函数定义:C:CorrectionXYZ( )
;功能：根据位置判定图中点和文件中的点是否是同一个点，若平面距离小于指定值，则视为同一点，并移动图中点到文件中点的位置
;前置条件:导入管线点坐标之前，请用(错误处理->检查连接错误)建立连接关系.
;参数：
;返回：
;创建时间：2015/4/20   11:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:CorrectionXYZ(/  allpointset data datalist dist0 e eobj errorlist filep filepath 
	herror i ipoint j mindistance minh npoint p0 p01 p1 path path0 pname pos )
	(prompt "\n根据位置判定图中点和文件中的点是否是同一个点，若平面距离小于指定值，则视为同一点，并移动图中点到文件中点的位置。
			\n并生成点对照表,统计导入点,未导入点,缺少坐标点.
			\n测量成果为文本文件,数据格式为:点号,北坐标,东坐标,高程.
			\n导入管线点坐标之前，请用(错误处理->检查连接错误)建立连接关系.")
	(setq MINDISTANCE (Fun_InPutValue 0.03 "USERR2" "\n请输入平面允许误差（m）：" 6))
	(if (= nil MINDISTANCE) (setq MINDISTANCE 0.03))
	
	(setq MINH (Fun_InPutValue 0.05 "USERR3" "\n请输入高程允许误差（m）：" 6))
	(if (= nil MINH) (setq MINH 0.05))
	
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.csv")
	)
	(if (setq path (getfiled "选择测量成果文件" path0 "csv;txt;dat" 4))
		(if (setq datalist (ReadCoordiantesFile path))
			(progn
				
				(if gl_DEBUG
					(setq gl_Time (getvar "TDUSRTIMER"))
				)
				
				;;1.移动管线点
				(setq AllPointSet (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName))))
					i 0 j 0
					errorlist nil ;;无测量坐标列表
				
					npoint (length datalist)
				)
				(repeat (sslength AllPointSet)
					(setq e (ssname AllPointSet i)
						p0 (cdr (assoc 10 (entget e)));;3d point
						p01 (list (car p0) (cadr p0))	;;x y position
						eobj (vlax-ename->vla-object e)
						pname (ldata-get e "Map_No")
                                              dist0 (+ MINDISTANCE 1)	
					)
					;;查找相近的点
					(setq ipoint 0)
					(while (and (> dist0 MINDISTANCE) (< ipoint npoint))
						(setq data (nth ipoint datalist)
							p1 (list (cadr data) (caddr data))
							dist0 (distance p01 p1)
							ipoint (1+ ipoint)
						)
					)
					;;找到了满足误差的点
					(if (<= dist0 MINDISTANCE)
						(progn
							;;(setq datalist (vl-remove data datalist))
							(MoveOnePoint2 p0 (cdr data) e)
							;;高程误差过大，提出警告。
							(setq herror (abs (- (last p0) (last (cdr data)))))
							(if (< MINH herror)
								(prompt (strcat "\n" pname "两点高程误差" (rtos herror 2 4) "m" "，超过高程校正误差。"))
							)
							(setq j (1+ j))
						)
						;;esle 
						(progn
							(setq errorlist (cons (cons pname p0 ) errorlist))
							;;(prompt (strcat "\n" pname "两点高程误差" (rtos herror 2 4) "m" "，超过高程校正误差。"))
						)
					)
					(setq i (1+ i))
				)
				(prompt (strcat "\n校正了" (rtos j 2 0) "个管线点."
								"\n\t有" (rtos (length errorlist) 2 0) "个管线点无对应的测量点."
								"\n\t有" (rtos (length datalist) 2 0) "个测量点无对应的管线点."))
				;;output to a file
				(setq filePath   "c:\\line_info_Correctionxyz.csv")
				;;打开点文件
				 (vl-file-delete filePath)
				 (if (not (setq filep (open filePath "w")))
					 (progn
						 (Prompt (strcat "\n打开文件" filePath "失败。退出！"))
						 (exit)
					 ) ;_ End_progn
				 ) ;if
				 (write-line (strcat "有" (rtos (length errorlist) 2 0) "个管线点无对应测量点.") filep)
				(foreach e errorlist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),管线点无对应测量点" )
								filep)
				)
				(write-line (strcat "有" (rtos (length datalist) 2 0) "个测量点无对应的管线点.") filep)
				(foreach e datalist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),测量点无对应的管线点" )
								filep)
				)
				(close filep)
				;;更新索引
				(GetLineRoot T)
				(GetInsertRoot T)
				(startapp "EXPLORER.EXE" filePath)
			)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\n校正坐标耗时")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "秒.")
		)
	)
	
	(princ)
)
;;清处所有块和线段的属性
(defun DeleteAllLdata( / AllPointSet AllLineSet i ent)
	(setq AllPointSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
		AllLineSet (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName))))
		i 0
		)
	(repeat (sslength AllLineSet)
		(setq ent (ssname AllLineSet i ))
		(if (setq listdata (vlax-ldata-list ent))
			(foreach e listdata
				(vlax-ldata-delete ent (car e))
			)
		)
		(setq i (1+ i))
	)	
	(setq i 0)
	(repeat (sslength AllPointSet)
		(setq ent (ssname AllPointSet i ))
		(if (setq listdata (vlax-ldata-list ent))
			(foreach e listdata
				(vlax-ldata-delete ent (car e))
			)
		)
		(setq i (1+ i))
	)	
)

;*********************************************************************************************
;函数定义:C:SetLinesProperty( )
;功能：修改选择集中的对象属性：
;前置条件:
;参数：
;返回：
;创建时间：2015/4/10   23:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:SetLinesProperty( / ent entset etype  i n newname npro pronames property str)
	;;定义可编辑的项目属性
	(setq str (list (cons 1 "项目名称") (cons 2 "单位名称") (cons 3 "子项目名称") (cons 4 "调查时间（格式为 2014/12/01）") (cons 5 "路名")))
	(setq pronames (list "Project" "Unit" "SProject" "Sur_Date" "Road_Name"))
	(print str)

	(if (setq npro	(getint "\n选择要修改的属性，输入属性前的代码："))
		(if (<= npro (length str))
			(if (setq  newname (getstring (strcat "\n请输入新的" (cdr (nth (- npro 1) str)) ":")))
				(progn
					(setq  i 0 n 0)
					(setq entset (ssget ))
					(if (and newname entset)
						(progn
							(repeat (sslength entset)
								(setq ent (ssname entset i)
									etype (cdr (assoc 0 (entget ent)))
								)
								(if (vlax-ldata-get ent gl_AppName)
									(if  (ldata-get ent (nth (- npro 1) pronames))
										(progn
											(ldata-put ent (nth (- npro 1) pronames) newname)
											(setq n (1+ n))
										)
									) 
								)
								(setq i (1+ i ))
							)
							(print (strcat "修改了" (rtos n 2 0) "个管线对象的" (cdr (nth (- npro 1) str)) "为" newname "."))
						)
					)
				)
			)
		)
	) 
	(princ)
)

;;
;;在CAD中删除对象
(defun C:DelEntitys(  / ent entlist ents i j p10 p11 tname)
	(princ "\n 选择删除的对象：")
	(setq i 0 j 0)
	(if (setq ents (ssget ))
		(repeat (sslength ents)
			(setq ent (ssname ents i)
				i (1+ i)
			)
			(if (vlax-ldata-get ent gl_AppName)
				(progn
					(setq entlist (entget ent)
						p10 (cdr (assoc 10 entlist))
						tname (cdr (assoc 0 entlist))
					)
					;;删除索引;;删除点名
					(cond 
						((= tname "INSERT") 
							(progn
								(setq gl_PointSpaceIndex (DelEntityIndex (GetInsertRoot nil) ent (car p10) (cadr p10)))
								(if gl_MapNameList (setq gl_MapNameList (vl-remove (ldata-get ent "Map_No") gl_MapNameList)))
							)
						)	
						((= tname "LINE") 
							(progn 
								(setq p11 (cdr (assoc 11 entlist)))
								(setq gl_LineSpaceIndex (DelEntityIndex (GetLineRoot nil) ent (car p10) (cadr p10)))
								(setq gl_LineSpaceIndex (DelEntityIndex (GetLineRoot nil) ent (car p11) (cadr p11)))
							)
						)
					)
					
					(setq j (1+ j))
				)
			)
			(entdel ent)
		)
	)
	(princ (strcat "\n选择了" (rtos i 2 0) "个图元，删除了" (rtos j 2 0) "个管线对象。"))
  	(princ)
)

;;属性刷
(defun C:PropertyCopy( / data datalist ent1 ent2 entp1 entp2 fstr  ldata2 maintype1 
		maintype2 mname name1 name2 p10 p11 sstr typename1 typename2)
	(if (setq ent1 (car (entsel "\n选择第一个管线对象:")))
		(progn
			(setq typename1 (cdr (assoc 0 (entget ent1))))
			(if(vlax-ldata-get ent1 gl_AppName)
				(progn
					(setq maintype1 (ldata-get ent1 "Main_Type"))
					(while (setq ent2 (car (entsel "\n选择下一个管线对象:")))
						(setq typename2 (cdr (assoc 0 (entget ent2)))
							maintype2 (ldata-get ent2 "Main_Type")
						)
						(cond 
							((and (= typename1 "INSERT")(= maintype1 maintype2) (= typename1 typename2) (vlax-ldata-get ent2 gl_AppName))
								(progn
									(if (setq data (ldata-get ent1 "S_Code")) (ldata-put ent2 "S_Code" data))
									(if (setq data (ldata-get ent1 "Feature")) (ldata-put ent2 "Feature" data))
									(if (setq data (ldata-get ent1 "Subsid")) (ldata-put ent2 "Subsid" data))
									(if (setq data (ldata-get ent1 "Sur_Unit")) (ldata-put ent2 "Sur_Unit" data))
									(if (setq data (ldata-get ent1 "Build_Unit")) (ldata-put ent2 "Build_Unit" data))
									(if (setq data (ldata-get ent1 "SProject")) (ldata-put ent2 "SProject" data))
									(if (setq data (ldata-get ent1 "Project")) (ldata-put ent2 "Project" data))
									(if (setq data (ldata-get ent1 "Point_Size")) (ldata-put ent2 "Point_Size" data))
									(if (setq data (ldata-get ent1 "Location")) (ldata-put ent2 "Location" data))
									(if (setq data (ldata-get ent1 "Version")) (ldata-put ent2 "Version" data))
									(if (setq data (ldata-get ent1 "JG_Material")) (ldata-put ent2 "JG_Material" data))
									(if (setq data (ldata-get ent1 "JS_Size")) (ldata-put ent2 "JS_Size" data))
									(if (setq data (ldata-get ent1 "JS_Type")) (ldata-put ent2 "JS_Type" data))
									(if (setq data (ldata-get ent1 "JG_Shape")) (ldata-put ent2 "JG_Shape" data))
									(ldata-delete ent2 "Edge")	;;动态生成
									(setq ldata2 (vlax-ldata-get ent2 gl_AppName)
										fstr (ldata-get ent2 "Feature")
										sstr (ldata-get ent2 "Subsid")
										mname (ldata-get ent2 "Map_No")
										p10 (cdr (assoc 10 (entget ent2))))
									(AddNewPoint p10 mname maintype2 fstr sstr ldata2)
									(DelEntity ent2)
									(prompt "\n已修改管线点属性。")
								)
							)	
							((and (= typename1 "LINE")(or (= maintype1 maintype2) (= nil maintype2)) (= typename1 typename2))	;;ent2 可以是未定义属性的直线
								(progn
									;;1.直线无属性
									(if (= nil maintype2)
										(progn
											;;1.找到StartPoint and End Point
											(setq p10 (cdr (assoc 10 (entget ent2)))
												p11 (cdr (assoc 11 (entget ent2)))
												entp1 (GetConnectEntity (car p10) (cadr p10) "INSERT")
												entp2 (GetConnectEntity (car p11) (cadr p11) "INSERT")
												datalist (vlax-ldata-get ent1 gl_AppName)
											)
											(if (and entp1 entp2)
												(if (and (setq entp1 (car entp1)) (setq entp2 (car entp2)))
													(if (and (setq name1 (ldata-get entp1 "Map_No")) 
															(setq name2 (ldata-get entp2 "Map_No")))
														(progn
															(DelEntity ent2)
															(setq newline (AddNewLine p10 p11 maintype1 datalist))
															(ldata-delete newline "Edge")
															(ldata-delete newline "Label")
															(ldata-put newline "Start_Point" name1)
															(ldata-put newline "End_Point" name2)
															(ldata-put newline "Start_Deep" -1)
															(ldata-put newline "End_Deep" -1)
														)
													)
												)
												(prompt "\n错误：选择的直线端点未连接到管线点。")
											)
										);;else 直线有属性
										(progn
											(if (setq data (ldata-get ent1 "Material")) (ldata-put ent2 "Material" data))
											(if (setq data (ldata-get ent1 "D_S")) (ldata-put ent2 "D_S" data))
											(if (setq data (ldata-get ent1 "D_Type")) (ldata-put ent2 "D_Type" data))
											(if (setq data (ldata-get ent1 "P_Material")) (ldata-put ent2 "P_Material"  data))
											(if (setq data (ldata-get ent1 "P_D_S")) (ldata-put ent2 "P_D_S" data))
											(if (setq data (ldata-get ent1 "Mdate")) (ldata-put ent2 "Mdate" data))
											(if (setq data (ldata-get ent1 "Build_Unit")) (ldata-put ent2 "Build_Unit" data))
											(if (setq data (ldata-get ent1 "Line_Style")) (ldata-put ent2 "Line_Style" data))
											(if (setq data (ldata-get ent1 "Cab_Count")) (ldata-put ent2 "Cab_Count" data))
											(if (setq data (ldata-get ent1 "Voltage")) (ldata-put ent2 "Voltage" data))
											(if (setq data (ldata-get ent1 "Pressure")) (ldata-put ent2 "Pressure" data))
											(if (setq data (ldata-get ent1 "Hole_Count")) (ldata-put ent2 "Hole_Count" data))
											(if (setq data (ldata-get ent1 "Hole_Used")) (ldata-put ent2 "Hole_Used" data))
											(if (setq data (ldata-get ent1 "Road_Name")) (ldata-put ent2 "Road_Name" data))
											(if (setq data (ldata-get ent1 "Sur_Unit")) (ldata-put ent2 "Sur_Unit" data))
											(if (setq data (ldata-get ent1 "SProject")) (ldata-put ent2 "SProject" data))
											(if (setq data (ldata-get ent1 "Project")) (ldata-put ent2 "Project" data))
											;;(if (setq data (ldata-get ent1 "Flowdirect")) (ldata-put ent2 "Flowdirect" data))
											(if (setq data (ldata-get ent1 "Location")) (ldata-put ent2 "Location" data))
											(if (setq data (ldata-get ent1 "Sur_Date")) (ldata-put ent2 "Sur_Date" data))
											(if (setq data (ldata-get ent1 "Property")) (ldata-put ent2 "Property" data))
											(if (setq data (ldata-get ent1 "BuryWay")) (ldata-put ent2 "BuryWay" data))
											(if (setq data (ldata-get ent1 "Line_Memo")) (ldata-put ent2 "Block_Size1" data))
											(if (setq data (ldata-get ent1 "Block_Size1")) (ldata-put ent2 "Block_Size1" data))
											(if (setq data (ldata-get ent1 "Block_Size2")) (ldata-put ent2 "Block_Size2" data))
											(ldata-delete ent2 "Edge")	;;动态生成
											(UpdateLabel ent2 nil nil)
										)
									)
									
									(prompt "\n已修改管线段属性。")
								)
							)
							(T (prompt "\n错误：对象类型不一致！"))
						)
					)
				)
				;;
				(prompt "\n错误：选择的对象不是有效的管线点或管线段！")
			)
		)
	)
	(princ)
)
;;;管线探测CAD-数据库系统2014-07-08
;;;

 ;(LineInfo_GetSupportPath)

(vl-load-com)

;;; 绘制点和线
(defun C:LineInfo_DrawAll  (/ connectstring pos scale tablenames tables tb)
	(if (= nil gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	(setq typelist nil)
	(foreach e gl_TableColorList
		(setq typelist (cons (strcat (car e) "-" (caddr e)) typelist))
	)
	(setq typelist (reverse typelist))
	
	(print typelist)
	(if (setq types (Fun_InPutString "ALL"
									  "USERS1"
									  "\n输入要绘制的管线类型（多个类型用逗号','分隔，全部选择输入'All'）：")
		  ;scale		 (Fun_InPutValue 1 "USERR2" "\n输入图形比例尺(1:1000输入1；1:500输入2；1:2000输入0.5)：" 2)
		  )
		(progn

			;;先绘制线，再绘制点，否则线被图块压住
			;;根据类型名来绘制
			(setq typelist nil
				types (strcase types))
			(if	(/= "ALL" types)
				(progn
					(while (setq pos (vl-string-search "," types))
						(setq tb		 (substr types 1 pos))
						(if (assoc tb gl_TableColorList)
							(setq typelist	 (cons tb typelist))
						)
						(setq  types (substr types (+ 2 pos)))
						) ;while
					(if (assoc types gl_TableColorList)
						(setq typelist	 (cons types typelist))
					)
				) ;progn
				(foreach e gl_TableColorList
					(setq typelist (cons (car e) typelist))
				)
			) ;if

			(if typelist
				(DrawTables typelist)
			)
		)
	)
	(princ)
) ;defun

 
;;;导入正元数据库，仅包含点线，不包含面	
(defun C:ImportZYDB ( / result strpath path0 )
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "\\database.mdb")
	)
    (if (setq strPath (getfiled "选择包含点线数据的Access数据库文件" path0 "MDB" 8))
        (progn
            (prompt (strcat "\n\n数据库路径：" strPath "\n"))
			(if (setq result (getstring "\n该数据库是正元管线数据库吗?(Y/N)"))
				(if (= "Y" (strcase result))
					(cond ((vl-catch-all-error-p
								(setq XL (vl-catch-all-apply
									'DrawTablesZY
									(list strPath))
								)
							)
							(vl-exit-with-error
								(strcat "\n出错: " (vl-catch-all-error-message XL))
							)
						)
						(T (princ "\n成功导入正元数据库。"))
					)
					;;(DrawTablesZY strPath)
				)
			)
        );progn
        (prompt (strcat "\n\n未打开任何数据库。" ))
    );if
    
    (princ)
)
 ;*********************************************************************************************
 ;函数定义:DrawTablesZY(mdbpath)
 ;功能：绘制线表和点表。线表格式固定，为杭州管线数据库格式
 ;参数：mdbpath MDB文件路径
 ;返回：无
 ;创建时间：2015/4/24 14:34
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun DrawTablesZY  (mdbpath /      attrib_objs cols connectionobject connstr deep1 deep2 e Exp_No 
		featureslist featurestr  index linelist lines linetablename maintype Map_No newblock_ref
		newl newp numl nump onerecord outlist p10 p11 pend pendname pointlist points pointtablename 
		pos pstart pstart_h pstartname pstartx pstarty rotation subsidslist subsidstr table tblist typelist)

	(LineInfo_GetSupportPath)
	;;连接到数据库，创建连接对象
	(setq connstr (LINE_INFO_ADOLib_ConnectString2 mdbpath)
		ConnectionObject nil
	)
	(if (not (setq ConnectionObject (ADOLISP_ConnectToDB connstr "admin" "")))
	    (progn
	      (prompt "\nConnection failed!")
	      (ADOLISP_ErrorPrinter)
		  (exit)
	    )
	)
	
	;;读取规则，数据表前缀必须和主类型一致，否者忽视该表
	(if (= nil gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	(setq typelist nil)
	(foreach e gl_TableColorList
		(setq typelist (cons (car e) typelist))
	)
	;;get all tables
	(setq tblist (car (ADOLISP_GetTablesAndViews ConnectionObject)))
	(while (> (length tblist) 0)
		(setq table (car tblist)
			maintype (substr table 1 1))
		(if (vl-position maintype typelist)
			(progn
				(setq linetablename	 (strcat maintype "_LINE")
					pointtablename (strcat maintype "_POINT")
				)
				(setq tblist (vl-remove maintype tblist)
					tblist (vl-remove linetablename tblist)
					tblist (vl-remove pointtablename tblist)
					tblist(vl-remove (strcat maintype "_REGION") tblist)
				)     
				
				(setq featureslist (getfeaturelist-fromtype maintype)
					subsidslist (getsubsidlist-fromtype maintype))
				;;读取数据
				(setq points (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " pointtablename 
								" WHERE 删除标记 is null AND X坐标 is not NULL AND Y坐标 is not NULL AND 地面高程 is not NULL"))) ;点数据
				(setq nump (length points)
					index	1
					pointlist nil
				)
				;;点表
				(setq cols (car points))	;;fist row has all coloumns
				(if (> nump 1)
					(repeat	(- nump 1)
						(setq oneRecord	 (nth index points)
							index (1+ index)
							pstartx  (nth (vl-position "Y坐标" cols) oneRecord)
							pstarty  (nth (vl-position "X坐标" cols) oneRecord)
							pstart_h (nth (vl-position "地面高程" cols) oneRecord)
							outlist nil
							featurestr (nth (vl-position "特征" cols) oneRecord)
							subsidstr (nth (vl-position "附属物" cols) oneRecord)
							Map_No (nth (vl-position "图上点号" cols) oneRecord)
							Exp_No (nth (vl-position "物探点号" cols) oneRecord)
							  
						)
						;;特征和附属物
						(if (not (vl-position featurestr featureslist)) (setq featurestr "直线点"))
						(if (not (vl-position subsidstr subsidslist)) (setq subsidstr ""))
						;;点号为空的问题
						(if (not Map_No) (setq Map_No Exp_No))								
						(setq outlist (cons (cons "Exp_No" Exp_No) outlist)
							  outlist (cons (cons "Map_No"  Map_No) outlist)
							  outlist (cons (cons "Feature"  featurestr) outlist)
							  outlist (cons (cons "Subsid"  subsidstr) outlist)
							  outlist (cons (cons "Main_Type" maintype) outlist)
							  
							  outlist (cons (cons "Unit" (nth (vl-position "探测单位" cols) oneRecord)) outlist)
							  outlist (cons (cons "SProject" (nth (vl-position "单位工程名称" cols) oneRecord)) outlist)
							  outlist (cons (cons "Project" (nth (vl-position "工程项目名称" cols) oneRecord)) outlist)
							  
						)
						
						(if (setq newp (AddNewPoint (list pstartx pstarty pstart_h) Exp_No maintype featurestr subsidstr outlist))
							(progn
								(setq pointlist (cons (list Exp_No newp) pointlist))
								(ldata-put newp "Status_DB" 0)
								;;设置属性文字的位置,数据库自定义字段；
								(setq newblock_ref (vlax-ename->vla-object newp))
								(if (setq pos (vl-position "角度" cols))
									(if (setq rotation (nth pos oneRecord))
										(progn
											(vla-put-rotation newblock_ref rotation)
											(if  (= :vlax-true (vla-get-HasAttributes newblock_ref))
												(progn
													;;text insertpoint
													(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
													(foreach e attrib_objs
														(if (= "点号" (vla-get-tagstring e))
															(vla-put-rotation e 0.0)
														)
													)
												);progn
											);if
										)
									)
								)
							)
						)

					)
				)
				(if pointlist
					(prompt (strcat "\n从数据表"pointtablename "中读取了" (rtos (length pointlist) 2 0) "个管线点."
									"数据表中共有" (rtos (- (length points) 1) 2 0) "个管线段."))
				)
				;;读取线表
				(setq lines	 (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " linetablename 
									" WHERE 删除标记 is null AND 起点物探点号 is not NULL AND 连接方向 is not NULL "))) ;线数据
				(setq numl	(length lines)
					index	1
					linelist nil
					cols (car lines)
				)
				(if (> numl 1)
					(repeat	(- numl 1)
						(setq  oneRecord	 (nth index lines)
							index (1+ index)
							outlist nil
							pstartname (nth (vl-position "起点物探点号" cols) oneRecord)
							pendname (nth (vl-position "连接方向" cols) oneRecord)
							outlist (cons (cons "Start_Point"  pstartname) outlist)
							outlist (cons (cons "End_Point"  pendname) outlist)
							deep1 (nth (vl-position "起点埋深" cols) oneRecord)
							deep2 (nth (vl-position "终点埋深" cols) oneRecord)
						)
						(if (setq pstart (assoc pstartname pointlist)) 
							(if (setq pend (assoc pendname pointlist))
								(progn
									(if (not deep1) 
										(progn 
											(princ (strcat "\n导入数据错误：管线段" pstartname "-" pendname "起点深度为空！已自动设为-1."))
											(setq deep1 -1)
										)
									)
									(if (not deep2) 
										(progn 
											(princ (strcat "\n导入数据错误：管线段" pstartname "-" pendname "终点深度为空！已自动设为-1."))
											(setq deep2 -1)
										)
									)
									(setq  outlist (cons (cons "Material" (nth (vl-position "材质" cols) oneRecord)) outlist)
										  outlist (cons (cons "D_S" (nth (vl-position "管径" cols) oneRecord)) outlist)
										  outlist (cons (cons "Main_Type" maintype) outlist)
										  outlist (cons (cons "Start_Deep" deep1) outlist)
										  outlist (cons (cons "End_Deep" deep2) outlist)
										  
										  outlist (cons (cons "Voltage" (nth (vl-position "电压值" cols) oneRecord)) outlist)
										  outlist (cons (cons "Pressure" (nth (vl-position "压力值" cols) oneRecord)) outlist)
										  outlist (cons (cons "Cab_Count" (nth (vl-position "孔数" cols) oneRecord)) outlist)
										  outlist (cons (cons "Hole_Count" (nth (vl-position "孔数" cols) oneRecord)) outlist)
										  outlist (cons (cons "Flowdirect" (nth (vl-position "流向" cols) oneRecord)) outlist)
										  
										  outlist (cons (cons "Sur_Date" (nth (vl-position "探测时间" cols) oneRecord)) outlist)
										  outlist (cons (cons "Unit" (nth (vl-position "探测单位" cols) oneRecord)) outlist)
										  outlist (cons (cons "SProject" (nth (vl-position "单位工程名称" cols) oneRecord)) outlist)
										  outlist (cons (cons "Project" (nth (vl-position "工程项目名称" cols) oneRecord)) outlist)
										  outlist (cons (cons "Mdate" (nth (vl-position "建设年代" cols) oneRecord)) outlist)
									)
									
									;;有些字段非标注杭州数据库，自定义字段
									
									(if (setq pos  (vl-position "权属单位代码" cols))
										(setq outlist (cons (cons "Property" (nth pos oneRecord)) outlist))
									)
									(if (setq pos  (vl-position "权属单位代码" cols))
										(setq outlist (cons (cons "Road_Name" (nth pos oneRecord)) outlist))
									)
									(if (setq pos  (vl-position "BuryWay" cols))
										(setq outlist (cons (cons "埋设类型" (nth pos oneRecord)) outlist))
									)
									(setq pstart (cadr pstart)
										p10 (cdr (assoc 10 (entget pstart)))
										p10 (list (car p10) (cadr p10) (- (caddr p10) deep1))
										pend (cadr pend)
										p11 (cdr (assoc 10 (entget pend)))
										p11 (list (car p11) (cadr p11) (- (caddr p11) deep2))
									)
									(ldata-put pstart "Depth" deep1)
									(ldata-put pend "Depth" deep2)
									
									(if (setq newl (AddNewLine p10 p11 maintype outlist))
										(progn
											(ldata-put newl "Status_DB" 0)
											(AddLineTextLabel newl  deep1  deep2)
											(AddLineFlowdirect newl)
											(setq linelist (cons newl linelist))
										)
									)	
								)
							)	
						)
						;; 点号只在线表中
						(if (or (= nil pstart ) (= nil pend))
							(prompt (strcat "\n错误！线表中的管线段" pstartname "-" pendname ",起点或终点不在点表内."))
						)

					)
				)

				(if linelist
					(prompt (strcat "\n从数据表" linetablename "中读取了" (rtos (length linelist) 2 0) "个管线段."
									"数据表中共有" (rtos (- (length lines) 1) 2 0) "个管线段."))
				)
			)
			;;
			(setq tblist (vl-remove table tblist))
		)
	)
	(ADOLISP_DisconnectFromDB ConnectionObject)
	(setq ConnectionObject nil)

	(princ)	
)		


;*********************************************************************************************
 ;函数定义:GetSelectLines(linetable,pointtable)
 ;功能：从点表和线表中获取线的数据,并包含点坐标,以用来画线;目的是为解决SELECT* 导致的 效率问题
 ;参数：线表和点表的名称
 ;返回：SELECTText SQL语句
 ;创建时间：2015/6/9   13:34
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun GetSelectLines(linetable pointtable)
	(setq sqlstr (strcat "SELECT " linetable ".*,p1.x,p1.y,p1.surf_h,p2.x,p2.y,p2.surf_h FROM " 
			linetable "," pointtable " As p1," pointtable " As p2 WHERE " 
			"p1.Map_No=" linetable ".Start_Point AND p2.Map_No=" linetable ".End_Point"
		)
	)
)
 
;*********************************************************************************************
 ;函数定义:DrawTables(maintypes)
 ;功能：绘制线表和点表。线表格式固定，为正元管线数据库格式，仅导入其中的点表和线表.
 ;参数：("J" "P"...)-主类型列表
 ;返回：T or False
 ;创建时间：2014/12/31   19:34
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun DrawTables (maintypes / attrib_objs cols connectionobject deep1 deep2 e featurestr index 
	linelist lines linetablename location maintype Map_No newblock_ref newl newp numl nump onerecord 
	outlist p10 p11 pend pendname pointlist points pointtablename pstart pstart_h pstartname pstartx 
	pstarty ptextx ptexty ptextz rotation subsidstr pos)
	(LineInfo_GetSupportPath)
	(foreach maintype maintypes
		(setq linetablename	 (strcat maintype "L")
			  pointtablename (strcat maintype "P"))
		;; 读取数据
		(if	(setq ConnectionObject (GetConnectionObject))
			(progn
				(setq
					lines	 (ADOLISP_DoSQL ConnectionObject (GetSelectLines linetablename pointtablename)) ;线数据
					;;lines	 (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " linetablename)) ;线数据
					  points (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " pointtablename)) ;点数据
					  )
				(if	(and lines points)
					(progn
						(setq numl	(length lines)
							nump (length points)
							index	1
							pointlist nil
							cols (car points)
						)
						;;点表
						(if (> nump 1)
							(repeat	(- nump 1)
								(setq oneRecord	 (nth index points)
									  outlist nil
									  featurestr (nth (vl-position "Feature" cols) oneRecord)
									  subsidstr (nth (vl-position "Subsid" cols) oneRecord)
									  Map_No (nth (vl-position "Map_No" cols) oneRecord)
									  Exp_No (nth (vl-position "Exp_No" cols) oneRecord)
									  pstartx  (nth (vl-position "Y" cols) oneRecord)
									  pstarty  (nth (vl-position "X" cols) oneRecord)
									  pstart_h (nth (vl-position "Surf_H" cols) oneRecord)
									  index (1+ index)
								)	
								;;点号为空的问题
								(if (not Map_No) (setq Map_No Exp_No))								
								(setq outlist (cons (cons "Exp_No" Exp_No) outlist)
									  outlist (cons (cons "Map_No"  Map_No) outlist)
									  outlist (cons (cons "Feature"  featurestr) outlist)
									  outlist (cons (cons "Subsid"  subsidstr) outlist)
									  outlist (cons (cons "Main_Type" maintype) outlist)
									  
									  outlist (cons (cons "Unit" (nth (vl-position "Sur_Unit" cols) oneRecord)) outlist)
									  outlist (cons (cons "SProject" (nth (vl-position "SProject_Name" cols) oneRecord)) outlist)
									  outlist (cons (cons "Project" (nth (vl-position "Project_Name" cols) oneRecord)) outlist)
									  
								)
								;;自定义字段
								(if (setq pos (vl-position "Point_Size" cols)) (setq outlist (cons (cons "Point_Size" (nth pos oneRecord)) outlist)))
								(if (setq pos (vl-position "Well_Deep" cols)) 
									(if (setq data (nth pos oneRecord))
										(setq outlist (cons (cons "Well_Deep" (rtos data 2 4)) outlist))
										(setq outlist (cons (cons "Well_Deep" "") outlist))
										)
								)
								(if (setq pos (vl-position "Mark_Angle" cols)) (setq outlist (cons (cons "Mark_Angle" (nth pos oneRecord)) outlist)))
								(if (setq pos (vl-position "Version" cols)) (setq outlist (cons (cons "Version" (nth pos oneRecord)) outlist)))
								(if (setq pos (vl-position "GC_X" cols)) (setq outlist (cons (cons "GC_X" (nth pos oneRecord)) outlist)))
								(if (setq pos (vl-position "GC_Y" cols)) (setq outlist (cons (cons "GC_Y" (nth pos oneRecord)) outlist)))
								(if (setq pos (vl-position "Text_X" cols)) (setq outlist (cons (cons "Text_X" (nth pos oneRecord)) outlist)))
								(if (setq pos (vl-position "Text_Y" cols)) (setq outlist (cons (cons "Text_Y" (nth pos oneRecord)) outlist)))
								(if (setq pos (vl-position "Text_Z" cols)) (setq outlist (cons (cons "Text_Z" (nth pos oneRecord)) outlist)))
								
								(if (setq pos (vl-position "Location" cols)) (setq outlist (cons (cons "Location" (nth pos oneRecord)) outlist)))
								;;(if (setq pos (vl-position "Mdate" cols)) (setq outlist (cons (cons "Mdate" (nth pos oneRecord)) outlist)))
								;;(if (setq pos (vl-position "Sur_Date" cols)) (setq outlist (cons (cons "Sur_Date" (nth pos oneRecord)) outlist)))
								
								(if (setq newp (AddNewPoint (list pstartx pstarty pstart_h) Map_No maintype featurestr subsidstr outlist))
									(progn
										(setq pointlist (cons (list Map_No newp) pointlist))
										(ldata-put newp "Status_DB" 1)
										;;设置属性文字的位置,数据库自定义字段；
										(setq newblock_ref (vlax-ename->vla-object newp))
										;;rotation
										(if (setq pos (vl-position "Mark_Angle" cols))
											(if (setq rotation (nth pos oneRecord))
                                                (progn
													;;(setq rotation (atof rotation))
													(vla-put-rotation newblock_ref rotation)
													(if  (= :vlax-true (vla-get-HasAttributes newblock_ref))
														(progn
															;;text insertpoint
															(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
															(foreach e attrib_objs
																(if (= "点号" (vla-get-tagstring e))
																	(progn 
																		(setq ptextx (nth (vl-position "Text_X" cols) oneRecord)
																			ptexty (nth (vl-position "Text_Y" cols) oneRecord)
																			ptextz (nth (vl-position "Text_Z" cols) oneRecord)
																		)
																		(vla-put-InsertionPoint  e (vlax-3D-point (list ptextx ptexty ptextz)))
																		(vla-put-rotation e 0.0)
																	)
																)
															)
														);progn
													);if
												)
											)
										)
									)
								)
							)
						)
						(if pointlist
							(prompt (strcat "\n从数据表" pointtablename "中读取了" (rtos (length pointlist) 2 0) "个管线点."
											"数据表中共有" (rtos (- (length points) 1) 2 0) "个管线点."))
						)
						;;线表
						(setq numl	(length lines)
							index	1
							linelist nil
							cols (car lines) 
						)
						(if (> numl 1)
							(repeat	(- numl 1)
								(setq 
									  oneRecord	 (nth index lines)
									  outlist nil
									  pstartname (nth (vl-position "Start_Point" cols) oneRecord)
									  pendname (nth (vl-position "End_Point" cols) oneRecord)

									  ;outlist (cons (cons "Road_Name"  (nth (vl-position "Road_name" cols) oneRecord)) outlist)
									  
									  outlist (cons (cons "Start_Point"  pstartname) outlist)
									  outlist (cons (cons "End_Point"  pendname) outlist)
									  deep1 (nth (vl-position "Start_Deep" cols) oneRecord)
									  deep2 (nth (vl-position "End_Deep" cols) oneRecord)
								)
								
								
								;(if (setq pstart (assoc pstartname pointlist)) ;;效率最低点
									;(if (setq pend (assoc pendname pointlist))	;;效率最低点
										;(progn
											(if (not deep1) 
												(progn 
													(princ (strcat "\n导入数据错误：管线段" pstartname "-" pendname "起点深度为空！已自动设为-1."))
													(setq deep1 -1)
												)
											)
											(if (not deep2) 
												(progn 
													(princ (strcat "\n导入数据错误：管线段" pstartname "-" pendname "终点深度为空！已自动设为-1."))
													(setq deep2 -1)
												)
											)
											(setq  outlist (cons (cons "Material" (nth (vl-position "Material" cols) oneRecord)) outlist)
												  outlist (cons (cons "D_S" (nth (vl-position "D_S" cols) oneRecord)) outlist)
												  outlist (cons (cons "Main_Type" maintype) outlist)
												  outlist (cons (cons "Start_Deep" deep1) outlist)
												  outlist (cons (cons "End_Deep" deep2) outlist)
												  
												  outlist (cons (cons "Voltage" (nth (vl-position "Voltage" cols) oneRecord)) outlist)
												  outlist (cons (cons "Pressure" (nth (vl-position "Pressure" cols) oneRecord)) outlist)
												  outlist (cons (cons "Cab_Count" (nth (vl-position "Cab_Count" cols) oneRecord)) outlist)
												  outlist (cons (cons "Hole_Count" (nth (vl-position "Hole_Count" cols) oneRecord)) outlist)
												  outlist (cons (cons "Flowdirect" (nth (vl-position "Flowdirect" cols) oneRecord)) outlist)
												  
												  outlist (cons (cons "Sur_Date" (nth (vl-position "Sur_Date" cols) oneRecord)) outlist)
												  outlist (cons (cons "Unit" (nth (vl-position "Sur_Unit" cols) oneRecord)) outlist)
												  outlist (cons (cons "SProject" (nth (vl-position "SProject_Name" cols) oneRecord)) outlist)
												  outlist (cons (cons "Project" (nth (vl-position "Project_Name" cols) oneRecord)) outlist)
												  outlist (cons (cons "Mdate" (nth (vl-position "Mdate" cols) oneRecord)) outlist)
											)
											
											;;有些字段非标注杭州数据库，自定义字段
											(if (setq pos  (vl-position "Location" cols))
												(setq outlist (cons (cons "Location" (nth pos oneRecord)) outlist))
											)
											
											(if (setq pos  (vl-position "Hole_Used" cols))
												(setq outlist (cons (cons "Hole_Used" (nth pos oneRecord)) outlist))
											)
											(if (setq pos  (vl-position "Property" cols))
												(setq outlist (cons (cons "Property" (nth pos oneRecord)) outlist))
											)
											(if (setq pos  (vl-position "Road_name" cols))
												(setq outlist (cons (cons "Road_Name" (nth pos oneRecord)) outlist))
											)
											(if (setq pos  (vl-position "BuryWay" cols))
												(setq outlist (cons (cons "BuryWay" (nth pos oneRecord)) outlist))
											)
											; (setq pstart (cadr pstart)
												; p10 (cdr (assoc 10 (entget pstart)))
												; p10 (list (car p10) (cadr p10) (- (caddr p10) deep1))
												; pend (cadr pend)
												; p11 (cdr (assoc 10 (entget pend)))
												; p11 (list (car p11) (cadr p11) (- (caddr p11) deep2))
											; )
											(setq ncols (length cols)
												p10 (list (nth (- ncols 5) oneRecord ) (nth (- ncols 6) oneRecord) (nth (- ncols 4) oneRecord))
												p10 (list (car p10) (cadr p10) (- (caddr p10) deep1))
												p11 (list (nth (- ncols 2) oneRecord) (nth (- ncols 3) oneRecord) (nth (- ncols 1) oneRecord))
												p11 (list (car p11) (cadr p11) (- (caddr p11) deep2))
											)
											;;(ldata-put pstart "Depth" deep1)
											;;(ldata-put pend "Depth" deep2)
											
											;(ldata-put pstart "Location" location)
											;(ldata-put pend "Location" location)
											
											(if (setq newl (AddNewLine p10 p11 maintype outlist))
												(progn
													(ldata-put newl "Status_DB" 1)
													;;3.4.0版Mdate保存了调查时间，数据库时也是如此,使用Mdata代替Sur_Date
													(if (= "" (ldata-get newl "Sur_Date"))
														(if (/= "" (setq mdata (ldata-get newl "Mdate")))
															(ldata-put newl "Sur_Date" mdata)
														)
													)
													(AddLineTextLabel newl  deep1  deep2)
													(AddLineFlowdirect newl)
													
													(setq linelist (cons newl linelist))
												)
											)	
										;)
									;)	
								;)
								; ;; 点号只在线表中
								; (if (or (= nil pstart ) (= nil pend))
									; (prompt (strcat "\n错误！线表中的管线段" pstartname "-" pendname ",起点或终点不在点表内."))
								; )
								(setq index (1+ index))
							)
						)
						(if linelist
							(prompt (strcat "\n从数据表" linetablename "中读取了" (rtos (length linelist) 2 0) "个管线段."
											"数据表中共有" (rtos (- (length lines) 1) 2 0) "个管线段."))
						)
					)
					(progn
						(ADOLISP_ErrorPrinter)
					)
				) ;if
			) ;progn
		) ;if
;;;(prompt "\n\nDisconnecting from the database\n")
	(ADOLISP_DisconnectFromDB ConnectionObject)
	(setq ConnectionObject nil)
	)
	(princ)
)
;;
;;点名，所在的表名
; (defun DrawDBLine  (p1 p2 tableline	tablep ConnectionObject	/ dend dstart lineobj lines	onerecord pend pend_h pendname pendx pendy points pstart pstart_h pstartname pstartx pstarty sqlstatement)
	; (AddLayer tableline (strcat tableline "_Line"))
	; (if	ConnectionObject
		; (progn
			; (setq SQLStatement (strcat "SELECT * FROM "	tableline " WHERE (Start_Point=" "'" p1	"'"	" AND End_Point=" "'" p2 "') OR (Start_Point=" "'" p2 "'" " AND End_Point="	"'"	p1 ")")
				  ; lines		   (ADOLISP_DoSQL ConnectionObject SQLStatement) ;线数据
				  ; points	   (ADOLISP_DoSQL ConnectionObject
											  ; (strcat "SELECT * FROM " tablep " WHERE Map_No=" p1 " OR Map_No=" p2)) ;点数据
				  ; )
			; (if	(and (>= (length lines) 2) (>= (length points) 3))
				; (progn
					; (setq oneRecord	 (nth 1 lines)
						  ; pstartname (nth 0 oneRecord)
						  ; pendname	 (nth 1 oneRecord)
						  ; dstart	 (nth 2 oneRecord)
						  ; dend		 (nth 3 oneRecord)
						  ; )
					; (setq pstart   (assoc pstartname points)
						  ; pstartx  (nth 4 pstart)
						  ; pstarty  (nth 3 pstart)
						  ; pstart_h (nth 5 pstart)
						  ; )
					; (setq pend	 (assoc pendname points)
						  ; pendx	 (nth 4 pend)
						  ; pendy	 (nth 3 pend)
						  ; pend_h (nth 5 pend)
						  ; )
					; (setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
											   ; (vlax-3D-point pstartx pstarty (- pstart_h dstart))
											   ; (vlax-3D-point pendx pendy (- pend_h dend))))
					; (if	lineobj
						; (progn
							; (vlax-ldata-put lineobj gl_AppName (cons tableline oneRecord)) ;加入数据库记录
							; (vlax-ldata-put lineobj "STATUS" (list "STATUS" 1 0)) ;状态：已在数据库中，未修改
							; ) ;progn
						; )
					; ) ;progn
				; (progn
					; (prompt (strcat "\n未找到" p1 "," p2 "记录。"))
					; )
				; ) ;if
			; ) ;progn
		; ) ;if
	; ) ;

 ;*********************************************************************************************
 ;函数定义:DrawPoint (strP,x,y,z,scode,scale pointtype)
 ;功能：插入一个点，并显示点号和符号,
 ;参数：点号，x坐标，y坐标，z坐标，符号编码，比例尺（1:1000,为1；1:500为2;以此类推）
 ;返回：点块
 ;创建时间：2014/07/10   08:30
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
; (defun DrawPoint  (strP	x y	z scode	scale pointtype	/ ts tablename app attrib_obj attrib_objs doc mspace newblock_ref symbol symbolpath	tmpresult myscale)
	; ;;加载符号表

	; (setq tablename (strcat pointtype "P"))
	; (AddLayer tablename (strcat tablename "_Point"))

	; (setq ts (Addfont 'acstyle))
	; (if	(= nil gl_SymbolBlockList)
		; (setq gl_SymbolBlockList (ReadSymbolConfig nil))
		; )
	; (setq scode	 (strcat pointtype (rtos scode 2 0))
		  ; symbol (assoc scode gl_SymbolBlockList))
	; (if	(= nil symbol) ;图块库中没有该代码，以0号取代
		; (setq symbol (assoc "ALL1" gl_SymbolBlockList))
		; )
	; (setq symbolpath (strcat gl_BLOCKLIB_PATH (cadr symbol)))

	; ;;
	; ;;从图库创建图块
	; (setq app		   (vlax-get-acad-object)
		  ; Doc		   (vla-get-Activedocument app)
		  ; Mspace	   (vla-get-modelspace Doc)
		  ; myscale	   (/ 1.0 scale) ;！注意：以1.0代替1，否则只计算整数部分
		  ; newblock_ref (vla-InsertBlock Mspace (vlax-3D-point x y z) symbolpath 1 1 1 0)
		  ; )
	; ;;修改点号属性
	; (if	(= :vlax-true (vla-get-HasAttributes newblock_ref))
		; (progn
			; (setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
			; (if	(> (length attrib_objs) 0)
				; (progn
					; (setq attrib_obj (car attrib_objs))
					; (vla-put-TextString attrib_obj strP) ;内容
					; (vla-put-Height attrib_obj 2) ;字高
					; (vla-put-scalefactor attrib_obj 0.8) ;宽度因子
					; (if	ts
						; (vla-put-stylename attrib_obj (vla-get-name ts)))
					; ) ;progn
				; ) ;if
			; ) ;progn
		; ) ;if
	; (vla-put-XScaleFactor newblock_ref myscale)
	; (vla-put-YScaleFactor newblock_ref myscale)
	; (vla-put-ZScaleFactor newblock_ref myscale)

	; newblock_ref
	; ) ;defun

 ;*********************************************************************************************
 ;函数定义:AddLayer(layername)
 ;功能：添加图层，并设置为当前图层,图层名称的第一个字母是主类型
 ;参数：tablename-类型名称 layername-图层名称
 ;返回：nil or 当前图层layerobj
 ;创建时间：2014/07/09   22:10
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun AddLayer	 (tablename layername / aclayer app colorindex doc layers mspace pointlayer)
	(setq app	 (vlax-get-acad-object)
		  Doc	 (vla-get-Activedocument app)
		  Mspace (vla-get-modelspace Doc)
		  ) ;_ end setq
	(setq Layers  (vla-get-layers Doc)
		  AcLayer (vla-get-activelayer Doc)
		  ) ;_ end setq

	(setq pointlayer (vla-add Layers layername)
		  )
	(if	(= gl_TableColorList nil)
		(setq gl_TableColorList (ReadColorConfig nil))
		) ;if
	(setq colorindex (cadr (assoc (substr tablename 1 1) gl_TableColorList)))
	(if	(= nil colorindex)
		(setq colorindex 7) ;没有类别的对象，使用白色
		)

	(vla-put-color pointlayer colorindex)
	(vla-put-activelayer Doc (vla-item Layers layername))
	(vla-get-activelayer Doc)
		
)
 ;*********************************************************************************************
 ;函数定义:AddFont()
 ;功能：使用字体simtxt.shx,hztxt.shx
 ;参数：
 ;返回：字体对象,nil
 ;创建时间：2014/08/05  10：36
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun AddFont	(pretxtstyle / app bigfile doc fontfile mainpath mypath rbig rfont ts txtstyles)
	(setq app (vlax-get-acad-object)
		  Doc (vla-get-Activedocument app)
		  ) ;_ end setq
	(setq txtstyles	  (vla-get-TextStyles Doc)
		pretxtstyle (vla-get-ActiveTextStyle Doc)
	) ;_ end setq
;;;	(if (not (setq ts (vla-item txtstyles "DB_DXT")))
		(progn
			(LineInfo_GetSupportPath)
			(setq ts (vla-add txtstyles "DB_DXT") ;管线宋体等线体
				  ) ;_ end_setq

			(setq mainpath (strcat (vl-filename-directory (findfile "acad.exe"))
								   "\\Fonts\\"
								   ) ;_ end_strcat
				  mypath   (strcat gl_INFO_LINE_PATH "fonts\\")
				  ) ;_ end_setq
			(setq fontfile (findfile (strcat mainpath "SMSIM.shx"))
				  bigfile  (findfile (strcat mainpath "hztxt.shx"))
				  ) ;_ end_setq
			;;
			(setq rfont	1
				  rbig 1
				  ) ;_ end_setq
			;;复制到系统fonts目录
			(if	(null fontfile)
				(setq rfont	(vl-file-copy (strcat mypath "SMSIM.shx")
										  (strcat mainpath "SMSIM.shx")
										  ) ;_ end_vl-file-copy
					  ) ;_ end_setq
				) ;_ end_if
			(if	(null bigfile)
				(setq rbig (vl-file-copy (strcat mypath "hztxt.shx")
										 (strcat mainpath "hztxt.shx")
										 ) ;_ end_vl-file-copy
					  ) ;_ end_setq
				) ;_ end_if
			;;
			(if	(and rfont rbig)
				(progn
					(vla-put-fontfile ts (strcat mainpath "SMSIM.shx"))
					(vla-put-bigfontfile ts (strcat mainpath "hztxt.shx"))
					(vla-put-Width ts 0.8)
					(vla-put-activetextstyle doc ts)
					) ;_ end_progn
				(setq ts nil)
				) ;_ end_if
		)
		;;else
;;;;;;		(progn
;;;;;;			(vla-put-activetextstyle doc ts)
;;;;;;		)
;;;	)
	ts
	) ;_ end_defun
 ;*********************************************************************************************
 ;函数定义:GetTableFromLine()
 ;功能：从线中创建连接关系表，并输出到文件
 ;参数：layername
 ;返回：nil or 当前图层layerobj
 ;创建时间：2014/07/09   22:10
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
; (defun GetTableFromLine	 (/ e file i lineset outpath p1 p2 point1 point2 pointlist strout tmp)
	; (setq lineset (ssget "X" (list (cons 0 "LINE")))) ;选择所有LINE对象
	; (setq i		  0
		  ; outpath "C:\\zhd-lines.csv")
	; (if	(setq pointlist (GetPointsFromBlock))
		; (progn
; ;;; 输出到文件
			; (vl-file-delete outpath)
			; (setq file (open outpath "w"))
			; (write-line "起点号，终点号，起点深度，终点深度" file)
			; (repeat	(sslength lineset)
				; (setq e		 (entget (ssname lineset i))
					  ; point1 (cdr (assoc 10 e))
					  ; point2 (cdr (assoc 11 e))
					  ; p1	 (GetPointFromXY pointlist (car point1) (cadr point1))
					  ; p2	 (GetPointFromXY pointlist (car point2) (cadr point2))
					  ; )
				; (if	(and p1 p2)
					; (setq strout (strcat (car p1)
										 ; ","
										 ; (car p2)
										 ; ","
										 ; (rtos (- (last p1) (last point1)) 2 4)
										 ; ","
										 ; (rtos (- (last p2) (last point2)) 2 4))
						  ; tmp	 (write-line strout file)
						  ; )
					; )
				; (setq i (+ 1 i))
				; )
			; (close file)
			; (startapp "notepad" outpath)
			; ) ;progn
		; ) ;if
	; )
 ;*********************************************************************************************
 ;函数定义:GetPointFromXY(pointlist x y)
 ;功能：根据x y坐标，查找最近的点
 ;参数：pointlist (("p1" x1 y1 z1)("p2" x2 y2 z2)...)
 ;返回：nil or 点("Pi" xi yi zi)
 ;创建时间：2014/07/09   23:00
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
; (defun GetPointFromXY  (pointlist x y / p0 x0 y0 num i)
	; (setq num (length pointlist)
		  ; p0  (car pointlist)
		  ; x0  (nth 1 p0)
		  ; y0  (nth 2 p0)
		  ; i	  1)
	; (while (and (< i num) (> (abs (- x0 x)) gl_MIN) (> (abs (- y0 y)) gl_MIN))
		; (setq p0 (nth i pointlist)
			  ; x0 (nth 1 p0)
			  ; y0 (nth 2 p0)
			  ; i	 (+ 1 i)
			  ; ) ;
		; ) ;while
	; (if	(= i num)
		; (setq p0 nil)
		; )
	; p0
	; )

;;修改文字样式
;;使用标准字体，1:1000时，字高2。
;;根据比例尺调整;
(defun C:ModifyTextStyle  (/ entset i ts acts ename newblock_ref attrib_objs num scale)
	(setq entset (ssget "X" (list (cons 0 "INSERT")))
		  i		 0
		  num	 0
		  ) ;选择所有enttype对象
	(setq scale (Xrecord-Get gl_AppName "SCALE"))
	(if	scale
		(setq gl_MAP_SCALE (atof (car scale))))
	(setq ts (AddFont acts))
	(if	(/= nil entset)
		(repeat	(sslength entset)
			(setq ename	(ssname entset i)
				  ) ;_ end_setq
			(if	(vlax-ldata-get ename gl_AppName)
				(progn
					(setq newblock_ref (vlax-ename->vla-object ename))
					;;修改点号属性
					(if	(= :vlax-true (vla-get-HasAttributes newblock_ref))
						(progn
							(setq attrib_objs (vlax-safearray->list
												  (vlax-variant-value (vla-GetAttributes newblock_ref))
												  ) ;_ end_vlax-safearray->list
								  ) ;_ end_setq
							(if	(> (length attrib_objs) 0)
								(progn
									(setq attrib_obj (car attrib_objs))
									(vla-put-Height attrib_obj (/ 2.0 gl_MAP_SCALE)) ;字高
									(vla-put-ScaleFactor attrib_obj 0.8)
									(if	ts
										(vla-put-stylename attrib_obj (vla-get-name ts))
										) ;文字样式
									(setq num (1+ num))
									) ;progn
								) ;if
							) ;progn
						) ;if
					) ;_ end_progn
				) ;_ end_if
			(setq i (1+ i))
			) ;repeat	
		) ;if

	(prompt (strcat "\n已修改" (rtos num 2 0) "个管线点的文字样式。\n"))
	) ;_ end_defun
;*********************************************************************************************
 ;函数定义:Drawblock (pos direction blockname)
 ;功能：绘制图块，加入旋转角度
 ;参数：块对象
 ;返回：
 ;创建时间：2014/09/04   21:00
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun Drawblock (pos rotation blockname / myscale newblock_ref symbolpath basename)
    (setq symbolpath (strcat gl_BLOCKLIB_PATH blockname)
		basename (vl-filename-base blockname)
		newblock_ref nil
		myscale      (/ 1.0 gl_MAP_SCALE) );！注意：以1.0代替1，否则只计算整数部分
	(if (vl-position basename gl_BlockNameList) 
		 (setq newblock_ref (vla-InsertBlock (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
                                        (vlax-3D-point pos)
                                        basename
                                        myscale
                                        myscale
                                        myscale
                                        rotation
                       )) ;_ end_vla-InsertBlock
          (setq newblock_ref (vla-InsertBlock (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
                                        (vlax-3D-point pos)
                                        symbolpath
                                        myscale
                                        myscale
                                        myscale
                                        rotation
                       )
					gl_BlockNameList (cons basename gl_BlockNameList)		   
			) ;_ end_vla-InsertBlock
    ) 
	newblock_ref
) ;_ end_defun

;;;修改管点块的方向
(defun C:YBDirection( / allp ent i np subsidstr featurestr)
	(print "自动修正管线点块的方向，雨水篦，污水篦与连接线垂直。")
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0 )
			(repeat np
				(setq ent (ssname allp i)
					i (1+ i))
				(if (setq subsidstr (ldata-get ent "Subsid"))
					(cond
						((= subsidstr "雨水篦") (ModifyblockAngle ent pi))
						((= subsidstr "污水篦") (ModifyblockAngle ent pi))
						((= subsidstr "雨篦") (ModifyblockAngle ent pi))
						((= subsidstr "预留口") (ModifyblockAngle ent pi))
					)
				)
				(if (setq featurestr (ldata-get ent "Feature"))
					(cond
						((= featurestr "出水口") (ModifyblockAngle ent pi))
						((= featurestr "非测区去向点") (ModifyblockAngle ent pi))
					)
				)	
			)
		)		
	)
	(print "自动修正方向完成。")
)

;*********************************************************************************************
 ;函数定义:ModifyblockAngle (entblock direction)
 ;功能：修改管线点的方向
 ;参数：带属性的管线点，与连接直接的方向夹角（弧度）
 ;返回：
 ;创建时间：2015/03/24  12:00
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************	
(defun ModifyblockAngle (entblock direction / a0 attrib_objs attrib_pt block_ref connectlines
		dist e el entlist langle linelist nline p10 p11 pos pos0 )
	(if entblock
		(if (vlax-ldata-get entblock gl_AppName)
			(progn
				(setq entlist (entget entblock)
					pos (cdr (assoc 10 entlist))
					pos0 (list (car pos) (cadr pos) 0)
					;edge (ldata-get entblock "Edge")
					ConnectLines	(GetConnectEntity (car pos) (cadr pos) "LINE")
				)
					;;有边
				(if (listp ConnectLines)
					(progn
						(setq nline (length ConnectLines)) 
						(if (> nline 0)
							(progn
								(setq el (car ConnectLines))
								(setq linelist (entget el)
									;Flowdirect (ldata-get el "Flowdirect")
									p10 (cdr (assoc 10 linelist))
									p10 (list (car p10) (cadr p10) 0)
									p11 (cdr (assoc 11 linelist))
									p11 (list (car p11) (cadr p11) 0)
									dist (distance pos0  p10 )
								)
								
								(if (< dist gl_MIN);;直线方位角
									(setq langle (angle pos0 p11))
									(setq langle (angle pos0 p10))
								)
								(setq a0 (+ direction langle))

								;;获取属性文字的位置信息
								;;只旋转图块，不移动文字；
								(setq block_ref (vlax-ename->vla-object entblock)
									rotation (vla-get-rotation block_ref)
									attrib_pt nil)
								(if (or gl_DEBUG (= 0 rotation));;只调整未旋转的对象,调试时忽略所有已旋转对象。
									(if (= :vlax-true (vla-get-HasAttributes block_ref))
										(progn
											;;text insertpoint
											(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes block_ref))))
											(foreach e attrib_objs
												(if (= "点号" (vla-get-tagstring e))
													(if (setq attrib_pt (vla-get-InsertionPoint  e ))
														(progn 
															;(setq attrib_pt (vlax-safearray->list (vlax-variant-value attrib_pt)))
															(vla-put-rotation block_ref a0)
															(vla-put-rotation e 0)
															(vla-put-InsertionPoint e attrib_pt)
														)
													)
												)
											)
										 );progn
									);if
								)
							)
							(prompt (strcat "\n错误：" (ldata-get entblock "Map_No") "不与管线段相连。"))
						)		
					)
					(progn
						(prompt (strcat "\n错误：" (ldata-get entblock "Map_No") "不与管线段相连。"))
					)
				)
			)
		)
	)	
)
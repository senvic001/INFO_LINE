
;*********************************************************************************************
;函数定义:C:CheckErrors()
;功能：检查点线中的问题
;参数：
;返回：
;创建时间：2014/12/18   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:CheckErrors( /  allpointset bm  bfix edges entp errorstr fstr hd i mapstr ne nerror 
						namelist e1 e2 handle1 handle2 mapstr1 mapstr2 pos1 pos2 tname)
	;;检查是否存在未定义端点的线段,端点重复的线段,并建立拓扑关系,统计线段数量
	(princ "\n开始查找管线错误,可能需要较长时间,请耐心等待....")
	(if (setq bfix (getstring "\n是否自动修复连接管线错误？(Y/N)"))
	    (setq bfix (strcase bfix))
	)
	(setq gl_Time (getvar "TDUSRTIMER"))
	(UpdateTopo)
	;;检查各点的feature如三通等,是否连接了三根线;是否是孤立的点;起讫点是否是连一个线
	(setq AllPointSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
		i 0
		gl_ErrorList nil
		errorstr nil
	)
	(if AllPointSet
        (repeat (sslength AllPointSet)
			(setq entp (ssname AllPointSet i)
				hd (cdr (assoc 5 (entget entp)))
			)
			(if (vlax-ldata-get entp gl_AppName)
				(progn
					(setq mapstr (ldata-get entp "Map_No")
						fstr (ldata-get entp "Feature")
						tname (ldata-get entp "Main_Type"))
					(if (setq edges (ldata-get entp "Edge"))
						(progn
							(setq ne (length edges)
								errorstr nil
								bfixed nil)	;是否已经修复
							(cond
								((= fstr "起讫点") 
									(if (/= ne 1)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)
								((= fstr "出地") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "入地") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "上杆") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "下杆") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "堵头") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "断头") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "进水口") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "出水口") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "预留口") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "转折点") 
									(if (/= ne 2)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "直线点") 
									(if (/= ne 2)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "变深") 
									(if (/= ne 2) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "变径") 
									(if (/= ne 2) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "变材") 
									(if (/= ne 2) (setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0)"根管线,与点特征\(" fstr "\)不一致."))))
								((= fstr "三通") 
									(if (/= ne 3)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "四通") 
									(if (/= ne 4)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "五通") 
									(if (/= ne 5)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "六通") 
									(if (/= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "多通") 
									(if (<= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "三分支") 
									(if (/= ne 3)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "四分支") 
									(if (/= ne 4)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "五分支") 
									(if (/= ne 5)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "六分支") 
									(if (/= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "多分支") 
									(if (<= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "连接" (rtos ne 2 0) "根管线,与点特征\(" fstr "\)不一致."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "已修改为：" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								(T (setq errorstr nil))
							)
							(if (and (/= nil errorstr)(= 0 ne )) (setq errorstr (strcat errorstr "该点未与其他点连接.")))
							(if (and (not bfixed) errorstr )
								(progn
									(prompt (strcat "\n" errorstr))
									(setq gl_ErrorList (cons (cons hd errorstr) gl_ErrorList))
								)
							)
						)
						;;else
						(progn
							(setq errorstr (strcat "PointError:" mapstr "孤立点,或未定义边属性.")
								gl_ErrorList (cons (cons hd errorstr) gl_ErrorList)
							)
							(prompt (strcat "\n" errorstr))
						)
					)	
				)
			)
			(setq i (1+ i))
		)
	)
	
	(princ "\n错误检查共用时")
	(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
	(princ "秒.")
	
	(setq nerror (length gl_ErrorList))
	(if (= nerror 0)
		(prompt "\n 未发现管线点和管线段的错误.")
		(progn
			(prompt (strcat "\n共找到" (rtos nerror 2 0) "个管线点或管线段连接错误.	"))
			(if (setq bm (Fun_InPutString "Y" "USERS2" "\n现在就立即修复吗?(Y/N)"))
				(if (= "Y" (setq bm (strcase bm)))
					(C:EditErrorList)
				)
			)
		)
	)
	
	(princ)
)

(defun setfeature(iedge ent typename / bfix)
	(setq bfix nil)
	(if (or (= typename "Y")(= typename "W")(= typename "P")(= typename "J")(= typename "Y"))
		(cond
			((= iedge 0) (progn (ldata-put ent "Feature" "起讫点") (setq bfix T)))
			((= iedge 1) (progn (ldata-put ent "Feature" "起讫点") (setq bfix T)))
			((= iedge 2) (progn (ldata-put ent "Feature" "直线点") (setq bfix T)))
			((= iedge 3) (progn (ldata-put ent "Feature" "三通") (setq bfix T)))
			((= iedge 4) (progn (ldata-put ent "Feature" "四通") (setq bfix T)))
			((= iedge 5) (progn (ldata-put ent "Feature" "五通") (setq bfix T)))
			((= iedge 6) (progn (ldata-put ent "Feature" "六通") (setq bfix T)))
			((> iedge 6) (progn (ldata-put ent "Feature" "多通") (setq bfix T)))
		)
		(cond
			((= iedge 0) (progn (ldata-put ent "Feature" "起讫点") (setq bfix T)))
			((= iedge 1) (progn (ldata-put ent "Feature" "起讫点") (setq bfix T)))
			((= iedge 2) (progn (ldata-put ent "Feature" "直线点") (setq bfix T)))
			((= iedge 3) (progn (ldata-put ent "Feature" "三分支") (setq bfix T)))
			((= iedge 4) (progn (ldata-put ent "Feature" "四分支") (setq bfix T)))
			((= iedge 5) (progn (ldata-put ent "Feature" "五分支") (setq bfix T)))
			((= iedge 6) (progn (ldata-put ent "Feature" "六分支") (setq bfix T)))
			((> iedge 6) (progn (ldata-put ent "Feature" "多分支") (setq bfix T)))
		)
	)
	bfix
)
;*********************************************************************************************
;函数定义:C:RepairErrorSameName()
;功能：修复重名错误
;参数：
;返回：
;创建时间：2015/3/25   1：00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:RepairErrorSameName( / allpointset e1 e2 entp errorstr  i mapstr1 mapstr2 namelist newname nfix
	ent allent pos pos10)
	(princ "\n自动修复点名重复错误。" )
	(if (setq AllPointSet (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		;;检查点的重名问题
		(progn
			(setq namelist nil
				nfix 0
				i 0)
			(repeat (sslength AllPointSet)
				(setq entp (ssname AllPointSet i))
				(if (vlax-ldata-get entp gl_AppName)
					(setq namelist (cons (list entp (ldata-get entp "Map_No")) namelist))
				)
				(setq i (1+ i))
			)
			(setq gl_MapNameList (GetMapNamesList)
				gl_ExpNameList (GetExpNamesList))
			
			(while (< 1 (length namelist))
				(setq e1 (car namelist)
					namelist (cdr namelist)
					mapstr1 (cadr e1))
				(foreach e2 namelist
					(setq mapstr2 (cadr e2)
						e2 (car e2))
					(if (= mapstr1 mapstr2)
						(progn
							(setq newname (CreateNewPointName mapstr2 (ldata-get e2 "Main_Type") "MAP"))
							(if (SetAttrib-PointName e2 "点号" newname)
								(progn
									(ldata-put e2 "Map_No" newname)
									(ldata-put e2 "Exp_No" newname)
									(setq errorstr (strcat "\nPointError:" mapstr1 "图上点号重复.图号和物探点号都自动修改为" newname ".")
										namelist (vl-remove e2 namelist)
										gl_MapNameList (cons newname gl_MapNameList)
										gl_ExpNameList (cons newname gl_ExpNameList)
										nfix (1+ nfix)
									)
									(princ errorstr)
									
									;;处理相关联的线段，startpoint endpoint
									(setq pos (cdr (assoc 10 (entget e2)))
										allent (GetConnectEntity (car pos) (cadr pos) "LINE")
										i 0)
									(repeat (length allent)
										(setq   ent (nth i allent)
											pos10 (cdr (assoc 10 (entget ent)))
										)
										(if (IsSameXY pos pos10);;pos10 is the same point
											(ldata-put ent "Start_Point" newname)
											;;else
											(ldata-put ent "End_Point" newname)
										)
										(setq i (1+ i))
									)	
								)
							)
						)
					)
				)
			)
		) 
	)
	(princ (strcat "\n修复了" (rtos nfix 2 0) "个点的重名。"))
	(princ)
)
 ;;删除实体,并新建实体时，需要更新StackList
    ;;(oldent 删除的实体  newent 新建的实体 StackList)
(defun RebulidErrorList (oldent newent errorlist / e p1 hnew hold)
	;;(setq errorlist gl_ErrorList)
	(if (and (/= nil newent) (/= nil oldent) (/= nil errorlist))
		(foreach e errorlist
			(setq p1 (car e)
				hnew (cdr (assoc 5 (entget newent)))
				hold (cdr (assoc 5 (entget oldent))))
			(if (= p1 hold)
				(setq errorlist (subst (cons hnew (cdr e)) e errorlist)))
			);foreach
		 );if
	errorlist
)

;*********************************************************************************************
;函数定义:C:FlowError()
;功能：检查流向错误.(1)某点只有流出没有流入,(2) 形成循环流向 (3) 流向与高程不吻合 (4) 缺少流向
;前置条件:建立点线之间的关系Edge
;参数：
;返回：追加到错误列表gl_ErrorList
;创建时间：2015/1/7   10:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************		
(defun C:FlowError(/ alllinelist bdifferflow e edge edge1 edge2 elist endname entl entls errorstr 
			flowdirect flowerrorlist hentl hentp i linelist linkedges linkp Map_Nos 
			nextline nline p10 p11 pent1 pent2 preflow singlelist stackin stackout startedges startname
			startp tns typename x)
	(prin1 "\n检查雨水、污水、排水流向错误。\n错误类型：(1)管线点只有流出或流入 (2)流向形成闭合环 (3)流向与高程相反 (4)未定义流向")
	(prin1)
	; (setq tns (Fun_InPutString "P,Y,W" "USERS1" "\n管线类别：Y-雨水，W-污水，P-排水。
							; \n请输入需要检查流向的管线类别代码，多个类型用逗号(,)分隔："))
	; (if tns 
		; (setq tns (strcat "(" (strcase tns) ")"))                                            
	; )
	; (if (not (listp (setq tns (read tns))))
		; (progn
			; (prompt "\n错误：输入类别格式不正确.")
			; (exit)
		; )
	; )
	; (setq tns (mapcar '(lambda (x) (vl-symbol-name x)) tns))
	(setq tns (list "Y" "W" "P"))
	(setq AllLineList (GetTypedLineList)
		flowerrorlist nil)	;;错误实体列表
	(foreach e AllLineList                                   
		(setq linelist (cadr e)
			typename (car e)
			nline (length linelist)
			singlelist nil ;the first edges
			stackin nil
			stackout nil
			i 0)
		(if (and (> nline 0) (vl-position typename tns))
			(progn
				;;find all end edges
				(while (< i nline)
					(setq entl (nth i linelist))
					(if (setq edge (ldata-get entl "Edge"))
						(progn
							(setq pent1 (handent(car edge))
								pent2 (handent(cadr edge))
								edge1 (ldata-get pent1 "Edge")
								edge2 (ldata-get pent2 "Edge")
							)
							(if (or (= (length edge1 ) 1) (= (length edge2) 1));the point only has one edge.
								(setq singlelist (cons entl singlelist))
							)
						)
					)
					(setq i (1+ i))
				)
				;;
				(while (> (length singlelist) 0)
					;;initialize startp and linkp and stackin
					(setq entl (car singlelist)
						edge (ldata-get entl "Edge")
						singlelist (cdr singlelist)
						pent1 (handent(car edge))
						pent2 (handent(cadr edge))
						edge1 (ldata-get pent1 "Edge")
						stackout nil
					)
					(if (= 1 (length edge1));;start point and link point
						(setq startp pent1
							linkp pent2) 
						(setq startp pent2 
							linkp pent1)
					)
					(setq stackin (cons (list startp linkp entl) stackin))
					;;深度优先 
					(while (> (length stackin) 0)
						(setq entl (caddr (car stackin))
							  startp (car (car stackin))
							  linkp (cadr (car stackin))
							  stackin (cdr stackin)
						)
						
						;;beign check flowdirect errors
						(setq elist (entget entl)
							startname (ldata-get entl "Start_Point")
							endname (ldata-get entl "End_Point")
							p10 (cdr (assoc 10 elist))
							p11 (cdr (assoc 11 elist))
							flowdirect (ldata-get entl "Flowdirect")
							hentl (cdr (assoc 5 elist))
							hdist (distance (list (car p10) (cadr p10)) (list (car p11) (cadr p11)))
							vdist (abs (- (last p10) (last p11)))
						)
						(if (> hdist gl_MIN)
							(setq bpl (/ vdist hdist))
							(setq bpl nil)
						)
						;;Error:no definition of flowdirection
						(if (= 0 (ldata-get entl "Flowdirect")) 
							(progn
								(setq errorstr (strcat "FlowError:管线段" startname "-" endname "未定义流向。")
									gl_ErrorList (cons (cons hentl errorstr) gl_ErrorList)
									flowerrorlist (cons entl flowerrorlist)
								)
								(print errorstr)
							)
						)
						;;Error:check elevation
						(if (= 1 flowdirect);p10->p11
							(if (< (last p10) (last p11))
								(progn
									(setq errorstr  (strcat "FlowError:管线段" startname "-" endname "流向与管底高程不一致。" 
													 "距离：" (rtos hdist 2 3) ",高差：" (rtos vdist 2 3) ",坡率：" (rtos bpl 2 5))
										gl_ErrorList (cons (cons hentl errorstr) gl_ErrorList)
										flowerrorlist (cons entl flowerrorlist)
									)
									(print errorstr)
								)
							)
						)
						(if (= 2 flowdirect);p10<-p11
							(if (< (last p11) (last p10))
								(progn
									(setq errorstr  (strcat "FlowError:管线段" startname "-" endname "流向与管底高程不一致。" 
													 "距离：" (rtos hdist 2 3) ",高差：" (rtos vdist 2 3) ",坡率：" (rtos bpl 2 5))
										gl_ErrorList (cons (cons hentl errorstr) gl_ErrorList)
										flowerrorlist (cons entl flowerrorlist)
									)
									(print errorstr)
								)
							)
						)
						
						;;Error:all in or all out
						(setq linkedges (ldata-get linkp "Edge")
							startedges (ldata-get startp "Edge")
							Map_Nos (ldata-get startp "Map_No")
							bdifferflow nil
							preflow -1	;前一个边的流向，用于比较流向的编号
						)
						(if (> (length startedges) 1)
							(progn
								(foreach e startedges
									(setq entls (handent (last e))
										  pent1 (handent (car e))
										  pent2 (handent (cadr e))
										  startname (ldata-get entls "Start_Point")
										  endname (ldata-get entls "End_Point")
										  flowdirect (ldata-get entls "Flowdirect")
									)
									
									(if (> flowdirect 0)	;;流入本点的方向为1，流出本点的方向为2
										(progn
											(if (= Map_Nos startname) 
												(if (= 1 flowdirect)
													(setq flowdirect 2);flow out
													(setq flowdirect 1));flow in
											)
											(if (/= -1 preflow) 
												(if (/= preflow flowdirect) (setq bdifferflow T))
												(setq preflow flowdirect);初值
											)
										)
										(setq bdifferflow T)	;包含未定义流向的边,则不考虑流向一致问题.
									)
									
								)
								(if (not bdifferflow) ;流向相同
									(progn
										(setq  hentp (cdr (assoc 5 (entget startp)))
											errorstr (strcat  "FlowError:管线点" Map_Nos "流向相同。")
											gl_ErrorList (cons (cons hentp  errorstr) gl_ErrorList)
											flowerrorlist (cons startp flowerrorlist)
										)
										(print errorstr)
									)
								)
							)
						)
						
						;;put line to stackout
						(setq stackout (cons entl stackout)
							singlelist (vl-remove entl singlelist) 
						)
						
						(foreach e linkedges
							(setq nextline (handent (last e))
								  pent1 (handent (car e))
								  pent2 (handent (cadr e))
								  )
							(if (not (equal nextline entl))
								(if  (vl-position nextline stackout);是否存闭合管线
									(progn
										(setq errorstr (strcat "FlowError:管线段" (ldata-get nextline "Start_Point") "-" (ldata-get nextline "End_Point") "属于闭合管线段。"))
										(setq gl_ErrorList (cons (cons (cdr (assoc 5 (entget nextline))) errorstr) gl_ErrorList))
										(print errorstr )
									)
									;;else
									(if (equal pent1 linkp )
										(setq stackin(cons (list linkp pent2 nextline) stackin))
										(setq stackin(cons (list linkp pent1 nextline) stackin))
									)
								)
							)
						)
					)
				)
			)
		)
	)
	(setq nerror (length flowerrorlist))
	(if (= nerror 0)
		(prompt "\n 未发现流向错误.")
		(progn
			(prompt (strcat "\n共找到" (rtos nerror 2 0) "个流向错误.\n未处理的错误共有" (rtos (length gl_ErrorList) 2 0) "个."))
			(if (setq bm (Fun_InPutString "Y" "USERS2" "\n现在就立即修复吗?(Y/N)"))
				(if (= "Y" (setq bm (strcase bm)))
					(C:EditErrorList)
				)
			)
		)
	)
	(princ)
)
;;输出错误列表到文件
;;输出错误列表到文件
(defun C:OutPutErrorList ( / pointPath filep e)
	;;打开点文件
	(setq pointPath "C:\\Line_Info_Errors.txt")
     (vl-file-delete pointPath)
     (if (not (setq filep (open pointPath "w")))
         (progn
             (prompt (strcat "\n打开文件" pointPath "失败。退出！"))
             (exit)
         ) ;_ End_progn
     ) ;if
    
	(foreach e gl_ErrorList
		(write-line (strcat (car e) "," (cdr e)) filep)
	)
	(close filep)
	(prompt (strcat "\n输出" (rtos (length gl_ErrorList) 2 0) "条错误信息到文件" pointPath "."))
    (if (> (length gl_ErrorList) 0) (startapp "EXPLORER.EXE" pointPath))
    (princ)
)

;;;查找未定义属性的管线段
(defun C:FindLineWithnothing (/ lineset findline i ent ecolor lobj len num )
    (setq lineset  (ssget "X" (list (cons 0 "LINE")))
          i        0
	  len (sslength lineset)
	  ent nil
          num 0
          ;findline T
    ) ;_ end_setq

    (repeat  len
        (setq ent (ssname lineset i))
        (if (not (vlax-ldata-get ent gl_AppName))
			(progn
				(setq 
					lobj (vlax-ename->vla-object (ssname lineset i))
					ecolor (vla-get-truecolor lobj)
					num (1+ num)
				) ;_ end_setq
				(vla-put-colorindex ecolor acCyan)
				(vla-put-truecolor lobj ecolor)
			)
        )
        (setq i  (1+ i));
    ) ;_ end_repeat
    (if (> num 0)
		(prompt (strcat "\n错误:有 " (rtos num 2 0) " 条线段未定义属性.线段的颜色被改为青色."))
		(prompt "\n所有直线段都定义为管线段。")
	)
	(princ)
)


;;？？？？部分对象无Xdata，不知缘由
(defun C:SetAllXdata( / i j allset)
	(setq allset (ssget "X")
		i 0
		j 0)
	(repeat (sslength allset)
		(setq ent (ssname allset i)
			i (1+ i)
			)
		(if (vlax-ldata-get ent gl_AppName)
			(if (not (getxdata ent gl_AppName))
				(progn
					(setxdata ent gl_AppName (list (cons 1000 gl_Version)))
					(setq j (1+ j))
				)
			)	
		)
	)
	(prompt (strcat "\n设置Xdata：" (rtos j 2 0)))
	(princ)

)

;*********************************************************************************************
;函数定义:C:ReSetMapNames()
;功能：重新设置管线点的图上点名，但不修改物探点号
;参数：
;返回：
;创建时间：2015/4/1   24:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:ReSetMapNames( / allp clines el ent i j mapname mtype newname pos pos10 ptlist pts)
	(princ "\n重新设置图上点号。")
	(setq allp (GetTypedPointList))
	(if (listp allp)
		(foreach pts allp
			(setq mtype (car pts)	;;maintype
				ptlist (cadr pts)	;;insert entity list
				i 1)				;;name index
			(foreach ent ptlist
				(setq mapname (ldata-get ent "Map_No")
					pos (cdr (assoc 10 (entget ent)))
					clines (GetConnectEntity (car pos) (cadr pos) "LINE")
					newname (strcat mtype (rtos i 2 0))
					j 0
					i (1+ i)
				)
				;;设置新的点名
				(ldata-put ent "Map_No" newname)
				(SetAttrib-PointName ent "点号" newname)
				;;设置线段startpoint endpoint
				(repeat (length clines)
					(setq   el (nth j clines)
						pos10 (cdr (assoc 10 (entget el)))
					)
					(if (IsSameXY pos pos10);;pos10 is the same point
						(ldata-put el "Start_Point" newname)
						;;else
						(ldata-put el "End_Point" newname)
					)
					(setq j (1+ j))
				)	
			)	
		)
	)
	;;重新设置点名列表
	(setq gl_ExpNameList (GetExpNamesList)
		gl_MapNameList (GetMapNamesList))
	(princ "\n完成设置图上点号。")
    (princ)
)


;;删除同样位置的线段，和点。
(defun C:DelSameEntitys  (/ t1 delList e time2)
	; (if gl_DEBUG (setq gl_Time (getvar "TDUSRTIMER")))
    ; (setq AllBlockSet (ssget "X" (list (cons 0 "INSERT")))
          ; AllLineSet  (ssget "X" (list (cons 0 "LINE")))
          ; AllTextSet  (ssget "X" (list (cons 0 "TEXT"))))
    ; (setq dim  (Fun_InPutValue 0.0001 "USERR5" "\n删除相同的文字、图块、直线图元。若图形对象较多，可能费时较长。\n请输入位置精度：" 4))
    ; (DelSameEntity AllLineSet "LINE" dim)
    ; (DelSameEntity AllBlockSet "INSERT" dim)
    ; (DelSameEntity AllTextSet "TEXT" dim)
	; (if gl_DEBUG
		; (progn
		; (princ "\n删除相同图元共用时")
		; (princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
		; (princ "秒.")
		; )
	; )	
	(setq t1 (StartWatch))
	(setq delList (Index-GetSamePositionEntity (GetAllPointRoot T))
		delList (ListRemoveSameElement delList)
	)
	(foreach e delList
		 (DelEntity e)
	)
	(princ (strcat "\n删除" (rtos (length delList) 2 0) "个重复管线点对象,"))
	
	
	(setq delList (Index-GetSamePositionEntity (GetLineRoot T))
		delList (ListRemoveSameElement delList)
	)
	(foreach e delList
		 (DelEntity e)
	)
	(princ (strcat "\n删除" (rtos (length delList) 2 0) "个重复管线段对象,"))
	
	(setq time2 (TimeCosted "删除重复管线点" t1))
	
	(princ)
)

(defun DelSameEntity  (sset typestr dim /  delent dellist ent ent2 entdata entdata2 isame leftlist newlist p10 p11 p20 p21 s1 s2 startlist t1 t2 type1 type2)
    (setq newlist   nil
          dellist   nil
          startlist sset
          )
    (if sset
        (progn
            (repeat (- (sslength sset) 1)
                (setq isame    0 ;相同标志
                      p11      nil
                      p21      nil
                      t1       nil
                      t2       nil
                      s1       nil
                      s2       nil
                      ent      (ssname startlist 0)
                      leftlist (ssdel ent startlist)
                      entdata  (entget ent)
                      type1    (cdr (assoc 0 entdata))
                      p10      (cdr (assoc 10 entdata))
                      i 0
                      )
                (repeat   (sslength leftlist)
                    (setq ent2 (ssname leftlist i)
                        entdata2 (entget ent2)
                          type2    (cdr (assoc 0 entdata2))
                          p20      (cdr (assoc 10 entdata2))
                          )
                    (if (= type1 "LINE")
                        (setq p11 (cdr (assoc 11 entdata))))
                    (if (= type2 "LINE")
                        (setq p21 (cdr (assoc 11 entdata2))))
                    (if (= type1 "TEXT")
                        (setq t1 (cdr (assoc 1 entdata))))
                    (if (= type2 "TEXT")
                        (setq t2 (cdr (assoc 1 entdata2))))
                    (if (= type1 "INSERT")
                        (setq s1 (cdr (assoc 2 entdata))))
                    (if (= type2 "INSERT")
                        (setq s2 (cdr (assoc 2 entdata2))))

                    (if (and (= type1 type2) (/= nil p11)) ;line
                        (if (or (and (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                     (and (< (abs (- (car p11) (car p21))) dim) (< (abs (- (cadr p11) (cadr p21))) dim)));(and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                (and (and (< (abs (- (car p10) (car p21))) dim) (< (abs (- (cadr p10) (cadr p21))) dim))
                                     (and (< (abs (- (car p11) (car p20))) dim) (< (abs (- (cadr p11) (cadr p20))) dim))))
                            (setq isame (1+ isame))
                            ) ;if
                        ) ;if

                    (if (and (= type1 type2) (/= nil t1)) ;text
                        (if (and (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                 (= t1 t2))
                            (setq isame (1+ isame))
                            ) ;if
                        ) ;if

                    (if (and (= type1 type2) (/= nil s1)) ;insert
                        (if (and
                                (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                (= s1 s2))
                            (setq isame (1+ isame))
                            ) ;if
                        ) ;if
                    (setq i (1+ i))
                    ) ;repeat
                (if (= 0 isame) ;only one
                    (setq newlist (append newlist (list ent)))
                    )
                (if (>= isame 1) ;only one
                    (setq dellist (append dellist (list ent)))
                    )
                (setq startlist leftlist)
                ) ;foreach
            (setq newlist (append newlist (list (ssname startlist 0))))
            ) ;progn
        ) ;if
    (if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
                );foreach
            (prompt (strcat "\n删除了"
                            (rtos (length dellist) 2 0)
                            "个重复的"
                            typestr
                            "图元。"))
            ) ;progn
        (prompt (strcat "\n删除了0个重复的" typestr "图元。"))
        );if
		(princ)
    ) 
;;;	使用新的算法	
;;;选择一个点域，然后删除区域内的重复点，划分区块。
(defun DelSameEntity2 (dim / allblockset alllineset alltextset app delent dellist doc e1 ent
		entlist entlist2 i idel linelist mspace p10 p11 p20 p21 
			slen ss typestr)
	(if gl_DEBUG (setq gl_Time (getvar "TDUSRTIMER")))
    (setq AllBlockSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
          AllLineSet  (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName))))
          AllTextSet  (ssget "X" (list (cons 0 "TEXT"))))
	(setq app	 (vlax-get-acad-object)
		  Doc	 (vla-get-Activedocument app)
		  Mspace (vla-get-modelspace Doc)
	) ;_ End_setq
	
	(vla-ZoomAll app)
	
	(setq dellist nil);;需要删除的图元
	;;del point
	(setq i 0)
	(if AllBlockSet
		(repeat (sslength AllBlockSet)
			(setq ent (ssname AllBlockSet i)
				entlist (entget ent)
				p10 (cdr (assoc 10 entlist))
                              	P10 (list (car p10) (cadr p10))
				ss (ssget p10 (list (cons 0 "INSERT")))
				slen (sslength ss)
				idel 0
				i (1+ i)
			)
			(if (> slen 1)
				(repeat slen
					(setq e1 (ssname ss idel))
					(if (/= e1 ent)
						(if (not (vl-position e1 dellist))
							(setq dellist (cons e1 dellist))
						)
					)
					(setq idel (1+ idel))
				)
			)
		)
	)
	(setq typestr "INSERT")
	(if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
            );foreach
            (prompt (strcat "\n删除了"
                            (rtos (length dellist) 2 0)
                            "个重复的"
                            typestr
                            "图元。"))
        ) ;progn
        (prompt (strcat "\n删除了0个重复的" typestr "图元。"))
    );if
	(setq i 0
		dellist nil)
	(if AllTextSet
		(repeat (sslength AllTextSet)
			(setq ent (ssname AllTextSet i)
				entlist (entget ent)
				p10 (cdr (assoc 10 entlist))
                              P10 (list (car p10) (cadr p10))
				ss (ssget p10 (list (cons 0 "TEXT")))
				slen (sslength ss)
				idel 0
				i (1+ i)
			)
			(if (> slen 1)
				(repeat slen
					(setq e1 (ssname ss idel))
					(if (/= e1 ent)
						(if (not (vl-position e1 dellist))
							(setq dellist (cons e1 dellist))
						)
					)
					(setq idel (1+ idel))
				)
			)
		)
	)
	(setq typestr "TEXT")
	(if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
            );foreach
            (prompt (strcat "\n删除了"
                            (rtos (length dellist) 2 0)
                            "个重复的"
                            typestr
                            "图元。"))
        ) ;progn
        (prompt (strcat "\n删除了0个重复的" typestr "图元。"))
    );if
	(setq i 0
		linelist nil)
	(if AllLineSet
		(repeat (sslength AllLineSet)
			(setq ent (ssname AllLineSet i)
				entlist (entget ent)
				p10 (cdr (assoc 10 entlist))
                              P10 (list (car p10) (cadr p10))
				p11 (cdr (assoc 10 entlist))
                              P11 (list (car p11) (cadr p11))
				ss (ssget p10 (list (cons 0 "LINE")(list -3 (list gl_AppName))))
				slen (sslength ss)
				idel 0
				i (1+ i)
			)
			(if (> slen 1)
				(repeat slen
					(setq e1 (ssname ss idel))
					(if (/= e1 ent)
						(if (not (vl-position e1 dellist))
							(progn
								(setq entlist2 (entget e1)
									p20 (cdr (assoc 10 entlist2))
									p21 (cdr (assoc 11 entlist2))
								)
								(if (or (and (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
											 (and (< (abs (- (car p11) (car p21))) dim) (< (abs (- (cadr p11) (cadr p21))) dim)))
										(and (and (< (abs (- (car p10) (car p21))) dim) (< (abs (- (cadr p10) (cadr p21))) dim))
											 (and (< (abs (- (car p11) (car p20))) dim) (< (abs (- (cadr p11) (cadr p20))) dim))))
									(setq dellist (cons e1 dellist))
								) 
							)
						)
					)
					(setq idel (1+ idel))
				)
			)
		)
	)
	(setq typestr "LINE")
	(if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
            );foreach
            (prompt (strcat "\n删除了"
                            (rtos (length dellist) 2 0)
                            "个重复的"
                            typestr
                            "图元。"))
        ) ;progn
        (prompt (strcat "\n删除了0个重复的" typestr "图元。"))
    );if
	(if gl_DEBUG
		(progn
		(princ "\n删除相同图元共用时")
		(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
		(princ "秒.")
		)
	)	
	(princ)
)	
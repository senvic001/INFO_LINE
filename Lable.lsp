;;;标注对象，文字和图块
;;;数据结构：标注对象和原对象之间保存彼此的句柄

(setq gl_Type_Label_List nil) ;类型 标记字段名1，字段名2...
 ;*********************************************************************************************
 ;函数定义:ReadTypeLabelList()
 ;功能：读取参数配置文件中的[Type_Label]部分;配置为逗号分隔;
 ;参数：gl_Type_Label_List
 ;返回：((J 1 D_S Material Flowdirect))
 ;创建时间：2014/08/28   13:00
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun ReadTypeLabelList (path / typetablelist str file tmplist pos)
    (if (= nil path)
        (setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ End_if
    (setq typetablelist nil)
    (if (setq file (open path "r"))
        (progn
            (while (setq str (read-line file))
                (if (= str "[Type_Label]")
                    (while (/= "[/Type_Label]" (setq str (read-line file)))
                        (setq tmplist nil
                              str     (vl-string-left-trim " ," str)
                              str     (vl-string-right-trim " ," str)
                        ) ;_ End_setq
                        (while (setq pos (vl-string-search "," str))
                            (setq tmplist (append tmplist (list (substr str 1 pos)))
                                  str     (substr str (+ 2 pos))
                            ) ;_ End_setq
                        ) ;_ End_while
                        (setq tmplist (append tmplist (list str)))
                        (if (listp tmplist)
                            (setq typetablelist (append typetablelist (list tmplist)))
                        ) ;_ End_if
                    ) ;while
                ) ;if
            ) ;while
            (close file)
        ) ;progn
        ;;else
        (prompt (strcat "\nError！打开文件失败：" path))
    ) ;if
    typetablelist
) ;_ End_defun
 ;*********************************************************************************************
 ;函数定义:WriteTypeLabelList()
 ;功能：把标注信息写入配置文件
 ;参数：labellist 标注信息表  path 文件路径
 ;返回：
 ;创建时间：2015/01/17 14:00
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun WriteTypeLabelList(labellist path / tmpfile tmpfilestr e file  str n i outstr)
    (if	(= nil path)
	(setq path	 (strcat gl_BLOCKLIB_PATH "config.ini")
	      tmpfilestr (strcat gl_BLOCKLIB_PATH "lineinfotmp.dat")
	) ;_ end_setq
    ) ;_ End_if
    (vl-file-delete tmpfilestr)

    (if	(and (/= nil labellist) (setq file (open path "r")) (setq tmpfile (open tmpfilestr "w")))
	(progn
	    (while (setq str (read-line file))
		(write-line str tmpfile)
		(if (= str "[Type_Label]")
		    (progn
			(foreach e labellist
			    (setq n (length e)
					outstr ""
					i 0)
				(repeat n
					(setq outstr (strcat outstr "," (nth i e))
						i (1+ i)
					)
				)
				(write-line (vl-string-trim "," outstr) tmpfile)
			    
			) ;_ end_foreach
			(while (/= "[/Type_Label]" (setq str (read-line file)))
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
 ;***************************************************************  ******************************
 ;函数定义:AddLineTextLabel()
 ;功能：绘制文字标签，并加入字典 Key:Label,Data:图形对象的句柄列表
 ;参数：lent 直线段实体,depth1 起点深度  depth2 终点深度
 ;返回：
 ;创建时间：2014/08/28   13:00
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun AddLineTextLabel (lent       depth1     depth2     /          direction  field      i          label      labelfields           labelstr   ldata      linea      layername  linelen
                         linelist   linep1     linep2     ptext      ptext2     textlength textobj    tyname     d1         d2         blockobj   enttext    tpos       dist0      tstr
                         bnewtext   oldtextobj enttype
                        )
    ;;1 if lent has properties
    (setq tyname (ldata-get lent "Main_Type"))
    (if (setq label (ldata-get lent "Label"))
        (setq enttext (handent label))
    ) ;_ end_if
    (if enttext
        (setq oldtextobj (vlax-ename->vla-object enttext)
			enttype (cdr (assoc 0 (entget enttext))))
    ) ;_ end_if
	;;句柄有可能指向其他对象
	(if (/= enttype "TEXT") 
		(progn
			(setq oldtextobj nil)
			(ldata-put lent "Label" nil)
		)
	)	
    ;; length and angle of the line 
    (setq linelist  (entget lent)
          layername (cdr (assoc 8 linelist))
          linep1    (cdr (assoc 10 linelist))
          linep2    (cdr (assoc 11 linelist))
          linelen   (distance linep1 linep2)
          linea     (angle linep1 linep2)
          ptext     (list (/ (+ (car linep1) (car linep2)) 2)
                          (/ (+ (cadr linep1) (cadr linep2)) 2)
                          0
                    ) ;_ end_list
    ) ;_ end_setq
    (if (= nil gl_Type_Label_List)
        (setq gl_Type_Label_List (ReadTypeLabelList nil))
    ) ;_ end_if
    (if (= gl_TableColorList nil)
        (setq gl_TableColorList (ReadColorConfig nil))
    ) ;_ end_if

    (if (and (/= nil tyname) gl_Type_Label_List)
        (progn
            (setq labelfields (assoc tyname gl_Type_Label_List)
            ) ;_ end_setq
            (if labelfields
                ;;Draw a label
                (progn
                    (setq labelstr ""
                          i 0
                          d1 nil ;start_deep
                          d2 nil ;the end_deep
                          textobj nil
						  bshowlabel (atoi (nth 1 labelfields))
                    ) ;_ end_setq
					(if (and (= 0 bshowlabel) oldtextobj (= "TEXT" enttype))
						(progn
							(vla-delete oldtextobj)
							(setq oldtextobj nil)
						)
					)
					(if (= 1 bshowlabel)
						(progn
							(repeat (length labelfields)
								(setq field (nth i labelfields))

								(if (= field "TYPE")
									(setq labelstr (strcat labelstr (nth 2 (assoc tyname gl_TableColorList))))
								) ;类型
								(if (= field "D_S")
									(setq labelstr (strcat labelstr " DN" (ldata-get lent "D_S")))
								) ;管径
								(if (= field "LENGTH")
									(setq labelstr (strcat labelstr "-" (rtos linelen 2 1)))
								) ;长度
								(if (= field "Material")
									(setq labelstr (strcat labelstr " " (ldata-get lent "Material")))
								) ;_ end_if
								(if (= field "Pressure")
									(setq labelstr (strcat labelstr " " (ldata-get lent "Pressure")))
								) ;_ end_if
								(if (= field "Voltage")
									(setq labelstr (strcat labelstr " " (ldata-get lent "Voltage")))
								) ;_ end_if
								(if (= field "Start_Deep")
									(setq d1 (rtos depth1 2 2))
								) ;_ end_if
								(if (= field "End_Deep")
									(setq d2 (rtos depth2 2 2))
								) ;_ end_if
								(setq i (1+ i))
							) ;repeat

							(setq linea_t linea) ;label text angle
							(if (and (>= linea (* pi 0.5)) (< linea (* pi 1.5)))
								(setq linea_t (- linea pi))
							) ;text direction:left to right
							(if (and d1 d2)
								(if (= d1 d2)
									(setq labelstr (strcat labelstr " " d1 "m"))
									(if (= linea linea_t)
										(setq labelstr (strcat labelstr " " d1 "~" d2 "m"))
										(setq labelstr (strcat labelstr " " d2 "~" d1 "m"))
									) ;_ end_if
								) ;_ end_if
							) ;_ end_if

							(setq labelstr (vl-string-trim " " labelstr))

							(setq textlength (* (strlen labelstr) (/ 2.0 gl_MAP_SCALE))
								  ptext2     (polar (list (car ptext) (cadr ptext)) (+ linea_t (/ pi 2.0)) (/ 1.5 gl_MAP_SCALE))
							) ;_ end_setq

							(setq bnewtext nil)
							(if oldtextobj
								(progn
									(setq tpos  (ldata-get lent "Text_Pos")
										  tpos  (list (car tpos) (cadr tpos) (caddr tpos))
										  dist0 (distance tpos ptext2)
										  tstr  (vla-get-textstring oldtextobj)
									) ;_ end_setq
									(if (> dist0 (/ 5 gl_MAP_SCALE));;标注到线段种点的距离>5,重绘.
										(setq bnewtext T)
									) ;_ end_if
									(if (or (>= textlength linelen) (<= linelen 20)) ;;调整端点位置后，文字太长，删除标注。
										(vla-delete oldtextobj)
									)
									(if (/= tstr labelstr)
										(setq bnewtext T)
									) ;_ end_if
								) ;_ end_progn
								(setq bnewtext T) ;else enttext nil
							) ;_ end_if

							(if (and bnewtext (> linelen 20) (< textlength linelen)) ;文字长度小于线段长度,且线段长度大于20m
								(progn
									(setq textobj (vla-addtext (vla-get-modelspace
																   (vla-get-Activedocument (vlax-get-acad-object))
															   ) ;_ end_vla-get-modelspace
															   labelstr
															   (vlax-3D-point ptext2)
															   (/ 2.0 gl_MAP_SCALE)
												  ) ;_ end_vla-addtext
									) ;_ end_setq
									(if (and textobj oldtextobj)
										(vla-delete oldtextobj)
									) ;_ end_if
									(vla-put-layer textobj layername)
									(vla-put-alignment textobj acAlignmentMiddle)
									(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext2))
									(vla-put-Rotation textobj linea_t)
									;;attach the text handle to line
									(if (= nil label)
										(setq label "")
									) ;_ end_if
									(ldata-put lent "Label" (vla-get-handle textobj))
									(ldata-put lent "Text_Pos" (append ptext2 (list linea_t)))
									(ldata-put lent "M_Text" labelstr)
									(ldata-put textobj
													"LINE"
													(list (cdr (assoc 5 linelist)))
									) ;_ end_vlax-ldata-put
								) ;_ end_progn
							) ;if
						)
					)
                ) ;progn
            ) ;if
        ) ;progn
    ) ;if
) ;_ end_defun

 
 ;;删除文字标注
 ;;参数:线段实体名称
 (defun DelTextLabel(entl / label enttext oldtextobj)
	(if (setq label (ldata-get entl "Label"))
        (if (setq enttext (handent label))
			(if (setq oldtextobj (vlax-ename->vla-object enttext))
				(progn
					(vla-delete oldtextobj)
					(ldata-put entl "Label" nil)
					(ldata-put entl "Text_Pos" (list 0 0 0 0))
				)
			)
		)
    ) ;_ end_if
 )
 ;*********************************************************************************************
 ;函数定义:AddLineFlowdirect()
 ;功能：绘制流向,并把流向对象句柄保存在FlowBlock 中
 ;参数：lent 直线段实体
 ;返回：
 ;创建时间：2014/12/20  13:00
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun AddLineFlowdirect (lent / blockobj entf flowdirect fb layername linea linelist linemid
						  		linep1 linep2 tyname entobj etype entlist)
    (setq tyname (ldata-get lent "Main_Type"))
    (if (or (= tyname "G") (= tyname "P") (= tyname "Y") (= tyname "W"))
        (progn
            ;; length and angle of the line 
            (setq linelist   (entget lent)
                  layername  (cdr (assoc 8 linelist))
                  linep1     (cdr (assoc 10 linelist))
                  linep2     (cdr (assoc 11 linelist))
                  linea      (angle linep1 linep2)

                  Flowdirect (ldata-get lent "Flowdirect")
            ) ;_ end_setq
			;;del old flowdirection block
			(if (setq fb (ldata-get lent "FlowBlock"))
                (if (setq entf (handent fb))
					(progn
						(setq entlist (entget entf))
						(if (setq etype (cdr (assoc 0 entlist)))
							(if (= "INSERT" etype)
								(if (= "lx" (cdr (assoc 2 entlist)))
									(vla-delete  (vlax-ename->vla-object entf))
								)
							)
						
						)
					)	
                ) ;_ end_if
            ) ;_ end_if
			
			(if (= 0 Flowdirect)
			     (ldata-delete lent "FlowBlock" )
			)
            ;;绘制流向
            (setq blockobj nil
                  linea    (angle linep1 linep2)
                  linemid  (list (/ (+ (car linep1) (car linep2)) 2)
                                 (/ (+ (cadr linep1) (cadr linep2)) 2)
                                 (/ (+ (caddr linep1) (caddr linep2)) 2)
                           ) ;_ end_list
            ) ;_ end_setq
			
            (if (= 1 Flowdirect)
                (setq blockobj (Drawblock linemid linea "lx.dwg"))
            ) ;_ end_if
            (if (= 2 Flowdirect)
                (setq blockobj (Drawblock linemid (+ linea pi) "lx.dwg"))
            ) ;_ end_if
            (if blockobj
                (progn
                    (vla-put-layer blockobj layername)
		    		(ldata-put lent "FlowBlock" (vla-get-handle blockobj))
                ) ;_ end_progn
            ) ;_ end_if
        ) ;_ end_progn
    ) ;_ end_if
) ;_ end_defun

 ;*********************************************************************************************
 ;函数定义:C:SetShowlabel (ent)
 ;功能：设置是否显示文字标注
 ;参数：
 ;返回：
 ;创建时间：2015/01/17   10:35
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
 (defun C:SetShowlabel()
	(if (= nil gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	(setq typelist nil)
	(foreach e gl_TableColorList
		(setq typelist (cons (strcat (car e) "-" (caddr e)) typelist))
	)
	(setq typelist (reverse typelist))
	
	(print typelist)
	(setq types (Fun_InPutString "ALL"
									  "USERS1"
									  "\n输入要绘制的管线类型（多个类型用逗号','分隔，全部选择输入'All'）：")
		  )
 )
 ;*********************************************************************************************
 ;函数定义:UpdateLabel (ent)
 ;功能：当线段位置变化时,修改标注,包括流向和文字标注
 ;参数：线实体名
 ;返回：
 ;创建时间：2015/01/17   00:05
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
 (defun UpdateLabel(ent d1 d2 / deeps)
 ;;标注文字，显示内容由配置文件决定
	(if	(and (/= nil d1) (/= nil d2))
		(progn
			(AddLineTextLabel ent d1 d2)
			(AddLineFlowdirect ent)
		)
		(if (setq deeps (GetDeeps ent))
			(progn
				(AddLineTextLabel ent (car deeps) (cadr deeps))
				(AddLineFlowdirect ent)
			)
		)
	)
 )
 
(defun C:Addlabel (/ ent)
    (if (setq ent (car (entsel "\n添加标注，请选择管线段：")))
        (if (= "LINE" (cdr (assoc 0 (entget ent))))
            (AddLineTextLabel ent)
            (prompt "\n错误：选择的对象不是直线段。")
        ) ;_ end_if
    ) ;_ end_if
    (prin1)
) ;_ end_defun

;*********************************************************************************************
 ;函数定义:C:InsertOneHeightMark()
 ;功能：插入相连管线段的高程
 ;参数：
 ;返回：
 ;创建时间：2015/03/21   00:05
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
 (defun C:InsertOneHeightMark( / bpoint entlist ep1 sp typename )
	(prompt "\n插入单个管线点高程标注.\n")
	(setq ep1 nil 
		bpoint nil)
	(Addfont 'acstyle)
	(while (not bpoint)
		(if (setq ep1 (car (entsel "\n选择管线点:")))
			(progn
				(setq entlist (entget ep1)
					typename (cdr (assoc 0 entlist)))
				(if (and (= "INSERT" typename) (vlax-ldata-get ep1 gl_AppName))
					(setq bpoint T)
					(progn 
						(setq ep1 nil
							bpoint nil)
						(prompt "\n选择的对象不是管线点！请重新选择.")
					)
				)
			)
		)
	)
	(if ep1
		(if (setq sp (getpoint (cdr (assoc 10 (entget ep1))) (strcat "\n选择高程标注点的位置：")))
			(progn
				;;save sp
				(ldata-put ep1 "GC_X" (cadr sp))
				(ldata-put ep1 "GC_Y" (car sp))
				(AddOneHeightMark ep1)
			);;
		)
	)
	(princ)
 )
 ;*********************************************************************************************
 ;函数定义:C:InsertOneHeightMark()
 ;功能：插入相连管线段的高程
 ;参数：
 ;返回：
 ;创建时间：2015/03/21   00:05
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
 (defun C:InsertOneSHeightMark( / bpoint entlist ep1 sp typename )
	(prompt "\n插入多连通管线点的地表高程标注.\n")
	(setq ep1 nil 
		bpoint nil)
	(Addfont 'acstyle)
	(while (not bpoint)
		(if (setq ep1 (car (entsel "\n选择管线点:")))
			(progn
				(setq entlist (entget ep1)
					typename (cdr (assoc 0 entlist)))
				(if (and (= "INSERT" typename) (vlax-ldata-get ep1 gl_AppName))
					(setq bpoint T)
					(progn 
						(setq ep1 nil
							bpoint nil)
						(prompt "\n选择的对象不是管线点！请重新选择.")
					)
				)
			)
		)
	)
	(if ep1
		(if (setq sp (getpoint (cdr (assoc 10 (entget ep1))) (strcat "\n选择高程标注点的位置：")))
			(progn
				;;save sp
				(ldata-put ep1 "SGC_X" (cadr sp))
				(ldata-put ep1 "SGC_Y" (car sp))
				(AddOneSHeightMark ep1)
			);;
		)
	)
	(princ)
 )
 ;*********************************************************************************************
 ;函数定义:AddOneHeightMark(ep1)
 ;功能：插入相连管线段的高程
 ;参数：
 ;返回：
 ;创建时间：2015/03/21   00:05
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
 (defun AddOneHeightMark(ep1 / connectpoints depth depth1 depth2 depthe depths edge el 
	entlist ep2  height linea linea_t lineobj llength maintype midp newarray pent pl10 pl11 
	plineobj pos pos1 pose ptext sp subsidstr textobj typename etext)
	(if (and (setq gcx (ldata-get  ep1 "GC_X")) (setq gcy (ldata-get  ep1 "GC_Y")))
		(if (and (/= gcx 0) (/= gcy 0))	;;;BUG (0,0,0)无法自动插入标注
			(progn
				;;save sp
				(setq sp (list gcy gcx 0))
				;;add layer
				(setq maintype (ldata-get ep1 "Main_Type"))
				(Addlayer maintype (strcat maintype "_GC"))
				;;add line and gc text
				(setq pos (cdr (assoc 10 (entget ep1)))
					ConnectPoints	(GetConnectPoints (car pos) (cadr pos) "INSERT")
					edge nil
					llength (/ 11.0 gl_MAP_SCALE);;标注线长度5.5
				)
				(if	(listp ConnectPoints)
					(progn
						;;用多段线连接到插入点，不用直线。
						(setq newarray (vlax-make-safearray vlax-vbDouble (cons 0 3) )) ;_ End_vlax-make-safearray
						(vlax-safearray-fill newarray (list (car pos) (cadr pos) (car sp) (cadr sp))) ;_ End_vlax-safearray-fill
						(setq plineobj (vla-addlightweightpolyline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
																   (vlax-make-variant newarray))
						) ;_ End_vla-addlightweightpolyline
						(setq npoint (length ConnectPoints))
						(foreach pent  ConnectPoints
							;;add line
							(setq ep2 (car pent)
								pos1 (cdr (assoc 10 (entget ep2)))
								linea (angle pos pos1)	;;mark line angle
								pose (polar sp linea llength);;end point
								el (cadr pent)
								pl10 (cdr (assoc 10 (entget el)))
								pl11 (cdr (assoc 11 (entget el)))
								height 0;;mark height
							)
							(if (vlax-ldata-get el gl_AppName);;直线必须是管线段，与其他直线区分
								(progn
									
									;;and text
									(setq linea_t linea) ;label text angle
										(if (and (>= linea (* pi 0.5)) (< linea (* pi 1.5)))
											(setq linea_t (- linea pi))
										) ;text direction:left to right
									(if (and (setq depths (ldata-get el "Start_Deep")) (setq depthe (ldata-get el "End_Deep")));;the line has depth 
										;;depth in line
										(if (< (distance (list (car pos) (cadr pos) 0) (list (car pl10) (cadr pl10) 0)) gl_MIN);;same point
											(setq depth depths
												height (- (last pos) depth)
											)
											(setq depth depthe
												height (- (last pos) depth)
											)
										)
										;;else depth in point
										(if (and (setq depth1 (ldata-get ep1 "Depth")) (setq depth2 (ldata-get ep2 "Depth")))
											(progn
												(setq height (- (last pos) depth1))
												;;雨水篦和污水篦
												(if (setq subsidstr (ldata-get ep2 "Subsid"))
													(if (or (= subsidstr "雨水篦") (= subsidstr "污水篦"))
														(setq height (- (last pos1) depth2)
															height (- height 0.06));;高程低10cm内的随机数
													)
												)
												;;深度相差过大,2倍
												(if (> depth2 gl_MIN)
													(if (> (/ depth1 depth2) 2)
														(setq height (- (last pos1) depth2)
															height (- height 0.06)
														)
														(setq height (- (last pos) depth1))
													)
												)
											)
										)
									)
									(if (= 1 npoint)
										(progn
											
											(setq pose (polar sp 0 llength)
												midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
												ptext1 (polar (list (car midp) (cadr midp)) (* pi 0.5) (/ 1.5 gl_MAP_SCALE))
												ptext2 (polar (list (car midp) (cadr midp)) (* pi 1.5) (/ 1.5 gl_MAP_SCALE)))
											(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
													 (vlax-3D-point sp)
													 (vlax-3D-point pose)))	
											;;管底高程
											(setq textobj (vla-addtext (vla-get-modelspace
																		   (vla-get-Activedocument (vlax-get-acad-object))
																	   ) ;_ end_vla-get-modelspace
																	   (rtos height 2 3)
																	   (vlax-3D-point ptext2)
																	   (/ 2.0 gl_MAP_SCALE)
														  ) ;_ end_vla-addtext
											) ;_ end_setq
											(vla-put-alignment textobj acAlignmentMiddle)
											(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext2))
											;;地面高程
											(Addlayer maintype (strcat maintype "_SGC"))
											(setq textobj (vla-addtext (vla-get-modelspace
																		   (vla-get-Activedocument (vlax-get-acad-object))
																	   ) ;_ end_vla-get-modelspace
																	   (rtos (last pos) 2 3)
																	   (vlax-3D-point ptext1)
																	   (/ 2.0 gl_MAP_SCALE)
														  ) ;_ end_vla-addtext
											) ;_ end_setq
											(vla-put-alignment textobj acAlignmentMiddle)
											;(setq ecolor (vla-get-truecolor textobj))
											;(vla-put-colorindex ecolor acRed)
											;(vla-put-truecolor textobj ecolor)
											(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext1))
											
											(setq etext (vlax-vla-object->ename textobj))
											(ldata-put etext "SGC_X" (cadr pos))
											(ldata-put etext "SGC_Y" (car pos))
											
										)
										(progn
											(setq midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
												ptext (polar (list (car midp) (cadr midp)) (+ linea_t (/ pi 2.0)) (/ 1.5 gl_MAP_SCALE)))
											(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
													 (vlax-3D-point sp)
													 (vlax-3D-point pose)))
											(setq textobj (vla-addtext (vla-get-modelspace
																		   (vla-get-Activedocument (vlax-get-acad-object))
																	   ) ;_ end_vla-get-modelspace
																	   (rtos height 2 3)
																	   (vlax-3D-point ptext)
																	   (/ 2.0 gl_MAP_SCALE)
														  ) ;_ end_vla-addtext
											) ;_ end_setq
											(vla-put-alignment textobj acAlignmentMiddle)
											(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext))
											(vla-put-Rotation textobj linea_t)
										)
									)
									
									

								)
							)
						)
					)
					(progn
						(prompt "\n选择的管线点未与其他点相连，不能标注高程。")
						(exit)
					)
				)
		
			)
		)	
	)
 )
 
  ;*********************************************************************************************
 ;函数定义:C:ReAddHeightMark()
 ;功能：根据插入点属性重新标注
 ;参数：
 ;返回：
 ;创建时间：2015/03/22  00:05
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
 (defun C:ReAddHeightMark( / allp entp gcx i np)
	(prompt "\n根据插入点属性重新标注管线点高程。")
	(Addfont 'acstyle)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0
			)
			(repeat np
				(setq entp (ssname allp i)
					i (1+ i)
				)
				(if (and  (setq gcx (ldata-get entp "GC_X")) (setq gcx (ldata-get entp "GC_X")))
					(AddOneHeightMark entp)
				)
				(if (and  (setq gcx (ldata-get entp "SGC_X")) (setq gcx (ldata-get entp "SGC_X")))
					(AddOneSHeightMark entp)
				)
			)
		)
	)
	(princ)
 )
 
  (defun C:ContinueAddHeightMark( /  allp app con entp  i np p0 pmax pmin sp gcx gcy)
	(prompt "\n连续标注管线点管底高程。")
	(Addfont 'acstyle)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0
				con "Y"
				app	 (vlax-get-acad-object)
			)
			(while (and (< i np) (= con "Y"))
				(setq entp (ssname allp i)
					i (1+ i)
					p0 (cdr (assoc 10 (entget entp)))
					p0 (list (car p0) (cadr p0) 0)
					pmin (polar p0 (* 1.25 pi) 30 )
					pmax (polar p0 (* 0.25 pi) 30)
					gcx (ldata-get entp "GC_X")
					gcy (ldata-get entp "GC_Y")
				)
				(if (or (= nil gcx) (= nil gcy))
					(progn
						(vla-zoomwindow app (vlax-3D-point pmin) (vlax-3D-point pmax))
						(if (setq sp (getpoint p0 (strcat "\n选择高程标注点的位置：")))
							(progn
								;;save sp
								(ldata-put entp "GC_X" (cadr sp))
								(ldata-put entp "GC_Y" (car sp))
								(AddOneHeightMark entp)
							)
						);;(defaultvalue useri promotstr / tmpvalue)
						(if (setq con (Fun_InPutString "Y" "USERS1" "继续标注(N/Y):"))
							(setq con (strcase con))
						)
					)	
				)	
			)
			(prompt (strcat "\n标注了" (rtos i 2 0) "个管线点高程。")) 
		)
	)
	(princ)
 )
 
 ;;标注地面高程
  (defun C:ContinueAddSHeightMark( /  allp app con entp  i np p0 pmax pmin sp gcx gcy)
	(prompt "\n连续标注管线点地面高程。")
	(Addfont 'acstyle)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0
				con "Y"
				app	 (vlax-get-acad-object)
			)
			(while (and (< i np) (= con "Y"))
				(setq entp (ssname allp i)
					i (1+ i)
					p0 (cdr (assoc 10 (entget entp)))
					p0 (list (car p0) (cadr p0) 0)
					pmin (polar p0 (* 1.25 pi) 30 )
					pmax (polar p0 (* 0.25 pi) 30)
					gcx (ldata-get entp "SGC_X")
					gcy (ldata-get entp "SGC_Y")
				)
				(if (or (= nil gcx) (= nil gcy))
					(progn
						(vla-zoomwindow app (vlax-3D-point pmin) (vlax-3D-point pmax))
						(if (setq sp (getpoint p0 (strcat "\n选择地面高程标注点的位置：")))
							(progn
								;;save sp
								(ldata-put entp "SGC_X" (cadr sp))
								(ldata-put entp "SGC_Y" (car sp))
								(AddOneSHeightMark entp)
							)
						);;(defaultvalue useri promotstr / tmpvalue)
						(if (setq con (Fun_InPutString "Y" "USERS1" "继续标注(N/Y):"))
							(setq con (strcase con))
						)
					)	
				)	
			)
			(prompt (strcat "\n标注了" (rtos i 2 0) "个管线点地表高程。")) 
		)
	)
	(princ)
 )
 
 (defun AddOneSHeightMark(ep1 / connectpoints edge el gcsx gcsy gcx gcy  height lineobj llength maintype 
				midp newarray npoint plineobj pos pose ptext sp textob)
	(if (and (setq gcsx (ldata-get  ep1 "SGC_X")) (setq gcsy (ldata-get  ep1 "SGC_Y")))
		(if (and (/= gcsx 0) (/= gcsy 0))	;;;BUG (0,0,0)无法自动插入标注
			(progn
				;;save sp
				(setq sp (list gcsy gcsx 0))
				;;add layer
				(setq maintype (ldata-get ep1 "Main_Type"))
				(Addlayer maintype (strcat maintype "_SGC"))
				;;add line and gc text
				(setq pos (cdr (assoc 10 (entget ep1)))
					ConnectPoints	(GetConnectPoints (car pos) (cadr pos) "INSERT")
					edge nil
					llength (/ 11.0 gl_MAP_SCALE);;标注线长度5.5
					npoint 0
				)
				(if	(listp ConnectPoints)
					(progn
						(setq npoint (length ConnectPoints))
						(if (and (= 1 npoint) (setq gcx (ldata-get  ep1 "GC_X")) (setq gcy (ldata-get  ep1 "GC_Y")))
							(setq sp (list gcy gcx 0))
						)
					)	
				)
				;;用多段线连接到插入点，不用直线。
				(setq newarray (vlax-make-safearray vlax-vbDouble (cons 0 3) )) ;_ End_vlax-make-safearray
				(vlax-safearray-fill newarray (list (car pos) (cadr pos) (car sp) (cadr sp))) ;_ End_vlax-safearray-fill
				(setq plineobj (vla-addlightweightpolyline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
														   (vlax-make-variant newarray))
				) ;_ End_vla-addlightweightpolyline
			
				;(foreach pent  ConnectPoints
					;;add line
				(setq pose (polar sp 0 llength);;end point
					height (last pos);;mark height
				)
				(if (vlax-ldata-get ep1 gl_AppName);;直线必须是管线段，与其他直线区分
					(if (> npoint 1 )
						(progn
							(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
												 (vlax-3D-point sp)
												 (vlax-3D-point pose)))
							;;and text
							
							(setq midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
								ptext (polar (list (car midp) (cadr midp)) (* pi 0.5) (/ 1.5 gl_MAP_SCALE)))
							(setq textobj (vla-addtext (vla-get-modelspace
														   (vla-get-Activedocument (vlax-get-acad-object))
													   ) ;_ end_vla-get-modelspace
													   (rtos (last pos) 2 3)
													   (vlax-3D-point ptext)
													   (/ 2.0 gl_MAP_SCALE)
										  ) ;_ end_vla-addtext
							) ;_ end_setq
							(vla-put-alignment textobj acAlignmentMiddle)
							(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext))
						)
						; (progn
							; (setq pose (polar sp 0 llength)
								; midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
								; ptext1 (polar (list (car midp) (cadr midp)) (* pi 0.5) (/ 1.5 gl_MAP_SCALE))
								; ptext2 (polar (list (car midp) (cadr midp)) (* pi 1.5) (/ 1.5 gl_MAP_SCALE)))
							
							; ;;地面高程
							
							; (setq textobj (vla-addtext (vla-get-modelspace
														   ; (vla-get-Activedocument (vlax-get-acad-object))
													   ; ) ;_ end_vla-get-modelspace
													   ; (rtos (last pos) 2 3)
													   ; (vlax-3D-point ptext1)
													   ; (/ 2.0 gl_MAP_SCALE)
										  ; ) ;_ end_vla-addtext
							; ) ;_ end_setq
							; (vla-put-alignment textobj acAlignmentMiddle)
							; ;(setq ecolor (vla-get-truecolor textobj))
							; ;(vla-put-colorindex ecolor acRed)
							; ;(vla-put-truecolor textobj ecolor)
							; (vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext1))
							
							; (setq etext (vlax-vla-object->ename textobj))
							; (ldata-put etext "SGC_X" (cadr pos))
							; (ldata-put etext "SGC_Y" (car pos))
							; ;;管底高程
							; (Addlayer maintype (strcat maintype "_GC"))
							; (setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
									 ; (vlax-3D-point sp)
									 ; (vlax-3D-point pose)))	
							; (setq textobj (vla-addtext (vla-get-modelspace
														   ; (vla-get-Activedocument (vlax-get-acad-object))
													   ; ) ;_ end_vla-get-modelspace
													   ; (rtos height 2 3)
													   ; (vlax-3D-point ptext2)
													   ; (/ 2.0 gl_MAP_SCALE)
										  ; ) ;_ end_vla-addtext
							; ) ;_ end_setq
							; (vla-put-alignment textobj acAlignmentMiddle)
							; (vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext2))
							; (ldata-put etext "GC_X" (cadr pos))
							; (ldata-put etext "GC_Y" (car pos))
							
						; )
					)
				)
			)
		)	
	)
 )
 
 ;;清除高程标注属性
 (defun C:ClearGCData( / bpoint entlist ep1 i j pset typename)
	(prompt "\n清除单个管线点高程标注属性，但不删除标注的管底或管顶高程文字.\n请选择需要清除高程属性的管线点：")
	(setq ep1 nil 
		bpoint nil
		i 0 j 0)
	
	(if (setq pset (ssget))
		(repeat (sslength pset)
			(setq ep1 (ssname pset i)
				i (1+ i))
			(setq entlist (entget ep1)
				typename (cdr (assoc 0 entlist)))
			(if (and (= "INSERT" typename) (vlax-ldata-get ep1 gl_AppName))
				(progn
					(ldata-delete ep1 "GC_X")
					(ldata-delete ep1 "GC_Y")
					(ldata-delete ep1 "SGC_X")
					(ldata-delete ep1 "SGC_Y")
					(setq j (1+ j))
				)
			)
		)
	)
	(prompt (strcat "\n清除了" (rtos j 2 0) "个管线点的高程属性。"))
	(princ)
 )

;;;对话框内容函数
;;;两个点实体，可以为text或insert
;*********************************************************************************************
;函数定义:DraftDlg()
;功能：点属性编辑对话框
;参数：pent1 起点,pent2 连接点 entl 线段,mode 模式(1-编辑 2-错误修复 )
;创建时间：2014/12/01   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun DraftDlg (pent1 pent2 entl mode / deep1 deep2 Main_Type typename Exp_No Depth1 Depth2 feature subsid t_QuickInsert
	PointSize1 Exp_No2 feature2 subsid2 Depth2 PointSize2 D_S Flowdirect Pressure Voltage Material
	 Cab_Count Hole_Count Hole_Used Road_Name PaiWu unit date place subproject project 
	 subsidlist subsidstr subsidstr2 featurelist featurestr featurestr2 x0 x1 x2 y0 y1 y2 p0 p1 p2 p10 p11)
	;; 从控件得到的值  
	(defun draft_getdata  ()      
		(setq
			;;;p1
			Main_Type (get_tile "Main_Type")
			typename (car (nth (atoi Main_Type) gl_TableColorList))
			
			Exp_No     (get_tile "Exp_No")
			Depth1 (atof (get_tile "Depth1"))	
			feature    (get_tile "feature")
			subsid     (get_tile "subsid")
			t_QuickInsert (get_tile "t_QuickInsert")
			Point_Size1	(get_tile "text-PointSize1")
			
			;;;P2
			Exp_No2    (get_tile "LinkPoint")
			feature2   (get_tile "feature2")
			subsid2    (get_tile "subsid2")
			Depth2   (atof (get_tile "Depth2"))
			Point_Size2	(get_tile "text-PointSize2")
			;;;line 
			D_S        (get_tile "D_S")
			Flowdirect (atoi (get_tile "Flowdirect"))
			Pressure   (get_tile "Pressure")
			Voltage    (get_tile "Voltage")
			Material   (get_tile "Material")
			Cab_Count  (get_tile "Cab_Count")
			Hole_Used   (get_tile "Hole_Used")
			Hole_Count   (get_tile "Hole_Count")
			Road_Name	(get_tile "Road_Name")
			PaiWu	(get_tile "PaiWu")

			;;;project
			unit       (get_tile "unit")
			date       (get_tile "date")
			place      (get_tile "place")
			subproject (get_tile "subproject")
			project    (get_tile "project")
			)
		
		(if subsidlist
			(setq subsidstr (nth (atoi subsid) subsidlist)
				  subsidstr2 (nth (atoi subsid2) subsidlist))
			)
		(if featurelist
			(setq featurestr (nth (atoi feature) featurelist)
				  featurestr2 (nth (atoi feature2) featurelist))
			)
	) ;end_draft_getdata


	;;编辑点号时,查询是否是唯一点号
	(defun OnEditPointName (val reason pname / strname subtypename newname)
		(if (and (= 2 reason) (/= pname val))
			(progn
				;;is the uniqu No.?
				(if (HasSameExpName val)
					(progn
						(setq newname (CreateNewPointName val typename "EXP")) ;;typename global var,大类名称
						(alert (strcat "管线点" val "已经存在!\n建议新的点号为：" newname))
						(mode_tile "Exp_No" 2)
					)
				)
				;;subtype 是否存在
				(if	(null gl_SubList)
					(setq gl_SubList (ReadSubsymbolConfig nil))
				) ;if
				;;使用子类型判断类型
				(setq strName  (vl-string-trim "0123456789-_+." val) ;去掉右边的数字序号
					subtypename (assoc strName gl_SubList)
				) ;_ end_setq
				(if (= nil subtypename)
					(progn
						(alert (strcat "错误!管线点名前缀" strname "不在配置文件中,不能使用!\n若要使用,请先修改配置文件[Sub_Type]部分,并重新打开当前文件."))
						(mode_tile "Exp_No" 2)
					)
				)
			)
			
		)
	)
	 

	
	;;选择点类型时，修改subsid内容
	(defun OnSelectPointType  ( / typenum)
		(setq typenum (atoi (get_tile "Main_Type")))
		(if (/= nil typelist)
			(setq typename   (substr (nth typenum typelist) 1 1) ;第一个字符为类型
				  subsidlist (getsubsidlist-fromtype typename)
				  )
			) ;if

		;;设置subsid poplist内容
		(start_list "subsid" 3)
		(mapcar 'add_list subsidlist)
		(end_list)
		(set_tile "subsid" "0")
	  
		(start_list "subsid2" 3)
		(mapcar 'add_list subsidlist)
		(end_list)
		(set_tile "subsid2" "0")
		
		;;设置按钮状态
		;;雨水
		(if (= typename "Y")
			(progn
				(setq PaiWu "0")
				(set_tile "PaiWu" PaiWu)
				(mode_tile "PaiWu" 0)
			)
			(progn
				(setq PaiWu "2")
				(set_tile "PaiWu" PaiWu)
				(mode_tile "PaiWu" 1)
			)
		)
		;;流向
		(if (or (= typename "G") (= typename "P") (= typename "Y") (= typename "W") (= typename "R"))
			(mode_tile "Flowdirect" 0)
			(mode_tile "Flowdirect" 1)
		)
		;;电力
		(if (or (= typename "D") (= typename "L") (= typename "X") )
			(progn
				(mode_tile "Cab_Count" 0)
				(mode_tile "Hole_Used" 0)
				(mode_tile "Hole_Count" 0)
			)
			(progn
				(mode_tile "Cab_Count" 1)
				(mode_tile "Hole_Used" 1)
				(mode_tile "Hole_Count" 1)
			)
		)
		;;电压
		(if (or (= typename "D") (= typename "L") )
			(mode_tile "Voltage" 0)
			(mode_tile "Voltage" 1)
		)
		;;压力
		(if (or (= typename "Q") (= typename "G") )
			(mode_tile "Pressure" 0)
			(mode_tile "Pressure" 1)
		)
	) ;

	;;;从相邻点个数和点的类别，自动设置featurestr
	(defun GetFeaturestr  (count typename / str)
		(cond
			;((= count 1) (setq str "直线点"))
			((= count 2) (setq str "转折点"))
			((= count 3)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "三分支")
				 (setq str "三通")))
			((= count 4)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "四分支")
				 (setq str "四通")))
			((= count 5)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "五分支")
				 (setq str "五通")))
			((= count 6)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "六分支")
				 (setq str "六通")))
			((> count 6)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "多分支")
				 (setq str "多通")))
			(T (setq str nil))
			) ;cond
		str
		) ;defun

		;;保存对话框中的点线对象
		;;
		;;
		(defun SaveDraftDlg (projectinfo / sstr p10 p11 strf strs mn en ent bUpdate newinfo strname olddata newdata deep1 deep2)
			(if (= nil entP1)
				(progn
					(if (/= "0" subsid) ;;无附属物
						(setq sstr subsidstr)
						(setq sstr featurestr)
					)
					(setq entP1 (AddNewPoint (list x1 y1 z1) Exp_No  typename featurestr subsidstr (GetPoint1AttribsList))
						prePointName Exp_No)
				)
				;else 设置点属性,若符号改变,则创建新点
				(progn
					(setq strf (ldata-get entP1 "Feature")
						strs (ldata-get entP1 "Subsid"))
					(if (or (/= strf featurestr) (/= strs subsidstr))
						(if (setq ent (AddNewPoint (list x1 y1 z1) Exp_No  typename featurestr subsidstr (GetPoint1AttribsList)))
							(progn
								(vla-delete (vlax-ename->vla-object entP1))
								(setq entP1 ent)
							)
						)
						(PutEntityAttribs entP1 (GetPoint1AttribsList))
					)
				)
			)
			;;p2
			(if (/= nil entP2)
				(progn
					(setq strf (ldata-get entP2 "Feature")
						strs (ldata-get entP2 "Subsid"))
					(if (or (/= strf featurestr2) (/= strs subsidstr2))
						(if (setq ent (AddNewPoint (list x2 y2 z2) Exp_No2  typename featurestr2 subsidstr2 (GetPoint2AttribsList)))
							(progn
								(vla-delete (vlax-ename->vla-object entP2))
								(setq entP2 ent)
							)
						)
						(PutEntityAttribs entP2 (GetPoint2AttribsList))
					)
				)
			)
			;;创建线段
			(if (and (= nil entLine) entP1 entP2)
				(setq p11 (cdr (assoc 10 (entget entP2)))
					p10 (cdr (assoc 10 (entget entP1)))
					p11 (list (car p11) (cadr p11) (- (last p11) (ldata-get entP2 "Depth")))
					p10 (list (car p10) (cadr p10) (- (last p10) (ldata-get entP1 "Depth")))
					entLine (AddNewLine p10 p11 typename (GetLineAttribsList))
				)
			)
			(if entLine 
				(progn
					(PutEntityAttribs entLine (GetLineAttribsList))
					; (AddLineTextLabel entLine (ldata-get entP1 "Depth") (ldata-get entP2 "Depth"))
					; (AddLineFlowdirect entLine)
					(if (not (setq deep1 (ldata-get entLine "Start_Deep"))) 
						(setq deep1 (ldata-get entP1 "Depth")))
					(if (not (setq deep2 (ldata-get entLine "End_Deep"))) 
						(setq deep2 (ldata-get entP2 "Depth")))
					(UpdateLabel entLine deep1 deep2)

                                    ;;更新项目信息
					(setq bUpdate nil)
					(if (= nil projectinfo) (setq projectinfo (ReadProjectInfo nil)))
					(setq  newinfo (GetLineAttribsList))
					(foreach e projectinfo
						(setq strname (car e)
							olddata (cdr e)
							newdata (cdr (assoc strname newinfo))
						)
						(if (and (> (strlen newdata) 0)(/= olddata newdata))
							(setq projectinfo (subst (cons strname newdata) e projectinfo)
								bUpdate T)
						)
					)
					(if bUpdate (WriteProjectInfo projectinfo nil))
				)
			)
			
			
		)

	;;
	;;根据拾取的坐标初始化草图对话框
	;;参数:Main_Type typename 全局变量
	;; pent1 pent2 entl 对话框上的两点一线
	(defun InitDraftDlg  (prePointName pent1 pent2 entl / newname e entlist enttype eobj ldata pent pos tmp)
		;;查找P1的相关点
		;(setq gl_connectpoints nil	;与p1关联的点表((p1 Line1) (p2 Line2)...) or nil
		;);
		(setq typelist nil
			featurelist nil
			subsidlist nil)
		(if pent1
			(progn
				(setq ;;p1
					typename  (ldata-get pent1 "Main_Type")
					Exp_No (ldata-get pent1 "Exp_No")
					Depth1 (ldata-get pent1 "Depth")		
					featurestr (ldata-get pent1 "Feature")
					subsidstr (ldata-get pent1 "Subsid")
					Point_Size1 (ldata-get pent1 "Point_Size")
					p1 (cdr (assoc 10 (entget pent1)))
					x1 (car p1)
					y1 (cadr p1)
					z1 (caddr p1)
				)
				(if (> (strlen (ldata-get pent1 "Project")) 0)
					(setq project (ldata-get pent1 "Project"))
				)
				(if (> (strlen (ldata-get pent1 "SProject")) 0)
					(setq subproject (ldata-get pent1 "SProject"))
				)
				; (if (> (strlen (ldata-get pent1 "Sur_Date")) 0)
					; (setq date (ldata-get pent1 "Sur_Date"))
				; )
				(if (> (strlen (ldata-get pent1 "Unit")) 0)
					(setq unit (ldata-get pent1 "Unit"))
				)
				(if (> (strlen (ldata-get pent1 "Location")) 0)
					(setq place (ldata-get pent1 "Location"))
				)
			)
		)
		(if pent2
			(progn
				(setq 
					typename  (ldata-get pent2 "Main_Type")
					Exp_No2 (ldata-get pent2 "Exp_No")
					Depth2 (ldata-get pent2 "Depth")
					featurestr2 (ldata-get pent2 "Feature")
					subsidstr2 (ldata-get pent2 "Subsid")
					Point_Size2 (ldata-get pent2 "Point_Size")
					p2 (cdr (assoc 10 (entget pent2)))
					x2 (car p2)
					y2 (cadr p2)
					z2 (caddr p2)
				)
				(if (> (strlen (ldata-get pent2 "Project")) 0)
					(setq project (ldata-get pent2 "Project"))
				)
				(if (> (strlen (ldata-get pent2 "SProject")) 0)
					(setq subproject (ldata-get pent2 "SProject"))
				)
				; (if (> (strlen (ldata-get pent2 "Sur_Date")) 0)
					; (setq date (ldata-get pent2 "Sur_Date"))
				; )
				(if (> (strlen (ldata-get pent2 "Unit")) 0)
					(setq unit (ldata-get pent2 "Unit"))
				)
				(if (> (strlen (ldata-get pent2 "Location")) 0)
					(setq place (ldata-get pent2 "Location"))
				)
			)
			
		)
		(if entl
			(progn
				(setq 
					D_S        (ldata-get entl "D_S")
					Flowdirect (ldata-get entl "Flowdirect")
					Pressure   (ldata-get entl "Pressure")
					Voltage    (ldata-get entl "Voltage")
					Material   (ldata-get entl "Material")
					Cab_Count  (ldata-get entl "Cab_Count")
					Hole_Used   (ldata-get entl "Hole_Used")
					Hole_Count   (ldata-get entl "Hole_Count")
					Road_Name (ldata-get entl "Road_Name")
					p10 (cdr (assoc 10 (entget entl)))
					p11 (cdr (assoc 11 (entget entl)))
				)
				;;兼容：深度保存在点中，深度保存在线中
				(setq tmps (ldata-get entl "Start_Deep")
					tmpe (ldata-get entl "Start_Deep"))
				(if (and  tmps tmpe)
					(progn
						(cond
							( pent1
								(if (= Exp_No (ldata-get entl "Start_Point"))
									(setq Depth1 tmps
										Depth2 tmpe
									)
									(setq Depth1 tmpe
										Depth2 tmps
									)
								)
							)
							( pent2
								(if (= Exp_No2 (ldata-get entl "Start_Point"))
									(setq Depth2 tmps
										Depth1 tmpe
									)
									(setq Depth2 tmpe
										Depth1 tmps
									)
								)
							)
							(T
								(setq Depth2 tmpe
										Depth1 tmps
									)
							)
						)
					)
				)
				
				(if (setq tmp (ldata-get entl "PaiWu"))
						(cond 
							((= tmp "是")(setq PaiWu "0"))
							((= tmp "否") (setq PaiWu "1"))
							((= tmp "无") (setq PaiWu "2"))
							( T (setq PaiWu "2"))
						)
					)
				(if (> (strlen (ldata-get entl "Project")) 0)
					(setq project (ldata-get entl "Project"))
				)
				(if (> (strlen (ldata-get entl "SProject")) 0)
					(setq subproject (ldata-get entl "SProject"))
				)
				(if (> (strlen (ldata-get entl "Sur_Date")) 0)
					(setq date (ldata-get entl "Sur_Date"))
				)
				(if (> (strlen (ldata-get entl "Unit")) 0)
					(setq unit (ldata-get entl "Unit"))
				)
				(if (> (strlen (ldata-get entl "Location")) 0)
					(setq place (ldata-get entl "Location"))
				)
			)
		)
		
		;;根据前一个点号,自动获得新的点号
		(if (= nil pent1)
			(if (= nil typename)
				(setq Exp_No (CreateNewPointName prePointName "J" "EXP"))
				(setq Exp_No (CreateNewPointName prePointName typename "EXP"))
			)
		)
		;;2初始化列表值,
		(if (= gl_TableColorList nil)
			(setq gl_TableColorList (ReadColorConfig nil))
			) ;if
		(foreach e  gl_TableColorList
			(setq typelist (append typelist (list (strcat (car e) "-" (caddr e)))))
			) ;foreach
			
		;;设置Main_Type
		(if (and (= nil pent1) (= nil pent2))
			(if prePointName (setq typename (GetTypeFromPointName prePointName)))
		)
		(if (setq e (assoc typename gl_TableColorList))
			(if (setq pos (vl-position e gl_TableColorList))
				(setq Main_Type (rtos (vl-position e gl_TableColorList) 2 0))
				) ;if
			) ;if

		
		;;设置feature1
		(setq featurelist (getfeaturelist-fromtype typename))
		(if (setq pos (vl-position featurestr featurelist))
			(setq feature (rtos pos 2 0))) ;if

		;;设置subsid1
		(setq subsidlist (getsubsidlist-fromtype typename))
		(if (/= 0 (strlen subsidstr))
			(if (setq pos (vl-position subsidstr subsidlist))
				(setq subsid (rtos pos 2 0))
				) ;if
		) ;if
		;;设置点2
		(if (setq pos (vl-position featurestr2 featurelist))
			(setq feature2 (rtos pos 2 0))) ;if
		(if (/= 0 (strlen subsidstr2))
			(if (setq pos (vl-position subsidstr2 subsidlist))
				(setq subsid2 (rtos pos 2 0))
			) ;if
		) ;if	
		
	) ;defun
	;; ---------------------------end of functions define----------------------------------
	;;-------------------------------------------------------------------------------------
	;;-----beginning of dlg code-----------------------------------------------------------
    (LineInfo_GetSupportPath)
	;;;1选择P1初始点，可以是TEXT 或INSERT
    ;;全局变量 两个点实体和线实体
    (setq entP1 pent1
          entP2 pent2
          entLine entl)
	;;2-2设置控件参数默认值
	(setq ;;p1
	  typename  "J"
	  Exp_No ""
	  Depth1 0			
	  featurestr ""
	  feature "0"
	  subsidstr ""
	  subsid "0"
	  Point_Size1 ""
	  
	  x1 0.0
	  y1 0.0
	  z1 0.0
	  p1 (list 0 0 0)
	  ;;p2
	  Exp_No2 ""
	  Depth2 0;
	  featurestr2 ""
	  feature2 "0"
	  subsid2 "0"
	  subsidstr2 ""
	  Point_Size2 ""
	  x2 0.0
	  y2 0.0
	  z2 0.0
	  p2 (list 0 0 0)
	  ;;project
	  unit ""
	  date ""
	  place ""
	  subproject ""
	  project ""
	  ;;line
	  D_S        ""
	  Flowdirect 0
	  Pressure   ""
	  Voltage    ""
	  Material   "0"
	  Cab_Count  ""
	  Hole_Used   ""
	  Hole_Count   ""
	  Road_Name ""
	  PaiWu "2"
	  ;;
	  featurelist nil
	  typelist nil
	  subsidlist (list "无")
	 )
    ;;设置项目信息的默认值
	(if (setq ProjectInfoList (ReadProjectInfo nil))
		(setq unit (cdr (assoc "Unit" ProjectInfoList))
			date (cdr (assoc "Sur_Date" ProjectInfoList))
			place (cdr (assoc "Location" ProjectInfoList))
			subproject (cdr (assoc "SProject" ProjectInfoList))
			project	(cdr (assoc "Project" ProjectInfoList))
		)
	)
	; ;; x1 x2 y1 y2 p1 p2 全局变量
	(if (= nil entP1)
		(progn
			(if (= nil (setq p1 (getpoint "\n拾取点:")))
				(exit))
			(setq p1 (SetPrecisionPoint p1 4))
			(setq x1 (car p1) y1 (cadr p1) z1 (caddr p1))	
		)
		(setq p1 (cdr (assoc 10 (entget entP1)))
			x1 (car p1)
			y1 (cadr p1)
			z1 (caddr p1))
	)
	; (if entP2
		; (setq p2 (cdr (assoc 10 (entget entP2)))
			; x2 (car p2)
			; y2 (cadr p2)
		; )
	; )
    (InitDraftDlg prePointName entP1 entP2 entLine)

    (if (and entP1 entP2)
        (ZoomWndtoLeft p1 p2)
    ) ;

    (setq id (load_dialog (strcat gl_INFO_LINE_PATH "dlg\\draftpoint.dcl")
             ) ;_ End_load_dialog
    ) ;装入对话框文件
    (if (< id 0)
        (exit)
    ) ;_ End_if
  
	;;对话框显示位置：右半侧
    (setq app       (vlax-get-acad-object)
          width     (vla-get-width app)
          height    (vla-get-height app)
    ) ;_ End_setq
    
    ;;522*620,自己量出来的
    (setq DLG_WIDTH 455
          DLG_HEIGHT 543
          windpoint (list (- width (+ 10 DLG_WIDTH)) 30)
        )
  
    (setq std 6) ;0 cancel 1 ok 2 di-biao 3 shejixian
    (while (> std 1)
        (if (not (new_dialog "draftpointdlg" id "" windpoint))
            (exit)
        ) ;初始化对话框

        ;;设置typepoint poplist内容:J-给水
        (if (/= typelist nil)
            (progn
                (start_list "Main_Type" 3)
                (mapcar 'add_list typelist)
                (end_list)
            ) ;progn
        ) ;if


        ;;设置feature poplist内容    
        (start_list "feature" 3 )
        (mapcar 'add_list featurelist)
        (end_list)
        ;;设置subsid poplist内容
        (start_list "subsid" 3 )
        (mapcar 'add_list subsidlist)
        (end_list)
        
		(start_list "feature2" 3 )
		(mapcar 'add_list featurelist)
		(end_list)
		(start_list "subsid2" 3 )
		(mapcar 'add_list subsidlist)
		(end_list)

		(start_list "Material" 3 )
        (mapcar 'add_list gl_Material_List)
        (end_list)
		
        ;;设置控件的值
        ;;p1
        (setq t_QuickInsert "0")
        (set_tile "Main_Type" Main_Type)
        (set_tile "Exp_No" Exp_No)
		(set_tile "Depth1" (rtos Depth1 2 2))
        (set_tile "feature" feature)
        (set_tile "subsid" subsid)
		(set_tile "t_QuickInsert" t_QuickInsert)
		(set_tile "text-PointSize1" Point_Size1)
		(mode_tile "Exp_No" 3)
        ;;p2
		; ;(mode_tile "LinkPointList" 0)
		; (mode_tile "feature2" 1)
		; (mode_tile "subsid2" 1)
		; (mode_tile "Depth2" 1)
		(if entP2
            (progn
				(set_tile "LinkPoint" Exp_No2)
				(set_tile "feature2" feature2)
				(set_tile "subsid2" subsid2)
				(set_tile "Depth2" (rtos Depth2 2 2))
				(set_tile "text-PointSize2" Point_Size2)
            )
		)
        ;;line 
		(set_tile "D_S" D_S)
		(set_tile "Flowdirect" (rtos Flowdirect 2 0))
		(set_tile "Pressure" Pressure)
		(set_tile "Voltage" Voltage)
		(set_tile "Material" Material)
		(set_tile "Cab_Count" Cab_Count)
		(set_tile "Hole_Used" Hole_Used)
		(set_tile "Hole_Count" Hole_Count)
		(set_tile "Road_Name" Road_Name)
		(set_tile "PaiWu" PaiWu)
        ;;project
        (set_tile "unit" unit)
        (set_tile "date" date)
        (set_tile "place" place)
        (set_tile "subproject" subproject)
        (set_tile "project" project)

        ;;设置按钮活动
		(action_tile "cancel" "(done_dialog 0)")
        (action_tile "accept" "(draft_getdata)(done_dialog 1)")
		
        (action_tile "bt-selectbt1" "(draft_getdata)(done_dialog 2)")		;拾取点
		(action_tile "bt_toOldPoint" "(draft_getdata)(done_dialog 3)")	;连接到已知点	
        (action_tile "bt_toNewPoint" "(draft_getdata)(done_dialog 4)")	;连接到新点

        (action_tile "Main_Type" "(OnSelectPointType)")			;改变主类型
		;(action_tile "LinkPointList" "(OnSelectLinkPoint)")			;选择第二个点
		(action_tile "Exp_No" "(OnEditPointName $value $reason Exp_No)")					;编辑点号
		(action_tile "LinkPoint" "(OnEditPointName $value $reason Exp_No2)")					;编辑点号
        ;;设置按钮状态
		 ;;雨水
		(if (= typename "Y")
			(progn
				(setq PaiWu "0")
				(mode_tile "PaiWu" 0)
			)
			(progn
				(setq PaiWu "2")
				(mode_tile "PaiWu" 1)
			)
		)
		;;流向
		(if (or (= typename "G") (= typename "P") (= typename "Y") (= typename "W") (= typename "R"))
			(mode_tile "Flowdirect" 0)
			(mode_tile "Flowdirect" 1)
		)
		;;电力
		(if (or (= typename "D") (= typename "L") (= typename "X") )
			(progn
				(mode_tile "Cab_Count" 0)
				(mode_tile "Hole_Used" 0)
				(mode_tile "Hole_Count" 0)
			)
			(progn
				(mode_tile "Cab_Count" 1)
				(mode_tile "Hole_Used" 1)
				(mode_tile "Hole_Count" 1)
			)
		)
		;;电压
		(if (or (= typename "D") (= typename "L") )
			(mode_tile "Voltage" 0)
			(mode_tile "Voltage" 1)
		)
		;;压力
		(if (or (= typename "Q") (= typename "G") )
			(mode_tile "Pressure" 0)
			(mode_tile "Pressure" 1)
		)
		
        ;;显示对话框
        (setq std (start_dialog))

        (if (= std 1) ;ok
           (SaveDraftDlg ProjectInfoList)
		)

        (if (= std 2) ;选择点1坐标
            (if (setq sp (getpoint (list x1 y1) (strcat "\n选择点" Map_No "的位置：")))
                (progn
                    (setq sp (SetPrecisionPoint sp 4)
							x1 (car sp)
                          y1 (cadr sp)
						  z1 (caddr sp)
                          p1 sp
                    ) ;_ End_setq

                    (SaveDraftDlg ProjectInfoList)
                    ;;更新对象
                    (if entP1
                        (progn ;更新点和连接的线段
                            (setq el   (entget entP1)
                                  pos1 (cdr (assoc 10 e1))
                                  el   (subst (cons 10 (list x1 y1 (caddr pos1))) (assoc 10 el) el)
                            ) ;_ end_setq
                            (entmod el)
                            ;;移动线段
                            (if entLine
                                (progn
                                    (setq entl  (entget entLine)
                                          pos10 (cdr (assoc 10 entl))
                                          pos11 (cdr (assoc 11 entl))
                                    ) ;_ end_setq
                                    (if (equal pos10 pos1)
                                        (setq entl (subst (cons 10 pos1) (assoc 10 pos10) entl))
                                        (setq entl (subst (cons 11 pos1) (assoc 11 pos11) entl))
                                    ) ;_ end_if
                                    (entmod entl)
									
                                ) ;_ end_progn
                            ) ;_ end_if
                        ) ;_ end_progn
                    ) ;_ end_if
                ) ;progn
            ) ;if
        ) ;end if

        (if (= std 3) ;连接到已知点
			(progn
				(SaveDraftDlg ProjectInfoList)
				(if (setq ent (car (entsel "\选择一个已知点对象:")))
					(progn
						
						
						(setq bsame nil)
						;;1.判断已知点和entP1之间是否已经有线段,若有,则更新entLine,否则,新建entLine
						; (foreach e LinkPointList
							; (if (equal e (ldata-get ent "Exp_No")) (setq bsame T))
						; )
						(if (not bsame)
							(setq p10 (cdr (assoc 10 (entget entP1)))
								p11 (cdr (assoc 10 (entget ent)))
								entLine (AddNewLine p10 p11 typename (GetLineAttribsList))
							)
						)
						
						(setq entP2 entP1
							entP1 ent;
						)
						(InitDraftDlg Exp_No entP1 entP2 entLine)
					) ;progn
				) ;_ End_if
			)
			
        ) ;end if
		
		(if (= std 4) ;连接到新点
			(progn
				;;save entP1 and entLine
				;; entP1 不存在,则新建对象
				(SaveDraftDlg ProjectInfoList)
               				
				(if (setq sp (getpoint (list x1 y1 )(strcat "\n选择插入点的位置：")))
					(progn ;;else 快速输入
						(setq sp (SetPrecisionPoint sp 4))
						(setq bsameAttrib "Y")
						(while (= "Y" (setq bsameAttrib (strcase (Fun_InPutString "Y" "USERS3" "\n除物探点号和埋深,其他属性同前一个点吗?(Y or N):" ))))
							(setq newExpNo (strcase (Fun_InPutString (CreateNewPointName prePointName typename "EXP") "USERS4" "\n输入管线点名:"))
								newdepth (Fun_InPutValue Depth1 "USERR4" "\n输入管线点深度:" 4)
							)
							(while (HasSameExpName newExpNo)
								;(alert (strcat "点名" newExpNo "重复!\n请输入新的点号:"))
								(prompt (strcat "点名" newExpNo "重复!"))
								(setq newExpNo (strcase (Fun_InPutString (CreateNewPointName newExpNo typename "EXP") "USERS4" "\n输入管线点名:")))
							)

							;;add new point and line
                            (setq attriblist (GetPoint1AttribsList)
								attriblist (subst (cons "Exp_No" newExpNo) (assoc "Exp_No" attriblist) attriblist)
								attriblist (subst (cons "Map_No" newExpNo) (assoc "Map_No" attriblist) attriblist)
								attriblist (subst (cons "Depth" newdepth) (assoc "Depth" attriblist) attriblist)
							)
							(setq p10 (cdr (assoc 10 (entget entP1)))
								newp (AddNewPoint sp newExpNo typename featurestr subsidstr attriblist)
								entLine (AddNewLine  
										(list (car sp) (cadr sp) (- (last sp) newdepth))
										(list (car p10) (cadr p10) (- (last p10) (ldata-get entP1 "Depth"))) typename (GetLineAttribsList))
								prePointName newExpNo)
							(ldata-put entLine "Start_Point" (ldata-get  newp "Exp_No"))
							(ldata-put entLine "End_Point" (ldata-get entP1 "Exp_No"))
							(ldata-put entLine "Start_Deep" newdepth)
							(ldata-put entLine "End_Deep" (ldata-get entP1 "Depth"))
							
							(if (and entLine entP1 newp)
                                			    (progn
								; (AddLineTextLabel entLine (ldata-get  newp "Depth") (ldata-get entP1 "Depth"))
								; (AddLineFlowdirect entLine)
								(if (not (setq deep1 (ldata-get entLine "Start_Deep"))) 
									(setq deep1 (ldata-get newp "Depth")))
								(if (not (setq deep2 (ldata-get entLine "End_Deep"))) 
									(setq deep2 (ldata-get entP1 "Depth")))
								(UpdateLabel entLine deep1 deep2)
                              				  )
							)
							(setq 	entP2 entP1
								entP1 newp)
							
							(setq  
								Exp_No2 Exp_No
								Depth2	Depth1
								feature2 feature
								featurestr2 featurestr
								subsid2	subsid
								subsidstr2	subsidstr

								Exp_No newExpNo
								Depth1 newdepth
								
                                x1 (car sp)
                                y1 (cadr sp)
								z1 (caddr sp)
                                x2 (car p10)
                                y2 (cadr p10)
								z2 (caddr p10)
							)

                            ;;下一点
                            (prompt "\n继续插入管线点.")
                            (setq sp (getpoint (list x1 y1 )(strcat "\n选择插入点的位置："))
								sp (SetPrecisionPoint sp 4))
						)
						;;在对话框中编辑点线
						(if (= "N" (strcase bsameAttrib))
							(progn
								(setq  
									Exp_No2 Exp_No
									Depth2	Depth1									

									x1 (car sp)
									y1 (cadr sp)
									z1 (caddr sp)
									x2 x1
									y2 y1
									z2 z1
									
									feature2 feature
									featurestr2 featurestr
									subsid2	subsid
									subsidstr2	subsidstr
									
									entP2 entP1
									entP1 nil
									entLine nil
								)
								(InitDraftDlg Exp_No entP1 entP2 entLine)
							)
						)
					) ;progn
				)
			);progn
		) ;end if
        
		;;对话框显示位置：右半侧
        (setq app       (vlax-get-acad-object)
              width     (vla-get-width app)
              height    (vla-get-height app)
              windpoint (list (- width (+ 10 DLG_WIDTH)) 30)
        ) ;_ End_setq
    )

    (unload_dialog id) ;卸载对话框文件
    (princ) ;静默退出
)   ;

;;;得到对话框参数
(defun GetPoint1AttribsList ( / outlist)
	(setq outlist nil
		outlist (cons (cons "Exp_No" Exp_No) outlist)
		outlist (cons (cons "Map_No" Exp_No) outlist)
		outlist (cons (cons "Depth" Depth1) outlist)
		outlist (cons (cons "Subsid" subsidstr) outlist)
		outlist (cons (cons "Feature" featurestr) outlist)
		outlist (cons (cons "Main_Type" typename) outlist)
		outlist (cons (cons "Point_Size" Point_Size1) outlist)
		
		;;outlist (cons (cons "Sur_Date" date) outlist)
		outlist (cons (cons "Unit" unit) outlist)
		outlist (cons (cons "Location" place) outlist)
		outlist (cons (cons "SProject" subproject) outlist)
		outlist (cons (cons "Project" project) outlist)
	)
	outlist
)

(defun GetPoint2AttribsList ( / outlist)
	(setq outlist nil
		outlist (cons (cons "Exp_No" Exp_No2) outlist)
		outlist (cons (cons "Map_No" Exp_No2) outlist)
		outlist (cons (cons "Depth" Depth2) outlist)
		outlist (cons (cons "Subsid" subsidstr2) outlist)
		outlist (cons (cons "Feature" featurestr2) outlist)
		outlist (cons (cons "Main_Type" typename) outlist)
		outlist (cons (cons "Point_Size" Point_Size2) outlist)
		
		;;outlist (cons (cons "Sur_Date" date) outlist)
		outlist (cons (cons "Unit" unit) outlist)
		outlist (cons (cons "Location" place) outlist)
		outlist (cons (cons "SProject" subproject) outlist)
		outlist (cons (cons "Project" project) outlist)
	)
	outlist
)
;;;得到管线段属性
(defun GetLineAttribsList ( / outlist PaiWustr)
	(cond 
			((= PaiWu "0") (setq PaiWustr "是"))
			((= PaiWu "1") (setq PaiWustr "否"))
			((= PaiWu "2") (setq PaiWustr "无"))
			(T (setq PaiWu "无"))
		)
	(setq outlist nil
		outlist (cons (cons "Start_Point" Exp_No) outlist)
		outlist (cons (cons "Start_Deep" Depth1) outlist)
		outlist (cons (cons "End_Deep" Depth2) outlist)
		outlist (cons (cons "End_Point" Exp_No2) outlist)
		outlist (cons (cons "Material" (nth (atoi Material) gl_Material_List)) outlist)
		outlist (cons (cons "D_S" D_S) outlist)
		outlist (cons (cons "Cab_Count" Cab_Count) outlist)
		outlist (cons (cons "Main_Type" typename) outlist)
		outlist (cons (cons "Hole_Count" Hole_Count) outlist)
		outlist (cons (cons "Hole_Used" Hole_Used) outlist)
		outlist (cons (cons "Pressure" Pressure) outlist)
		outlist (cons (cons "Voltage" Voltage) outlist)
		outlist (cons (cons "Flowdirect" Flowdirect) outlist)
		outlist (cons (cons "Road_Name" Road_Name) outlist)
		outlist (cons (cons "PaiWu" PaiWustr) outlist)
		
		outlist (cons (cons "Sur_Date" date) outlist)
		;;outlist (cons (cons "Mdate" date) outlist)  ;;4.0版，对话框暂无对应参数
		outlist (cons (cons "Unit" unit) outlist)
		outlist (cons (cons "Location" place) outlist)
		outlist (cons (cons "SProject" subproject) outlist)
		outlist (cons (cons "Project" project) outlist)
	)
	outlist
)
	;;;选择点图元
(defun Choose_pointent  (/ ent enttype entlist)
(setq ent     (entsel "\n可编辑的点对象包括图块、单行文字、管线段，并生成标准以点名为属性的图块。\n选择要编辑的点对象：")
	  enttype nil
	  entlist nil) ;实体类型
(if (= nil ent)
	(progn
		(prompt "\n没有选择图元。")
		) ;progn
	(progn
		(setq ent     (car ent)
			  entlist (entget ent)
			  enttype (cdr (assoc 0 entlist)))
		(if (and (/= enttype "TEXT") (/= enttype "INSERT") (/= enttype "LINE")) ;(/= enttype "POINT")
			(progn
				(prompt "\n图元不是单行文字和块或管线段，退出。")
				(setq ent nil)
				) ;progn
			) ;if
		) ;progn
	) ;if
ent
) ;defun

;;;编辑点线属性
;*********************************************************************************************
;函数定义:LineInfo_EditDraftPoint()
;功能：编辑管线点,插入管线点
;参数：
;创建时间：2014/12/01   12:40
;修改时间：2015/12/24
;创建人：沈雄君
;*********************************************************************************************
(defun C:LineInfo_EditDraftPoint  ( / attribslist  layername layerobj pname points pt tname)
    ;(DraftDlg nil nil nil 1)
	;;0 promt
	(if (setq pt (getpoint "\n插入管线点。\n选择插入点的位置："))
		(progn
			(setq pt (SetPrecisionPoint pt 4))
			;;0当前位置是否有其他点对象
			(if (setq points (SearchPoint (GetInsertRoot nil) (car pt) (cadr pt)))
						;;已有相同位置点
				(*error* "选择位置已有管线点，请重新选择！")
			)	
			;;1获取当前图层类型,默认J
			(setq layerObj (vla-get-ActiveLayer (vla-get-Activedocument (vlax-get-acad-object)))
				layerName (vla-get-Name layerObj)
				tname (GetTypeFromPointName layerName)
			)
			;;2获取点号
			(setq pname (CreateNewPointName tname tname "MAP"))
			
			;;3以默认参数创建点
			(setq attribslist (list (cons "Exp_No" pname) 
								  (cons "Map_No" pname) 
								  (cons "Main_Type" tname)
								  (cons "Feature" "起讫点"))
			)
				;;判断点是否超出索引范围
				;;！！！！代码耦合
			(if (IsOutRange gl_PointSpaceIndex (car pt) (cadr pt))
				(setq gl_PointSpaceIndex nil)	;;AddNewPoint 不更新索引
			)
			(AddNewPoint pt pname tname "起讫点" "" attribslist)
			
			;;4.加入空间索引和名称列表
			(setq gl_MapNameList (cons pname gl_MapNameList))
			(if (not gl_PointSpaceIndex) (setq gl_PointSpaceIndex (GetInsertRoot T)))
			;;5 回复当前图层
			(vla-put-ActiveLayer (vla-get-Activedocument (vlax-get-acad-object)) layerObj)
		)
	)
	(princ)
 ) ;_ end_defun

 ;*********************************************************************************************
;函数定义:C:InsertPointInLine()
;功能：在现有管线段上插入一个管线点，自动加入名称和索引；
;参数：
;创建时间：2014/12/19   13:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:InsertPointInLine( / ent entl entlist ep1 ep2 fstr  lattriblist line1 line2 
		mysnap newentp newname p0 p10 p11 pattriblist pname snap ss sstr typename)
	(prompt "\n在管线段中插入一个管线点.")
	(setq snap (getvar "OSMODE")
		mysnap 512 ;;最近的点
		entl nil ;;线段
	)
	(setvar "OSMODE" mysnap)
	
	(if (setq p0 (getpoint "\n拾取点:"))
		(if (setq ss (ssget p0))
			(if (= 1 (sslength ss))
				(progn
					(setq ent (ssname ss 0)
						entlist (entget ent)
					)
					(if (= "LINE" (cdr(assoc 0 entlist)))
						(if (vlax-ldata-get ent gl_AppName)
							(setq entl ent)
							(prompt "\n直线段未定义管线属性.")
						)
						(prompt "\n未拾取到管线段上.")
					)
				)
				(prompt "\请勿选择两个对象的交点位置.")
			)
		)
		(prompt "\n未拾取到点.")
	)
	;;找到了一条直线
	(if entl
		(progn
			(setq p10 (cdr (assoc 10 entlist))
				p11 (cdr (assoc 11 entlist))
				typename (ldata-get entl "Main_Type")
				lattriblist (vlax-ldata-get entl gl_AppName)
			)
			
			;;获取点属性
			(setq ep1 nil
				ep2 nil
				pattriblist nil)  ;;点属性列表
			(if (setq ep1 (GetConnectEntity (car p10) (cadr p10) "INSERT"))
				(setq ep1 (car ep1)
					pattriblist (vlax-ldata-get ep1 gl_AppName)
					pname (ldata-get ep1 "Map_No")
					fstr (ldata-get ep1 "Feature")
					sstr (ldata-get ep1 "Subsid")
					)
				;else
				(if (setq ep2 (GetConnectEntity (car p11) (cadr p11) "INSERT"))
					(setq ep2 (car ep2)
						pattriblist (vlax-ldata-get ep2 gl_AppName)
						pname (ldata-get ep2 "Map_No")
						fstr (ldata-get ep2 "Feature")
						sstr (ldata-get ep2 "Subsid")
					)
				)
			)
			
			;;加入管线点
			(if pattriblist
				(progn
					(setq newname (CreateNewPointName pname (ldata-get entl "Main_Type") typename)
						pattriblist (subst (cons "Exp_No" newname) (assoc "Exp_No" pattriblist) pattriblist)
						pattriblist (subst (cons "Map_No" newname) (assoc "Map_No" pattriblist) pattriblist)
						pattriblist (subst (cons "Feature" "直线点") (assoc "Feature" pattriblist) pattriblist)
					)
					(setq p0 (SetPrecisionPoint p0 4))
					(if (setq newentp(AddNewPoint (list (car p0) (cadr p0) 0) newname typename "直线点" sstr pattriblist))
						(progn
							(setq gl_MapNameList (cons newname gl_MapNameList))
							(ldata-put newentp "Status_DB" 0)
							(ldata-put newentp "Status_Modify" 1)
							(ldata-put newentp "Text_Pos" (list 0 0 0 0))
							(ldata-put newentp "Edge" nil)
							
							;;add new line1
							(setq line1 (AddNewLine p10 p0 typename lattriblist))
							(ldata-put line1 "End_Point" newname)
							(ldata-put line1 "Start_Deep" (ldata-get entl "Start_Deep"))
							(ldata-put line1 "End_Deep" 0.0)
							(ldata-put line1 "Status_DB" 0)
							(ldata-put line1 "Status_Modify" 1)
							(ldata-put line1 "Edge" nil)
							
							;;add another lines
							(setq line2 (AddNewLine p0 p11 typename lattriblist))
							(ldata-put line2 "Start_Point" newname)
							(ldata-put line2 "Start_Deep" 0.0)
							(ldata-put line2 "End_Deep" (ldata-get entl "End_Deep"))
							(ldata-put line2 "Status_DB" 0)
							(ldata-put line2 "Status_Modify" 1)
							(ldata-put line2 "Edge" nil)
							
							(DelTextLabel entl)
							(DelEntity  entl)
							
							;;showDlg
							; (if ep1
								; (DraftDlg  ep1 newentp line1 1)
							; )
							; (if ep2
								; (DraftDlg newentp ep2 line2 1)
							; )
							
						)
					)
				)
			)
		)
	)
	
	(setvar "OSMODE" snap)
	(princ)
)
 ;*********************************************************************************************
;函数定义:AddDraftLine()
;功能：选择两个管线段,然后连线
;参数：
;创建时间：2014/12/01   12:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:AddDraftLine ( / colorobj ent1 ent2 entl entlist ents len lineobj maintype1 maintype2 nextpt p10 p11 typename osmode)
	(prompt "\n绘制管线段,连接两个管线点.")
	;;osnap
	;;(osnap "_INS")
	(setq osmode (getvar "OSMODE"))
	(setvar "OSMODE" 64)	;;_INS
	(if (setq ent1 (car (entsel "\n选择第一个管线点:")))
		(progn
			(setq entlist (entget ent1)
				typename (cdr (assoc 0 entlist))
				p10 (cdr (assoc 10 entlist)))
			(if (and (= "INSERT" typename) (vlax-ldata-get ent1 gl_AppName))
				(progn
					(setq maintype1 (ldata-get ent1 "Main_Type"))
					(while (setq nextpt (getpoint p10 "\n选择下一个管线点:"));(setq ent2 (car (entsel "\n选择下一个管线点:")))
						;;通过索引找到点
						(setq ents (SearchPoint (GetInsertRoot nil) (car nextpt) (cadr nextpt))
							ents (ListRemoveSameElement ents)
							len (length ents)
							ent2 nil
						)	
						(cond 
							((= 0 len) (prompt "\n当前位置无有效管线点，请重新选择。"))
							((> len 1) (*error* "当前位置有多个管线点，请删除重复管线点。"))
							((= 1 len) (setq ent2 (nth 2 (car ents))))
						)
						(if ent2
							(progn
								(setq entlist (entget ent2)
									typename (cdr (assoc 0 entlist))
									p11 (cdr (assoc 10 entlist)))
								(if (and (= "INSERT" typename) (vlax-ldata-get ent2 gl_AppName))
									(progn
										(setq maintype2 (ldata-get ent2 "Main_Type"))
										;;warning
										(if (/= maintype1 maintype2) (prompt "\n警告！两个管线点的类型不同。"))
										;;warning
										(if (< (distance p10 p11) gl_MIN) (*error* "错误：两点重合，退出！"))
										;;add layer
										(AddLayer maintype2 (strcat maintype2 "_Line"))
										;;add line
										(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
												 (vlax-3D-point p10)
												 (vlax-3D-point p11)))
										(vla-put-layer lineobj (strcat maintype2 "_Line"))
										(setq colorObj (vla-get-truecolor lineobj))
										(vla-put-colorindex colorObj 4);;set acCyan
										(vla-put-truecolor lineobj colorObj)
										;;put into index
										(if gl_LineSpaceIndex
											(progn
												(setq entl (vlax-vla-object->ename lineobj))
												(setq gl_LineSpaceIndex (PutEntityIndex gl_LineSpaceIndex entl (car p10) (cadr p10)))
												(setq gl_LineSpaceIndex (PutEntityIndex gl_LineSpaceIndex entl (car p11) (cadr p11)))
											)
										)
										;;next point
										(setq p10 p11
											maintype1 maintype2)
									)
								)
							)
							;;else
							(prompt "\n当前对象不是有效管线点，请重新选择。")
						)
					)
					;(prompt "\n选择的对象不是管线点,退出!")
				)
				;;else
				(prompt "\n选择的对象不是管线点,退出!")
			)
		)
		;;else
		(prompt "\n选择的对象不是管线点,退出!.")
	)
	(setvar "OSMODE" osmode)
	(princ)
)
;*********************************************************************************************
;函数定义:C:EditErrorList()
;功能：从错误列表gl_ErrorList中获取对象,并用对话框方式编辑
;;		前提条件:必须使用CheckErrors建立拓扑关系,找到错误
;参数：
;创建时间：2014/12/19   13:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:SetDefaultProjectInfo( /  allent bapply date ent  i place project projectinfolist 
									j subproject unit e)
	(setq unit "" date "" place "" subproject "" project "" )
	(if (setq ProjectInfoList (ReadProjectInfo nil))
		(progn
			(setq unit (cdr (assoc "Unit" ProjectInfoList))
				date (cdr (assoc "Sur_Date" ProjectInfoList))
				place (cdr (assoc "Location" ProjectInfoList))
				subproject (cdr (assoc "SProject" ProjectInfoList))
				project	(cdr (assoc "Project" ProjectInfoList))
			)
		)
	)
	(setq unit (Fun_InPutString unit "USERS1" "\n请输入单位名称:")
		ProjectInfoList (subst (cons "Unit" unit) (assoc "Unit" ProjectInfoList) ProjectInfoList))
	(setq project (Fun_InPutString project "USERS2" "\n请输入项目名称:")
		ProjectInfoList (subst (cons "Project" project) (assoc "Project" ProjectInfoList) ProjectInfoList))
	(setq subproject (Fun_InPutString subproject "USERS3" "\n请输入子项目名称:")
		ProjectInfoList (subst (cons "SProject" subproject) (assoc "SProject" ProjectInfoList) ProjectInfoList))
	(setq date (Fun_InPutString date "USERS4" "\n请输入子调查时间:")
		ProjectInfoList (subst (cons "Sur_Date" date) (assoc "Sur_Date" ProjectInfoList) ProjectInfoList))
	(setq place (Fun_InPutString place "USERS5" "\n请输入子调查地点:")
		ProjectInfoList (subst (cons "Location" place) (assoc "Location" ProjectInfoList) ProjectInfoList))
	(if (setq bapply (Fun_InPutString "Y" "USERS1" "\n替换所有管线的项目属性吗?(Y/N)"))
		(if (= "Y" (strcase bapply))
			(progn
				(setq i 0 j 0)
				(if (setq allent (ssget "X" ))
					(progn
						(repeat (sslength allent)
							(setq ent (ssname allent i))
							(if (vlax-ldata-get ent gl_AppName)
								(progn
									(foreach e ProjectInfoList
										(ldata-put ent (car e) (cdr e))
									)
									(setq j (1+ j))
								)
							)
							;(PutEntityAttribs ent ProjectInfoList)
							(setq i (1+ i))
						)
						(prompt (strcat "\n修改了" (rtos i 2 0) "个管线对象的属性."))
					)
				)
				(WriteProjectInfo ProjectInfoList nil)
			)
		)
	)
	(princ)
)

;*********************************************************************************************
;函数定义:C:MovePoint()
;功能：移动点.并移动相关联的线段,更新标注.
;参数：
;创建时间：2015/1/23   10:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:MovePoint(  / bpoint ent1 entlist pos pxy sp typename x0 y0 allent ent   i newpos pos10 pos11 deeps)
	(prompt "\n移动管线点.")
	(setq ent1 nil 
		bpoint nil)
	(while (not bpoint)
		(if (setq ent1 (car (entsel "\n选择管线点:")))
			(progn
				(setq entlist (entget ent1)
					typename (cdr (assoc 0 entlist)))
				(if (and (= "INSERT" typename) (vlax-ldata-get ent1 gl_AppName))
					(setq bpoint T)
					(progn 
						(setq ent1 nil
							bpoint nil)
						(prompt "\n选择的对象不是管线点,请更新版本,重新选择.")
					)
				)
			)
		)
	)
	(if ent1
		(progn
			(setq pos (cdr (assoc 10 (entget ent1)))
				x0 (car pos)
				y0 (cadr pos)
				pxy (list x0 y0 )
			)
			(if (setq sp (getpoint pxy (strcat "\n选择移动点的位置：")))
				(progn 
					(setq sp (SetPrecisionPoint sp 4))
					;(MoveOnePoint2 pos sp ent1)
					
					(if (not (IsSamePoint pos sp))
						(progn
							(setq allent (GetConnectEntity (car pos) (cadr pos) "LINE"))
							(setq i 0)
							;;平面移动
							(vla-move (vlax-ename->vla-object ent1) (vlax-3D-point pos) (vlax-3D-point (car sp ) (cadr sp) (caddr pos)))
							;;update space index
							(setq gl_PointSpaceIndex (UpdatePointIndex (GetInsertRoot nil) ent1 x0 y0))
							;;移动管线，修改点坐标
							(repeat (length allent)
								(setq   ent (nth i allent)
									entlist (entget ent)
									pos10 (cdr (assoc 10 entlist))
									pos11 (cdr (assoc 11 entlist))
								)
								(if (IsSameXY pos pos10);;pos10 is the same point
									(setq newpos (list (car sp) (cadr sp) (caddr pos10))
										entlist (subst (cons 10 newpos) (cons 10 pos10) entlist)
										gl_LineSpaceIndex (UpdatePointIndex (GetLineRoot nil) ent (car pos10) (cadr pos10))
									)
									(setq newpos (list (car sp) (cadr sp) (caddr pos11));;pos11 is the same point
										entlist (subst (cons 11 newpos) (cons 11 pos11) entlist)
										gl_LineSpaceIndex (UpdatePointIndex (GetLineRoot nil) ent (car pos11) (cadr pos11))
									)
								)
								(entmod entlist)
								;(DelTextLabel ent)
								(UpdateLabel ent (ldata-get ent "Start_Deep") (ldata-get ent "End_Deep"))
								(setq i (1+ i))
							)
							(princ "\n点移动，从：")
							(princ pos)
							(princ "移动到：")
							(princ (list (car sp ) (cadr sp) (caddr pos)))
						)
					)
				)
			)
		)
	)
	
	(princ)
)
;;移动一个点ptname from pos to sp
(defun MoveOnePoint(pos sp ptname / allent ent entlist enttype i newpos pos10 pos11)
	(setq i 0
		sp (SetPrecisionPoint sp 4))
	(if (setq allent (ssget "X" (list  (cons -4 "<AND") (list -3 (list gl_AppName))
							(cons -4 "<OR")
							(cons -4 "=,=,*") (cons 10 pos) (cons -4 "=,=,*") (cons 11 pos)
							(cons -4 "OR>")
							(cons -4 "AND>"))))
		(progn
			;;删除点号不一致的点
			(repeat (sslength allent)
				(setq ent (ssname allent i)
					mapno (ldata-get ent "Map_No")
					enttype (cdr (assoc 0 (entget ent)))
				)
				(if (and (= enttype "INSERT") (/= ptname mapno))
					(ssdel ent allent)
				)
				(setq i (1+ i))
			)
			(setq i 0)
			(repeat (sslength allent)
				(setq   ent (ssname allent i)
					entlist (entget ent)
					enttype (cdr (assoc 0 entlist))
				)
				(if (= "INSERT" enttype)
					(progn
						(setq pos10 (cdr (assoc 10 entlist))
							newpos (list (car sp) (cadr sp) (caddr pos10));;keep elevation same
							entlist (subst (cons 10 newpos) (cons 10 pos10) entlist)
						)
						(vla-move (vlax-ename->vla-object ent) (vlax-3D-point pos) (vlax-3D-point newpos))
					)
				)
				(if (= "LINE" enttype)	
					(progn
						(setq pos10 (cdr (assoc 10 entlist))
							pos11 (cdr (assoc 11 entlist))
						)
						(if (IsSameXY pos pos10);;pos10 is the same point
							(setq newpos (list (car sp) (cadr sp) (caddr pos10))
								entlist (subst (cons 10 newpos) (cons 10 pos10) entlist)
							)
							(setq newpos (list (car sp) (cadr sp) (caddr pos11));;pos11 is the same point
								entlist (subst (cons 11 newpos) (cons 11 pos11) entlist)
							)
						)
						(entmod entlist)
						;(DelTextLabel ent)
						(UpdateLabel ent (ldata-get ent "Start_Deep") (ldata-get ent "End_Deep"))
					)
				)
				(setq i (1+ i))
			)
		)	
		
	)

)
;;移动一个点，以及相关联的线段
;;参数：pos 起点，sp 目标点，entp 点实体
;;返回：
(defun MoveOnePoint2(pos sp entp / allent ent entlist  i newpos pos10 pos11 deeps)
	(setq i 0
		sp (SetPrecisionPoint sp 4))
	
	(if (not (IsSamePoint pos sp))
		(progn
			(setq allent (GetConnectEntity (car pos) (cadr pos) "LINE"))
			
			(vla-move (vlax-ename->vla-object entp) (vlax-3D-point pos) (vlax-3D-point sp))
			(repeat (length allent)
				(setq   ent (nth i allent)
					entlist (entget ent)
					pos10 (cdr (assoc 10 entlist))
					pos11 (cdr (assoc 11 entlist))
					deeps (GetDeeps ent)
				)
				(if (IsSameXY pos pos10);;pos10 is the same point
					(setq newpos (list (car sp) (cadr sp) (- (caddr sp) (car deeps)))
						entlist (subst (cons 10 newpos) (cons 10 pos10) entlist)
					)
					(setq newpos (list (car sp) (cadr sp) (- (caddr sp) (cadr deeps)));;pos11 is the same point
						entlist (subst (cons 11 newpos) (cons 11 pos11) entlist)
					)
				)
				(entmod entlist)
				;(DelTextLabel ent)
				(UpdateLabel ent (ldata-get ent "Start_Deep") (ldata-get ent "End_Deep"))
				(setq i (1+ i))
			)
		)
	)
)
;*********************************************************************************************
;函数定义:SetPrecisionPoint()
;功能：把点的精度保存为4为小数,使之在图形中和输出文件中一致。
;参数：
;创建时间：2015/3/22   10:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun SetPrecisionPoint(pt precision / outp len i ps)
	(setq outp nil
		i 1)
	(if (and (listp pt) precision)
		(progn 
			(setq len (length pt))
			(repeat len
				(setq ps (rtos (nth (- len i) pt) 2 precision)
					outp (cons (atof ps) outp)
					i (1+ i)
				)	
			)
		)
	)
	(if (not outp)
		(setq outp pt)
	)
	outp
)
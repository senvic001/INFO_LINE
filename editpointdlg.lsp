;;;对话框内容函数
;;;两个点实体，可以为text或insert
;;;mode 工作模式:编辑模式1 ,错误编辑模式2

(defun pointdlg (ent1 ent2 mode / app cab_count d_s date dlg_height dlg_width e end_deep 
entline entp1 entp2 Exp_No Exp_No2 feature feature2 featurelist flowdirect   height hole_count
 hole_used id Map_No Map_No2 material place point_size1 point_size2 Well_Deep1 Well_Deep2 point_type preedgelist
 pressure project  road_name paiwu sp  start_deep std subproject subsid ibranch
 subsid2 subsidlist surf_h surf_h2 typelist typename unit voltage width windpoint x x2 y y2 )

	;;定义分支列表,在InitP1中初始化；
	(setq BranchList nil)
	(defun getdata  () ; 从控件得到的值       
        (setq
			;;p1
            point_type (get_tile "point_type")
			typename   (substr (nth (atoi point_type) typelist) 1 1)
            Exp_No     (get_tile "Exp_No")
            Map_No     (get_tile "Map_No")
            x          (atof (get_tile "x"))
            y          (atof (get_tile "y"))
            Surf_h     (atof (get_tile "Surf_h"))

            feature    (get_tile "feature")
            subsid     (get_tile "subsid")
            subsidstr  (nth (atoi subsid) subsidlist)
			featurestr	(nth (atoi feature) featurelist)
			Point_Size1 (get_tile "text-PointSize1")
			Well_Deep1 (get_tile "Well_Deep1")
			;;---------------湖州管线普查------------
			Neck_Deep1 (get_tile "Neck_Deep1")
			JG_Material1 (get_tile "JG_Material1")
			JG_Shape1	(get_tile "JG_Shape1")
			
			JS_Type1	(get_tile "JS_Type1")
			JS_Size1	(get_tile "JS_Size1")
			
			Off_Well1	(get_tile "Off_Well1")
			Well_Memo1	(get_tile "Well_Memo1")
			
;;;P2
            Exp_No2    (get_tile "Exp_No2")
            Map_No2    (get_tile "Map_No2")
            x2         (atof (get_tile "x2"))
            y2         (atof (get_tile "y2"))
            Surf_h2    (atof (get_tile "Surf_h2"))

            feature2   (get_tile "feature2")
            subsid2    (get_tile "subsid2")
            subsidstr2  (nth (atoi subsid2) subsidlist)
			featurestr2	(nth (atoi feature2) featurelist)
			Point_Size2 (get_tile "text-PointSize2")
			Well_Deep2 (get_tile "Well_Deep2")
			;;---------------湖州管线普查------------
			Neck_Deep2 (get_tile "Neck_Deep2")
			JG_Material2 (get_tile "JG_Material2")
			JG_Shape2	(get_tile "JG_Shape2")
			
			JS_Type2	(get_tile "JS_Type2")
			JS_Size2	(get_tile "JS_Size2")
			
			Off_Well2	(get_tile "Off_Well2")
			Well_Memo2	(get_tile "Well_Memo2")
;;;line
            Start_Deep (atof (get_tile "Start_Deep"))
            End_Deep   (atof (get_tile "End_Deep"))
            D_S        (get_tile "D_S")
            Flowdirect (atoi (get_tile "Flowdirect"))
            Pressure   (get_tile "Pressure")
            Voltage    (get_tile "Voltage")
            Material   (get_tile "Material")
			Hole_Count (get_tile "Hole_Count")
            Cab_Count  (get_tile "Cab_Count")
            Hole_Used   (get_tile "Hole_Used")
			Road_Name (get_tile "Road_Name")
			PaiWu	(get_tile "PaiWu")
			BuryWay (get_tile "BuryWay")
			;;---------------湖州管线普查------------
			Line_Style	(get_tile "LineStyle")
			Line_Memo	(get_tile "Line_Memo")
			Build_Unit	(get_tile "Build_Unit")
			;;两端截面size
			Block_Size1	(get_tile "Block_Size1")
			Block_Size2	(get_tile "Block_Size2")
;;;project
            unit       (get_tile "unit")
            date       (get_tile "date")
            place      (get_tile "place")
            subproject (get_tile "subproject")
            project    (get_tile "project")
			
			gl_ErrorInfoString (get_tile "PromptInfo")
            )
        ) ;end_getdata
	;;流向改变
	(defun OnSelectFlowDirect (entl / delth editinfo entlist length_l lx p10 p11 podu)
		(setq EditInfo "")
		(setq lx (atoi (get_tile "Flowdirect")))
		;;长度与坡度
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
		(if (and podu lx)
			(if (or (and (= lx 1) (< deltH 0)) (and (= lx 2) (> deltH 0))) 
				(setq podu (- 0 podu))
			)
		)
		(if (> length_l gl_MIN)
			(progn
				(setq EditInfo (strcat EditInfo " 长度:" (rtos length_l 2 2) ",坡度:" (rtos podu 2 5)))
				(if (> podu 0.1) (setq EditInfo (strcat EditInfo " 警告:坡度偏大！")))
				(if (< podu 0) (setq EditInfo (strcat EditInfo " 警告:流向与高程相反！")))
			)
			(setq EditInfo  (strcat EditInfo " 警告:两点重合！"))
		)
		
		(set_tile "PromptInfo" EditInfo)
	)
    ;;选择点类型时，修改subsid内容
    (defun OnSelectPointType  ( / typenum )
        (setq typenum (atoi (get_tile "point_type")))
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
	
	;;附属物选择井之后，自动调整井盖相关属性，否则使其为“无”
	(defun OnChangeSubsid (key val / str)
		(getdata)
		(setq str (nth (atoi val) subsidlist))
		(if (vl-string-search "井" str)
			(cond 
				((= key "subsid")
					(progn
						(if (= "0" JG_Material1) (set_tile "JG_Material1" (setq JG_Material1 "1")))	;;无->默认值1
						(if (= "0" JG_Shape1) (set_tile "JG_Shape1" (setq JG_Shape1 "1")))	;;无->默认值1
						(if (= "0" JS_Type1) (set_tile "JS_Type1" (setq JS_Type1 "1")))	;;无->默认值1
						(if (= "0" Well_Memo1) (set_tile "Well_Memo1" (setq Well_Memo1 "1")))	;;无->默认值
					)
				)	
				;;else subsid2 = key
				((= key "subsid2")
					(progn
						(if (= "0" JG_Material2) (set_tile "JG_Material2" (setq JG_Material2 "1")))	;;无->默认值1
						(if (= "0" JG_Shape2) (set_tile "JG_Shape2" (setq JG_Shape2 "1")))	;;无->默认值1
						(if (= "0" JS_Type2) (set_tile "JS_Type2" (setq JS_Type2 "1")))	;;无->默认值1
						(if (= "0" Well_Memo2) (set_tile "Well_Memo2" (setq Well_Memo2 "1")))	;;无->默认值
					)
				)	
			)
		)
	)

;;;从相邻点个数和点的类别，自动设置featurestr
    (defun GetFeaturestr  (count typename / str)
        (cond
            ((= count 1) (setq str "直线点"))
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
            ((> count 5)
             (if (or (= typename "D") (= typename "L") (= typename "X"))
                 (setq str "多分支")
                 (setq str "多通")))
            (T (setq str nil))
            ) ;cond
        str
        ) ;defun

    ;;初始化起点和第二点,输入参数是TEXT或INSERT实体名称
    (defun InitP1  (ent / connectpoints e entlist enttype eobj ldata pent pos edge hp1 hp2 hl i)
		;;查找P1的相关点,并作为Stack
		(setq entlist (entget ent)
			  enttype (cdr (assoc 0 entlist))
			  pos	  (cdr (assoc 10 entlist)) ;!!!ent 必须是TEXT 和INSERT
		      entP1 ent
			  BranchList nil
			  ) ;
		;;把边存入stacklist ((p1 p2 L1) (p1 p3 L2))
		(if	(= 0 (length StackList))
			(progn
				(setq 
					  ConnectPoints	(GetConnectPoints (car pos) (cadr pos) "INSERT")
					  ;ConnectPoints	(append ConnectPoints (GetConnectPoints (car pos) (cadr pos) "INSERT"))
					  edge nil
				)
				(if	(listp ConnectPoints)
					(foreach pent  ConnectPoints
						(setq hp1 (cdr (assoc 5 entlist))
							hp2 (cdr (assoc 5 (entget (car pent))))
							hl (cdr (assoc 5 (entget (cadr pent))))
							edge (cons (list hp1 hp2 hl) edge)
							StackList (cons (cons ent pent) StackList)	;;深度优先	
							;;StackList (append StackList(list (cons ent pent)))	;;广度优先
							BranchList (cons (cons ent pent) BranchList)	;;分支数
						)
						(ldata-put (cadr pent) "Edge" (list hp1 hp2 hl)) ;;线段加入边属性
					)
				) ;if
				(ldata-put ent "Edge" edge) ;;加入边属性,用于更新关联数据
			) ;progn
			;;if StackList is not nil,find the branches
			(progn
				(setq BranchList (cons (car StackList) BranchList))	;;StackList的第一个元素
				(setq i 1)
				(while (and (< i (length StackList))(equal ent (car (nth i StackList))))
					(setq BranchList (append  BranchList (list (nth i StackList)))
						i (1+ i)
					)
				)
			)
		) ;if
		
		
		;;2初始化值,
		(if	(= gl_TableColorList nil)
			(setq gl_TableColorList (ReadColorConfig nil))
			) ;if
		(if (= nil typelist)
			(foreach e	gl_TableColorList
				(setq typelist (append typelist (list (strcat (car e) "-" (caddr e)))))
			)
		) ;foreach

		;;2-2设置其他参数的初始值
		(setq ;;p1
			  point_type "0"
			  typename "J"
			  Exp_No ""
			  Map_No Exp_No
			  x	0
			  y	0
			  Surf_h 0
			  Start_Deep 0

			  feature "0"
			  featurestr ""
			  subsid "0"
			  subsidstr	""
			  Point_Size1 ""
			  Well_Deep1 ""

			  ;;p2
			  Exp_No2 ""
			  Map_No2 Exp_No2
			  x2 0
			  y2 0
			  Surf_h2 0
			  End_Deep 0

			  feature2 "0"
			  featurestr2 ""
			  subsid2 "0"
			  subsidstr2 ""
			  Point_Size2 ""
			  Well_Deep2 ""

			  ;;project
			  unit ""
			  date ""
			  place	""
			  subproject ""
			  project ""
			  )
		;;
		; (if	(setq depth0 (ldata-get ent "Depth"))
			; (setq Start_Deep depth0)
			; (setq Start_Deep -1) ;恢复默认值
			; )

		;;2-3根据实体类型和参数，设置起点P1控件的值，
		;;根据typename 确定subsidlist的内容
		;;根据connectpoints 确定feature的建议值
		(cond
			((= enttype "TEXT")
			 (setq Exp_No	(vl-string-trim " " (cdr (assoc 1 entlist)))
				   Map_No	Exp_No
				   pos		(cdr (assoc 10 entlist))
				   y		(car pos)
				   x		(cadr pos)
				   Surf_h	(last pos)
				   typename	(GetTypeFromPointName Exp_No)
				   ) ;_ endofsetq
			 )
			((= enttype "INSERT")
			 (progn
				 (setq pos	  (cdr (assoc 10 entlist))
					   y	  (car pos)
					   x	  (cadr pos)
					   Surf_h (last pos)
					   ) ;_ endofsetq
				 (if  (vlax-ldata-get ent gl_AppName)
					 ;;从数据库生成的点
					 (progn
						 (setq ;;p1
							   typename	  (ldata-get ent "Main_Type")
							   Exp_No	  (ldata-get ent "Exp_No")
							   Map_No	  (ldata-get ent "Map_No")
							  ; Start_Deep (ldata-get ent "Depth")
							   featurestr (ldata-get ent "Feature")
							   subsidstr  (ldata-get ent "Subsid")
							   )
						(if (not (setq Point_Size1 (ldata-get ent "Point_Size")))
							(setq Point_Size1 "")
						)
						 (if (not (setq Well_Deep1(ldata-get ent "Well_Deep")))
							(setq Well_Deep1 "")
						 )
						 ;;------------湖州管线普查------------
						(if (not (setq Neck_Deep1 (ldata-get ent "Neck_Deep")))
							(setq Neck_Deep1 "")
						)
						(if (not (setq data (ldata-get ent "JG_Material")))
							(setq JG_Material1 "0")
							(setq JG_Material1 (rtos (vl-position data gl_JG_Material) 2 0))
						)
						(if (not (setq data (ldata-get ent "JG_Shape")))
							(setq JG_Shape1 "0")	;;poplist 0
							(setq JG_Shape1 (rtos (vl-position data gl_JG_Shape) 2 0))
						)
						(if (not (setq data (ldata-get ent "JS_Type")))
							(setq JS_Type1 "0")	;;poplist 
							(setq JS_Type1 (rtos (vl-position data gl_JS_Type) 2 0))
						)
						(if (not (setq JS_Size1 (ldata-get ent "JS_Size")))
							(setq JS_Size1 "")
						)
						(if (not (setq Off_Well1 (ldata-get ent "Off_Well")))
							(setq Off_Well1 "")
						)
						(if (not (setq Build_Unit (ldata-get ent "Build_Unit")))
							(setq Build_Unit "")
						)
						(if (not (setq data (ldata-get ent "Well_Memo")))
							(setq Well_Memo1 "0")	;;poplist 
							(setq Well_Memo1 (rtos (vl-position data gl_Well_Memo) 2 0))
						)
						;;------------湖州管线普查------------	end   
						 (if (> (strlen (ldata-get ent "Project")) 0)
							 (setq project (ldata-get ent "Project"))
							 )
						 (if (> (strlen (ldata-get ent "SProject")) 0)
							 (setq subproject (ldata-get ent "SProject"))
							 )
						 ; (if (> (strlen (ldata-get ent "Sur_Date")) 0)
							 ; (setq date (ldata-get ent "Sur_Date"))
							 ; )
						 (if (> (strlen (ldata-get ent "Unit")) 0)
							 (setq unit (ldata-get ent "Unit"))
							 )
						 (if (> (strlen (ldata-get ent "Location")) 0)
							 (setq place (ldata-get ent "Location"))
							 )
						 )
					 ) ;if
				 ) ;progn
			 )
			(T nil)
			) ;cond

		;;设置point_type
		(if	(setq e (assoc typename gl_TableColorList))
			(if	(setq pos (vl-position e gl_TableColorList))
				(setq point_type (rtos (vl-position e gl_TableColorList) 2 0))
				) ;if
			) ;if

		;;设置feature
		(setq featurelist (getfeaturelist-fromtype typename))
		(if	(= "" featurestr)
			(setq featurestr (GetFeaturestr (length ConnectPoints) typename)))
		(if	(setq pos (vl-position featurestr featurelist))
			(setq feature (rtos pos 2 0))) ;if

		;;设置subsid
		(setq subsidlist (getsubsidlist-fromtype typename))
		(if	(/= "" subsidstr)
			(if	(setq pos (vl-position subsidstr subsidlist))
				(setq subsid (rtos pos 2 0))
				(setq subsid "0")
				) ;if
			) ;if
        ent
		) ;defun

    ;;初始化第二点,只使用点坐标和features和subsid
    ;;ent StackList 都不能为空
    ;;返回点实体ent2
    (defun InitP2  (ent / connectpoints2 e ent1 ent2 entlist entlist2 enttype2 eobj ldata pos tablename
							edge hp1 hp2 hl)
		(setq ent2 nil
			  entLine nil)
		(if	(and (/= ent nil) (= 0 (length StackList))) ;Stacklist =nil or empty
			;;P2为非连接点
			(setq ent2 ent
				  ent1 nil
				  CurEdge nil)
			) ;if
		(if	(< 0 (length StackList))
			;;else P2为连接点,全局变量
			(setq CurEdge	  (car StackList)
				  PreEdgeList (cons CurEdge PreEdgeList)
				  ent1		  (car CurEdge)
				  ent2		  (cadr CurEdge) ;取最后加入的点
				  entLine	  (nth 2 CurEdge)
				  StackList	  (cdr StackList)) ;取出第一条边
			) ;if
		;;if p2 is a entity,fill the dlg with p2
		(if	ent2
			(progn
				(setq entlist2		 (entget ent2)
					  enttype2		 (cdr (assoc 0 entlist2))
					  pos			 (cdr (assoc 10 entlist2))

					  ;;第二点的相连点数，用于确定featurestr
					  ConnectPoints2 (GetConnectPoints (car pos) (cadr pos) "INSERT")
					  ;ConnectPoints2 (append ConnectPoints2 (GetConnectPoints (car pos) (cadr pos) "INSERT")) ;_ end_append
					  ) ;_ end_setq
				(if	(listp ConnectPoints2)
					(progn
						(setq edge nil)
						(foreach e	ConnectPoints2
							(setq hp1 (cdr (assoc 5 (entget ent2)))
								hp2 (cdr (assoc 5 (entget (car e))))
								hl (cdr (assoc 5 (entget (cadr e))))
								edge (cons (list hp1 hp2 hl) edge))
							(ldata-put (cadr e) "Edge" (list hp1 hp2 hl)) ;;线段加入边属性
					
							;;P2为非连接点
							(if	(= nil ent1)
								(setq StackList (append (list (list (cons ent2 e)) StackList)))
								;;else P2为连接点
								(if	(and (not (isInEdgeList (cadr e) StackList)) (not (isInEdgeList (cadr e) PreEdgeList)))
									(setq StackList (cons (cons ent2 e) StackList)) ;把新点加入到列表前部；first in first out
									;;(setq StackList (append StackList(list (cons ent2 e))))	;;把新点加入到队尾：first in last out
								) ;if
							) ;if
						) ;foreach
						(ldata-put ent2 "Edge" edge) ;;加入边属性,用于更新关联数据
					)
				) ;if

				; (if	(setq depth0 (ldata-get ent2 "Depth"))
					; (setq End_Deep depth0)
					; (setq End_Deep 0) ;恢复默认值
					; )
				(cond
					((= enttype2 "TEXT")
					 (setq Exp_No2 (vl-string-trim " " (cdr (assoc 1 entlist2)))
						   Map_No2 Exp_No2
						   pos	   (cdr (assoc 10 entlist2))
						   y2	   (car pos)
						   x2	   (cadr pos)
						   Surf_h2 (last pos)
							;typename (GetTypeFromPointName Exp_No2)
						   ) ;_ end_setq
					 )

					((= enttype2 "INSERT")
						(progn
							(setq pos	   (cdr (assoc 10 entlist2))
							   y2	   (car pos)
							   x2	   (cadr pos)
							   Surf_h2 (last pos)
							   ) ;_ endofsetq
							(if (vlax-ldata-get ent2 gl_AppName)
							 ;;从数据库生成的点
								(progn
									(setq ;;p1
									   ;typename	   (ldata-get ent2 "Main_Type")
									   Exp_No2	   (ldata-get ent2 "Exp_No")
									   Map_No2	   (ldata-get ent2 "Map_No")
									   ;End_Deep	   (ldata-get ent2 "Depth")
									   featurestr2 (ldata-get ent2 "Feature")
									   subsidstr2  (ldata-get ent2 "Subsid")
									   )
									   
									(if (not (setq Point_Size2 (ldata-get ent2 "Point_Size")))
										(setq Point_Size2 "")
									)
									 (if (not (setq Well_Deep2 (ldata-get ent2 "Well_Deep")))
										(setq Well_Deep2 "")
									 )
									 ;;------------湖州管线普查------------
									(if (not (setq Neck_Deep2 (ldata-get ent2 "Neck_Deep")))
										(setq Neck_Deep2 "")
									)
									(if (not (setq data (ldata-get ent2 "JG_Material")))
										(setq JG_Material2 "0")
										(setq JG_Material2 (rtos (vl-position data gl_JG_Material) 2 0))
									)
									(if (not (setq data (ldata-get ent2 "JG_Shape")))
										(setq JG_Shape2 "0")	;;poplist 0
										(setq JG_Shape2 (rtos (vl-position data gl_JG_Shape) 2 0))
									)
									(if (not (setq data (ldata-get ent2 "JS_Type")))
										(setq JS_Type2 "0")	;;poplist 
										(setq JS_Type2 (rtos (vl-position data gl_JS_Type) 2 0))
									)
									(if (not (setq JS_Size2 (ldata-get ent2 "JS_Size")))
										(setq JS_Size2 "")
									)
									(if (not (setq Off_Well2 (ldata-get ent2 "Off_Well")))
										(setq Off_Well2 "")
									)
									(if (not (setq data (ldata-get ent2 "Well_Memo")))
										(setq Well_Memo2 "0")	;;poplist 
										(setq Well_Memo2 (rtos (vl-position data gl_Well_Memo) 2 0))
									)
									(if (not (setq Build_Unit (ldata-get ent2 "Build_Unit")))
										(setq Build_Unit "")
									)
									;;------------湖州管线普查------------	end      
									 (if (and (= 0 strlen project)(> (strlen (ldata-get ent2 "Project")) 0))
										 (setq project (ldata-get ent2 "Project"))
										 )
									 (if (and (= 0 strlen subproject)(> (strlen (ldata-get ent2 "SProject")) 0))
										 (setq subproject (ldata-get ent2 "SProject"))
										 )
									 ; (if (and (= 0 strlen date)(> (strlen (ldata-get ent2 "Sur_Date")) 0))
										 ; (setq date (ldata-get ent2 "Sur_Date"))
										 ; )
									 (if (and (= 0 strlen unit)(> (strlen (ldata-get ent2 "Unit")) 0))
										 (setq unit (ldata-get ent2 "Unit"))
										 )
									 (if (and (= 0 strlen place)(> (strlen (ldata-get ent2 "Location")) 0))
										 (setq place (ldata-get ent2 "Location"))
										 )
								 )
							 )
						 ) ;progn
					 )
					(T nil)
					) ;cond
				) ;progn
			) ;if

		;;设置feature,不能为空
		(if	(= "" featurestr2)
			(setq featurestr2 (GetFeaturestr (length ConnectPoints2) typename)))
		(if	(setq pos (vl-position featurestr2 featurelist))
			(setq feature2 (rtos pos 2 0))) ;if

		;;设置subsid	
		(if	(/= nil subsidstr2)
			(if	(setq pos (vl-position subsidstr2 subsidlist))
				(setq subsid2 (rtos pos 2 0))
				) ;if
			) ;if
		;;global var
		(if entl1 (setq entP1 ent1))
		(setq entP2 ent2)
		ent2
		) ;defun

;;;保存数据到图形文件:保存到点对象ldata，保存到数据列表，并在保存到数据库的时候保存到数据库
;;;参数：P1 P2的实体，，则创建新的INSERT，并删除原对象
;;;entP1 不可为nil ，entP2可以为nil
;;;返回：
    (defun Dlg_SavetoDWG  (/ newp1 newp2 bUpdate newinfo strname olddata newdata )
		;;save p1
		;;get text of value in a list
		
		(setq newp1	(SaveEditPoint entP1 point_type	subsid feature Exp_No Map_No y x Surf_h	Start_Deep Point_Size1 Well_Deep1 unit date subproject project place Neck_Deep1 JG_Material1 JG_Shape1 JS_Size1 JS_Type1 Off_Well1 Well_Memo1 Build_Unit))
		(if	newp1
			(setq entP1 newp1))

		(if entP2
			(if(setq newp2	(SaveEditPoint entP2 point_type	subsid2	feature2 Exp_No2 Map_No2 y2	x2 Surf_h2 End_Deep Point_Size2 Well_Deep2 unit date subproject	project	place Neck_Deep2 JG_Material2 JG_Shape2 JS_Size2 JS_Type2 Off_Well2 Well_Memo2 Build_Unit))
				(setq entP2 newp2)
			)
		)
		
		(if (and entP1 entP2)
			(setq entLine (SaveEditLine entLine entP1 entP2 typename D_S Flowdirect Pressure Voltage (nth (atoi Material) gl_Material_List) Cab_Count
							Hole_Count Hole_Used Road_Name BuryWay PaiWu unit date subproject project place Line_Style Line_Memo Build_Unit Block_Size1 Block_Size2))
		)
	) ;defun
    
	;;保存对话框中的直线
	(defun SaveEditLine  (entl entp1 entp2 typename d_s flowdirect pressure voltage material 
						  Cab_Count holecount holeused Road_Name BuryWay paiwu unit date subproject  project place Line_Style Line_Memo Build_Unit Block_Size1 Block_Size2
						  /  createline d1 d2  newline oldpos1 oldpos2 outlist p10 p11 pos1 pos2 PaiWustr)
		(if (or (= nil entp1) (= nil entp2)) (exit))
		(setq oldPos1 nil
			  oldPos2 nil
			  newline nil
			  createline nil ;是否要创建新的线段
			  pos1 (cdr (assoc 10 (entget entp1)))
			  pos2 (cdr (assoc 10 (entget entp2)))
			  ;p10 (list (car pos1) (cadr pos1) (- (caddr pos1) (ldata-get entp1 "Depth")))
			  ;p11 (list (car pos2) (cadr pos2) (- (caddr pos2) (ldata-get entp2 "Depth")))
			  p10 (list (car pos1) (cadr pos1) (- (caddr pos1) Start_Deep))
			  p11 (list (car pos2) (cadr pos2) (- (caddr pos2) End_Deep))
			  ) 
		(if (= 'ENAME (type entl ))
			(setq linelist (entget entl)
				oldPos1 (cdr (assoc 10 linelist))
				oldPos2 (cdr (assoc 11 linelist))
			)
		)
		;;保存或创建线段
            	(if (not (vlax-ldata-get entl gl_AppName))
                    (setq createline 1)
                    )
	  	;;两点之间无线段
		(if	(and (= entl nil) entp1 entp2)
			(setq createline 1))
		;;点位置变动
		(if	(and (= nil createline) (/= nil oldPos1))
			(if	(not (equal oldPos1 p10))
				(progn
					(setq linelist (subst (cons 10 p10) (cons 10 oldPos1) linelist))
					(if (not (entmod linelist))
						(setq createline 1)
					)
				)
			)
		)
		(if	(and (= nil createline) (/= nil oldPos2))
			(if	(not (equal oldPos2 p11))
				(progn
					(setq linelist (subst (cons 11 p11) (cons 11 oldPos2) linelist))
					(if (not (entmod linelist))
						(setq createline 1)
					)
				)
			)
		)
		;;属性列表
		(cond 
			((= PaiWu "0") (setq PaiWustr "是"))
			((= PaiWu "1") (setq PaiWustr "否"))
			((= PaiWu "2") (setq PaiWustr "无"))
			(T (setq PaiWustr "无"))
		)
		(setq outlist nil
			outlist (cons (cons "Start_Point" (ldata-get entp1 "Map_No")) outlist)
			outlist (cons (cons "End_Point" (ldata-get entp2 "Map_No")) outlist)
			outlist (cons (cons "Start_Deep" Start_Deep) outlist)
			outlist (cons (cons "End_Deep" End_Deep) outlist)
			
			outlist (cons (cons "Material" material) outlist)
			outlist (cons (cons "D_S" d_s) outlist)
			outlist (cons (cons "Cab_Count" cab_count) outlist)
			outlist (cons (cons "Main_Type" typename) outlist)
			outlist (cons (cons "Hole_Count" holecount) outlist)
			outlist (cons (cons "Hole_Used" holeused) outlist)
			outlist (cons (cons "Pressure" pressure) outlist)
			outlist (cons (cons "Voltage" voltage) outlist)
			outlist (cons (cons "Flowdirect" flowdirect) outlist)
			outlist (cons (cons "Road_Name" Road_Name) outlist)
			outlist (cons (cons "PaiWu" PaiWustr) outlist)
			outlist (cons (cons "Sur_Date" date) outlist)
			;;outlist (cons (cons "Mdate" date) outlist) ;;4.0版对话框无对应参数
			outlist (cons (cons "Unit" unit) outlist)
			outlist (cons (cons "Location" place) outlist)
			outlist (cons (cons "SProject" subproject) outlist)
			outlist (cons (cons "Project" project) outlist)
			outlist (cons (cons gl_AppName gl_Version) outlist)
		)
		;;---------------湖州管线普查-------------------
		(if Line_Style (setq outlist (cons (cons "Line_Style" (nth (atoi Line_Style) gl_Line_Style)) outlist)))
		(if BuryWay (setq outlist (cons (cons "BuryWay" (nth (atoi BuryWay) gl_BuryWay)) outlist)))
		(if Line_Memo (setq outlist (cons (cons "Line_Memo" Line_Memo) outlist)))
		(if Build_Unit (setq outlist (cons (cons "Build_Unit" Build_Unit) outlist)))
		(if Block_Size1 (setq outlist (cons (cons "Block_Size1" Block_Size1) outlist)))
		(if Block_Size2 (setq outlist (cons (cons "Block_Size2" Block_Size2) outlist)))
		;;-----------------湖州管线end------------------
		(if createline
			(setq newline (AddNewLine p10 p11 typename outlist)
			)
			(if entl
				(setq entl (PutEntityAttribs entl outlist))
			)
		)
		;;删除原有线段
		(if	(and newline entl) ;全局变量，InitP2
			(progn
				(setq StackList	(RebulidStackList entl newline StackList)
					PreEdgeList (RebulidStackList entl newline PreEdgeList)
					gl_ErrorList (RebulidErrorList entl newline gl_ErrorList))
				(RebulidEdges entl newline)
				;;更新索引V5.1
				(DelEntity entl)
			) ;progn
		) ;if
		(if (= newline nil) (setq newline entl))
		;;标注文字，显示内容由配置文件决定
		(if	newline
			(progn
				(SetDefaultLayer newline typename)
				(UpdateLabel newline Start_Deep End_Deep)
			)
		)
		;;设置AppID 用于ssget
		(setxdata newline gl_AppName (list (cons 1000 gl_Version))) 
		newline
	)
    ;;;保存单个点
    ;;;若点对象存在，则建立新点块，并删除旧点对象
    ;;;返回值：点块实体 or nil
    (defun SaveEditPoint  (entityPoint typestring subsidstring  featurestring  
					ExpNo  MapNo xpos ypos zpos depth pointsize welldeep unit date subproject 
					project place  Neck_Deep JG_Material JG_Shape JS_Size JS_Type Off_Well Well_Memo Build_Unit 
					/ createpoint dist0 dist1 e el elist enddeep enttype expold fstr fstrold mapold newent
					oldpos outlist p1 p10 p11 pos sstr sstrold startdeep tstr)
		(if (= 'ENAME (type entityPoint))
			(setq enttype (cdr (assoc 0 (entget entityPoint))))
			(exit)
		)
		(setq newent nil
              createpoint nil
			  pos (list xpos ypos zpos)
			  oldpos (cdr (assoc 10 (entget entityPoint)))
			  dist0 (distance pos oldpos)
			  ;dist0 (distance (list xpos ypos) (list (car oldpos) (cadr oldpos)))
		)
		;;更新管线点
        (if (and (/= "" MapNo) (/= "" ExpNo))
            (progn
                (setq fstr (nth (atoi featurestring) featurelist) ;feature
                      sstr (nth (atoi subsidstring) subsidlist) ;subsid
                      tstr (substr (nth (atoi typestring) typelist) 1 1) ;type "J"
                      )
                ;;点中保存最大的深度值，兼容性3.4之前版本考虑,
				;;错误输入最大值时，将无法修改过来。
				; (setq maxdepth nil)
				; (if (= "INSERT" enttype)
					; (if (vlax-ldata-get entityPoint gl_AppName)
						; (if (setq maxdepth (ldata-get entityPoint "Depth"))
							; (if (< maxdepth depth)
								; (setq maxdepth depth ) ;;最大深度值
							; )
						; )
					; )
				; )	
                ;;创建点块,新建点块，并删除原对象
				(setq outlist nil
					outlist (cons (cons "Exp_No" ExpNo) outlist)
					outlist (cons (cons "Map_No" MapNo) outlist)
					;outlist (cons (cons "Depth" depth) outlist)
					outlist (cons (cons "Subsid" sstr) outlist)
					outlist (cons (cons "Feature" fstr) outlist)
					outlist (cons (cons "Main_Type" tstr) outlist)
					outlist (cons (cons "Well_Deep" welldeep) outlist)
					
					;;outlist (cons (cons "Sur_Date" date) outlist)
					outlist (cons (cons "Unit" unit) outlist)
					outlist (cons (cons "Location" place) outlist)
					outlist (cons (cons "SProject" subproject) outlist)
					outlist (cons (cons "Project" project) outlist)
					;;运行时会修改边属性
					outlist (cons (cons "Edge" (ldata-get entityPoint "Edge")) outlist)
				)
				;; Neck_Deep JG_Material JG_Shape JS_Size JS_Type Off_Well Well_Memo
				(if Neck_Deep (setq outlist (cons (cons "Neck_Deep" Neck_Deep) outlist)))
				(if JG_Material (setq outlist (cons (cons "JG_Material" (nth (atoi JG_Material) gl_JG_Material)) outlist)))
				(if JG_Shape (setq outlist (cons (cons "JG_Shape" (nth (atoi JG_Shape) gl_JG_Shape)) outlist)))
				(if JS_Size (setq outlist (cons (cons "JS_Size" JS_Size) outlist)))
				(if pointsize (setq outlist (cons (cons "Point_Size" pointsize) outlist)))
				(if JS_Type (setq outlist (cons (cons "JS_Type" (nth (atoi JS_Type) gl_JS_Type)) outlist)))
				(if Off_Well (setq outlist (cons (cons "Off_Well" Off_Well) outlist)))
				(if Well_Memo (setq outlist (cons (cons "Well_Memo" (nth (atoi Well_Memo) gl_Well_Memo)) outlist)))
				(if Build_Unit (setq outlist (cons (cons "Build_Unit" Build_Unit) outlist)))
				
				(if (= "TEXT" enttype)
					(setq newent (AddNewPoint pos ExpNo  tstr fstr sstr outlist))
				)
				(if (= "INSERT" enttype)
					(if (vlax-ldata-get entityPoint gl_AppName)
						(progn
							;;点特征或附属物变化,位置变化 创建新对象
							(setq fstrold (ldata-get entityPoint "Feature")
								sstrold (ldata-get entityPoint "Subsid")
								mapold (ldata-get entityPoint "Map_No")
								expold (ldata-get entityPoint "Exp_No"))
							;;feature or subsid changed,create a new block
							(if (or   (/= fstrold fstr) (/= sstrold sstr))
								(setq newent (AddNewPoint pos ExpNo  tstr fstr sstr outlist))
								(PutEntityAttribs entityPoint outlist)
							)
							;;mapname or expname change ,just change the properties and line startp or endpoint
							(if (/= mapold MapNo)
								(progn
									(SetAttrib-PointName entityPoint "点号" MapNo)
									(ldata-put entityPoint "Exp_No" ExpNo)
									;;update mapnamelist
									(setq gl_MapNameList (subst MapNo mapold gl_MapNameList))
									;;update lines Start_Point or End_Point
									(foreach e (ldata-get entityPoint "Edge")
										(setq el (handent (caddr e)))
										(if (= mapold (ldata-get el "Start_Point")) (ldata-put el "Start_Point" MapNo))
										(if (= mapold (ldata-get el "End_Point")) (ldata-put el "End_Point" MapNo))
									)
								)
							)
						)
					)
				)
				; (if (and maxdepth newent) (ldata-put newent "Depth" maxdepth))
				; (if (and maxdepth entityPoint) (ldata-put entityPoint "Depth" maxdepth))
				;;更新句柄
				(if  (and newent entityPoint)
					(progn
						(setq StackList (RebulidStackList  entityPoint newent StackList)
							PreEdgeList (RebulidStackList entityPoint newent PreEdgeList)
							gl_ErrorList (RebulidErrorList entityPoint newent gl_ErrorList))
						(RebulidEdges entityPoint newent)
						
						;;更新索引V5.1
						(DelEntity entityPoint)
						
					) ;progn
				) ;if
				
				(if (= nil newent ) (setq newent entityPoint))
            ) ;progn
        ) ;if
		;;如果点坐标变化,则需要修改关联线段的端点
		;;!!!坐标变化,一个原因是对话框控件中的值是实际坐标转为文本之后的值
		(if (> dist0 0)
			(progn
				;;此处使用 vla-move 导致错误，newent可能是新建的实体，坐标已经是最新；
				;;(vla-move (vlax-ename->vla-object newent) (vlax-3D-point oldpos) (vlax-3D-point pos))
				(entmod (subst (cons 10 pos) (cons 10 oldpos) (entget newent)))
				;;update spaceindex v5.3
				(if gl_PointSpaceIndex
					(setq gl_PointSpaceIndex (UpdatePointIndex gl_PointSpaceIndex newent oldpos pos))
				)
				
				(foreach e (ldata-get newent "Edge")
					(setq el (handent (caddr e))
						elist (entget el)
						p10 (cdr (assoc 10 elist))
						p11 (cdr (assoc 11 elist))
						dist1 (distance (list (car p10) (cadr p10)) (list (car oldpos) (cadr oldpos)))
					)
					;;注意管线的深度
					(if (not (setq startdeep (ldata-get el "Start_Deep")))
						(setq startdeep 0)
					)
					(if (not (setq enddeep (ldata-get el "End_Deep")))
						(setq enddeep 0)
					)
					(UpdateLabel el (ldata-get el "Start_Deep") (ldata-get el "End_Deep"))
					(if (< dist1 gl_MIN) 
						(setq p1 (list (car p10) (cadr p10) (- (caddr pos) startdeep))
							elist (subst (cons 10 p1) (assoc 10 elist) elist);p10端点连接当前点,修改之
							gl_LineSpaceIndex (UpdatePointIndex gl_LineSpaceIndex el p10 pos)
						)
						(setq p1 (list (car p11) (cadr p11) (- (caddr pos) enddeep))
							elist (subst (cons 11 p1) (assoc 11 elist) elist);p11端点连接当前点,修改之
							gl_LineSpaceIndex  (UpdatePointIndex gl_LineSpaceIndex el p11 pos)
						)	
					)
					(entmod elist)
					;;update spaceindex v5.3
				); 
				
			)
		)
	newent
    ) ;

    ;;删除实体,并新建实体时，需要更新StackList
    ;;(oldent 删除的实体  newent 新建的实体 StackList)
    (defun RebulidStackList (oldent newent  slist / e p1 p2 l)
        (if (and (/= nil newent) (/= nil oldent) (/= nil slist))
            (foreach e slist
                (setq p1 (car e)
                      p2 (cadr e)
                      l (nth 2 e))
                (if (= p1 oldent)
                    (setq slist (subst (list newent p2 l) e slist)))
                (if (= p2 oldent)
                    (setq slist (subst (list p1 newent l) e slist)))
                (if (= l oldent)
                    (setq slist (subst (list p1 p2 newent) e slist)))
                );foreach
             );if
        slist
        )

    ;;;初始化线段
	;;;必须在初始化点之后进行
    (defun InitLine (entl / Start_Point End_Point tmp Start_Deep1 End_Deep1 data pos)
        (if entl
            (if (vlax-ldata-get entl gl_AppName)
				(progn
					(setq 
						Start_Point (ldata-get entl "Start_Point")
						End_Point 	(ldata-get entl "End_Point")
						Flowdirect (ldata-get entl "Flowdirect")
					)
					;;下列值若为空,则保留对话框原来的值
					(if (setq data (ldata-get entl "D_S")) 
						(if (> (strlen data) 0) (setq D_S data)))
					(if  (setq data (ldata-get entl "Pressure")) 
						(if (> (strlen data) 0) (setq Pressure data)))
					(if  (setq data (ldata-get entl "Voltage")) 
						(if (> (strlen data) 0) (setq Voltage data)))
					(if  (setq data (ldata-get entl "Material")) 
						(if (> (strlen data) 0) (setq Material (rtos (vl-position data gl_Material_List) 2 0))))
					(if  (setq data (ldata-get entl "Cab_Count")) 
						(if (> (strlen data) 0) (setq Cab_Count data)))
					(if  (setq data (ldata-get entl "Hole_Used")) 
						(if (> (strlen data) 0) (setq Hole_Used data)))
					(if  (setq data (ldata-get entl "Hole_Count")) 
						(if (> (strlen data) 0) (setq Hole_Count data)))
					(if  (setq data (ldata-get entl "Road_Name")) 
						(if (> (strlen data) 0) (setq Road_Name data)))
						
					;;--------------湖州管线普查5.0---------------
					(if (setq data (ldata-get entl "Line_Memo")) 
						(if (> (strlen data) 0) (setq Line_Memo data)))
					(if (setq data (ldata-get entl "Line_Style")) 
						(if (> (strlen data) 0)
							(if (setq pos (vl-position data gl_Line_Style)) 
								(setq Line_Style (rtos pos  2 0))
								(setq Line_Style "0")
							)
							(setq Line_Style "0")
						)
						(setq Line_Style "0")
					)
					(if (setq data (ldata-get entl "BuryWay")) 
						(if (> (strlen data) 0)
							(if (setq pos (vl-position data gl_BuryWay)) 
								(setq BuryWay (rtos pos  2 0))
								(setq BuryWay "0")
							)
							(setq BuryWay "0")
						)
						(setq BuryWay "0")
					)
					; (if  (setq data (ldata-get entl "BuryWay")) 
						; (if (> (strlen data) 0) (setq BuryWay (rtos (vl-position data gl_BuryWay) 2 0)))
						; (setq BuryWay "0")
					; )
					(if (setq data (ldata-get entl "Build_Unit"))
						(if (> (strlen data) 0) (setq Build_Unit data ))
					)
					;;设置深度
					(if  (setq data (ldata-get entl "Start_Point"))
						(progn
							(if (and (> (strlen data) 0) (or (= data Exp_No) (= data Map_No)))
								(progn 
									(if  (setq data (ldata-get entl "Start_Deep")) (setq Start_Deep data))
									(if  (setq data (ldata-get entl "End_Deep")) (setq End_Deep data))
									(if (setq data (ldata-get entl "Block_Size2")) ;;终点管块size
										(if (> (strlen data) 0) (setq Block_Size2 data)))
									(if (setq data (ldata-get entl "Block_Size1")) 		;;起点管块size
										(if (> (strlen data) 0) (setq Block_Size1 data)))	
								)
								(progn 
									(if  (setq data (ldata-get entl "Start_Deep")) (setq End_Deep data))
									(if  (setq data (ldata-get entl "End_Deep")) (setq Start_Deep data))
									(if (setq data (ldata-get entl "Block_Size2")) ;;终点管块size
										(if (> (strlen data) 0) (setq Block_Size1 data)))
									(if (setq data (ldata-get entl "Block_Size1")) 		;;起点管块size
										(if (> (strlen data) 0) (setq Block_Size2 data)))	
								)
							)
						)
					)
					
					;;paiwu
					(if (setq tmp (ldata-get entl "PaiWu"))
						(cond 
							((= tmp "是")(setq PaiWu "0"))
							((= tmp "否") (setq PaiWu "1"))
							((= tmp "无") (setq PaiWu "2"))
							( T (setq PaiWu "2"))
						)
					)
					
					(if (= 0 (strlen project))
						(if (> (strlen (setq data (ldata-get entl "Project"))) 0)
							(setq project data)
						 )
					)	 
					(if (= 0 (strlen subproject))
						(if (> (strlen (setq data (ldata-get entl "SProject"))) 0)
							(setq subproject data)
						)
					)	 
					 (if (= 0 (strlen date))
						(if (> (strlen (setq data (ldata-get entl "Sur_Date"))) 0)
							(setq date data)
						)
					)	
					 (if (= 0 (strlen unit))
						(if (> (strlen (setq data (ldata-get entl "Unit"))) 0)
						 (setq unit data)
						 )
					) 
					 (if (= 0 (strlen place))
						(if (> (strlen (setq data (ldata-get entl "Location"))) 0)
						 (setq place data)
						 )
					)	 

                    ;;线段的方向不是P1->P2
                    (if (and (= End_Point Map_No) (= Start_Point Map_No2))
                        (progn
;;;                            (setq tmp Start_Deep1
;;;                                  Start_Deep1 End_Deep1
;;;                                  End_Deep1 tmp
;;;                            ) ;_ End_setq
                            (if (= 1 Flowdirect)
                                (setq Flowdirect 2)
								(if (= 2 Flowdirect)
									(setq Flowdirect 1)
								) ;_ End_if
                            ) ;_ End_if
                            
                        ) ;progn
                    ) ;if
                ) ;progn
            ) ;if
        ) ;_ End_if

    ) ;_ End_defun

    ;;判断直线段是否在边 PreEdgeList或StackList中
 (defun isInEdgeList (entline pelist / result e)
     (setq result nil)
     (if (and entline pelist)
         (foreach e pelist
             (if (eq (nth 2 e) entline)
                 (setq result T)
             ) ;_ end_if
         ) ;_ end_foreach
     ) ;_ end_if
     result
 ) ;_ end_defun

 ;;编辑点号时,查询是否是唯一点号
 ;;pname 点的名字Map_No Exp_No，val 编辑框中的文字，NameType :"EXP" "MAP" 类别
	(defun OnEditMapName (val reason pname NameType / newname )
		(if (and (= 2 reason) (/= val pname))
			(progn
				(if (= NameType "EXP")	;;物探点号
					(if (and (/= val pname) (HasSameExpName val))
						(progn
							(setq newname (CreateNewPointName val typename NameType))
							(alert (strcat "管线点" val "已经存在!\n建议新的点号为：" newname))
						)
					)
				)
				(if (= NameType "MAP")	;;图上点号
					(if (and (/= val pname)(HasSameMapName val))
						(progn
							(setq newname (CreateNewPointName val typename NameType))
							(alert (strcat "管线点" val "已经存在!\n建议新的点号为：" newname))
						)
					)
				)
			)	
		)
	)
	
	;;保存项目信息为默认
	(defun OnSaveProject ( / bUpdate newinfo e strname olddata newdata projectinfo)
		;;更新项目信息
		(setq bUpdate nil)
		(setq projectinfo (ReadProjectInfo nil))
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
	
	;;载入项目信息
	(defun OnLoadProject ( / ProjectInfoList)
		(if (setq ProjectInfoList (ReadProjectInfo nil))
			(progn
				(setq unit (cdr (assoc "Unit" ProjectInfoList))
					date (cdr (assoc "Sur_Date" ProjectInfoList))
					place (cdr (assoc "Location" ProjectInfoList))
					subproject (cdr (assoc "SProject" ProjectInfoList))
					project	(cdr (assoc "Project" ProjectInfoList))
				)
				 ;;project
				(set_tile "unit" unit)
				(set_tile "date" date)
				(set_tile "place" place)
				(set_tile "subproject" subproject)
				(set_tile "project" project)
			)
		)
	)
	
	;;属性复制
	(defun OnCopyP1_P2 ()
		(set_tile "feature2" (get_tile "feature"))
		(set_tile "subsid2" (get_tile "subsid"))
		(set_tile "Well_Deep2" (get_tile "Well_Deep1"))
		(set_tile "text-PointSize2" (get_tile "text-PointSize1"))
		(set_tile "JS_Size2" (get_tile "JS_Size1"))
		(set_tile "Off_Well2" (get_tile "Off_Well1"))
		(set_tile "Well_Memo2" (get_tile "Well_Memo1"))
		(set_tile "Neck_Deep2" (get_tile "Neck_Deep1"))
		(set_tile "JG_Shape2" (get_tile "JG_Shape1"))
		(set_tile "JG_Material2" (get_tile "JG_Material1"))
		(set_tile "JS_Type2" (get_tile "JS_Type1"))
		(set_tile "Block_Size2" (get_tile "Block_Size1"))
	)
	(defun OnCopyP2_P1 ()
		(set_tile "feature" (get_tile "feature2"))
		(set_tile "subsid" (get_tile "subsid2"))
		(set_tile "Well_Deep1" (get_tile "Well_Deep2"))
		(set_tile "text-PointSize1" (get_tile "text-PointSize2"))
		(set_tile "JS_Size1" (get_tile "JS_Size2"))
		(set_tile "Off_Well1" (get_tile "Off_Well2"))
		(set_tile "Well_Memo1" (get_tile "Well_Memo2"))
		(set_tile "Neck_Deep1" (get_tile "Neck_Deep2"))
		(set_tile "JG_Shape1" (get_tile "JG_Shape2"))
		(set_tile "JG_Material1" (get_tile "JG_Material2"))
		(set_tile "JS_Type1" (get_tile "JS_Type2"))
		(set_tile "Block_Size1" (get_tile "Block_Size2"))
	)
	(defun OnCopyL1_Lcur ( / entl data tmp)
		(if (> (length PreEdgeList) 1)
			(progn
				(setq entl (caddr (cadr PreEdgeList)))
				;;下列值若为空,则保留对话框原来的值
				(if (setq data (ldata-get entl "D_S")) 
					(if (> (strlen data) 0) (setq D_S data)))
				(if  (setq data (ldata-get entl "Pressure")) 
					(if (> (strlen data) 0) (setq Pressure data)))
				(if  (setq data (ldata-get entl "Voltage")) 
					(if (> (strlen data) 0) (setq Voltage data)))
				(if  (setq data (ldata-get entl "Material")) 
					(if (> (strlen data) 0) (setq Material (rtos (vl-position data gl_Material_List) 2 0))))
				(if  (setq data (ldata-get entl "Cab_Count")) 
					(if (> (strlen data) 0) (setq Cab_Count data)))
				(if  (setq data (ldata-get entl "Hole_Used")) 
					(if (> (strlen data) 0) (setq Hole_Used data)))
				(if  (setq data (ldata-get entl "Hole_Count")) 
					(if (> (strlen data) 0) (setq Hole_Count data)))
				(if  (setq data (ldata-get entl "Road_Name")) 
					(if (> (strlen data) 0) (setq Road_Name data)))
				(if  (setq data (ldata-get entl "Flowdirect")) 
					(setq Flowdirect data))	
				;;--------------湖州管线普查5.0---------------
				(if (setq data (ldata-get entl "Line_Memo")) 
					(if (> (strlen data) 0) (setq Line_Memo data)))
				(if (setq data (ldata-get entl "Line_Style")) 
					(if (> (strlen data) 0)
						(if (setq pos (vl-position data gl_Line_Style)) 
							(setq Line_Style (rtos pos  2 0))
							(setq Line_Style "0")
						)
						(setq Line_Style "0")
					)
					(setq Line_Style "0")
				)
				(if (setq data (ldata-get entl "BuryWay")) 
					(if (> (strlen data) 0)
						(if (setq pos (vl-position data gl_BuryWay)) 
							(setq BuryWay (rtos pos  2 0))
							(setq BuryWay "0")
						)
						(setq BuryWay "0")
					)
					(setq BuryWay "0")
				)
				(if (setq data (ldata-get entl "Build_Unit"))
					(if (> (strlen data) 0) (setq Build_Unit data ))
				)
				;;设置深度
				(if  (setq data (ldata-get entl "Start_Point"))
					(progn
						(if (and (> (strlen data) 0) (or (= data Exp_No) (= data Map_No)))
							(progn 
								(if  (setq data (ldata-get entl "Start_Deep")) (setq Start_Deep data))
								(if  (setq data (ldata-get entl "End_Deep")) (setq End_Deep data))
								(if (setq data (ldata-get entl "Block_Size2")) ;;终点管块size
									(if (> (strlen data) 0) (setq Block_Size2 data)))
								(if (setq data (ldata-get entl "Block_Size1")) 		;;起点管块size
									(if (> (strlen data) 0) (setq Block_Size1 data)))	
							)
							(progn 
								(if  (setq data (ldata-get entl "Start_Deep")) (setq End_Deep data))
								(if  (setq data (ldata-get entl "End_Deep")) (setq Start_Deep data))
								(if (setq data (ldata-get entl "Block_Size2")) ;;终点管块size
									(if (> (strlen data) 0) (setq Block_Size1 data)))
								(if (setq data (ldata-get entl "Block_Size1")) 		;;起点管块size
									(if (> (strlen data) 0) (setq Block_Size2 data)))	
							)
						)
					)
				)
				
				;;paiwu
				(if (setq tmp (ldata-get entl "PaiWu"))
					(cond 
						((= tmp "是")(setq PaiWu "0"))
						((= tmp "否") (setq PaiWu "1"))
						((= tmp "无") (setq PaiWu "2"))
						( T (setq PaiWu "2"))
					)
				)
				
				(if (= 0 (strlen project))
					(if (> (strlen (setq data (ldata-get entl "Project"))) 0)
						(setq project data)
					 )
				)	 
				(if (= 0 (strlen subproject))
					(if (> (strlen (setq data (ldata-get entl "SProject"))) 0)
						(setq subproject data)
					)
				)	 
				 (if (= 0 (strlen date))
					(if (> (strlen (setq data (ldata-get entl "Sur_Date"))) 0)
						(setq date data)
					)
				)	
				 (if (= 0 (strlen unit))
					(if (> (strlen (setq data (ldata-get entl "Unit"))) 0)
					 (setq unit data)
					 )
				) 
				 (if (= 0 (strlen place))
					(if (> (strlen (setq data (ldata-get entl "Location"))) 0)
					 (setq place data)
					 )
				)	 
				;;
				;;line
				(if D_S (set_tile "D_S" D_S))
				(if Flowdirect (set_tile "Flowdirect" (rtos Flowdirect 2 0)))
				(if Pressure (set_tile "Pressure" Pressure))
				(if Voltage (set_tile "Voltage" Voltage))
				(if Material (set_tile "Material" Material))
				(if Cab_Count (set_tile "Cab_Count" Cab_Count))
				(if Hole_Used (set_tile "Hole_Used" Hole_Used))
				(if Hole_Count (set_tile "Hole_Count" Hole_Count))
				(if Start_Deep (set_tile "Start_Deep" (rtos Start_Deep 2 4)))
				(if End_Deep (set_tile "End_Deep" (rtos End_Deep 2 4)))
				(if Road_Name (set_tile "Road_Name" Road_Name))
				(if PaiWu (set_tile "PaiWu" PaiWu))
				(if BuryWay (set_tile "BuryWay" BuryWay))
				;;---------------湖州管线普查------------
				(if Line_Style (set_tile "LineStyle" Line_Style))
				(if Line_Memo (set_tile "Line_Memo" Line_Memo))
				(if Build_Unit (set_tile "Build_Unit" Build_Unit))
				;;两端截面size
				(if Block_Size1 (set_tile "Block_Size1" Block_Size1))
				(if Block_Size2 (set_tile "Block_Size2" Block_Size2))
				;;project
				(if unit (set_tile "unit" unit))
				(if date (set_tile "date" date))
				(if place (set_tile "place" place))
				(if subproject (set_tile "subproject" subproject))
				(if project (set_tile "project" project))
			)
		)
	)
;; ---------------------------end of functions define----------------------------------

;;-------------------------------------------------------------------------------------
;;-----beginning of dlg code-----------------------------------------------------------
    (LineInfo_GetSupportPath)  
	;;***************************************
    ;;根据实体类型和参数，设置终点P2控件和entLine  
    (InitP1 ent1)
    (setq entP2 (InitP2 ent2))
    ;;if entLine is not nil ,fill the line in dlg with entline
	;;设置线段初始值
	(setq D_S ""
		  Flowdirect 0
		  Pressure ""
		  Voltage ""
		  Material "0"
		  Cab_Count ""
		  Hole_Used "" 
		  Hole_Count  ""
		  Road_Name ""
		  PaiWu "2"
	      BuryWay "0"
	) ;_ End_setq
	;;埋设方式  gl_BuryWay (list "0-直埋" "1-矩形管沟" "2-圆形管沟" "3-拱形管沟" "4-人防" "5-管块" "6-套管" "7-小通道(d<1m)" "8-地上(架空)" "9-井内")
	(if (or (= typename "Z") (= typename "P") (= typename "Y") (= typename "W")  (= typename "L"))
		(setq BuryWay "2")
	)
	(if (= typename "X") 
		(setq BuryWay "5")
	)
	(if (= typename "H") 
		(setq BuryWay "0")
	)
    (InitLine entLine)

    (if (and (/= 0 x2) (/= 0 y2))
        (ZoomWndtoLeft (list y x) (list y2 x2))
        (ZoomWndtoLeft (list (+ 5 y) (+ 5 x))
                       (list (- y 5) (- x 5))
        ) ;_ End_ZoomWndtoLeft
    ) ;
	
	;;设置项目信息的默认值
	; (if (setq ProjectInfoList (ReadProjectInfo nil))
		; (setq unit (cdr (assoc "Unit" ProjectInfoList))
			; date (cdr (assoc "Sur_Date" ProjectInfoList))
			; place (cdr (assoc "Location" ProjectInfoList))
			; subproject (cdr (assoc "SProject" ProjectInfoList))
			; project	(cdr (assoc "Project" ProjectInfoList))
		; )
	; )
    (setq id (load_dialog (strcat gl_INFO_LINE_PATH "dlg\\editpoint-huzhou.dcl")
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
    (setq DLG_WIDTH 724
          DLG_HEIGHT 636
          windpoint (list (- width (+ 10 DLG_WIDTH)) 30)
        )
  
    (setq std 4) ;0 cancel 1 ok 2 di-biao 3 shejixian
    (while (> std 1)
        (if (not (new_dialog "editpointdlg" id "" windpoint))
            (exit)
        ) ;初始化对话框

        ;;设置typepoint poplist内容:J-给水
        (if (/= typelist nil)
            (progn
                (start_list "point_type" 3)
                (mapcar 'add_list typelist)
                (end_list)
            ) ;progn
        ) ;if


        ;;设置feature poplist内容    
        (start_list "feature" 3 )
        (mapcar 'add_list featurelist)
        (end_list)
        (start_list "feature2" 3 )
        (mapcar 'add_list featurelist)
        (end_list)
        ;;设置subsid poplist内容
        (start_list "subsid" 3 )
        (mapcar 'add_list subsidlist)
        (end_list)
        (start_list "subsid2" 3 )
        (mapcar 'add_list subsidlist)
        (end_list)
		
		(start_list "Material" 3 )
        (mapcar 'add_list gl_Material_List)
        (end_list)

        ;;设置控件的值
        ;;p1
        (if point_type (set_tile "point_type" point_type))
        (if Exp_No (set_tile "Exp_No" Exp_No))
        (if Map_No (set_tile "Map_No" Map_No))
        (if x (set_tile "x" (rtos x 2 4)))
        (if y (set_tile "y" (rtos y 2 4)))
        (if Surf_h(set_tile "Surf_h" (rtos Surf_h 2 4)))
		(if Point_Size1 (set_tile "text-PointSize1" Point_Size1))
		
		;;2015-9-25
		(if Well_Deep1(set_tile "Well_Deep1" Well_Deep1))

        (if feature(set_tile "feature" feature))
        (if subsid (set_tile "subsid" subsid))
		;;---------------湖州管线普查------------
		(if Neck_Deep1 (set_tile "Neck_Deep1" Neck_Deep1))
		(if JG_Material1 (set_tile "JG_Material1" JG_Material1))
		(if JG_Shape1 (set_tile "JG_Shape1" JG_Shape1))
	
		(if JS_Type1 (set_tile "JS_Type1" JS_Type1))
		(if JS_Size1 (set_tile "JS_Size1" JS_Size1))
	
		(if Off_Well1 (set_tile "Off_Well1" Off_Well1))
		(if Well_Memo1 (set_tile "Well_Memo1" Well_Memo1))
		;;p2
        (if Exp_No2 (set_tile "Exp_No2" Exp_No2))
        (if Map_No2 (set_tile "Map_No2" Map_No2))
        (if x2(set_tile "x2" (rtos x2 2 4)))
        (if y2 (set_tile "y2" (rtos y2 2 4)))
        (if Surf_h2 (set_tile "Surf_h2" (rtos Surf_h2 2 4)))
		(if Point_Size2 (set_tile "text-PointSize2" Point_Size2))
		
		;;2015-9-25
		(if Well_Deep2 (set_tile "Well_Deep2" Well_Deep2))

        (if feature2 (set_tile "feature2" feature2))
        (if subsid2 (set_tile "subsid2" subsid2))
		;;---------------湖州管线普查------------
		(if Neck_Deep2 (set_tile "Neck_Deep2" Neck_Deep2))
		(if JG_Material2 (set_tile "JG_Material2" JG_Material2))
		(if JG_Shape2 (set_tile "JG_Shape2" JG_Shape2))
	
		(if JS_Type2 (set_tile "JS_Type2" JS_Type2))
		(if JS_Size2 (set_tile "JS_Size2" JS_Size2))
	
		(if  Off_Well2 (set_tile "Off_Well2" Off_Well2))
		(if  Well_Memo2 (set_tile "Well_Memo2" Well_Memo2))
        ;;line
        (if D_S (set_tile "D_S" D_S))
        (if Flowdirect (set_tile "Flowdirect" (rtos Flowdirect 2 0)))
        (if Pressure (set_tile "Pressure" Pressure))
        (if Voltage (set_tile "Voltage" Voltage))
        (if Material (set_tile "Material" Material))
        (if Cab_Count (set_tile "Cab_Count" Cab_Count))
        (if Hole_Used (set_tile "Hole_Used" Hole_Used))
		(if Hole_Count (set_tile "Hole_Count" Hole_Count))
        (if Start_Deep (set_tile "Start_Deep" (rtos Start_Deep 2 4)))
        (if End_Deep (set_tile "End_Deep" (rtos End_Deep 2 4)))
		(if Road_Name (set_tile "Road_Name" Road_Name))
		(if PaiWu (set_tile "PaiWu" PaiWu))
		(if BuryWay (set_tile "BuryWay" BuryWay))
		;;---------------湖州管线普查------------
		(if Line_Style (set_tile "LineStyle" Line_Style))
		(if Line_Memo (set_tile "Line_Memo" Line_Memo))
		(if Build_Unit (set_tile "Build_Unit" Build_Unit))
		;;两端截面size
		(if Block_Size1 (set_tile "Block_Size1" Block_Size1))
		(if Block_Size2 (set_tile "Block_Size2" Block_Size2))
        ;;project
        (if unit (set_tile "unit" unit))
        (if date (set_tile "date" date))
        (if place (set_tile "place" place))
        (if subproject (set_tile "subproject" subproject))
        (if project (set_tile "project" project))
		(if gl_ErrorInfoString (set_tile "PromptInfo" gl_ErrorInfoString))
	;;设置按钮活动
        (action_tile "bt_getpoint1" "(getdata)(done_dialog 2)")
        ;(action_tile "bt-selectbt1" "(getdata)(done_dialog 3)")

        (action_tile "bt_getpoint2" "(getdata)(done_dialog 4)")
        ;(action_tile "bt-selectbt2" "(getdata)(done_dialog 5)")
	(action_tile "bt-Branch" "(getdata)(done_dialog 5)")
		
        (action_tile "bt-save" "(getdata)(done_dialog 6)")
        (action_tile "bt-nextpoint" "(getdata)(done_dialog 7)")
        (action_tile "bt-prepoint" "(getdata)(done_dialog 8)")

        (action_tile "accept" "(getdata)(done_dialog 1)")
        (action_tile "cancel" "(done_dialog 0)")

        (action_tile "point_type" "(OnSelectPointType)")
		(action_tile "Flowdirect" "(OnSelectFlowDirect entLine)")
		(action_tile "subsid" "(OnChangeSubsid $key $value)")
		(action_tile "subsid2" "(OnChangeSubsid $key $value)")
		
		(action_tile "Map_No" "(OnEditMapName $value $reason Map_No \"MAP\")")
		(action_tile "Exp_No" "(OnEditMapName $value $reason Exp_No \"EXP\")")
		(action_tile "Map_No2" "(OnEditMapName $value $reason Map_No2 \"MAP\")")
		(action_tile "Exp_No2" "(OnEditMapName $value $reason Exp_No2 \"EXP\")")
		
		(action_tile "bt_LoadProject" "(OnLoadProject)")
		(action_tile "bt_SaveProject" "(getdata)(OnSaveProject)")
		
		;;属性复制
		(action_tile "bt-copyP1-P2" "(OnCopyP1_P2)")
		(action_tile "bt-copyP2-P1" "(OnCopyP2_P1)")
		(action_tile "bt-copyL1-Lcur" "(OnCopyL1_Lcur)")
		
		;;复制属性按钮状态
		(if (and (> (length PreEdgeList) 1) (equal entP1 (cadr (cadr PreEdgeList))))
			(mode_tile "bt-copyL1-Lcur" 0)
			(mode_tile "bt-copyL1-Lcur" 1)
		)

	;;分支数>1，则设置按键有效
		(if (> (length BranchList) 1)
			(mode_tile "bt-Branch" 0)
			(mode_tile "bt-Branch" 1)
		)
      ;;设置按钮状态	
		(if (= mode 1) ;编辑模式
			(progn
				(mode_tile "bt-nextpoint" 0)
				(mode_tile "bt-prepoint" 0)
				(mode_tile "bt-save" 0)
				
				;;提示信息
				(set_tile "PromptInfo" (GetEditInfo entP1 entP2 entLine))
			)
		)
		(if (= mode 2) ;错误模式
			(progn
				(mode_tile "bt-nextpoint" 1)
				(mode_tile "bt-prepoint" 1)
				(mode_tile "bt-save" 1)
			)
		)
        (if (and (listp StackList) (= mode 1))
            (if (= 0 (length StackList))
                (mode_tile "bt-nextpoint" 1)
                ;;else
                (mode_tile "bt-nextpoint" 0)
            ) ;_ End_if
            (mode_tile "bt-nextpoint" 1) ;StackList = nil
        ) ;_ End_if

        (if (and (listp PreEdgeList)  (= mode 1))
            (if (<= (length PreEdgeList) 1 )
                (mode_tile "bt-prepoint" 1)
                ;;else
                (mode_tile "bt-prepoint" 0)
            ) ;_ End_if
            (mode_tile "bt-prepoint" 1) ;StackList = nil
        )
        (mode_tile "bt-selectbt2" 1)

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
		(if (or (= typename "G") (= typename "P") (= typename "Y") (= typename "W"))
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
            (progn
                (Dlg_SavetoDWG)
            ) ;progn
        ) ;end if

        (if (= std 2) ;输入点1坐标
            (progn
                (setq sp (getpoint (strcat "\n选择点" Map_No "的位置：")))
                (if (listp sp)
                    (progn
						(setq sp (SetPrecisionPoint sp 4))
                        (setq x (cadr sp)
                              y (car sp)
                        ) ;_ End_setq
                        (if (= 3 (length sp))
                            (setq Surf_h (last sp))
                        ) ;_ End_if
						(Dlg_SavetoDWG)
                    ) ;progn
                ) ;if
            ) ;progn
        ) ;end if

        (if (= std 4) ;输入点2坐标
            (progn
                (setq sp (getpoint (strcat "\n选择点" Map_No2 "的位置：")))
                (if (listp sp)
                    (progn
						(setq sp (SetPrecisionPoint sp 4))
                        (setq x2 (cadr sp)
                              y2 (car sp)
                        ) ;_ End_setq
                        (if (= 3 (length sp))
                            (setq Surf_h2 (last sp))
                        ) ;_ End_if
						(Dlg_SavetoDWG)
                    ) ;progn
                ) ;if
            ) ;progn
        ) ;end if

        ; (if (= std 3) ;选择点1对象
            ; (progn
                ; ;;(save current data)
                ; (if (setq ent (Choose_pointent))
                    ; (progn
                        ; (setq StackList (list)
                              ; CurEdge nil
                        ; ) ;_ End_setq
                        ; (InitP1 ent)
                        ; (InitP2 nil)
                        ; (InitLine entLine)
                    ; ) ;progn
                ; ) ;_ End_if
            ; ) ;progn
        ; ) ;end if

;;;        (if (= std 5) ;选择点2对象
;;;            (progn
;;;                (setq ent (Choose_pointent)
;;;                      StackList nil
;;;                      CurEdge nil
;;;                      )
;;;                (InitP2 ent StackList)
;;;                ) ;progn
;;;            ) ;end if

        (if (= std 6) ;save point
            (progn
                (Dlg_SavetoDWG)
            ) ;progn
        ) ;end if
		
		(if (and (> (length BranchList) 0)(= std 5)) ;branch
            (progn
                (Dlg_SavetoDWG)
				;;1.remove entP2 in StackList
				(while (and StackList (equal entP2 (car(car StackList))))
					(setq StackList (cdr StackList))
				)
				(if (listp PreEdgeList)
					(if (equal entP1 (car (car PreEdgeList)))
						(setq StackList (cons (car PreEdgeList) StackList)
							PreEdgeList (cdr PreEdgeList)
						)
					)
					(*error* "Error! editpointdlg.lsp Line 1561")
				)
				; ;;2.next Branch.Keep BranchList same with StackList
				(setq e (car BranchList)
					BranchList (cdr BranchList)
					BranchList (append BranchList (list e))
					ibranch (- (length BranchList) 1)
				)
				(while (and StackList (equal entP1 (car (car StackList))))
					(setq StackList (cdr StackList))
				)
				(repeat (length BranchList)
					(setq StackList (cons (nth ibranch BranchList) StackList)
						ibranch (- ibranch 1)
					)
				)
				
                (setq e     (car StackList)
                      entP1 (car e)
                      entP2 (cadr e)
                ) ;_ End_setq

                (InitP1 entP1)
                (InitP2 nil)
                (InitLine entLine)
            ) ;progn
        ) ;end if
		
        (if (= std 7) ;next point
            (progn
                (Dlg_SavetoDWG)
                (setq e     (car StackList)
                      entP1 (car e)
                      entP2 (cadr e)
                ) ;_ End_setq

                (InitP1 entP1)
                (InitP2 nil)
                (InitLine entLine)
            ) ;progn
        ) ;end if

        (if (= std 8) ;pre point
            (progn
                (Dlg_SavetoDWG)
                ;;PreEdgeList  1-cur edge 2-preedge
                (setq e     (car PreEdgeList)
                      StackList (cons e StackList)
                      PreEdgeList (cdr PreEdgeList)
                      e     (car PreEdgeList)
                      StackList (cons e StackList)
                      entP1 (car e)
                      PreEdgeList (cdr PreEdgeList)
                      entLine (nth 2 e)
                ) ;_ End_setq

                (InitP1 entP1)
                (InitP2 nil)
                (InitLine entLine)
            ) ;progn
        )
        
	;;对话框显示位置：右半侧
        (setq app       (vlax-get-acad-object)
              width     (vla-get-width app)
              height    (vla-get-height app)
              windpoint (list (- width (+ 10 DLG_WIDTH)) 30)
        ) ;_ End_setq

        (if (and (/= 0 x2) (/= 0 y2))
            (ZoomWndtoLeft (list y x) (list y2 x2))
            (ZoomWndtoLeft (list (+ 5 y) (+ 5 x))
                           (list (- y 5) (- x 5))
            ) ;_ End_ZoomWndtoLeft
        ) ;
    ) ;end if

    (unload_dialog id) ;卸载对话框文件
    (princ) ;静默退出
)   ;

;*********************************************************************************************
;函数定义:GetEditInfo()
;功能：编辑点线对话框时，显示提示信息
;参数：两点一线
;返回：
;创建时间：2015/10/27  23：00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun GetEditInfo(entP1 entP2 entLine / edge1 edge2 editinfo len len-slope slope)
	(setq EditInfo "")
	(if (and entP1 entP2 entLine)
		(progn
			;;连通数提示
			(if (and (setq edge1 (ldata-get entP1 "Edge"))  (setq edge2 (ldata-get entP2 "Edge")))
				(setq EditInfo (strcat "连通数:" (rtos (length edge1) 2 0) "-" (rtos (length edge2) 2 0) "  "))
			)
			;;长度与坡度
			(setq len-slope (GetLineProjectdLength_Slope entLine)
				len (car len-slope)
				slope (cadr len-slope)
			)
			(if (> len gl_MIN)
				(progn
					(setq EditInfo (strcat EditInfo " 长度:" (rtos len 2 2) ",坡度:" (rtos slope 2 5)))
					(if (> slope 0.1) (setq EditInfo (strcat EditInfo " 警告:坡度偏大！")))
					(if (< slope 0) (setq EditInfo (strcat EditInfo " 警告:流向与高程相反！")))
				)
				(setq EditInfo  (strcat EditInfo " 警告:两点重合！"))
			)
			
			;;类型名称不一致
			(if (/= (ldata-get entP1 "Main_Type") (ldata-get entP2 "Main_Type"))
				(setq EditInfo (strcat EditInfo " 警告:两点类型不同！"))
			)
			(if (or (= typename "P") (= typename "Y") (= typename "W"))
				(if (setq lx (ldata-get entLine "Flowdirect"))
					(if (and (/= lx 1) (/= lx 2)) (setq EditInfo (strcat EditInfo " 未定义流向。")))
				)
			)
			
			;;MapNo点名重复检查
			(if (not (isUniqueMapNo(ldata-get entP1 "Map_No")))
				(setq EditInfo (strcat EditInfo  " " (ldata-get entP1 "Map_No") "点号重复."))
			)
			(if (not (isUniqueMapNo(ldata-get entP2 "Map_No")))
				(setq EditInfo (strcat EditInfo  " " (ldata-get entP2 "Map_No") "点号重复."))
			)
		)
	)
	EditInfo
)

;;;编辑点线属性
(defun C:LineInfo_EditPoint  ()
;;;1选择P1初始点，可以是TEXT 或INSERT
    ;;全局变量 两个点实体和线实体
    (setq entP1 nil
          entP2 nil
          entLine nil
		  gl_ErrorInfoString "")
    (if (= nil (setq entP1 (Choose_pointent)))
        (exit))

    ;;保存各个边的stack
    (setq StackList nil
          PreEdgeList nil;编辑过的边，从StackList中取出的边
          CurEdge nil;当前边，StackList的第一个元素，DLG中P1 P2的连接线
          )
		  
    ;;增加对编辑直线段的支持entp1为直线段
    (setq entlist (entget entP1)
          enttype (cdr (assoc 0 entlist)))
    (if (= enttype "LINE")
        (progn
            (setq p0             (cdr (assoc 10 entlist))
                  p1             (cdr (assoc 11 entlist))
                  ConnectPoints1 (GetConnectEntity (car p0) (cadr p0) "INSERT")
                  ;ConnectPoints1 (append ConnectPoints1 (GetConnectEntity (car p0) (cadr p0) "INSERT")) ;_ end_append
                  ConnectPoints2 (GetConnectEntity (car p1) (cadr p1) "INSERT")
                  ;ConnectPoints2 (append ConnectPoints2 (GetConnectEntity (car p1) (cadr p1) "INSERT")) ;_ end_append
            ) ;_ end_setq
            (if (or (= nil ConnectPoints1) (= nil ConnectPoints2))
                (progn
                    (prompt "\n错误：选择的直线段一端不与任何点相连,退出!\n")
                    (exit)
                ) ;_ end_progn
            ) ;_ end_if
            (setq len1 (length ConnectPoints1)
                  len2 (length ConnectPoints2)
            ) ;_ end_setq
            (if (or (/= len1 1) (/= len2 1))
                (progn
                    (prompt "\n错误：选择的直线段与多于2个点相连,退出!\n")
                    (exit)
                ) ;_ end_progn
            ) ;_ end_if
            ;;只剩下直线与两个点相连的情况
            (setq entLine entP1
                  entP1 (car ConnectPoints1)
                  entP2 (car ConnectPoints2)
            ) ;_ end_setq

            ;;把边存入stacklist ((p1 p2 L1) (p1 p3 L2))
	        (if (= 0 (length StackList))
	            (progn
	                (setq ConnectPoints (GetConnectPoints (car p0) (cadr p0) "INSERT"))
		              ;ConnectPoints (append ConnectPoints (GetConnectPoints (car p0) (cadr p0) "INSERT")))
	                (if (listp ConnectPoints)
		            (foreach pent  ConnectPoints
                        (if (not(eq (car pent) entP2)) 
							(setq StackList (cons  (cons entP1 pent) StackList)))
		                )
		            ) ;if
	                );progn
	            );if
            (setq StackList (cons  (list entP1 entP2 entLine) StackList)
                  entLine nil);entLine在InitP2中初始化;
            (pointdlg entP1 nil 1)

        ) ;_ end_progn
    ) ;_ end_if

    ;;对于文字或INSERT图元
    (if (or (= enttype "TEXT") (= enttype "INSERT"))
    	(pointdlg entP1 nil 1))

    );end_defun

;*********************************************************************************************
;函数定义:C:EditErrorList()
;功能：处理gl_ErrorList
;参数：
;返回：
;创建时间：2014/12/22  17:52
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:EditErrorList( / edge enter enterr enttype error0 hl hp1 hp2 len pos x y next)
	(if (= 0 (setq len (length gl_ErrorList)))
		(if (setq enter (Fun_InPutString "Y" "USERS1" "\n错误列表为空,需要重新进行错误检查吗?(Y/N)"))
			(if (= "Y" (strcase enter)) (UpdateTopo))
		)
	)
	(setq next "Y")
	
	(while (and (< 0 len ) (= "Y" (strcase next)))
		(setq error0 (car gl_ErrorList)
			
			enterr (handent (car error0))
			edge (ldata-get enterr "Edge")
			StackList nil	;设置StackList
		)
		(if (= 'ENAME (type enterr))
			(progn
				(setq enttype (cdr (assoc 0 (entget enterr))))
				(if (= "INSERT" enttype) ;管线点
					(if (> (length edge) 0)
						(progn
							(setq edge (car edge)
								hp1 (car edge)
								hp2 (cadr edge)
								hl (last edge)
								entP1 (handent hp1)
								entP2 (handent hp2)
								entLine (handent hl)
								StackList (cons  (list entP1 entP2 entLine) StackList)
								entLine nil ;entLine在InitP2中初始化;
								entP2 nil
								gl_ErrorInfoString (GetEdgeErrors edge gl_ErrorList)
							)
							(pointdlg entP1 nil 2)
						)
						;;孤立点
						(progn
							(setq gl_ErrorInfoString (GetEdgeErrors edge gl_ErrorList))
							(prompt (strcat "\n" gl_ErrorInfoString))
							(prompt (strcat "\n还有" (rtos (length gl_ErrorList) 2 0) "条错误未处理." ))
							(setq pos (cdr (assoc 10 (entget enterr)))
								y (car pos)
								x (cadr pos))
							(ZoomWndtoLeft (list (- y 10) (- x 10)) (list (+ y 10) (+ x 10)))
							
							(setq len 0) ;终止循环
							(princ)
						)
					)
				)
				(if (= "LINE" enttype)
					(if (> (length edge) 0)
						(progn
							(setq
								hp1 (car edge)
								hp2 (cadr edge)
								hl (last edge)
								entP1 (handent hp1)
								entP2 (handent hp2)
								entLine (handent hl)
								StackList (cons  (list entP1 entP2 entLine) StackList)
								entLine nil ;entLine在InitP2中初始化;
								entP2 nil
								gl_ErrorInfoString (GetEdgeErrors edge gl_ErrorList)
							)
							(pointdlg entP1 nil 2)
						)
					)
				)
				(setq len (length gl_ErrorList))
				(if (> len 0)
                   (progn
						(prompt (strcat "\n还有" (rtos (length gl_ErrorList) 2 0) "条错误未处理." ))
						(setq next (Fun_InPutString "Y" "USERS1" "\n要处理下一项吗?(Y/N)"))
                     )
					 (prompt "\n错误处理完成.")
				)
			)
		)
	)
	(princ)
)

;;返回一条边所包含的全部错误,并从错误列表中删除这些错误
(defun GetEdgeErrors ( edge errorlist / errstr len hp1 hp2 hl e )
	;;(setq errorlist gl_ErrorList)
	(setq errstr "" 
		len (length errorlist))
	(if (< 0 len) 
		(progn
			;;单个句柄
			(if (= 'STR (type edge))
				(foreach e errorlist
					(if (= (car e) edge) 
						(setq errstr (strcat errstr (cdr e))
							errorlist (vl-remove e errorlist))
					)
				)
			)
			;;边,包含p1 p2 line
			(if (= 'LIST (type edge))
				(progn
					(setq hp1 (car edge)
						hp2 (cadr edge)
						hl (caddr edge)
						errstr ""
					)
					(foreach e errorlist
						(if (= (car e) hp1) 
							(setq errstr (strcat errstr (cdr e))
								errorlist (vl-remove e errorlist))
						)
						(if (= (car e) hp2) 
							(setq errstr (strcat "\n" errstr (cdr e))
								errorlist (vl-remove e errorlist))
						)
						(if (= (car e) hl) 
							(setq errstr (strcat "\n" errstr (cdr e))
								errorlist (vl-remove e errorlist))
						)
						
					)
				)
			)
			(setq errstr (strcat "("(rtos (length errorlist) 2 0) ")" errstr))
		)
	)
	(setq gl_ErrorList errorlist)
	errstr
)
		
;;修改实体后,需要更新边的句柄
(defun RebulidEdges (oldent newent / e hnew hold enttype edge edge1 edge2 el e2 ep1 ep2 )
	(if (and (/= nil newent) (/= nil oldent))
		(if (setq edge (ldata-get oldent "Edge"))
			(if (> (length edge) 0)
				(progn
					(setq hnew (cdr (assoc 5 (entget newent)))
						hold (cdr (assoc 5 (entget oldent)))
						enttype (cdr (assoc 0 (entget oldent)))
					)
					(if (= "INSERT" enttype)
						(progn
							(foreach e edge
								(setq el (handent (caddr e))
									ep1 (handent (car e))
									ep2 (handent (cadr e))
									) ;边
								;;ep1	
								(if (= hold (car e)) (setq edge (subst (cons hnew (cdr e)) e edge)))
								(if (= hold (cadr e))(setq edge (subst (list (car e) hnew (caddr e)) e edge)))
								
								;;ep2
								(if (setq edge2 (ldata-get ep2 "Edge"))
									(progn
										(foreach e2 edge2
											(if (= hold (car e2)) (setq edge2 (subst (cons hnew (cdr e2)) e2 edge2)))
											(if (= hold (cadr e2))(setq edge2 (subst (list (car e2) hnew (caddr e2)) e2 edge2)))
										)
										(ldata-put ep2 "Edge" edge2)
									)
								)
								;;el
								(if (setq edge2 (ldata-get el "Edge"))
									(if (> (length edge2 ) 0)
										(progn
											(if (= hold (car edge2)) (setq edge2 (cons hnew (cdr edge2))))
											(if (= hold (cadr edge2))(setq edge2 (list (car edge2) hnew (caddr edge2))))
											(ldata-put el "Edge" edge2)
										)
									)
								)
								(ldata-put newent "Edge" edge)
							)
						)
					)
					(if (= "LINE" enttype)
						(progn
							(setq ep1 (handent (car edge))
								ep2 (handent (cadr edge))
								edge1 (ldata-get ep1 "Edge")
								edge2 (ldata-get ep2 "Edge")
							)
							(if (= hold (caddr edge))
								(progn
									(setq edge (list (car edge) (cadr edge) hnew))
									(ldata-put newent "Edge" edge)
								)
							)
							;;更新两个端点
							(foreach e edge1
								(if (= hold (caddr e)) 
									(progn
										(setq edge1 (subst (list (car e) (cadr e) hold) e edge1))
										(ldata-put ep1 "Edge" edge1)
									)
								)
							)
							(foreach e edge2
								(if (= hold (caddr e)) 
									(progn
										(setq edge2 (subst (list (car e) (cadr e) hold) e edge2))
										(ldata-put ep2 "Edge" edge2)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

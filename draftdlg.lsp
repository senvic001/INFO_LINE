
;;;�Ի������ݺ���
;;;������ʵ�壬����Ϊtext��insert
;*********************************************************************************************
;��������:DraftDlg()
;���ܣ������Ա༭�Ի���
;������pent1 ���,pent2 ���ӵ� entl �߶�,mode ģʽ(1-�༭ 2-�����޸� )
;����ʱ�䣺2014/12/01   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun DraftDlg (pent1 pent2 entl mode / deep1 deep2 Main_Type typename Exp_No Depth1 Depth2 feature subsid t_QuickInsert
	PointSize1 Exp_No2 feature2 subsid2 Depth2 PointSize2 D_S Flowdirect Pressure Voltage Material
	 Cab_Count Hole_Count Hole_Used Road_Name PaiWu unit date place subproject project 
	 subsidlist subsidstr subsidstr2 featurelist featurestr featurestr2 x0 x1 x2 y0 y1 y2 p0 p1 p2 p10 p11)
	;; �ӿؼ��õ���ֵ  
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


	;;�༭���ʱ,��ѯ�Ƿ���Ψһ���
	(defun OnEditPointName (val reason pname / strname subtypename newname)
		(if (and (= 2 reason) (/= pname val))
			(progn
				;;is the uniqu No.?
				(if (HasSameExpName val)
					(progn
						(setq newname (CreateNewPointName val typename "EXP")) ;;typename global var,��������
						(alert (strcat "���ߵ�" val "�Ѿ�����!\n�����µĵ��Ϊ��" newname))
						(mode_tile "Exp_No" 2)
					)
				)
				;;subtype �Ƿ����
				(if	(null gl_SubList)
					(setq gl_SubList (ReadSubsymbolConfig nil))
				) ;if
				;;ʹ���������ж�����
				(setq strName  (vl-string-trim "0123456789-_+." val) ;ȥ���ұߵ��������
					subtypename (assoc strName gl_SubList)
				) ;_ end_setq
				(if (= nil subtypename)
					(progn
						(alert (strcat "����!���ߵ���ǰ׺" strname "���������ļ���,����ʹ��!\n��Ҫʹ��,�����޸������ļ�[Sub_Type]����,�����´򿪵�ǰ�ļ�."))
						(mode_tile "Exp_No" 2)
					)
				)
			)
			
		)
	)
	 

	
	;;ѡ�������ʱ���޸�subsid����
	(defun OnSelectPointType  ( / typenum)
		(setq typenum (atoi (get_tile "Main_Type")))
		(if (/= nil typelist)
			(setq typename   (substr (nth typenum typelist) 1 1) ;��һ���ַ�Ϊ����
				  subsidlist (getsubsidlist-fromtype typename)
				  )
			) ;if

		;;����subsid poplist����
		(start_list "subsid" 3)
		(mapcar 'add_list subsidlist)
		(end_list)
		(set_tile "subsid" "0")
	  
		(start_list "subsid2" 3)
		(mapcar 'add_list subsidlist)
		(end_list)
		(set_tile "subsid2" "0")
		
		;;���ð�ť״̬
		;;��ˮ
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
		;;����
		(if (or (= typename "G") (= typename "P") (= typename "Y") (= typename "W") (= typename "R"))
			(mode_tile "Flowdirect" 0)
			(mode_tile "Flowdirect" 1)
		)
		;;����
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
		;;��ѹ
		(if (or (= typename "D") (= typename "L") )
			(mode_tile "Voltage" 0)
			(mode_tile "Voltage" 1)
		)
		;;ѹ��
		(if (or (= typename "Q") (= typename "G") )
			(mode_tile "Pressure" 0)
			(mode_tile "Pressure" 1)
		)
	) ;

	;;;�����ڵ�����͵������Զ�����featurestr
	(defun GetFeaturestr  (count typename / str)
		(cond
			;((= count 1) (setq str "ֱ�ߵ�"))
			((= count 2) (setq str "ת�۵�"))
			((= count 3)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "����֧")
				 (setq str "��ͨ")))
			((= count 4)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "�ķ�֧")
				 (setq str "��ͨ")))
			((= count 5)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "���֧")
				 (setq str "��ͨ")))
			((= count 6)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "����֧")
				 (setq str "��ͨ")))
			((> count 6)
			 (if (or (= typename "D") (= typename "L") (= typename "X"))
				 (setq str "���֧")
				 (setq str "��ͨ")))
			(T (setq str nil))
			) ;cond
		str
		) ;defun

		;;����Ի����еĵ��߶���
		;;
		;;
		(defun SaveDraftDlg (projectinfo / sstr p10 p11 strf strs mn en ent bUpdate newinfo strname olddata newdata deep1 deep2)
			(if (= nil entP1)
				(progn
					(if (/= "0" subsid) ;;�޸�����
						(setq sstr subsidstr)
						(setq sstr featurestr)
					)
					(setq entP1 (AddNewPoint (list x1 y1 z1) Exp_No  typename featurestr subsidstr (GetPoint1AttribsList))
						prePointName Exp_No)
				)
				;else ���õ�����,�����Ÿı�,�򴴽��µ�
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
			;;�����߶�
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

                                    ;;������Ŀ��Ϣ
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
	;;����ʰȡ�������ʼ����ͼ�Ի���
	;;����:Main_Type typename ȫ�ֱ���
	;; pent1 pent2 entl �Ի����ϵ�����һ��
	(defun InitDraftDlg  (prePointName pent1 pent2 entl / newname e entlist enttype eobj ldata pent pos tmp)
		;;����P1����ص�
		;(setq gl_connectpoints nil	;��p1�����ĵ��((p1 Line1) (p2 Line2)...) or nil
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
				;;���ݣ���ȱ����ڵ��У���ȱ���������
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
							((= tmp "��")(setq PaiWu "0"))
							((= tmp "��") (setq PaiWu "1"))
							((= tmp "��") (setq PaiWu "2"))
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
		
		;;����ǰһ�����,�Զ�����µĵ��
		(if (= nil pent1)
			(if (= nil typename)
				(setq Exp_No (CreateNewPointName prePointName "J" "EXP"))
				(setq Exp_No (CreateNewPointName prePointName typename "EXP"))
			)
		)
		;;2��ʼ���б�ֵ,
		(if (= gl_TableColorList nil)
			(setq gl_TableColorList (ReadColorConfig nil))
			) ;if
		(foreach e  gl_TableColorList
			(setq typelist (append typelist (list (strcat (car e) "-" (caddr e)))))
			) ;foreach
			
		;;����Main_Type
		(if (and (= nil pent1) (= nil pent2))
			(if prePointName (setq typename (GetTypeFromPointName prePointName)))
		)
		(if (setq e (assoc typename gl_TableColorList))
			(if (setq pos (vl-position e gl_TableColorList))
				(setq Main_Type (rtos (vl-position e gl_TableColorList) 2 0))
				) ;if
			) ;if

		
		;;����feature1
		(setq featurelist (getfeaturelist-fromtype typename))
		(if (setq pos (vl-position featurestr featurelist))
			(setq feature (rtos pos 2 0))) ;if

		;;����subsid1
		(setq subsidlist (getsubsidlist-fromtype typename))
		(if (/= 0 (strlen subsidstr))
			(if (setq pos (vl-position subsidstr subsidlist))
				(setq subsid (rtos pos 2 0))
				) ;if
		) ;if
		;;���õ�2
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
	;;;1ѡ��P1��ʼ�㣬������TEXT ��INSERT
    ;;ȫ�ֱ��� ������ʵ�����ʵ��
    (setq entP1 pent1
          entP2 pent2
          entLine entl)
	;;2-2���ÿؼ�����Ĭ��ֵ
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
	  subsidlist (list "��")
	 )
    ;;������Ŀ��Ϣ��Ĭ��ֵ
	(if (setq ProjectInfoList (ReadProjectInfo nil))
		(setq unit (cdr (assoc "Unit" ProjectInfoList))
			date (cdr (assoc "Sur_Date" ProjectInfoList))
			place (cdr (assoc "Location" ProjectInfoList))
			subproject (cdr (assoc "SProject" ProjectInfoList))
			project	(cdr (assoc "Project" ProjectInfoList))
		)
	)
	; ;; x1 x2 y1 y2 p1 p2 ȫ�ֱ���
	(if (= nil entP1)
		(progn
			(if (= nil (setq p1 (getpoint "\nʰȡ��:")))
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
    ) ;װ��Ի����ļ�
    (if (< id 0)
        (exit)
    ) ;_ End_if
  
	;;�Ի�����ʾλ�ã��Ұ��
    (setq app       (vlax-get-acad-object)
          width     (vla-get-width app)
          height    (vla-get-height app)
    ) ;_ End_setq
    
    ;;522*620,�Լ���������
    (setq DLG_WIDTH 455
          DLG_HEIGHT 543
          windpoint (list (- width (+ 10 DLG_WIDTH)) 30)
        )
  
    (setq std 6) ;0 cancel 1 ok 2 di-biao 3 shejixian
    (while (> std 1)
        (if (not (new_dialog "draftpointdlg" id "" windpoint))
            (exit)
        ) ;��ʼ���Ի���

        ;;����typepoint poplist����:J-��ˮ
        (if (/= typelist nil)
            (progn
                (start_list "Main_Type" 3)
                (mapcar 'add_list typelist)
                (end_list)
            ) ;progn
        ) ;if


        ;;����feature poplist����    
        (start_list "feature" 3 )
        (mapcar 'add_list featurelist)
        (end_list)
        ;;����subsid poplist����
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
		
        ;;���ÿؼ���ֵ
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

        ;;���ð�ť�
		(action_tile "cancel" "(done_dialog 0)")
        (action_tile "accept" "(draft_getdata)(done_dialog 1)")
		
        (action_tile "bt-selectbt1" "(draft_getdata)(done_dialog 2)")		;ʰȡ��
		(action_tile "bt_toOldPoint" "(draft_getdata)(done_dialog 3)")	;���ӵ���֪��	
        (action_tile "bt_toNewPoint" "(draft_getdata)(done_dialog 4)")	;���ӵ��µ�

        (action_tile "Main_Type" "(OnSelectPointType)")			;�ı�������
		;(action_tile "LinkPointList" "(OnSelectLinkPoint)")			;ѡ��ڶ�����
		(action_tile "Exp_No" "(OnEditPointName $value $reason Exp_No)")					;�༭���
		(action_tile "LinkPoint" "(OnEditPointName $value $reason Exp_No2)")					;�༭���
        ;;���ð�ť״̬
		 ;;��ˮ
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
		;;����
		(if (or (= typename "G") (= typename "P") (= typename "Y") (= typename "W") (= typename "R"))
			(mode_tile "Flowdirect" 0)
			(mode_tile "Flowdirect" 1)
		)
		;;����
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
		;;��ѹ
		(if (or (= typename "D") (= typename "L") )
			(mode_tile "Voltage" 0)
			(mode_tile "Voltage" 1)
		)
		;;ѹ��
		(if (or (= typename "Q") (= typename "G") )
			(mode_tile "Pressure" 0)
			(mode_tile "Pressure" 1)
		)
		
        ;;��ʾ�Ի���
        (setq std (start_dialog))

        (if (= std 1) ;ok
           (SaveDraftDlg ProjectInfoList)
		)

        (if (= std 2) ;ѡ���1����
            (if (setq sp (getpoint (list x1 y1) (strcat "\nѡ���" Map_No "��λ�ã�")))
                (progn
                    (setq sp (SetPrecisionPoint sp 4)
							x1 (car sp)
                          y1 (cadr sp)
						  z1 (caddr sp)
                          p1 sp
                    ) ;_ End_setq

                    (SaveDraftDlg ProjectInfoList)
                    ;;���¶���
                    (if entP1
                        (progn ;���µ�����ӵ��߶�
                            (setq el   (entget entP1)
                                  pos1 (cdr (assoc 10 e1))
                                  el   (subst (cons 10 (list x1 y1 (caddr pos1))) (assoc 10 el) el)
                            ) ;_ end_setq
                            (entmod el)
                            ;;�ƶ��߶�
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

        (if (= std 3) ;���ӵ���֪��
			(progn
				(SaveDraftDlg ProjectInfoList)
				(if (setq ent (car (entsel "\ѡ��һ����֪�����:")))
					(progn
						
						
						(setq bsame nil)
						;;1.�ж���֪���entP1֮���Ƿ��Ѿ����߶�,����,�����entLine,����,�½�entLine
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
		
		(if (= std 4) ;���ӵ��µ�
			(progn
				;;save entP1 and entLine
				;; entP1 ������,���½�����
				(SaveDraftDlg ProjectInfoList)
               				
				(if (setq sp (getpoint (list x1 y1 )(strcat "\nѡ�������λ�ã�")))
					(progn ;;else ��������
						(setq sp (SetPrecisionPoint sp 4))
						(setq bsameAttrib "Y")
						(while (= "Y" (setq bsameAttrib (strcase (Fun_InPutString "Y" "USERS3" "\n����̽��ź�����,��������ͬǰһ������?(Y or N):" ))))
							(setq newExpNo (strcase (Fun_InPutString (CreateNewPointName prePointName typename "EXP") "USERS4" "\n������ߵ���:"))
								newdepth (Fun_InPutValue Depth1 "USERR4" "\n������ߵ����:" 4)
							)
							(while (HasSameExpName newExpNo)
								;(alert (strcat "����" newExpNo "�ظ�!\n�������µĵ��:"))
								(prompt (strcat "����" newExpNo "�ظ�!"))
								(setq newExpNo (strcase (Fun_InPutString (CreateNewPointName newExpNo typename "EXP") "USERS4" "\n������ߵ���:")))
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

                            ;;��һ��
                            (prompt "\n����������ߵ�.")
                            (setq sp (getpoint (list x1 y1 )(strcat "\nѡ�������λ�ã�"))
								sp (SetPrecisionPoint sp 4))
						)
						;;�ڶԻ����б༭����
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
        
		;;�Ի�����ʾλ�ã��Ұ��
        (setq app       (vlax-get-acad-object)
              width     (vla-get-width app)
              height    (vla-get-height app)
              windpoint (list (- width (+ 10 DLG_WIDTH)) 30)
        ) ;_ End_setq
    )

    (unload_dialog id) ;ж�ضԻ����ļ�
    (princ) ;��Ĭ�˳�
)   ;

;;;�õ��Ի������
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
;;;�õ����߶�����
(defun GetLineAttribsList ( / outlist PaiWustr)
	(cond 
			((= PaiWu "0") (setq PaiWustr "��"))
			((= PaiWu "1") (setq PaiWustr "��"))
			((= PaiWu "2") (setq PaiWustr "��"))
			(T (setq PaiWu "��"))
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
		;;outlist (cons (cons "Mdate" date) outlist)  ;;4.0�棬�Ի������޶�Ӧ����
		outlist (cons (cons "Unit" unit) outlist)
		outlist (cons (cons "Location" place) outlist)
		outlist (cons (cons "SProject" subproject) outlist)
		outlist (cons (cons "Project" project) outlist)
	)
	outlist
)
	;;;ѡ���ͼԪ
(defun Choose_pointent  (/ ent enttype entlist)
(setq ent     (entsel "\n�ɱ༭�ĵ�������ͼ�顢�������֡����߶Σ������ɱ�׼�Ե���Ϊ���Ե�ͼ�顣\nѡ��Ҫ�༭�ĵ����")
	  enttype nil
	  entlist nil) ;ʵ������
(if (= nil ent)
	(progn
		(prompt "\nû��ѡ��ͼԪ��")
		) ;progn
	(progn
		(setq ent     (car ent)
			  entlist (entget ent)
			  enttype (cdr (assoc 0 entlist)))
		(if (and (/= enttype "TEXT") (/= enttype "INSERT") (/= enttype "LINE")) ;(/= enttype "POINT")
			(progn
				(prompt "\nͼԪ���ǵ������ֺͿ����߶Σ��˳���")
				(setq ent nil)
				) ;progn
			) ;if
		) ;progn
	) ;if
ent
) ;defun

;;;�༭��������
;*********************************************************************************************
;��������:LineInfo_EditDraftPoint()
;���ܣ��༭���ߵ�,������ߵ�
;������
;����ʱ�䣺2014/12/01   12:40
;�޸�ʱ�䣺2015/12/24
;�����ˣ����۾�
;*********************************************************************************************
(defun C:LineInfo_EditDraftPoint  ( / attribslist  layername layerobj pname points pt tname)
    ;(DraftDlg nil nil nil 1)
	;;0 promt
	(if (setq pt (getpoint "\n������ߵ㡣\nѡ�������λ�ã�"))
		(progn
			(setq pt (SetPrecisionPoint pt 4))
			;;0��ǰλ���Ƿ������������
			(if (setq points (SearchPoint (GetInsertRoot nil) (car pt) (cadr pt)))
						;;������ͬλ�õ�
				(*error* "ѡ��λ�����й��ߵ㣬������ѡ��")
			)	
			;;1��ȡ��ǰͼ������,Ĭ��J
			(setq layerObj (vla-get-ActiveLayer (vla-get-Activedocument (vlax-get-acad-object)))
				layerName (vla-get-Name layerObj)
				tname (GetTypeFromPointName layerName)
			)
			;;2��ȡ���
			(setq pname (CreateNewPointName tname tname "MAP"))
			
			;;3��Ĭ�ϲ���������
			(setq attribslist (list (cons "Exp_No" pname) 
								  (cons "Map_No" pname) 
								  (cons "Main_Type" tname)
								  (cons "Feature" "������"))
			)
				;;�жϵ��Ƿ񳬳�������Χ
				;;���������������
			(if (IsOutRange gl_PointSpaceIndex (car pt) (cadr pt))
				(setq gl_PointSpaceIndex nil)	;;AddNewPoint ����������
			)
			(AddNewPoint pt pname tname "������" "" attribslist)
			
			;;4.����ռ������������б�
			(setq gl_MapNameList (cons pname gl_MapNameList))
			(if (not gl_PointSpaceIndex) (setq gl_PointSpaceIndex (GetInsertRoot T)))
			;;5 �ظ���ǰͼ��
			(vla-put-ActiveLayer (vla-get-Activedocument (vlax-get-acad-object)) layerObj)
		)
	)
	(princ)
 ) ;_ end_defun

 ;*********************************************************************************************
;��������:C:InsertPointInLine()
;���ܣ������й��߶��ϲ���һ�����ߵ㣬�Զ��������ƺ�������
;������
;����ʱ�䣺2014/12/19   13:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:InsertPointInLine( / ent entl entlist ep1 ep2 fstr  lattriblist line1 line2 
		mysnap newentp newname p0 p10 p11 pattriblist pname snap ss sstr typename)
	(prompt "\n�ڹ��߶��в���һ�����ߵ�.")
	(setq snap (getvar "OSMODE")
		mysnap 512 ;;����ĵ�
		entl nil ;;�߶�
	)
	(setvar "OSMODE" mysnap)
	
	(if (setq p0 (getpoint "\nʰȡ��:"))
		(if (setq ss (ssget p0))
			(if (= 1 (sslength ss))
				(progn
					(setq ent (ssname ss 0)
						entlist (entget ent)
					)
					(if (= "LINE" (cdr(assoc 0 entlist)))
						(if (vlax-ldata-get ent gl_AppName)
							(setq entl ent)
							(prompt "\nֱ�߶�δ�����������.")
						)
						(prompt "\nδʰȡ�����߶���.")
					)
				)
				(prompt "\����ѡ����������Ľ���λ��.")
			)
		)
		(prompt "\nδʰȡ����.")
	)
	;;�ҵ���һ��ֱ��
	(if entl
		(progn
			(setq p10 (cdr (assoc 10 entlist))
				p11 (cdr (assoc 11 entlist))
				typename (ldata-get entl "Main_Type")
				lattriblist (vlax-ldata-get entl gl_AppName)
			)
			
			;;��ȡ������
			(setq ep1 nil
				ep2 nil
				pattriblist nil)  ;;�������б�
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
			
			;;������ߵ�
			(if pattriblist
				(progn
					(setq newname (CreateNewPointName pname (ldata-get entl "Main_Type") typename)
						pattriblist (subst (cons "Exp_No" newname) (assoc "Exp_No" pattriblist) pattriblist)
						pattriblist (subst (cons "Map_No" newname) (assoc "Map_No" pattriblist) pattriblist)
						pattriblist (subst (cons "Feature" "ֱ�ߵ�") (assoc "Feature" pattriblist) pattriblist)
					)
					(setq p0 (SetPrecisionPoint p0 4))
					(if (setq newentp(AddNewPoint (list (car p0) (cadr p0) 0) newname typename "ֱ�ߵ�" sstr pattriblist))
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
;��������:AddDraftLine()
;���ܣ�ѡ���������߶�,Ȼ������
;������
;����ʱ�䣺2014/12/01   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:AddDraftLine ( / colorobj ent1 ent2 entl entlist ents len lineobj maintype1 maintype2 nextpt p10 p11 typename osmode)
	(prompt "\n���ƹ��߶�,�����������ߵ�.")
	;;osnap
	;;(osnap "_INS")
	(setq osmode (getvar "OSMODE"))
	(setvar "OSMODE" 64)	;;_INS
	(if (setq ent1 (car (entsel "\nѡ���һ�����ߵ�:")))
		(progn
			(setq entlist (entget ent1)
				typename (cdr (assoc 0 entlist))
				p10 (cdr (assoc 10 entlist)))
			(if (and (= "INSERT" typename) (vlax-ldata-get ent1 gl_AppName))
				(progn
					(setq maintype1 (ldata-get ent1 "Main_Type"))
					(while (setq nextpt (getpoint p10 "\nѡ����һ�����ߵ�:"));(setq ent2 (car (entsel "\nѡ����һ�����ߵ�:")))
						;;ͨ�������ҵ���
						(setq ents (SearchPoint (GetInsertRoot nil) (car nextpt) (cadr nextpt))
							ents (ListRemoveSameElement ents)
							len (length ents)
							ent2 nil
						)	
						(cond 
							((= 0 len) (prompt "\n��ǰλ������Ч���ߵ㣬������ѡ��"))
							((> len 1) (*error* "��ǰλ���ж�����ߵ㣬��ɾ���ظ����ߵ㡣"))
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
										(if (/= maintype1 maintype2) (prompt "\n���棡�������ߵ�����Ͳ�ͬ��"))
										;;warning
										(if (< (distance p10 p11) gl_MIN) (*error* "���������غϣ��˳���"))
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
							(prompt "\n��ǰ��������Ч���ߵ㣬������ѡ��")
						)
					)
					;(prompt "\nѡ��Ķ����ǹ��ߵ�,�˳�!")
				)
				;;else
				(prompt "\nѡ��Ķ����ǹ��ߵ�,�˳�!")
			)
		)
		;;else
		(prompt "\nѡ��Ķ����ǹ��ߵ�,�˳�!.")
	)
	(setvar "OSMODE" osmode)
	(princ)
)
;*********************************************************************************************
;��������:C:EditErrorList()
;���ܣ��Ӵ����б�gl_ErrorList�л�ȡ����,���öԻ���ʽ�༭
;;		ǰ������:����ʹ��CheckErrors�������˹�ϵ,�ҵ�����
;������
;����ʱ�䣺2014/12/19   13:00
;�޸�ʱ�䣺
;�����ˣ����۾�
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
	(setq unit (Fun_InPutString unit "USERS1" "\n�����뵥λ����:")
		ProjectInfoList (subst (cons "Unit" unit) (assoc "Unit" ProjectInfoList) ProjectInfoList))
	(setq project (Fun_InPutString project "USERS2" "\n��������Ŀ����:")
		ProjectInfoList (subst (cons "Project" project) (assoc "Project" ProjectInfoList) ProjectInfoList))
	(setq subproject (Fun_InPutString subproject "USERS3" "\n����������Ŀ����:")
		ProjectInfoList (subst (cons "SProject" subproject) (assoc "SProject" ProjectInfoList) ProjectInfoList))
	(setq date (Fun_InPutString date "USERS4" "\n�������ӵ���ʱ��:")
		ProjectInfoList (subst (cons "Sur_Date" date) (assoc "Sur_Date" ProjectInfoList) ProjectInfoList))
	(setq place (Fun_InPutString place "USERS5" "\n�������ӵ���ص�:")
		ProjectInfoList (subst (cons "Location" place) (assoc "Location" ProjectInfoList) ProjectInfoList))
	(if (setq bapply (Fun_InPutString "Y" "USERS1" "\n�滻���й��ߵ���Ŀ������?(Y/N)"))
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
						(prompt (strcat "\n�޸���" (rtos i 2 0) "�����߶��������."))
					)
				)
				(WriteProjectInfo ProjectInfoList nil)
			)
		)
	)
	(princ)
)

;*********************************************************************************************
;��������:C:MovePoint()
;���ܣ��ƶ���.���ƶ���������߶�,���±�ע.
;������
;����ʱ�䣺2015/1/23   10:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:MovePoint(  / bpoint ent1 entlist pos pxy sp typename x0 y0 allent ent   i newpos pos10 pos11 deeps)
	(prompt "\n�ƶ����ߵ�.")
	(setq ent1 nil 
		bpoint nil)
	(while (not bpoint)
		(if (setq ent1 (car (entsel "\nѡ����ߵ�:")))
			(progn
				(setq entlist (entget ent1)
					typename (cdr (assoc 0 entlist)))
				(if (and (= "INSERT" typename) (vlax-ldata-get ent1 gl_AppName))
					(setq bpoint T)
					(progn 
						(setq ent1 nil
							bpoint nil)
						(prompt "\nѡ��Ķ����ǹ��ߵ�,����°汾,����ѡ��.")
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
			(if (setq sp (getpoint pxy (strcat "\nѡ���ƶ����λ�ã�")))
				(progn 
					(setq sp (SetPrecisionPoint sp 4))
					;(MoveOnePoint2 pos sp ent1)
					
					(if (not (IsSamePoint pos sp))
						(progn
							(setq allent (GetConnectEntity (car pos) (cadr pos) "LINE"))
							(setq i 0)
							;;ƽ���ƶ�
							(vla-move (vlax-ename->vla-object ent1) (vlax-3D-point pos) (vlax-3D-point (car sp ) (cadr sp) (caddr pos)))
							;;update space index
							(setq gl_PointSpaceIndex (UpdatePointIndex (GetInsertRoot nil) ent1 x0 y0))
							;;�ƶ����ߣ��޸ĵ�����
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
							(princ "\n���ƶ����ӣ�")
							(princ pos)
							(princ "�ƶ�����")
							(princ (list (car sp ) (cadr sp) (caddr pos)))
						)
					)
				)
			)
		)
	)
	
	(princ)
)
;;�ƶ�һ����ptname from pos to sp
(defun MoveOnePoint(pos sp ptname / allent ent entlist enttype i newpos pos10 pos11)
	(setq i 0
		sp (SetPrecisionPoint sp 4))
	(if (setq allent (ssget "X" (list  (cons -4 "<AND") (list -3 (list gl_AppName))
							(cons -4 "<OR")
							(cons -4 "=,=,*") (cons 10 pos) (cons -4 "=,=,*") (cons 11 pos)
							(cons -4 "OR>")
							(cons -4 "AND>"))))
		(progn
			;;ɾ����Ų�һ�µĵ�
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
;;�ƶ�һ���㣬�Լ���������߶�
;;������pos ��㣬sp Ŀ��㣬entp ��ʵ��
;;���أ�
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
;��������:SetPrecisionPoint()
;���ܣ��ѵ�ľ��ȱ���Ϊ4ΪС��,ʹ֮��ͼ���к�����ļ���һ�¡�
;������
;����ʱ�䣺2015/3/22   10:40
;�޸�ʱ�䣺
;�����ˣ����۾�
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
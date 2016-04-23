;;;����̽��CAD-���ݿ�ϵͳ2014-07-08
;;;

 ;(LineInfo_GetSupportPath)

(vl-load-com)

;;; ���Ƶ����
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
									  "\n����Ҫ���ƵĹ������ͣ���������ö���','�ָ���ȫ��ѡ������'All'����")
		  ;scale		 (Fun_InPutValue 1 "USERR2" "\n����ͼ�α�����(1:1000����1��1:500����2��1:2000����0.5)��" 2)
		  )
		(progn

			;;�Ȼ����ߣ��ٻ��Ƶ㣬�����߱�ͼ��ѹס
			;;����������������
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

 
;;;������Ԫ���ݿ⣬���������ߣ���������	
(defun C:ImportZYDB ( / result strpath path0 )
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "\\database.mdb")
	)
    (if (setq strPath (getfiled "ѡ������������ݵ�Access���ݿ��ļ�" path0 "MDB" 8))
        (progn
            (prompt (strcat "\n\n���ݿ�·����" strPath "\n"))
			(if (setq result (getstring "\n�����ݿ�����Ԫ�������ݿ���?(Y/N)"))
				(if (= "Y" (strcase result))
					(cond ((vl-catch-all-error-p
								(setq XL (vl-catch-all-apply
									'DrawTablesZY
									(list strPath))
								)
							)
							(vl-exit-with-error
								(strcat "\n����: " (vl-catch-all-error-message XL))
							)
						)
						(T (princ "\n�ɹ�������Ԫ���ݿ⡣"))
					)
					;;(DrawTablesZY strPath)
				)
			)
        );progn
        (prompt (strcat "\n\nδ���κ����ݿ⡣" ))
    );if
    
    (princ)
)
 ;*********************************************************************************************
 ;��������:DrawTablesZY(mdbpath)
 ;���ܣ������߱�͵���߱��ʽ�̶���Ϊ���ݹ������ݿ��ʽ
 ;������mdbpath MDB�ļ�·��
 ;���أ���
 ;����ʱ�䣺2015/4/24 14:34
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun DrawTablesZY  (mdbpath /      attrib_objs cols connectionobject connstr deep1 deep2 e Exp_No 
		featureslist featurestr  index linelist lines linetablename maintype Map_No newblock_ref
		newl newp numl nump onerecord outlist p10 p11 pend pendname pointlist points pointtablename 
		pos pstart pstart_h pstartname pstartx pstarty rotation subsidslist subsidstr table tblist typelist)

	(LineInfo_GetSupportPath)
	;;���ӵ����ݿ⣬�������Ӷ���
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
	
	;;��ȡ�������ݱ�ǰ׺�����������һ�£����ߺ��Ӹñ�
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
				;;��ȡ����
				(setq points (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " pointtablename 
								" WHERE ɾ����� is null AND X���� is not NULL AND Y���� is not NULL AND ����߳� is not NULL"))) ;������
				(setq nump (length points)
					index	1
					pointlist nil
				)
				;;���
				(setq cols (car points))	;;fist row has all coloumns
				(if (> nump 1)
					(repeat	(- nump 1)
						(setq oneRecord	 (nth index points)
							index (1+ index)
							pstartx  (nth (vl-position "Y����" cols) oneRecord)
							pstarty  (nth (vl-position "X����" cols) oneRecord)
							pstart_h (nth (vl-position "����߳�" cols) oneRecord)
							outlist nil
							featurestr (nth (vl-position "����" cols) oneRecord)
							subsidstr (nth (vl-position "������" cols) oneRecord)
							Map_No (nth (vl-position "ͼ�ϵ��" cols) oneRecord)
							Exp_No (nth (vl-position "��̽���" cols) oneRecord)
							  
						)
						;;�����͸�����
						(if (not (vl-position featurestr featureslist)) (setq featurestr "ֱ�ߵ�"))
						(if (not (vl-position subsidstr subsidslist)) (setq subsidstr ""))
						;;���Ϊ�յ�����
						(if (not Map_No) (setq Map_No Exp_No))								
						(setq outlist (cons (cons "Exp_No" Exp_No) outlist)
							  outlist (cons (cons "Map_No"  Map_No) outlist)
							  outlist (cons (cons "Feature"  featurestr) outlist)
							  outlist (cons (cons "Subsid"  subsidstr) outlist)
							  outlist (cons (cons "Main_Type" maintype) outlist)
							  
							  outlist (cons (cons "Unit" (nth (vl-position "̽�ⵥλ" cols) oneRecord)) outlist)
							  outlist (cons (cons "SProject" (nth (vl-position "��λ��������" cols) oneRecord)) outlist)
							  outlist (cons (cons "Project" (nth (vl-position "������Ŀ����" cols) oneRecord)) outlist)
							  
						)
						
						(if (setq newp (AddNewPoint (list pstartx pstarty pstart_h) Exp_No maintype featurestr subsidstr outlist))
							(progn
								(setq pointlist (cons (list Exp_No newp) pointlist))
								(ldata-put newp "Status_DB" 0)
								;;�����������ֵ�λ��,���ݿ��Զ����ֶΣ�
								(setq newblock_ref (vlax-ename->vla-object newp))
								(if (setq pos (vl-position "�Ƕ�" cols))
									(if (setq rotation (nth pos oneRecord))
										(progn
											(vla-put-rotation newblock_ref rotation)
											(if  (= :vlax-true (vla-get-HasAttributes newblock_ref))
												(progn
													;;text insertpoint
													(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
													(foreach e attrib_objs
														(if (= "���" (vla-get-tagstring e))
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
					(prompt (strcat "\n�����ݱ�"pointtablename "�ж�ȡ��" (rtos (length pointlist) 2 0) "�����ߵ�."
									"���ݱ��й���" (rtos (- (length points) 1) 2 0) "�����߶�."))
				)
				;;��ȡ�߱�
				(setq lines	 (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " linetablename 
									" WHERE ɾ����� is null AND �����̽��� is not NULL AND ���ӷ��� is not NULL "))) ;������
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
							pstartname (nth (vl-position "�����̽���" cols) oneRecord)
							pendname (nth (vl-position "���ӷ���" cols) oneRecord)
							outlist (cons (cons "Start_Point"  pstartname) outlist)
							outlist (cons (cons "End_Point"  pendname) outlist)
							deep1 (nth (vl-position "�������" cols) oneRecord)
							deep2 (nth (vl-position "�յ�����" cols) oneRecord)
						)
						(if (setq pstart (assoc pstartname pointlist)) 
							(if (setq pend (assoc pendname pointlist))
								(progn
									(if (not deep1) 
										(progn 
											(princ (strcat "\n�������ݴ��󣺹��߶�" pstartname "-" pendname "������Ϊ�գ����Զ���Ϊ-1."))
											(setq deep1 -1)
										)
									)
									(if (not deep2) 
										(progn 
											(princ (strcat "\n�������ݴ��󣺹��߶�" pstartname "-" pendname "�յ����Ϊ�գ����Զ���Ϊ-1."))
											(setq deep2 -1)
										)
									)
									(setq  outlist (cons (cons "Material" (nth (vl-position "����" cols) oneRecord)) outlist)
										  outlist (cons (cons "D_S" (nth (vl-position "�ܾ�" cols) oneRecord)) outlist)
										  outlist (cons (cons "Main_Type" maintype) outlist)
										  outlist (cons (cons "Start_Deep" deep1) outlist)
										  outlist (cons (cons "End_Deep" deep2) outlist)
										  
										  outlist (cons (cons "Voltage" (nth (vl-position "��ѹֵ" cols) oneRecord)) outlist)
										  outlist (cons (cons "Pressure" (nth (vl-position "ѹ��ֵ" cols) oneRecord)) outlist)
										  outlist (cons (cons "Cab_Count" (nth (vl-position "����" cols) oneRecord)) outlist)
										  outlist (cons (cons "Hole_Count" (nth (vl-position "����" cols) oneRecord)) outlist)
										  outlist (cons (cons "Flowdirect" (nth (vl-position "����" cols) oneRecord)) outlist)
										  
										  outlist (cons (cons "Sur_Date" (nth (vl-position "̽��ʱ��" cols) oneRecord)) outlist)
										  outlist (cons (cons "Unit" (nth (vl-position "̽�ⵥλ" cols) oneRecord)) outlist)
										  outlist (cons (cons "SProject" (nth (vl-position "��λ��������" cols) oneRecord)) outlist)
										  outlist (cons (cons "Project" (nth (vl-position "������Ŀ����" cols) oneRecord)) outlist)
										  outlist (cons (cons "Mdate" (nth (vl-position "�������" cols) oneRecord)) outlist)
									)
									
									;;��Щ�ֶηǱ�ע�������ݿ⣬�Զ����ֶ�
									
									(if (setq pos  (vl-position "Ȩ����λ����" cols))
										(setq outlist (cons (cons "Property" (nth pos oneRecord)) outlist))
									)
									(if (setq pos  (vl-position "Ȩ����λ����" cols))
										(setq outlist (cons (cons "Road_Name" (nth pos oneRecord)) outlist))
									)
									(if (setq pos  (vl-position "BuryWay" cols))
										(setq outlist (cons (cons "��������" (nth pos oneRecord)) outlist))
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
						;; ���ֻ���߱���
						(if (or (= nil pstart ) (= nil pend))
							(prompt (strcat "\n�����߱��еĹ��߶�" pstartname "-" pendname ",�����յ㲻�ڵ����."))
						)

					)
				)

				(if linelist
					(prompt (strcat "\n�����ݱ�" linetablename "�ж�ȡ��" (rtos (length linelist) 2 0) "�����߶�."
									"���ݱ��й���" (rtos (- (length lines) 1) 2 0) "�����߶�."))
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
 ;��������:GetSelectLines(linetable,pointtable)
 ;���ܣ��ӵ����߱��л�ȡ�ߵ�����,������������,����������;Ŀ����Ϊ���SELECT* ���µ� Ч������
 ;�������߱�͵�������
 ;���أ�SELECTText SQL���
 ;����ʱ�䣺2015/6/9   13:34
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun GetSelectLines(linetable pointtable)
	(setq sqlstr (strcat "SELECT " linetable ".*,p1.x,p1.y,p1.surf_h,p2.x,p2.y,p2.surf_h FROM " 
			linetable "," pointtable " As p1," pointtable " As p2 WHERE " 
			"p1.Map_No=" linetable ".Start_Point AND p2.Map_No=" linetable ".End_Point"
		)
	)
)
 
;*********************************************************************************************
 ;��������:DrawTables(maintypes)
 ;���ܣ������߱�͵���߱��ʽ�̶���Ϊ��Ԫ�������ݿ��ʽ�����������еĵ����߱�.
 ;������("J" "P"...)-�������б�
 ;���أ�T or False
 ;����ʱ�䣺2014/12/31   19:34
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun DrawTables (maintypes / attrib_objs cols connectionobject deep1 deep2 e featurestr index 
	linelist lines linetablename location maintype Map_No newblock_ref newl newp numl nump onerecord 
	outlist p10 p11 pend pendname pointlist points pointtablename pstart pstart_h pstartname pstartx 
	pstarty ptextx ptexty ptextz rotation subsidstr pos)
	(LineInfo_GetSupportPath)
	(foreach maintype maintypes
		(setq linetablename	 (strcat maintype "L")
			  pointtablename (strcat maintype "P"))
		;; ��ȡ����
		(if	(setq ConnectionObject (GetConnectionObject))
			(progn
				(setq
					lines	 (ADOLISP_DoSQL ConnectionObject (GetSelectLines linetablename pointtablename)) ;������
					;;lines	 (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " linetablename)) ;������
					  points (ADOLISP_DoSQL ConnectionObject (strcat "SELECT * FROM " pointtablename)) ;������
					  )
				(if	(and lines points)
					(progn
						(setq numl	(length lines)
							nump (length points)
							index	1
							pointlist nil
							cols (car points)
						)
						;;���
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
								;;���Ϊ�յ�����
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
								;;�Զ����ֶ�
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
										;;�����������ֵ�λ��,���ݿ��Զ����ֶΣ�
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
																(if (= "���" (vla-get-tagstring e))
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
							(prompt (strcat "\n�����ݱ�" pointtablename "�ж�ȡ��" (rtos (length pointlist) 2 0) "�����ߵ�."
											"���ݱ��й���" (rtos (- (length points) 1) 2 0) "�����ߵ�."))
						)
						;;�߱�
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
								
								
								;(if (setq pstart (assoc pstartname pointlist)) ;;Ч����͵�
									;(if (setq pend (assoc pendname pointlist))	;;Ч����͵�
										;(progn
											(if (not deep1) 
												(progn 
													(princ (strcat "\n�������ݴ��󣺹��߶�" pstartname "-" pendname "������Ϊ�գ����Զ���Ϊ-1."))
													(setq deep1 -1)
												)
											)
											(if (not deep2) 
												(progn 
													(princ (strcat "\n�������ݴ��󣺹��߶�" pstartname "-" pendname "�յ����Ϊ�գ����Զ���Ϊ-1."))
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
											
											;;��Щ�ֶηǱ�ע�������ݿ⣬�Զ����ֶ�
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
													;;3.4.0��Mdate�����˵���ʱ�䣬���ݿ�ʱҲ�����,ʹ��Mdata����Sur_Date
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
								; ;; ���ֻ���߱���
								; (if (or (= nil pstart ) (= nil pend))
									; (prompt (strcat "\n�����߱��еĹ��߶�" pstartname "-" pendname ",�����յ㲻�ڵ����."))
								; )
								(setq index (1+ index))
							)
						)
						(if linelist
							(prompt (strcat "\n�����ݱ�" linetablename "�ж�ȡ��" (rtos (length linelist) 2 0) "�����߶�."
											"���ݱ��й���" (rtos (- (length lines) 1) 2 0) "�����߶�."))
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
;;���������ڵı���
; (defun DrawDBLine  (p1 p2 tableline	tablep ConnectionObject	/ dend dstart lineobj lines	onerecord pend pend_h pendname pendx pendy points pstart pstart_h pstartname pstartx pstarty sqlstatement)
	; (AddLayer tableline (strcat tableline "_Line"))
	; (if	ConnectionObject
		; (progn
			; (setq SQLStatement (strcat "SELECT * FROM "	tableline " WHERE (Start_Point=" "'" p1	"'"	" AND End_Point=" "'" p2 "') OR (Start_Point=" "'" p2 "'" " AND End_Point="	"'"	p1 ")")
				  ; lines		   (ADOLISP_DoSQL ConnectionObject SQLStatement) ;������
				  ; points	   (ADOLISP_DoSQL ConnectionObject
											  ; (strcat "SELECT * FROM " tablep " WHERE Map_No=" p1 " OR Map_No=" p2)) ;������
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
							; (vlax-ldata-put lineobj gl_AppName (cons tableline oneRecord)) ;�������ݿ��¼
							; (vlax-ldata-put lineobj "STATUS" (list "STATUS" 1 0)) ;״̬���������ݿ��У�δ�޸�
							; ) ;progn
						; )
					; ) ;progn
				; (progn
					; (prompt (strcat "\nδ�ҵ�" p1 "," p2 "��¼��"))
					; )
				; ) ;if
			; ) ;progn
		; ) ;if
	; ) ;

 ;*********************************************************************************************
 ;��������:DrawPoint (strP,x,y,z,scode,scale pointtype)
 ;���ܣ�����һ���㣬����ʾ��źͷ���,
 ;��������ţ�x���꣬y���꣬z���꣬���ű��룬�����ߣ�1:1000,Ϊ1��1:500Ϊ2;�Դ����ƣ�
 ;���أ����
 ;����ʱ�䣺2014/07/10   08:30
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
; (defun DrawPoint  (strP	x y	z scode	scale pointtype	/ ts tablename app attrib_obj attrib_objs doc mspace newblock_ref symbol symbolpath	tmpresult myscale)
	; ;;���ط��ű�

	; (setq tablename (strcat pointtype "P"))
	; (AddLayer tablename (strcat tablename "_Point"))

	; (setq ts (Addfont 'acstyle))
	; (if	(= nil gl_SymbolBlockList)
		; (setq gl_SymbolBlockList (ReadSymbolConfig nil))
		; )
	; (setq scode	 (strcat pointtype (rtos scode 2 0))
		  ; symbol (assoc scode gl_SymbolBlockList))
	; (if	(= nil symbol) ;ͼ�����û�иô��룬��0��ȡ��
		; (setq symbol (assoc "ALL1" gl_SymbolBlockList))
		; )
	; (setq symbolpath (strcat gl_BLOCKLIB_PATH (cadr symbol)))

	; ;;
	; ;;��ͼ�ⴴ��ͼ��
	; (setq app		   (vlax-get-acad-object)
		  ; Doc		   (vla-get-Activedocument app)
		  ; Mspace	   (vla-get-modelspace Doc)
		  ; myscale	   (/ 1.0 scale) ;��ע�⣺��1.0����1������ֻ������������
		  ; newblock_ref (vla-InsertBlock Mspace (vlax-3D-point x y z) symbolpath 1 1 1 0)
		  ; )
	; ;;�޸ĵ������
	; (if	(= :vlax-true (vla-get-HasAttributes newblock_ref))
		; (progn
			; (setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
			; (if	(> (length attrib_objs) 0)
				; (progn
					; (setq attrib_obj (car attrib_objs))
					; (vla-put-TextString attrib_obj strP) ;����
					; (vla-put-Height attrib_obj 2) ;�ָ�
					; (vla-put-scalefactor attrib_obj 0.8) ;�������
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
 ;��������:AddLayer(layername)
 ;���ܣ����ͼ�㣬������Ϊ��ǰͼ��,ͼ�����Ƶĵ�һ����ĸ��������
 ;������tablename-�������� layername-ͼ������
 ;���أ�nil or ��ǰͼ��layerobj
 ;����ʱ�䣺2014/07/09   22:10
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
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
		(setq colorindex 7) ;û�����Ķ���ʹ�ð�ɫ
		)

	(vla-put-color pointlayer colorindex)
	(vla-put-activelayer Doc (vla-item Layers layername))
	(vla-get-activelayer Doc)
		
)
 ;*********************************************************************************************
 ;��������:AddFont()
 ;���ܣ�ʹ������simtxt.shx,hztxt.shx
 ;������
 ;���أ��������,nil
 ;����ʱ�䣺2014/08/05  10��36
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
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
			(setq ts (vla-add txtstyles "DB_DXT") ;�������������
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
			;;���Ƶ�ϵͳfontsĿ¼
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
 ;��������:GetTableFromLine()
 ;���ܣ������д������ӹ�ϵ����������ļ�
 ;������layername
 ;���أ�nil or ��ǰͼ��layerobj
 ;����ʱ�䣺2014/07/09   22:10
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
; (defun GetTableFromLine	 (/ e file i lineset outpath p1 p2 point1 point2 pointlist strout tmp)
	; (setq lineset (ssget "X" (list (cons 0 "LINE")))) ;ѡ������LINE����
	; (setq i		  0
		  ; outpath "C:\\zhd-lines.csv")
	; (if	(setq pointlist (GetPointsFromBlock))
		; (progn
; ;;; ������ļ�
			; (vl-file-delete outpath)
			; (setq file (open outpath "w"))
			; (write-line "���ţ��յ�ţ������ȣ��յ����" file)
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
 ;��������:GetPointFromXY(pointlist x y)
 ;���ܣ�����x y���꣬��������ĵ�
 ;������pointlist (("p1" x1 y1 z1)("p2" x2 y2 z2)...)
 ;���أ�nil or ��("Pi" xi yi zi)
 ;����ʱ�䣺2014/07/09   23:00
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
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

;;�޸�������ʽ
;;ʹ�ñ�׼���壬1:1000ʱ���ָ�2��
;;���ݱ����ߵ���;
(defun C:ModifyTextStyle  (/ entset i ts acts ename newblock_ref attrib_objs num scale)
	(setq entset (ssget "X" (list (cons 0 "INSERT")))
		  i		 0
		  num	 0
		  ) ;ѡ������enttype����
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
					;;�޸ĵ������
					(if	(= :vlax-true (vla-get-HasAttributes newblock_ref))
						(progn
							(setq attrib_objs (vlax-safearray->list
												  (vlax-variant-value (vla-GetAttributes newblock_ref))
												  ) ;_ end_vlax-safearray->list
								  ) ;_ end_setq
							(if	(> (length attrib_objs) 0)
								(progn
									(setq attrib_obj (car attrib_objs))
									(vla-put-Height attrib_obj (/ 2.0 gl_MAP_SCALE)) ;�ָ�
									(vla-put-ScaleFactor attrib_obj 0.8)
									(if	ts
										(vla-put-stylename attrib_obj (vla-get-name ts))
										) ;������ʽ
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

	(prompt (strcat "\n���޸�" (rtos num 2 0) "�����ߵ��������ʽ��\n"))
	) ;_ end_defun
;*********************************************************************************************
 ;��������:Drawblock (pos direction blockname)
 ;���ܣ�����ͼ�飬������ת�Ƕ�
 ;�����������
 ;���أ�
 ;����ʱ�䣺2014/09/04   21:00
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun Drawblock (pos rotation blockname / myscale newblock_ref symbolpath basename)
    (setq symbolpath (strcat gl_BLOCKLIB_PATH blockname)
		basename (vl-filename-base blockname)
		newblock_ref nil
		myscale      (/ 1.0 gl_MAP_SCALE) );��ע�⣺��1.0����1������ֻ������������
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

;;;�޸Ĺܵ��ķ���
(defun C:YBDirection( / allp ent i np subsidstr featurestr)
	(print "�Զ��������ߵ��ķ�����ˮ������ˮ���������ߴ�ֱ��")
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0 )
			(repeat np
				(setq ent (ssname allp i)
					i (1+ i))
				(if (setq subsidstr (ldata-get ent "Subsid"))
					(cond
						((= subsidstr "��ˮ��") (ModifyblockAngle ent pi))
						((= subsidstr "��ˮ��") (ModifyblockAngle ent pi))
						((= subsidstr "����") (ModifyblockAngle ent pi))
						((= subsidstr "Ԥ����") (ModifyblockAngle ent pi))
					)
				)
				(if (setq featurestr (ldata-get ent "Feature"))
					(cond
						((= featurestr "��ˮ��") (ModifyblockAngle ent pi))
						((= featurestr "�ǲ���ȥ���") (ModifyblockAngle ent pi))
					)
				)	
			)
		)		
	)
	(print "�Զ�����������ɡ�")
)

;*********************************************************************************************
 ;��������:ModifyblockAngle (entblock direction)
 ;���ܣ��޸Ĺ��ߵ�ķ���
 ;�����������ԵĹ��ߵ㣬������ֱ�ӵķ���нǣ����ȣ�
 ;���أ�
 ;����ʱ�䣺2015/03/24  12:00
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
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
					;;�б�
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
								
								(if (< dist gl_MIN);;ֱ�߷�λ��
									(setq langle (angle pos0 p11))
									(setq langle (angle pos0 p10))
								)
								(setq a0 (+ direction langle))

								;;��ȡ�������ֵ�λ����Ϣ
								;;ֻ��תͼ�飬���ƶ����֣�
								(setq block_ref (vlax-ename->vla-object entblock)
									rotation (vla-get-rotation block_ref)
									attrib_pt nil)
								(if (or gl_DEBUG (= 0 rotation));;ֻ����δ��ת�Ķ���,����ʱ������������ת����
									(if (= :vlax-true (vla-get-HasAttributes block_ref))
										(progn
											;;text insertpoint
											(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes block_ref))))
											(foreach e attrib_objs
												(if (= "���" (vla-get-tagstring e))
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
							(prompt (strcat "\n����" (ldata-get entblock "Map_No") "������߶�������"))
						)		
					)
					(progn
						(prompt (strcat "\n����" (ldata-get entblock "Map_No") "������߶�������"))
					)
				)
			)
		)
	)	
)
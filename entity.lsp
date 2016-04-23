;;;���ߵ��ߣ�ʵ������ļ�
;*********************************************************************************************
;��������:InitPointAttribs()
;���ܣ���ʼ�����Բ���д�뵽�ֶΣ������ؼ���
;������pename����ʵ�����ƣ��޶�Ϊ��飩
;���أ�ename
;����ʱ�䣺2014/12/01   12:40
;�޸�ʱ�䣺2015/3/9	114:40
;�޸�����Ϊһ��ldata��(Info_Line (("Map_No"."JS001")  ("Depth".0)......))
;�����ˣ����۾�
;*********************************************************************************************
(defun InitPointAttribs(pename  / mapno datalist)
	;;gl_AppName
	; (if (not (vlax-ldata-get pename gl_AppName))
		; (vlax-ldata-put pename gl_AppName gl_Version)	;?!��Ҫ�޸�
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
	; (vlax-ldata-put pename "Exp_No" "")			;��̽���
	; ;;Depth
	; (vlax-ldata-put pename "Depth" 0)			;���
	; (vlax-ldata-put pename "Feature" "")			;������
	; (vlax-ldata-put pename "Subsid" "")			;������
	; (vlax-ldata-put pename "Main_Type" "")		;������,13��,�����ַ�
	; ;(vlax-ldata-put pename "Sub_Type" "")		;������,1-3���ַ�
	; (vlax-ldata-put pename "Use_Status" 0)		;0-����;1-ʹ��;2-ʩ����
	; (vlax-ldata-put pename "Point_Size" "")		;���ǹ��,Բ��Ϊֱ��,����ΪX*Y
	
	; (vlax-ldata-put pename "Text_Pos" (list 0 0 0 0))	;��ŵı��λ��,4������(x y z angle)
	; ;(vlax-ldata-put pename "Mark_Angle" 0)		;ͼ��ı�׼�Ƕ�
	
	; ;;project info
	; (vlax-ldata-put pename "Sur_Date" "")			;����ʱ��
	; (vlax-ldata-put pename "SProject" "")		;����Ŀ����
	; (vlax-ldata-put pename "Project" "")			;����Ŀ����
	; (vlax-ldata-put pename "Location" "")		;����λ��
	; (vlax-ldata-put pename "Unit" "")			;���鵥λ
	
	; ;;runtime info
	; (vlax-ldata-put pename "Edge" '())				;�����ڵı�,����ľ��((p1 p2 e1)....)
	; (vlax-ldata-put pename "Status_DB" 0)		;���ݿ�״̬ 0-����DB ;1-��db��
	; (vlax-ldata-put pename "Status_Modify" 0)	;�༭״̬ 0-δ�޸�,1-���޸�
	
	pename
)
;;�õ����й��ߵ��ͼ�ϵ��mapnamelist
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
	;;ȥ���ظ��ĵ��
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

;;�жϵ���Ƿ��ظ�,
;;����:val ����
;;����:nil or �ظ���ʵ�������б�
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
;;�жϵ����Ƿ�Ψһ
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
;;�ж���̽����Ƿ�Ψһ
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
;;�������е��,������һ��Ψһ���
;;����:ǰһ����� or nil��maintype ���ǰ׺ ,NameType :"EXP" "MAP" ����
;;����:�µ�Ψһ���
(defun CreateNewPointName (preName maintype NameType / newname prestr prestr0 pcount bfound allp np i pname )
	(if (= nil maintype ) (setq maintype "J"))
	(if (= nil preName) 
		(setq prestr maintype
			pcount 101
			newname (strcat maintype "101"))
		;else 
		(setq preName (strcase preName)
			prestr0 (vl-string-left-trim "0123456789-+._" preName);���ǰ׺-������	
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

;;���úͻ�ȡ���ߵ�ĵ������
;;������entp ��ʵ�����ƣ�attrstr ��������
;;����:nil or text
(defun GetAttrib-PointName(entp attrstr / eobj attrib_objs e text)
	(setq text nil)
	(if (vlax-ldata-get entp gl_AppName)
		(progn
			(setq eobj (vlax-ename->vla-object entp)
			)
			;;��ȡ�����������
			(if (= :vlax-true (vla-get-HasAttributes eobj))
				(progn
					(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes eobj))))
					(foreach e attrib_objs
						(if (= attrstr (vla-get-tagstring e))
							(setq text (vla-get-TextString e));����
						)
					)
				 );progn
			);if
		)
	)
	text
)
;;���úͻ�ȡ���ߵ�ĵ������
;;������entp ��ʵ�����ƣ�attrstr ��������;text ������������
;;����nil or T
(defun SetAttrib-PointName(entp attrstr text / eobj attrib_objs e ret)
	(setq ret nil)
	(if (vlax-ldata-get entp gl_AppName)
		(progn
			(setq eobj (vlax-ename->vla-object entp)
			)
			;;��ȡ�����������
			(if (= :vlax-true (vla-get-HasAttributes eobj))
				(progn
					(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes eobj))))
					(foreach e attrib_objs
						(if (= attrstr (vla-get-tagstring e))
							(progn
								(vla-put-TextString e text);����
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
;;;����ʵ������
;;;PutEntityAttribs
;;;entname-ʵ������,AttribsList-�����б�(("Map_No" "JS101")()....)
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
			;;��ȡ�����е�Map_No
			(setq eobj (vlax-ename->vla-object entname))
			(if (= :vlax-true (vla-get-HasAttributes eobj))
				(progn
					(setq attrib_objs (vlax-safearray->list
										  (vlax-variant-value (vla-GetAttributes eobj))
									  ) ;_ end_vlax-safearray->list
					) ;_ end_setq
					(setq i 0)
					(if (> (length attrib_objs) 0)
						(while (and (< i (length attrib_objs)) (= "���" (vla-get-tagstring (nth i attrib_objs))))
							(setq attrib_obj (nth i attrib_objs)
								  mapno     (vla-Get-TextString attrib_obj) ;����
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
;��������:GetLPointMapNo()
;���ܣ������Կ��л�ȡ���ߵ�ĵ��,�Լ���ldata Map_No��ȡ��ţ��򷵻��������ơ�
;(block_Map_No ldata-getMap_No)
;������pename����ʵ�����ƣ��޶�Ϊ��飩
;���أ�MapNolist or nil
;����ʱ�䣺2014/12/01   13:20
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun GetLPointMapNo(pename /  attrib_obj attrib_objs eobj i Map_No Map_No2 result)

	;;�޸ĵ������
	(if (not (setq eobj (vlax-ename->vla-object pename)))
		(progn
			(prompt "\n����Чʵ�塣")
			(exit)
		)	
	)
	(setq result nil)
	
	;;��ȡldata�еĵ��
	(if (setq Map_No2 (ldata-get pename "Map_No"))
		(setq result (list Map_No2))
		(setq result (list nil))
	)
	;;��ȡ�����е�Map_No
	(if (= :vlax-true (vla-get-HasAttributes eobj))
		(progn
			(setq attrib_objs (vlax-safearray->list
								  (vlax-variant-value (vla-GetAttributes eobj))
							  ) ;_ end_vlax-safearray->list
			) ;_ end_setq
			(setq i 0)
			(if (> (length attrib_objs) 0)
				(while (and (< i (length attrib_objs)) (= "���" (vla-get-tagstring (nth i attrib_objs))))
					(setq attrib_obj (nth i attrib_objs)
						  Map_No     (vla-Get-TextString attrib_obj) ;����
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
;��������:InitLineAttribs()
;���ܣ���ʼ�����Բ���д�뵽�ֶΣ������ؼ���
;������lename����ʵ�����ƣ��޶�Ϊֱ�߶Σ�
;���أ�lename
;����ʱ�䣺2014/12/01   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
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
	; ;;ͨ������
	; (vlax-ldata-put lename "Start_Point"  "")			;�����
	; (vlax-ldata-put lename "End_Point" "")			;�յ���
	; (vlax-ldata-put lename "Material" "")			;����
	; (vlax-ldata-put lename "Main_Type" "")		;������,13��,�����ַ�
	; (vlax-ldata-put lename "Sub_Type" "")		;������,1-3���ַ�
	; (vlax-ldata-put lename "Use_Status" 0)		;0-����;1-ʹ��;2-ʩ����
	
	; ;;����
	; (vlax-ldata-put lename "D_S" "")				;�ܾ���ߴ�DN100Բ��,����100*100���� mm
													; ;������:�ǵ��ŵ����ܵ�
	; (vlax-ldata-put lename "Cab_Count" "")		;������:���������
	; (vlax-ldata-put lename "Hole_Count" "")		;����:���������
	; (vlax-ldata-put lename "Hole_Used" "")		;ʹ�ÿ���:���������
	; (vlax-ldata-put lename "Pressure" "")		;ѹ��:
	; (vlax-ldata-put lename "Voltage" "")			;��ѹ;kV V
	; (vlax-ldata-put lename "Road_Name" "")		;���ڵ�·����
	; (vlax-ldata-put lename "Flowdirect" 0)		;����:ʹ������ˮ�͸�ˮ,����ʯ��ȼ����ˮ
													; ;���� 0 start-end ,1 end-start-end
	; (vlax-ldata-put lename "P_Material" "")			;�����ײ���												
	; (vlax-ldata-put lename "P_D_S" "")			;�����׹��

	; (vlax-ldata-put lename "Remark" "")			;��ע
	; (vlax-ldata-put lename "Property " "")			;Ȩ����λ	
	; (vlax-ldata-put lename "BuryWay " "")			;���跽ʽ	
													
	; ;;���
	; (vlax-ldata-put lename "Text_Pos" (list 0 0 0 0 ))	;��ŵı��λ��,4������(x y z angle)
	; ;(vlax-ldata-put lename "M_Text" "")			;�����������;
	; (vlax-ldata-put lename "Label" "") ;��ű�ǵĶ�����
	
	; ;;project info
	; (vlax-ldata-put lename "Sur_Date" "")			;����ʱ��
	; (vlax-ldata-put lename "SProject" "")	;����Ŀ����
	; (vlax-ldata-put lename "Project" "")	;����Ŀ����
	; (vlax-ldata-put lename "Location" "")		;����λ��
	; (vlax-ldata-put lename "Unit" "")			;���鵥λ
	
	; ;;runtime info
	; (vlax-ldata-put lename "Edge" '())				;�����ڵı�,����ľ��((p1 p2 e1))
	; (vlax-ldata-put lename "Status_DB" 0)			;���ݿ�״̬ 0-����DB ;1-��db��
	; (vlax-ldata-put lename "Status_Modify" 0)		;�༭״̬ 0-δ�޸�,1-���޸�,2-��ɾ����3-
	lename
)
;;����ʵ������ԣ�ʹ֮bylayer
(defun SetDefaultLayer(ent maintype)
	(AddLayer maintype (strcat maintype "_Line"))
	(vla-put-layer (vlax-ename->vla-object ent) (strcat maintype "_Line"))
)
;*********************************************************************************************
;��������:AddNewPoint(p1 pname maintype featurestr subsidstr attribslist)
;���ܣ�����һ���½��Ĺ��ߵ�
;������p1-(x y z)����,maintype-����������,attribslist-���Ա�
;		pname �����ַ�����mapname,featurestr-�����ַ��� subsidstr-������)
;���أ����ߵ�ʵ����
;����ʱ�䣺2014/12/05   10:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
 (defun AddNewPoint(p1 pname maintype featurestr subsidstr attribslist / attrib_objs e entp 
		layerobj newblock_ref symbolpath ts sstr symname  app Doc Mspace expname mapname)
	(setq layerobj (AddLayer maintype (strcat maintype "_Point")))
	
	(setq ts (Addfont 'acstyle))
   ;;
   ;;��ͼ�ⴴ��ͼ��
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
   ;;�޸ĵ����������
	(if (= :vlax-true (vla-get-HasAttributes newblock_ref))
		(progn
			(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
			(foreach e attrib_objs
				(if (= "���" (vla-get-tagstring e))
					(progn 
						(vla-put-TextString e pname);����
						(vla-put-Height e 2);�ָ�
						(vla-put-scalefactor e 0.8);�������
						(if ts (vla-put-stylename e (vla-get-name ts)))
					)
				)
			)
		 );progn
    );if
	
	;;�޸Ĺ��ߵ�����
	(setq entp (vlax-vla-object->ename newblock_ref))
	(InitPointAttribs entp)
	(PutEntityAttribs entp attribslist)
	
	(vla-put-XScaleFactor newblock_ref (/ 1.0 gl_MAP_SCALE))
	(vla-put-YScaleFactor newblock_ref (/ 1.0 gl_MAP_SCALE))
    	(vla-put-ZScaleFactor newblock_ref (/ 1.0 gl_MAP_SCALE))
	(vla-put-layer newblock_ref (strcat maintype "_Point"))
    
	;;����AppID ����ssget
	(setxdata entp gl_AppName (list (cons 1000 gl_Version))) 
	
	;;�����ּ�����When creating newpoint ,mapname and expname have the same name.
	;;else ,only change the property of name
	; (if (not gl_MapNameList)
		; (setq gl_MapNameList (GetMapNamesList))
	; )
	; (setq gl_MapNameList (cons pname gl_MapNameList))
	
	;;��������
	(if gl_PointSpaceIndex
		(setq gl_PointSpaceIndex (PutEntityIndex gl_PointSpaceIndex entp (car p1) (cadr p1)))
	)
    entp
 )
 
 ;;ɾ�����߶���(�������߶���),ͬʱ��������ɾ��
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
;��������:AddNewLine(p10 p11 maintype attribslist)
;���ܣ�����һ���½��Ĺ��߶�
;������p10 p11 -�߶�����,maintype-����������,attribslist-���Ա�
;���أ����ߵ�ʵ����
;����ʱ�䣺2014/12/05   10:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
 (defun AddNewLine(p10 p11 maintype attribslist / lineobj entl d_s)
	(AddLayer maintype (strcat maintype "_Line"))

	(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
                                      	 (vlax-3D-point p10)
                                      	 (vlax-3D-point p11)))
	;;�޸Ĺ��߶�����
	(setq entl (vlax-vla-object->ename lineobj))
	(InitLineAttribs entl)
	(PutEntityAttribs entl attribslist)
	(vla-put-layer lineobj (strcat maintype "_Line"))

    (UpdateLabel entl (ldata-get entl "Start_Deep") (ldata-get entl "End_Deep"))
	;;����AppID ����ssget
	(setxdata entl gl_AppName (list (cons 1000 gl_Version))) 
	
	;;D_S��Ϊ�߿�
	(if (setq d_s (ldata-get entl "D_S"))
		(if (> (strlen d_s) 0)
			(vla-put-LineWeight lineobj (GetLnWtFromDS (atoi d_s)))
		)
	)
	;;��������
	(if gl_LineSpaceIndex
		(progn
			(setq gl_LineSpaceIndex (PutEntityIndex gl_LineSpaceIndex entl (car p10) (cadr p10)))
			(setq gl_LineSpaceIndex (PutEntityIndex gl_LineSpaceIndex entl (car p11) (cadr p11)))
		)
	)
    entl
 )
 
 ;;���ݹܾ���ȡ�߿�
 ;;d_s Ϊ����,��λmm
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
 
 ;;��ȡ�߶���Z��ͶӰ���Ⱥ��¶�
 ;;���������������һ�£���Ϊ��������Ϊ��ֵ
 ;;�������С��gl_MIN�����¶�Ϊnil
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
	
	;;����
	(if (and podu (setq lx (ldata-get entl "Flowdirect")))
		(if (or (and (= lx 1) (< deltH 0)) (and (= lx 2) (> deltH 0))) 
			(setq podu (- 0 podu))
		)
	)
	
	(list length_l podu)
 )
 
 ;;�õ��߶ε������յ�߳�(d1 d2)
 (defun GetDeeps(entl / d1 d2 edge endp entlist name1 name2 outdata p1 p10 p11 p2 startp)
	(setq edge (ldata-get entl "Edge")
		outdata (list 99999 99999))
	(setq d1 (ldata-get entl "Start_Deep")
		 d2 (ldata-get entl "End_Deep")
	) 	
	(if (and  d1 d2)
		(setq outdata (list d1 d2))
		;;����3.0�棬�����д�ھ���
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
			; (if (not edge)	;;�ޱ�
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
									
									; ;;������������⴦�������Կ��ǣ�3.4֮ǰ��ȷ��ڵ��ϣ�
									; (if (and d1 d2)
										; (progn
											; ;;��ˮ������ˮ��
											; (if (setq subsidstr (ldata-get p2 "Subsid"))
												; (if (or (= subsidstr "��ˮ��") (= subsidstr "��ˮ��"))
													; (if (> d2 gl_MIN)
														; (if (> (/ d1 d2) 2)
															; (setq d1 (- d2 0.06))
														; )
													; )	
												; )
											; )
											; (if (setq subsidstr (ldata-get p1 "Subsid"))
												; (if (or (= subsidstr "��ˮ��") (= subsidstr "��ˮ��"))
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
			; (prompt "\nֱ�߶��ϲ����߳����ԣ���99999����̡߳�")
			; (if (= nil (car outdata))
			; (setq outdata (list 99999 (cadr outdata))))
			; (if (= nil (cadr outdata))
			; (setq outdata (list (car outdata) 99999)))
		; )
	; )
	outdata
 )
 

;*********************************************************************************************
;��������:GetTypedLineList()
;���ܣ��������߶ΰ��չ��������࣬����ɹ��߶����Main_Type����ȷ����
;������
;���أ�((J (l1 l2 l3)) (W (w1 w2..))...) ��� ͼԪ�����б�
;����ʱ�䣺2014/12/16   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
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
;��������:GetTypedPointList()
;���ܣ��������ߵ㰴�չ��������࣬����ɹ��߶����Main_Type����ȷ����
;������
;���أ�((J.(l1 l2 l3)) (W.(w1 w2..))...) ���.ͼԪ�����б�
;����ʱ�䣺2014/12/16   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
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
;��������:C:StatisticAllEntity()
;;;����:ͳ�ƹ������ϣ��ڿ���̨�����
;;;����:��
;;;����:��
;;;����ʱ�䣺2015/1/15   12:40
;;;�޸�ʱ�䣺
;;;�����ˣ����۾�
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
	(prompt "\n���ߵ�ͳ�ƽ��:")
	;;���߶η���,���,��д������ı�
	(prompt "\n\t���\t����\t���Թ��ߵ�\t���ι��ߵ�")
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
					;;�жϸ�����,�������κ����Թ��ߵ�
					(if (setq subsidstr (ldata-get ep "Subsid"))
						(if (and (/= "��" subsidstr) (> (strlen subsidstr) 0))
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
	(prompt (strcat "\n���ߵ�����:" (rtos nAll 2 0) "\t���Թ��ߵ�����Ϊ:" (rtos nmingAll 2 0) "\t���ι��ߵ�����Ϊ:" (rtos nYibiAll 2 0) "."))
	
	;;ͳ�Ƹ�������߶γ���
	(if (= gl_TableColorList nil)
		(setq gl_TableColorList (ReadColorConfig nil))
	) ;if
	(prompt "\n���߶�ͳ�ƽ��:")
	(prompt "\n\t���\t����\t���߶γ���")
	(setq lenAll 0
		nAll 0) ;�ܳ���
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
					;;���߳���Ϊƽ�泤��
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
	(prompt (strcat "\n���߶�����:" (rtos nAll 2 0) ",   �ܳ���:" (rtos lenAll 2 3) "m."))
	
	;;ͳ�Ʊ߽��߳���
	(setq i 0 len 0)
	(if	(setq allpl (ssget "X" (list (cons 0 "POLYLINE") (list -3 (list gl_AppName)))))
		(progn
				(repeat	(sslength allpl)
				(setq ent (ssname allpl i)
					i  (1+ i)
				)
				(setq len (+ len (vla-get-length (vlax-ename->vla-object ent))))
			) ;_ end_repeat
			(prompt (strcat "\n�߽�����:" (rtos i 2 0) ",   �ܳ���:" (rtos len 2 3) "m."))
		)
	) ;_ end_if
	(princ)
)


;;����������
(defun C:HSectionTable	(/		  aclayer  app		blockname		  cabcount cabused	d1		 d2		  data	   datalist	depthstr dist	  dist1	   dist2	doc		 ds		  dstr	   e1
						 e2		  ent1	   ent2		entlist	 index	  interps  layers	ldata	 len	  lineobj  mater	materialstr		  mspace   ndata	newarray newblock objblocks
						 p0		  p1	   p2		pt1		 pt2	  plineobj pmax		pmin	 press	  sxstr	   tabname	textpos	 tslayer  typestr  vol		x0		 xnum	  xrefset  y0
						 y01	  txth	   dimz		deeps)
	(prompt "\n���ƹ��ߺ������")
	(setq pt1	  (getpoint "\nѡ���������㣺")
		  pt2	  (getpoint "\nѡ��������յ㣺")
		  textpos (getpoint "\nѡ���������ֲ���㣺")
		  ) ;_ End_setq
	(if	(or (= nil pt1) (= nil pt2) (= nil textpos))
		(progn
			(prompt "\nȡ��ѡ�����꣬�˳���")
			(exit)
			) ;_ End_progn
		) ;_ End_if

	;;1���������,Z=0
	(setq app	 (vlax-get-acad-object)
		  Doc	 (vla-get-Activedocument app)
		  Mspace (vla-get-modelspace Doc)
		  ) ;_ End_setq
	;;����ͼ��
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

	;;2ѡ������ֱ��
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
			(prompt "\nδѡ���߶Σ��˳���")
			(vla-delete (vlax-ename->vla-object ent1))
			(exit)
			) ;_ End_progn
		) ;_ End_if

	(setq xnum	   (sslength xrefset)
		  index	   0
		  datalist nil
		  ) ;_ end setq

	;;3���㽻��
	(repeat	xnum
		(setq ent2 (ssname xrefset index)
			  ) ;_ End_setq
		(if	(setq interps (m_IntersectWith ent1 ent2))
			(progn
				(setq p0	   (car interps) ;ֱ�߶�ֻ��Ψһ�Ľ���
					  dist	   (vlax-curve-getDistAtPoint plineobj
														  (list (car p0) (cadr p0) 0)
														  ) ;_ End_vlax-curve-getDistAtPoint
					  datalist (cons (list dist ent2 p0) datalist) ;λ�ã�ͼԪ�����������ꣻ
					  ) ;_ End_setq
				) ;_ End_progn
			) ;_ End_if
		(setq index (1+ index))
		) ;_ End_repeat
	
	;;4���ݾ�������
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

	;;5����������ݵ�CAD:���ͣ����ʣ��ܾ������棩����������������ѹ�����ԣ������
;;;    (if datalist
;;;        (setq num (length datalist))
;;;    ) ;_ End_if
	;;�������������һ����
	(setq dimz (getvar "DIMZIN"))
	(setvar "DIMZIN" 1)

	(setq ts (AddFont 'acts))
	(setq objBlocks (vla-get-Blocks Doc))
	(setq BlockName (strcat "DB_" (rtos (* 100000000 (ZL-RAND)) 2 0)))

	;;������,�������
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
				;;���
				(if	(= gl_TableColorList nil)
					(setq gl_TableColorList (ReadColorConfig nil))
					) ;if

				;;����
				;;(setq materialstr (nth (atoi Mater) gl_Material_List))
				(setq materialstr Mater)
				;;size DS
				(setq Dstr DS)
				;;����
				(setq sxstr "")
				(if	(or (= typestr "L") (= typestr "D")) ;����
					(setq sxstr Vol)
					) ;_ End_if
				(if	(= typestr "Q") ;ȼ��
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
			;;(prompt "\n�����߶�δ�������ԣ��˳���\n�����߶�δ�������ԣ��˳���")
			) ;_ End_if

		) ;_ End_repeat

	(setvar "DIMZIN" dimz)
	;;д����ͷ��
	(if	newblock
		(progn
			(setq x0   0.0
				  y0   (+ (* ndata 4.5) 2.25)
				  txth 3
				  ) ;_ End_setq

			(setq txtobj (vla-addtext newblock "����" (vlax-3D-point 4 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 4 y0 0))

			(setq txtobj (vla-addtext newblock "����" (vlax-3D-point 12 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 12 y0 0))

			(setq txtobj (vla-addtext newblock "�ܾ�" (vlax-3D-point 22 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 22 y0 0))

			(setq txtobj (vla-addtext newblock "�ܿ���" (vlax-3D-point 32.5 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 32.5 y0 0))

			(setq txtobj (vla-addtext newblock "���ÿ�" (vlax-3D-point 41.5 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 41.5 y0 0))

			(setq txtobj (vla-addtext newblock "����" (vlax-3D-point 51 y0 0) txth))
			(vla-put-alignment txtobj acAlignmentMiddle)
			(vla-put-TextAlignmentPoint txtobj (vlax-3D-point 51 y0 0))

			(setq txtobj (vla-addtext newblock "���" (vlax-3D-point 60 y0 0) txth))
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

	;;�����InsertBlock(InsertionPoint, Name, Xscale, Yscale, ZScale, Rotation [, Password])
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

	;;�ָ���ǰͼ��
	(vla-put-activelayer Doc AcLayer)
	(vla-ZoomPrevious app)

	) ;_ End_defun



;*********************************************************************************************
;��������:C:Import_Coordinates()
;���ܣ��Ӳ����ļ������,���ֲ�
;������
;���أ�
;����ʱ�䣺2014/12/26   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:Import_Coordinates( / attribslist borderflag bordergroup borderpoints datalist depth e 
	e1 e2 firstname firstpoint index i iborder maintype nborder path path0 pipepoints
		pname points pt s1 s2 tmplist vectors)
	(prompt "\n������ߵ������ļ�,�ļ���ʽΪ�ı�.\n�ļ����ݸ�ʽΪ:���,������,������,�߳�.
				\n�Զ�����ͼ��(���ݵ��ǰ׺����ͼ��).")
	; (setq  division (Fun_InPutString "1" "USERS1" "ѡ�񻮷ַ�ʽ(1-�������,2-���ǰ׺):"))
	; (if (or (/= division "1") (/= division "2"))
		; (exit)
	; )
	(setq borderFlag (getstring "\n���ߵ�ı߽�������ĵ��+�߽��ʶ��+˳��ű�ʾ����DL101-1��\n��������߽߱��־��"))
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.csv")
		i 0
	)
	(setq gl_PointSpaceIndex (GetInsertRoot T))
	(if (setq path (getfiled "ѡ�������ļ�" path0 "csv;txt;dat" 4))
		(if (setq datalist (ReadCoordiantesFile path))
			(progn
				;;1.���Ϊ��ͨ���ߵ�ͱ߽�㣬��'-'Ϊ��־��V5.7
				(setq borderPoints nil	;;�߽���
					pipePoints nil		;;���ߵ��
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
				
				;;2.����߽�㡣����3������ɱպ϶���ߣ�
				(if (and borderPoints borderFlag (> (setq nBorder (length borderPoints)) 2))
					(progn
						;;2.1���յ�Ŵ�С����˳������
						(setq borderPoints (vl-sort borderPoints 
											(function (lambda (s1 s2) (< (car s1) (car s2)))))
						)
						;;2.2����"-"ǰ��ĵ�ŷ���
						(setq iBorder 0)
						(while borderPoints
							(setq borderGroup nil		;;1���߽�
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
							;;���Ʊ߽�
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
						(prompt (strcat "\n������" (rtos nBorder 2 0) "���߽�㣬" (rtos  iBorder 2 0) "���߽����ߡ�"))
					)
				)
				;;3.������ͨ���ߵ㣬�����ҵ���߽�ĵ㣬�����θ���������
				;;3.1���򣬲��ҷ�Χ��
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
				;;�ѱ߽�����ǰ�棬���ȼ���
				(setq pipePoints (append borderGroup pipePoints))
				(foreach e pipePoints
					(setq pname (car e)
						pt (cdr e)
					)
					;;�Ƿ������֪��
					(if (setq points (SearchPoint gl_PointSpaceIndex (car pt) (cadr pt)))
						;;������ͬλ�õ�
						(prompt (strcat "\n���" pname "��ͼ�е�λ���غϣ�δ���룡"))
						(if (not(HasSameMapName pname))
							(progn
								(setq 	maintype (GetTypeFromPointName pname)
									attribslist (list (cons "Exp_No" pname) 
													  (cons "Map_No" pname) 
													  (cons "Main_Type" maintype)
													  (cons "Feature" "������"))
								)
								;;�жϵ��Ƿ񳬳�������Χ
								;;���������������
								(if (IsOutRange gl_PointSpaceIndex (car pt) (cadr pt))
									(setq gl_PointSpaceIndex nil)	;;AddNewPoint ����������
								)
								(AddNewPoint pt pname maintype "������" "" attribslist)
								(setq gl_MapNameList (cons pname gl_MapNameList))
								;;��������
								(if (not gl_PointSpaceIndex )(setq gl_PointSpaceIndex (GetInsertRoot T)))
								(setq i (1+ i))
							)
							(prompt (strcat "\n���" pname "������δ���룡"))
						)
					)
				)
				(prompt (strcat "\n�ļ��й���" (rtos (length datalist) 2 0) "���㣬����" (rtos i 2 0) "�����ߵ�."))
			)
		)
	)
	(princ)
)

;*********************************************************************************************
;��������:ReadCoordiantesFile( path)
;���ܣ��Ӳ����ļ��������� x y z,�ļ����ݸ�ʽΪ:���,������,������,�߳�,.....
;������·��
;���أ�list ((no x y z)...)
;����ʱ�䣺2014/12/26   23:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun ReadCoordiantesFile (path / filep icount datalist str pos x y z)
	(if (not (setq filep (open path "r")))
		(progn
			 (prompt (strcat "\n���ļ�" path "ʧ�ܡ��˳���"))
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
				;;�Ǳ�����
				(if (/= 0.0 y)
					(setq datalist (cons (list pname x y z) datalist))
				)
			)
		)	  
	)
	datalist
)

;*********************************************************************************************
;��������:C:CorrectionDraft( )
;���ܣ�У����ͼ����,�����ɵ���ձ�,ͳ�Ƶ����,δ�����,ȱ�������
;ǰ������:���е㶼Ϊ���,��Ϊ���ߵ�,����������.
;������·��
;���أ�list ((no x y z)...)
;����ʱ�䣺2014/12/26   23:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:CorrectionDraft(/ allpointset data datalist e eobj errorlist filep filepath i path path0 pname pos newpos oldpos mapname )
	(prompt "\n�����������ɹ�,У����ͼ����,�����ɵ���ձ�,ͳ�Ƶ����,δ�����,ȱ�������.
			\n�����ɹ�Ϊ�ı��ļ�,���ݸ�ʽΪ:���,������,������,�߳�.
			\n������ߵ�����֮ǰ������(������->������Ӵ���)�������ӹ�ϵ.")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.csv")
	)
	(if (setq path (getfiled "ѡ������ɹ��ļ�" path0 "csv;txt;dat" 4))
		(if (setq datalist (ReadCoordiantesFile path))
			(progn
				;;1.�ƶ����ߵ�
				(setq AllPointSet (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName))))
					i 0 j 0
					errorlist nil ;;�޲��������б�
				)
				(repeat (sslength AllPointSet)
					(setq e (ssname AllPointSet i)
						eobj (vlax-ename->vla-object e)
						pname (ldata-get e "Exp_No")	;;����̽���һ�£�
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
				(prompt (strcat "\nУ����" (rtos j 2 0) "�����ߵ�."
								"\n\t��" (rtos (length errorlist) 2 0) "�����ߵ��޶�Ӧ�Ĳ�����."
								"\n\t��" (rtos (length datalist) 2 0) "���������޶�Ӧ�Ĺ��ߵ�."))
				;;output to a file
				(setq filePath   "c:\\line_info_CorrectionError.csv")
				;;�򿪵��ļ�
				 (vl-file-delete filePath)
				 (if (not (setq filep (open filePath "w")))
					 (progn
						 (Prompt (strcat "\n���ļ�" filePath "ʧ�ܡ��˳���"))
						 (exit)
					 ) ;_ End_progn
				 ) ;if
				 (write-line (strcat "��" (rtos (length errorlist) 2 0) "�����ߵ��޶�Ӧ������.") filep)
				(foreach e errorlist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),���ߵ��޶�Ӧ������" )
								filep)
				)
				(write-line (strcat "��" (rtos (length datalist) 2 0) "���������޶�Ӧ�Ĺ��ߵ�.") filep)
				(foreach e datalist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),�������޶�Ӧ�Ĺ��ߵ�" )
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
;��������:C:CorrectionXYZ( )
;���ܣ�����λ���ж�ͼ�е���ļ��еĵ��Ƿ���ͬһ���㣬��ƽ�����С��ָ��ֵ������Ϊͬһ�㣬���ƶ�ͼ�е㵽�ļ��е��λ��
;ǰ������:������ߵ�����֮ǰ������(������->������Ӵ���)�������ӹ�ϵ.
;������
;���أ�
;����ʱ�䣺2015/4/20   11:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:CorrectionXYZ(/  allpointset data datalist dist0 e eobj errorlist filep filepath 
	herror i ipoint j mindistance minh npoint p0 p01 p1 path path0 pname pos )
	(prompt "\n����λ���ж�ͼ�е���ļ��еĵ��Ƿ���ͬһ���㣬��ƽ�����С��ָ��ֵ������Ϊͬһ�㣬���ƶ�ͼ�е㵽�ļ��е��λ�á�
			\n�����ɵ���ձ�,ͳ�Ƶ����,δ�����,ȱ�������.
			\n�����ɹ�Ϊ�ı��ļ�,���ݸ�ʽΪ:���,������,������,�߳�.
			\n������ߵ�����֮ǰ������(������->������Ӵ���)�������ӹ�ϵ.")
	(setq MINDISTANCE (Fun_InPutValue 0.03 "USERR2" "\n������ƽ��������m����" 6))
	(if (= nil MINDISTANCE) (setq MINDISTANCE 0.03))
	
	(setq MINH (Fun_InPutValue 0.05 "USERR3" "\n������߳�������m����" 6))
	(if (= nil MINH) (setq MINH 0.05))
	
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.csv")
	)
	(if (setq path (getfiled "ѡ������ɹ��ļ�" path0 "csv;txt;dat" 4))
		(if (setq datalist (ReadCoordiantesFile path))
			(progn
				
				(if gl_DEBUG
					(setq gl_Time (getvar "TDUSRTIMER"))
				)
				
				;;1.�ƶ����ߵ�
				(setq AllPointSet (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName))))
					i 0 j 0
					errorlist nil ;;�޲��������б�
				
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
					;;��������ĵ�
					(setq ipoint 0)
					(while (and (> dist0 MINDISTANCE) (< ipoint npoint))
						(setq data (nth ipoint datalist)
							p1 (list (cadr data) (caddr data))
							dist0 (distance p01 p1)
							ipoint (1+ ipoint)
						)
					)
					;;�ҵ����������ĵ�
					(if (<= dist0 MINDISTANCE)
						(progn
							;;(setq datalist (vl-remove data datalist))
							(MoveOnePoint2 p0 (cdr data) e)
							;;�߳�������������档
							(setq herror (abs (- (last p0) (last (cdr data)))))
							(if (< MINH herror)
								(prompt (strcat "\n" pname "����߳����" (rtos herror 2 4) "m" "�������߳�У����"))
							)
							(setq j (1+ j))
						)
						;;esle 
						(progn
							(setq errorlist (cons (cons pname p0 ) errorlist))
							;;(prompt (strcat "\n" pname "����߳����" (rtos herror 2 4) "m" "�������߳�У����"))
						)
					)
					(setq i (1+ i))
				)
				(prompt (strcat "\nУ����" (rtos j 2 0) "�����ߵ�."
								"\n\t��" (rtos (length errorlist) 2 0) "�����ߵ��޶�Ӧ�Ĳ�����."
								"\n\t��" (rtos (length datalist) 2 0) "���������޶�Ӧ�Ĺ��ߵ�."))
				;;output to a file
				(setq filePath   "c:\\line_info_Correctionxyz.csv")
				;;�򿪵��ļ�
				 (vl-file-delete filePath)
				 (if (not (setq filep (open filePath "w")))
					 (progn
						 (Prompt (strcat "\n���ļ�" filePath "ʧ�ܡ��˳���"))
						 (exit)
					 ) ;_ End_progn
				 ) ;if
				 (write-line (strcat "��" (rtos (length errorlist) 2 0) "�����ߵ��޶�Ӧ������.") filep)
				(foreach e errorlist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),���ߵ��޶�Ӧ������" )
								filep)
				)
				(write-line (strcat "��" (rtos (length datalist) 2 0) "���������޶�Ӧ�Ĺ��ߵ�.") filep)
				(foreach e datalist
					(setq pos (cdr e))
					(write-line (strcat (car e) ",("
										(rtos (car pos) 2 4) ","
										(rtos (cadr pos) 2 4) ","
										(rtos (caddr pos) 2 4) "),�������޶�Ӧ�Ĺ��ߵ�" )
								filep)
				)
				(close filep)
				;;��������
				(GetLineRoot T)
				(GetInsertRoot T)
				(startapp "EXPLORER.EXE" filePath)
			)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nУ�������ʱ")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "��.")
		)
	)
	
	(princ)
)
;;�崦���п���߶ε�����
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
;��������:C:SetLinesProperty( )
;���ܣ��޸�ѡ���еĶ������ԣ�
;ǰ������:
;������
;���أ�
;����ʱ�䣺2015/4/10   23:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:SetLinesProperty( / ent entset etype  i n newname npro pronames property str)
	;;����ɱ༭����Ŀ����
	(setq str (list (cons 1 "��Ŀ����") (cons 2 "��λ����") (cons 3 "����Ŀ����") (cons 4 "����ʱ�䣨��ʽΪ 2014/12/01��") (cons 5 "·��")))
	(setq pronames (list "Project" "Unit" "SProject" "Sur_Date" "Road_Name"))
	(print str)

	(if (setq npro	(getint "\nѡ��Ҫ�޸ĵ����ԣ���������ǰ�Ĵ��룺"))
		(if (<= npro (length str))
			(if (setq  newname (getstring (strcat "\n�������µ�" (cdr (nth (- npro 1) str)) ":")))
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
							(print (strcat "�޸���" (rtos n 2 0) "�����߶����" (cdr (nth (- npro 1) str)) "Ϊ" newname "."))
						)
					)
				)
			)
		)
	) 
	(princ)
)

;;
;;��CAD��ɾ������
(defun C:DelEntitys(  / ent entlist ents i j p10 p11 tname)
	(princ "\n ѡ��ɾ���Ķ���")
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
					;;ɾ������;;ɾ������
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
	(princ (strcat "\nѡ����" (rtos i 2 0) "��ͼԪ��ɾ����" (rtos j 2 0) "�����߶���"))
  	(princ)
)

;;����ˢ
(defun C:PropertyCopy( / data datalist ent1 ent2 entp1 entp2 fstr  ldata2 maintype1 
		maintype2 mname name1 name2 p10 p11 sstr typename1 typename2)
	(if (setq ent1 (car (entsel "\nѡ���һ�����߶���:")))
		(progn
			(setq typename1 (cdr (assoc 0 (entget ent1))))
			(if(vlax-ldata-get ent1 gl_AppName)
				(progn
					(setq maintype1 (ldata-get ent1 "Main_Type"))
					(while (setq ent2 (car (entsel "\nѡ����һ�����߶���:")))
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
									(ldata-delete ent2 "Edge")	;;��̬����
									(setq ldata2 (vlax-ldata-get ent2 gl_AppName)
										fstr (ldata-get ent2 "Feature")
										sstr (ldata-get ent2 "Subsid")
										mname (ldata-get ent2 "Map_No")
										p10 (cdr (assoc 10 (entget ent2))))
									(AddNewPoint p10 mname maintype2 fstr sstr ldata2)
									(DelEntity ent2)
									(prompt "\n���޸Ĺ��ߵ����ԡ�")
								)
							)	
							((and (= typename1 "LINE")(or (= maintype1 maintype2) (= nil maintype2)) (= typename1 typename2))	;;ent2 ������δ�������Ե�ֱ��
								(progn
									;;1.ֱ��������
									(if (= nil maintype2)
										(progn
											;;1.�ҵ�StartPoint and End Point
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
												(prompt "\n����ѡ���ֱ�߶˵�δ���ӵ����ߵ㡣")
											)
										);;else ֱ��������
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
											(ldata-delete ent2 "Edge")	;;��̬����
											(UpdateLabel ent2 nil nil)
										)
									)
									
									(prompt "\n���޸Ĺ��߶����ԡ�")
								)
							)
							(T (prompt "\n���󣺶������Ͳ�һ�£�"))
						)
					)
				)
				;;
				(prompt "\n����ѡ��Ķ�������Ч�Ĺ��ߵ����߶Σ�")
			)
		)
	)
	(princ)
)
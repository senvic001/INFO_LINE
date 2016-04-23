;;;������:LINE_INFO_ADOLib_ConnectString2
;;;�������:dbFile(�����ݿ��·����)
;;;���ֵ:ConnectString
;;;ע��:ʹ��JET4.0����MS-Access���ݿ�,64λϵͳ�ϣ�JET�����Ѳ�֧��,�����ACE����
;;;     ʾ���� (LINE_INFO_ADOLib_ConnectString2 "d:/dbfiles/products.mdb")
;;;�����: 
;;;�������:2013��8��
;;;�޸���:
;;;�޸�����:
(defun LINE_INFO_ADOLib_ConnectString2 (dbFile / ConnectString proc_arch)
    (if (and
            (setq proc_arch (getenv "PROCESSOR_ARCHITECTURE"))
            (< 1 (strlen proc_arch))
            (eq "64" (substr proc_arch (1- (strlen proc_arch))))
        ) ;_ end_and
        (progn
            (setq ConnectString
                     (strcat "Provider=Microsoft.ACE.OLEDB.12.0;"
                             "Data Source="
                             dbFile
                             ";Persist Security Info=False"
                     ) ;_ end_strcat
            ) ;����64λϵͳ�����ݿ���������
        ) ;_ end_progn
        (progn
            (setq ConnectString
                     (strcat "Provider=Microsoft.JET.OLEDB.4.0;"
                             "Data Source="
                             dbFile
                             ";Persist Security Info=False"
                     ) ;_ end_strcat
            ) ;����32λϵͳ�����ݿ���������
        ) ;_ end_progn
    ) ;end if
    ConnectString
) ;_ end_defun


;;�����ݿ��ļ�

(defun C:LineInfo_OpenMDBFile ( / result strpath)
    ; (if (setq strpath (Xrecord-Get gl_AppName "ConnectString"))
        ; (progn
            ; (prompt (strcat "\n\n��ͼ���������ݿ⣺" (car strPath) "\n���������������ٽ������ӣ�"
                        ; "����ɾ�������ݿ⣬�ٴ��µ����ݿ⡣\n"))
            ; (exit)
            ; );progn
        ; )
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "\\database.mdb")
	)
    (if (setq strPath (getfiled "ѡ������������ݵ�Access���ݿ��ļ�" path0 "MDB" 0))
        (progn
            (prompt (strcat "\n\n���ݿ�·����" strPath "\n"))
	    (if (setq result (getstring "\n��֧�ֺ��ݹ������ݿ⣬�����ݿ��Ǳ�׼���ݹ������ݿ���?(Y/N)"))
	        (if (= "Y" (strcase result))
				(progn
					(INIT_LINEINFO strPath)
					;�������ݿ�
					; (setq result (getstring "\n�������ݿ�,����ͼ��?(Y/N)"))
					; (if (= "Y" (strcase result))
						; (C:LineInfo_DrawAll)
					; )
				)
	        )
	    )
            );progn
        (prompt (strcat "\n\nδ���κ����ݿ⡣" ))
        );if
    
    (prin1)
)

;;�½����ݿ��ļ�������һ���µĵ���ͼʱ����Ҫ�������ݿ�
(defun LineInfo_NewMDBFile (dbfile / result strpath dbpath path0)
    (LineInfo_GetSupportPath)
    ; (if (setq strpath (Xrecord-Get gl_AppName "ConnectString"))
        ; (progn
            ; (prompt (strcat "\n\n��ͼ���������ݿ⣺" (car strPath) "\n���������������ٽ������ӣ�"
                        ; "����ɾ�������ݿ⣬�ٴ��µ����ݿ⡣"))
            ; (exit)
            ; );progn
        ; )
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "\\database.mdb")
	)
    (setq strPath (getfiled "�½������������ݵ�Access���ݿ��ļ�" path0 "MDB" 1))
    ;;���ƿհ����ݿ⣬������
    (if strPath
        (progn
            (setq dbpath (strcat gl_INFO_LINE_PATH "database\\" dbfile ))
            (vl-file-delete strPath)
            (if (vl-file-copy  dbpath  strPath )
                (progn
                    (prompt (strcat "\n�½����ݿ�ɹ���\n���ݿ�·����" strPath "\n"))
                    (INIT_LINEINFO strPath)
                    );progn
                ;;else
                (prompt (strcat "\n\�����ݿ��ļ�" strPath "����\n"))
                )
            );prong
        (progn
            (prompt (strcat "\n���ļ�����"))
            );
        );if
    
    (prin1)
)

;;;;;��ʾ���ݿ���Ϣ
(defun C:LineInfo_GetDBInfo ( /  strpath tables tb tbs)
    (if (setq strpath (Xrecord-Get gl_AppName "ConnectString"))
        (progn
            (print strpath)
            (setq tables (Xrecord-Get gl_AppName "Tables"))
            (print tables)
            (foreach tb tables
                (setq tbs (Xrecord-Get gl_AppName (strcat "Table_" tb)))
                (print tbs)
                )
            );progn
        (prompt (strcat "\nͼ�в���" gl_AppName "���ݿ⡣\n"))
      );if  
);defun

;;ɾ��ͼ����Xrecord��ŵ����ݿ����Ϣ��
;;����ɾ��ͼԪ�е����ݼ�¼��Ҳ��ɾ�����ݿ��ļ���
(defun C:LineInfo_DelDBInfo ( / strdb tables tb)
    (if (setq strdb (Xrecord-Get gl_AppName "ConnectString"))
        (progn
            (setq tables (Xrecord-Get gl_AppName "Tables"))
            (foreach tb tables
                (Xrecord-Delete gl_AppName (strcat "Table_" tb))
                )
            (Xrecord-Delete gl_AppName "Tables")
            (Xrecord-Delete gl_AppName "ConnectString")

            (prompt (strcat "\nɾ��" gl_AppName "���ݿ�ɹ���\n"))
        );tpron
        (prompt (strcat "\nͼ�в���" gl_AppName "���ݿ⡣\n"))
    );if

);defun

;*********************************************************************************************
;��������:Init_LineInfo(dbpathname)
;���ܣ��������ݿ���Ϣ��
;�����ֵ�gl_AppName�������������ݣ�TABLE_POINT ����ֶ� TABLE_LINE �߱��ֶ�
;������MDB���ݿ��ļ���·��
;���أ�
;����ʱ�䣺2014/07/11   16:10
;�޸�ʱ�䣺2015/12/31	13:53
;�����ˣ����۾�
;*********************************************************************************************
(defun Init_LineInfo(dbpathname / app collist columnslist connectionobject connectstring dict dictname dicts doc  index num tableslist tname)
   (setq app (vlax-get-acad-object)
          doc (vla-get-activedocument app)
          dicts (vla-get-Dictionaries doc)
          dict (vla-add dicts gl_AppName) ;��appName��Ϊ�ֵ���
          dictname gl_AppName
          )
   ;;�������ݿ�
   (setq ConnectionObject nil
	 ;;ConnectString (strcat "Provider=MSDASQL;Driver={Microsoft Access Driver (*.mdb)};DBQ=" dbpathname ))
	 ConnectString (LINE_INFO_ADOLib_ConnectString2 dbpathname));��Ӧ32��64λϵͳ
   (prompt (strcat "\n\nConnecting to the database using \n\""
                  ConnectString
                  "\""))
    (if (not (setq ConnectionObject (ADOLISP_ConnectToDB ConnectString "admin" "")))
    (progn
      (prompt "\nConnection failed!")
      (ADOLISP_ErrorPrinter)
    )
     (prompt "\nResult: succeeded!")
  )
  ;; If we got a connection ,get the table info of database
	(if ConnectionObject
		(progn
			; (setq TablesList (car (ADOLISP_GetTablesAndViews ConnectionObject)))
			; (Xrecord-Rebuild dictname "Tables" TablesList);д�����б���("T1name" "T2name"...)
			
			;;д������߱��ֶε�xrecord "TABLE_POINT"
			; (setq tname "JP"
				; ColumnsList (ADOLISP_GetColumns ConnectionObject tname))
			; (Xrecord-Rebuild dictname "TABLE_POINT" ColumnsList);"Table_JP"
			; ;;д������߱��ֶε�xrecord TABLE_LINE
			; (setq tname "JL"
				; ColumnsList (ADOLISP_GetColumns ConnectionObject tname))
			; (Xrecord-Rebuild dictname "TABLE_LINE" ColumnsList);"Table_JP"
			
			; (ADOLISP_DisconnectFromDB ConnectionObject)
			; (setq ConnectionObject nil)
				;;д�������ַ�
			(Xrecord-Rebuild dictname "ConnectString" ConnectString)
		);pron
    );
);defun

;;��ȡ���Ӷ���
;;�����ݿ⣬�������ݿ���� or nil
;;�ǵùر����ݿ�����
(defun GetConnectionObject( / connectstr )
    (setq ConnectionObject nil)
    ;;
    (if(setq connectstr (Xrecord-Get gl_AppName "ConnectString"))
		(if (not (setq ConnectionObject (ADOLISP_ConnectToDB (car connectstr) "admin" "")))
			(progn
			  (prompt "\nConnection failed!")
			  (ADOLISP_ErrorPrinter)
			)
		);if
		;;else open a existing db file
		(progn
			(C:LineInfo_OpenMDBFile)
			(setq ConnectionObject (GetConnectionObject))
		)
    );if
    ConnectionObject
);defun

;*********************************************************************************************
;��������:C:toHZDB()
;���ܣ��ѵ���е�ʵ�屣�浽���ݿ�
;������
;���أ�
;����ʱ�䣺2014/12/30   22:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:toHZDB	(/ all-line allp entlist i outlist ent)
	(print "\n����ͼ�����ݵ����ݹ������ݿ�.")
	(LineInfo_NewMDBFile "Line_Info_empty.MDB") 
	
	(setq entlist nil
		  outlist nil
		  i	0
	) ;_ end_setq
	;;save points
	(if	(setq allp (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName)))))
		(repeat	(sslength allp)
            (setq ent (ssname allp i)
				entlist (cons ent entlist)
				i  (1+ i)
			)
		) ;_ end_repeat
	) ;_ end_if
	(if	(setq outlist (SavePoinsDB entlist nil))
		(prompt (strcat "\n����" (rtos (length outlist) 2 0) "�����ߵ㵽���ݿ�."))
	) ;_ end_if
	
	;;save lines
	(setq outlist nil entlist nil i 0)
	(if	(setq all-line (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName)))))
		(repeat	(sslength all-line)
			(setq ent (ssname all-line i)
				entlist (cons ent entlist)
				i  (1+ i)
			)
		) ;_ end_repeat
	) ;_ end_if
	(if	(setq outlist (SaveLinesDB entlist nil))
		(prompt (strcat "\n����" (rtos (length outlist) 2 0) "�����߶������ݿ�."))
	) ;_ end_if
	
	;;save border
	(SaveBorderData)
	(princ)
) ;_ end_defun


;*********************************************************************************************
;��������:C:toDB()
;���ܣ��ѵ���е�ʵ�屣�浽DB���ݿ�Line_Info_empty_s
;������
;���أ�
;����ʱ�䣺2016/4/13  15:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:toDB( / all-line allp entlist i outlist ent)
	(print "\n����ͼ�����ݵ�DB�������ݿ�.")
	(LineInfo_NewMDBFile "Line_Info_empty_s.MDB") 
	
	(setq entlist nil
		  outlist nil
		  i	0
	) ;_ end_setq
	;;save points
	(if	(setq allp (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName)))))
		(repeat	(sslength allp)
            (setq ent (ssname allp i)
				entlist (cons ent entlist)
				i  (1+ i)
			)
		) ;_ end_repeat
	) ;_ end_if
	(if	(setq outlist (SavePoinsDB entlist "POINTS"))
		(prompt (strcat "\n����" (rtos (length outlist) 2 0) "�����ߵ㵽���ݿ�."))
	) ;_ end_if
	
	;;save lines
	(setq outlist nil entlist nil i 0)
	(if	(setq all-line (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName)))))
		(repeat	(sslength all-line)
			(setq ent (ssname all-line i)
				entlist (cons ent entlist)
				i  (1+ i)
			)
		) ;_ end_repeat
	) ;_ end_if
	(if	(setq outlist (SaveLinesDB entlist "LINES"))
		(prompt (strcat "\n����" (rtos (length outlist) 2 0) "�����߶ε����ݿ�."))
	) ;_ end_if
	
	;;save border
	(SaveBorderData)
	(princ)
)

;*********************************************************************************************
;��������:C:fromDB()
;���ܣ���DB���ݿ��ͼ
;������
;���أ�
;����ʱ�䣺2016/4/13  15:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:fromDB()
	(DrawTables2)
	(princ)
)

;*********************************************************************************************
;��������:SaveEntitysDB(entlist)
;���ܣ���ʵ����е�ʵ�屣�浽���ݿ�
;������ʵ�������ldata
;���أ������ʵ�������ɹ�����ñ���Ϊentlist,���޸�ʵ���״̬�룻
;����ʱ�䣺2014/07/18   18:00
;�޸�ʱ�䣺2014/12/30   21:00
;�����ˣ����۾�
;*********************************************************************************************
;;tablename ���������nil or LINES�����Ϊnil���Զ�ʶ��Ϊ������+P��Ϊ�������������ͳһ�ĵ��POINTS
(defun SavePoinsDB (entlist tablename / attrib_objs attrib_pt columnslist columnstr connectionobject data 
	datalist delstr e endp maintype newblock_ref outlist pent result rotation sqlstr startp 	typename valuestr)
	(setq outlist nil)
	(if (setq ConnectionObject (GetConnectionObject))
		(foreach pent  entlist
			(if (vlax-ldata-get pent gl_AppName)
				(progn
					(setq typename (cdr (assoc 0 (entget pent)))
						maintype (ldata-get pent "Main_Type")
						columnstr ""
						valuestr ""
						sqlstr ""
					)
					;;ȷ������
				    (if (= nil tablename)
						(setq tablename (strcat maintype "P"))
					)
					;;����SQL���
					(if (and tablename (= "INSERT" typename))
						(progn
							(setq datalist (vlax-ldata-get pent gl_AppName))
							
							(if (setq data  (assoc "Main_Type" datalist))
								(setq valuestr (strcat "'" (cdr data) "'")
									columnstr "Main_Type")
							)
							(if (setq data  (assoc "Exp_No" datalist))
								(setq valuestr (strcat valuestr ",'" (cdr data) "'")
									columnstr (strcat columnstr ", Exp_No"))
							)
							(if (setq data (assoc "Map_No" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Map_No"))
							)
						
							(if (setq data (assoc "Subsid" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Subsid"))	
							)
							(if (setq data (assoc "GC_X" datalist))
								(setq valuestr (strcat valuestr ", " (rtos (cdr data) 2 4))
									columnstr (strcat columnstr ", GC_X"))
							)
							(if (setq data (assoc "GC_Y" datalist))
								(setq valuestr (strcat valuestr ", " (rtos (cdr data) 2 4))
									columnstr (strcat columnstr ", GC_Y"))
							)
							(if (setq data (assoc "Point_Size" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Point_Size"))
							)
							;;real
							(if (setq data (assoc "Well_Deep" datalist))
								(if (> (strlen (cdr data)) 0)
									(setq valuestr (strcat valuestr ", " (cdr data) )
										columnstr (strcat columnstr ", Well_Deep"))
									(setq valuestr (strcat valuestr ", Null" )
										columnstr (strcat columnstr ", Well_Deep"))
								)
							)
							(if (setq data (assoc "Location" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Location"))
							)
							(if (setq data (assoc "Version" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Version"))
							)
							
							(if (setq data (assoc "Feature" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Feature"))
							)
							;;��ȡ�������ֵ�λ����Ϣ
							(setq newblock_ref (vlax-ename->vla-object pent))
							(if (= :vlax-true (vla-get-HasAttributes newblock_ref))
								(progn
									;;rotation
									(setq rotation (vla-get-rotation newblock_ref)
										valuestr (strcat valuestr ", " (rtos rotation 2 4))
										columnstr (strcat columnstr ", Mark_Angle"))
									;;text insertpoint
									(setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes newblock_ref))))
									(foreach e attrib_objs
										(if (= "���" (vla-get-tagstring e))
											(if (setq attrib_pt (vla-get-InsertionPoint  e ))
												(progn 
													(setq attrib_pt (vlax-safearray->list (vlax-variant-value attrib_pt))
														valuestr (strcat valuestr ", " (rtos (car attrib_pt) 2 4) ", " (rtos (cadr attrib_pt) 2 4) ", " (rtos (caddr attrib_pt) 2 4))
														columnstr (strcat columnstr ", Text_X, Text_Y, Text_Z"))
												)
											)
										)
									)
								 );progn
							);if
							(if (setq data (cdr (assoc 10 (entget pent))))
								(setq valuestr (strcat valuestr ", " (rtos (cadr data) 2 4) ", " (rtos (car data) 2 4) ", " (rtos (caddr data) 2 4))
									columnstr (strcat columnstr ", X, Y, Surf_H"))
							)
							
							(if (setq data (assoc "SProject" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", SProject_Name"))
							)
							
							(if (setq data (assoc "Project" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Project_Name"))
							)
							
							(if (setq data (assoc "Unit" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Sur_Unit"))
							)
							;;--------���ݹ��ߵ���5.0-----------------
							(if (setq data (assoc "JG_Material" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", JG_Material"))
							)
							(if (setq data (assoc "JG_Shape" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", JG_Shape"))
							)
							(if (setq data (assoc "JS_Type" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", JS_Code"))	;;JS_Code
							)
							; (if (setq data (assoc "JS_Size" datalist))
								; (setq valuestr (strcat valuestr ", '" (cdr data) "'")
									; columnstr (strcat columnstr ", JS_Size"))
							; )
							(if (setq data (assoc "Neck_Deep" datalist))
								(if (> (strlen (cdr data)) 0)
									(setq valuestr (strcat valuestr ", " (cdr data) )
										columnstr (strcat columnstr ", Neck_Deep"))
									(setq valuestr (strcat valuestr ", Null" )
										columnstr (strcat columnstr ", Neck_Deep"))
								)	
							)
							(if (setq data (assoc "Off_Well" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Off_Well"))
							)
							(if (setq data (assoc "Well_Memo" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Well_Memo"))
							)
							(if (setq data (assoc "Build_Unit" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", B_Code"))	;;B_Code
							)
							;;Neck_Deep JG_Material JG_Shape JS_Size JS_Type Off_Well Well_Memo Build_Unit
							;;----------------���ݹ��ߵ���-----------end
						)
					)
				
					(setq sqlstr (strcat "INSERT INTO " tablename " (" columnstr ") VALUES (" valuestr ")" ))
					(if(setq Result (ADOLISP_DoSQL ConnectionObject sqlstr))
						(progn
							(ldata-put  pent "Status_DB" 1)
							(setq outlist (cons pent outlist ))
						);progn
					   (ADOLISP_ErrorPrinter )
					)
				)
			)		
		)
		(ADOLISP_DisconnectFromDB ConnectionObject)
	)	
	outlist
)
;;tablename ���������nil or LINES�����Ϊnil���Զ�ʶ��Ϊ������+L��Ϊ�������������ͳһ���߱�LINES
(defun SaveLinesDB (entlist tablename / attrib_objs attrib_pt columnslist columnstr connectionobject data 
	datalist delstr e endp maintype newblock_ref outlist pent result rotation sqlstr startp typename valuestr)
	(setq outlist nil)
	(if (setq ConnectionObject (GetConnectionObject))
		(foreach pent  entlist
			(if (vlax-ldata-get pent gl_AppName)
				(progn
					(setq typename (cdr (assoc 0 (entget pent)))
						maintype (ldata-get pent "Main_Type")
						columnstr ""
						valuestr ""
						sqlstr ""
					)
					;;ȷ������
				    (if (= nil tablename)
						(setq tablename (strcat maintype "L"))
					)
					;;�����߶�
					(if (and tablename (= "LINE" typename))
						(progn
							(setq datalist (vlax-ldata-get pent gl_AppName)
								startp (cdr (assoc "Start_Point" datalist))
								endp (cdr (assoc "End_Point" datalist))
							)
							
							(if startp
								(setq valuestr (strcat "'" startp "'")
									columnstr "Start_Point")
							)
							(if endp
								(setq valuestr (strcat valuestr ", '" endp "'")
									columnstr (strcat columnstr ", End_Point"))
							)
							
							(if (setq data  (assoc "Main_Type" datalist))
								(setq valuestr (strcat valuestr ",'" (cdr data) "'")
									columnstr (strcat columnstr ", Main_Type"))
							)
							(if (setq data (GetDeeps pent))
								(setq valuestr (strcat valuestr ", " (rtos (car data) 2 3) ", " (rtos (cadr data) 2 3))
									columnstr (strcat columnstr ", Start_Deep, End_Deep"))
							)
							
							(if (setq data (assoc "Material" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Material"))
							)
							(if (setq data (assoc "D_S" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", D_S"))
							)
							
							(if (setq data (assoc "Sur_Date" datalist))
								(if (= "" (cdr data))
									(setq valuestr (strcat valuestr ", Null")
										columnstr (strcat columnstr ", Sur_Date"))
									(setq valuestr (strcat valuestr ", '" (cdr data) "'")
										columnstr (strcat columnstr ", Sur_Date"))
								)
							)
							(if (setq data (assoc "Mdate" datalist))
								(if (= "" (cdr data))
									(setq valuestr (strcat valuestr ", Null")
										columnstr (strcat columnstr ", Mdate"))
									(setq valuestr (strcat valuestr ", '" (cdr data) "'")
										columnstr (strcat columnstr ", Mdate"))
								)
							)
							(if (setq data (assoc "Pressure" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Pressure"))
							)
							
							(if (setq data (assoc "Voltage" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Voltage"))
							)
							
							(if (setq data (assoc "Cab_Count" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Cab_Count"))
							)
							
							(if (setq data (assoc "Hole_Count" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Hole_Count"))
							)
							(if (setq data (assoc "Hole_Used" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Hole_Used"))
							)
							(if (setq data (assoc "Flowdirect" datalist))
								(setq valuestr (strcat valuestr ", " (rtos  (cdr data) 2 0))
									columnstr (strcat columnstr ", Flowdirect"))
							)
							
							(if (setq data (assoc "SProject" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", SProject_Name"))
							)
							
							(if (setq data (assoc "Project" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Project_Name"))
							)
							
							(if (setq data (assoc "Unit" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Sur_Unit"))
							)
							(if (setq data (assoc "Road_Name" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Road_Name"))
							)
							(if (setq data (assoc "Location" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Location"))
							)
							;;--------------------------���ݹ��ߵ���5-------------
							;;Line_Style Line_Memo Build_Unit Block_Size1 Block_Size2
							(if (setq data (assoc "Line_Style" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Line_Style"))
							)
							(if (setq data (assoc "Line_Memo" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Line_Memo"))
							)
							(if (setq data (assoc "Build_Unit" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", B_Code"))
							)
							(if (setq data (assoc "BuryWay" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", BuryWay"))
							)
							(if (setq data (assoc "Block_Size1" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Block_Size1"))
							)
							(if (setq data (assoc "Block_Size2" datalist))
								(setq valuestr (strcat valuestr ", '" (cdr data) "'")
									columnstr (strcat columnstr ", Block_Size2"))
							)
							;;--------------------------���ݹ��ߵ���end------------------
						)
					)
				
					(setq sqlstr (strcat "INSERT INTO " tablename " (" columnstr ") VALUES (" valuestr ")" ))
					(if(setq Result (ADOLISP_DoSQL ConnectionObject sqlstr))
						(progn
							(ldata-put  pent "Status_DB" 1)
							(setq outlist (cons pent outlist ))
						);progn
					   (ADOLISP_ErrorPrinter )
					)
				)
			)		
		)
		(ADOLISP_DisconnectFromDB ConnectionObject)
	)	
	outlist
)

;��������:DelPointRows(ConnectionObject tablename namelist)
;���ܣ�delelte points 
;����: 
;����ֵ��bool
;����ʱ�䣺2011/02/08 12:00
;�޸�ʱ�䣺2014/07/20 12:00
;�����ˣ����۾�
;*********************************************************************************************
(defun DelPointRows  (ConnectionObject tablename namelist / pname sqlstr)
    (foreach pname  namelist
        (setq sqlstr (strcat "DELETE * FROM " tablename " WHERE Map_No='" pname "' "))
        (ADOLISP_DoSQL ConnectionObject sqlstr)
        )
    ) ;defun

;*********************************************************************************************
;��������:AddNewRows(ConnectionObject tablename datalist)
;���ܣ���datalist ���뵽����
;����: ���Ӷ��󣬱��������ݱ�(() () ....).datalist�е�ÿ��Ԫ�ر�ʾ���ݿ���һ��
;����ֵ��bool
;����ʱ�䣺2011/02/06 12:00
;�޸�ʱ�䣺2014/07/20 12:00
;�����ˣ����۾�
;*********************************************************************************************
(defun AddNewRows  (ConnectionObject tablename datalist / datastr idata num oneitem onerow result sqlstr)
    (setq num   (length datalist)
          idata 0)
    (if (> num 0)
        (foreach onerow  datalist ;onerow is a list
            (progn
                (setq datastr ""
                      sqlstr "")
                (foreach oneitem  onerow
                    (progn
                        (cond
                            ((= (type oneitem) 'STR)
                            	(setq oneitem (strcat "'" oneitem "'")))
                            ((= (type oneitem) 'INT)
                            	(setq oneitem (rtos oneitem 2 0)))
                            ((= (type oneitem) 'REAL)
                            	(setq oneitem (rtos oneitem 2 4)))
                            ((= oneitem nil)
                            	(setq oneitem "Null"))
                            ) ;if
                        (setq datastr (strcat datastr oneitem ","))
                        ) ;progn
                    ) ;foreach
                (setq datastr (vl-string-right-trim "," datastr)
                      datastr (strcat "( " datastr " )")
                      sqlstr  (strcat "INSERT INTO " tablename " VALUES " datastr))
 		;;(ADOLISP_DoSQL ConnectionObject sqlstr)
                (setq Result (ADOLISP_DoSQL ConnectionObject sqlstr))
                ) ;progn
            ) ;foreach
        ) ;if
    ) ;defun


;*********************************************************************************************
;��������:GetAllPointsFromDB()
;���ܣ���ȡ���ݿ��еĵ㣬���ڵ���У�
;������
;���أ����(("p1" "JP")( "p2" "JP)...)
;����ʱ�䣺2014/07/20   18:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun GetAllPointsFromDB( / connectionobject  outlist result sqlstr tables tb)
    (setq outlist nil)
    (if (setq ConnectionObject (GetConnectionObject))
        (progn
            (setq tables (Xrecord-Get gl_AppName "Tables"));tablenames
            (foreach tb  tables
                (if (vl-string-search "P" tb 1);�����ĵڶ����ַ�����P
                    (Progn
                        (setq SQLstr (strcat "SELECT Map_No FROM " tb ))
                        (if (setq Result (ADOLISP_DoSQL ConnectionObject sqlstr))
                            (Progn
                            	(setq Result (cdr Result))
                                (foreach e Result
                                	(setq outlist (append outlist (list (cons (car e) tb))))
                                    );foreach
                                );progn
                            ) ;if
                        ) ;progn
                    ) ;if
                ) ;foreach
            
            (ADOLISP_DisconnectFromDB ConnectionObject)
            ) ;progn
        ) ;if
    outlist
    ) ;defun

;��������:GetAllPoinsFromDB()
;���ܣ���ȡ���ݿ��еĵ㣬���ڵ���У�
;���أ����((p1 p2 JL)(p3 p4 JL)...)
(defun GetAllLinesFromDB( / connectionobject  outlist result sqlstr tables tb)
    (setq outlist nil)
    (if (setq ConnectionObject (GetConnectionObject))
        (progn
            (setq tables (Xrecord-Get gl_AppName "Tables"));tablenames
            (foreach tb  tables
                (if (vl-string-search "L" tb 1);�����ĵڶ����ַ�����P
                    (Progn
                        (setq SQLstr (strcat "SELECT Start_Point,End_Point FROM " tb ))
                        (if (setq Result (ADOLISP_DoSQL ConnectionObject sqlstr))
                            ;;(setq outlist (append outlist (cdr Result)))
                            (Progn
                            	(setq Result (cdr Result))
                                (foreach e Result
                                	(setq outlist (append outlist (list (list (car e) (cadr e) tb))))
                                    );foreach
                                )
                            ) ;if
                        ) ;progn
                    ) ;if
                ) ;foreach
            
            (ADOLISP_DisconnectFromDB ConnectionObject)
            ) ;progn
        ) ;if
    outlist
    )
;*********************************************************************************************
;��������:LINF_INFO_Stat()
;���ܣ�������ͳ�ƣ��������߶��ܳ��������߶γ���
;������
;���أ�
;����ʱ�䣺2014/07/21   15:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************


;*********************************************************************************************
;��������:SynchDB-DWG()
;���ܣ�ͬ�����ݿ��ͼ���ļ�������
;������
;���أ�
;����ʱ�䣺2014/07/21   15:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
; (defun C:SynchDWG-DB  (/ alldb-lines alldb-points alllinerefset allpointblockset allpointlist bs connectionobject dbsuccess
                       ; dline dpoint e1 el endp i indb indwg ldata line mapno multilines multipoints nline outlist
                       ; p1 p2 pdata point pointdatalist pretodb pretodwg-line pretodwg-point ptype re result scode startp strsql table tableline tablep xpos ypos zpos)
    ; (setq ConnectionObject (GetConnectionObject))
    ; (if (= nil ConnectionObject)
        ; (progn
            ; (setq re (Fun_InPutString "N"
                                      ; "USERS5"
                                      ; "\nδ�ҵ����ݿ��ļ����½�һ�����ݿ��ļ����һ�����е�MDB�ļ���(�½�N/��O)"))
            ; (if (= "N" (strcase re))
                ; (C:LineInfo_NewMDBFile))
            ; (if (= "O" (strcase re))
                ; (C:LineInfo_OpenMDBFile))
            ; ) ;progn
        ; ) ;if
    ; (setq ConnectionObject (GetConnectionObject))
    
    ; (prompt "\n���ݿ���ͼ���ļ���ʼͬ��......")
    ; (setq pretoDB nil ;������ͼԪ��
          ; pretoDWG-point nil ;����������
          ; pretoDWG-line nil ;����������
          ; )
    ; (setq AllPointBlockSet (ssget "X" (list (cons 0 "INSERT")))
          ; AllLineRefSet    (ssget "X" (list (cons 0 "LINE")))
          ; MultiPoints      nil ;�ظ����
          ; MultiLines       nil
          ; )
    ; ;;1Points form DWG to DB
    ; (if (setq ConnectionObject (GetConnectionObject))
        ; (progn
            ; (setq AllDB-points (GetAllPointsFromDB)
                  ; i            0)
            ; ;;DWG-POINTS to DB-POINT
            ; (repeat (sslength AllPointBlockSet)
                ; (setq bs (ssname AllPointBlockSet i))
                ; (if (setq ldata (vlax-ldata-get bs gl_AppName))
                    ; (progn
                        ; (setq MapNo (caddr ldata))
                        ; (if (null (assoc MapNo AllDB-points))
                            ; (setq pretoDB (append pretoDB (list bs))) ;Ψһ�㣬��������б�
                            ; )
                        ; ) ;progn
                    ; ) ;if
                ; (setq i (1+ i))
                ; ) ;foreach

            ; ;;DWG-LINES to DB-LINES
            ; (setq i           0
                  ; AllDB-lines (GetAllLinesFromDB)
                  ; inDB 0)
            ; (repeat (sslength AllLineRefSet)
                ; (setq bs (ssname AllLineRefSet i))
                ; (if (setq ldata (vlax-ldata-get bs gl_AppName))
                    ; (progn
                        ; (setq startp (nth 1 ldata)
                              ; endp   (nth 2 ldata))

                        ; (foreach el AllDB-lines
                            ; (if (or (and (= startp (car el)) (= endp (cadr e1)))
                                    ; (and (= endp (car el)) (= startp (cadr e1))))
                                ; (setq inDB 1)
                                ; )
                            ; );foreach
                        ; (if (= 0 inDB)
                            ; (setq pretoDB (append pretoDB (list bs)));Ψһ�߶Σ���������б�
                            ; )
                        ; (setq inDB 0)
                        ; ) ;progn
                    ; ) ;if
                ; (setq i (1+ i))
                ; ) ;repeat

            ; ;;2.db->dwg
            ; ;;DB-PointS to Dwg-Points
            ; (setq i 0
                  ; inDWG 0)
            ; (foreach dpoint  AllDB-points
                ; (repeat (sslength AllPointBlockSet)
                    ; (setq bs (ssname AllPointBlockSet i))
                    ; (if (setq ldata (vlax-ldata-get bs gl_AppName))
                        ; (progn
                            ; (setq MapNo (caddr ldata))
                            ; (if (= MapNo (car dpoint))
                                ; (setq inDWG 1))
                            ; ) ;progn
                        ; ) ;if
                    ; (setq i (1+ i))
                    ; ) ;repeat
                ; (if (= 0 inDWG) ;�õ㲻��ͼ��
                    ; (setq pretoDWG-point (append pretoDWG-point (list dpoint)))
                    ; )
                ; (setq inDWG 0)
                ; ) ;foreach

            ; ;;DB-Lines to Dwg-Lines
            ; (setq i 0
                  ; inDWG 0)
            ; (foreach dline  AllDB-lines
                ; (repeat (sslength AllLineRefSet)
                    ; (setq bs (ssname AllLineRefSet i))
                    ; (if (setq ldata (vlax-ldata-get bs gl_AppName))
                        ; (progn
                            ; (setq startp (nth 1 ldata)
                                  ; endp   (nth 2 ldata))
                                  
                            ; (if (or (and (= startp (car dline)) (= endp (cadr dline)))
                                    ; (and (= endp (car dline)) (= startp (cadr dline))))
                                ; (setq inDB 1)
                                ; );if
                            ; ) ;progn
                        ; ) ;if
                    ; (setq i (1+ i))
                    ; ) ;repeat
                ; (if (= 0 inDWG) ;�õ㲻��ͼ��
                    ; (setq pretoDWG-line (append pretoDWG-line (list dline)))
                    ; )
                ; (setq inDWG 0)
                ; ) ;foreach



            ; ;;2  save data to DB
            ; (setq outlist nil
                  ; dbsuccess 0)
            ; (if pretoDB
                ; (setq outlist (SaveEntitysDB pretoDB))
                ; ) ;
	    ; (if (= (length pretoDB) (length outlist))
                ; (setq dbsuccess 1);д�����ݳɹ���
                ; )

            ; ;;Draw db to DWG)
            ; ;;pretoDWG-point nil ;����������
            ; ;;pretoDWG-line nil ;����������
            ; ;;3-1�õ�������Ҫ��ͼ�ĵ����������ݱ�;
            ; (setq allPointlist pretoDWG-point)
            ; (if (/= nil pretoDWG-line)
                ; (foreach nline  pretoDWG-line
                    ; (setq table (strcat (substr (caddr nline) 1 1) "P"))
                    ; (if allPointlist
                        ; (Progn
	                        ; (if (= nil (assoc (car nline) allPointlist))
	                            ; (setq allPointlist (append allPointlist (list (list (car nline) table))))
	                         ; );if
	                        ; (if (= nil (assoc (cadr nline) allPointlist))
	                            ; (setq allPointlist (append allPointlist (list (list (cadr nline) table))))
	                         ; );if
                        ; );progn
                        ; (setq allPointlist (append allPointlist (list (list (car nline) table)))
                              ; allPointlist (append allPointlist (list (list (cadr nline) table))))
                        ; ) ;if
                    ; ) ;foreach
                ; ) ;
            ; ;;3-2��ѯ����
            ; (setq pointdatalist nil)
	    ; (foreach  point allPointlist
                ; (setq strSQL (strcat "SELECT * FROM " (cadr point) " WHERE Map_No=" "'" (car point) "'"))
                ; (if (setq result (ADOLISP_DoSQL ConnectionObject strSQL))
                    ; (setq pointdatalist (append pointdatalist (cdr result)))
                    ; );if
                ; );foreach
            
            ; ;;3-3����ͼ��
            ; ;;3-3-1���Ƶ�DrawPoint (strP x y z scode scale  pointtype
            ; (foreach point pretoDWG-point
                ; (setq mapno (car point)
                      ; pdata (assoc2 mapno pointdatalist 1)
                      ; xpos (nth 4 pdata)
                      ; ypos (nth 3 pdata)
                      ; zpos (nth 5 pdata)
                      ; scode (nth 2 pdata)
                      ; ptype (substr (cdr point) 1 1))
                ; (DrawPoint mapno xpos ypos zpos scode gl_MAP_SCALE ptype)
                ; );foreach
            ; (foreach line pretoDWG-line
                ; (setq p1 (car line)
                      ; p2 (cadr line)
                      ; tableline (caddr line)
                      ; tablep (strcat (substr tableline 1 1) "P"))
                ; (DrawDBLine  p1 p2 tableline tablep ConnectionObject)
                ; );foreach
            ; (ADOLISP_DisconnectFromDB ConnectionObject)
            ; (setq ConnectionObject nil)

            ; ;;Reprort
            ; (prompt "\nͬ����ɡ�")
            ; (prompt (strcat "\n" (rtos (length pretoDWG-point) 2 0) "�����ߵ�����ݿ���Ƶ�ͼ�Σ�\n"))
            ; (vl-prin1-to-string pretoDWG-point)
            ; (prompt (strcat "\n" (rtos (length pretoDWG-line) 2 0) "�����߶δ����ݿ���Ƶ�ͼ�Σ�\n"))
            ; (vl-prin1-to-string pretoDWG-line)
            ; (prompt (strcat "\n" (rtos (length pretoDB) 2 0) "�����ߵ���߶α�д�����ݿ⣡\n"))
            ; ;;
            ; ) ;progn
        ; ) ;if
    ; ) ;defun
; ;;
;*********************************************************************************************
;��������:C:outhz()
;���ܣ���������ݳɹ���,������̽Ժ��ʽ.
;������
;���أ�
;����ʱ�䣺2014/12/16   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;V5.2 �� ����ʹ��
;*********************************************************************************************
(defun C:outhz( /  alllinelist alllineset cab_count cabstr depth depth2 dlist e edge edge1 edge2 endp
			   ent entl entlist filep filepath flist flowdirect glist handle headstr hlist hole_count
			   hole_used i jlist ldata linelist linkedges linkp llist lnum nextline nline outstr pent1
			   pent2 plist pos1 pos2 pressure pstart qlist rlist singlelist stackin stackout startp
			   subsidstr tname typelist typename typestr voltage wlist xlist ylist zh zlist DimZin1 p10 
			   p11 deltH podu exchangePoint)
	(princ "\n����ɹ���,��ʽΪ�㽭ʡ������̽����Ժ��ʽ.")
	(if (setq enter (Fun_InPutString "Y" "USERS1" "\n����ɹ���֮ǰ������д�����.�Ƿ����ڽ��д�����?(Y/N)"))
		(if (= "Y" (strcase enter)) (UpdateTopo))
	)
	
	(if (= nil gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	
	(setq AllLineSet (ssget "X" (list (cons 0 "LINE"))))
	(setq Jlist nil Plist nil Wlist nil Ylist nil Qlist nil Llist nil Dlist nil Xlist nil
		Hlist nil Rlist nil Glist nil Flist nil Zlist nil
		i 0
		lnum 0
	)
	(if AllLineSet
		(repeat (sslength AllLineSet)
			(setq ent (ssname AllLineSet i))
			(if (vlax-ldata-get ent gl_AppName)
				(progn
					(setq tname  (ldata-get ent "Main_Type")
						entlist (entget ent)
						startp (ldata-get ent "Start_Point")
						endp   (ldata-get ent "End_Point")
						pos1 (cdr (assoc 10 entlist))
						pos2 (cdr (assoc 11 entlist))
						handle (cdr (assoc 5 entlist));���
						lnum (1+ lnum))
					
					(cond 
						((= tname "J") (setq Jlist (cons handle Jlist)))
						((= tname "P") (setq Plist (cons handle Plist)))
						((= tname "W") (setq Wlist (cons handle Wlist)))
						((= tname "Y") (setq Ylist (cons handle Ylist)))
						((= tname "Q") (setq Qlist (cons handle Qlist)))
						((= tname "L") (setq Llist (cons handle Llist)))
						((= tname "D") (setq Dlist (cons handle Dlist)))
						((= tname "X") (setq Xlist (cons handle Xlist)))
						((= tname "H") (setq Hlist (cons handle Hlist)))
						((= tname "R") (setq Rlist (cons handle Rlist)))
						((= tname "G") (setq Glist (cons handle Glist)))
						((= tname "F") (setq Flist (cons handle Flist)))
						((= tname "Z") (setq Zlist (cons handle Zlist)))
					)
				)
			)
			(setq i (1+ i))
		)
	)
	(setq AllLineList (list (cons "J" (list Jlist))
							(cons "P" (list Plist))
							(cons "W" (list Wlist))
							(cons "Y" (list Ylist))
							(cons "Q" (list Qlist))
							(cons "L" (list Llist))
							(cons "D" (list Dlist))
							(cons "X" (list Xlist))
							(cons "H" (list Hlist))
							(cons "R" (list Rlist))
							(cons "G" (list Glist))
							(cons "F" (list Flist))
							(cons "Z" (list Zlist))
		)
	)				   
	;;����ɹ���-����-�㽭ʡ������̽����Ժ�ɹ���
	(setq filePath   "c:\\line_info_achive_HZ.csv"
		headstr "ͼ�ϵ��,���ӵ��,����,����,������,X_m,Y_m,����߳�m,�ܶ�(����)�߳�m,�ܾ�mm,ѹ��/��ѹ,����m,��������,���ÿ���/�ܿ���,Ȩ����λ����,���跽ʽ,�������,����,����,��ע,����,�¶�,����,�����"
	)
	;;�򿪵��ļ�
     (vl-file-delete filePath)
	 (setq DimZin1 (getvar "DIMZIN"))
	 (setvar "DIMZIN" 0)
     (if (not (setq filep (open filePath "w")))
         (progn
             (Prompt (strcat "\n���ļ�" filePath "ʧ�ܡ��˳���"))
             (exit)
         ) ;_ End_progn
     ) ;if
	(foreach e AllLineList
		(setq linelist (cadr e)
			nline (length linelist)
			singlelist nil ;the first edges
			i 0)
		(setq typelist (assoc (car e) gl_TableColorList)
			typestr (strcat (car typelist) "-" (caddr typelist))
		)
		(if (> nline 0)
			(progn
				(write-line typestr filep)
				(write-line headstr filep)
				;;find all of end points 
				(while (< i nline)
					(setq entl (handent (nth i linelist)))
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
						typename (ldata-get entl "Main_Type")
						stackout nil
					)
					
					(if (= 1 (length edge1));;start point and link point
						(setq startp pent1
							linkp pent2) 
						(setq startp pent2 
							linkp pent1)
					)
					(setq stackin (cons (list startp linkp entl) stackin))
					;;������� 
					(while (> (length stackin) 0)
						(setq entl (caddr (car stackin))
							  startp (car (car stackin))
							  linkp (cadr (car stackin))
							  stackin (cdr stackin)
							  p10(cdr (assoc 10 (entget entl)))
							  p11 (cdr (assoc 11 (entget entl)))
							  deltH (- (last p10) (last p11))
							  
							  p10 (list (car p10) (cadr p10)) 
							  p11 (list (car p11) (cadr p11))
						      	  length_l (distance p10 p11)
							  ;;podu (/ deltH length_l)
						)
						(if (> length_l 0.0001) 
							(setq podu (/ deltH length_l))
							(setq podu 0)
							)
					  	(setq pstart (cdr (assoc 10 (entget startp))))
						(if (> (distance p10 pstart) 0.001)
							(setq depth (ldata-get entl "End_Deep")
								depth2 (ldata-get entl "Start_Deep")
								zh (- (caddr pstart) depth)
							)	;���߸߳�
							(setq depth (ldata-get entl "Start_Deep")
								depth2 (ldata-get entl "End_Deep")
								zh (- (caddr pstart) depth)
							)	;���߸߳�
						)
						
							
					  
						(setq fd(ldata-get entl "Flowdirect"))
						(cond
							((= fd 0) (setq podu (abs podu)))
							((= fd 2) (setq podu (* -1.0 podu)))
							( T (setq podu (abs podu)))
						)
						(cond
							((> podu 0) (setq podu (strcat "'1/" (rtos (/ 1 podu) 2 0))))
							((= podu 0) (setq podu "'0"))
							((< podu 0) (setq podu (strcat "'-1/" (rtos (/ 1 (abs podu)) 2 0))))
						)

						;;output startp linkp and line.
						(if (or (= typename "D") (= typename "L") (= typename "X") )
							(progn
								(setq cab_count (ldata-get entl "Cab_Count")
									Hole_count (ldata-get entl "Hole_Count")
									Hole_Used (ldata-get entl "Hole_Used")
									cabstr (strcat cab_count "," Hole_Used "/" Hole_count))
							)
							(setq cabstr " , ")
						)
						(if (or (= typename "G") (= typename "P") (= typename "Y") (= typename "W") (= typename "R"))
							(setq flowdirect (rtos (ldata-get entl "Flowdirect")))
							(setq flowdirect "")
						)
						(if (= nil (setq subsidstr (ldata-get startp "Subsid")));;������ 2.0�� subsid Ϊnil 
							(setq subsidstr "")
							)
						
						(setq outstr (ldata-get startp "Map_No")
							outstr (strcat outstr "," (ldata-get linkp "Map_No"))
							outstr (strcat outstr "," (ldata-get entl "Material"))
							outstr (strcat outstr "," (ldata-get startp "Feature"))
							outstr (strcat outstr "," subsidstr)
							outstr (strcat outstr "," (rtos (cadr pstart) 2 4) "," (rtos (car pstart) 2 4) ","(rtos (caddr pstart) 2 4))
							outstr (strcat outstr "," (rtos zh 2 4))
							outstr (strcat outstr "," (ldata-get entl "D_S"))
							voltage (ldata-get entl "Voltage")
							pressure (ldata-get entl "Pressure")
							outstr (strcat outstr "," voltage pressure)	;����֮һΪ���ַ���
							outstr (strcat outstr ",'" (rtos  depth 2 2) "/" (rtos depth2 2 2))
							outstr (strcat outstr "," cabstr)
							
							outstr (strcat outstr "," " , , ," flowdirect ", , ")
							outstr (strcat outstr "," (rtos length_l 2 3))
							
							outstr (strcat outstr "," podu)
							outstr (strcat outstr ",'" (ldata-get startp "Well_Deep") "/" (ldata-get linkp "Well_Deep"))
							outstr (strcat outstr "," (ldata-get startp "Point_Size"))
						)
						(write-line outstr filep)
						;;linkp is a end point
						(if (= nil (setq subsidstr (ldata-get linkp "Subsid")));;������ 2.0�� subsid Ϊnil 
							(setq subsidstr "")
						)
						(setq pstart (cdr (assoc 10 (entget linkp)))
							;depth (ldata-get linkp "Depth")
							zh (- (caddr pstart) depth2))	;���߸߳�
						(if (= 1 (length (ldata-get linkp "Edge")))
							(progn
								(setq outstr (ldata-get linkp "Map_No")
									  outstr (strcat outstr ", , ," (ldata-get linkp "Feature"))
									  outstr (strcat outstr "," subsidstr)
									  outstr (strcat outstr "," (rtos (cadr pstart) 2 4) "," (rtos (car pstart) 2 4) ","(rtos (caddr pstart) 2 4))
									  outstr (strcat outstr "," (rtos zh 2 4))
									  outstr (strcat outstr ", , ," (rtos  depth2 2 2))
									  outstr (strcat outstr ", , , , , , , , , , ,'" (ldata-get linkp "Well_Deep") "/" (ldata-get linkp "Well_Deep"))
									  outstr (strcat outstr "," (ldata-get linkp "Point_Size"))
								)
								(write-line outstr filep)
							)
						)
						;;
						(setq stackout (cons entl stackout)
							singlelist (vl-remove entl singlelist) 
						)
						;;
						(setq linkedges (ldata-get linkp "Edge"))
						(foreach e linkedges
							(setq nextline (handent (last e))
								  pent1 (handent (car e))
								  pent2 (handent (cadr e))
								  )
							(if (not (equal nextline entl))
								(if  (vl-position nextline stackout);�Ƿ��պϹ���
									(prompt (strcat "\n����:���ֹ��߱պ�!	�������ߵ�:" (ldata-get nextline "Start_Point") "\t" (ldata-get nextline "End_Point")))
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
	(setvar "DIMZIN" DimZin1)
	(close filep)
	(startapp "EXPLORER.EXE" filePath)
    (prin1)
)
;*********************************************************************************************
;��������:C:outCGB_xls()
;���ܣ�������ɹ���ģ��,����Ԫ��ʽһ��.
;��������λ���ƣ�����ʱ�䣬Ŀ¼��-ҳ�� ��ҳ��
;������
;���أ�
;����ʱ�䣺2015/10/25   9:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:outCGB_xls( / *xls* activesheet alllinelist alllineset cab_count cabstr cgbfile currownum 
			delth depth depth2 dimzin1 dlist e edge edge1 edge2 endp ent enter entl entlist fd flist 
			flowdirect glist handle hlist hole_count hole_used holestr i jlist length_l linelist 
			linkedges linkp llist lnum nextline nline outdata p10 p11 path0 pent1 pent2 plist podu 
			pos1 pos2 pstart qlist rlist sheets singlelist stackin stackout startp subsidstr templatefile 
			tname typel welldeepstr welldeepstr2 welldeepstr1)
	(princ "\n������߳ɹ���.")
	;;����ɹ���-����-�㽭ʡ������̽����Ժ�ɹ���
	;;��ģ�����ɳɹ���
	(LineInfo_GetSupportPath)
	(setq templateFile (strcat gl_INFO_LINE_PATH "database\\���߳ɹ���.xls"))
	(setq path0 (vla-get-FullName (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (substr  path0 1 (- (strlen path0) 4))
		CGBfile (strcat path0 ".xls")
	)
	(setq i 1)
	(while (findfile CGBfile)
		(setq CGBfile (strcat path0 (rtos i 2 0) ".xls")
			i (1+ i)
		)
	)
	;;copy templateFile
	;(if (not (vl-file-copy templateFile CGBfile))
	;	(*error* (strcat �������ļ��� CGBfile "ʧ�ܡ�"))
	;)
	
	;;����TOPO��ϵ
	(if (setq enter (Fun_InPutString "Y" "USERS1" "\n����ɹ���֮ǰ������д�����.�Ƿ����ڽ��д�����?(Y/N)"))
		(if (= "Y" (strcase enter)) (UpdateTopo))
	)
	
	(if (= nil gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	
	;;���߷���
	(setq AllLineSet (ssget "X" (list (cons 0 "LINE"))))
	(setq Jlist nil Plist nil Wlist nil Ylist nil Qlist nil Llist nil Dlist nil Xlist nil
		Hlist nil Rlist nil Glist nil Flist nil Zlist nil
		i 0
		lnum 0
	)
	(if AllLineSet
		(repeat (sslength AllLineSet)
			(setq ent (ssname AllLineSet i))
			(if (vlax-ldata-get ent gl_AppName)
				(progn
					(setq tname  (ldata-get ent "Main_Type")
						entlist (entget ent)
						startp (ldata-get ent "Start_Point")
						endp   (ldata-get ent "End_Point")
						pos1 (cdr (assoc 10 entlist))
						pos2 (cdr (assoc 11 entlist))
						;handle (cdr (assoc 5 entlist));���
						lnum (1+ lnum))
					
					(cond 
						((= tname "J") (setq Jlist (cons ent Jlist)))
						((= tname "P") (setq Plist (cons ent Plist)))
						((= tname "W") (setq Wlist (cons ent Wlist)))
						((= tname "Y") (setq Ylist (cons ent Ylist)))
						((= tname "Q") (setq Qlist (cons ent Qlist)))
						((= tname "L") (setq Llist (cons ent Llist)))
						((= tname "D") (setq Dlist (cons ent Dlist)))
						((= tname "X") (setq Xlist (cons ent Xlist)))
						((= tname "H") (setq Hlist (cons ent Hlist)))
						((= tname "R") (setq Rlist (cons ent Rlist)))
						((= tname "G") (setq Glist (cons ent Glist)))
						((= tname "F") (setq Flist (cons ent Flist)))
						((= tname "Z") (setq Zlist (cons ent Zlist)))
					)
				)
			)
			(setq i (1+ i))
		)
	)
	(setq AllLineList (list (cons "J" (list Jlist))
							(cons "P" (list Plist))
							(cons "W" (list Wlist))
							(cons "Y" (list Ylist))
							(cons "Q" (list Qlist))
							(cons "L" (list Llist))
							(cons "D" (list Dlist))
							(cons "X" (list Xlist))
							(cons "H" (list Hlist))
							(cons "R" (list Rlist))
							(cons "G" (list Glist))
							(cons "F" (list Flist))
							(cons "Z" (list Zlist))
		)
	)				   
	
	;;�����xls templateFile
	
	 (setq DimZin1 (getvar "DIMZIN"))
	 (setvar "DIMZIN" 0)
    (setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;��ʼ�� excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open templateFile nil))
		(progn
			(setq sheets (vlxls-sheet-get-all *xls*)
				;;usedrange  (vlxls-sheet-get-UsedRange *xls*)
			) 
		)
		(*error* (strcat "���ļ�" templateFile "ʧ�ܡ�"))
	)
	(foreach e AllLineList
		(setq linelist (cadr e)
			tname (car e)
			nline (length linelist)
			singlelist nil ;the first edges
			i 0
			curRowNum 3)	;;��ǰ�к�
		(setq typelist (assoc tname gl_TableColorList)
			typestr (strcat (car typelist) "-" (caddr typelist))
		)
		;;find the sheet with the same typename
		(setq activeSheet nil)	;;active sheet
		(foreach e sheets
			(if (vl-string-position (ascii tname) e)
				(setq activeSheet e)
			)
		)
		(if (not activeSheet)
			(*error* (strcat "�����ɹ������ģ���ļ�����ȱ��" tname "."))
			(vlxls-sheet-put-active *xls* activeSheet)
		)
		;;delete not exiting sheets
		(if (<= nline 0)
			;(vlxls-sheet-delete *xls* activeSheet)
		  	(msxl-put-visible (msxl-get-ActiveSheet *xls*) :vlax-false)
		)
		(if (> nline 0)
			(progn
				;;find all of end points 
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
				(while (> (length linelist) 0)
					;;initialize startp and linkp and stackin
					;;��������˵�
					(if (> (length singlelist) 0)
						(setq entl (car singlelist)
							singlelist (cdr singlelist)
							linelist (vl-remove entl linelist)
						)
						(setq entl (car linelist)
							linelist (cdr linelist)
						)
					)
					
					(setq edge (ldata-get entl "Edge")
						pent1 (handent(car edge))
						pent2 (handent(cadr edge))
						edge1 (ldata-get pent1 "Edge")
						typename (ldata-get entl "Main_Type")
						stackout nil
					)
					
					(if (= 1 (length edge1));;start point and link point
						(setq startp pent1
							linkp pent2) 
						(setq startp pent2 
							linkp pent1)
					)
					(setq stackin (cons (list startp linkp entl) stackin))
					;;������� 
					(while (> (length stackin) 0)
						(setq entl (caddr (car stackin))
							  startp (car (car stackin))
							  linkp (cadr (car stackin))
							  stackin (cdr stackin)
							  p10(cdr (assoc 10 (entget entl)))
							  p11 (cdr (assoc 11 (entget entl)))
							  deltH (- (last p10) (last p11))
							  
							  p10 (list (car p10) (cadr p10)) 
							  p11 (list (car p11) (cadr p11))
						      	  length_l (distance p10 p11)
							  ;;podu (/ deltH length_l)
						)
						(if (> length_l 0.0001) 
							(setq podu (/ deltH length_l))
							(setq podu 0)
							)
					  	(setq pstart (cdr (assoc 10 (entget startp)))
							fd(ldata-get entl "Flowdirect")
						)
						;;pstart �� �߶ε�����Ƿ�һ��
						(if (> (distance p10 pstart) 0.001)
							(progn
								(setq depth (ldata-get entl "End_Deep")
										depth2 (ldata-get entl "Start_Deep")
										zh (- (caddr pstart) depth)
								)	;���߸߳�
								(if (= fd 1) 
									(setq fd 2)
									(if (= fd 2) (setq fd 1))
								)
							)
							(progn
								(setq depth (ldata-get entl "Start_Deep")
										depth2 (ldata-get entl "End_Deep")
										zh (- (caddr pstart) depth)
									)	;���߸߳�
							)
						)
						(if (or (= typename "G") (= typename "P") (= typename "Y") (= typename "W") (= typename "R"))
							(setq flowdirect (rtos fd 2 0))
							(setq flowdirect "")
						)					  
						
						(cond
							((= fd 0) (setq podu (abs podu)))
							((= fd 2) (setq podu (* -1.0 podu)))
							( T (setq podu (abs podu)))
						)
						(cond
							((> podu 0) (setq podu (strcat "'1/" (rtos (/ 1 podu) 2 0))))
							((= podu 0) (setq podu "'0"))
							((< podu 0) (setq podu (strcat "'-1/" (rtos (/ 1 (abs podu)) 2 0))))
						)

						;;output startp linkp and line.
						(if (or (= typename "D") (= typename "L") (= typename "X") )
							(progn
								(setq cab_count (ldata-get entl "Cab_Count")
									Hole_count (ldata-get entl "Hole_Count")
									Hole_Used (ldata-get entl "Hole_Used")
									cabstr cab_count
									holeStr (strcat Hole_Used "/" Hole_count)
									)
							)
							(setq cabstr ""
								holeStr "")
						)
						
						(if (= nil (setq subsidstr (ldata-get startp "Subsid")));;������ 2.0�� subsid Ϊnil 
							(setq subsidstr "")
						)
						
						;;���� ����5.0��ǰ�汾������"Well_Deep"
						(setq welldeepstr1 (ldata-get startp "Well_Deep") 
							welldeepstr2 (ldata-get linkp "Well_Deep"))
						(if (= nil welldeepstr1) (setq welldeepstr1 ""))
						(if (= nil welldeepstr2) (setq welldeepstr2 ""))
						(setq welldeepstr (strcat welldeepstr1  "/" welldeepstr2))
						;;�������һ��
						(setq outdata (list (ldata-get startp "Map_No")
							(ldata-get linkp "Map_No")
							(ldata-get entl "Material")
							(ldata-get startp "Feature")
							subsidstr
							(rtos (cadr pstart) 2 3) (rtos (car pstart) 2 3) (rtos (caddr pstart) 2 3) (rtos  zh 2 3)
							(ldata-get entl "D_S")
							(strcat (ldata-get entl "Voltage") (ldata-get entl "Pressure"))
							(strcat (rtos  depth 2 2) "/" (rtos depth2 2 2))	;;������/�յ����
							cabstr	
							holeStr ;;���ÿ���/�ܿ���
							nil	;;Ȩ����λ����
							(ldata-get entl "BuryWay")
							nil	;;������
							flowdirect
							nil 	;;����
							(ldata-get entl "Remark")
							nil 	;;�����Զ�������
							(rtos length_l 2 2)	;;����
							podu		;;�¶�
							(ldata-get startp "Point_Size")	;;�����
							welldeepstr ;;����
							))
						(vlxls-put-row-value *xls* (list 1 curRowNum) outdata)
						(setq curRowNum (1+ curRowNum))
						
						
						(if (= nil (setq subsidstr (ldata-get linkp "Subsid")));;������ 2.0�� subsid Ϊnil 
							(setq subsidstr "")
						)
						(setq pstart (cdr (assoc 10 (entget linkp)))
							;;depth (ldata-get linkp "Depth")
							zh (- (caddr pstart) depth2))	;���߸߳�
						
						;;linkp is a end point
						(if (= 1 (length (ldata-get linkp "Edge")))
							(progn
								(setq outdata (list (ldata-get linkp "Map_No") nil nil 
									(ldata-get linkp "Feature")
									subsidstr
									(cadr pstart) (car pstart) (caddr pstart) (rtos  zh 2 3) nil nil (rtos depth2 2 2)
									nil nil nil nil nil nil nil nil nil nil nil
									(ldata-get linkp "Point_Size")
									(ldata-get linkp "Well_Deep")
								))
								(vlxls-put-row-value *xls* (list 1 curRowNum) outdata)
								(setq curRowNum (1+ curRowNum))
							)
						)
						;;�����
						(setq stackout (cons entl stackout))
						(if (> (length singlelist) 0)
							(setq singlelist (vl-remove entl singlelist))
						)
						(setq linelist (vl-remove entl linelist))
						;;
						(setq linkedges (ldata-get linkp "Edge"))	;;���������ܰ����������Ĺ��߶�
						(foreach e linkedges
							(setq nextline (handent (last e))
								  pent1 (handent (car e))
								  pent2 (handent (cadr e))
								  linkTypeName (ldata-get nextline "Main_Type")
								  )
							(if (and  (not (equal nextline entl)) (= typename linkTypeName))
								(if  (vl-position nextline stackout);�Ƿ��պϹ���
									(prompt (strcat "\n����:���ֹ��߱պ�!	�������ߵ�:" (ldata-get nextline "Start_Point") "\t" (ldata-get nextline "End_Point")))
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
	(setvar "DIMZIN" DimZin1)
	(vlxls-app-saveas *xls* CGBfile)
	;(vlxls-app-quit *xls* "Y")
	(vlxls-app-quit *xls* nil)
	
	(startapp "EXPLORER.EXE" CGBfile)
    (prin1)
)
;*********************************************************************************************
;��������:C:ImportFromCGB(XLSFile)
;���ܣ��ӳɹ����е���������ݣ� ��̽�����������һ�¡�
;������XLSFile��excel xls�ļ�
;���أ�
;����ʱ�䣺2015/03/06  21��00
;�޸�ʱ�䣺
;�����ˣ����۾�
;V5.2 �� ����ʹ��
;*********************************************************************************************
(defun C:ImportFromCGB( / bf bs cellid emptyrowcount entlist feature featurelist featurestr 
 hent holecount holestr holeused linename1 linename2 lineslist linkent linkpoint linkpointlist
 maintype mapno mapno2 ndata outlist p1 p2 path0 pent pent1 pent2 pointslist pos pressure
 rangeid1 rangeid2 row rowdata sheet sheets subsidlist subsidstr usedrange voltage xlsfile)
 
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n�������̽��ɹ���,�ļ���ʽΪxls.
				\n�Զ�����ͼ��(���ݵ��ǰ׺����ͼ��).")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "ѡ��ɹ���" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;��ʼ�� excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open XLSFile nil))
		(progn
			(setq sheets (vlxls-sheet-get-all *xls*)
				;;usedrange  (vlxls-sheet-get-UsedRange *xls*)
			)
			(foreach sheet sheets
				(setq usedrange  (vlxls-range-getid (vlxls-sheet-get-UsedRange *xls* sheet))
					cellid (vlxls-cellid usedrange)
					rangeid1  (vlxls-rangeid (car cellid))
					rangeid2  (vlxls-rangeid (cadr cellid))
					row 3	;;�к�
					pointslist nil	;;���е���ĵ�
					linkpointlist nil
					emptyrowcount 0	;;�������е�����
					lineslist nil	;;��ʱδ������߶�
					maintype (GetTypeFromPointName sheet)	;;�������ӱ��������������
					featurelist ( getfeaturelist-fromtype maintype)
					subsidlist (getsubsidlist-fromtype maintype)
					gl_BlockNameList nil
				)
				
				(if (vlxls-sheet-put-active *xls* sheet)
					(progn
						(while (and (< emptyrowcount 10) (<= row  (cadr rangeid2)))	;;row in the range and ��������С��10��
							;;��ȡһ��
							(setq rowdata (vlxls-cell-get-value *xls*  (strcat "A"  (rtos row 2 0) ":" "R"  (rtos row 2 0) ))
								rowdata (car rowdata)
								ndata (length rowdata)
								mapno (car rowdata)
								linkpoint (cadr rowdata)
							)
							(if (<= (strlen mapno) 0)	
								(setq emptyrowcount (1+ emptyrowcount))	;;����
								(progn
									;;�����б��У������µ�
									(if (null (assoc mapno pointslist))
										(progn	;;�ǿ���
											;;��������
											(setq p1 (list (atof (nth 4 rowdata)) (atof (nth 3 rowdata)) (atof (nth 5 rowdata)))
												feature (nth 2 rowdata)
												featurestr "���ղ�"
												subsidstr ""
												emptyrowcount 0 
												bf T bs T
											)
											(if (member feature featurelist)
												(setq subsidstr ""
														featurestr feature)
												(setq bf nil)
											)
											(if (member feature subsidlist)
												(setq featurestr ""
													subsidstr feature)
												(setq bs nil)
											)
											(if (and (= nil bf) (= nil bs))
												(prompt (strcat "\n �����ļ���δ���������͸���������:" feature))
											)
											(setq outlist nil
														outlist (cons (cons "Exp_No" mapno) outlist)
														outlist (cons (cons "Map_No" mapno) outlist)
														outlist (cons (cons "Depth" (atof (nth 6 rowdata ))) outlist)
														outlist (cons (cons "Subsid" subsidstr) outlist)
														outlist (cons (cons "Feature" featurestr) outlist)
														outlist (cons (cons "Main_Type" maintype) outlist)
													)
											(setq pent (AddNewPoint p1 mapno maintype featurestr subsidstr outlist)
												hent (cdr (assoc 5 (entget pent)))
												)
											;;add new point  to pointslist
											(setq pointslist (cons (cons mapno hent)  pointslist))														
																									
											; (if  (and (= 0 (strlen featurestr)) (= 0 (strlen subsidstr)))
												; (progn
													; (prompt (strcat "\n��" mapno "δ�ҵ���Ӧ�������������õ㱻���ԡ�" ))
													; ;;(exit)
												; )
												; (progn
													; ;;������
													
													
												; )
											; )
										)
									)	
									;;������
									(if (> (strlen linkpoint) 0)
										(progn
											(setq linename1 (strcat mapno linkpoint)
												linename2 (strcat linkpoint mapno)
											)
											;;�߶λ�δ����
											(if (not (or (member linename1 lineslist) (member linename2 lineslist)))
												(if (setq linkent  (cdr  (assoc linkpoint pointslist)))	;;���Ѿ��ڵ����
													(progn
														;;add line
														(setq pent1 (cdr (assoc mapno pointslist))
															p1 (cdr (assoc 10 (entget (handent pent1))))
															entlist (entget (handent linkent))
															p2 (cdr (assoc 10 entlist))
														)
														;;����
														(setq holestr (nth 12 rowdata))
														(if (> (strlen holestr) 0)
															(if (setq pos (vl-string-search "/" holestr))
																(setq holecount (substr holestr 1 pos)
																	holeused (substr holestr (+ pos 2)))
															)
														)
														;;ѹ�����ѹ
														(if (or (= "Q" maintype)  (= "R" maintype)  (= "G" maintype))
															(setq pressure (nth 14 rowdata)
																voltage ""
															)
															(setq voltage (nth 14 rowdata)
																pressure ""
															)
														)
														(setq outlist nil
															outlist (cons (cons "Start_Point" mapno) outlist)
															outlist (cons (cons "End_Point" linkpoint) outlist)
															outlist (cons (cons "Material" (nth 10 rowdata)) outlist)
															outlist (cons (cons "D_S" (nth 9 rowdata)) outlist)
															outlist (cons (cons "Cab_Count" (nth 13 rowdata)) outlist)
															outlist (cons (cons "Main_Type" maintype) outlist)
															outlist (cons (cons "Hole_Count" holecount) outlist)
															outlist (cons (cons "Hole_Used" holeused) outlist)
															outlist (cons (cons "Pressure" pressure) outlist)
															outlist (cons (cons "Voltage" voltage) outlist)
															outlist (cons (cons "P_Material" (nth 11 rowdata)) outlist)
															outlist (cons (cons "Property" (nth 16 rowdata)) outlist)
															outlist (cons (cons "Remark" (nth 17 rowdata)) outlist)
															outlist (cons (cons "BuryWay" (nth 15 rowdata)) outlist)
														)
														(AddNewLine p1 p2 maintype outlist)
														(setq lineslist (cons linename1 lineslist)
																lineslist (cons linename2 lineslist)
														)
													)
													(progn
														(if (> (strlen linkpoint) 0) (setq linkpointlist (cons rowdata linkpointlist)))	;;������������
													)
												)
											
											)
										)
									)
									
									
								)
							)
							(setq row (1+ row))
						)
						;;����δ���ߵ����ӵ�
						(if (> (length linkpointlist) 0)
							(foreach rowdata linkpointlist
								(setq mapno (car rowdata)
									mapno2 (cadr rowdata)
									pent1 (assoc mapno pointslist)
									pent2 (assoc mapno2 pointslist)
									linename1 (strcat mapno mapno2)
									linename2 (strcat mapno2 mapno)
								)
								(if (and pent1 pent2 (not (or (member linename1 lineslist) (member linename2 lineslist))))
									(progn
										(setq pent1 (handent (cdr pent1))
											p1 (cdr (assoc 10 (entget pent1)))
											pent2 (handent (cdr pent2))
											p2 (cdr (assoc 10 (entget pent2)))
										)
										;;����
										(setq holestr (nth 12 rowdata))
										(if (> (strlen holestr) 0)
											(if (setq pos (vl-string-search "/" holestr))
												(setq holecount (substr holestr 1 pos)
													holeused (substr holestr (+ pos 1)))
											)
										)
										;;ѹ�����ѹ
										(if (or (= "Q" maintype)  (= "Q" maintype)  (= "Q" maintype))
											(setq pressure (nth 14 rowdata)
												voltage ""
											)
											(setq voltage (nth 14 rowdata)
												pressure ""
											)
										)
										(setq outlist nil
											outlist (cons (cons "Start_Point" mapno) outlist)
											outlist (cons (cons "End_Point" mapno2) outlist)
											outlist (cons (cons "Material" (nth 10 rowdata)) outlist)
											outlist (cons (cons "D_S" (nth 9 rowdata)) outlist)
											outlist (cons (cons "Cab_Count" (nth 13 rowdata)) outlist)
											outlist (cons (cons "Main_Type" maintype) outlist)
											outlist (cons (cons "Hole_Count" holecount) outlist)
											outlist (cons (cons "Hole_Used" holeused) outlist)
											outlist (cons (cons "Pressure" pressure) outlist)
											outlist (cons (cons "Voltage" voltage) outlist)
											outlist (cons (cons "P_Material" (nth 11 rowdata)) outlist)
											outlist (cons (cons "Property" (nth 16 rowdata)) outlist)
											outlist (cons (cons "Remark" (nth 17 rowdata)) outlist)
											outlist (cons (cons "BuryWay" (nth 15 rowdata)) outlist)
											outlist (cons (cons gl_AppName gl_Version) outlist)
										)
										(AddNewLine p1 p2 maintype outlist)
									)
								)
							)
						)
					)
				)
			)
			 (vlxls-app-quit *xls* nil)
		)
		(progn
			(prompt (strcat "\n���ļ�" XLSFile "ʧ�ܣ�"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGB��ʱ")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "��.")
		)
	)
	(princ)
)

;*********************************************************************************************
;��������:C:ImportFromCGBZY2(XLSFile)
;���ܣ��ӳɹ����е���������ݣ� ��̽�����������һ�¡�
;������XLSFile��excel xls�ļ�
;���أ�
;����ʱ�䣺2015/03/06  21��00
;�޸�ʱ�䣺����
;�����ˣ����۾�
;*********************************************************************************************
(defun C:ImportFromCGBZY2( / bf bs cellid emptyrowcount entlist feature featurelist featurestr 
 hent holecount holestr holeused linename1 linename2 lineslist linkent linkpoint linkpointlist
 maintype mapno mapno2 ndata outlist p1 p2 path0 pent pent1 pent2 pointslist pos pressure
 rangeid1 rangeid2 row rowdata sheet sheets subsidlist subsidstr usedrange voltage xlsfile d1 d2 
 depthstr newl cabcount flowdirect)
 
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n�������̽��ɹ���,�ļ���ʽΪxls.
				\n�Զ�����ͼ��(���ݵ��ǰ׺����ͼ��).")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "ѡ��ɹ���" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;��ʼ�� excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open XLSFile nil))
		(progn
			(setq sheets (vlxls-sheet-get-all *xls*)
				;;usedrange  (vlxls-sheet-get-UsedRange *xls*)
				pointslist nil	;;���е���ĵ�
			)
			
			(foreach sheet sheets
				(setq usedrange  (vlxls-range-getid (vlxls-sheet-get-UsedRange *xls* sheet))
					cellid (vlxls-cellid usedrange)
					rangeid1  (vlxls-rangeid (car cellid))
					rangeid2  (vlxls-rangeid (cadr cellid))
					row 3	;;�к�
					linkpointlist nil
					emptyrowcount 0	;;�������е�����
					lineslist nil	;;��ʱδ������߶�
					maintype (GetTypeFromPointName sheet)	;;�������ӱ��������������
					
					featurelist ( getfeaturelist-fromtype maintype)
					subsidlist (getsubsidlist-fromtype maintype)
					gl_BlockNameList nil
					hasdata T
				)
				(if (= maintype "B");����
					(if (and (/= sheet "B") (/= sheet "����"))
						(setq hasdata nil)
					)
				)
				
				(if (and hasdata (vlxls-sheet-put-active *xls* sheet))
					(progn
						(while (and (< emptyrowcount 10) (<= row  (cadr rangeid2)))	;;row in the range and ��������С��10��
							;;��ȡһ��
							(setq rowdata (vlxls-cell-get-value *xls*  (strcat "A"  (rtos row 2 0) ":T"  (rtos row 2 0) ))
								rowdata (car rowdata)
								ndata (length rowdata)
								mapno (car rowdata)
								linkpoint (cadr rowdata)
							)
							(if (<= (strlen mapno) 0)	
								(setq emptyrowcount (1+ emptyrowcount))	;;����
								(progn
									;;�����б��У������µ�
									(if (null (assoc mapno pointslist))
										(progn	;;�ǿ���
											;;��������
											(setq p1 (list (atof (nth 6 rowdata)) (atof (nth 5 rowdata)) (atof (nth 7 rowdata)))
												feature (nth 3 rowdata)
												subsid	(nth 4 rowdata)
												featurestr "���ղ�"
												subsidstr ""
												emptyrowcount 0 
												bf nil bs nil
												depth (- (atof (nth 7 rowdata)) (atof (nth 8 rowdata)))
											)
											(if (member feature featurelist)
												(setq featurestr feature
													bf T)
												(setq bf nil)
											)
											(if (member subsid subsidlist)
												(setq subsidstr subsid
													bs T)
												(setq bs nil)
											)
											(if (and (= nil bf) (= nil bs))
												(prompt (strcat "\n �����ļ���δ���������͸���������:" feature))
											)
											(setq outlist nil
												outlist (cons (cons "Exp_No" mapno) outlist)
												outlist (cons (cons "Map_No" mapno) outlist)
												outlist (cons (cons "Depth" depth) outlist)
												outlist (cons (cons "Subsid" subsidstr) outlist)
												outlist (cons (cons "Feature" featurestr) outlist)
												outlist (cons (cons "Main_Type" maintype) outlist)
											)
											(setq pent (AddNewPoint p1 mapno maintype featurestr subsidstr outlist)
												hent (cdr (assoc 5 (entget pent)))
												)
											;;add new point  to pointslist
											(setq pointslist (cons (cons mapno hent)  pointslist))														
																									
										)
									)	
									;;������
									(if (> (strlen linkpoint) 0)
										(progn
											(setq linename1 (strcat mapno linkpoint)
												linename2 (strcat linkpoint mapno)
											)
											;;�߶λ�δ����
											(if (not (or (member linename1 lineslist) (member linename2 lineslist)))
												(if (setq linkent  (cdr  (assoc linkpoint pointslist)))	;;���Ѿ��ڵ����
													(progn
														;;add line
														(setq pent1 (cdr (assoc mapno pointslist))
															p1 (cdr (assoc 10 (entget (handent pent1))))
															entlist (entget (handent linkent))
															p2 (cdr (assoc 10 entlist))
														)
														;;���
														(setq depthstr (nth 11 rowdata)
															d1 0 d2 0)
														(if (> (strlen depthstr) 2)
															(if (setq pos (vl-string-search "/" depthstr))
																(progn
																	(setq d1 (atof (substr depthstr 1 pos))
																		d2 (atof (substr depthstr (+ pos 2)))
																		p1 (list (car p1) (cadr p1) (- (caddr p1) d1))
																		p2 (list (car p2) (cadr p2) (- (caddr p2) d2))
																	)
																)
															)
														)
														;;����
														(setq holestr (nth 13 rowdata)
															holecount ""
															holeused "")
														(if (> (strlen holestr) 0)
															(if (setq pos (vl-string-search "/" holestr))
																(setq holecount (substr holestr 1 pos)
																	holeused (substr holestr (+ pos 2)))
															)
														)
														;;ѹ�����ѹ
														(if (or (= "Q" maintype)  (= "R" maintype)  (= "G" maintype))
															(setq pressure (nth 10 rowdata)
																voltage ""
															)
															(setq voltage (nth 10 rowdata)
																pressure ""
															)
														)
														;;cab_count
														(if (or (= "X" maintype) (= "L" maintype) (= "D" maintype) (= "H" maintype))
															(setq cabcount (nth 12 rowdata))
															(setq cabcount "")
														)
														;;Flowdirect
														(if (or (= "P" maintype) (= "W" maintype) (= "Y" maintype) (= "G" maintype))
															(setq flowdirect (atoi(nth 17 rowdata)))
															(setq flowdirect 0)
														)
														(setq outlist nil
															outlist (cons (cons "Start_Point" mapno) outlist)
															;outlist (cons (cons "Start_Deep" d1) outlist)
															outlist (cons (cons "End_Point" linkpoint) outlist)
															;outlist (cons (cons "End_Deep" d2) outlist)
															
															outlist (cons (cons "Material" (nth 2 rowdata)) outlist)
															outlist (cons (cons "D_S" (nth 9 rowdata)) outlist)
															outlist (cons (cons "Cab_Count" cabcount) outlist)
															outlist (cons (cons "Main_Type" maintype) outlist)
															outlist (cons (cons "Hole_Count" holecount) outlist)
															outlist (cons (cons "Hole_Used" holeused) outlist)
															outlist (cons (cons "Pressure" pressure) outlist)
															outlist (cons (cons "Voltage" voltage) outlist)
															outlist (cons (cons "Flowdirect" flowdirect) outlist)
															;outlist (cons (cons "P_Material" (nth 11 rowdata)) outlist)
															outlist (cons (cons "Property" (nth 14 rowdata)) outlist)
															outlist (cons (cons "Remark" (nth 19 rowdata)) outlist)
															outlist (cons (cons "BuryWay" (nth 15 rowdata)) outlist)
															;;nth 16  18 ������� ������ δ����
														)
														
														
														(if (setq newl (AddNewLine p1 p2 maintype outlist))
															(progn
																(ldata-put newl "Start_Deep" d1)
																(ldata-put newl "End_Deep" d2)
															)
														)
														(setq lineslist (cons linename1 lineslist)
																lineslist (cons linename2 lineslist)
														)
													)
													(progn
														(if (> (strlen linkpoint) 0) (setq linkpointlist (cons rowdata linkpointlist)))	;;������������
													)
												)
											)
										)
									)
								)
							)
							(setq row (1+ row))
						)
						;;����δ���ߵ����ӵ�
						(if (> (length linkpointlist) 0)
							(foreach rowdata linkpointlist
								(setq mapno (car rowdata)
									mapno2 (cadr rowdata)
									pent1 (assoc mapno pointslist)
									pent2 (assoc mapno2 pointslist)
									linename1 (strcat mapno mapno2)
									linename2 (strcat mapno2 mapno)
								)
								(if (and pent1 pent2 (not (or (member linename1 lineslist) (member linename2 lineslist))))
									(progn
										(setq pent1 (handent (cdr pent1))
											p1 (cdr (assoc 10 (entget pent1)))
											pent2 (handent (cdr pent2))
											p2 (cdr (assoc 10 (entget pent2)))
										)
										;;���
										(setq depthstr (nth 11 rowdata)
											d1 0 d2 0)
										(if (> (strlen depthstr) 2)
											(if (setq pos (vl-string-search "/" depthstr))
												(progn
													(setq d1 (atof (substr depthstr 1 pos))
														d2 (atof (substr depthstr (+ pos 2)))
														p1 (list (car p1) (cadr p1) (- (caddr p1) d1))
														p2 (list (car p2) (cadr p2) (- (caddr p2) d2))
													)
												)
											)
										)
										;;����
										(setq holestr (nth 13 rowdata))
										(if (> (strlen holestr) 0)
											(if (setq pos (vl-string-search "/" holestr))
												(setq holecount (substr holestr 1 pos)
													holeused (substr holestr (+ pos 2)))
											)
										)
										;;ѹ�����ѹ
										(if (or (= "Q" maintype)  (= "Q" maintype)  (= "Q" maintype))
											(setq pressure (nth 10 rowdata)
												voltage ""
											)
											(setq voltage (nth 10 rowdata)
												pressure ""
											)
										)
										;;cab_count
										(if (or (= "X" maintype) (= "L" maintype) (= "D" maintype) (= "H" maintype))
											(setq cabcount (nth 12 rowdata))
											(setq cabcount "")
										)
										;;Flowdirect
										(if (or (= "P" maintype) (= "W" maintype) (= "Y" maintype) (= "G" maintype))
											(setq flowdirect (atoi(nth 17 rowdata)))
											(setq flowdirect 0)
										)
										(setq outlist nil
											outlist (cons (cons "Start_Point" mapno) outlist)
											outlist (cons (cons "End_Point" mapno2) outlist)
											outlist (cons (cons "End_Deep" d2) outlist)
											outlist (cons (cons "Start_Deep" d1) outlist)
											
											outlist (cons (cons "Material" (nth 2 rowdata)) outlist)
											outlist (cons (cons "D_S" (nth 9 rowdata)) outlist)
											outlist (cons (cons "Cab_Count" cabcount) outlist)
											outlist (cons (cons "Main_Type" maintype) outlist)
											outlist (cons (cons "Hole_Count" holecount) outlist)
											outlist (cons (cons "Hole_Used" holeused) outlist)
											outlist (cons (cons "Pressure" pressure) outlist)
											outlist (cons (cons "Voltage" voltage) outlist)
											outlist (cons (cons "Flowdirect" flowdirect) outlist)
											;outlist (cons (cons "P_Material" (nth 11 rowdata)) outlist)
											outlist (cons (cons "Property" (nth 14 rowdata)) outlist)
											outlist (cons (cons "Remark" (nth 19 rowdata)) outlist)
											outlist (cons (cons "BuryWay" (nth 15 rowdata)) outlist)
											outlist (cons (cons gl_AppName gl_Version) outlist)
										)
										;(AddNewLine p1 p2 maintype outlist)
										(if (setq newl (AddNewLine p1 p2 maintype outlist))
											(progn
												(ldata-put newl "Start_Deep" d1)
												(ldata-put newl "End_Deep" d2)
											)
										)
									)
								)
							)
						)
					)
				)
			)
			 (vlxls-app-quit *xls* nil)
		)
		(progn
			(prompt (strcat "\n���ļ�" XLSFile "ʧ�ܣ�"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGBZY��ʱ")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "��.")
		)
	)
	(princ)
)
;*********************************************************************************************
;��������:C:ImportFromCGBZY(XLSFile)
;���ܣ��ӳɹ����е���������ݣ� ��̽�����������һ�¡�
;������XLSFile��excel xls�ļ�
;���أ�
;����ʱ�䣺2015/03/06  21��00
;�޸�ʱ�䣺2015/10/29  21��00
;�����ˣ����۾�
;*********************************************************************************************
(defun C:ImportFromCGBZY( / bf bs cellid emptyrowcount entlist feature featurelist featurestr 
 hent holecount holestr holeused linename1 linename2 lineslist linkent linkpoint linkpointlist
 maintype mapno mapno2 ndata outlist p1 p2 path0 pent pent1 pent2 pointslist pos pressure
 rangeid1 rangeid2 row rowdata sheet sheets subsidlist subsidstr usedrange voltage xlsfile d1 d2 
 depthstr newl cabcount flowdirect)
 
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n�������̽��ɹ���,�ļ���ʽΪxls.
				\n�Զ�����ͼ��(���ݵ��ǰ׺����ͼ��).")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "ѡ��ɹ���" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;��ʼ�� excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open XLSFile nil))
		(progn
			(setq sheets (vlxls-sheet-get-all *xls*)
				;;usedrange  (vlxls-sheet-get-UsedRange *xls*)
				pointslist nil	;;���е���ĵ�
				lineslist nil	;;��ʱδ������߶�
				linkpointlist nil
			)
			
			(foreach sheet sheets
				(setq usedrange  (vlxls-range-getid (vlxls-sheet-get-UsedRange *xls* sheet))
					cellid (vlxls-cellid usedrange)
					rangeid1  (vlxls-rangeid (car cellid))
					rangeid2  (vlxls-rangeid (cadr cellid))
					row 3	;;�к�
					emptyrowcount 0	;;�������е�����
					maintype (GetTypeFromPointName sheet)	;;�������ӱ��������������
					
					featurelist ( getfeaturelist-fromtype maintype)
					subsidlist (getsubsidlist-fromtype maintype)
					gl_BlockNameList nil
					hasdata T
				)
				(if (= maintype "B");����
					(if (and (/= sheet "B") (/= sheet "����"))
						(setq hasdata nil)
					)
				)
				
				(if (and hasdata (vlxls-sheet-put-active *xls* sheet))
					(progn
						(while (and (< emptyrowcount 10) (<= row  (cadr rangeid2)))	;;row in the range and ��������С��10��
							;;��ȡһ��
							(setq rowdata (vlxls-cell-get-value *xls*  (strcat "A"  (rtos row 2 0) ":T"  (rtos row 2 0) ))
								rowdata (car rowdata)
								ndata (length rowdata)
								mapno (car rowdata)
								linkpoint (cadr rowdata)
							)
							(if (<= (strlen mapno) 0)	
								(setq emptyrowcount (1+ emptyrowcount))	;;����
								(progn
									;;�����б��У������µ�
									(if (null (assoc mapno pointslist))
										(progn	;;�ǿ���
											;;��������
											(setq p1 (list (atof (nth 6 rowdata)) (atof (nth 5 rowdata)) (atof (nth 7 rowdata)))
												feature (nth 3 rowdata)
												subsid	(nth 4 rowdata)
												featurestr "���ղ�"
												subsidstr ""
												emptyrowcount 0 
												bf nil bs nil
												depth (- (atof (nth 7 rowdata)) (atof (nth 8 rowdata)))
											)
											(if (member feature featurelist)
												(setq featurestr feature
													bf T)
												(setq bf nil)
											)
											(if (member subsid subsidlist)
												(setq subsidstr subsid
													bs T)
												(setq bs nil)
											)
											(if (and (= nil bf) (= nil bs))
												(prompt (strcat "\n �����ļ���δ���������͸���������:" feature))
											)
											(setq outlist nil
												outlist (cons (cons "Exp_No" mapno) outlist)
												outlist (cons (cons "Map_No" mapno) outlist)
												outlist (cons (cons "Depth" depth) outlist)
												outlist (cons (cons "Subsid" subsidstr) outlist)
												outlist (cons (cons "Feature" featurestr) outlist)
												outlist (cons (cons "Main_Type" maintype) outlist)
											)
											(setq pent (AddNewPoint p1 mapno maintype featurestr subsidstr outlist)
												hent (cdr (assoc 5 (entget pent)))
												)
											;;add new point  to pointslist
											(setq pointslist (cons (cons mapno hent)  pointslist))														
																									
										)
									)	
									;;���߶μ����б�
									(if (> (strlen linkpoint) 0)
										(setq linkpointlist (cons (cons maintype rowdata) linkpointlist))
									)
								)
							)
							(setq row (1+ row))
						)
					)
				)
			)
			;;����ȫ���߶�
			(foreach rowdata linkpointlist
				(setq maintype (car rowdata)
					rowdata (cdr rowdata)
					mapno (car rowdata)
					mapno2 (cadr rowdata)
					pent1 (assoc mapno pointslist)
					pent2 (assoc mapno2 pointslist)
					linename1 (strcat mapno mapno2)
					linename2 (strcat mapno2 mapno)
				)
				(if (and pent1 pent2 (not (or (member linename1 lineslist) (member linename2 lineslist))))
					(progn
						(setq pent1 (handent (cdr pent1))
							p1 (cdr (assoc 10 (entget pent1)))
							pent2 (handent (cdr pent2))
							p2 (cdr (assoc 10 (entget pent2)))
						)
						;;���
						(setq depthstr (nth 11 rowdata)
							d1 0 d2 0)
						(if (> (strlen depthstr) 2)
							(if (setq pos (vl-string-search "/" depthstr))
								(progn
									(setq d1 (atof (substr depthstr 1 pos))
										d2 (atof (substr depthstr (+ pos 2)))
										p1 (list (car p1) (cadr p1) (- (caddr p1) d1))
										p2 (list (car p2) (cadr p2) (- (caddr p2) d2))
									)
								)
							)
						)
						;;����
						(setq holestr (nth 13 rowdata))
						(if (> (strlen holestr) 0)
							(if (setq pos (vl-string-search "/" holestr))
								(setq holecount (substr holestr 1 pos)
									holeused (substr holestr (+ pos 2)))
							)
						)
						;;ѹ�����ѹ
						(if (or (= "Q" maintype)  (= "Q" maintype)  (= "Q" maintype))
							(setq pressure (nth 10 rowdata)
								voltage ""
							)
							(setq voltage (nth 10 rowdata)
								pressure ""
							)
						)
						;;cab_count
						(if (or (= "X" maintype) (= "L" maintype) (= "D" maintype) (= "H" maintype))
							(setq cabcount (nth 12 rowdata))
							(setq cabcount "")
						)
						;;Flowdirect
						(if (or (= "P" maintype) (= "W" maintype) (= "Y" maintype) (= "G" maintype))
							(setq flowdirect (atoi(nth 17 rowdata)))
							(setq flowdirect 0)
						)
						(setq outlist nil
							outlist (cons (cons "Start_Point" mapno) outlist)
							outlist (cons (cons "End_Point" mapno2) outlist)
							outlist (cons (cons "End_Deep" d2) outlist)
							outlist (cons (cons "Start_Deep" d1) outlist)
							
							outlist (cons (cons "Material" (nth 2 rowdata)) outlist)
							outlist (cons (cons "D_S" (nth 9 rowdata)) outlist)
							outlist (cons (cons "Cab_Count" cabcount) outlist)
							outlist (cons (cons "Main_Type" maintype) outlist)
							outlist (cons (cons "Hole_Count" holecount) outlist)
							outlist (cons (cons "Hole_Used" holeused) outlist)
							outlist (cons (cons "Pressure" pressure) outlist)
							outlist (cons (cons "Voltage" voltage) outlist)
							outlist (cons (cons "Flowdirect" flowdirect) outlist)
							;outlist (cons (cons "P_Material" (nth 11 rowdata)) outlist)
							outlist (cons (cons "Property" (nth 14 rowdata)) outlist)
							outlist (cons (cons "Remark" (nth 19 rowdata)) outlist)
							outlist (cons (cons "BuryWay" (nth 15 rowdata)) outlist)
							outlist (cons (cons gl_AppName gl_Version) outlist)
						)
						;(AddNewLine p1 p2 maintype outlist)
						(if (setq newl (AddNewLine p1 p2 maintype outlist))
							(progn
								(ldata-put newl "Start_Deep" d1)
								(ldata-put newl "End_Deep" d2)
								(setq lineslist (cons linename1 lineslist)
										lineslist (cons linename2 lineslist)
								)
							)
						)
					)
				)
			)
			(vlxls-app-quit *xls* nil)
		)
		(progn
			(prompt (strcat "\n���ļ�" XLSFile "ʧ�ܣ�"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGBZY��ʱ")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "��.")
		)
	)
	(princ)
)
;*********************************************************************************************
;��������:C:ImportFromTable(XLSFile)
;���ܣ������ֶ���Ա���������ݡ�
;������
;���أ�
;����ʱ�䣺2015/03/17  21��33
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:ImportFromTable()
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n�������ݿ������ļ��������ݿ��еĵ����,�����ļ���ʽΪxls,���ݿ�Ϊaccess.
				\n")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "ѡ�����ݿ������ļ�" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;��ʼ�� excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open XLSFile nil))
		(progn
			(setq asheet (vlxls-Sheet-Get-Active *xls*)
				usedrange  (vlxls-range-getid (vlxls-sheet-get-UsedRange *xls* asheet))
				cellid (vlxls-cellid usedrange)
				rangeid1  (vlxls-rangeid (car cellid))
				rangeid2  (vlxls-rangeid (cadr cellid))
				row 0	;;�к�
				info ())
			(vlxls-app-quit *xls* nil)
		)
		;;else
		(progn
			(prompt (strcat "\n���ļ�" XLSFile "ʧ�ܣ�"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGB��ʱ")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "��.")
		)
	)
	(princ)
)







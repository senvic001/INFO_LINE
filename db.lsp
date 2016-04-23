;;;函数名:LINE_INFO_ADOLib_ConnectString2
;;;输入变量:dbFile(打开数据库的路径名)
;;;输出值:ConnectString
;;;注释:使用JET4.0连接MS-Access数据库,64位系统上，JET引擎已不支持,需改用ACE引擎
;;;     示例： (LINE_INFO_ADOLib_ConnectString2 "d:/dbfiles/products.mdb")
;;;设计者: 
;;;设计日期:2013年8月
;;;修改者:
;;;修改日期:
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
            ) ;对于64位系统的数据库引擎连接
        ) ;_ end_progn
        (progn
            (setq ConnectString
                     (strcat "Provider=Microsoft.JET.OLEDB.4.0;"
                             "Data Source="
                             dbFile
                             ";Persist Security Info=False"
                     ) ;_ end_strcat
            ) ;对于32位系统的数据库引擎连接
        ) ;_ end_progn
    ) ;end if
    ConnectString
) ;_ end_defun


;;打开数据库文件

(defun C:LineInfo_OpenMDBFile ( / result strpath)
    ; (if (setq strpath (Xrecord-Get gl_AppName "ConnectString"))
        ; (progn
            ; (prompt (strcat "\n\n该图形已有数据库：" (car strPath) "\n不能与其他数据再建立连接！"
                        ; "请先删除该数据库，再打开新的数据库。\n"))
            ; (exit)
            ; );progn
        ; )
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "\\database.mdb")
	)
    (if (setq strPath (getfiled "选择包含点线数据的Access数据库文件" path0 "MDB" 0))
        (progn
            (prompt (strcat "\n\n数据库路径：" strPath "\n"))
	    (if (setq result (getstring "\n仅支持杭州管线数据库，该数据库是标准杭州管线数据库吗?(Y/N)"))
	        (if (= "Y" (strcase result))
				(progn
					(INIT_LINEINFO strPath)
					;导入数据库
					; (setq result (getstring "\n导入数据库,并成图吗?(Y/N)"))
					; (if (= "Y" (strcase result))
						; (C:LineInfo_DrawAll)
					; )
				)
	        )
	    )
            );progn
        (prompt (strcat "\n\n未打开任何数据库。" ))
        );if
    
    (prin1)
)

;;新建数据库文件：当打开一个新的点线图时，需要增加数据库
(defun LineInfo_NewMDBFile (dbfile / result strpath dbpath path0)
    (LineInfo_GetSupportPath)
    ; (if (setq strpath (Xrecord-Get gl_AppName "ConnectString"))
        ; (progn
            ; (prompt (strcat "\n\n该图形已有数据库：" (car strPath) "\n不能与其他数据再建立连接！"
                        ; "请先删除该数据库，再打开新的数据库。"))
            ; (exit)
            ; );progn
        ; )
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "\\database.mdb")
	)
    (setq strPath (getfiled "新建包含点线数据的Access数据库文件" path0 "MDB" 1))
    ;;复制空白数据库，并改名
    (if strPath
        (progn
            (setq dbpath (strcat gl_INFO_LINE_PATH "database\\" dbfile ))
            (vl-file-delete strPath)
            (if (vl-file-copy  dbpath  strPath )
                (progn
                    (prompt (strcat "\n新建数据库成功！\n数据库路径：" strPath "\n"))
                    (INIT_LINEINFO strPath)
                    );progn
                ;;else
                (prompt (strcat "\n\打开数据库文件" strPath "错误！\n"))
                )
            );prong
        (progn
            (prompt (strcat "\n打开文件错误！"))
            );
        );if
    
    (prin1)
)

;;;;;显示数据库信息
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
        (prompt (strcat "\n图中不含" gl_AppName "数据库。\n"))
      );if  
);defun

;;删除图形中Xrecord存放的数据库的信息，
;;但不删除图元中的数据记录，也不删除数据库文件；
(defun C:LineInfo_DelDBInfo ( / strdb tables tb)
    (if (setq strdb (Xrecord-Get gl_AppName "ConnectString"))
        (progn
            (setq tables (Xrecord-Get gl_AppName "Tables"))
            (foreach tb tables
                (Xrecord-Delete gl_AppName (strcat "Table_" tb))
                )
            (Xrecord-Delete gl_AppName "Tables")
            (Xrecord-Delete gl_AppName "ConnectString")

            (prompt (strcat "\n删除" gl_AppName "数据库成功！\n"))
        );tpron
        (prompt (strcat "\n图中不含" gl_AppName "数据库。\n"))
    );if

);defun

;*********************************************************************************************
;函数定义:Init_LineInfo(dbpathname)
;功能：加载数据库信息，
;建立字典gl_AppName，加入两个数据：TABLE_POINT 点表字段 TABLE_LINE 线表字段
;参数：MDB数据库文件的路径
;返回：
;创建时间：2014/07/11   16:10
;修改时间：2015/12/31	13:53
;创建人：沈雄君
;*********************************************************************************************
(defun Init_LineInfo(dbpathname / app collist columnslist connectionobject connectstring dict dictname dicts doc  index num tableslist tname)
   (setq app (vlax-get-acad-object)
          doc (vla-get-activedocument app)
          dicts (vla-get-Dictionaries doc)
          dict (vla-add dicts gl_AppName) ;以appName作为字典名
          dictname gl_AppName
          )
   ;;连接数据库
   (setq ConnectionObject nil
	 ;;ConnectString (strcat "Provider=MSDASQL;Driver={Microsoft Access Driver (*.mdb)};DBQ=" dbpathname ))
	 ConnectString (LINE_INFO_ADOLib_ConnectString2 dbpathname));适应32和64位系统
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
			; (Xrecord-Rebuild dictname "Tables" TablesList);写入所有表名("T1name" "T2name"...)
			
			;;写入点表和线表字段到xrecord "TABLE_POINT"
			; (setq tname "JP"
				; ColumnsList (ADOLISP_GetColumns ConnectionObject tname))
			; (Xrecord-Rebuild dictname "TABLE_POINT" ColumnsList);"Table_JP"
			; ;;写入点表和线表字段到xrecord TABLE_LINE
			; (setq tname "JL"
				; ColumnsList (ADOLISP_GetColumns ConnectionObject tname))
			; (Xrecord-Rebuild dictname "TABLE_LINE" ColumnsList);"Table_JP"
			
			; (ADOLISP_DisconnectFromDB ConnectionObject)
			; (setq ConnectionObject nil)
				;;写入连接字符
			(Xrecord-Rebuild dictname "ConnectString" ConnectString)
		);pron
    );
);defun

;;获取连接对象
;;打开数据库，返回数据库对象 or nil
;;记得关闭数据库连接
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
;函数定义:C:toHZDB()
;功能：把点表中的实体保存到数据库
;参数：
;返回：
;创建时间：2014/12/30   22:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:toHZDB	(/ all-line allp entlist i outlist ent)
	(print "\n保存图形数据到杭州管线数据库.")
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
		(prompt (strcat "\n保存" (rtos (length outlist) 2 0) "个管线点到数据库."))
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
		(prompt (strcat "\n保存" (rtos (length outlist) 2 0) "个管线对象到数据库."))
	) ;_ end_if
	
	;;save border
	(SaveBorderData)
	(princ)
) ;_ end_defun


;*********************************************************************************************
;函数定义:C:toDB()
;功能：把点表中的实体保存到DB数据库Line_Info_empty_s
;参数：
;返回：
;创建时间：2016/4/13  15:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:toDB( / all-line allp entlist i outlist ent)
	(print "\n保存图形数据到DB管线数据库.")
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
		(prompt (strcat "\n保存" (rtos (length outlist) 2 0) "个管线点到数据库."))
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
		(prompt (strcat "\n保存" (rtos (length outlist) 2 0) "个管线段到数据库."))
	) ;_ end_if
	
	;;save border
	(SaveBorderData)
	(princ)
)

;*********************************************************************************************
;函数定义:C:fromDB()
;功能：从DB数据库成图
;参数：
;返回：
;创建时间：2016/4/13  15:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:fromDB()
	(DrawTables2)
	(princ)
)

;*********************************************************************************************
;函数定义:SaveEntitysDB(entlist)
;功能：把实体表中的实体保存到数据库
;参数：实体表，包含ldata
;返回：保存的实体名表，成功，则该表长度为entlist,并修改实体的状态码；
;创建时间：2014/07/18   18:00
;修改时间：2014/12/30   21:00
;创建人：沈雄君
;*********************************************************************************************
;;tablename ：两种情况nil or LINES，如果为nil，自动识别为“类型+P”为表名，否则加入统一的点表POINTS
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
					;;确定表名
				    (if (= nil tablename)
						(setq tablename (strcat maintype "P"))
					)
					;;建立SQL语句
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
							;;获取属性文字的位置信息
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
										(if (= "点号" (vla-get-tagstring e))
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
							;;--------湖州管线调查5.0-----------------
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
							;;----------------湖州管线调查-----------end
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
;;tablename ：两种情况nil or LINES，如果为nil，自动识别为“类型+L”为表名，否则加入统一的线表LINES
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
					;;确定表名
				    (if (= nil tablename)
						(setq tablename (strcat maintype "L"))
					)
					;;保存线段
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
							;;--------------------------湖州管线调查5-------------
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
							;;--------------------------湖州管线调查end------------------
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

;函数定义:DelPointRows(ConnectionObject tablename namelist)
;功能：delelte points 
;参数: 
;返回值：bool
;创建时间：2011/02/08 12:00
;修改时间：2014/07/20 12:00
;创建人：沈雄君
;*********************************************************************************************
(defun DelPointRows  (ConnectionObject tablename namelist / pname sqlstr)
    (foreach pname  namelist
        (setq sqlstr (strcat "DELETE * FROM " tablename " WHERE Map_No='" pname "' "))
        (ADOLISP_DoSQL ConnectionObject sqlstr)
        )
    ) ;defun

;*********************************************************************************************
;函数定义:AddNewRows(ConnectionObject tablename datalist)
;功能：把datalist 加入到表中
;参数: 链接对象，表名，数据表(() () ....).datalist中的每个元素表示数据库表的一行
;返回值：bool
;创建时间：2011/02/06 12:00
;修改时间：2014/07/20 12:00
;创建人：沈雄君
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
;函数定义:GetAllPointsFromDB()
;功能：读取数据库中的点，放在点表中；
;参数：
;返回：点表(("p1" "JP")( "p2" "JP)...)
;创建时间：2014/07/20   18:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun GetAllPointsFromDB( / connectionobject  outlist result sqlstr tables tb)
    (setq outlist nil)
    (if (setq ConnectionObject (GetConnectionObject))
        (progn
            (setq tables (Xrecord-Get gl_AppName "Tables"));tablenames
            (foreach tb  tables
                (if (vl-string-search "P" tb 1);表名的第二个字符包含P
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

;函数定义:GetAllPoinsFromDB()
;功能：读取数据库中的点，放在点表中；
;返回：点表((p1 p2 JL)(p3 p4 JL)...)
(defun GetAllLinesFromDB( / connectionobject  outlist result sqlstr tables tb)
    (setq outlist nil)
    (if (setq ConnectionObject (GetConnectionObject))
        (progn
            (setq tables (Xrecord-Get gl_AppName "Tables"));tablenames
            (foreach tb  tables
                (if (vl-string-search "L" tb 1);表名的第二个字符包含P
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
;函数定义:LINF_INFO_Stat()
;功能：工作量统计：点数，线段总长，分类线段长度
;参数：
;返回：
;创建时间：2014/07/21   15:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************


;*********************************************************************************************
;函数定义:SynchDB-DWG()
;功能：同步数据库和图形文件中数据
;参数：
;返回：
;创建时间：2014/07/21   15:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
; (defun C:SynchDWG-DB  (/ alldb-lines alldb-points alllinerefset allpointblockset allpointlist bs connectionobject dbsuccess
                       ; dline dpoint e1 el endp i indb indwg ldata line mapno multilines multipoints nline outlist
                       ; p1 p2 pdata point pointdatalist pretodb pretodwg-line pretodwg-point ptype re result scode startp strsql table tableline tablep xpos ypos zpos)
    ; (setq ConnectionObject (GetConnectionObject))
    ; (if (= nil ConnectionObject)
        ; (progn
            ; (setq re (Fun_InPutString "N"
                                      ; "USERS5"
                                      ; "\n未找到数据库文件，新建一个数据库文件或打开一个现有的MDB文件吗？(新建N/打开O)"))
            ; (if (= "N" (strcase re))
                ; (C:LineInfo_NewMDBFile))
            ; (if (= "O" (strcase re))
                ; (C:LineInfo_OpenMDBFile))
            ; ) ;progn
        ; ) ;if
    ; (setq ConnectionObject (GetConnectionObject))
    
    ; (prompt "\n数据库与图形文件开始同步......")
    ; (setq pretoDB nil ;点名，图元名
          ; pretoDWG-point nil ;点名，数据
          ; pretoDWG-line nil ;点名，数据
          ; )
    ; (setq AllPointBlockSet (ssget "X" (list (cons 0 "INSERT")))
          ; AllLineRefSet    (ssget "X" (list (cons 0 "LINE")))
          ; MultiPoints      nil ;重复点表
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
                            ; (setq pretoDB (append pretoDB (list bs))) ;唯一点，加入入库列表
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
                            ; (setq pretoDB (append pretoDB (list bs)));唯一线段，加入入库列表
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
                ; (if (= 0 inDWG) ;该点不在图中
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
                ; (if (= 0 inDWG) ;该点不在图中
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
                ; (setq dbsuccess 1);写入数据成功！
                ; )

            ; ;;Draw db to DWG)
            ; ;;pretoDWG-point nil ;点名，数据
            ; ;;pretoDWG-line nil ;点名，数据
            ; ;;3-1得到所有需要绘图的点名，和数据表;
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
            ; ;;3-2查询数据
            ; (setq pointdatalist nil)
	    ; (foreach  point allPointlist
                ; (setq strSQL (strcat "SELECT * FROM " (cadr point) " WHERE Map_No=" "'" (car point) "'"))
                ; (if (setq result (ADOLISP_DoSQL ConnectionObject strSQL))
                    ; (setq pointdatalist (append pointdatalist (cdr result)))
                    ; );if
                ; );foreach
            
            ; ;;3-3绘制图形
            ; ;;3-3-1绘制点DrawPoint (strP x y z scode scale  pointtype
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
            ; (prompt "\n同步完成。")
            ; (prompt (strcat "\n" (rtos (length pretoDWG-point) 2 0) "个管线点从数据库绘制到图形！\n"))
            ; (vl-prin1-to-string pretoDWG-point)
            ; (prompt (strcat "\n" (rtos (length pretoDWG-line) 2 0) "个管线段从数据库绘制到图形！\n"))
            ; (vl-prin1-to-string pretoDWG-line)
            ; (prompt (strcat "\n" (rtos (length pretoDB) 2 0) "个管线点或线段被写入数据库！\n"))
            ; ;;
            ; ) ;progn
        ; ) ;if
    ; ) ;defun
; ;;
;*********************************************************************************************
;函数定义:C:outhz()
;功能：输出到杭州成果表,符合物探院格式.
;参数：
;返回：
;创建时间：2014/12/16   12:40
;修改时间：
;创建人：沈雄君
;V5.2 版 不再使用
;*********************************************************************************************
(defun C:outhz( /  alllinelist alllineset cab_count cabstr depth depth2 dlist e edge edge1 edge2 endp
			   ent entl entlist filep filepath flist flowdirect glist handle headstr hlist hole_count
			   hole_used i jlist ldata linelist linkedges linkp llist lnum nextline nline outstr pent1
			   pent2 plist pos1 pos2 pressure pstart qlist rlist singlelist stackin stackout startp
			   subsidstr tname typelist typename typestr voltage wlist xlist ylist zh zlist DimZin1 p10 
			   p11 deltH podu exchangePoint)
	(princ "\n输出成果表,格式为浙江省工程物探勘察院格式.")
	(if (setq enter (Fun_InPutString "Y" "USERS1" "\n输出成果表之前必须进行错误检查.是否现在进行错误检查?(Y/N)"))
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
						handle (cdr (assoc 5 entlist));句柄
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
	;;输出成果表-杭州-浙江省工程物探勘察院成果表
	(setq filePath   "c:\\line_info_achive_HZ.csv"
		headstr "图上点号,连接点号,材质,特征,附属物,X_m,Y_m,地面高程m,管顶(沟底)高程m,管径mm,压力/电压,埋深m,电缆条数,已用孔数/总孔数,权属单位代码,埋设方式,埋设年代,流向,线型,备注,长度,坡度,井深,井规格"
	)
	;;打开点文件
     (vl-file-delete filePath)
	 (setq DimZin1 (getvar "DIMZIN"))
	 (setvar "DIMZIN" 0)
     (if (not (setq filep (open filePath "w")))
         (progn
             (Prompt (strcat "\n打开文件" filePath "失败。退出！"))
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
					;;深度优先 
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
							)	;管线高程
							(setq depth (ldata-get entl "Start_Deep")
								depth2 (ldata-get entl "End_Deep")
								zh (- (caddr pstart) depth)
							)	;管线高程
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
						(if (= nil (setq subsidstr (ldata-get startp "Subsid")));;兼容性 2.0版 subsid 为nil 
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
							outstr (strcat outstr "," voltage pressure)	;两者之一为空字符串
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
						(if (= nil (setq subsidstr (ldata-get linkp "Subsid")));;兼容性 2.0版 subsid 为nil 
							(setq subsidstr "")
						)
						(setq pstart (cdr (assoc 10 (entget linkp)))
							;depth (ldata-get linkp "Depth")
							zh (- (caddr pstart) depth2))	;管线高程
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
								(if  (vl-position nextline stackout);是否存闭合管线
									(prompt (strcat "\n警告:发现管线闭合!	包含管线点:" (ldata-get nextline "Start_Point") "\t" (ldata-get nextline "End_Point")))
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
;函数定义:C:outCGB_xls()
;功能：输出到成果表模版,与正元格式一致.
;变量：单位名称，报告时间，目录名-页码 总页数
;参数：
;返回：
;创建时间：2015/10/25   9:40
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:outCGB_xls( / *xls* activesheet alllinelist alllineset cab_count cabstr cgbfile currownum 
			delth depth depth2 dimzin1 dlist e edge edge1 edge2 endp ent enter entl entlist fd flist 
			flowdirect glist handle hlist hole_count hole_used holestr i jlist length_l linelist 
			linkedges linkp llist lnum nextline nline outdata p10 p11 path0 pent1 pent2 plist podu 
			pos1 pos2 pstart qlist rlist sheets singlelist stackin stackout startp subsidstr templatefile 
			tname typel welldeepstr welldeepstr2 welldeepstr1)
	(princ "\n输出管线成果表.")
	;;输出成果表-杭州-浙江省工程物探勘察院成果表
	;;从模版生成成果表
	(LineInfo_GetSupportPath)
	(setq templateFile (strcat gl_INFO_LINE_PATH "database\\管线成果表.xls"))
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
	;	(*error* (strcat “创建文件” CGBfile "失败。"))
	;)
	
	;;建立TOPO关系
	(if (setq enter (Fun_InPutString "Y" "USERS1" "\n输出成果表之前必须进行错误检查.是否现在进行错误检查?(Y/N)"))
		(if (= "Y" (strcase enter)) (UpdateTopo))
	)
	
	(if (= nil gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	
	;;管线分类
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
						;handle (cdr (assoc 5 entlist));句柄
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
	
	;;输出到xls templateFile
	
	 (setq DimZin1 (getvar "DIMZIN"))
	 (setvar "DIMZIN" 0)
    (setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;初始化 excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open templateFile nil))
		(progn
			(setq sheets (vlxls-sheet-get-all *xls*)
				;;usedrange  (vlxls-sheet-get-UsedRange *xls*)
			) 
		)
		(*error* (strcat "打开文件" templateFile "失败。"))
	)
	(foreach e AllLineList
		(setq linelist (cadr e)
			tname (car e)
			nline (length linelist)
			singlelist nil ;the first edges
			i 0
			curRowNum 3)	;;当前行号
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
			(*error* (strcat "导出成果表错误！模版文件类型缺少" tname "."))
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
					;;优先输出端点
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
					;;深度优先 
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
						;;pstart 与 线段的起点是否一致
						(if (> (distance p10 pstart) 0.001)
							(progn
								(setq depth (ldata-get entl "End_Deep")
										depth2 (ldata-get entl "Start_Deep")
										zh (- (caddr pstart) depth)
								)	;管线高程
								(if (= fd 1) 
									(setq fd 2)
									(if (= fd 2) (setq fd 1))
								)
							)
							(progn
								(setq depth (ldata-get entl "Start_Deep")
										depth2 (ldata-get entl "End_Deep")
										zh (- (caddr pstart) depth)
									)	;管线高程
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
						
						(if (= nil (setq subsidstr (ldata-get startp "Subsid")));;兼容性 2.0版 subsid 为nil 
							(setq subsidstr "")
						)
						
						;;井深 兼容5.0以前版本，不含"Well_Deep"
						(setq welldeepstr1 (ldata-get startp "Well_Deep") 
							welldeepstr2 (ldata-get linkp "Well_Deep"))
						(if (= nil welldeepstr1) (setq welldeepstr1 ""))
						(if (= nil welldeepstr2) (setq welldeepstr2 ""))
						(setq welldeepstr (strcat welldeepstr1  "/" welldeepstr2))
						;;输出数据一行
						(setq outdata (list (ldata-get startp "Map_No")
							(ldata-get linkp "Map_No")
							(ldata-get entl "Material")
							(ldata-get startp "Feature")
							subsidstr
							(rtos (cadr pstart) 2 3) (rtos (car pstart) 2 3) (rtos (caddr pstart) 2 3) (rtos  zh 2 3)
							(ldata-get entl "D_S")
							(strcat (ldata-get entl "Voltage") (ldata-get entl "Pressure"))
							(strcat (rtos  depth 2 2) "/" (rtos depth2 2 2))	;;起点深度/终点深度
							cabstr	
							holeStr ;;已用孔数/总孔数
							nil	;;权属单位代码
							(ldata-get entl "BuryWay")
							nil	;;埋藏年代
							flowdirect
							nil 	;;线型
							(ldata-get entl "Remark")
							nil 	;;以下自定义内容
							(rtos length_l 2 2)	;;长度
							podu		;;坡度
							(ldata-get startp "Point_Size")	;;井规格
							welldeepstr ;;井深
							))
						(vlxls-put-row-value *xls* (list 1 curRowNum) outdata)
						(setq curRowNum (1+ curRowNum))
						
						
						(if (= nil (setq subsidstr (ldata-get linkp "Subsid")));;兼容性 2.0版 subsid 为nil 
							(setq subsidstr "")
						)
						(setq pstart (cdr (assoc 10 (entget linkp)))
							;;depth (ldata-get linkp "Depth")
							zh (- (caddr pstart) depth2))	;管线高程
						
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
						;;输出表
						(setq stackout (cons entl stackout))
						(if (> (length singlelist) 0)
							(setq singlelist (vl-remove entl singlelist))
						)
						(setq linelist (vl-remove entl linelist))
						;;
						(setq linkedges (ldata-get linkp "Edge"))	;;！！！可能包含其他类别的管线段
						(foreach e linkedges
							(setq nextline (handent (last e))
								  pent1 (handent (car e))
								  pent2 (handent (cadr e))
								  linkTypeName (ldata-get nextline "Main_Type")
								  )
							(if (and  (not (equal nextline entl)) (= typename linkTypeName))
								(if  (vl-position nextline stackout);是否存闭合管线
									(prompt (strcat "\n警告:发现管线闭合!	包含管线点:" (ldata-get nextline "Start_Point") "\t" (ldata-get nextline "End_Point")))
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
;函数定义:C:ImportFromCGB(XLSFile)
;功能：从成果表中导入点线数据， 物探点号与测量点号一致。
;参数：XLSFile：excel xls文件
;返回：
;创建时间：2015/03/06  21：00
;修改时间：
;创建人：沈雄君
;V5.2 版 不再使用
;*********************************************************************************************
(defun C:ImportFromCGB( / bf bs cellid emptyrowcount entlist feature featurelist featurestr 
 hent holecount holestr holeused linename1 linename2 lineslist linkent linkpoint linkpointlist
 maintype mapno mapno2 ndata outlist p1 p2 path0 pent pent1 pent2 pointslist pos pressure
 rangeid1 rangeid2 row rowdata sheet sheets subsidlist subsidstr usedrange voltage xlsfile)
 
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n导入管线探测成果表,文件格式为xls.
				\n自动划分图层(根据点号前缀划分图层).")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "选择成果表" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;初始化 excel
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
					row 3	;;行号
					pointslist nil	;;所有导入的点
					linkpointlist nil
					emptyrowcount 0	;;连续空行的数量
					lineslist nil	;;暂时未加入的线段
					maintype (GetTypeFromPointName sheet)	;;！！！从表名获得类型名称
					featurelist ( getfeaturelist-fromtype maintype)
					subsidlist (getsubsidlist-fromtype maintype)
					gl_BlockNameList nil
				)
				
				(if (vlxls-sheet-put-active *xls* sheet)
					(progn
						(while (and (< emptyrowcount 10) (<= row  (cadr rangeid2)))	;;row in the range and 连续空行小于10个
							;;读取一行
							(setq rowdata (vlxls-cell-get-value *xls*  (strcat "A"  (rtos row 2 0) ":" "R"  (rtos row 2 0) ))
								rowdata (car rowdata)
								ndata (length rowdata)
								mapno (car rowdata)
								linkpoint (cadr rowdata)
							)
							(if (<= (strlen mapno) 0)	
								(setq emptyrowcount (1+ emptyrowcount))	;;空行
								(progn
									;;不在列表中，则建立新点
									(if (null (assoc mapno pointslist))
										(progn	;;非空行
											;;分析数据
											(setq p1 (list (atof (nth 4 rowdata)) (atof (nth 3 rowdata)) (atof (nth 5 rowdata)))
												feature (nth 2 rowdata)
												featurestr "非普查"
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
												(prompt (strcat "\n 配置文件中未包含特征和附属物类型:" feature))
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
													; (prompt (strcat "\n点" mapno "未找到对应的特征点或附属物，该点被忽略。" ))
													; ;;(exit)
												; )
												; (progn
													; ;;创建点
													
													
												; )
											; )
										)
									)	
									;;创建线
									(if (> (strlen linkpoint) 0)
										(progn
											(setq linename1 (strcat mapno linkpoint)
												linename2 (strcat linkpoint mapno)
											)
											;;线段还未建立
											(if (not (or (member linename1 lineslist) (member linename2 lineslist)))
												(if (setq linkent  (cdr  (assoc linkpoint pointslist)))	;;点已经在点表中
													(progn
														;;add line
														(setq pent1 (cdr (assoc mapno pointslist))
															p1 (cdr (assoc 10 (entget (handent pent1))))
															entlist (entget (handent linkent))
															p2 (cdr (assoc 10 entlist))
														)
														;;孔数
														(setq holestr (nth 12 rowdata))
														(if (> (strlen holestr) 0)
															(if (setq pos (vl-string-search "/" holestr))
																(setq holecount (substr holestr 1 pos)
																	holeused (substr holestr (+ pos 2)))
															)
														)
														;;压力或电压
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
														(if (> (strlen linkpoint) 0) (setq linkpointlist (cons rowdata linkpointlist)))	;;保存整行数据
													)
												)
											
											)
										)
									)
									
									
								)
							)
							(setq row (1+ row))
						)
						;;处理未划线的连接点
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
										;;孔数
										(setq holestr (nth 12 rowdata))
										(if (> (strlen holestr) 0)
											(if (setq pos (vl-string-search "/" holestr))
												(setq holecount (substr holestr 1 pos)
													holeused (substr holestr (+ pos 1)))
											)
										)
										;;压力或电压
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
			(prompt (strcat "\n打开文件" XLSFile "失败！"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGB耗时")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "秒.")
		)
	)
	(princ)
)

;*********************************************************************************************
;函数定义:C:ImportFromCGBZY2(XLSFile)
;功能：从成果表中导入点线数据， 物探点号与测量点号一致。
;参数：XLSFile：excel xls文件
;返回：
;创建时间：2015/03/06  21：00
;修改时间：废弃
;创建人：沈雄君
;*********************************************************************************************
(defun C:ImportFromCGBZY2( / bf bs cellid emptyrowcount entlist feature featurelist featurestr 
 hent holecount holestr holeused linename1 linename2 lineslist linkent linkpoint linkpointlist
 maintype mapno mapno2 ndata outlist p1 p2 path0 pent pent1 pent2 pointslist pos pressure
 rangeid1 rangeid2 row rowdata sheet sheets subsidlist subsidstr usedrange voltage xlsfile d1 d2 
 depthstr newl cabcount flowdirect)
 
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n导入管线探测成果表,文件格式为xls.
				\n自动划分图层(根据点号前缀划分图层).")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "选择成果表" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;初始化 excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open XLSFile nil))
		(progn
			(setq sheets (vlxls-sheet-get-all *xls*)
				;;usedrange  (vlxls-sheet-get-UsedRange *xls*)
				pointslist nil	;;所有导入的点
			)
			
			(foreach sheet sheets
				(setq usedrange  (vlxls-range-getid (vlxls-sheet-get-UsedRange *xls* sheet))
					cellid (vlxls-cellid usedrange)
					rangeid1  (vlxls-rangeid (car cellid))
					rangeid2  (vlxls-rangeid (cadr cellid))
					row 3	;;行号
					linkpointlist nil
					emptyrowcount 0	;;连续空行的数量
					lineslist nil	;;暂时未加入的线段
					maintype (GetTypeFromPointName sheet)	;;！！！从表名获得类型名称
					
					featurelist ( getfeaturelist-fromtype maintype)
					subsidlist (getsubsidlist-fromtype maintype)
					gl_BlockNameList nil
					hasdata T
				)
				(if (= maintype "B");不明
					(if (and (/= sheet "B") (/= sheet "不明"))
						(setq hasdata nil)
					)
				)
				
				(if (and hasdata (vlxls-sheet-put-active *xls* sheet))
					(progn
						(while (and (< emptyrowcount 10) (<= row  (cadr rangeid2)))	;;row in the range and 连续空行小于10个
							;;读取一行
							(setq rowdata (vlxls-cell-get-value *xls*  (strcat "A"  (rtos row 2 0) ":T"  (rtos row 2 0) ))
								rowdata (car rowdata)
								ndata (length rowdata)
								mapno (car rowdata)
								linkpoint (cadr rowdata)
							)
							(if (<= (strlen mapno) 0)	
								(setq emptyrowcount (1+ emptyrowcount))	;;空行
								(progn
									;;不在列表中，则建立新点
									(if (null (assoc mapno pointslist))
										(progn	;;非空行
											;;分析数据
											(setq p1 (list (atof (nth 6 rowdata)) (atof (nth 5 rowdata)) (atof (nth 7 rowdata)))
												feature (nth 3 rowdata)
												subsid	(nth 4 rowdata)
												featurestr "非普查"
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
												(prompt (strcat "\n 配置文件中未包含特征和附属物类型:" feature))
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
									;;创建线
									(if (> (strlen linkpoint) 0)
										(progn
											(setq linename1 (strcat mapno linkpoint)
												linename2 (strcat linkpoint mapno)
											)
											;;线段还未建立
											(if (not (or (member linename1 lineslist) (member linename2 lineslist)))
												(if (setq linkent  (cdr  (assoc linkpoint pointslist)))	;;点已经在点表中
													(progn
														;;add line
														(setq pent1 (cdr (assoc mapno pointslist))
															p1 (cdr (assoc 10 (entget (handent pent1))))
															entlist (entget (handent linkent))
															p2 (cdr (assoc 10 entlist))
														)
														;;深度
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
														;;孔数
														(setq holestr (nth 13 rowdata)
															holecount ""
															holeused "")
														(if (> (strlen holestr) 0)
															(if (setq pos (vl-string-search "/" holestr))
																(setq holecount (substr holestr 1 pos)
																	holeused (substr holestr (+ pos 2)))
															)
														)
														;;压力或电压
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
															;;nth 16  18 埋设年代 和线型 未导入
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
														(if (> (strlen linkpoint) 0) (setq linkpointlist (cons rowdata linkpointlist)))	;;保存整行数据
													)
												)
											)
										)
									)
								)
							)
							(setq row (1+ row))
						)
						;;处理未划线的连接点
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
										;;深度
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
										;;孔数
										(setq holestr (nth 13 rowdata))
										(if (> (strlen holestr) 0)
											(if (setq pos (vl-string-search "/" holestr))
												(setq holecount (substr holestr 1 pos)
													holeused (substr holestr (+ pos 2)))
											)
										)
										;;压力或电压
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
			(prompt (strcat "\n打开文件" XLSFile "失败！"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGBZY耗时")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "秒.")
		)
	)
	(princ)
)
;*********************************************************************************************
;函数定义:C:ImportFromCGBZY(XLSFile)
;功能：从成果表中导入点线数据， 物探点号与测量点号一致。
;参数：XLSFile：excel xls文件
;返回：
;创建时间：2015/03/06  21：00
;修改时间：2015/10/29  21：00
;创建人：沈雄君
;*********************************************************************************************
(defun C:ImportFromCGBZY( / bf bs cellid emptyrowcount entlist feature featurelist featurestr 
 hent holecount holestr holeused linename1 linename2 lineslist linkent linkpoint linkpointlist
 maintype mapno mapno2 ndata outlist p1 p2 path0 pent pent1 pent2 pointslist pos pressure
 rangeid1 rangeid2 row rowdata sheet sheets subsidlist subsidstr usedrange voltage xlsfile d1 d2 
 depthstr newl cabcount flowdirect)
 
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n导入管线探测成果表,文件格式为xls.
				\n自动划分图层(根据点号前缀划分图层).")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "选择成果表" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;初始化 excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open XLSFile nil))
		(progn
			(setq sheets (vlxls-sheet-get-all *xls*)
				;;usedrange  (vlxls-sheet-get-UsedRange *xls*)
				pointslist nil	;;所有导入的点
				lineslist nil	;;暂时未加入的线段
				linkpointlist nil
			)
			
			(foreach sheet sheets
				(setq usedrange  (vlxls-range-getid (vlxls-sheet-get-UsedRange *xls* sheet))
					cellid (vlxls-cellid usedrange)
					rangeid1  (vlxls-rangeid (car cellid))
					rangeid2  (vlxls-rangeid (cadr cellid))
					row 3	;;行号
					emptyrowcount 0	;;连续空行的数量
					maintype (GetTypeFromPointName sheet)	;;！！！从表名获得类型名称
					
					featurelist ( getfeaturelist-fromtype maintype)
					subsidlist (getsubsidlist-fromtype maintype)
					gl_BlockNameList nil
					hasdata T
				)
				(if (= maintype "B");不明
					(if (and (/= sheet "B") (/= sheet "不明"))
						(setq hasdata nil)
					)
				)
				
				(if (and hasdata (vlxls-sheet-put-active *xls* sheet))
					(progn
						(while (and (< emptyrowcount 10) (<= row  (cadr rangeid2)))	;;row in the range and 连续空行小于10个
							;;读取一行
							(setq rowdata (vlxls-cell-get-value *xls*  (strcat "A"  (rtos row 2 0) ":T"  (rtos row 2 0) ))
								rowdata (car rowdata)
								ndata (length rowdata)
								mapno (car rowdata)
								linkpoint (cadr rowdata)
							)
							(if (<= (strlen mapno) 0)	
								(setq emptyrowcount (1+ emptyrowcount))	;;空行
								(progn
									;;不在列表中，则建立新点
									(if (null (assoc mapno pointslist))
										(progn	;;非空行
											;;分析数据
											(setq p1 (list (atof (nth 6 rowdata)) (atof (nth 5 rowdata)) (atof (nth 7 rowdata)))
												feature (nth 3 rowdata)
												subsid	(nth 4 rowdata)
												featurestr "非普查"
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
												(prompt (strcat "\n 配置文件中未包含特征和附属物类型:" feature))
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
									;;把线段加入列表
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
			;;绘制全部线段
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
						;;深度
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
						;;孔数
						(setq holestr (nth 13 rowdata))
						(if (> (strlen holestr) 0)
							(if (setq pos (vl-string-search "/" holestr))
								(setq holecount (substr holestr 1 pos)
									holeused (substr holestr (+ pos 2)))
							)
						)
						;;压力或电压
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
			(prompt (strcat "\n打开文件" XLSFile "失败！"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGBZY耗时")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "秒.")
		)
	)
	(princ)
)
;*********************************************************************************************
;函数定义:C:ImportFromTable(XLSFile)
;功能：根据字段配对表导入点线数据。
;参数：
;返回：
;创建时间：2015/03/17  21：33
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:ImportFromTable()
	(setq gl_Time (getvar "TDUSRTIMER"))
	
	(prompt "\n根据数据库配置文件导入数据库中的点和线,配置文件格式为xls,数据库为access.
				\n")
	(setq path0 (vla-get-path (vla-get-Activedocument (vlax-get-acad-object)))
		path0 (strcat path0 "*.xls")
	)
	(if (null (setq XLSFile  (getfiled "选择数据库配置文件" path0 "xls" 4)))
		(exit)
	)
	(setq
		sheets nil ;;all sheets
		*xls* nil 	;;
		)
	;;初始化 excel
	(vlxls-app-init)

	;;open file 
	(if (setq  *xls* (vlxls-app-open XLSFile nil))
		(progn
			(setq asheet (vlxls-Sheet-Get-Active *xls*)
				usedrange  (vlxls-range-getid (vlxls-sheet-get-UsedRange *xls* asheet))
				cellid (vlxls-cellid usedrange)
				rangeid1  (vlxls-rangeid (car cellid))
				rangeid2  (vlxls-rangeid (cadr cellid))
				row 0	;;行号
				info ())
			(vlxls-app-quit *xls* nil)
		)
		;;else
		(progn
			(prompt (strcat "\n打开文件" XLSFile "失败！"))
			(exit)
		)
	)
	
	(if gl_DEBUG
		(progn
			(princ "\nC:ImportFromCGB耗时")
			(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
			(princ "秒.")
		)
	)
	(princ)
)







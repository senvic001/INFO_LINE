;;;用户与项目管理
;;当前用户
(setq gl_Me_ID nil
	gl_Me_Name nil)

;;功能：用户登录，并修改登录次数
;;参数：注册名，密码
;;返回：nil or T
(defun login (regname pwd / cstr cobj sqlstr n blogin data col)
	(setq blogin nil)
	(if	(setq cstr (mysql-ConnectString nil nil nil nil))
		(if (setq cobj (mysql-GetConnectionObject cstr))
			(progn
				(setq sqlstr (strcat "SELECT * FROM users WHERE regname='" 
							 regname "'"
							 " AND pwd=MD5('" pwd "')"
						 ) ;_ end_strcat
				) ;_ end_setq
				(if (setq n (ADOLISP_DoSQL cobj sqlstr))
					(progn
						(if gl_DEBUG (princ n))
						(setq col (car n)
							data (cadr n)
							gl_Me_ID (car n)
							gl_Me_Name (cadr n)
						)
						(princ (strcat "\n登录成功！\n欢迎" regname "归来！"))
						(setq blogin T)
						;;登录次数+1
						(setq sqlstr (strcat "UPDATE users SET nlogin=nlogin+1,lastlogintime=NOW() WHERE regname='" regname "'" ))
						(if (not  (ADOLISP_DoSQL cobj sqlstr))
							(ADOLISP_ErrorPrinter)
						)
					) ;_ end_progn
					(ADOLISP_ErrorPrinter)
				) ;_ end_if
			) ;_ end_progn
		) ;_ end_if
    ) ;_ end_if
	(if cobj (ADOLISP_DisconnectFromDB cobj))
    blogin
) ;_ end_defun

;;功能：保存用户信息到文件
;;参数：
;;返回：
(defun  SaveMe()
	(prin1)
)
;;功能：保存用户信息到文件
;;参数：注册名，密码
;;返回：nil or T
(defun  GetMe()
	(prin1)
)
;;功能：增加一个用户，昵称为nil时，注册名与昵称相同
;;参数：注册名，密码，昵称
;;返回：nil or T
(defun AddUser (regname pwd nickname / sqlstr cstr cobj n badd)
	(setq badd nil)
    (if	(setq cstr (mysql-ConnectString nil nil nil nil))
		(if (setq cobj (mysql-GetConnectionObject cstr))
			(progn
				(setq sqlstr (strcat "SELECT COUNT(Id) FROM users WHERE regname='"
							 regname
							 "'"
					) ;_ end_strcat
				) ;_ end_setq
				(if (setq n (ADOLISP_DoSQL cobj sqlstr))
					(progn
					(setq n (car (cadr n)))
						(if (> n 0) ; same name
							(alert (strcat "用户" regname "已存在！"))
							(progn
								(if (not nickname)
									(setq nickname regname)
								) ;_ end_if
								(setq sqlstr (strcat "INSERT INTO users (regname,pwd,nickname,firstlogintime) VALUES ('" 
														regname "',MD5(" pwd "),'" nickname "',NOW())") ;_ end_strcat
								) ;_ end_setq
								(if (setq n (ADOLISP_DoSQL cobj sqlstr))
									(progn
										(setq badd T)
										(princ (strcat "\n成功添加用户" regname "."))
									)
									(ADOLISP_ErrorPrinter)
								) ;_ end_if
							) ;_ end_progn
						) ;_ end_if
					) ;_ end_progn
					(ADOLISP_ErrorPrinter)
				) ;_ end_if
			) ;_ end_progn
		) ;_ end_if
    ) ;_ end_if
	(if cobj(ADOLISP_DisconnectFromDB cobj))
    badd
) ;_ end_defun

;;功能：删除一个用户
;;参数：注册名
;;返回：nil or T
(defun DelUser (regname / cstr cobj sqlstr n bdel)
	(setq bdel nil)
	(if	(setq cstr (mysql-ConnectString nil nil nil nil))
		(if (setq cobj (mysql-GetConnectionObject cstr))
			(progn
				(setq sqlstr (strcat "DELETE FROM users WHERE regname='" 
							 regname
							 "'"
						 ) ;_ end_strcat
				) ;_ end_setq
				(if (setq n (ADOLISP_DoSQL cobj sqlstr))
					(progn
						(if gl_DEBUG (princ n))
						(setq bdel T)
						(princ (strcat "\n成功删除用户" regname))
					) ;_ end_progn
					(ADOLISP_ErrorPrinter)
				) ;_ end_if
			) ;_ end_progn
		) ;_ end_if
    ) ;_ end_if
	(if cobj (ADOLISP_DisconnectFromDB cobj))
    bdel
)

; (defun IsUser (regname pwd)
	; (princ)
; )
; (defun IsLogin (regname)
	; (princ)
; )

;;功能：修改密码
;;参数：注册名，旧密码，新密码
;;返回：nil or T
(defun ModifyPwd (regname oldpwd newpwd / cstr cobj sqlstr n bmodify)
	(setq bmodify nil)
	(if	(setq cstr (mysql-ConnectString nil nil nil nil))
		(if (setq cobj (mysql-GetConnectionObject cstr))
			(progn
				;;首先判断
				(setq sqlstr (strcat "SELECT COUNT(regname) FROM users WHERE regname='" 
							 regname "'"
							 " AND pwd=MD5('" oldpwd "')"
						 ) ;_ end_strcat
				) ;_ end_setq
				(if (setq n (ADOLISP_DoSQL cobj sqlstr))
					(progn
						(setq n (car (cadr n)));;COUNT(regname)
						(if (> n 0)
							(progn
								(setq sqlstr (strcat "UPDATE users SET pwd=MD5('" newpwd "') WHERE regname='" regname "'"))
								(if (setq n (ADOLISP_DoSQL cobj sqlstr))
									(progn
										(princ (strcat "\n修改密码成功！"))
										(setq bmodify T)
									) ;_ end_progn
									(ADOLISP_ErrorPrinter)
								) ;_ end_if
							)
						)
					)
				)	
			) ;_ end_progn
		) ;_ end_if
    ) ;_ end_if
	(if cobj (ADOLISP_DisconnectFromDB cobj))
    bmodify
)

(defun GetPorjects (regname)
	(princ)
)

;;功能：创建项目。项目名称不唯一，但每个用户自己的项目名称必须唯一。
;;参数：项目名称，用户id（整形字符串）
;;返回：nil or T
(defun AddPrj (prjname username / sqlstr sqlstr2 cstr cobj data data2 badd id rolesID userid)
	(setq badd nil)
    (if	(setq cstr (mysql-ConnectString nil nil nil nil))
		(if (setq cobj (mysql-GetConnectionObject cstr))
			(progn
				;;1用户是否存在
				(setq sqlstr2 (strcat "SELECT Id FROM users WHERE regname='" username "'" )
					userid nil)
				(if (setq data2 (ADOLISP_DoSQL cobj sqlstr2))
					(progn
						(setq userid (rtos (car (cadr data2)) 2 0))
						(setq sqlstr (strcat "SELECT COUNT(Id) FROM projects WHERE name='"
							 prjname "' AND creatorID=" userid ))
						(if (setq data (ADOLISP_DoSQL cobj sqlstr))
							(progn
								(setq data (car (cadr data)))
								(if (> data 0) ; same name
									(alert (strcat "项目名称" prjname "已存在！"))
									(progn
										(setq sqlstr (strcat "INSERT INTO projects (name,creatorID,createtime) VALUES ('" 
																prjname "'," userid ",NOW())") ;_ end_strcat
										) ;_ end_setq
										(if (setq data (ADOLISP_DoSQL cobj sqlstr))
											(progn
												(setq badd T)
												(princ (strcat "\n成功添加项目" prjname "."))
												;;加入到关联表
												(setq sqlstr (strcat "SELECT Id FROM projects WHERE name='" prjname "' AND creatorID=" userid))
												(if (setq id (ADOLISP_DoSQL cobj sqlstr))
													(progn
														(setq id (rtos (car (cadr id)) 2 0)
															rolesID "4" ;;项目负责人
															sqlstr (strcat "INSERT INTO userprj (userID,prjID,rolesID) VALUES (" userid  "," id "," rolesID ")")
														)
														(if (not (ADOLISP_DoSQL cobj sqlstr))
															(ADOLISP_ErrorPrinter)
														)
													)
													((ADOLISP_ErrorPrinter))
												)
											)
											(ADOLISP_ErrorPrinter)
										) ;_ end_if
									) ;_ end_progn
								) ;_ end_if
							) ;_ end_progn
							(ADOLISP_ErrorPrinter)
						) 
					)
				)
			) ;_ end_progn
		) ;_ end_if
    ) ;_ end_if
	(if cobj(ADOLISP_DisconnectFromDB cobj))
    badd
)

(defun DelPrj (prjname unit)
	(princ)
)

(defun ModifyPrj (prjname unit)
	(princ)
)

(defun AddUsertoPrj (regname)
	(princ)
)
;;鏌ユ壘鍚屼簨锛屽崟浣嶅悕绉扮浉鍚?
(defun SearchMates (unit)
	(princ)
)
;;鍚屼竴涓」鐩殑浼欎即
(defun GetMates (prjname)
	(princ)
)
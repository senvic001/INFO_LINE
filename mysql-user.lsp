;;;�û�����Ŀ����
;;��ǰ�û�
(setq gl_Me_ID nil
	gl_Me_Name nil)

;;���ܣ��û���¼�����޸ĵ�¼����
;;������ע����������
;;���أ�nil or T
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
						(princ (strcat "\n��¼�ɹ���\n��ӭ" regname "������"))
						(setq blogin T)
						;;��¼����+1
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

;;���ܣ������û���Ϣ���ļ�
;;������
;;���أ�
(defun  SaveMe()
	(prin1)
)
;;���ܣ������û���Ϣ���ļ�
;;������ע����������
;;���أ�nil or T
(defun  GetMe()
	(prin1)
)
;;���ܣ�����һ���û����ǳ�Ϊnilʱ��ע�������ǳ���ͬ
;;������ע���������룬�ǳ�
;;���أ�nil or T
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
							(alert (strcat "�û�" regname "�Ѵ��ڣ�"))
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
										(princ (strcat "\n�ɹ�����û�" regname "."))
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

;;���ܣ�ɾ��һ���û�
;;������ע����
;;���أ�nil or T
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
						(princ (strcat "\n�ɹ�ɾ���û�" regname))
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

;;���ܣ��޸�����
;;������ע�����������룬������
;;���أ�nil or T
(defun ModifyPwd (regname oldpwd newpwd / cstr cobj sqlstr n bmodify)
	(setq bmodify nil)
	(if	(setq cstr (mysql-ConnectString nil nil nil nil))
		(if (setq cobj (mysql-GetConnectionObject cstr))
			(progn
				;;�����ж�
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
										(princ (strcat "\n�޸�����ɹ���"))
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

;;���ܣ�������Ŀ����Ŀ���Ʋ�Ψһ����ÿ���û��Լ�����Ŀ���Ʊ���Ψһ��
;;��������Ŀ���ƣ��û�id�������ַ�����
;;���أ�nil or T
(defun AddPrj (prjname username / sqlstr sqlstr2 cstr cobj data data2 badd id rolesID userid)
	(setq badd nil)
    (if	(setq cstr (mysql-ConnectString nil nil nil nil))
		(if (setq cobj (mysql-GetConnectionObject cstr))
			(progn
				;;1�û��Ƿ����
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
									(alert (strcat "��Ŀ����" prjname "�Ѵ��ڣ�"))
									(progn
										(setq sqlstr (strcat "INSERT INTO projects (name,creatorID,createtime) VALUES ('" 
																prjname "'," userid ",NOW())") ;_ end_strcat
										) ;_ end_setq
										(if (setq data (ADOLISP_DoSQL cobj sqlstr))
											(progn
												(setq badd T)
												(princ (strcat "\n�ɹ������Ŀ" prjname "."))
												;;���뵽������
												(setq sqlstr (strcat "SELECT Id FROM projects WHERE name='" prjname "' AND creatorID=" userid))
												(if (setq id (ADOLISP_DoSQL cobj sqlstr))
													(progn
														(setq id (rtos (car (cadr id)) 2 0)
															rolesID "4" ;;��Ŀ������
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
;;查找同事，单位名称相�?
(defun SearchMates (unit)
	(princ)
)
;;同一个项目的伙伴
(defun GetMates (prjname)
	(princ)
)
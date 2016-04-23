;;;LINE_INFO Menu
;;;�ο�AY�Ĵ���

;;;���ز˵������Ѳ˵��ļ�·����ӵ�����·����
;;;��Ϊϵͳ·��������ʼ�������ȫ�ֱ���

;;;******************
;;; No.1 ���ز˵�    
;;;******************

(defun LineInfo_loadmenu  (/ Menufilename i searchpath)
    (setvar "cmdecho" 0)
    (if (null (menugroup "LINE_INFO")) ;autoCAD��δ���ظ��˵���.
        (progn
            (setq Menufilename (findfile "LINE_INFO.mnu")) ;Search from autocad setup folder.
;;;            (if (null Menufilename)
;;;                (setq Menufilename (findfile "LINE_INFO.mnu"))
;;;                ) ;if
            (if (null Menufilename)
                (setq Menufilename (getfiled "��LINE_INFO�˵��ļ�" "LINE_INFO.mnu" "mnu" 4))
                ) ;end_if
            (if (null Menufilename)
                (exit)) ;open dialog is ""Cancle" press.
		;;��·�����뵽ϵͳ����·��
            (setq searchpath (substr Menufilename
                                     1
                                     (vl-string-search "LINE_INFO.mnu" Menufilename)))
            (setq gl_INFO_LINE_PATH searchpath)
            (LineInfo_AddSupportPath
                (vl-string-right-trim "\\" searchpath))

            (LineInfo_AddSupportPath (strcat searchpath "BLOCKLIB"))
            (setq gl_BLOCKLIB_PATH (strcat searchpath "BLOCKLIB\\"))
            
            (LineInfo_AddSupportPath (strcat searchpath "DLG"))
            (setq gl_DLG_PATH (strcat searchpath "DLG\\"))
            (setq gl_PPROJECT_PATH (strcat searchpath "PROJECT\\"))

            (LineInfo_AddSupportPath (strcat searchpath "FONTS"))
            
            (setq i 1)
            (while (< i 24)
                (if (menucmd (strcat "P" (itoa i) ".1=?"))
                    (setq i (+ i 1)) ;then
                    (progn ;else
                        (vl-cmdf "_menuload" Menufilename)
                        (menucmd (strcat "P" (itoa i) "=+LINE_INFO.POP1"))
                        (princ "\n�� LINE_INFO���߲˵��Ѽ���,��ӭ��ʹ��.")
                        (princ "\nEmail:shen_xiongjun@126.com QQ:85373714")
                        (setq i 25)
                        ) ;end_progn
                    ) ;end_if
                ) ;end_while i
            ) ;end_progn
        ) ;end_if
    (prin1)
    ) ;end_defun

;;;����֧�ֿ�·��
;*********************************************************************************************
;��������:AddSupportPath()
;���ܣ���֧��·�������
;������
;���أ�
;����ʱ�䣺2014/07/10   11:10
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun LineInfo_AddSupportPath  (spath / app reffiles path mypath)
   (setq app (vlax-get-acad-object))
   (setq reffiles (vla-get-files (vla-get-Preferences app))
	 path	  (vla-get-supportpath reffiles)
	 mypath	  (vl-string-right-trim "\\" spath))
   (if (= nil (vl-string-search mypath path))
      (progn
      	(vla-put-supportpath reffiles (strcat path ";" mypath))
      	);progn
      );if
   )
(defun LineInfo_GetSupportPath  (/ app reffiles path mypath tempath pos)
    (setq app (vlax-get-acad-object))
    (setq reffiles (vla-get-files (vla-get-Preferences app))
          path     (vla-get-supportpath reffiles)
	  path	(strcase path)
          tempath  path
          )
    (while (setq pos (vl-string-search ";" tempath))
        (setq mypath  (substr tempath 1 pos)
              tempath (substr tempath (+ pos 2)))
        (if (and (vl-string-search "INFO_LINE" mypath)
		 (not (vl-string-search "BLOCKLIB" mypath))
		 (not (vl-string-search "DLG" mypath))
		 (not (vl-string-search "PROJECT" mypath))
		 (not (vl-string-search "FONTS" mypath))
		 )
            (setq gl_INFO_LINE_PATH (strcat mypath "\\")
		  gl_BLOCKLIB_PATH (strcat gl_INFO_LINE_PATH "BLOCKLIB\\")
		  gl_PPROJECT_PATH (strcat gl_INFO_LINE_PATH "PROJECT\\")
		  )
            ) ;if
        ) ;while

    (if tempath
        (if (and (vl-string-search "INFO_LINE" tempath)
		 (not (vl-string-search "BLOCKLIB" tempath))
		 (not (vl-string-search "DLG" tempath))
		 (not (vl-string-search "PROJECT" tempath))
		 (not (vl-string-search "FONTS" tempath))
		 )
            (setq gl_INFO_LINE_PATH (strcat tempath "\\")
		  gl_BLOCKLIB_PATH (strcat gl_INFO_LINE_PATH "BLOCKLIB\\")
		  gl_PPROJECT_PATH (strcat gl_INFO_LINE_PATH "PROJECT\\")
		  )
            )
        ) ;if
    ) ;defun

(defun LineInfo_DelSupportPath  ( / app reffiles path mypath tempath pos)
   (setq app (vlax-get-acad-object))
   (setq reffiles (vla-get-files (vla-get-Preferences app))
	 path	  (vla-get-supportpath reffiles)
         tempath path
         outpath ";"
	 )
    (while (setq pos (vl-string-search ";" tempath))
        (setq mypath (substr tempath 1 pos)
              tempath (substr tempath (+ pos 2)))
        (if (not (vl-string-search "INFO_LINE" mypath)) (setq outpath (strcat outpath ";" mypath)))
        );while
    (if tempath
    	(if (not (vl-string-search "INFO_LINE" tempath))
            (setq outpath (strcat outpath ";" tempath))))
    (setq outpath (vl-string-left-trim ";" outpath))
    (vla-put-supportpath reffiles outpath)
   )


;;(AddSupportPath gl_BLOCKLIB_PATH)

;;;*******************
;;; No.2 ж�ز˵�     
;;;*******************
(defun C:LineInfo_Unmenu()
	(setvar "cmdecho" 0)
	(if (= "LINE_INFO" (strcase (menugroup "LINE_INFO")))
		(progn
                    (LineInfo_DelSupportPath )
			(vl-cmdf "menuunload" "LINE_INFO")
			(princ "\nLINE_INFO �Ѿ�ж�ء�ллʹ��LINE_INFO��")
			(princ "\nEmail:shen_xiongjun@163.com QQ:85373714")
		);end_progn
	);end_if
	(prin1)
);end_defun

;;;*******************
;;; No.3 ���²˵�     
;;;*******************
(defun C:LineInfo_Updatemenu( / Menufilename)
	(setvar "cmdecho" 0)
	(if (= "LINE_INFO" (strcase (menugroup "LINE_INFO")))
		(progn
		  	
			(vl-cmdf "menuunload" "LINE_INFO")
			(princ "\nLINE_INFO �Ѿ�ж�ء�ллʹ��LINE_INFO��")
			(princ "\nEmail:shen_xiongjun@163.com QQ:85373714")
		);end_progn
	);end_if
	(LineInfo_loadmenu)
	(prin1)
);end_defun

;;�򿪰����ļ�
(defun C:Info_Line_helpdoc(/ file)
	(if (setq file (findfile "DB����˵��5.0.pdf")) 
		(startapp "EXPLORER.EXE" file)
		(alert "δ�ҵ������ļ�help.pdf!")
	)
	(princ)
)
;;;���ز˵�
(LineInfo_loadmenu)

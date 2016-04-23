;;;LINE_INFO Menu
;;;参考AY的代码

;;;加载菜单，并把菜单文件路径添加到搜索路径；
;;;作为系统路径，并初始化程序的全局变量

;;;******************
;;; No.1 加载菜单    
;;;******************

(defun LineInfo_loadmenu  (/ Menufilename i searchpath)
    (setvar "cmdecho" 0)
    (if (null (menugroup "LINE_INFO")) ;autoCAD中未加载给菜单组.
        (progn
            (setq Menufilename (findfile "LINE_INFO.mnu")) ;Search from autocad setup folder.
;;;            (if (null Menufilename)
;;;                (setq Menufilename (findfile "LINE_INFO.mnu"))
;;;                ) ;if
            (if (null Menufilename)
                (setq Menufilename (getfiled "打开LINE_INFO菜单文件" "LINE_INFO.mnu" "mnu" 4))
                ) ;end_if
            (if (null Menufilename)
                (exit)) ;open dialog is ""Cancle" press.
		;;把路径加入到系统搜索路径
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
                        (princ "\n◎ LINE_INFO工具菜单已加载,欢迎您使用.")
                        (princ "\nEmail:shen_xiongjun@126.com QQ:85373714")
                        (setq i 25)
                        ) ;end_progn
                    ) ;end_if
                ) ;end_while i
            ) ;end_progn
        ) ;end_if
    (prin1)
    ) ;end_defun

;;;加载支持库路径
;*********************************************************************************************
;函数定义:AddSupportPath()
;功能：在支持路径中添加
;参数：
;返回：
;创建时间：2014/07/10   11:10
;修改时间：
;创建人：沈雄君
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
;;; No.2 卸载菜单     
;;;*******************
(defun C:LineInfo_Unmenu()
	(setvar "cmdecho" 0)
	(if (= "LINE_INFO" (strcase (menugroup "LINE_INFO")))
		(progn
                    (LineInfo_DelSupportPath )
			(vl-cmdf "menuunload" "LINE_INFO")
			(princ "\nLINE_INFO 已经卸载。谢谢使用LINE_INFO！")
			(princ "\nEmail:shen_xiongjun@163.com QQ:85373714")
		);end_progn
	);end_if
	(prin1)
);end_defun

;;;*******************
;;; No.3 更新菜单     
;;;*******************
(defun C:LineInfo_Updatemenu( / Menufilename)
	(setvar "cmdecho" 0)
	(if (= "LINE_INFO" (strcase (menugroup "LINE_INFO")))
		(progn
		  	
			(vl-cmdf "menuunload" "LINE_INFO")
			(princ "\nLINE_INFO 已经卸载。谢谢使用LINE_INFO！")
			(princ "\nEmail:shen_xiongjun@163.com QQ:85373714")
		);end_progn
	);end_if
	(LineInfo_loadmenu)
	(prin1)
);end_defun

;;打开帮助文件
(defun C:Info_Line_helpdoc(/ file)
	(if (setq file (findfile "DB管线说明5.0.pdf")) 
		(startapp "EXPLORER.EXE" file)
		(alert "未找到帮助文件help.pdf!")
	)
	(princ)
)
;;;加载菜单
(LineInfo_loadmenu)

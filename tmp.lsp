(defun AddFont ( / app bigfile doc fontfile  mainpath mypath pretxtstyle rbig rfont ts txtstyles)
    (setq app (vlax-get-acad-object)
          Doc (vla-get-Activedocument app)
    ) ;_ end setq
    (setq txtstyles (vla-get-TextStyles Doc)
          pretxtstyle    (vla-get-ActiveTextStyle Doc)
    ) ;_ end setq

    (setq ts (vla-add txtstyles "DB_DXT") ;管线宋体等线体
    ) ;_ end_setq

    (setq mainpath (strcat (vl-filename-directory (findfile "acad.exe"))
                           "\\Fonts\\"
                   ) ;_ end_strcat
          mypath   (strcat gl_INFO_LINE_PATH "fonts\\")
    ) ;_ end_setq
    (setq fontfile (findfile (strcat mainpath "simtxt.shx"))
          bigfile  (findfile (strcat mainpath "hztxt.shx"))
    ) ;_ end_setq
    ;;
    (setq rfont 1
          rbig 1
    ) ;_ end_setq
    ;;复制到系统fonts目录
    (if (null fontfile)
        (setq rfont (vl-file-copy (strcat mypath "simtxt.shx")
                                  (strcat mainpath "simtxt.shx")
                    ) ;_ end_vl-file-copy
        ) ;_ end_setq
    ) ;_ end_if
    (if (null bigfile)
        (setq rbig (vl-file-copy (strcat mypath "hztxt.shx")
                                 (strcat mainpath "hztxt.shx")
                   ) ;_ end_vl-file-copy
        ) ;_ end_setq
    ) ;_ end_if
    ;;
    (if (and rfont rbig)
        (progn
            (vla-put-fontfile ts (strcat mainpath "simtxt.shx"))
            (vla-put-bigfontfile ts (strcat mainpath "hztxt.shx"))
            (vla-put-activetextstyle doc ts)
        ) ;_ end_progn
        (setq ts nil)
    ) ;_ end_if
ts
)
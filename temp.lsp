野外测量的DAT文件，从仪器中导出后，需要在未安装CASS的电脑上展点。找到网上前辈的帖子，如下，但是出现问题，参数类型错误。求高人指点迷津：
;;CAD读取坐标文件展点程序
(defun c:zhan_datedh ()
  ;;LISP展碎部点程序(数据添加日期属性)
  ;;坐标数据文件格式为四种
  ;;格式1:点号[任意空格]纵坐标X[任意空格]横坐标Y[任意空格]高程H[任意空格]点码
  ;;格式2:点号[,]点码[,]横坐标Y[,]纵坐标X[,]高程H
  ;;格式3:点号[任意空格]纵坐标X[任意空格]横坐标Y[任意空格]高程H
  ;;格式4:点号[,]纵坐标X[,]横坐标Y[,]高程H
  ;;(c:zhan_datedh)
  ;;保留小数位数不截零 
  (initget 1 "1 2")
  (setq zhandian_lx (getkword "\n 1：展点号 /2：展高程 "))
  (setq doslibfilename (doslib_setup))
  (if (= doslibfilename nil)
    (progn (alert (strcat "\n请确认在CAD支持的路径下存在名称为["
                          doslibfilename
                          "]的ARX程序或CAD的版本低于R15!"
                  )
           )
           (exit)
    )
  )
  (if (/= (getvar "pdmode") 33)
    (progn (setvar "pdmode" 33) (setvar "pdsize" 0.15))
  )
  (setvar "DIMZIN" 0)
  (dingblc)                                ;定义图形比例尺及参数置 
  (setq        fhb           nil
        t0           (getvar "cdate")
        cm           (getvar "cmdecho")
        os           (getvar "osmode")
        dtextgao   (* stsca 0.5)
        dtextcolor (+ (atoi (substr (rtos t0 2 0) 3 2))
                      (atoi (substr (rtos t0 2 0) 5 2))
                      (atoi (substr (rtos t0 2 0) 7 2))
                   )
        dtext_date (strcat (substr (rtos t0 2 0) 1 4)
                           "-"
                           (substr (rtos t0 2 0) 5 2)
                           "-"
                           (substr (rtos t0 2 0) 7 2)
                   )
        gcd_lay           "GCD"
        gcd_thk           1610000.0
        gcd_zigao  (* stsca 2.0)
        gcd_xsws   2
  )
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (cond        ((= zhandian_lx "1")
         (progn        (setq zdh_layer        (dos_combolist
                                  "展点图层设置对话框"
                                  "请选择展点的图层"
                                  '("ZDH" "TCJC")
                                )
                )
                (if (= zdh_layer nil)
                  (setq zdh_layer "ZDH")
                )
         )
        )
        ((= zhandian_lx "2") (setq zdh_layer "ZDH"))
        (t nil)
  )
  (setq        zdhpeizhi (list        (cons "展点图层" zdh_layer)
                        (cons "点号注记字高" (rtos dtextgao 2 2))
                        (cons "颜色设置" (itoa dtextcolor))
                        (cons "展点标注日期" dtext_date)
                        (cons "高程点图层" gcd_lay)
                        (cons "高程点厚度值" (rtos gcd_thk 2 0))
                        (cons "高程点字高" (rtos gcd_zigao 2 2))
                        (cons "高程注记小数位数" (itoa gcd_xsws))
                  )
  )
  (setq        zdhpeizhi (dos_proplist
                    "展点配置对话框"
                    "请核实以下展点配置"
                    zdhpeizhi
                  )
  )
  (while (or (= (distof (cdr (nth 1 zdhpeizhi))) nil)
             (= (distof (cdr (nth 2 zdhpeizhi))) nil)
         )
    (cond
      ((= (distof (cdr (nth 1 zdhpeizhi))) nil)
       (alert "点号注记字高应为数值,请核实!")
      )
      ((= (distof (cdr (nth 2 zdhpeizhi))) nil)
       (alert "颜色设置应为数值,请核实!")
      )
      ((= (distof (cdr (nth 5 zdhpeizhi))) nil)
       (alert "高程点厚度值应为数值,请核实!")
      )
      ((= (distof (cdr (nth 6 zdhpeizhi))) nil)
       (alert "高程注记字高应为数值,请核实!")
      )
      (t nil)
    )
    (setq zdhpeizhi (list (cons "展点图层" zdh_layer)
                          (cons "点号注记字高" (rtos dtextgao 2 2))
                          (cons "颜色设置" (itoa dtextcolor))
                          (cons "展点标注日期" dtext_date)
                          (cons "高程点图层" gcd_lay)
                          (cons "高程点厚度值" (rtos gcd_thk 2 0))
                          (cons "高程点字高" (rtos gcd_zigao 2 2))
                          (cons "高程注记小数位数" (itoa gcd_xsws))
                    )
    )
    (setq zdhpeizhi (dos_proplist
                      "展点配置对话框"
                      "请核实以下展点配置"
                      zdhpeizhi
                    )
    )
  )
  (if (/= zdhpeizhi nil)
    (progn
      (setq zdh_layer  (cdr (nth 0 zdhpeizhi))
            dtextgao   (distof (cdr (nth 1 zdhpeizhi)))
            dtextcolor (fix (distof (cdr (nth 2 zdhpeizhi))))
            dtext_date (cdr (nth 3 zdhpeizhi))
            gcd_lay    (cdr (nth 4 zdhpeizhi))
            gcd_thk    (atof (cdr (nth 5 zdhpeizhi)))
            gcd_zigao  (distof (cdr (nth 6 zdhpeizhi)))
            gcd_xsws   (fix (distof (cdr (nth 7 zdhpeizhi))))
      )
      (if (or (< dtextcolor 0) (> dtextcolor 255))
        (setq dtextcolor
               (+ (atoi (substr (rtos t0 2 0) 3 2))
                  (atoi (substr (rtos t0 2 0) 5 2))
                  (atoi (substr (rtos t0 2 0) 7 2))
               )
        )
      )
      (cond ((= zhandian_lx "1") (jcszlayer zdh_layer nil))
            ((= zhandian_lx "2") (jcszlayer gcd_lay nil))
            (t nil)
      )
      (setq zdshujugs
             (dos_listbox
               "展点数据格式选择"
               "请选择一种坐标文件数据格式"
               '("空格分隔坐标格式(点号 纵坐标X 横坐标Y 高程H 点码)"
                 "CASS坐标格式(点号,点码,横坐标Y,纵坐标X,高程H)"
                 "标准坐标格式(点号 横坐标Y 纵坐标X 高程H)"
                 "标准坐标格式(点号,横坐标Y,纵坐标X,高程H)"
                )
             )
      )
      (cond
        ((= zdshujugs
            "空格分隔坐标格式(点号 纵坐标X 横坐标Y 高程H 点码)"
         )
         (setq zbgs 1)
        )
        ((= zdshujugs
            "CASS坐标格式(点号,点码,横坐标Y,纵坐标X,高程H)"
         )
         (setq zbgs 2)
        )
        ((= zdshujugs "标准坐标格式(点号 横坐标Y 纵坐标X 高程H)")
         (setq zbgs 3)
        )
        ((= zdshujugs "标准坐标格式(点号,横坐标Y,纵坐标X,高程H)")
         (setq zbgs 4)
        )
        (t (progn (alert "展点数据文件格式非标准!") (exit)))
      )
      (if (= (type filepath) 'STR)
        (progn
          (cond        ((and (= (substr filepath (strlen filepath)) "\\")
                      (= (substr filepath 2 2) ":\\")
                 )
                 (setq filepath1 filepath)
                )
                (t
                 (setq filepath1 (getvar "TEMPPREFIX")
                       filepath         filepath1
                 )
                )
          )
        )
        (setq filepath1        (getvar "TEMPPREFIX")
              filepath        filepath1
        )
      )
      (setq file (dos_getfiled
                   "请选择展点坐标文件"
                   filepath1
                   "文本文件 (*.dat)|*.dat|All files (*.*)|*.*||"
                 )
      )
      (if (/= file nil)
        (progn
          (setq filepath file)
          (while (/= (substr filepath (strlen filepath)) "\\")
            (setq filepath (substr filepath 1 (1- (strlen filepath))))
          )
        )
        (exit)
      )
      (cond ;;读取雷生碎部点格式文件
            ((= zbgs 1) (setq zdsj_list (read_lssbdcgb file)))
            ;;读取CASS格式坐标文件
            ((= zbgs 2) (setq zdsj_list (read_casscgb file)))
            ;;读取标准坐标文件(空格分隔)
            ((= zbgs 3) (setq zdsj_list (read_biaozhuncgb1 file)))
            ;;读取标准坐标文件(逗号分隔)
            ((= zbgs 4) (setq zdsj_list (read_biaozhuncgb2 file)))
            (t (progn (alert "数据非法") (exit)))
      )
      (setq t1 (getvar "cdate"))
      ;;开始展点
      (cond ;;展点号
            ((= zhandian_lx "1")
             (progn (regapp "ZD_Date")
                    (setq pointlay   zdh_layer
                          pointthk   0.0
                          pointcolor dtextcolor
                          textlay    zdh_layer
                          textzg     dtextgao
                          textcolor  dtextcolor
                          textdmh    2
                    )
             )
            )
            ;;展高程
            ((= zhandian_lx "2")
             (progn (setq pointlay gcd_lay
                          pointthk gcd_thk
                          pointcolor
                           nil
                          textlay gcd_lay
                          textzg gcd_zigao
                          textcolor nil
                          textdmh 2
                    )
             )
            )
            (t nil)
      )
      (setq zbzd_i    0
            textthk   0.0
            textro    0.0
            textkuan  1.0
            textlcr   0
            textstyle "STANDARD"
      )
      (while (< zbzd_i (length zdsj_list))
        (setq pointpt (nth 1 (nth zbzd_i zdsj_list)))
        (cond ;;展点号
              ((= zhandian_lx "1")
               (setq textnr         (nth 0 (nth zbzd_i zdsj_list))
                     pointkzsx         (list
                                   (cons
                                     '"ZD_Date"
                                     (list
                                       (cons '1000 (strcat dtext_date "/" textnr))
                                     )
                                   )
                                 )
                     zhandian_yn T
               )
              )
              ;;展高程
              ((= zhandian_lx "2")
               (progn (setq pointkzsx nil
                            textnr    (rtos (nth 2 pointpt) 2 gcd_xsws)
                      )
                      (if (= (nth 2 pointpt) 0.0)
                        (setq zhandian_yn nil)
                        (setq zhandian_yn T)
                      )
               )
              )
              (t nil)
        )
        (if (= zhandian_yn T)
          (progn (setq
                   pointst (emakepoint
                             pointlay pointthk pointpt pointcolor
                             pointkzsx)
                 )
                 (setq textpt10        (mapcar '+ pointpt (list (* 0.5 stsca) 0.0 0.0))
                       textpt11        textpt10
                 )
                 (cond ;;展点号
                       ((= zhandian_lx "1")
                        (setq dtext_bj (strcat dtext_date
                                               "/"
                                               (rtos (nth 0 pointpt) 2 3)
                                               "#"
                                               (rtos (nth 1 pointpt) 2 3)
                                               "#"
                                               (rtos (nth 2 pointpt) 2 3)
                                       )
                              textkzsx (list (cons '"ZD_Date"
                                                   (list (cons '1000 dtext_bj))
                                             )
                                       )
                        )
                       )
                       ;;展高程
                       ((= zhandian_lx "2") (setq textkzsx nil))
                       (t nil)
                 )
                 (setq textst (emaketext textlay     textnr
                                         textthk     textpt10
                                         textzg             textro
                                         textkuan    textqxie
                                         textcolor   textstyle
                                         textlcr     textdmh
                                         textpt11    textkzsx
                                        )
                 )
          )
        )
        (setq zbzd_i (1+ zbzd_i))
        (printbar_jd "正在展绘碎部点" (length zdsj_list) zbzd_i)
      )
      (Example_ZoomExtents)
      (setq t2 (getvar "cdate"))
    )
  )
  ;|(setq t3 (getvar "cdate")
       dt1 (* 1000000 (- t1 t0))
       dt2 (* 1000000 (- t3 t2)))
(alert (strcat "\n读入数据共耗时："
   (rtos dt1 2 3)
   "秒"
   "\n展点共耗时"
   (rtos dt2 2 3)
   "秒"
   "\n展点数："
   (itoa (length zdsj_list))
   "个"
   "\n 每展一点耗时："
   (rtos (/ dt2 (length zdsj_list)) 2 10)
   "秒"))|;
  (setvar "cmdecho" cm)
  (setvar "osmode" os)
  (princ)
)
;;-----------------------------------------------------------
(defun dingblc ()
  ;;(dingblc)
  ;;定义图形比例尺及参数设置
  ;; (getvar "USERR1")--图形比例尺系统变量 
  ;; (getvar"LTSCALE")--线型比例系数
  (while (<= (getvar "USERR1") 0.0)
    (progn (initget 4)
           (setq kah (getreal "\n请输入测图比例尺<500>:"))
           (if (= kah nil)
             (setq kah 500.0)
           )
           (setvar "userr1" kah)
           (setvar "ltscale" (/ kah 1000.0))
    )
    (setvar "ltscale" (/ (getvar "USERR1") 1000.0))
  )
  (setq stsca (getvar "ltscale"))
  (cond        ;;1:500
        ((= (getvar "userr1") 500.0) (setq dtblc 1))
        ;;1:1000
        ((= (getvar "userr1") 1000.0) (setq dtblc 2))
        ;;1:2000
        ((= (getvar "userr1") 2000.0) (setq dtblc 3))
        ;;1:5000
        ((= (getvar "userr1") 5000.0) (setq dtblc 4))
        ;;任意比例尺
        (t (setq dtblc 10))
  )
)
;;-----------------------------------------------------------
(defun read_biaozhuncgb1 (biaozhuncgb)
  ;;读取标准坐标文件(空格分隔)
  ;;(read_biaozhuncgb1 biaozhuncgb)
  ;;"标准坐标格式(点号 横坐标Y 纵坐标X 高程H)"
  (setq zhandian_list nil)
  (setq ls_file (open casscgbfile "r"))
  (setq p (read-line ls_file))
  (while (/= p nil)
    (progn (while (= (substr p 1 1) " ") (setq p (substr p 2)))
           (while (= (substr p (strlen p)) " ")
             (setq p (substr p 1 (1- (strlen p))))
           )
           (while (/= (strlen p) (strlen (vl-string-subst " " " " p)))
             (setq p (vl-string-subst " " " " p))
           )
           (setq ppp (string_strlist1 p " "))
           (if (and (= (length ppp) 4)
                    (/= (distof (nth 1 ppp)) nil)
                    (/= (distof (nth 2 ppp)) nil)
                    (/= (distof (nth 3 ppp)) nil)
               )
             (progn (setq pp (nth 0 ppp)
                          px (atof (nth 1 ppp))
                          py (atof (nth 2 ppp))
                          ph (atof (nth 3 ppp))
                    )
                    (setq pt (list px py ph))
                    (setq zhandian_list
                           (append zhandian_list (list (list pp pt)))
                    )
             )
           )
           (setq p (read-line ls_file))
    )
  )
  (close ls_file)
  zhandian_list
)
;;--------------------------------------------------
(defun read_biaozhuncgb2 (biaozhuncgb)
  ;;读取标准坐标文件(逗号分隔)
  ;;(read_biaozhuncgb2 biaozhuncgb)
  ;;"标准坐标格式(点号,横坐标Y,纵坐标X,高程H)"
  (setq zhandian_list nil)
  (setq ls_file (open casscgbfile "r"))
  (setq p (read-line ls_file))
  (while (/= p nil)
    (progn
      (setq ppp (string_strlist1 p ","))
      (if (and (= (length ppp) 4)
               (/= (distof (nth 1 ppp)) nil)
               (/= (distof (nth 2 ppp)) nil)
               (/= (distof (nth 3 ppp)) nil)
          )
        (progn (setq pp (nth 0 ppp))
               (while (= (substr pp 1 1) " ") (setq pp (substr pp 2)))
               (while (= (substr pp (strlen pp)) " ")
                 (setq pp (substr pp 1 (1- (strlen pp))))
               )
               (setq px        (atof (nth 1 ppp))
                     py        (atof (nth 2 ppp))
                     ph        (atof (nth 3 ppp))
               )
               (setq pt (list px py ph))
               (setq zhandian_list
                      (append zhandian_list (list (list pp pt)))
               )
        )
      )
      (setq p (read-line ls_file))
    )
  )
  (close ls_file)
  zhandian_list
)
;;--------------------------------------------------
(defun read_casscgb (casscgbfile / p pp ppp)
  ;;读取CASS格式坐标文件
  ;;(read_casscgb casscgbfile)
  ;;"CASS坐标格式(点号,点码,横坐标Y,纵坐标X,高程H)"
  (setq zhandian_list nil)
  (setq ls_file (open casscgbfile "r"))
  (setq p (read-line ls_file))
  (while (/= p nil)
    (progn
      (setq ppp (string_strlist1 p ","))
      (if (and (= (length ppp) 5)
               (/= (distof (nth 2 ppp)) nil)
               (/= (distof (nth 3 ppp)) nil)
               (/= (distof (nth 4 ppp)) nil)
          )
        (progn (setq pp (nth 0 ppp))
               (while (= (substr pp 1 1) " ") (setq pp (substr pp 2)))
               (while (= (substr pp (strlen pp)) " ")
                 (setq pp (substr pp 1 (1- (strlen pp))))
               )
               (setq pcode (nth 1 ppp)
                     px           (atof (nth 2 ppp))
                     py           (atof (nth 3 ppp))
                     ph           (atof (nth 4 ppp))
               )
               (setq pt (list px py ph))
               (setq zhandian_list
                      (append zhandian_list (list (list pp pt)))
               )
        )
      )
      (setq p (read-line ls_file))
    )
  )
  (close ls_file)
  zhandian_list
)
;;--------------------------------------------------
(defun read_lssbdcgb (sbdcgbfile / p pp)
  ;;读取雷生碎部点成果表转换为展点格式列表
  ;;(read_lssbdcgb sbdcgbfile)
  ;;"雷生坐标格式(点号 纵坐标X 横坐标Y 高程H 点码)"
  ;;sbdcgbfile--雷生格式碎部点成果表包含路径的完整文件名
  (setq zhandian_list nil)
  (setq ls_file (open sbdcgbfile "r"))
  (setq p (read-line ls_file))
  (while (/= p nil)
    (progn (while (= (substr p 1 1) " ") (setq p (substr p 2)))
           (while (= (substr p (strlen p)) " ")
             (setq p (substr p 1 (1- (strlen p))))
           )
           (while (/= (strlen p) (strlen (vl-string-subst " " " " p)))
             (setq p (vl-string-subst " " " " p))
           )
           (setq ppp (string_strlist1 p " "))
           (if (and (= (length ppp) 5)
                    (/= (distof (nth 1 ppp)) nil)
                    (/= (distof (nth 2 ppp)) nil)
                    (/= (distof (nth 3 ppp)) nil)
               )
             (progn (setq pp (nth 0 ppp))
                    (setq py        (atof (nth 1 ppp))
                          px        (atof (nth 2 ppp))
                          ph        (atof (nth 3 ppp))
                          pcode        (nth 4 ppp)
                    )
                    (if        (or (= pcode "103") (= pcode "108"))
                      (setq pt (list px py 0.0))
                      (setq pt (list px py ph))
                    )
                    (setq zhandian_list
                           (append zhandian_list (list (list pp pt)))
                    )
             )
           )
           (setq p (read-line ls_file))
    )
  )
  (close ls_file)
  zhandian_list
)
;;-----------------------------------------------------------
(defun jcszlayer (layname laycolor)
  ;;(jcszlayer layname laycolor)
  ;;检查设置所操作图层是否存在，如果不存在则新建，否则解锁、解冻、打开
  (vl-load-com)
  (setq        acadobject1   (vlax-get-acad-object)
        acaddocument1 (vla-get-activedocument acadobject1)
        mspace1              (vla-get-modelspace acaddocument1)
  )
  (setq LayerSel (vla-get-Layers AcadDocument1))
  (setq LayerObj (vla-add LayerSel layname))
  (if (/= nil (tblsearch "layer" layname))
    (progn ;;解锁
           (if (= (vla-get-lock layerobj) :vlax-true)
             (vla-put-Lock LayerObj :vlax-false)
           )
           ;;解冻
           (if (= (vla-get-Freeze layerobj) :vlax-true)
             (vla-put-Freeze LayerObj :vlax-false)
           )
           ;;打开
           (if (= (vla-get-layeron layerobj) :vlax-false)
             (vla-put-layeron LayerObj :vlax-true)
           )
           ;;设为当前层
           (vla-put-activelayer AcadDocument1 LayerObj)
           ;;设置图层颜色
           (if (/= nil laycolor)
             (vla-put-color LayerObj laycolor)
           )
    )
    (progn ;;新建图层
           (vla-get-activelayer AcadDocument1 LayerObj)
           ;;设置图层颜色
           (if (/= nil laycolor)
             (vla-put-color LayerObj laycolor)
           )
    )
  )
)
;;-----------------------------------------------------------
(defun emaketext (textlay     textnr          textthk     textpt10
                  textzg      textro          textkuan    textqxie
                  textcolor   textstyle          textlcr     textdmh
                  textpt11    textkzsx
                 )
  ;; (emaketext textlay textnr textthk textpt10 textzg textro
  ;; textkuan
  ;; textqxie textcolor textstyle textlcr textdmh textpt11
  ;; textkzsx)
  ;;用entmake方法添加文字注记实体
  ;; textlay--注记图层 textnr---注记内容 textthk---注记厚值 
  ;; textzg---注记字高textro---注记旋转方向 textkuan---注记宽度系数 
  ;; textqxie---注记倾斜角度 textstyle---注记文字样式
  ;; textcolor---注记颜色
  ;; textlcr--注记左中右对齐方式(0,1,2,nil)
  ;; textdmh--注记上中下对齐方式(3,2,1,nil)
  ;; textpt10---注记点坐标10 textpt11---注记点坐标11
  ;; textkzsx--扩展属性
  ;;加载(vl-load-com)环境
  (vl-load-com)
  (setq        acadobject1   (vlax-get-acad-object)
        acaddocument1 (vla-get-activedocument acadobject1)
        mspace1              (vla-get-modelspace acaddocument1)
  )
  ;;注记位置textpt10和字高textzg
  (setq textst_name nil)
  (setq insertionpnt (vlax-make-safearray vlax-vbdouble '(0 . 2)))
  (if (= (length textpt10) 2)
    (setq textpt10 (list (nth 0 textpt10) (nth 1 textpt10) 0.0))
  )
  (vlax-safearray-fill insertionpnt textpt10)
  (setq textobj (vla-addtext mspace1 textnr insertionpnt textzg))
  ;; textlcr--注记左中右对齐方式(0,1,2,nil)
  ;; textdmh--注记上中下对齐方式(3,2,1,nil)
  (cond        ;;左上
        ((and (= textlcr 0) (= textdmh 3))
         (vla-put-alignment textobj acalignmenttopleft)
        )
        ;;左中
        ((and (= textlcr 0) (= textdmh 2))
         (vla-put-alignment textobj acalignmentmiddleleft)
        )
        ;;左下
        ((and (= textlcr 0) (= textdmh 1))
         (vla-put-alignment textobj acalignmentbottomleft)
        )
        ;;中上
        ((and (= textlcr 1) (= textdmh 3))
         (vla-put-alignment textobj acalignmenttopcenter)
        )
        ;;中中
        ((and (= textlcr 1) (= textdmh 2))
         (vla-put-alignment textobj acalignmentmiddlecenter)
        )
        ;;中下
        ((and (= textlcr 1) (= textdmh 1))
         (vla-put-alignment textobj acalignmentbottomcenter)
        )
        ;;右上
        ((and (= textlcr 2) (= textdmh 3))
         (vla-put-alignment textobj acalignmenttopright)
        )
        ;;右中
        ((and (= textlcr 2) (= textdmh 2))
         (vla-put-alignment textobj acalignmentmiddleright)
        )
        ;;右下
        ((and (= textlcr 2) (= textdmh 1))
         (vla-put-alignment textobj acalignmentbottomright)
        )
        ;;默认
        (t (vla-put-alignment textobj acalignmentleft))
  )
  (if (and (/= textlcr nil) (/= textdmh nil))
    (vla-put-textalignmentpoint textobj insertionpnt)
  )
  ;;注记颜色textcolor
  (if (/= nil textcolor)
    (vla-put-color textobj textcolor)
    (vla-put-color textobj acbylayer)
  )
  ;;注记字型样式textstyle
  (if (and (/= textstyle (vla-get-stylename textobj))
           (/= (tblsearch "style" textstyle) nil)
      )
    (vla-put-stylename textobj textstyle)
  )
  ;;注记厚度textthk
  (if (/= textthk nil)
    (vla-put-thickness textobj textthk)
    (vla-put-thickness textobj 0.0)
  )
  ;;注记旋转角度textro
  (if (/= nil textro)
    (vla-put-rotation textobj textro)
    (vla-put-rotation textobj 0.0)
  )
  ;;注记图层textlay
  (if (= (tblsearch "layer" textlay) nil)
    (progn (setq layersel (vla-get-layers acaddocument1))
           (setq layerobj (vla-add layersel textlay))
    )
  )
  (vla-put-layer textobj textlay)
  ;;注记的宽度系数textkuan
  (if (/= textkuan nil)
    (vla-put-ScaleFactor TextObj textkuan)
    (vla-put-ScaleFactor TextObj 1.0)
  )
  ;;注记的倾斜系数textqxie
  (if (/= textqxie nil)
    (vla-put-ObliqueAngle TextObj textqxie)
    (vla-put-ObliqueAngle TextObj 0.0)
  )
  (setq textst_name (vlax-vla-object->ename TextObj))
  ;;注记的扩展属性textkzsx
  (if (/= textkzsx nil)
    (progn (setq textst_ss (entget textst_name (list "*")))
           (setq textst_ss (append textst_ss (list (cons -3 textkzsx))))
           (entmod textst_ss)
           (entupd textst_name)
    )
  )
  textst_name
)
;;-----------------------------------------------------------
(defun emakepoint (pointlay pointthk pointpt pointcolor pointkzsx)
  ;;(emakepoint pointlay pointthk pointpt pointcolor pointkzsx)
  ;;用entmake方法添加点实体
  ;;pointlay-图层 pointthk-厚度值 pointpt-3D插入点 
  ;;pointcolor-颜色   pointkzsx-扩展属性 
  (vl-load-com)
  (setq        acadobject1   (vlax-get-acad-object)
        acaddocument1 (vla-get-activedocument acadobject1)
        mspace1              (vla-get-modelspace acaddocument1)
  )
  (setq point_point (vlax-3d-point pointpt))
  (setq pointobj (vla-addpoint mspace1 point_point))
  ;;point的图层blklay
  (if (= (tblsearch "layer" pointlay) nil)
    (progn (setq layersel (vla-get-layers acaddocument1))
           (setq layerobj (vla-add layersel blklay))
    )
  )
  (vla-put-layer pointobj pointlay)
  ;;厚度pointthinkness
  (if (and (/= (type pointthk) 'REAL) (/= (type pointthk) 'INT))
    (setq pointthk 0.0)
  )
  (vla-put-Thickness pointobj pointthk)
  ;;颜色pointcolor
  (if (/= nil pointcolor)
    (vla-put-color pointobj pointcolor)
    (vla-put-color pointobj acbylayer)
  )
  (setq pointst_name (vlax-vla-object->ename pointobj))
  ;;point的扩展属性pointkzsx
  (if (/= pointkzsx nil)
    (progn (setq pointst_ss (entget pointst_name (list "*")))
           (setq pointst_ss (append pointst_ss (list (cons -3 pointkzsx))))
           (entmod pointst_ss)
           (entupd pointst_name)
    )
  )
  pointst_name
)
;;-----------------------------------------------------------
(defun Example_ZoomExtents ()
  ;;当前图形缩放(command "zoom" "e")
  ;;(Example_ZoomExtents)
  (vl-load-com)
  (setq        acadobject1   (vlax-get-acad-object)
        acaddocument1 (vla-get-activedocument acadobject1)
        mspace1              (vla-get-modelspace acaddocument1)
  )
  (setq application1 (vla-get-application acaddocument1))
  (vla-zoomextents application1)
  (princ)
)
;;-----------------------------------------------------------
(defun string_strlist1
       (stringstr stringfgf / shujui shujuj strlst shujustr1)
  ;;(string_strlist1 stringstr stringfgf)
  ;;将输入的分隔符分隔的字符串数据转换为包含数据列信息的字符串列表子程序
  ;;stringstr--以分隔符分隔的字符串 stringfgf--字符串的分隔符
  ;;测试: (string_strlist1 "Hello,2World,12,5456.1568," ",")
  ;;    = ("Hello" "2World" "12" "5456.1568")
  (setq        shujui 0
        shujuj 1
        shujustr1 ""
        strlst nil
  )
  (cond
    ((/= (strlen stringstr) 0)
     (progn
       (if (/= (substr stringstr (strlen stringstr) 1) stringfgf)
         (setq stringstr (strcat stringstr stringfgf))
       )
       (while
         (/= ""
             (setq
               shujulist (substr stringstr (setq shujui (1+ shujui)) 1)
             )
         )
          (cond
            ((/= stringfgf shujulist)
             (setq shujustr1 (strcat shujustr1 shujulist))
            )
            (T
             (setq strlst    (append strlst (list shujustr1))
                   shujustr1 ""
                   shujuj    (1+ shujuj)
             )
            )
          )
       )
       (if (/= shujustr1 "")
         (append strlst (list shujustr1))
         strlst
       )
     )
    )
    (t (setq strlst (list "")))
  )
  strlst
)
;;-----------------------------------------------------------
(defun printbar_jd (jdstr s_fm s_fz)
  ;;在状态兰显示程序运行进度
  ;;(printbar_jd jdstr s_fm s_fz)
  ;;jdstr--显示进度的文字说明 s_fm--进度总数 s_fz--当前的个数
  (if (/= s_fm 1)
    (progn
      (cond
        ((or (= s_fz 1) (= s_fz 0))
         (dos_progbar (strcat jdstr ",请稍候...") s_fm)
        )
        ((and (> s_fz 1) (< s_fz s_fm)) (dos_progbar -1))
        ((>= s_fz s_fm) (dos_progbar))
        (t nil)
      )
    )
  )
)
;;-----------------------------------------------------------
(defun doslib_setup ()
  ;;测试Doslib工具是否被安装
  ;;(doslib_setup)
  (setq cadver (substr (getvar "ACADVER") 1 2))
  (setq        newarx nil
        arx_i 0
        doslibfile nil
  )
  (while (< arx_i (length (arx)))
    (setq newarx (append newarx (list (strcase (nth arx_i (arx))))))
    (setq arx_i (1+ arx_i))
  )
  (cond        ((and (= cadver "15")
              (= (member (strcase "doslib2k.arx") newarx) nil)
         )
         (setq doslibfile     (arxload (findfile "doslib2k.arx"))
               doslibfilename "doslib2k.arx"
         )
        )
        ((and (= cadver "15")
              (/= (member (strcase "doslib2k.arx") newarx) nil)
         )
         (setq doslibfilename "doslib2k.arx")
        )
        ((and (= cadver "16")
              (= (member (strcase "doslib2004.arx") newarx) nil)
         )
         (setq doslibfile     (arxload (findfile "doslib2004.arx"))
               doslibfilename "doslib2004.arx"
         )
        )
        ((and (= cadver "16")
              (/= (member (strcase "doslib2004.arx") newarx) nil)
         )
         (setq doslibfilename "doslib2004.arx")
        )
        ((and (= cadver "17")
              (= (member (strcase "DOSLib17.arx") newarx) nil)
         )
         (setq doslibfile     (arxload (findfile "DOSLib17.arx"))
               doslibfilename "DOSLib17.arx"
         )
        )
        ((and (= cadver "17")
              (/= (member (strcase "DOSLib17.arx") newarx) nil)
         )
         (setq doslibfilename "DOSLib17.arx")
        )
        (t nil)
  )
  doslibfilename
)
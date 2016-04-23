;0·初始化
;|
; Examples:
; (vlxls-app-init)  ==>  33
; |;
(defun vlxls-app-Init
   (/ OSVar GGG Olb8 Olb9 Olb10 TLB Out msg msg1 msg2)
   (if *Chinese*
      (setq msg "\n 初始化微软Excel " 
            msg1 "\042初始化Excel错误\042" 
            msg2 (strcat
                     "\042 警告" 
                     "\n ====" 
                     "\n 无法在您的计算机上检测到微软Excel软件" 
                     "\n 如果您确认已经安装Excel, 请发送电子邮" 
                     "\n 件到kozmosovia@hotmail.com获取更多的解决方案\042" 
                  )
      )
      (setq msg "\n Initializing Microsoft Excel " 
            msg1 "\042Initialization Error\042" 
            msg2 (strcat
                     "\042 WARNING" 
                     "\n =======" 
                     "\n Can NOT detect Excel97/200X/XP in your computer" 
                     "\n If you already have Excel installed, please email" 
                     "\n us to get more solution via kozmosovia@hotmail.com\042" 
                  )
      )
   )
   (if (null msxl-xl24HourClock)
      (progn
         (if (and (setq GGG
                        (vl-registry-read
                           "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\Excel.EXE" 
                           "Path" 
                        )
                  )
                  (setq GGG (strcase (strcat GGG "Excel.EXE" )))
            )
            (progn
               (foreach OSVar (list "SYSTEMROOT" "WINDIR" 
                                 "WINBOOTDIR" "SYSTEMDRIVE" 
                                 "USERNAME" "COMPUTERNAME" 
                                 "HOMEDRIVE" "HOMEPATH" 
                                 "PROGRAMFILES" 
                              )
                  (if (vl-string-search (strcat "%" OSVar "%" ) GGG)
                     (setq GGG (vl-string-subst (strcase (getenv OSVar)) (strcat "%" OSVar "%" ) GGG))
                  )
               )
               (setq Olb8 (findfile (vl-string-subst "EXCEL8.OLB" "EXCEL.EXE" GGG))
                     Olb9 (findfile (vl-string-subst "EXCEL9.OLB" "EXCEL.EXE" GGG))
                     Olb10 (findfile (vl-string-subst "EXCEL10.OLB" "EXCEL.EXE" GGG))
               )
               (cond 
                  ((= (vl-filename-base (vl-filename-directory GGG)) "OFFICE12")
                     (setq TLB GGG Out "2007")
                  )
                  ((= (vl-filename-base (vl-filename-directory GGG)) "OFFICE11")
                     (setq TLB GGG Out "2003")
                  )
                  ((= (vl-filename-base (vl-filename-directory GGG)) "OFFICE10")
                     (setq TLB GGG Out "XP")
                  )
                  (Olb9 (setq TLB Olb9 Out "2000"))
                  (Olb8 (setq TLB Olb8 Out "97"))
                  (T (setq Out "Version Unknown" ))
               )
               (if TLB
                  (progn
                     (princ (strcat MSG Out "..." ))
                     (vlax-import-type-library
                        :tlb-filename TLB :methods-prefix
                        "msxl-" :properties-prefix
                        "msxl-" :constants-prefix "msxl-" 
                     )
                  )
               )
            )
            (progn
               ; (if vldcl-msgbox
                  ; (vldcl-msgbox "x" msg1 msg2)
                  ; (alert (read msg2))
               ; )
               ; (exit)
			   (*error* msg1)
            )
         )
      )
   )
   msxl-xl24HourClock
)


; ;0·新建xls
; ;|
; Examples:
; (setq *xlapp* (vlxls-app-new T))  ==>  #<VLA-OBJECT _Application 001db27c>
; |;
(defun vlxls-app-New (UnHide / Rtn)
   (if (vlxls-app-init)
      (progn
         (if *Chinese*
            (princ "\n 新建微软Excel工作表..." )
            (princ "\n Creating new Excel Spreadsheet file..." )
         )
         (if (setq Rtn (vlax-get-or-create-object "Excel.Application" ))
            (progn
               (vlax-invoke
                  (vlax-get-property Rtn 'WorkBooks)
                  'Add
               )
               (if UnHide
                  (vla-put-visible Rtn 1)
                  (vla-put-visible Rtn 0)
               )
            )
         )
      )
   )
   Rtn
)


; ;0·打开xls
; ;|
; Examples:
; (setq *xlapp* (vlxls-app-open "C:/test.XLS" T))  ==>  #<VLA-OBJECT _Application 001efd2c>
; |;
(defun vlxls-app-open
   (XLSFile UnHide / ExcelApp WorkSheet Sheets ActiveSheet Rtn)
   (setq XLSFile (strcase XLSFile))
   (if (null (wcmatch XLSFile "*.XLS" ))
      (setq XLSFile (strcat XLSFile ".XLS" ))
   )
   (if (and (findfile XLSFile) (setq Rtn (vlax-get-or-create-object "Excel.Application" )))
      (progn
         (vlax-invoke
            (vlax-get-property Rtn 'WorkBooks)
            'Open
            XLSFile
         )
         (if UnHide
            (vla-put-visible Rtn 1)
            (vla-put-visible Rtn 0)
         )
      )
   )
   Rtn
)


; ;0·保存xls
; ;|
; Examples:
; (vlxls-app-save *xlapp*)  ==>  T
; |;
(defun vlxls-app-save (xlapp)
   (equal (vlax-invoke (vlax-get-property Xlapp "ActiveWorkbook") "Save") :vlax-true)
)


; ;0·重命名xls
; ;|
; Examples:
; (vlxls-app-saveas *xlapp* nil)  ==>  "C:/Temp-Folder/XLS.XLS"
; (vlxls-app-saveas *xlapp* "C:/Temp-Folder/XLS.XLS")  ==>  "C:/Temp-Folder/XLS.XLS"
; (vlxls-app-saveas *xlapp* nil)  ==>  NIL
; |;
(defun vlxls-app-saveas (xlapp Filename / Rtn)
   (if (null filename)
      (setq filename (strcat (getvar "dwgprefix" ) "XLS.XLS" ))
   )
   (if (null (wcmatch (setq filename (strcase Filename)) "*`.XLS" ))
      (setq filename (strcat filename ".XLS" ))
   )
   (if (findfile Filename)
      (vl-file-delete (findfile Filename))
   )
   (vlax-invoke-method
      (vlax-get-property Xlapp "ActiveWorkbook" )
      "SaveAs" 
      Filename
      msxl-xlNormal
      "" 
      "" 
      :vlax-False
      :vlax-False
	  msxl-xlExclusive
	  msxl-xlLocalSessionChanges
	  :vlax-False
	  nil
      nil
	  :vlax-False
   )
   (findfile Filename)
)


;0·关闭指定xls
;|
; Examples:
; (vlxls-app-quit *xlapp* nil)  ==>  nil
; |;
(defun vlxls-app-quit (ExlObj SaveYN)
   (if SaveYN
      (vlax-invoke
         (vlax-get-property ExlObj "ActiveWorkbook" )
         'Close
      )
      (vlax-invoke
         (vlax-get-property ExlObj "ActiveWorkbook" )
         'Close
         :vlax-False
      )
   )
   (vlax-invoke ExlObj 'QUIT)
   (vlax-release-object ExlObj)
   (setq ExlObj nil)
   (gc)
)


;0·关闭所有文件
;|
; Examples:
; (vlxls-app-kill T)  ==>  nil
; |;
(defun vlxls-app-kill (SaveYN / ExlObj)
   (while (setq ExlObj (vlax-get-object "Excel.Application" ))
      (vlxls-app-quit ExlObj SaveYN)
   )
)



;0·获取xls文件的所有页面名称列表
;|
; Examples:
; (vlxls-sheet-get-all *xlapp*)  ==>  ( "Sheet1" "Sheet2" "Sheet3" )
; |;
(defun vlxls-sheet-get-all (xlapp / SH Rtn)
   (vlax-for SH (vlax-get-property Xlapp "sheets" )
      (setq Rtn (cons (vlax-get-property sh "Name" ) Rtn))
   )
   (reverse Rtn)
)


;0·获取当前页面名称
;|
; Examples:
; (vlxls-sheet-get-active *xlapp*)  ==>  "Sheet2"
; |;
(defun vlxls-Sheet-Get-Active (xlapp)
   (vlax-get (msxl-get-ActiveSheet Xlapp) 'name)
)


;0·删除页面
;|
; Examples:
; (vlxls-sheet-delete *xlapp* "Sheet1")  ==>  T
; (vlxls-sheet-delete *xlapp* "UnExistingSheet")  ==>  NIL
; |;
(defun vlxls-sheet-delete (xlapp Name / sh Rtn)
   (setq Rtn (vlxls-sheet-get-all Xlapp))
   (vlax-for sh (vlax-get-property Xlapp "sheets" )
      (if (= (vlax-get-property sh "Name" ) Name)
         (vlax-invoke-method sh "Delete" )
      )
   )
   (not (equal Rtn (vlxls-sheet-get-all Xlapp)))
)


;0·更改页名称
;|
; Examples:
; (vlxls-sheet-rename "New" "Sheet1" *xlapp*)  ==>  T
; (vlxls-sheet-rename "New" NIL *xlapp*)  ==>  T
; (vlxls-sheet-rename "Sheet3" NIL *xlapp*)  ==>  NIL
; (vlxls-sheet-rename "Sheet2" "Sheet1" *xlapp*)  ==>  NIL
; (vlxls-sheet-rename "Sheet2" "UnExistSheet" *xlapp*)  ==>  NIL
; |;
(defun vlxls-sheet-rename (New Old Xlapp / sh Rtn)
   (if (null old)
      (setq old (msxl-get-name (msxl-get-activesheet Xlapp)))
   )
   (if (member New (vlxls-sheet-get-all Xlapp))
      (setq Rtn nil)
      (progn
         (vlax-for sh (vlax-get-property Xlapp "sheets" )
            (if (= (msxl-get-name sh) Old)
               (msxl-put-name sh New)
            )
         )
         (setq Rtn
            (equal New
               (vlax-get (msxl-get-ActiveSheet Xlapp) 'name)
            )
         )
      )
   )
   Rtn
)


;0·新增页
;|
; Examples:
; (vlxls-sheet-add *xlapp* "Sheet1")  ==>  T
; (vlxls-sheet-add *xlapp* NIL)  ==>  T
; (vlxls-sheet-add *xlapp* "NewSheet")  ==>  NIL
; |;
(defun vlxls-sheet-add (xlapp Name / Rtn)
   (if (member name (vlxls-sheet-get-all xlapp))
      (setq Rtn nil)
      (progn
         (vlax-put-property
            (vlax-invoke-method
               (vlax-get-property Xlapp "sheets" )
               "Add" 
            )
            "name" 
            Name
         )
         (setq Rtn (equal (vlxls-sheet-get-active xlapp) name))
      )
   )
   Rtn
)


;0·激活指定页面
;|
; Examples:
; (vlxls-sheet-put-active *xlapp* "Sheet1")  ==>  T
; (vlxls-sheet-put-active *xlapp* "NewSheet")  ==>  T
; |;
(defun vlxls-sheet-put-active (xlapp Name / sh)
   (if (null (vlxls-sheet-add xlapp name))
      (vlax-for sh (vlax-get-property Xlapp "sheets" )
         (if (= (vlax-get-property sh "Name" ) Name)
            (vlax-invoke-method sh "Activate" )
         )
      )
   )
   (equal (vlxls-sheet-get-active xlapp) name)
)



;|获取指定页面的使用区域
; Examples:
; (vlxls-sheet-get-usedrange *xlapp* "Sheet1")  ==>  "A1:AW35"
; (vlxls-sheet-get-usedrange *xlapp* "NewSheet")  ==>  "A1:AW35"
; |;
;0·获取指定页面的使用区域
(defun vlxls-sheet-get-UsedRange (xlapp Name / sh Rtn)
   (if (null Name)
      (setq Name (vlax-get (msxl-get-ActiveSheet Xlapp) 'Name))
   )
   (vlax-for sh (vlax-get-property Xlapp "sheets" )
      (if (= (vlax-get-property sh "Name" ) Name)
         (setq Rtn (vlax-get-property sh "UsedRange" ))
      )
   )
   Rtn
)


;0·获取区域的起始与末尾坐标（****结果不对******）
;|
; Examples:
; (vlxls-range-getid RangeObject)  ==>  "C12:G19"
; (vlxls-range-getid RangeObject)  ==>  "B16:B16"
; |;
(defun vlxls-range-getID (range / str col row dx dy)
   (if (equal (vlxls-get-property range "mergecells" ) :vlax-true)
      (setq str "MergeArea." )
      (setq str "" )
   )
   (setq dx (vlxls-get-property range (strcat str "Rows.Count" ))
         dy (vlxls-get-property range (strcat str "Columns.Count" ))
         row (vlxls-get-property range (strcat str "Row" ))
         col (vlxls-get-property range (strcat str "Column" ))
   )
   (strcat (vlxls-rangeid (list col row)) ":" (vlxls-rangeid (list (1- (+ col dy)) (1- (+ row dx)))))
)


;0·获取指定范围的每格尺寸(行高、列宽)
;|
; Examples:
; (vlxls-range-size RangeObject)  ==>  ((27.0 27.0 110.25 51.0 69.75) (14.25 14.25 14.25 14.25 14.25 57.0 14.25))
; |;
(defun vlxls-range-size (range / xl row col rrr ccc xxx yyy)
   (setq xl (msxl-get-parent range)
         Row (msxl-get-count (msxl-get-rows Range))
         Col (msxl-get-count (msxl-get-columns Range))
         RRR (1- (msxl-get-row Range))
         CCC (msxl-get-column Range)
   )
   (repeat Row
      (setq   yyy (cons    (vlax-variant-value
                           (msxl-get-height
                              (msxl-get-range xl (vlxls-rangeid (list CCC (setq RRR (1+ RRR)))))
                           )
                        )
                        yyy
               )
      )
   )
   (setq RRR (msxl-get-row Range) CCC (1- (msxl-get-column Range)))
   (repeat Col
      (setq   xxx   (cons (vlax-variant-value
                           (msxl-get-width
                              (msxl-get-range xl (vlxls-rangeid (list (setq CCC (1+ CCC)) RRR)))
                           )
                        )
                        xxx
                  )
      )
   )
   (list (reverse xxx) (reverse yyy))
)


;0·获取区域对象的属性
;|
; Examples:
; (vlxls-get-property RangeObject "Application.ActiveSheet.Name")  ==>  "Sheet1"
; (vlxls-get-property RangeObject "MergeArea.Columns.Count")  ==>  3
; |;
(defun vlxls-get-property (top prop / vlstring->list item Rtn)
   (defun vlstring->list (str st / lst e)
      (setq str (strcat str st))
      (while (vl-string-search st str)
         (setq lst (append lst (list (substr str 1 (vl-string-search st str)))))
         (setq str (substr str (+ (1+ (strlen st)) (vl-string-search st str))))
      )
      (if lst (mapcar '(lambda (e) (vl-string-trim " " e)) lst))
   )
   (cond
      ((= (type prop) 'sym)(setq Rtn (vlax-get-property top prop)))
      ((= (type prop) 'str)
         (if (null (vl-string-search "." prop))
            (setq Rtn (vlax-get-property top prop))
            (foreach item (vlstring->list prop "." )
               (if (null Rtn)
                  (setq Rtn (vlax-get-property top item))
                  (setq Rtn (vlax-get-property Rtn item))
               )
            )
         )
      )
   )
   (cond
      ((= (type Rtn) 'variant) (setq Rtn (vlax-variant-value Rtn)))
      ((= (type Rtn) 'safearray) (setq Rtn (vlxls-variant->list Rtn)))
   )
   Rtn
)



;0·区域坐标与坐标列表互换
;|
; Examples:
; (vlxls-rangeid ‘ (3 14))  ==>  "C14" 
; (vlxls-rangeid "D23")  ==>  (4 23)
; (vlxls-rangeid "DD23")  ==>  (108 23)
; |;
(defun vlxls-rangeid (id / str->list list->str xid->str Rtn)
   (defun str->list (str / ii xk xv rr pos x y)
      (setq rr (strlen str))
      (foreach ii '( "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" )
         (if (setq pos (vl-string-search ii str))
               (setq rr (min pos rr))
         )
      )
      (setq x (substr str 1 rr)
            y (substr str (1+ rr))
      )
      (if (= (strlen x) 2)
         (setq xk (- (ascii (substr x 1 1)) 64)
               xv (- (ascii (substr x 2)) 64)
         )
         (setq xk 0
               xv (- (ascii x) 64)
         )
      )
      (list (+ (* xk 26) xv) (read y))
   )
   (defun xid->str (IntNum / PosNum Nm-One)
      (setq Nm-One (1- IntNum)
         PosNum (/ Nm-One 26)
      )
      (if (= PosNum 0)
         (chr (+ 65 (rem Nm-One 26)))
         (strcat (chr (+ 64 PosNum)) (chr (+ 65 (rem Nm-One 26))))
      )
   )
   (defun list->str (idr / x y)
      (setq x (car idr)
            y (cadr idr)
            x (xid->str x)
            y (itoa y)
      )
      (strcat x y)
   )
   (cond 
      ((= (type id) 'str) (setq Rtn (str->list id)))
      ((= (type id) 'list) (setq Rtn (list->str id)))
   )
   Rtn
)


;0·单元格/区域坐标转换为列表
;|
; Examples:
; (vlxls-cellid ‘ (3 14))  ==>  ( "C14" "" )
; (vlxls-cellid "D23")  ==>  ( "D23" "" )
; (vlxls-cellid "C12:F3")  ==>  ( "C3" "F12" )
; (vlxls-cellid "F15:G22")  ==>  ( "F15" "G22" )
; |;
(defun vlxls-cellid (id / xx id1 id2 Rtn)
   (if (= (type id) 'list)
      (setq id (vlxls-rangeid id))
   )
   (setq id (strcase id))
   (if (null (setq xx (vl-string-search ":" id)))
      (setq Rtn (list id "" ))
      (setq id1 (substr id 1 xx)
            id2 (substr id (+ xx 2))
            id1 (vlxls-rangeid id1)
            id2 (vlxls-rangeid id2)
            Rtn (list   (vlxls-rangeid
                           (list (min (car id1) (car id2))
                                 (min (cadr id1) (cadr id2))
                           )
                        )
                        (vlxls-rangeid
                           (list (max (car id1) (car id2))
                                 (max (cadr id1) (cadr id2))
                           )
                        )
               )
      )
   )
   Rtn
)

;0·设置当前选定的单元格
;|
; Examples:
; (vlxls-cell-put-active *xlapp* "C12:F15")  ==>  #<VLA-OBJECT Range 09d1998c>
; (vlxls-cell-put-active *xlapp* "F12")  ==>  #<VLA-OBJECT Range 06c389a2>
; |;
(defun vlxls-cell-put-active (xl id / Rtn)
   (if (= (type id) 'list)
      (setq id (vlxls-rangeid id))
   )
   (msxl-activate (setq Rtn (msxl-get-range xl id)))
   Rtn
)


;0·从指定单元格开始获取值
;|
; Examples:
; (vlxls-cell-get-value *xlapp* "C12")  ==>  "g"
; (vlxls-cell-get-value *xlapp* "C12:C12")  ==>  "g"
; (vlxls-cell-get-value *xlapp* "C12:C15")  ==>  (( "g" ) ( "" ) ( "" ) ( "" ))
; (vlxls-cell-get-value *xlapp* "C12:F12")  ==>  (( "g" "ds" "" "" ))
; (vlxls-cell-get-value *xlapp* "C12:F15")  ==>  (( "g" "ds" "" "" ) ( "" "" "g" "" ) ( "" "" "" "" ) ( "" "" "" "" ))
; |;
(defun vlxls-cell-get-value (xl id)
   (if (= (type id) 'list)
      (setq id (vlxls-rangeid id))
   )
   (vlxls-variant->list
      (msxl-get-value2 (msxl-get-range xl id))
   )
)


;0·从指定单元格开始赋值
;|
; Examples:
; (vlxls-cell-put-value *xlapp* "C12" "xx")  ==>  #<VLA-OBJECT Range 093a7764>
; (vlxls-cell-put-value *xlapp* "C12:F3" "xx")  ==>  #<VLA-OBJECT Range 43c5ac64>
; (vlxls-cell-put-value *xlapp* "C12:D13" ‘ (("zz" "xx") ("xx" "zz")))  ==>  #<VLA-OBJECT Range 1b8f2a64>
; |;
(defun vlxls-cell-put-value
   (xl id Data / vllist-explode idx xx yy ary Rtn)
   (defun vllist-explode (lst)
      (cond
         ((not lst) nil)
         ((atom lst) (list lst))
         ((append (vllist-explode (car lst))
                        (vllist-explode (cdr lst))
               )
         )
      )
   )
   (if (null id) (setq id "A1" ))
   (if (= (type id) 'list) (setq id (vlxls-rangeid id)))
   (if (= (type (car Data)) 'LIST)
      (setq   ARY    (vlax-make-safearray
                     vlax-vbstring
                     (cons 0 (1- (length Data)))
                     (cons 1 (length (car Data)))
                  )
            XX (1- (length (car Data)))
            YY (1- (length Data))
      )
      (setq   ARY    (vlax-make-safearray
                     vlax-vbstring
                     (cons 0 1)
                     (cons 1 (length Data))
                  )
            XX (1- (length Data))
            YY 0
      )
   )
   (if (= xx yy 0)
      (msxl-put-value2
         (setq Rtn (msxl-get-range xl id))
         (car (vllist-explode data))
      )
      (progn
         (setq id (vlxls-cellid-calc id xx yy))
         (msxl-put-value2
            (setq Rtn (msxl-get-range xl id))
            (vlax-safearray-fill ary data)
         )
      )
   )
   Rtn
)


;0·计算从指定单元格开始偏移的区域范围
;|
; Examples:
; (vlxls-cellid-calc "C12" 2 20)  ==>  "C12:E32" 
; (vlxls-cellid-calc ‘ (2 23) 2 -120)  ==>  "B1:D23" 
; |;
(defun vlxls-cellid-calc (id x y / idx)
   (setq id (car (vlxls-cellid id))
         idx (vlxls-rangeid id)
         x (+ x (car idx))
         x (if (< x 1) 1 x)
         y (+ y (cadr idx))
         y (if (< y 1) 1 y)
         idx (vlxls-rangeid (list x y))
         id (vlxls-cellid (strcat id ":" idx))
         id (strcat (car id) ":" (cadr id))
   )
   id
)

;0·在指定位置，按列获取值
;|
; Examples:
; (vlxls-get-row-value *xlapp* "C12" 2)  ==>  ( "zz" "xxx" )
; (vlxls-get-row-value *xlapp* "C12" -20)  ==>  ( "" "" "zz" )
; |;
(defun vlxls-get-row-value (xl id len / vllist-explode Rtn)
   (defun vllist-explode (lst)
      (cond
         ((not lst) nil)
         ((atom lst) (list lst))
         ((append (vllist-explode (car lst)) (vllist-explode (cdr lst))))
      )
   )
   (if (> len 0)
      (setq id (vlxls-cellid-calc id (1- len) 0))
      (setq id (vlxls-cellid-calc id (1+ len) 0))
   )
   (setq Rtn (vllist-explode (vlxls-cell-get-value xl id)))
   Rtn
)

;0·从指定位置开始，每列赋值
;|
; Examples:
; (vlxls-put-row-value *xlapp* "C12" "abc")  ==>  #<VLA-OBJECT Range 2a621cac>
; (vlxls-put-row-value *xlapp* ‘ (12 3) "abc")  ==>  #<VLA-OBJECT Range 7a36c491>
; (vlxls-put-row-value *xlapp* "C12" ‘ ( "zz" "xxx" ))  ==>  #<VLA-OBJECT Range 09d1da1c>
; (vlxls-put-row-value *xlapp* ‘ (12 3) ‘ ( "zz" "xxx" ))  ==>  #<VLA-OBJECT Range 0a26c4f3>
; |;
(defun vlxls-put-row-value (xl id data / Rtn)
   (if (= (type data) 'str)
      (setq data (list data))
   )
   (setq id (car (vlxls-cellid id))
         id (vlxls-cellid-calc id (1- (length data)) 0)
   )
   ;;(vlxls-range-autofit
      (setq Rtn (vlxls-cell-put-value xl id (list data)))
  ;; )
   Rtn
)

;0·在指定位置，按行获取值
;|
; Examples:
; (vlxls-get-column-value *xlapp* "C12" 2)  ==>  ( "zz" "sdfsdf" )
; (vlxls-get-column-value *xlapp* "C12" -20)  ==>  ( "" "" "xxx" "xxx" "xxx" "xxx" "xxx" "xxx" "xxx" "xxx" "xxx" "zz" )
; |;
(defun vlxls-get-column-value (xl id len / vllist-explode Rtn)
   (defun vllist-explode (lst)
      (cond
         ((not lst) nil)
         ((atom lst) (list lst))
         ((append (vllist-explode (car lst)) (vllist-explode (cdr lst))))
      )
   )
   (setq id (car (vlxls-cellid id)))
   (if (> len 0)
      (setq id (vlxls-cellid-calc id 0 (1- len)))
      (setq id (vlxls-cellid-calc id 0 (1+ len)))
   )
   (setq Rtn (vllist-explode (vlxls-cell-get-value xl id)))
   Rtn
)


;0·从指定位置开始，每行赋值
;|
; Examples:
; (vlxls-put-column-value *xlapp* "C12" "abc")  ==>  #<VLA-OBJECT Range 049c521b>
; (vlxls-put-column-value *xlapp* ‘ (12 3) "abc")  ==>  #<VLA-OBJECT Range 0235cba1>
; (vlxls-put-column-value *xlapp* "C12" ‘ ( "zz" "xxx" ))  ==>  #<VLA-OBJECT Range 09d1da1c>
; (vlxls-put-column-value *xlapp* ‘ (12 3) ‘ ( "zz" "xxx" ))  ==>  #<VLA-OBJECT Range 0a26c4f3>
; |;
(defun vlxls-put-column-value (xl id data / item Rtn)
  (if (= (type data) 'str)
      (setq data (list data))
  )
  (setq   id (car (vlxls-cellid id))
         id (vlxls-cellid-calc id 0 (1- (length data)))
  )
  (foreach item data
      (setq Rtn (cons (list item) Rtn))
  )
  (vlxls-range-autofit
      (setq Rtn (vlxls-cell-put-value xl id (reverse Rtn)))
  )
  Rtn
)


;0·设置单元格边界(????????)
;|
; Examples:
; (vlxls-cell-border *xlapp* "C12:F14" T)  ==>  NIL
; (vlxls-cell-border *xlapp* "B8" NIL)  ==>  NIL
; |;
(defun vlxls-cell-border (xl id flg / bdr)
   (if flg
      (msxl-put-value (msxl-get-borders (msxl-get-range xl id)) 1)
      (msxl-put-value (msxl-get-borders (msxl-get-range xl id)) 'linestyle msxl-xlnone)
   )
)


;0·单元格合并
;|
; Examples:
; (vlxls-cell-merge *xlapp* "C12:F14")  ==>  #<VLA-OBJECT Range 0023ab7c>
; |;
(defun vlxls-cell-merge (xl id / vllist-explode Val Rtn)
   (defun vllist-explode (lst)
      (cond
         ((not lst) nil)
         ((atom lst) (list lst))
         ((append (vllist-explode (car lst)) (vllist-explode (cdr lst))))
      )
   )
   (setq val (vllist-explode (vlxls-cell-get-value xl id)))
   (while (vl-position "" val)
      (setq val (vl-remove "" val))
   )
   (setq val (car val) Rtn (msxl-get-range xl id))
   (msxl-clear Rtn)
   (msxl-merge Rtn nil)
   (msxl-put-value2 Rtn Val)
   (msxl-put-HorizontalAlignment Rtn -4108)
   Rtn
)


;0·单元格拆分
;|
; Examples:
; (vlxls-cell-unmerge *xlapp* "C12:F14")  ==>  #<VLA-OBJECT Range 0023ab7c>
; (vlxls-cell-unmerge *xlapp* "E14")  ==>  #<VLA-OBJECT Range 09ce72e4>
; |;
(defun vlxls-cell-unmerge (xl id / Rtn)
   (if (vlxls-cell-merge-p xl id)
      (progn
         (vlax-invoke-method (msxl-get-range xl id) 'unmerge)
         (setq Rtn (msxl-get-range xl id))
      )
   )
   Rtn
)


;0·单元格合并状态检测
;|
; Examples:
; (vlxls-cell-merge-p *xlapp* "C12:F14")  ==>  T
; (vlxls-cell-merge-p *xlapp* "E14")  ==>  NIL
; |;
(defun vlxls-cell-merge-p (xl id)
   (equal (vlax-variant-value
            (msxl-get-mergecells (msxl-get-range xl id))
         )
         :vlax-true
   )
)

;0·获取合并单元格的范围
;|
; Examples:
; (vlxls-cell-get-mergeid *xlapp* "C12:F14")  ==>  "B9:G19"
; (vlxls-cell-get-mergeid *xlapp* "E14")  ==>  "A11:G19"
; |;
(defun vlxls-cell-get-mergeid (XL ID / Rtn)
   (if (vlxls-cell-merge-p xl id)
      (progn
         (msxl-select (msxl-get-range xl id))
         (setq Rtn (vlxls-range-getid (msxl-get-selection xl)))
      )
   )
   Rtn
)

;0·当前文件自适应尺寸
;|
; Examples:
; (vlxls-app-autofit *xlapp*)  ==>  T
; (vlxls-app-autofit *xlapp*)  ==>  NIL
; |;
(defun vlxls-app-autofit (xlapp / sh act Rtn)
   (setq act (vlxls-Sheet-Get-Active xlapp))
   (foreach sh (append (vl-remove act (vlxls-sheet-get-all Xlapp)) (list act))
      (setq Rtn (variant-value
                  (msxl-autofit
                     (msxl-get-columns (msxl-get-Cells (vlxls-sheet-get-usedrange xlapp sh)))
                  )
               )
      )
   )
   (equal Rtn :vlax-true)
)


;0·设置区域对象自适应尺寸
;|
; Examples:
; (vlxls-range-autofit (msxl-get-range *xlapp* "C12:F15"))  ==>  T
; (vlxls-range-autofit RangeObject)  ==>  NIL
; |;
(defun vlxls-range-autofit (range)
   (equal   (vlax-variant-value
               (msxl-autofit
                  (msxl-get-columns (msxl-get-Cells range))
               )
            )
            :vlax-true
   )
)


;0·获取单元格的填充色
;|
; Examples:
; (vlxls-cell-get-aci *xlapp* "C12")  ==>  256
; (vlxls-cell-get-aci *xlapp* ‘ (12 3))  ==>  15
; |;
(defun vlxls-cell-get-aci (xl id)
   (vlxls-color-eci->aci
      (vlax-variant-value
         (msxl-get-colorindex
            (msxl-get-interior (msxl-get-range xl id))
         )
      )
   )
)

;0`设置单元格的填充色
;|
; Examples:
; (vlxls-cell-put-aci *xlapp* "C12" 6)  ==>  #<VLA-OBJECT Range 09d1369c>
; (vlxls-cell-put-aci *xlapp* "C12" nil)  ==>  #<VLA-OBJECT Range 09d1369c>
; |;
(defun vlxls-cell-put-aci (xl id aci / Rtn)
   (if (null aci)
   (msxl-put-colorindex
      (msxl-get-interior (setq Rtn (msxl-get-range xl id)))
      (vlax-make-variant -4142)
   )
   (msxl-put-colorindex
      (msxl-get-interior (setq Rtn (msxl-get-range xl id)))
      (vlxls-color-aci->eci aci)
   )
  )
  Rtn
)

;0·获取单元格内文字的颜色
;|
; Examples:
; (vlxls-text-get-aci *xlapp* "C12")  ==>  256
; (vlxls-text-get-aci *xlapp* ‘ (12 3))  ==>  15
; |;
(defun vlxls-text-get-aci (xl id)
     (vlxls-color-eci->aci
      (vlax-variant-value
         (msxl-get-colorindex
            (msxl-get-font (msxl-get-range xl id))
         )
      )
   )
   Rtn
)

;0·设置单元格内文字的颜色
;|
; Examples:
; (vlxls-text-put-aci *xlapp* "C12" 6)  ==>  #<VLA-OBJECT Range 09d1369c>
; (vlxls-text-put-aci *xlapp* "C12" nil)  ==>  #<VLA-OBJECT Range 09d1369c>
; |;
(defun vlxls-text-put-aci (xl id aci / Rtn)
   (if (null aci)
      (msxl-put-colorindex
         (msxl-get-font (setq Rtn (msxl-get-range xl id)))
         (vlax-make-variant -4105)
      )
      (msxl-put-colorindex
         (msxl-get-font (setq Rtn (msxl-get-range xl id)))
         (vlxls-color-aci->eci aci)
      )
   )
   Rtn
)

;0·获取单元格内文字的属性
;|
; Examples:
; (vlxls-text-get-prop *xlapp* "C12")  ==>  ((0 . "C12" ) (7 . "Arial" ) (40 . 12.0) (62 . 256) (72 . 9) (420 . 16711935))
; (vlxls-text-get-prop *xlapp* ‘ (2 10))  ==>  ((0 . "B10" ) (7 . "Arial" ) (40 . 12.0) (62 . 256) (72 . 11) (420 . 16711935))
; |;
(defun vlxls-text-get-prop
   (xl id / Cell Font DXF1 DXF7 DXF40 DXF72 DXF62 DXF420 Rtn)
   (setq id (car (vlxls-cellid id))
         cell (msxl-get-range xl id)
         font (msxl-get-font cell)
         DXF7 (vlax-variant-value (msxl-get-name Font))
         DXF40 (vlax-variant-value (msxl-get-size Font))
         DXF72 (vlax-variant-value (msxl-get-HorizontalAlignment Cell))
         DXF72 (cond ((= DXF72 -4152) 11) ((= DXF72 -4108) 10) (T 9))
         DXF62 (vlxls-color-eci->aci (vlax-variant-value (msxl-get-colorIndex Font)))
         DXF420 (vlxls-color-eci->truecolor (vlax-variant-value (msxl-get-colorIndex Font)))
         Rtn   (list 
                  (cons 0 (strcase id))
                  (cons 7 DXF7)
                  (cons 40 DXF40)
                  (cons 62 DXF62)
                  (cons 72 DXF72)
                  (cons 420 DXF420)
               )
   )
   Rtn
)

;0·获取单元格的属性
;| 
; Examples:
; (vlxls-cell-get-prop *xlapp* "C12:F14")  ==>  ((0 . "C12:F14" ) (1 ( "zz" "xxx" "xxx" "xxx" ) ( "sdfsdf" "sdfsdf" "sdfsdf" "sdfsdf" ) ( "sdfsdf" "sdfsdf" "sdfsdf" "sdfsdf" )) (10 108.0 156.75) (41 . 156.0) (42 . 42.75) (-1 (0 . "C12" ) (7 . "Arial" ) (40 . 12.0) (62 . 256) (72 . 9) (420 . 16711935)))
; (vlxls-cell-get-prop *xlapp* "B8")  ==>  ((0 . "B8" ) (1 . "sdg" ) (10 54.0 99.75) (41 . 54.0) (42 . 14.25) (-1 (0 . "B8" ) (7 . "Arial" ) (40 . 12.0) (62 . 256) (72 . 10) (420 . 16711935)))
; |;
(defun vlxls-cell-get-prop
  (xl id / range left top width height dxf10 Rtn)
  (if (vlxls-cell-merge-p xl id)
      (setq id (vlxls-cell-get-mergeid xl id))
  )
  (setq   range (msxl-get-range xl id)
         left (vlax-variant-value (msxl-get-left Range))
         top (vlax-variant-value (msxl-get-top Range))
         width (vlax-variant-value (msxl-get-width Range))
         height (vlax-variant-value (msxl-get-height Range))
         dxf10 (list left top)
         Rtn    (list
                  (cons 0 (strcase id))
                  (cons 1 (vlxls-cell-get-value xl id))
                  (cons 10 dxf10)
                  (cons 41 width)
                  (cons 42 height)
                  (cons -1 (vlxls-text-get-prop xl id))
               )
   )
   Rtn
)


;0·颜色转换
;|
; Examples:
; (vlxls-color-eci->aci 0)  ==>  256
; (vlxls-color-eci->aci 1)  ==>  18
; (vlxls-color-eci->aci 12)  ==>  56
; (vlxls-color-eci->aci 120)  ==>  256
; |;
(defun vlxls-color-eci->aci (Color / Rtn)
   (if (null (setq Rtn (cdr (assoc Color *xls-color*))))
      (setq Rtn 256)
      (setq Rtn (nth 0 Rtn))
   )
   Rtn
)


;0·颜色转换
;|
; Examples:
; (vlxls-color-aci->eci 0)  ==>  0
; (vlxls-color-aci->eci 1)  ==>  3
; (vlxls-color-aci->eci 12)  ==>  0
; (vlxls-color-aci->eci 120)  ==>  0
; |;
(defun vlxls-color-aci->eci (Color / Item Rtn)
   (foreach Item *xls-color*
      (if (= (nth 1 Item) Color)
         (setq Rtn (car Item))
      )
   )
   (if (null Rtn)
      (setq Rtn 0)
   )
   Rtn
)


;0·颜色转换
;|
; Examples:
; (vlxls-color-aci-> truecolor 0)  ==>  16711935
; (vlxls-color-aci->truecolor 1)  ==>  16711680
; (vlxls-color-aci-> truecolor 12)  ==>  16711935
; (vlxls-color-aci-> truecolor 120)  ==>  16711935
; |;
(defun vlxls-color-aci->truecolor (aci)
   (vlxls-color-eci->truecolor (vlxls-color-aci->eci aci))
)


;0·颜色转换
;|
; Examples:
; (vlxls-color-eci->truecolor 0)  ==>  16711935
; (vlxls-color-eci->truecolor 1)  ==>  0
; (vlxls-color-eci->truecolor 12)  ==>  8355584
; (vlxls-color-eci->truecolor 120)  ==>  16711935
; |;
(defun vlxls-color-ECI->truecolor (Color / Rtn)
   (if (setq Rtn (cdr (assoc Color *xls-color*)))
      (setq Rtn (nth 1 Rtn))
   )
   (if (null Rtn)
      (setq Rtn 16711935)
   )
   Rtn
)


;0·数组数据转换为数组对象？？？？
;|
; Examples:
; (vlxls-rangevalue->safearray ‘ (("A1" . "aaa") ("B4" . "ccc")))  ==>  #<safearray...>
; (vlxls-variant->list (vlxls-rangevalue->safearray '(( "A1" . "aaa" ) ( "B4" . "ccc" ))))  ==>  (( "aaa" "" ) ( "" "" ) ( "" "" ) ( "" "ccc" ))
; |;
(defun vlxls-Rangevalue->SafeArray (Data / XSub_GetXY XSub_GetMinMaxID xsub-MergeID->List 
                                             MinID MaxID ID ID1 ID2 IDN X minid xy Y Rtn Item
                                    )
   (defun xsub-MergeID->List (ID / KK ID1 ID2 IDX IDY Rtn)
      (Setq ID (strcase ID))
      (if (setq KK (vl-string-search ":" ID))
         (setq ID1 (substr ID 1 KK) ID2 (substr ID (+ 2 KK)))
         (setq ID1 ID ID2 ID)
      )
      (setq ID1 (vlxls-rangeid ID1)
            ID2 (vlxls-rangeid ID2)
            IDX (vlxls-rangeid (list (min (nth 0 ID1) (nth 0 ID2)) (min (nth 1 ID1) (nth 1 ID2))))
            IDY (vlxls-rangeid (list (max (nth 0 ID1) (nth 0 ID2)) (max (nth 1 ID1) (nth 1 ID2))))
            Rtn (list IDX IDY)
      )
      Rtn
   )
   (defun XSub_GetXY (ID SID / S10 S11 DX DY Rtn)
      (setq S10 (nth 0 MinID)
            S11 (nth 1 MinID)
            ID (vlxls-rangeid ID)
            DX (- (nth 0 ID) S10)
            DY (- (nth 1 ID) S11)
            Rtn (list DX DY)
      )
      Rtn
   )
   (defun XSub_GetMinMaxID (ID1 ID MinorMax / X Y X1 Y1 Rtn)
      (if (null ID)
         (setq Rtn ID1)
         (progn
            (setq ID1 (vlxls-rangeid ID1)
                  ID (vlxls-rangeid ID)
                  X1 (nth 0 ID1)
                  Y1 (nth 1 ID1)
                  X (nth 0 ID)
                  Y (nth 1 ID)
            )
            (if (null MinorMax)
               (setq Rtn (vlxls-rangeid (list (min X X1) (min Y Y1))))
               (setq Rtn (vlxls-rangeid (list (max X X1) (max Y Y1))))
            )
         )
      )
      Rtn
   )
   (foreach Item Data
      (setq ID (strcase (car Item)))
      (if (vl-string-search ":" ID)
         (setq IDN (xsub-MergeID->List ID))
         (setq IDN (list ID))
      )
      (foreach ID IDN
         (setq MinID (XSub_GetMinMaxID ID MinID nil)
               MaxID (XSub_GetMinMaxID ID MaxID T)
         )
      )
   )
   (setq MinID (vlxls-rangeid MinID)
         MaxID (vlxls-rangeid MaxID)
         X (- (nth 0 MaxID) (nth 0 MinID))
         Y (- (nth 1 MaxID) (nth 1 MinID))
         Rtn (vlax-make-safearray vlax-vbstring (cons 0 Y) (cons 1 (1+ X)))
   )
   (foreach Item Data
      (setq ID (strcase (car Item)))
      (if (vl-string-search ":" ID)
            (setq IDN (xsub-MergeID->List ID))
            (setq IDN (list ID))
      )
      (foreach ID IDN
         (setq XY (XSub_GetXY ID MinID))
         (vlax-safearray-put-element
            Rtn
            (nth 1 XY)
            (1+ (nth 0 XY))
            (cdr Item)
         )
      )
   )
   Rtn
)

;0·转换数据类型到cad数据
;|
; Examples:
; NONE
; |;
(defun vlxls-variant->list (VarX / Run Item Rtn)
   (setq Run T)
   (while
      Run
      (cond
         ((= (type VarX) 'SAFEARRAY)
            (setq VarX (vlax-safearray->list VarX))
         )
         ((= (type VarX) 'VARIANT)
            (if (member (vlax-variant-type VarX) (list 5 4 3 2))
               (setq VarX (vlax-variant-change-type Varx vlax-vbString))
            )
            (setq VarX (vlax-variant-value VarX))
         )
         (T (setq Run nil))
      )
   )
   (cond
      ((= (type VarX) 'LIST)
         (foreach Item VarX
            (setq Item (vlxls-variant->list Item)
                  Rtn (append Rtn (list Item))
            )
         )
      )
      ((= VarX nil) (setq Rtn "" ))
      (T (setq Rtn VarX))
   )
   Rtn
)


(setq *xls-color*
   (list 
      (list 1 18 0)
      (list 2 7 1677215)
      (list 3 1 16711680)
      (list 4 3 65280)
      (list 5 5 255)
      (list 6 2 16776960)
      (list 7 6 16711935)
      (list 8 4 65535)
      (list 9 16 8323072)
      (list 10 96 32512)
      (list 11 176 127)
      (list 12 56 8355584)
      (list 13 216 8323199)
      (list 14 136 32639)
      (list 15 9 12566463)
      (list 16 8 8355711)
      (list 17 161 9476095)
      (list 18 237 9449568)
      (list 19 7 1677167)
      (list 20 254 12648447)
      (list 21 218 6291552)
      (list 22 11 16744319)
      (list 23 152 24768)
      (list 24 254 13617407)
      (list 25 176 127)
      (list 26 6 16711935)
      (list 27 2 16776960)
      (list 28 4 65535)
      (list 29 216 8323199)
      (list 30 16 8323072)
      (list 31 136 32639)
      (list 32 5 255)
      (list 33 140 51455)
      (list 34 254 12648447)
      (list 35 254 13631439)
      (list 36 51 16777104)
      (list 37 151 9488639)
      (list 38 221 16750799)
      (list 39 191 13605119)
      (list 40 31 16763024)
      (list 41 150 3105023)
      (list 42 132 3131584)
      (list 43 62 9488384)
      (list 44 40 16762880)
      (list 45 30 16750336)
      (list 46 30 16738048)
      (list 47 165 6317968)
      (list 48 252 9475984)
      (list 49 148 12384)
      (list 50 105 3184736)
      (list 51 98 12032)
      (list 52 48 3158016)
      (list 53 24 9449472)
      (list 54 237 9449311)
      (list 55 177 3158160)
      (list 56 250 3092527)
   )
   *Chinese* nil
)

(if vl-load-com (vl-load-com))

(if vl-arx-import
   (foreach item '(ACAD_COLORDLG ACAD_truecolordlg
                     ACAD_STRLSORT INITDIA
                     ACAD-POP-DBMOD ACAD-PUSH-DBMOD
                     STARTAPP layoutlist
                  )
      (vl-arx-import item)
  )
)

(setq item nil
   *xls-ver* "4.0.80930 [Build: TRINNOLOV1]" 
)

;;(princ (strcat "\n KozMos VLXLS Project (Version " *xls-ver* ")"))
;;(princ "\n Copyright(C) 1994-2009 KozMos Inc. All rights reserved")
;;(princ)

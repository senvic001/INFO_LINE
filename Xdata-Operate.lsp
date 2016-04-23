 ;检索指定的扩展实体数据 
 ;entname为有效的图形实体名，appname为已登记的应用类型名 
 ;若检索成功则以LIST表的形式返回指定的扩展数据，否则返回nil 
 ;例如：(getxdata (entlast) "DESIGN") 
 ;返回：((1000."16Mn") (1040.32.45)) 
(defun getxdata  (entname appname / x0 x1)
  (setq appname (strcase appname))
  (if (= (type entname) 'ENAME)
    (if (tblsearch "APPID" appname)
      (progn
        (setq x1 (list appname))
        (if (setq x0 (assoc -3 (entget entname x1)))
          (cdr (assoc appname (cdr x0)))
          )
        ) ;_progn
      )
    )
  ) ;defun

 ;存储或修改指定的扩展实体数据 
 ;entname为有效的图形实体名，appname为已登记的应用类型名 
 ;xdata为属于appname应用类型的扩展数据表 
 ;若存储成功则返回包含指定扩展数据的实体数据表，否则返回nil 
 ;例如：
 ;(setxdata (entlast) "PRICE" ((1040.123.45) (1040.321.54))) 
(defun setxdata  (entname appname xdata / x0 x1)
  (setq appname (strcase appname))
  (if (= (type entname) 'ENAME)
    (if (tblsearch "APPID" appname)
      (if (setq x0 (entget entname))
        (progn
          (setq x1 (list -3
                         (append
                           (list appname)
                           xdata
                           )
                         )
                )
          (setq x0 (append
                     (entget entname)
                     (list x1)
                     )
                )
          (entmod x0)
          ))))
  )

 ;删除指定的扩展实体数据 
 ;entname为有效的图形实体名，appname为已登记的应用类型名 
 ;若删除成功则返回删除指定扩展数据后的实体数据表，否则返回nil 
 ;例如：(delxdata (entlast) "PRICE") 
(defun delxdata  (entname appname / x0 x1)
  (setq appname (strcase appname))
  (if (= (type entname) 'ENAME)
    (if (tblsearch "APPID" appname)
      (progn
        (setq x1 (list -3 (list appname)))
        (if (setq x0 (append
                       (entget entname)
                       (list x1)
                       )
                  )
          (entmod x0)
          );if
        );progn
      );if
    );if
  );defun


;*********************************************************************************************
;函数定义:SetRecordtoEntity(entname record tablename appname)
;功能：把记录数据写入到对象的Xdata,默认gl_appName 作为ID
;参数：实体名称，对象记录表("LD100" "LD100" x y h ""),表名
;返回：
;创建时间：2014/07/11   16:10
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun SetRecordtoEntity(entname record tablename appname/ index xdata data datatype)
   (setq index 0
      xdata (list (cons 1000 tablename)));Xdata第一个元素是表名
   ;;建立组码表
   (repeat (length record)
      (setq data (nth index record)
	 datatype (type data))
      (cond;只识别3种数据类型
	 ((= 'REAL datatype) (setq xdata (append xdata (list (cons 1040 data)))))
	 ((= 'STR datatype) (setq xdata (append xdata (list (cons 1000 data)))))
	 ((= 'INT datatype) (setq xdata (append xdata (list (cons 1071 data)))))
	 (t nil)
	 );cond
      (setq index (+ 1 index))
      );repeat
   (if (= nil appname)
      (setq appname gl_AppName)
      )
   ( setxdata  entname appname xdata)
   );defun

;;;返回以表名为第一个元素的字段数据列表
(defun GetRecordfromEntity(entname appname / outdata  index outlist data)
   (if (= nil appname)
      (setq appname gl_AppName)
      )
   (if (setq outdata (getxdata entname appname))
      (progn
	      (setq outlist nil
		 index 0
	      )
	 (repeat (length outdata)
	    (setq data (nth index outdata)
	 	outlist (append outlist (list (cdr data)))
		index (+ 1 index))
	    );repeat
	 outlist
      );progn
   );
   )
;;;
(defun DelRrecordfromEntity (entname appname)
   (if (= nil appname)
      (setq appname gl_AppName)
      )
   (delxdata entname appname)
   );defun
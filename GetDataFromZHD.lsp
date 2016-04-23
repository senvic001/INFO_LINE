;*********************************************************************************************
;函数定义: C:GetPointsFromZHD	
;功能：中海达手簿生成的点图,文字的插入点代表点坐标;从点图中读取点名，点坐标,输出到文件
;参数：
;返回：点表（（"p1" x1 y1 z1) ("p2" x2 y2 z2)...)
;创建时间：2014/07/10   21:10
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:GetPointsFromZHD	 (/ e file i outpath point str strout textset pointlist)
   (setq textset (ssget "X" (list (cons 0 "TEXT")))) ;选择所以Text对象
   (setq i	 0
	 outpath "C:\\zhd-points.csv"
      pointlist nil
      )

;;; 输出到文件和CAD图形
   (vl-file-delete outpath)
   (setq file (open outpath "w"))
   (write-line "点号，x坐标，y坐标" file)
   (repeat (sslength textset)
      (setq e	  (entget (ssname textset i))
	    point (cdr (assoc 10 e))
	    str	  (cdr (assoc 1 e))
	 str (vl-string-trim " " str)
	 pointlist (append pointlist (list (cons str point )))
	    )
      (setq strout
	      (strcat
		 str
		 ","
		 (rtos (car point) 2 4)
		 ","
		 (rtos (cadr point) 2 4)
		 ","
		 (rtos (last point) 2 4)))
      (write-line strout file)
      (setq i (+ 1 i))
      )

   (close file)
   (princ (strcat "\n点号和坐标数据已经写入文件" outpath))
   (startapp "notepad" outpath)
   pointlist
   );defun

;;所有点为文字的插入点，拾取所有文字，得到所有点,并排序
;;建立全局点表：((pointname handle ("status" 0))...)
;;;
;;;作为未编辑的数据点
(defun GetPointsFromTexts( / PointList i textset entlist pstr handle status e1 e2)
    (setq PointList nil
        i 0)
    (if (setq textset (ssget "X" '((0."TEXT")) ))
        (repeat (sslength textset)
            (setq entlist (entget (ssname  textset i))
                pstr (vl-string-trim " " (cdr (assoc 1 entlist)))
                handle (cdr (assoc 5 entlist))

                ;;;status
                ;;status (list "Status" 0);;是否在数据库中
                PointList (append PointList (list (list pstr handle status)))
                i (1+ i))
            );foreach
        );if
    
     ;;按照点号从小到大排序
    (setq      PointList (vl-sort
		  PointList
		  (function
		    (lambda (e1 e2)
		      (< (car e1) (car e2))))))
    PointList
    );defun

;;status:(是否在数据库中 ）----是否在图中 是孤立的点吗 被修改了吗)
;;;作为已经编辑过的数据点，或者是从数据库生成的点
(defun GetPointsFromBlocks (/ blset e i bl-obj pointname point pointlist attrib_objs)
 ;选择所有块对象
    (setq i 0
          pointlist nil
          )
    (if (setq blset (ssget "X" (list (cons 0 "INSERT"))))
        (repeat (sslength blset)
            (setq e      (ssname blset i)
                  bl-obj (vlax-ename->vla-object e)
                  handle (vla-get-Handle  bl-obj)
                  )
            ;;获取点号
            (if (= :vlax-true (vla-get-HasAttributes bl-obj))
                (progn
                    (setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes bl-obj)))
                          status (ldata-get bl-obj "STATUS" ))
                    (if (> (length attrib_objs) 0)
                        (setq pointname (vla-get-TextString (car attrib_objs))
                              pointlist (append pointlist (list (list pointname handle status)))
                              ) ;内容
                        )
                    ) ;progn
                ) ;if
            (setq i (1+  i))
            ) ;repeat
        ) ;if
    
    ;;按照点号从小到大排序
    (setq      pointlist (vl-sort
		  pointlist
		  (function
		    (lambda (e1 e2)
		      (< (car e1) (car e2))))))

    pointlist
    )


;*********************************************************************************************
;函数定义:C:GetPointsFromBlock
;功能：图块插入点代表点坐标，第一个属性表示点名；不输出无点名属性的图块
;参数：
;返回：点表（（"p1" x1 y1 z1) ("p2" x2 y2 z2)...)
;创建时间：2014/07/10   21:10
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
; (defun GetPointsFromBlocks (/ blset e i bl-obj pointname point pointlist attrib_objs)
   ; (setq blset (ssget "X" (list (cons 0 "INSERT")))) ;选择所有块对象
   ; (setq i	   0
	 ; pointlist nil
	 ; )

   ; (repeat (sslength blset)
      ; (setq e	   (entget (ssname blset i))
	    ; bl-obj (vlax-ename->vla-object (ssname blset i))
	    ; point  (cdr (assoc 10 e))
	    ; )
      ; ;;获取点号
      ; (if (vla-get-HasAttributes bl-obj)
	 ; (progn
	    ; (setq attrib_objs
		    ; (vlax-safearray->list
		       ; (vlax-variant-value
			  ; (vla-GetAttributes bl-obj))))
	    ; (if	(> (length attrib_objs) 0)
	       ; (setq pointname
		       ; (vla-get-TextString (car attrib_objs))
		     ; pointlist
		       ; (append
			  ; pointlist
			  ; (list (cons pointname point)))
		     ; ) ;内容
	       ; )
	    ; ) ;progn
	 ; ) ;if
      ; (setq i (+ 1 i))
      ; );repeat
   ; pointlist
   ; )
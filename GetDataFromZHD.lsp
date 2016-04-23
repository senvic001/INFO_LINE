;*********************************************************************************************
;��������: C:GetPointsFromZHD	
;���ܣ��к����ֲ����ɵĵ�ͼ,���ֵĲ������������;�ӵ�ͼ�ж�ȡ������������,������ļ�
;������
;���أ������"p1" x1 y1 z1) ("p2" x2 y2 z2)...)
;����ʱ�䣺2014/07/10   21:10
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:GetPointsFromZHD	 (/ e file i outpath point str strout textset pointlist)
   (setq textset (ssget "X" (list (cons 0 "TEXT")))) ;ѡ������Text����
   (setq i	 0
	 outpath "C:\\zhd-points.csv"
      pointlist nil
      )

;;; ������ļ���CADͼ��
   (vl-file-delete outpath)
   (setq file (open outpath "w"))
   (write-line "��ţ�x���꣬y����" file)
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
   (princ (strcat "\n��ź����������Ѿ�д���ļ�" outpath))
   (startapp "notepad" outpath)
   pointlist
   );defun

;;���е�Ϊ���ֵĲ���㣬ʰȡ�������֣��õ����е�,������
;;����ȫ�ֵ��((pointname handle ("status" 0))...)
;;;
;;;��Ϊδ�༭�����ݵ�
(defun GetPointsFromTexts( / PointList i textset entlist pstr handle status e1 e2)
    (setq PointList nil
        i 0)
    (if (setq textset (ssget "X" '((0."TEXT")) ))
        (repeat (sslength textset)
            (setq entlist (entget (ssname  textset i))
                pstr (vl-string-trim " " (cdr (assoc 1 entlist)))
                handle (cdr (assoc 5 entlist))

                ;;;status
                ;;status (list "Status" 0);;�Ƿ������ݿ���
                PointList (append PointList (list (list pstr handle status)))
                i (1+ i))
            );foreach
        );if
    
     ;;���յ�Ŵ�С��������
    (setq      PointList (vl-sort
		  PointList
		  (function
		    (lambda (e1 e2)
		      (< (car e1) (car e2))))))
    PointList
    );defun

;;status:(�Ƿ������ݿ��� ��----�Ƿ���ͼ�� �ǹ����ĵ��� ���޸�����)
;;;��Ϊ�Ѿ��༭�������ݵ㣬�����Ǵ����ݿ����ɵĵ�
(defun GetPointsFromBlocks (/ blset e i bl-obj pointname point pointlist attrib_objs)
 ;ѡ�����п����
    (setq i 0
          pointlist nil
          )
    (if (setq blset (ssget "X" (list (cons 0 "INSERT"))))
        (repeat (sslength blset)
            (setq e      (ssname blset i)
                  bl-obj (vlax-ename->vla-object e)
                  handle (vla-get-Handle  bl-obj)
                  )
            ;;��ȡ���
            (if (= :vlax-true (vla-get-HasAttributes bl-obj))
                (progn
                    (setq attrib_objs (vlax-safearray->list (vlax-variant-value (vla-GetAttributes bl-obj)))
                          status (ldata-get bl-obj "STATUS" ))
                    (if (> (length attrib_objs) 0)
                        (setq pointname (vla-get-TextString (car attrib_objs))
                              pointlist (append pointlist (list (list pointname handle status)))
                              ) ;����
                        )
                    ) ;progn
                ) ;if
            (setq i (1+  i))
            ) ;repeat
        ) ;if
    
    ;;���յ�Ŵ�С��������
    (setq      pointlist (vl-sort
		  pointlist
		  (function
		    (lambda (e1 e2)
		      (< (car e1) (car e2))))))

    pointlist
    )


;*********************************************************************************************
;��������:C:GetPointsFromBlock
;���ܣ�ͼ�������������꣬��һ�����Ա�ʾ������������޵������Ե�ͼ��
;������
;���أ������"p1" x1 y1 z1) ("p2" x2 y2 z2)...)
;����ʱ�䣺2014/07/10   21:10
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
; (defun GetPointsFromBlocks (/ blset e i bl-obj pointname point pointlist attrib_objs)
   ; (setq blset (ssget "X" (list (cons 0 "INSERT")))) ;ѡ�����п����
   ; (setq i	   0
	 ; pointlist nil
	 ; )

   ; (repeat (sslength blset)
      ; (setq e	   (entget (ssname blset i))
	    ; bl-obj (vlax-ename->vla-object (ssname blset i))
	    ; point  (cdr (assoc 10 e))
	    ; )
      ; ;;��ȡ���
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
		     ; ) ;����
	       ; )
	    ; ) ;progn
	 ; ) ;if
      ; (setq i (+ 1 i))
      ; );repeat
   ; pointlist
   ; )
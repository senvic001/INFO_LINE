 ;����ָ������չʵ������ 
 ;entnameΪ��Ч��ͼ��ʵ������appnameΪ�ѵǼǵ�Ӧ�������� 
 ;�������ɹ�����LIST�����ʽ����ָ������չ���ݣ����򷵻�nil 
 ;���磺(getxdata (entlast) "DESIGN") 
 ;���أ�((1000."16Mn") (1040.32.45)) 
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

 ;�洢���޸�ָ������չʵ������ 
 ;entnameΪ��Ч��ͼ��ʵ������appnameΪ�ѵǼǵ�Ӧ�������� 
 ;xdataΪ����appnameӦ�����͵���չ���ݱ� 
 ;���洢�ɹ��򷵻ذ���ָ����չ���ݵ�ʵ�����ݱ����򷵻�nil 
 ;���磺
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

 ;ɾ��ָ������չʵ������ 
 ;entnameΪ��Ч��ͼ��ʵ������appnameΪ�ѵǼǵ�Ӧ�������� 
 ;��ɾ���ɹ��򷵻�ɾ��ָ����չ���ݺ��ʵ�����ݱ����򷵻�nil 
 ;���磺(delxdata (entlast) "PRICE") 
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
;��������:SetRecordtoEntity(entname record tablename appname)
;���ܣ��Ѽ�¼����д�뵽�����Xdata,Ĭ��gl_appName ��ΪID
;������ʵ�����ƣ������¼��("LD100" "LD100" x y h ""),����
;���أ�
;����ʱ�䣺2014/07/11   16:10
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun SetRecordtoEntity(entname record tablename appname/ index xdata data datatype)
   (setq index 0
      xdata (list (cons 1000 tablename)));Xdata��һ��Ԫ���Ǳ���
   ;;���������
   (repeat (length record)
      (setq data (nth index record)
	 datatype (type data))
      (cond;ֻʶ��3����������
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

;;;�����Ա���Ϊ��һ��Ԫ�ص��ֶ������б�
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
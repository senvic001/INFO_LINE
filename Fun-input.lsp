;;�������
;;Ĭ��ֵ��"USERR1" "\n please input sth"  2


;��������:
;Fun_InPutValue(defaultvalue userr promotstr precision)
;Fun_InPutString
;Fun_InPutInt
;���ܣ�����ʵ�����ַ���������ʱ�������û�������
;ǰ���������û��Զ��������USERI1-5   USERR1-5   USERS1-5
;������defaultvalue Ĭ��ֵ userr ʹ�õ��û��Զ������ promotstr ��ʾ  precision ʵ���ľ���
;����ֵ��
;����ʱ�䣺2006/02/23 08:50
;�޸�ʱ�䣺2010/06/11 14:11  2011/02/08
;�汾��AutoCAD2008����ͨ��
;�����ˣ����۾�
(defun Fun_InPutValue (defaultvalue userr promotstr precision / tmpvalue)
  (if (= 0 (getvar userr))
    (setvar userr defaultvalue)
    )
   (if (setq tmpvalue (getreal (strcat promotstr "<"
           (rtos (getvar userr) 2 precision)
           ">:")))
     (setvar userr tmpvalue)
     (getvar userr)
   )
)

;;;;�Զ���1
(defun Fun_InPutValue2
       (defaultvalue userr promotstr precision / tmpvalue)
  (if (= 0 (getvar userr))
    (setvar userr defaultvalue)
    ) ;if
  (if (setq tmpvalue (getreal
		   (strcat promotstr
			   "<"
			   (rtos (+ 1 (getvar userr)) 2 precision)
			   ">:")))
    (setvar userr tmpvalue)
    (progn
      (setvar userr (+ 1 (getvar userr)))
      ) ;progn
    ) ;if
  )


;;;(defun Fun_InPutString (defaultvalue useri promotstr / tmpvalue)
;;;   (setq data (getvar useri))
;;;  (if (/= nil data)
;;;     (if (/= 'STR (type data))
;;;      (setvar useri defaultvalue)
;;;     )
;;;     (setvar useri defaultvalue)
;;;     )
;;;  (if (setq tmpvalue (getstring	(strcat	promotstr
;;;					"<"
;;;					(getvar useri)
;;;					">:"
;;;				)))
;;;      (if (/= 0 (strlen tmpvalue))
;;;	(setvar useri tmpvalue)
;;;      )					;if
;;;  );if
;;;  (getvar useri)
;;;)

(defun Fun_InPutString (defaultvalue useri promotstr / tmpvalue)
     (setvar useri defaultvalue)
  	 (if (setq tmpvalue (getstring	(strcat	promotstr
					"<"
					defaultvalue
					">:"
				)))
	     (if (/= 0 (strlen tmpvalue))
		 	(setvar useri tmpvalue)
	      )					;if
  );if
  (getvar useri)
)


(defun Fun_InPutInt (defaultvalue useri promotstr / tmpvalue)
  (if (= 0 (getvar useri))
    (setvar useri defaultvalue)
    )
   (if (setq tmpvalue (getreal (strcat promotstr "<"
           (rtos (getvar useri) 2 0)
           ">:")))
     (setvar useri tmpvalue)
     (getvar useri)
   )
)
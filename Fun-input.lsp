;;输入参数
;;默认值，"USERR1" "\n please input sth"  2


;函数定义:
;Fun_InPutValue(defaultvalue userr promotstr precision)
;Fun_InPutString
;Fun_InPutInt
;功能：输入实数、字符串、整数时，保存用户的输入
;前提条件：用户自定义变量：USERI1-5   USERR1-5   USERS1-5
;参数：defaultvalue 默认值 userr 使用的用户自定义变量 promotstr 提示  precision 实数的精度
;返回值：
;创建时间：2006/02/23 08:50
;修改时间：2010/06/11 14:11  2011/02/08
;版本：AutoCAD2008测试通过
;创建人：沈雄君
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

;;;;自动加1
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
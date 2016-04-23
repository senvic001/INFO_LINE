;;;WTCAD-common通用函数

;;从3D点表创建一条默认参数的3D多段线
;;para:pointList3D (x1 y1 z1 x1 y2 z2...)
;;return:vlaObject of a polyline,or nil
(defun Add3DPolyLineObject(pointList3D /  i len mspace newarray ret vertices)
	(setq ret nil i 0 vertices nil)
	(if (listp pointList3D)
		(progn
			; (setq len (length pointList3D))
			; (repeat len
				; (setq vertices (append  vertices  (nth i pointList3D))
					; i (1+ i)
				; )
			; )
			(setq newarray (vlax-make-safearray vlax-vbDouble (cons 0 (- (length pointList3D) 1)))
				mSpace (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
			) 
			(vlax-safearray-fill newarray pointList3D)
			(setq ret (vla-Add3DPoly mSpace (vlax-make-variant newarray)))
		)
	)
	ret
)

 ;;删除掉列表中所有重复的元素,保持各个元素唯一
 (defun ListRemoveSameElement (entlist / e1 outlist)
	;;entlist 边界点中包含重复的元素,删除掉
	(setq outlist nil)
	(if (> (length entlist) 1)
		(while (> (length entlist) 0)
			(setq e1 (car entlist)
				entlist (cdr entlist)
				entlist (vl-remove e1 entlist)
				outlist (cons e1 outlist)
			)
		)
		;;else
		(setq outlist entlist)
	)
	outlist
 )	

;;时间消耗函数
;;开始计时,返回当前时间
(defun StartWatch ()
	(setq gl_Time (getvar "TDUSRTIMER"))
)
;;显示当前消耗时间
;;返回当前时间
(defun TimeCosted (promptstr preTime / timecost)
	(princ (strcat "\n" promptstr "耗时:"))
	(if preTime 
		(setq timecost (* (- (getvar "TDUSRTIMER") preTime) 86400))
		(setq timecost (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
	)
	(princ timecost)
	(princ "s.")
	(getvar "TDUSRTIMER")
)

;;显示错误信息,并退出
(defun *error* (msg)
	(princ "\n错误:")
	(princ msg)
	(princ "退出！")
	(exit)
)


;;产生随机数 2位整数
(defun getrand  ()
        (rtos (rem (getvar "CPUTICKS") 100.0) 2 0)
        )

;;0-1之间的随机数
(defun ZL-RAND ()     (/ (rem (getvar "CPUTICKS") 1984) 1983.0) )

;;;函数：
;;;m_IntersectWith
;;;功能：
;;;两曲线求交点
;;;若无交点，则求取在xy平面上的投影交点，且求ent2的z值
(defun m_IntersectWith (m_ent1 m_ent2 / m_obj1 m_obj2 m_objcopy1 m_objcopy2 m_jdtab m_jdtab1 i p pz2)
    ;;适用对象: Line、Polyline、LWPolyline、Circle、Arc、Ellipse、3dPolyline、Spline
    ;;支持求空间虚交点，但Z坐标始终为0.0，要求Z坐标，请用(vlax-curve-getClosestPointToProjection)函数
    (setq m_obj1 (vlax-ename->vla-object m_ent1)
          m_obj2 (vlax-ename->vla-object m_ent2)
          m_objcopy1 nil
          m_objcopy2 nil
          m_jdtab nil)

    (setq m_objcopy1 (vla-copy m_obj1))
    ;;复制第一条曲线实体
    (setq m_objcopy2 (vla-copy m_obj2))
    ;;复制第二条曲线实体

    (setq m_objcopy1 (m_ShadowToXY m_objcopy1))
    ;;得到投影实体
    (setq m_objcopy2 (m_ShadowToXY m_objcopy2))
    ;;得到投影实体


    (if (and m_objcopy1 m_objcopy2)
        (setq m_jdtab1 (vla-intersectwith m_objcopy1 m_objcopy2 acExtendnone)))
    ;;得到交点集

    (if (> (vlax-safearray-get-u-bound (vlax-variant-value m_jdtab1) 1)
           1
        ) ;_ End_>
        ;;判断有无交点
        (progn
            (setq m_jdtab1 (vlax-safearray->list (vlax-variant-value m_jdtab1)))
            ;;safearray数组转换为list表
            (setq i 0)
            (repeat (/ (length m_jdtab1) 3)
                (setq m_jd (list (nth i m_jdtab1)
                                 (nth (+ 1 i) m_jdtab1)
                                 (nth (+ 2 i) m_jdtab1)
                           ) ;_ End_list
                ) ;_ End_setq
                ;;取得一个交点
                (setq m_jdtab (cons m_jd m_jdtab))
                ;;构造交点表（(第一个交点) (第二个交点)。。。）
                (setq i (+ 3 i))
            ) ;_ End_repeat
        ) ;_ End_progn
        ;;(princ "\n两曲线无交点!")
    ) ;_ End_if

    (if m_objcopy1 (vla-delete m_objcopy1))
    ;;删除复制的第一条曲线实体
    (if m_objcopy2 (vla-delete m_objcopy2))
    ;;删除复制的第二条曲线实体

    ;;找到Z坐标
    (foreach p m_jdtab
        (if (setq pz2 (vlax-curve-getClosestPointToProjection m_obj2 p (list 0 0 1)))
            (setq m_jdtab (subst pz2 p m_jdtab))
            )
        );

    m_jdtab
    ;;返回交点表，无交点返回nil
) ;_ End_defun

(defun m_ShadowToXY (m_obj / m_objname m_pts m_pts1 i)
    ;;对曲线实体m_obj创建一个投影至xy平面的曲线实体，即对曲线实体上每个控制点的z坐标值置为0.0
    ;;输入曲线实体(vla对象)
    ;;返回投影实体(vla对象)
    (setq m_objname (vla-get-objectname m_obj))
    ;;取得实体的类型名称
    ;;(m_princ "\nObjectName：" m_objname)
    (cond
        ((= "AcDbSpline" m_objname)
         ;;样条曲线(Spline)
         (setq i 0)
         (setq m_pts (vlax-variant-value (vla-get-fitpoints m_obj)))
         ;;取得样条曲线的拟合点
         (setq m_pts1 (vlax-variant-value (vla-get-controlpoints m_obj)))
         ;;取得样条曲线的控制点
         (repeat (vla-get-numberoffitpoints m_obj)
             ;;循环
             (vlax-safearray-put-element m_pts (+ i 2) 0.0)
             ;;改变每个拟合点的z值为0.0
             (setq i (+ i 3))
         ) ;_ End_repeat
         (vla-put-fitpoints m_obj m_pts)
         ;;更改曲线拟合点属性

         (setq i 0)

         (repeat (vla-get-numberofcontrolpoints m_obj)
             ;;循环
             (vlax-safearray-put-element m_pts1 (+ i 2) 0.0)
             ;;改变每个控制点的z值为0.0
             (setq i (+ i 3))
         ) ;_ End_repeat
         (vla-put-controlpoints m_obj m_pts1)
         ;;更改曲线控制点属性
        )

        ((= "AcDb3dPolyline" m_objname)
         ;;三维多段线(3dpolyline)
         (setq i 0)
         (setq m_pts (vlax-variant-value (vla-get-coordinates m_obj)))
         ;;取得3维多段线的控制点
         (repeat (/ (length (vlax-safearray->list m_pts)) 3)
             (vlax-safearray-put-element m_pts (+ i 2) 0.0)
             (setq i (+ i 3))
         ) ;_ End_repeat
         (vla-put-coordinates m_obj m_pts)
        )

        ((= "AcDbLine" m_objname)
         ;;直线(line)
         (setq i 0)
         (setq m_pts (vlax-variant-value (vla-get-startpoint m_obj)))
         ;;取得直线的起点座标
         (setq m_pts1 (vlax-variant-value (vla-get-endpoint m_obj)))
         ;;取得直线的端点座标
         (vlax-safearray-put-element m_pts 2 0.0)
         ;;改变起点座标z值为0.0
         (vlax-safearray-put-element m_pts1 2 0.0)
         (vla-put-startpoint m_obj m_pts)
         (vla-put-endpoint m_obj m_pts1)
        )

        ((or (= "AcDbCircle" m_objname)
             ;;园(circle)
             (= "AcDbArc" m_objname)
             ;;圆弧(arc)
             (= "AcDbEllipse" m_objname)
             ;;椭圆及椭圆弧(ellipse)
         ) ;_ End_or
         (setq m_pts (vlax-variant-value (vla-get-center m_obj)))
         ;;取得中心点座标
         (vlax-safearray-put-element m_pts 2 0.0)
         ;;改变中心点座标z值为0.0
         (vla-put-center m_obj m_pts)
        )

        ((or (= "AcDbPolyline" m_objname)
             ;;多段线(polyline、lwpolyline)
             (= "AcDb2dPolyline" m_objname)
             ;;拟合的2维多段线(polyline、lwpolyline)
         ) ;_ End_or
         (vla-put-elevation m_obj 0.0)
         ;;改变标高值为0.0
        )
    ) ;_ End_cond
    (setq m_obj m_obj)
) ;_ End_defun






;;;same point
;;;比较P1，P2是否相等，精度0.0001
;;;返回T or nil
(defun equal_point  (p1 p2 precision / len1 len2 result i x1 x2 )
    (if (and p1 p2)
        (progn
            (setq len1   (length p1)
                  len2   (length p2)
                  result T
                  i      0)
            (if (= len1 len2)
                (repeat len1
                    (setq x1 (nth i p1)
                          x2 (nth i p2))
                    (if (> (abs (- x1 x2)) precision)
                        (setq result nil)
                        ) ;if
                    (setq i (1+ i))
                    ) ;repeat
                ) ;if
            ) ;progn
        ) ;if
    result
    ) ;
       

;;;扩展 assoc：指定关键字在次一级表中的顺序号，以0为起点；
;;;((a1 b1 c1 d1)(a2 b2 c2 d2)..)
;;;assoc2 a list2 0 等价于 assoc a list2
;;;确保list2两层表，index在范围内
(defun assoc2(key list2 index / outdata i )
   (setq outdata nil
      i 0)
   (if (and (/= nil list2) (listp list2))
      (if (= 0 index)
      	(setq outdata (assoc key list2))
	 ;;else
	 (progn
	      	(while (and (< i (length list2)) (/= key (nth index (nth i list2))));次级list
		   (setq i (1+ i))
	       	);while
	    (if (< i (length list2))
	       (setq outdata (nth i list2))
	       );if
	);progn
   	);if
      );if
   outdata	   
   );defun

;;;求列表中最小值
(defun list-min (valuelist / mindata item )
  (if (listp valuelist)
    (progn
      (setq mindata (car valuelist))
      (foreach item valuelist
         (if (< item mindata) (setq mindata item));
        );foreach
      );progn
    );if
  mindata
  );defun

;;;求列表中最大值
(defun list-max (valuelist / maxdata item )
  (if (listp valuelist)
    (progn
      (setq maxdata (car valuelist))
      (foreach item valuelist
         (if (> item maxdata) (setq maxdata item));
        );foreach
      );progn
    );if
  maxdata
  );defun
  
  ;;; 修改依据: xiaomu 2005-10-12 

;;;****************************************************************************
;;; No.5-3    Windows多文件选择(适用于CADR15以上) 函数                         
;;; 说明: 本函数使用MsComDlg.Commondialog对象(Comdlg.OCX)                      
;;; 调用: (ayGetMultFiles "多选文件" "图形文件(*.dwg)|*.dwg|所有(*.*)|*.*" "") 
;;; 返回: ("C:\\DWG" "7b.dwg" "7c.dwg" "1.Dwg")                                
;;;****************************************************************************
(if (/= (vl-registry-read "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905")
    		"gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj")
   (vl-registry-write "HKEY_CLASSES_ROOT\\Licenses\\4D553650-6ABE-11cf-8ADB-00AA00C00905" ""
     								  "gfjmrfkfifkmkfffrlmmgmhmnlulkmfmqkqj")
);end_if
(defun GetMultFiles (strTitle strFilter strInitDir / Maxfiles Flags WinDlg mFiles Catchit)
  (vl-load-com)
  (setq WinDlg (vlax-create-object "MSComDlg.CommonDialog"))
	(if (not WinDlg)
		(progn;then
			(princ "\n【错误】系统中未安装通用控件Comdlg.OCX, 请安装后再运行!")
			(setq mFiles nil)
		);end_progn then
		
		(progn;else
			(setq Maxfiles 32767)
			(setq Flags (+ 4 512 524288 1048576 1024))
			(vlax-put-property WinDlg 'CancelError :vlax-true)
		  (vlax-put-property WinDlg 'MaxFileSize Maxfiles)
		  (vlax-put-property WinDlg 'Flags Flags)
		  (vlax-put-property WinDlg 'DialogTitle strTitle)
		  (vlax-put-property WinDlg 'Filter strFilter)
		  (vlax-put-property WinDlg 'InitDir strInitDir)
			(setq Catchit	nil)
			(setq Catchit	(vl-catch-all-apply '(lambda ()
																				 (vlax-invoke-method WinDlg 'ShowOpen)
																				 (setq mFiles (vlax-get WinDlg 'Filename)))))
		  (vlax-release-object WinDlg)
			(if	(not (vl-catch-all-error-p Catchit));处理"取消"错误.
				(ayFSTR->LST mFiles)
				nil;else
			);end_if
		);end_progn
	);end_if
);end_defun

;;;************************************************
;;; No.5-3-1 处理Windows多文件选择返回值 函数      
;;; 说明: 将"C:\\DWG1\0001.dwg\0002.dwg" 处理成:   
;;;        ("C:\\DWG1" "1.dwg" "2.dwg") 表形式.    
;;;************************************************
(Defun ayFSTR->LST (xMFileStr / mFileList k)
	(if (= xMFileStr "")
 		(setq mFileList nil);then
		(progn
			(if (vl-string-position (ascii "\000") xMFileStr)
			  (progn
			    (while (vl-string-position (ascii "\000") xMFileStr)
						(setq k (vl-string-position (ascii "\000") xMFileStr))
						(setq mFileList (append mFileList (list (substr xMFileStr 1 k))))
						(setq xMFileStr (substr xMFileStr (+ k 2) (- (strlen xMFileStr) k 1)))
			    );end_while
			    (setq mFileList (append mFileList (list (vl-string-left-trim "\\" xMFileStr))))
			  );end_progn then
			  (progn
			    (setq mFileList (vl-filename-directory xMFileStr))
			    (setq mFileList (list mFileList (vl-string-left-trim "\\" (vl-string-subst "" mFileList xMFileStr))))
			  );end_progn else
			);end_if
			mFileList
		);end_progn
	);end_if
);end_defun


;;;--------------------------------------------------------------;
;;;     函数：CleanReactors                                  ;
;;;--------------------------------------------------------------;
;;;     说明：用于清除反应器的  ;
;;;          通用的实用程序函数。它可以在调试时使用，
;;;          也能在关闭图形以前清除任何   ;
;;;          打开的反应器。                           ;
;;;--------------------------------------------------------------;
(defun CleanReactors ()
  (mapcar 'vlr-remove-all
         '(:VLR-AcDb-reactor
           :VLR-Editor-reactor
           :VLR-Linker-reactor
           :VLR-Object-reactor
          )
  )
)

(defun DelReactorAll (enObj / a b c )
  (foreach a (vlr-reactors :VLR-Object-Reactor) ;_ 所有对象反应器
    (foreach b (cdr a) ;_ 各类反应器对象
      (vl-some (function
                 (lambda (c)
                   (if (equal c enobj)
                     (vlr-remove b)
                   )
                 )
               )
               (vlr-owners b) ;_ 反应器拥有者对象
      )
    )
  )
)
;;直接输出组码值
(defun db_DXF (code ent / out )
	(setq out nil)
	(if (and (= 'INT (type code)) (= 'ENAME (type ent)))
		(if (setq out (assoc code (entget ent)))
			(setq out (cdr out))
		)
	)
	out 
)
(defun C:db_DXF_out (/ ent)
	(setq ent (car (entsel "\n选择对象：")))
	(dxf_out ent 0)
	(princ)
)
(defun dxf_out (ent n / e i enext)
	(setq i 0)
	(if(= 'ENAME (type ent))
		(foreach e (entget ent)
			(princ "\n")
			(repeat n (princ "    "))
			(princ i)
		  	(princ "   ")
			(princ e)
			(setq i (1+ i))
		)
	)
	(if (setq enext (entnext ent))
		(dxf_out enext (1+ n))
	)
)
;;测试
;(DelReactorAll (vlax-ename->vla-object (car(entsel "\n选择对象:"))))
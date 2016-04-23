;;;WTCAD-commonͨ�ú���

;;��3D�����һ��Ĭ�ϲ�����3D�����
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

 ;;ɾ�����б��������ظ���Ԫ��,���ָ���Ԫ��Ψһ
 (defun ListRemoveSameElement (entlist / e1 outlist)
	;;entlist �߽���а����ظ���Ԫ��,ɾ����
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

;;ʱ�����ĺ���
;;��ʼ��ʱ,���ص�ǰʱ��
(defun StartWatch ()
	(setq gl_Time (getvar "TDUSRTIMER"))
)
;;��ʾ��ǰ����ʱ��
;;���ص�ǰʱ��
(defun TimeCosted (promptstr preTime / timecost)
	(princ (strcat "\n" promptstr "��ʱ:"))
	(if preTime 
		(setq timecost (* (- (getvar "TDUSRTIMER") preTime) 86400))
		(setq timecost (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
	)
	(princ timecost)
	(princ "s.")
	(getvar "TDUSRTIMER")
)

;;��ʾ������Ϣ,���˳�
(defun *error* (msg)
	(princ "\n����:")
	(princ msg)
	(princ "�˳���")
	(exit)
)


;;��������� 2λ����
(defun getrand  ()
        (rtos (rem (getvar "CPUTICKS") 100.0) 2 0)
        )

;;0-1֮��������
(defun ZL-RAND ()     (/ (rem (getvar "CPUTICKS") 1984) 1983.0) )

;;;������
;;;m_IntersectWith
;;;���ܣ�
;;;�������󽻵�
;;;���޽��㣬����ȡ��xyƽ���ϵ�ͶӰ���㣬����ent2��zֵ
(defun m_IntersectWith (m_ent1 m_ent2 / m_obj1 m_obj2 m_objcopy1 m_objcopy2 m_jdtab m_jdtab1 i p pz2)
    ;;���ö���: Line��Polyline��LWPolyline��Circle��Arc��Ellipse��3dPolyline��Spline
    ;;֧����ռ��齻�㣬��Z����ʼ��Ϊ0.0��Ҫ��Z���꣬����(vlax-curve-getClosestPointToProjection)����
    (setq m_obj1 (vlax-ename->vla-object m_ent1)
          m_obj2 (vlax-ename->vla-object m_ent2)
          m_objcopy1 nil
          m_objcopy2 nil
          m_jdtab nil)

    (setq m_objcopy1 (vla-copy m_obj1))
    ;;���Ƶ�һ������ʵ��
    (setq m_objcopy2 (vla-copy m_obj2))
    ;;���Ƶڶ�������ʵ��

    (setq m_objcopy1 (m_ShadowToXY m_objcopy1))
    ;;�õ�ͶӰʵ��
    (setq m_objcopy2 (m_ShadowToXY m_objcopy2))
    ;;�õ�ͶӰʵ��


    (if (and m_objcopy1 m_objcopy2)
        (setq m_jdtab1 (vla-intersectwith m_objcopy1 m_objcopy2 acExtendnone)))
    ;;�õ����㼯

    (if (> (vlax-safearray-get-u-bound (vlax-variant-value m_jdtab1) 1)
           1
        ) ;_ End_>
        ;;�ж����޽���
        (progn
            (setq m_jdtab1 (vlax-safearray->list (vlax-variant-value m_jdtab1)))
            ;;safearray����ת��Ϊlist��
            (setq i 0)
            (repeat (/ (length m_jdtab1) 3)
                (setq m_jd (list (nth i m_jdtab1)
                                 (nth (+ 1 i) m_jdtab1)
                                 (nth (+ 2 i) m_jdtab1)
                           ) ;_ End_list
                ) ;_ End_setq
                ;;ȡ��һ������
                (setq m_jdtab (cons m_jd m_jdtab))
                ;;���콻���(��һ������) (�ڶ�������)��������
                (setq i (+ 3 i))
            ) ;_ End_repeat
        ) ;_ End_progn
        ;;(princ "\n�������޽���!")
    ) ;_ End_if

    (if m_objcopy1 (vla-delete m_objcopy1))
    ;;ɾ�����Ƶĵ�һ������ʵ��
    (if m_objcopy2 (vla-delete m_objcopy2))
    ;;ɾ�����Ƶĵڶ�������ʵ��

    ;;�ҵ�Z����
    (foreach p m_jdtab
        (if (setq pz2 (vlax-curve-getClosestPointToProjection m_obj2 p (list 0 0 1)))
            (setq m_jdtab (subst pz2 p m_jdtab))
            )
        );

    m_jdtab
    ;;���ؽ�����޽��㷵��nil
) ;_ End_defun

(defun m_ShadowToXY (m_obj / m_objname m_pts m_pts1 i)
    ;;������ʵ��m_obj����һ��ͶӰ��xyƽ�������ʵ�壬��������ʵ����ÿ�����Ƶ��z����ֵ��Ϊ0.0
    ;;��������ʵ��(vla����)
    ;;����ͶӰʵ��(vla����)
    (setq m_objname (vla-get-objectname m_obj))
    ;;ȡ��ʵ�����������
    ;;(m_princ "\nObjectName��" m_objname)
    (cond
        ((= "AcDbSpline" m_objname)
         ;;��������(Spline)
         (setq i 0)
         (setq m_pts (vlax-variant-value (vla-get-fitpoints m_obj)))
         ;;ȡ���������ߵ���ϵ�
         (setq m_pts1 (vlax-variant-value (vla-get-controlpoints m_obj)))
         ;;ȡ���������ߵĿ��Ƶ�
         (repeat (vla-get-numberoffitpoints m_obj)
             ;;ѭ��
             (vlax-safearray-put-element m_pts (+ i 2) 0.0)
             ;;�ı�ÿ����ϵ��zֵΪ0.0
             (setq i (+ i 3))
         ) ;_ End_repeat
         (vla-put-fitpoints m_obj m_pts)
         ;;����������ϵ�����

         (setq i 0)

         (repeat (vla-get-numberofcontrolpoints m_obj)
             ;;ѭ��
             (vlax-safearray-put-element m_pts1 (+ i 2) 0.0)
             ;;�ı�ÿ�����Ƶ��zֵΪ0.0
             (setq i (+ i 3))
         ) ;_ End_repeat
         (vla-put-controlpoints m_obj m_pts1)
         ;;�������߿��Ƶ�����
        )

        ((= "AcDb3dPolyline" m_objname)
         ;;��ά�����(3dpolyline)
         (setq i 0)
         (setq m_pts (vlax-variant-value (vla-get-coordinates m_obj)))
         ;;ȡ��3ά����ߵĿ��Ƶ�
         (repeat (/ (length (vlax-safearray->list m_pts)) 3)
             (vlax-safearray-put-element m_pts (+ i 2) 0.0)
             (setq i (+ i 3))
         ) ;_ End_repeat
         (vla-put-coordinates m_obj m_pts)
        )

        ((= "AcDbLine" m_objname)
         ;;ֱ��(line)
         (setq i 0)
         (setq m_pts (vlax-variant-value (vla-get-startpoint m_obj)))
         ;;ȡ��ֱ�ߵ��������
         (setq m_pts1 (vlax-variant-value (vla-get-endpoint m_obj)))
         ;;ȡ��ֱ�ߵĶ˵�����
         (vlax-safearray-put-element m_pts 2 0.0)
         ;;�ı��������zֵΪ0.0
         (vlax-safearray-put-element m_pts1 2 0.0)
         (vla-put-startpoint m_obj m_pts)
         (vla-put-endpoint m_obj m_pts1)
        )

        ((or (= "AcDbCircle" m_objname)
             ;;԰(circle)
             (= "AcDbArc" m_objname)
             ;;Բ��(arc)
             (= "AcDbEllipse" m_objname)
             ;;��Բ����Բ��(ellipse)
         ) ;_ End_or
         (setq m_pts (vlax-variant-value (vla-get-center m_obj)))
         ;;ȡ�����ĵ�����
         (vlax-safearray-put-element m_pts 2 0.0)
         ;;�ı����ĵ�����zֵΪ0.0
         (vla-put-center m_obj m_pts)
        )

        ((or (= "AcDbPolyline" m_objname)
             ;;�����(polyline��lwpolyline)
             (= "AcDb2dPolyline" m_objname)
             ;;��ϵ�2ά�����(polyline��lwpolyline)
         ) ;_ End_or
         (vla-put-elevation m_obj 0.0)
         ;;�ı���ֵΪ0.0
        )
    ) ;_ End_cond
    (setq m_obj m_obj)
) ;_ End_defun






;;;same point
;;;�Ƚ�P1��P2�Ƿ���ȣ�����0.0001
;;;����T or nil
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
       

;;;��չ assoc��ָ���ؼ����ڴ�һ�����е�˳��ţ���0Ϊ��㣻
;;;((a1 b1 c1 d1)(a2 b2 c2 d2)..)
;;;assoc2 a list2 0 �ȼ��� assoc a list2
;;;ȷ��list2�����index�ڷ�Χ��
(defun assoc2(key list2 index / outdata i )
   (setq outdata nil
      i 0)
   (if (and (/= nil list2) (listp list2))
      (if (= 0 index)
      	(setq outdata (assoc key list2))
	 ;;else
	 (progn
	      	(while (and (< i (length list2)) (/= key (nth index (nth i list2))));�μ�list
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

;;;���б�����Сֵ
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

;;;���б������ֵ
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
  
  ;;; �޸�����: xiaomu 2005-10-12 

;;;****************************************************************************
;;; No.5-3    Windows���ļ�ѡ��(������CADR15����) ����                         
;;; ˵��: ������ʹ��MsComDlg.Commondialog����(Comdlg.OCX)                      
;;; ����: (ayGetMultFiles "��ѡ�ļ�" "ͼ���ļ�(*.dwg)|*.dwg|����(*.*)|*.*" "") 
;;; ����: ("C:\\DWG" "7b.dwg" "7c.dwg" "1.Dwg")                                
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
			(princ "\n������ϵͳ��δ��װͨ�ÿؼ�Comdlg.OCX, �밲װ��������!")
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
			(if	(not (vl-catch-all-error-p Catchit));����"ȡ��"����.
				(ayFSTR->LST mFiles)
				nil;else
			);end_if
		);end_progn
	);end_if
);end_defun

;;;************************************************
;;; No.5-3-1 ����Windows���ļ�ѡ�񷵻�ֵ ����      
;;; ˵��: ��"C:\\DWG1\0001.dwg\0002.dwg" �����:   
;;;        ("C:\\DWG1" "1.dwg" "2.dwg") ����ʽ.    
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
;;;     ������CleanReactors                                  ;
;;;--------------------------------------------------------------;
;;;     ˵�������������Ӧ����  ;
;;;          ͨ�õ�ʵ�ó��������������ڵ���ʱʹ�ã�
;;;          Ҳ���ڹر�ͼ����ǰ����κ�   ;
;;;          �򿪵ķ�Ӧ����                           ;
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
  (foreach a (vlr-reactors :VLR-Object-Reactor) ;_ ���ж���Ӧ��
    (foreach b (cdr a) ;_ ���෴Ӧ������
      (vl-some (function
                 (lambda (c)
                   (if (equal c enobj)
                     (vlr-remove b)
                   )
                 )
               )
               (vlr-owners b) ;_ ��Ӧ��ӵ���߶���
      )
    )
  )
)
;;ֱ���������ֵ
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
	(setq ent (car (entsel "\nѡ�����")))
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
;;����
;(DelReactorAll (vlax-ename->vla-object (car(entsel "\nѡ�����:"))))
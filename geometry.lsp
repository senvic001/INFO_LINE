(vl-load-com)
;*********************************************************************************************
;��������:GetConnectEntity(x y enttype)
;���ܣ����x y �ཻ��ʵ�壬����ѡ������ߵ����ӹ�ϵ
;������x y enttype ("LINE")
;���أ�nil or (entname1 entname2)
;����ʱ�䣺2014/07/18   17:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun GetConnectEntity(x y enttype / ename  entset  i px3  return p0 p1)
	
    ; (setq
        ; px3 (list x y 0)
        ; return nil
        ; i 0
        ; entset nil
      ; ) 
    ; ;;��ȷѡ���
  ; (if (= enttype "LINE")
      ; (setq entset (ssget "X" (list (cons -4 "<AND") (cons 0 enttype)
                                    ; (cons -4 "<OR")
                                    ; (cons -4 "=,=,*") (cons 10 px3) (cons -4 "=,=,*") (cons 11 px3)
                                    ; (cons -4 "OR>")
                                    ; (cons -4 "AND>"))))
      ; (setq entset (ssget "X" (list (cons -4 "<AND") (cons 0 enttype)
                                    ; (cons -4 "=,=,*") (cons 10 px3)
                                    ; (cons -4 "AND>"))))
      ; )

   ; (if	entset
		; (repeat	(sslength entset)
			; (setq ename	 (ssname entset i)
				  ; return (cons ename return )
				  ; i		 (1+ i)
			; ) ;_ end_setq
		; ) ;_ end_repeat
	; ) ;_ end_if
	; return
	(GetConnectEntity-Index x y enttype)
   )
   
;;ʹ��Index�ķ�ʽ���ҵ�.
(defun GetConnectEntity-Index	(x y enttype / outlist datalist e)
	(setq outlist nil myRoot nil)
	(cond 
		((= enttype "LINE") (setq myRoot (GetLineRoot nil)))
		((= enttype "INSERT") (setq myRoot (GetInsertRoot nil)))
		(T (setq myRoot (GetAllPointRoot nil)))
	)
	(setq datalist (SearchPoint myRoot x y))
	(if (> (length datalist) 0)
		(progn
			(foreach e datalist
				(setq e (caddr e))
				(if (not (vlax-erased-p e))
					(setq outlist (cons e outlist))
					(cond 
						((= enttype "LINE") (setq gl_LineSpaceIndex (DelEntityIndex myRoot e x y)))
						((= enttype "INSERT") (setq gl_PointSpaceIndex (DelEntityIndex myRoot e x y)))
						(T (setq gl_AllPointSpaceIndex (DelEntityIndex myRoot e x y)))
					)
				)
			)
			(setq outlist (ListRemoveSameElement outlist))	;;�߶�Root�����ظ���ʵ��.ÿ���߶���������,enameһ��.
		)
	)
	outlist
) ;_ end_defun   
   
 ;;;��ȡ���ڷ�Χ�ڵĵ�ʵ��
 ;;;���ڲ�ͼ���ƹ��� 
 ;;;X y ������������ range-��Χֵ typestr-ʵ�������ַ�"INSERT"
 ;;;����:nil or ��Χ������ʵ���
(defun GetWndPoint(x y range typestr /  ename entset i px1 px2 return hrange)
    55
	(setq
		hrange (/ range 2.0)
        px1 (list (- x hrange)(- y hrange ))
		px2 (list (+ x hrange)(+ y hrange ))
        return nil
        i 0
        entset nil
      ) 
	(setq entset (ssget "C" px1 px2 (list (cons 0 typestr )(list -3 (list gl_AppName)))
					)
	)
    (if entset
        (repeat  (sslength entset)
        	(setq ename (ssname entset i)
                      return (cons ename return )
                      i (1+ i)
                      )
            )
        )
   return
   )

;;;�õ��õ㣬������ʵ��
;;;(defun GetALLConnectPoints(x y / ename entlist entset  i p0 p1 px range return x1 x2 y1 y2)
;;;   (setq px (list x y)
;;;      return nil
;;;      i 0
;;;      entset (ssget "X" (list (cons 0 "TEXT") (cons 0 "INSERT")))) ;ѡ������enttype����
;;;   (if (/= nil entset)
;;;      (repeat  (sslength entset)
;;;          (setq ename (ssname entset i)
;;;              entlist (entget ename)
;;;              p0 (cdr (assoc 10 entlist))
;;;              p0 (list (car p0) (cadr p0));x y
;;;              )
;;;          (if (= enttype "TEXT")
;;;              (progn
;;;		      (setq p1 (cdr (assoc 11 entlist))
;;;	                  p1 (list (car p1) (cadr p1)))
;;;	              (if (or (equal p0 px) (equal p1 px))
;;;	                  (setq return (append return (list  ename)))
;;;	                  )
;;;                  );progn
;;;              ;;esle INSERT TEXT
;;;              (Progn
;;;	              (if (and (< (abs (- (car p0) x)) gl_MIN) (< (abs (- (cadr p0) y)) gl_MIN));;ͬһ����
;;;	                  (setq return (append return (list  ename)))
;;;	                  );if
;;;              );progn
;;;             );if
;;;	    (setq i (1+ i))
;;;	    );repeat	
;;;      );if
;;;   return
;;;   )


;;���ݵ����������������߶ε���һ���˵�ʵ����߶��б�
;;���أ�((p1 Line1) (p2 Line2)...) or nil
;;enttype:TEXT INSERT
(defun GetConnectPoints	(x y enttype / entlist entname linelist	outlist	p p0 p1	pointlist)
	;;1�������߶�
	(setq linelist (GetConnectEntity x y "LINE")
		  outlist  nil
	) ;_ end_setq
	;;2�߶ε���һ���˵�
	(if	(/= nil linelist)
		(foreach entname linelist
			(setq entlist (entget entname)
				  p0	  (cdr (assoc 10 entlist))
				  p1	  (cdr (assoc 11 entlist))
				  p		  nil
			) ;_ end_setq
			;;1find the different point coordination
			(if	(< (distance (list (car p0) (cadr p0)) (list x y)) gl_MIN)
				(setq p p1)
 				;;else
				(setq p p0)
			) ;if
			;;2find the other point entity;only one point commonly
			(setq pointlist (GetConnectEntity (car p) (cadr p) enttype))
			;;3append the entities to a list,
			(if	(> (length pointlist) 0)
				(setq outlist (append outlist (list (list (car pointlist) entname))))
 					;;(princ (strcat "\nError��" (rtos (length pointlist))))
			) ;_ end_if
		) ;foreach
	) ;if
	outlist
) ;_ end_defun



;;;��ʾ���ڱ༭�ĵ��߶ε����һ�봰��
;;;�����������ʵ�����꣬real 
(defun ZoomWndtoLeft(p1 p2 / app xleft ylow xright yhigh width0 height0)
    (setq app (vlax-get-acad-object)
          xleft (min (car p1) (car p2))
          ylow (min (cadr p1) (cadr p2))
          xright (max (car p1) (car p2))
          yhigh (max (cadr p1) (cadr p2))

          width0 (+ 20 (- xright xleft))
          height0 (+ 10 (- yhigh ylow))
          xleft (- xleft (/ width0 10))
          ylow (- ylow (/ height0 10))
          xright (+ xright (max  width0 height0))
          yhigh (+ yhigh (/ height0 10))
          )
    (vla-zoomwindow app (vlax-3D-point xleft ylow 0) (vlax-3D-point xright yhigh 0))
    )
	
;;
(defun Point3D->2D (pt)
	(list (car pt) (cadr pt))
)
(defun Point2D->3D (pt z)
	(list (car pt) (cadr pt) z)
)
;;;�Ƿ���ͬһ����
;;;p1 p2 ��3D��(x y z)
(defun IsSamePoint(pt1 pt2 / return)
	(setq return nil)
	(if (and (listp pt1) (listp pt2))
		(if (< (distance pt1 pt2 ) gl_MIN)
			(setq return T)
		)
	)	
	return
)

;;;�ж�����ƽ���Ƿ��غ�
;;;pt1 pt2 ����Ϊ����
(defun IsSameXY(pt1 pt2 / return)
	(setq return nil)
	(if (and (listp pt1) (listp pt2))
		(if (< (distance  (list (car pt1) (cadr pt1)) (list (car pt2) (cadr pt2))) gl_MIN)
			(setq return T)
		)
	)	
	return
)
;;�ж�������ͬʵ���Ƿ�λ����ͬ,������ͬ
 (defun IsSameEntity(ent1 ent2 / elist1 elist2 p10 p11 p20 p21 ret type1 type2)
	(setq ret nil)
	(if (/= ent1 ent2)	;;ͬһ��ʵ��,�������ж�(�߶�����,ÿ��ʵ��������)
		(if (and (= 'ENAME (type ent1)) (= 'ENAME (type ent2)))
			(progn
				(setq elist1 (entget ent1)
					p10 (cdr (assoc 10 elist1))
					type1 (cdr (assoc 0 elist1))
					elist2 (entget ent2)
					p20 (cdr (assoc 10 elist2))
					type2 (cdr (assoc 0 elist2))
				)
				(if (= type1 type2)
					(cond
						((= "LINE" type1) 
							(progn
								(setq p11 (cdr (assoc 11 elist1))
									p21 (cdr (assoc 11 elist2))
								)
								(if (or (and (IsSameXY p10 p20) (IsSameXY p11 p21))
										(and (IsSameXY p10 p21) (IsSameXY p11 p20)))
									(setq ret T)
								) 
							)
						)
						((or (= "INSERT" type1) (= "TEXT" type1))
							(if (IsSameXY p10 p20)
								(setq ret T)
							)
						) 
					)
				)
			)
		)
	)
	
	ret
 )
 
;*********************************************************************************************
;��������:UpdateTopo()
;;;����:1.��������֮��Ĺ�ϵ,�ѱ�(p1 p2 line)���浽ldata "Edge"֮��
;;;		2.����:δ����˵���߶�,�˵��ظ����߶�
;;;����:��
;;;����:��
;;;����ʱ�䣺2014/12/16   12:40
;;;�޸�ʱ�䣺
;;;�����ˣ����۾�
;*********************************************************************************************
(defun UpdateTopo( /  alllinelist alllineset allpointset dlist e ecolor edge edge1 edge2 el elist1
			   elist2 emptylinelist endp ent entlist entp1 entp2 eobj flist glist handle handle1
			   handle2 hlist i jlist  len lenall linelist llist lnum nline plist pname1 pname2
			   pos1 pos2 qlist repeatpointlist rlist startp tname typelist typestr wlist xlist ylist 
			   zlist hstr errorstr err imingxian)
	(setq 
		;Jlist nil Plist nil Wlist nil Ylist nil Qlist nil Llist nil Dlist nil Xlist nil
		;Hlist nil Rlist nil Glist nil Flist nil Zlist nil
		AllLineSet (ssget "X" (list (cons 0 "LINE")))
		AllPointSet (ssget "X" (list (cons 0 "INSERT")))
		i 0
		lnum 0
		AllLineList nil
		repeatPointList nil 	;�˵��ظ��ĵ���߶�
		emptyLineList nil		;�˵��޹��ߵ���߶�
	)
	(prompt "\nͳ�ƽ��:")
	(setq imingxian 0)
	;;���߶η���,���,��д������ı�
	(if AllPointSet
        (repeat (sslength AllPointSet)
			;;ɾ����
			(setq entp (ssname AllPointSet i))
			(ldata-delete entp "Edge")
			(setq i (1+ i))
		)
	)
	;;reset spaceindex
	; (setq gl_LineSpaceIndex (GetLineRoot T)
		; gl_PointSpaceIndex (GetInsertRoot T)
	; )
	(C:ResetSpaceIndex)
	(setq i 0)
	(if AllLineSet 
		(repeat (sslength AllLineSet)
			(setq ent (ssname AllLineSet i))
			(if  (vlax-ldata-get ent gl_AppName)
				(progn
					(setq tname  (ldata-get ent "Main_Type")
						entlist (entget ent)
						pos1 (cdr (assoc 10 entlist))
						pos2 (cdr (assoc 11 entlist))
						handle (cdr (assoc 5 entlist));���
						lnum (1+ lnum))
					(ldata-delete ent "Edge")
					
					;;������߹�ϵ��ʵ��,�����߶��ڵ�����.�յ�� ��ű�
					(setq elist1 (GetConnectEntity (car pos1) (cadr pos1) "INSERT")
						len1 (length elist1)
						elist2 (GetConnectEntity (car pos2) (cadr pos2) "INSERT")
						len2 (length elist2)
					)
					(if (> len1 1) (setq repeatPointList (cons (list (car elist1) ent) repeatPointList)))
					(if (> len2 1) (setq repeatPointList (cons (list (car elist2) ent) repeatPointList)))
					(if (or (= len2 0)(= len1 0)) (setq emptyLineList (cons ent emptyLineList)))
					(if (and (= len1 1) (= len2 1))
						(progn
							(setq entp1 (car elist1)
								;pname1 (car (GetLPointMapNo entp1))
								pname1 (ldata-get entp1 "Map_No")
								handle1 (cdr (assoc 5 (entget entp1)))
								entp2 (car elist2)
								;pname2 (car (GetLPointMapNo entp2))
								pname2 (ldata-get entp2 "Map_No")
								handle2 (cdr (assoc 5 (entget entp2)))
								edge nil
							)
							;;
							;;(ldata-put ent "Start_Point" pname1)
							;(ldata-put entp1 "Map_No" pname1)
							;(ldata-put entp1 "Exp_No" pname1)
							
							;(ldata-put entp2 "Map_No" pname2)
							;(ldata-put entp2 "Exp_No" pname2)
							;;(ldata-put ent "End_Point" pname2)
							(setq 
								startp (ldata-get ent "Start_Point")
								endp   (ldata-get ent "End_Point")
							)
							;;(setq edge (list handle1 handle2 handle))
							;;
							(if (= startp pname1) 
								(setq edge (list handle1 handle2 handle))
								(setq edge (list handle2 handle1 handle))
							)
							(if (setq edge1 (ldata-get entp1 "Edge"))
								(if (not(member edge edge1))
									(ldata-put entp1 "Edge" (cons edge edge1))
								)
								(ldata-put entp1 "Edge" (list edge))
							)
							(if (setq edge2 (ldata-get entp2 "Edge"))
								(if (not(member edge edge2))
									(ldata-put entp2 "Edge" (cons edge edge2))
								)
								(ldata-put entp2 "Edge" (list edge))
							)
							(ldata-put ent "Edge" edge)
						)
					)
				)
			)
			(setq i (1+ i))
		)
	)
	
	;;���ͳ����Ϣ
	;;�߶�����
	(prompt (strcat "\n�߶�����:" (rtos i 2 0 ) ".\t�в����Ĺ��߶�����:" (rtos lnum 2 0) "."))
	(if (> (length emptyLineList) 0)
		(progn
			(foreach e emptyLineList
				(setq eobj (vlax-ename->vla-object e)
					ecolor (vla-get-truecolor eobj)
					hstr (vla-get-handle eobj)
					errorstr "Line_Error:���߶�����һ���˵�δ����ߵ�����."
				)
				;;�Ѵ����������б�
				(if (setq err (assoc hstr gl_ErrorList))
					(if (/= errorstr (cdr err)) (setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList)))
					(setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList))
				)
				(vla-put-colorindex ecolor acWhite)
                (vla-put-truecolor eobj ecolor)
			)
			(prompt (strcat "\n����:" (rtos (length emptyLineList) 2 0 ) "���߶εĶ˵㲻�ǹ��ߵ�,�߶���ɫ�Ѹ�Ϊ��ɫ!"))
		)
	)
	(if (> (length repeatPointList) 0)
		(progn
			(foreach e repeatPointList
				(setq eobj (vlax-ename->vla-object (cadr e))
					ecolor (vla-get-truecolor eobj)
					hstr (vla-get-handle eobj)
					errorstr (strcat "Line_Error:���߶�һ���˵�" (car (GetLPointMapNo (car e)))"�����ظ��Ĺ��ߵ�.")
				)
				;;�Ѵ����������б�
				(if (setq err (assoc hstr gl_ErrorList))
					(if (/= errorstr (cdr err)) (setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList)))
					(setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList))
				)
				(vla-put-colorindex ecolor acCyan)
                (vla-put-truecolor eobj ecolor)
			)
			(prompt (strcat "\n����:" (rtos (length repeatPointList) 2 0 ) "���߶εĶ˵�����ظ��ĵ�,�߶���ɫ�Ѹ�Ϊ��ɫ!"))
		)
	)
    (prin1)
)


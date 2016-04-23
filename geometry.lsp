(vl-load-com)
;*********************************************************************************************
;函数定义:GetConnectEntity(x y enttype)
;功能：与点x y 相交的实体，用于选择点与线的连接关系
;参数：x y enttype ("LINE")
;返回：nil or (entname1 entname2)
;创建时间：2014/07/18   17:00
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun GetConnectEntity(x y enttype / ename  entset  i px3  return p0 p1)
	
    ; (setq
        ; px3 (list x y 0)
        ; return nil
        ; i 0
        ; entset nil
      ; ) 
    ; ;;精确选择点
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
   
;;使用Index的方式查找点.
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
			(setq outlist (ListRemoveSameElement outlist))	;;线段Root包含重复的实体.每个线段有两个点,ename一样.
		)
	)
	outlist
) ;_ end_defun   
   
 ;;;获取窗口范围内的点实体
 ;;;用于草图绘制过程 
 ;;;X y 窗口中心坐标 range-范围值 typestr-实体类型字符"INSERT"
 ;;;返回:nil or 范围内所有实体表
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

;;;得到该点，插入点的实体
;;;(defun GetALLConnectPoints(x y / ename entlist entset  i p0 p1 px range return x1 x2 y1 y2)
;;;   (setq px (list x y)
;;;      return nil
;;;      i 0
;;;      entset (ssget "X" (list (cons 0 "TEXT") (cons 0 "INSERT")))) ;选择所有enttype对象
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
;;;	              (if (and (< (abs (- (car p0) x)) gl_MIN) (< (abs (- (cadr p0) y)) gl_MIN));;同一个点
;;;	                  (setq return (append return (list  ename)))
;;;	                  );if
;;;              );progn
;;;             );if
;;;	    (setq i (1+ i))
;;;	    );repeat	
;;;      );if
;;;   return
;;;   )


;;根据点坐标查找相关联的线段的另一个端点实体和线段列表
;;返回：((p1 Line1) (p2 Line2)...) or nil
;;enttype:TEXT INSERT
(defun GetConnectPoints	(x y enttype / entlist entname linelist	outlist	p p0 p1	pointlist)
	;;1相连的线段
	(setq linelist (GetConnectEntity x y "LINE")
		  outlist  nil
	) ;_ end_setq
	;;2线段的另一个端点
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
 					;;(princ (strcat "\nError：" (rtos (length pointlist))))
			) ;_ end_if
		) ;foreach
	) ;if
	outlist
) ;_ end_defun



;;;显示正在编辑的点线段到左侧一半窗口
;;;输入两个点的实际坐标，real 
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
;;;是否是同一个点
;;;p1 p2 是3D点(x y z)
(defun IsSamePoint(pt1 pt2 / return)
	(setq return nil)
	(if (and (listp pt1) (listp pt2))
		(if (< (distance pt1 pt2 ) gl_MIN)
			(setq return T)
		)
	)	
	return
)

;;;判断两点平面是否重合
;;;pt1 pt2 必须为坐标
(defun IsSameXY(pt1 pt2 / return)
	(setq return nil)
	(if (and (listp pt1) (listp pt2))
		(if (< (distance  (list (car pt1) (cadr pt1)) (list (car pt2) (cadr pt2))) gl_MIN)
			(setq return T)
		)
	)	
	return
)
;;判断两个不同实体是否位置相同,类型相同
 (defun IsSameEntity(ent1 ent2 / elist1 elist2 p10 p11 p20 p21 ret type1 type2)
	(setq ret nil)
	(if (/= ent1 ent2)	;;同一个实体,不进行判断(线段索引,每个实体两个点)
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
;函数定义:UpdateTopo()
;;;功能:1.建立点线之间的关系,把边(p1 p2 line)保存到ldata "Edge"之中
;;;		2.发现:未定义端点的线段,端点重复的线段
;;;参数:无
;;;返回:无
;;;创建时间：2014/12/16   12:40
;;;修改时间：
;;;创建人：沈雄君
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
		repeatPointList nil 	;端点重复的点和线段
		emptyLineList nil		;端点无管线点的线段
	)
	(prompt "\n统计结果:")
	(setq imingxian 0)
	;;管线段分类,检查,并写入关联的边
	(if AllPointSet
        (repeat (sslength AllPointSet)
			;;删除边
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
						handle (cdr (assoc 5 entlist));句柄
						lnum (1+ lnum))
					(ldata-delete ent "Edge")
					
					;;保存点线关系到实体,按照线段内的起点号.终点号 存放边
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
	
	;;输出统计信息
	;;线段总数
	(prompt (strcat "\n线段总数:" (rtos i 2 0 ) ".\t有参数的管线段总数:" (rtos lnum 2 0) "."))
	(if (> (length emptyLineList) 0)
		(progn
			(foreach e emptyLineList
				(setq eobj (vlax-ename->vla-object e)
					ecolor (vla-get-truecolor eobj)
					hstr (vla-get-handle eobj)
					errorstr "Line_Error:管线段至少一个端点未与管线点连接."
				)
				;;把错误加入错误列表
				(if (setq err (assoc hstr gl_ErrorList))
					(if (/= errorstr (cdr err)) (setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList)))
					(setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList))
				)
				(vla-put-colorindex ecolor acWhite)
                (vla-put-truecolor eobj ecolor)
			)
			(prompt (strcat "\n错误:" (rtos (length emptyLineList) 2 0 ) "条线段的端点不是管线点,线段颜色已改为白色!"))
		)
	)
	(if (> (length repeatPointList) 0)
		(progn
			(foreach e repeatPointList
				(setq eobj (vlax-ename->vla-object (cadr e))
					ecolor (vla-get-truecolor eobj)
					hstr (vla-get-handle eobj)
					errorstr (strcat "Line_Error:管线段一个端点" (car (GetLPointMapNo (car e)))"包含重复的管线点.")
				)
				;;把错误加入错误列表
				(if (setq err (assoc hstr gl_ErrorList))
					(if (/= errorstr (cdr err)) (setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList)))
					(setq gl_ErrorList (cons (cons hstr errorstr) gl_ErrorList))
				)
				(vla-put-colorindex ecolor acCyan)
                (vla-put-truecolor eobj ecolor)
			)
			(prompt (strcat "\n错误:" (rtos (length repeatPointList) 2 0 ) "条线段的端点包含重复的点,线段颜色已改为青色!"))
		)
	)
    (prin1)
)


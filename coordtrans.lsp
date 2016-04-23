;;坐标转换
;;四参数转换
 ;*********************************************************************************************
 ;函数定义:FourParaTrans()
 ;功能：四参数转换
 ;参数：pts:点表（x1 y1 z1 x2 y2 z2...) Dim 维度Dim=2 or 3	,DX DY K ,A单位为秒″
 ;返回：校正后的点表
 ;创建时间：2016/03/28   12:16
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun C:FourParaTrans( / a allpointset dx dy  id k path std)
	(setq DX -5.606204740411
		DY 2.075870294939
		K 1.000001514714
		A -0.171235)
	(defun getdata()
		(setq DX  (atof (get_tile "bt_dx"))
			DY (atof (get_tile "bt_dy"))
			K	(atof (get_tile "bt_k"))
			A	(atof (get_tile "bt_a"))
		)
	)
	(defun setdata (DX DY K A)
		(set_tile "bt_dx" (rtos DX 2 30))
		(set_tile "bt_dy" (rtos DY 2 30))
		(set_tile "bt_k" (rtos K 2 30))
		(set_tile "bt_a" (rtos A 2 30))
	)
	
	
	
	(setq path (strcat gl_INFO_LINE_PATH "dlg\\fourpara.dcl"))
	(setq id (load_dialog path)) ;装入对话框文件
    (if (< id 0)
        (exit)
    ) ;_ End_if
	
	(if (not (new_dialog "dlg_fourpara" id)) (*error* (strcat "打开对话框" path)))
	(setdata DX DY K A)
	(action_tile "accept" "(getdata)(done_dialog 1)")
	(action_tile "cancel" "(done_dialog 0)")
	
	(setq std (start_dialog))
	(unload_dialog id)
	
	(if (= std 1)
		(progn
			(prompt "\n 开始进行四参数转换：")
			;;点对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "POINT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个POINT实体。"))
				)
			)
			;;圆对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "CIRCLE"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个CIRCLE实体。"))
				)
			)	
			;;圆弧对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "ARC"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个ARC实体。"))
				)
			)	
			;;椭圆对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "ELLIPSE"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个ELLIPSE实体。"))
				)
			)	
			;;INSERT对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "INSERT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个INSERT实体。"))
				)
			)	
			;;TEXT对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "TEXT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个TEXT实体。"))
				))
				
			;;MTEXT对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "MTEXT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个MTEXT实体。"))
				)
			)	
			;;RAY对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "RAY"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个RAY实体。"))
				)
			)
			;;XLINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "XLINE"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个XLINE实体。"))
				)
			)
			;;LINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "LINE"))))
				(progn
					(FourParaTrans_Line AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个LINE实体。"))
				)
			)		
			;;SPLINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "SPLINE"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个SPLINE实体。"))
				)
			)		
			;;LWPOLYLINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "LWPOLYLINE"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个LWPOLYLINE实体。"))
				))	
			;;POLYLINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "POLYLINE"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个POLYLINE实体。"))
				))	
			;;HATCH对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "HATCH"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n 转换了" (rtos (sslength AllPointSet) 2 0) "个HATCH实体。"))
				))	
		(C:ResetSpaceIndex)
		)
	)
	(prin1)
)


;;适用于POINT TEXT MTEXT CIRCLE ARC ELLIPSE RAY INSERT
(defun FourParaTrans_Point(entityset DX DY K A / a0 m1 m2 p0 X Y X1 Y1 ent elist i)
	(setq a0 (/  (/ (* A PI) 180) 3600)
		m1 (* K (cos a0))
		m2 (* K (sin a0))
		i 0
	)
	(repeat (sslength entityset)
		(setq ent (ssname entityset i)
			i (1+ i)
			elist (entget ent)
			p0 (cdr (assoc 10 elist))
			Y (car p0)	;;东投影坐标
			X (cadr p0)	;;北投影坐标
			X1 (- (+ DX (* m1 X)) (* m2 Y))
			Y1 (+ DY (* m1 Y) (* m2 X))
			obj (vlax-ename->vla-object ent)
		)
		;;(entmod (subst (cons 10 (list Y1 X1 (caddr p0))) (cons 10 p0) elist))
		(vla-move (vlax-ename->vla-object ent) (vlax-3D-point p0) (vlax-3D-point (list Y1 X1 (caddr p0))))
	)
)

(defun FourParaTrans_Line(entityset DX DY K A / a0 m1 m2 p0 X Y X1 Y1 ent elist i)
	(setq a0 (/  (/ (* A PI) 180) 3600)
		m1 (* K (cos a0))
		m2 (* K (sin a0))
		i 0
	)
	(repeat (sslength entityset) 
		(setq ent (ssname entityset i)
			i (1+ i)
			elist (entget ent)
			p0 (cdr (assoc 10 elist))
			Y (car p0)	;;东投影坐标
			X (cadr p0)	;;北投影坐标
			X1 (- (+ DX (* m1 X)) (* m2 Y))
			Y1 (+ DY (* m1 Y) (* m2 X))
			elist (subst (cons 10 (list Y1 X1 (caddr p0))) (cons 10 p0) elist)
		)
		(setq  p0 (cdr (assoc 11 elist))
			Y (car p0)	;;东投影坐标
			X (cadr p0)	;;北投影坐标
			X1 (- (+ DX (* m1 X)) (* m2 Y))
			Y1 (+ DY (* m1 Y) (* m2 X))
			elist (subst (cons 11 (list Y1 X1 (caddr p0))) (cons 11 p0) elist)
		)
		(entmod elist)
	)
)


;;适用于LWPOLYLINE  POLYLINE HATCH
(defun FourParaTrans_LWPOLYLINE (entityset DX DY K A / a0 m1 m2 p0 p10 remain X Y X1 Y1 ent elist i tname ne elist1)
	(setq a0 (/  (/ (* A PI) 180) 3600)
		m1 (* K (cos a0))
		m2 (* K (sin a0))
		i 0
	)
	(repeat (sslength entityset) 
		(setq ent (ssname entityset i)
			i (1+ i)
			elist (entget ent)
			p0 (assoc 10 elist)
			tname (cdr (assoc 0 elist))
			remain elist
		)
		(while p0
			(setq p10 (cdr p0)
				Y (car p10)	;;东投影坐标
				X (cadr p10)	;;北投影坐标
				X1 (- (+ DX (* m1 X)) (* m2 Y))
				Y1 (+ DY (* m1 Y) (* m2 X))
			)
			
			(cond
				((= tname "LWPOLYLINE")
					(setq elist (subst (cons 10 (list Y1 X1)) p0 elist))
				)
				((= tname "SPLINE")
					(setq elist (subst (cons 10 (list Y1 X1 (caddr p10))) p0 elist))
				)
				((= tname "POLYLINE") 
					(if (setq ne (entnext ent))
						(while (= "VERTEX" (db_DXF 0 ne))
							(progn
								(setq elist1 (entget ne) 
									p10 (cdr (assoc 10 elist1))
									Y (car p10)	;;东投影坐标
									X (cadr p10)	;;北投影坐标
									X1 (- (+ DX (* m1 X)) (* m2 Y))
									Y1 (+ DY (* m1 Y) (* m2 X))
									ne (entnext ne)
								)
								(setq elist1 (subst (cons 10 (list Y1 X1 (caddr p10))) (cons 10 p10) elist1))
								(entmod elist1)
							)
						)
					)
				)
				(T (setq elist (subst (cons 10 (list Y1 X1 (caddr p10))) p0 elist)))
			)
			
			(setq remain (cdr(member p0 remain))
				p0 (assoc 10 remain)
			)
		)
		(entmod elist)
	)
)


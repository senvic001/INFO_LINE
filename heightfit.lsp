;;高程拟合
 ;*********************************************************************************************
 ;函数定义:FourParaTrans()
 ;功能：四参数拟合
 ;参数：pts:点表（x1 y1 z1 x2 y2 z2...) Dim 维度Dim=2 or 3	,DX DY K ,A单位为秒″
 ;返回：校正后的点表
 ;创建时间：2016/03/28   12:16
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun C:HeightFitting( / a0 a1 a2 a3 a4 a5 allpointset id path std x0 y0)
	(setq A0 0.166504888205221
		A1 -0.0000225913784938742
		A2 0.0000558523716532913
		A3 4.3723019265859E-09
		A4 8.46104622478209E-09
		A5 2.78937489272237E-08
		X0 3415098.07257143
		Y0 506465.30195)
	(defun getdata()
		(setq A0  (atof (get_tile "bt_a0"))
			A1 (atof (get_tile "bt_a1"))
			A2	(atof (get_tile "bt_a2"))
			A3	(atof (get_tile "bt_a3"))
			A4	(atof (get_tile "bt_a4"))
			A5	(atof (get_tile "bt_a5"))
			X0	(atof (get_tile "bt_x0"))
			Y0	(atof (get_tile "bt_y0"))
		)
	)
	(defun setdata (A0 A1 A2 A3 A4 A5 X0 Y0)
		(set_tile "bt_a0" (rtos A0 2 30))
		(set_tile "bt_a1" (rtos A1 2 30))
		(set_tile "bt_a2" (rtos A2 2 30))
		(set_tile "bt_a3" (rtos A3 2 30))
		(set_tile "bt_a4" (rtos A4 2 30))
		(set_tile "bt_a5" (rtos A5 2 30))
		(set_tile "bt_x0" (rtos X0 2 30))
		(set_tile "bt_y0" (rtos Y0 2 30))
	)
	
	
	
	(setq path (strcat gl_INFO_LINE_PATH "dlg\\高程拟合.dcl"))
	(setq id (load_dialog path)) ;装入对话框文件
    (if (< id 0)
        (exit)
    ) ;_ End_if
	
	(if (not (new_dialog "dlg_heightfit" id)) (*error* (strcat "打开对话框" path)))
	(setdata A0 A1 A2 A3 A4 A5 X0 Y0)
	(action_tile "accept" "(getdata)(done_dialog 1)")
	(action_tile "cancel" "(done_dialog 0)")
	
	(setq std (start_dialog))
	(unload_dialog id)
	
	(if (= std 1)
		(progn
			(prompt "\n 开始进行高程拟合：")
			;;点对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "POINT"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个POINT实体Z坐标。"))
				)
			)
			;;圆对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "CIRCLE"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个CIRCLE实体Z坐标。"))
				)
			)	
			;;圆弧对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "ARC"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个ARC实体Z坐标。"))
				)
			)	
			;;椭圆对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "ELLIPSE"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个ELLIPSE实体Z坐标。"))
				)
			)	
			;;INSERT对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "INSERT"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个INSERT实体Z坐标。"))
				)
			)	
			;;TEXT对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "TEXT"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个TEXT实体Z坐标。"))
				))
				
			;;MTEXT对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "MTEXT"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个MTEXT实体Z坐标。"))
				)
			)	
			;;RAY对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "RAY"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个RAY实体Z坐标。"))
				)
			)
			;;XLINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "XLINE"))))
				(progn
					(HeightFitting_POINT AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个XLINE实体Z坐标。"))
				)
			)
			;;LINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "LINE"))))
				(progn
					(HeightFitting_LINE AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个LINE实体Z坐标。"))
				)
			)		
			;;SPLINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "SPLINE"))))
				(progn
					(HeightFitting_SPLINE AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个SPLINE实体Z坐标。"))
				)
			)		
			
			;;POLYLINE对象
			(if (setq AllPointSet (ssget "X" (list (cons 0 "POLYLINE"))))
				(progn
					(HeightFitting_POLYLINE AllPointSet A0 A1 A2 A3 A4 A5 X0 Y0)
					(prompt (strcat "\n 拟合了" (rtos (sslength AllPointSet) 2 0) "个POLYLINE实体Z坐标。"))
				))	
		(C:ResetSpaceIndex)
		)
	)
	(prin1)
)

;;返回校正后的三维点
(defun heightfit_3D (p0 A0 A1 A2 A3 A4 A5 X0 Y0 / dx dy dh)
	(setq dx (- (cadr p0) X0)
		dy (- (car p0) Y0)
	)
	(setq dh (+  A0 (* A1 dx) (* A2 dy) (* A3 dx dx) (* A4 dy dy) (* A5 dx dy)))
	(list (car p0) (cadr p0) (- (caddr p0) dh))
)
;;适用于POINT TEXT MTEXT CIRCLE ARC ELLIPSE RAY INSERT
(defun HeightFitting_POINT(entityset A0 A1 A2 A3 A4 A5 X0 Y0 / p0 p1 ent elist i)
	(setq i 0)
	(repeat (sslength entityset)
		(setq ent (ssname entityset i)
			i (1+ i)
			elist (entget ent)
			p0 (cdr (assoc 10 elist))
			p1 (heightfit_3D p0 A0 A1 A2 A3 A4 A5 X0 Y0)
		)
		;;(entmod (subst (cons 10 (list Y1 X1 (caddr p0))) (cons 10 p0) elist))
		(vla-move (vlax-ename->vla-object ent) (vlax-3D-point p0) (vlax-3D-point p1))
	)
)


(defun HeightFitting_LINE(entityset A0 A1 A2 A3 A4 A5 X0 Y0 / p10 p11 p20 p21 ent elist i)
	(setq i 0)
	(repeat (sslength entityset) 
		(setq ent (ssname entityset i)
			i (1+ i)
			elist (entget ent)
			p10 (cdr (assoc 10 elist))
			p11 (cdr (assoc 11 elist))
			p20 (heightfit_3D p10 A0 A1 A2 A3 A4 A5 X0 Y0)
			p21 (heightfit_3D p11 A0 A1 A2 A3 A4 A5 X0 Y0)
			elist (subst (cons 10 p20) (cons 10 p10) elist)
			elist (subst (cons 11 p21) (cons 11 p11) elist)
		)
		(entmod elist)
		;(vla-move (vlax-ename->vla-object ent) (vlax-3D-point p0) (vlax-3D-point p1))
	)
)

;;适用于 POLYLINE 
(defun HeightFitting_POLYLINE(entityset A0 A1 A2 A3 A4 A5 X0 Y0 / p10  p20 ne elist1 ent elist i)
	(setq i 0)
	(repeat (sslength entityset)
		(setq ent (ssname entityset i)
			i (1+ i)
		)
		(if (setq ne (entnext ent))
			(while (= "VERTEX" (db_DXF 0 ne))
				(progn
					(setq elist (entget ne) 
						p10 (cdr (assoc 10 elist))
						p20 (heightfit_3D p10 A0 A1 A2 A3 A4 A5 X0 Y0)
						ne (entnext ne)
					)
					(setq elist (subst (cons 10 p20) (cons 10 p10) elist))
					(entmod elist)
				)
			)
		)	
	)
)	

(defun HeightFitting_SPLINE (entityset A0 A1 A2 A3 A4 A5 X0 Y0 / p0 p1 ent elist i remain)
	(setq i 0)
	(repeat (sslength entityset)
		(setq ent (ssname entityset i)
			i (1+ i)
			elist (entget ent)
			p0 (cdr (assoc 10 elist))
			remain elist
		)
		(while p0
			(setq p1 (heightfit_3D p0 A0 A1 A2 A3 A4 A5 X0 Y0)
				elist (subst (cons 10 p1) (cons 10 p0) elist)
			)
			(setq remain (cdr(member p0 remain))
				p0 (assoc 10 remain)
			)
		)
		(entmod elist)
	)
)
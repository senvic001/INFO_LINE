;;����ת��
;;�Ĳ���ת��
 ;*********************************************************************************************
 ;��������:FourParaTrans()
 ;���ܣ��Ĳ���ת��
 ;������pts:���x1 y1 z1 x2 y2 z2...) Dim ά��Dim=2 or 3	,DX DY K ,A��λΪ���
 ;���أ�У����ĵ��
 ;����ʱ�䣺2016/03/28   12:16
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
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
	(setq id (load_dialog path)) ;װ��Ի����ļ�
    (if (< id 0)
        (exit)
    ) ;_ End_if
	
	(if (not (new_dialog "dlg_fourpara" id)) (*error* (strcat "�򿪶Ի���" path)))
	(setdata DX DY K A)
	(action_tile "accept" "(getdata)(done_dialog 1)")
	(action_tile "cancel" "(done_dialog 0)")
	
	(setq std (start_dialog))
	(unload_dialog id)
	
	(if (= std 1)
		(progn
			(prompt "\n ��ʼ�����Ĳ���ת����")
			;;�����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "POINT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��POINTʵ�塣"))
				)
			)
			;;Բ����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "CIRCLE"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��CIRCLEʵ�塣"))
				)
			)	
			;;Բ������
			(if (setq AllPointSet (ssget "X" (list (cons 0 "ARC"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��ARCʵ�塣"))
				)
			)	
			;;��Բ����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "ELLIPSE"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��ELLIPSEʵ�塣"))
				)
			)	
			;;INSERT����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "INSERT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��INSERTʵ�塣"))
				)
			)	
			;;TEXT����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "TEXT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��TEXTʵ�塣"))
				))
				
			;;MTEXT����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "MTEXT"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��MTEXTʵ�塣"))
				)
			)	
			;;RAY����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "RAY"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��RAYʵ�塣"))
				)
			)
			;;XLINE����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "XLINE"))))
				(progn
					(FourParaTrans_Point AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��XLINEʵ�塣"))
				)
			)
			;;LINE����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "LINE"))))
				(progn
					(FourParaTrans_Line AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��LINEʵ�塣"))
				)
			)		
			;;SPLINE����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "SPLINE"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��SPLINEʵ�塣"))
				)
			)		
			;;LWPOLYLINE����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "LWPOLYLINE"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��LWPOLYLINEʵ�塣"))
				))	
			;;POLYLINE����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "POLYLINE"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��POLYLINEʵ�塣"))
				))	
			;;HATCH����
			(if (setq AllPointSet (ssget "X" (list (cons 0 "HATCH"))))
				(progn
					(FourParaTrans_LWPOLYLINE AllPointSet DX DY K A)
					(prompt (strcat "\n ת����" (rtos (sslength AllPointSet) 2 0) "��HATCHʵ�塣"))
				))	
		(C:ResetSpaceIndex)
		)
	)
	(prin1)
)


;;������POINT TEXT MTEXT CIRCLE ARC ELLIPSE RAY INSERT
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
			Y (car p0)	;;��ͶӰ����
			X (cadr p0)	;;��ͶӰ����
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
			Y (car p0)	;;��ͶӰ����
			X (cadr p0)	;;��ͶӰ����
			X1 (- (+ DX (* m1 X)) (* m2 Y))
			Y1 (+ DY (* m1 Y) (* m2 X))
			elist (subst (cons 10 (list Y1 X1 (caddr p0))) (cons 10 p0) elist)
		)
		(setq  p0 (cdr (assoc 11 elist))
			Y (car p0)	;;��ͶӰ����
			X (cadr p0)	;;��ͶӰ����
			X1 (- (+ DX (* m1 X)) (* m2 Y))
			Y1 (+ DY (* m1 Y) (* m2 X))
			elist (subst (cons 11 (list Y1 X1 (caddr p0))) (cons 11 p0) elist)
		)
		(entmod elist)
	)
)


;;������LWPOLYLINE  POLYLINE HATCH
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
				Y (car p10)	;;��ͶӰ����
				X (cadr p10)	;;��ͶӰ����
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
									Y (car p10)	;;��ͶӰ����
									X (cadr p10)	;;��ͶӰ����
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


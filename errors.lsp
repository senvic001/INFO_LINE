
;*********************************************************************************************
;��������:C:CheckErrors()
;���ܣ��������е�����
;������
;���أ�
;����ʱ�䣺2014/12/18   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:CheckErrors( /  allpointset bm  bfix edges entp errorstr fstr hd i mapstr ne nerror 
						namelist e1 e2 handle1 handle2 mapstr1 mapstr2 pos1 pos2 tname)
	;;����Ƿ����δ����˵���߶�,�˵��ظ����߶�,���������˹�ϵ,ͳ���߶�����
	(princ "\n��ʼ���ҹ��ߴ���,������Ҫ�ϳ�ʱ��,�����ĵȴ�....")
	(if (setq bfix (getstring "\n�Ƿ��Զ��޸����ӹ��ߴ���(Y/N)"))
	    (setq bfix (strcase bfix))
	)
	(setq gl_Time (getvar "TDUSRTIMER"))
	(UpdateTopo)
	;;�������feature����ͨ��,�Ƿ�������������;�Ƿ��ǹ����ĵ�;�������Ƿ�����һ����
	(setq AllPointSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
		i 0
		gl_ErrorList nil
		errorstr nil
	)
	(if AllPointSet
        (repeat (sslength AllPointSet)
			(setq entp (ssname AllPointSet i)
				hd (cdr (assoc 5 (entget entp)))
			)
			(if (vlax-ldata-get entp gl_AppName)
				(progn
					(setq mapstr (ldata-get entp "Map_No")
						fstr (ldata-get entp "Feature")
						tname (ldata-get entp "Main_Type"))
					(if (setq edges (ldata-get entp "Edge"))
						(progn
							(setq ne (length edges)
								errorstr nil
								bfixed nil)	;�Ƿ��Ѿ��޸�
							(cond
								((= fstr "������") 
									(if (/= ne 1)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)
								((= fstr "����") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "���") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "�ϸ�") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "�¸�") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "��ͷ") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "��ͷ") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "��ˮ��") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "��ˮ��") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "Ԥ����") 
									(if (/= ne 1) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "ת�۵�") 
									(if (/= ne 2)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "ֱ�ߵ�") 
									(if (/= ne 2)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "����") 
									(if (/= ne 2) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "�侶") 
									(if (/= ne 2) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "���") 
									(if (/= ne 2) (setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0)"������,�������\(" fstr "\)��һ��."))))
								((= fstr "��ͨ") 
									(if (/= ne 3)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "��ͨ") 
									(if (/= ne 4)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature"))
													bfixed T)
											)
										)
									)
								)	
								((= fstr "��ͨ") 
									(if (/= ne 5)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "��ͨ") 
									(if (/= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "��ͨ") 
									(if (<= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "����֧") 
									(if (/= ne 3)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "�ķ�֧") 
									(if (/= ne 4)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "���֧") 
									(if (/= ne 5)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "����֧") 
									(if (/= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								((= fstr "���֧") 
									(if (<= ne 6)	
										(progn
											(setq errorstr (strcat "PointError:" mapstr "����" (rtos ne 2 0) "������,�������\(" fstr "\)��һ��."))
											(if (setfeature ne entp tname)
												(setq	errorstr (strcat errorstr "���޸�Ϊ��" (ldata-get entp "Feature")) bfixed T)
											)
										)
									)
								)	
								(T (setq errorstr nil))
							)
							(if (and (/= nil errorstr)(= 0 ne )) (setq errorstr (strcat errorstr "�õ�δ������������.")))
							(if (and (not bfixed) errorstr )
								(progn
									(prompt (strcat "\n" errorstr))
									(setq gl_ErrorList (cons (cons hd errorstr) gl_ErrorList))
								)
							)
						)
						;;else
						(progn
							(setq errorstr (strcat "PointError:" mapstr "������,��δ���������.")
								gl_ErrorList (cons (cons hd errorstr) gl_ErrorList)
							)
							(prompt (strcat "\n" errorstr))
						)
					)	
				)
			)
			(setq i (1+ i))
		)
	)
	
	(princ "\n�����鹲��ʱ")
	(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
	(princ "��.")
	
	(setq nerror (length gl_ErrorList))
	(if (= nerror 0)
		(prompt "\n δ���ֹ��ߵ�͹��߶εĴ���.")
		(progn
			(prompt (strcat "\n���ҵ�" (rtos nerror 2 0) "�����ߵ����߶����Ӵ���.	"))
			(if (setq bm (Fun_InPutString "Y" "USERS2" "\n���ھ������޸���?(Y/N)"))
				(if (= "Y" (setq bm (strcase bm)))
					(C:EditErrorList)
				)
			)
		)
	)
	
	(princ)
)

(defun setfeature(iedge ent typename / bfix)
	(setq bfix nil)
	(if (or (= typename "Y")(= typename "W")(= typename "P")(= typename "J")(= typename "Y"))
		(cond
			((= iedge 0) (progn (ldata-put ent "Feature" "������") (setq bfix T)))
			((= iedge 1) (progn (ldata-put ent "Feature" "������") (setq bfix T)))
			((= iedge 2) (progn (ldata-put ent "Feature" "ֱ�ߵ�") (setq bfix T)))
			((= iedge 3) (progn (ldata-put ent "Feature" "��ͨ") (setq bfix T)))
			((= iedge 4) (progn (ldata-put ent "Feature" "��ͨ") (setq bfix T)))
			((= iedge 5) (progn (ldata-put ent "Feature" "��ͨ") (setq bfix T)))
			((= iedge 6) (progn (ldata-put ent "Feature" "��ͨ") (setq bfix T)))
			((> iedge 6) (progn (ldata-put ent "Feature" "��ͨ") (setq bfix T)))
		)
		(cond
			((= iedge 0) (progn (ldata-put ent "Feature" "������") (setq bfix T)))
			((= iedge 1) (progn (ldata-put ent "Feature" "������") (setq bfix T)))
			((= iedge 2) (progn (ldata-put ent "Feature" "ֱ�ߵ�") (setq bfix T)))
			((= iedge 3) (progn (ldata-put ent "Feature" "����֧") (setq bfix T)))
			((= iedge 4) (progn (ldata-put ent "Feature" "�ķ�֧") (setq bfix T)))
			((= iedge 5) (progn (ldata-put ent "Feature" "���֧") (setq bfix T)))
			((= iedge 6) (progn (ldata-put ent "Feature" "����֧") (setq bfix T)))
			((> iedge 6) (progn (ldata-put ent "Feature" "���֧") (setq bfix T)))
		)
	)
	bfix
)
;*********************************************************************************************
;��������:C:RepairErrorSameName()
;���ܣ��޸���������
;������
;���أ�
;����ʱ�䣺2015/3/25   1��00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:RepairErrorSameName( / allpointset e1 e2 entp errorstr  i mapstr1 mapstr2 namelist newname nfix
	ent allent pos pos10)
	(princ "\n�Զ��޸������ظ�����" )
	(if (setq AllPointSet (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		;;�������������
		(progn
			(setq namelist nil
				nfix 0
				i 0)
			(repeat (sslength AllPointSet)
				(setq entp (ssname AllPointSet i))
				(if (vlax-ldata-get entp gl_AppName)
					(setq namelist (cons (list entp (ldata-get entp "Map_No")) namelist))
				)
				(setq i (1+ i))
			)
			(setq gl_MapNameList (GetMapNamesList)
				gl_ExpNameList (GetExpNamesList))
			
			(while (< 1 (length namelist))
				(setq e1 (car namelist)
					namelist (cdr namelist)
					mapstr1 (cadr e1))
				(foreach e2 namelist
					(setq mapstr2 (cadr e2)
						e2 (car e2))
					(if (= mapstr1 mapstr2)
						(progn
							(setq newname (CreateNewPointName mapstr2 (ldata-get e2 "Main_Type") "MAP"))
							(if (SetAttrib-PointName e2 "���" newname)
								(progn
									(ldata-put e2 "Map_No" newname)
									(ldata-put e2 "Exp_No" newname)
									(setq errorstr (strcat "\nPointError:" mapstr1 "ͼ�ϵ���ظ�.ͼ�ź���̽��Ŷ��Զ��޸�Ϊ" newname ".")
										namelist (vl-remove e2 namelist)
										gl_MapNameList (cons newname gl_MapNameList)
										gl_ExpNameList (cons newname gl_ExpNameList)
										nfix (1+ nfix)
									)
									(princ errorstr)
									
									;;������������߶Σ�startpoint endpoint
									(setq pos (cdr (assoc 10 (entget e2)))
										allent (GetConnectEntity (car pos) (cadr pos) "LINE")
										i 0)
									(repeat (length allent)
										(setq   ent (nth i allent)
											pos10 (cdr (assoc 10 (entget ent)))
										)
										(if (IsSameXY pos pos10);;pos10 is the same point
											(ldata-put ent "Start_Point" newname)
											;;else
											(ldata-put ent "End_Point" newname)
										)
										(setq i (1+ i))
									)	
								)
							)
						)
					)
				)
			)
		) 
	)
	(princ (strcat "\n�޸���" (rtos nfix 2 0) "�����������"))
	(princ)
)
 ;;ɾ��ʵ��,���½�ʵ��ʱ����Ҫ����StackList
    ;;(oldent ɾ����ʵ��  newent �½���ʵ�� StackList)
(defun RebulidErrorList (oldent newent errorlist / e p1 hnew hold)
	;;(setq errorlist gl_ErrorList)
	(if (and (/= nil newent) (/= nil oldent) (/= nil errorlist))
		(foreach e errorlist
			(setq p1 (car e)
				hnew (cdr (assoc 5 (entget newent)))
				hold (cdr (assoc 5 (entget oldent))))
			(if (= p1 hold)
				(setq errorlist (subst (cons hnew (cdr e)) e errorlist)))
			);foreach
		 );if
	errorlist
)

;*********************************************************************************************
;��������:C:FlowError()
;���ܣ�����������.(1)ĳ��ֻ������û������,(2) �γ�ѭ������ (3) ������̲߳��Ǻ� (4) ȱ������
;ǰ������:��������֮��Ĺ�ϵEdge
;������
;���أ�׷�ӵ������б�gl_ErrorList
;����ʱ�䣺2015/1/7   10:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************		
(defun C:FlowError(/ alllinelist bdifferflow e edge edge1 edge2 elist endname entl entls errorstr 
			flowdirect flowerrorlist hentl hentp i linelist linkedges linkp Map_Nos 
			nextline nline p10 p11 pent1 pent2 preflow singlelist stackin stackout startedges startname
			startp tns typename x)
	(prin1 "\n�����ˮ����ˮ����ˮ�������\n�������ͣ�(1)���ߵ�ֻ������������ (2)�����γɱպϻ� (3)������߳��෴ (4)δ��������")
	(prin1)
	; (setq tns (Fun_InPutString "P,Y,W" "USERS1" "\n�������Y-��ˮ��W-��ˮ��P-��ˮ��
							; \n��������Ҫ�������Ĺ��������룬��������ö���(,)�ָ���"))
	; (if tns 
		; (setq tns (strcat "(" (strcase tns) ")"))                                            
	; )
	; (if (not (listp (setq tns (read tns))))
		; (progn
			; (prompt "\n������������ʽ����ȷ.")
			; (exit)
		; )
	; )
	; (setq tns (mapcar '(lambda (x) (vl-symbol-name x)) tns))
	(setq tns (list "Y" "W" "P"))
	(setq AllLineList (GetTypedLineList)
		flowerrorlist nil)	;;����ʵ���б�
	(foreach e AllLineList                                   
		(setq linelist (cadr e)
			typename (car e)
			nline (length linelist)
			singlelist nil ;the first edges
			stackin nil
			stackout nil
			i 0)
		(if (and (> nline 0) (vl-position typename tns))
			(progn
				;;find all end edges
				(while (< i nline)
					(setq entl (nth i linelist))
					(if (setq edge (ldata-get entl "Edge"))
						(progn
							(setq pent1 (handent(car edge))
								pent2 (handent(cadr edge))
								edge1 (ldata-get pent1 "Edge")
								edge2 (ldata-get pent2 "Edge")
							)
							(if (or (= (length edge1 ) 1) (= (length edge2) 1));the point only has one edge.
								(setq singlelist (cons entl singlelist))
							)
						)
					)
					(setq i (1+ i))
				)
				;;
				(while (> (length singlelist) 0)
					;;initialize startp and linkp and stackin
					(setq entl (car singlelist)
						edge (ldata-get entl "Edge")
						singlelist (cdr singlelist)
						pent1 (handent(car edge))
						pent2 (handent(cadr edge))
						edge1 (ldata-get pent1 "Edge")
						stackout nil
					)
					(if (= 1 (length edge1));;start point and link point
						(setq startp pent1
							linkp pent2) 
						(setq startp pent2 
							linkp pent1)
					)
					(setq stackin (cons (list startp linkp entl) stackin))
					;;������� 
					(while (> (length stackin) 0)
						(setq entl (caddr (car stackin))
							  startp (car (car stackin))
							  linkp (cadr (car stackin))
							  stackin (cdr stackin)
						)
						
						;;beign check flowdirect errors
						(setq elist (entget entl)
							startname (ldata-get entl "Start_Point")
							endname (ldata-get entl "End_Point")
							p10 (cdr (assoc 10 elist))
							p11 (cdr (assoc 11 elist))
							flowdirect (ldata-get entl "Flowdirect")
							hentl (cdr (assoc 5 elist))
							hdist (distance (list (car p10) (cadr p10)) (list (car p11) (cadr p11)))
							vdist (abs (- (last p10) (last p11)))
						)
						(if (> hdist gl_MIN)
							(setq bpl (/ vdist hdist))
							(setq bpl nil)
						)
						;;Error:no definition of flowdirection
						(if (= 0 (ldata-get entl "Flowdirect")) 
							(progn
								(setq errorstr (strcat "FlowError:���߶�" startname "-" endname "δ��������")
									gl_ErrorList (cons (cons hentl errorstr) gl_ErrorList)
									flowerrorlist (cons entl flowerrorlist)
								)
								(print errorstr)
							)
						)
						;;Error:check elevation
						(if (= 1 flowdirect);p10->p11
							(if (< (last p10) (last p11))
								(progn
									(setq errorstr  (strcat "FlowError:���߶�" startname "-" endname "������ܵ׸̲߳�һ�¡�" 
													 "���룺" (rtos hdist 2 3) ",�߲" (rtos vdist 2 3) ",���ʣ�" (rtos bpl 2 5))
										gl_ErrorList (cons (cons hentl errorstr) gl_ErrorList)
										flowerrorlist (cons entl flowerrorlist)
									)
									(print errorstr)
								)
							)
						)
						(if (= 2 flowdirect);p10<-p11
							(if (< (last p11) (last p10))
								(progn
									(setq errorstr  (strcat "FlowError:���߶�" startname "-" endname "������ܵ׸̲߳�һ�¡�" 
													 "���룺" (rtos hdist 2 3) ",�߲" (rtos vdist 2 3) ",���ʣ�" (rtos bpl 2 5))
										gl_ErrorList (cons (cons hentl errorstr) gl_ErrorList)
										flowerrorlist (cons entl flowerrorlist)
									)
									(print errorstr)
								)
							)
						)
						
						;;Error:all in or all out
						(setq linkedges (ldata-get linkp "Edge")
							startedges (ldata-get startp "Edge")
							Map_Nos (ldata-get startp "Map_No")
							bdifferflow nil
							preflow -1	;ǰһ���ߵ��������ڱȽ�����ı��
						)
						(if (> (length startedges) 1)
							(progn
								(foreach e startedges
									(setq entls (handent (last e))
										  pent1 (handent (car e))
										  pent2 (handent (cadr e))
										  startname (ldata-get entls "Start_Point")
										  endname (ldata-get entls "End_Point")
										  flowdirect (ldata-get entls "Flowdirect")
									)
									
									(if (> flowdirect 0)	;;���뱾��ķ���Ϊ1����������ķ���Ϊ2
										(progn
											(if (= Map_Nos startname) 
												(if (= 1 flowdirect)
													(setq flowdirect 2);flow out
													(setq flowdirect 1));flow in
											)
											(if (/= -1 preflow) 
												(if (/= preflow flowdirect) (setq bdifferflow T))
												(setq preflow flowdirect);��ֵ
											)
										)
										(setq bdifferflow T)	;����δ��������ı�,�򲻿�������һ������.
									)
									
								)
								(if (not bdifferflow) ;������ͬ
									(progn
										(setq  hentp (cdr (assoc 5 (entget startp)))
											errorstr (strcat  "FlowError:���ߵ�" Map_Nos "������ͬ��")
											gl_ErrorList (cons (cons hentp  errorstr) gl_ErrorList)
											flowerrorlist (cons startp flowerrorlist)
										)
										(print errorstr)
									)
								)
							)
						)
						
						;;put line to stackout
						(setq stackout (cons entl stackout)
							singlelist (vl-remove entl singlelist) 
						)
						
						(foreach e linkedges
							(setq nextline (handent (last e))
								  pent1 (handent (car e))
								  pent2 (handent (cadr e))
								  )
							(if (not (equal nextline entl))
								(if  (vl-position nextline stackout);�Ƿ��պϹ���
									(progn
										(setq errorstr (strcat "FlowError:���߶�" (ldata-get nextline "Start_Point") "-" (ldata-get nextline "End_Point") "���ڱպϹ��߶Ρ�"))
										(setq gl_ErrorList (cons (cons (cdr (assoc 5 (entget nextline))) errorstr) gl_ErrorList))
										(print errorstr )
									)
									;;else
									(if (equal pent1 linkp )
										(setq stackin(cons (list linkp pent2 nextline) stackin))
										(setq stackin(cons (list linkp pent1 nextline) stackin))
									)
								)
							)
						)
					)
				)
			)
		)
	)
	(setq nerror (length flowerrorlist))
	(if (= nerror 0)
		(prompt "\n δ�����������.")
		(progn
			(prompt (strcat "\n���ҵ�" (rtos nerror 2 0) "���������.\nδ����Ĵ�����" (rtos (length gl_ErrorList) 2 0) "��."))
			(if (setq bm (Fun_InPutString "Y" "USERS2" "\n���ھ������޸���?(Y/N)"))
				(if (= "Y" (setq bm (strcase bm)))
					(C:EditErrorList)
				)
			)
		)
	)
	(princ)
)
;;��������б��ļ�
;;��������б��ļ�
(defun C:OutPutErrorList ( / pointPath filep e)
	;;�򿪵��ļ�
	(setq pointPath "C:\\Line_Info_Errors.txt")
     (vl-file-delete pointPath)
     (if (not (setq filep (open pointPath "w")))
         (progn
             (prompt (strcat "\n���ļ�" pointPath "ʧ�ܡ��˳���"))
             (exit)
         ) ;_ End_progn
     ) ;if
    
	(foreach e gl_ErrorList
		(write-line (strcat (car e) "," (cdr e)) filep)
	)
	(close filep)
	(prompt (strcat "\n���" (rtos (length gl_ErrorList) 2 0) "��������Ϣ���ļ�" pointPath "."))
    (if (> (length gl_ErrorList) 0) (startapp "EXPLORER.EXE" pointPath))
    (princ)
)

;;;����δ�������ԵĹ��߶�
(defun C:FindLineWithnothing (/ lineset findline i ent ecolor lobj len num )
    (setq lineset  (ssget "X" (list (cons 0 "LINE")))
          i        0
	  len (sslength lineset)
	  ent nil
          num 0
          ;findline T
    ) ;_ end_setq

    (repeat  len
        (setq ent (ssname lineset i))
        (if (not (vlax-ldata-get ent gl_AppName))
			(progn
				(setq 
					lobj (vlax-ename->vla-object (ssname lineset i))
					ecolor (vla-get-truecolor lobj)
					num (1+ num)
				) ;_ end_setq
				(vla-put-colorindex ecolor acCyan)
				(vla-put-truecolor lobj ecolor)
			)
        )
        (setq i  (1+ i));
    ) ;_ end_repeat
    (if (> num 0)
		(prompt (strcat "\n����:�� " (rtos num 2 0) " ���߶�δ��������.�߶ε���ɫ����Ϊ��ɫ."))
		(prompt "\n����ֱ�߶ζ�����Ϊ���߶Ρ�")
	)
	(princ)
)


;;�����������ֶ�����Xdata����֪Ե��
(defun C:SetAllXdata( / i j allset)
	(setq allset (ssget "X")
		i 0
		j 0)
	(repeat (sslength allset)
		(setq ent (ssname allset i)
			i (1+ i)
			)
		(if (vlax-ldata-get ent gl_AppName)
			(if (not (getxdata ent gl_AppName))
				(progn
					(setxdata ent gl_AppName (list (cons 1000 gl_Version)))
					(setq j (1+ j))
				)
			)	
		)
	)
	(prompt (strcat "\n����Xdata��" (rtos j 2 0)))
	(princ)

)

;*********************************************************************************************
;��������:C:ReSetMapNames()
;���ܣ��������ù��ߵ��ͼ�ϵ����������޸���̽���
;������
;���أ�
;����ʱ�䣺2015/4/1   24:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:ReSetMapNames( / allp clines el ent i j mapname mtype newname pos pos10 ptlist pts)
	(princ "\n��������ͼ�ϵ�š�")
	(setq allp (GetTypedPointList))
	(if (listp allp)
		(foreach pts allp
			(setq mtype (car pts)	;;maintype
				ptlist (cadr pts)	;;insert entity list
				i 1)				;;name index
			(foreach ent ptlist
				(setq mapname (ldata-get ent "Map_No")
					pos (cdr (assoc 10 (entget ent)))
					clines (GetConnectEntity (car pos) (cadr pos) "LINE")
					newname (strcat mtype (rtos i 2 0))
					j 0
					i (1+ i)
				)
				;;�����µĵ���
				(ldata-put ent "Map_No" newname)
				(SetAttrib-PointName ent "���" newname)
				;;�����߶�startpoint endpoint
				(repeat (length clines)
					(setq   el (nth j clines)
						pos10 (cdr (assoc 10 (entget el)))
					)
					(if (IsSameXY pos pos10);;pos10 is the same point
						(ldata-put el "Start_Point" newname)
						;;else
						(ldata-put el "End_Point" newname)
					)
					(setq j (1+ j))
				)	
			)	
		)
	)
	;;�������õ����б�
	(setq gl_ExpNameList (GetExpNamesList)
		gl_MapNameList (GetMapNamesList))
	(princ "\n�������ͼ�ϵ�š�")
    (princ)
)


;;ɾ��ͬ��λ�õ��߶Σ��͵㡣
(defun C:DelSameEntitys  (/ t1 delList e time2)
	; (if gl_DEBUG (setq gl_Time (getvar "TDUSRTIMER")))
    ; (setq AllBlockSet (ssget "X" (list (cons 0 "INSERT")))
          ; AllLineSet  (ssget "X" (list (cons 0 "LINE")))
          ; AllTextSet  (ssget "X" (list (cons 0 "TEXT"))))
    ; (setq dim  (Fun_InPutValue 0.0001 "USERR5" "\nɾ����ͬ�����֡�ͼ�顢ֱ��ͼԪ����ͼ�ζ���϶࣬���ܷ�ʱ�ϳ���\n������λ�þ��ȣ�" 4))
    ; (DelSameEntity AllLineSet "LINE" dim)
    ; (DelSameEntity AllBlockSet "INSERT" dim)
    ; (DelSameEntity AllTextSet "TEXT" dim)
	; (if gl_DEBUG
		; (progn
		; (princ "\nɾ����ͬͼԪ����ʱ")
		; (princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
		; (princ "��.")
		; )
	; )	
	(setq t1 (StartWatch))
	(setq delList (Index-GetSamePositionEntity (GetAllPointRoot T))
		delList (ListRemoveSameElement delList)
	)
	(foreach e delList
		 (DelEntity e)
	)
	(princ (strcat "\nɾ��" (rtos (length delList) 2 0) "���ظ����ߵ����,"))
	
	
	(setq delList (Index-GetSamePositionEntity (GetLineRoot T))
		delList (ListRemoveSameElement delList)
	)
	(foreach e delList
		 (DelEntity e)
	)
	(princ (strcat "\nɾ��" (rtos (length delList) 2 0) "���ظ����߶ζ���,"))
	
	(setq time2 (TimeCosted "ɾ���ظ����ߵ�" t1))
	
	(princ)
)

(defun DelSameEntity  (sset typestr dim /  delent dellist ent ent2 entdata entdata2 isame leftlist newlist p10 p11 p20 p21 s1 s2 startlist t1 t2 type1 type2)
    (setq newlist   nil
          dellist   nil
          startlist sset
          )
    (if sset
        (progn
            (repeat (- (sslength sset) 1)
                (setq isame    0 ;��ͬ��־
                      p11      nil
                      p21      nil
                      t1       nil
                      t2       nil
                      s1       nil
                      s2       nil
                      ent      (ssname startlist 0)
                      leftlist (ssdel ent startlist)
                      entdata  (entget ent)
                      type1    (cdr (assoc 0 entdata))
                      p10      (cdr (assoc 10 entdata))
                      i 0
                      )
                (repeat   (sslength leftlist)
                    (setq ent2 (ssname leftlist i)
                        entdata2 (entget ent2)
                          type2    (cdr (assoc 0 entdata2))
                          p20      (cdr (assoc 10 entdata2))
                          )
                    (if (= type1 "LINE")
                        (setq p11 (cdr (assoc 11 entdata))))
                    (if (= type2 "LINE")
                        (setq p21 (cdr (assoc 11 entdata2))))
                    (if (= type1 "TEXT")
                        (setq t1 (cdr (assoc 1 entdata))))
                    (if (= type2 "TEXT")
                        (setq t2 (cdr (assoc 1 entdata2))))
                    (if (= type1 "INSERT")
                        (setq s1 (cdr (assoc 2 entdata))))
                    (if (= type2 "INSERT")
                        (setq s2 (cdr (assoc 2 entdata2))))

                    (if (and (= type1 type2) (/= nil p11)) ;line
                        (if (or (and (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                     (and (< (abs (- (car p11) (car p21))) dim) (< (abs (- (cadr p11) (cadr p21))) dim)));(and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                (and (and (< (abs (- (car p10) (car p21))) dim) (< (abs (- (cadr p10) (cadr p21))) dim))
                                     (and (< (abs (- (car p11) (car p20))) dim) (< (abs (- (cadr p11) (cadr p20))) dim))))
                            (setq isame (1+ isame))
                            ) ;if
                        ) ;if

                    (if (and (= type1 type2) (/= nil t1)) ;text
                        (if (and (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                 (= t1 t2))
                            (setq isame (1+ isame))
                            ) ;if
                        ) ;if

                    (if (and (= type1 type2) (/= nil s1)) ;insert
                        (if (and
                                (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
                                (= s1 s2))
                            (setq isame (1+ isame))
                            ) ;if
                        ) ;if
                    (setq i (1+ i))
                    ) ;repeat
                (if (= 0 isame) ;only one
                    (setq newlist (append newlist (list ent)))
                    )
                (if (>= isame 1) ;only one
                    (setq dellist (append dellist (list ent)))
                    )
                (setq startlist leftlist)
                ) ;foreach
            (setq newlist (append newlist (list (ssname startlist 0))))
            ) ;progn
        ) ;if
    (if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
                );foreach
            (prompt (strcat "\nɾ����"
                            (rtos (length dellist) 2 0)
                            "���ظ���"
                            typestr
                            "ͼԪ��"))
            ) ;progn
        (prompt (strcat "\nɾ����0���ظ���" typestr "ͼԪ��"))
        );if
		(princ)
    ) 
;;;	ʹ���µ��㷨	
;;;ѡ��һ������Ȼ��ɾ�������ڵ��ظ��㣬�������顣
(defun DelSameEntity2 (dim / allblockset alllineset alltextset app delent dellist doc e1 ent
		entlist entlist2 i idel linelist mspace p10 p11 p20 p21 
			slen ss typestr)
	(if gl_DEBUG (setq gl_Time (getvar "TDUSRTIMER")))
    (setq AllBlockSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
          AllLineSet  (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName))))
          AllTextSet  (ssget "X" (list (cons 0 "TEXT"))))
	(setq app	 (vlax-get-acad-object)
		  Doc	 (vla-get-Activedocument app)
		  Mspace (vla-get-modelspace Doc)
	) ;_ End_setq
	
	(vla-ZoomAll app)
	
	(setq dellist nil);;��Ҫɾ����ͼԪ
	;;del point
	(setq i 0)
	(if AllBlockSet
		(repeat (sslength AllBlockSet)
			(setq ent (ssname AllBlockSet i)
				entlist (entget ent)
				p10 (cdr (assoc 10 entlist))
                              	P10 (list (car p10) (cadr p10))
				ss (ssget p10 (list (cons 0 "INSERT")))
				slen (sslength ss)
				idel 0
				i (1+ i)
			)
			(if (> slen 1)
				(repeat slen
					(setq e1 (ssname ss idel))
					(if (/= e1 ent)
						(if (not (vl-position e1 dellist))
							(setq dellist (cons e1 dellist))
						)
					)
					(setq idel (1+ idel))
				)
			)
		)
	)
	(setq typestr "INSERT")
	(if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
            );foreach
            (prompt (strcat "\nɾ����"
                            (rtos (length dellist) 2 0)
                            "���ظ���"
                            typestr
                            "ͼԪ��"))
        ) ;progn
        (prompt (strcat "\nɾ����0���ظ���" typestr "ͼԪ��"))
    );if
	(setq i 0
		dellist nil)
	(if AllTextSet
		(repeat (sslength AllTextSet)
			(setq ent (ssname AllTextSet i)
				entlist (entget ent)
				p10 (cdr (assoc 10 entlist))
                              P10 (list (car p10) (cadr p10))
				ss (ssget p10 (list (cons 0 "TEXT")))
				slen (sslength ss)
				idel 0
				i (1+ i)
			)
			(if (> slen 1)
				(repeat slen
					(setq e1 (ssname ss idel))
					(if (/= e1 ent)
						(if (not (vl-position e1 dellist))
							(setq dellist (cons e1 dellist))
						)
					)
					(setq idel (1+ idel))
				)
			)
		)
	)
	(setq typestr "TEXT")
	(if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
            );foreach
            (prompt (strcat "\nɾ����"
                            (rtos (length dellist) 2 0)
                            "���ظ���"
                            typestr
                            "ͼԪ��"))
        ) ;progn
        (prompt (strcat "\nɾ����0���ظ���" typestr "ͼԪ��"))
    );if
	(setq i 0
		linelist nil)
	(if AllLineSet
		(repeat (sslength AllLineSet)
			(setq ent (ssname AllLineSet i)
				entlist (entget ent)
				p10 (cdr (assoc 10 entlist))
                              P10 (list (car p10) (cadr p10))
				p11 (cdr (assoc 10 entlist))
                              P11 (list (car p11) (cadr p11))
				ss (ssget p10 (list (cons 0 "LINE")(list -3 (list gl_AppName))))
				slen (sslength ss)
				idel 0
				i (1+ i)
			)
			(if (> slen 1)
				(repeat slen
					(setq e1 (ssname ss idel))
					(if (/= e1 ent)
						(if (not (vl-position e1 dellist))
							(progn
								(setq entlist2 (entget e1)
									p20 (cdr (assoc 10 entlist2))
									p21 (cdr (assoc 11 entlist2))
								)
								(if (or (and (and (< (abs (- (car p10) (car p20))) dim) (< (abs (- (cadr p10) (cadr p20))) dim))
											 (and (< (abs (- (car p11) (car p21))) dim) (< (abs (- (cadr p11) (cadr p21))) dim)))
										(and (and (< (abs (- (car p10) (car p21))) dim) (< (abs (- (cadr p10) (cadr p21))) dim))
											 (and (< (abs (- (car p11) (car p20))) dim) (< (abs (- (cadr p11) (cadr p20))) dim))))
									(setq dellist (cons e1 dellist))
								) 
							)
						)
					)
					(setq idel (1+ idel))
				)
			)
		)
	)
	(setq typestr "LINE")
	(if dellist
        (progn
            (foreach delent  dellist
                (vla-delete (vlax-ename->vla-object delent))
            );foreach
            (prompt (strcat "\nɾ����"
                            (rtos (length dellist) 2 0)
                            "���ظ���"
                            typestr
                            "ͼԪ��"))
        ) ;progn
        (prompt (strcat "\nɾ����0���ظ���" typestr "ͼԪ��"))
    );if
	(if gl_DEBUG
		(progn
		(princ "\nɾ����ͬͼԪ����ʱ")
		(princ (* (- (getvar "TDUSRTIMER") gl_Time) 86400))
		(princ "��.")
		)
	)	
	(princ)
)	
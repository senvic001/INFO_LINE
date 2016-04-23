;;;��ע�������ֺ�ͼ��
;;;���ݽṹ����ע�����ԭ����֮�䱣��˴˵ľ��

(setq gl_Type_Label_List nil) ;���� ����ֶ���1���ֶ���2...
 ;*********************************************************************************************
 ;��������:ReadTypeLabelList()
 ;���ܣ���ȡ���������ļ��е�[Type_Label]����;����Ϊ���ŷָ�;
 ;������gl_Type_Label_List
 ;���أ�((J 1 D_S Material Flowdirect))
 ;����ʱ�䣺2014/08/28   13:00
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun ReadTypeLabelList (path / typetablelist str file tmplist pos)
    (if (= nil path)
        (setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ End_if
    (setq typetablelist nil)
    (if (setq file (open path "r"))
        (progn
            (while (setq str (read-line file))
                (if (= str "[Type_Label]")
                    (while (/= "[/Type_Label]" (setq str (read-line file)))
                        (setq tmplist nil
                              str     (vl-string-left-trim " ," str)
                              str     (vl-string-right-trim " ," str)
                        ) ;_ End_setq
                        (while (setq pos (vl-string-search "," str))
                            (setq tmplist (append tmplist (list (substr str 1 pos)))
                                  str     (substr str (+ 2 pos))
                            ) ;_ End_setq
                        ) ;_ End_while
                        (setq tmplist (append tmplist (list str)))
                        (if (listp tmplist)
                            (setq typetablelist (append typetablelist (list tmplist)))
                        ) ;_ End_if
                    ) ;while
                ) ;if
            ) ;while
            (close file)
        ) ;progn
        ;;else
        (prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if
    typetablelist
) ;_ End_defun
 ;*********************************************************************************************
 ;��������:WriteTypeLabelList()
 ;���ܣ��ѱ�ע��Ϣд�������ļ�
 ;������labellist ��ע��Ϣ��  path �ļ�·��
 ;���أ�
 ;����ʱ�䣺2015/01/17 14:00
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun WriteTypeLabelList(labellist path / tmpfile tmpfilestr e file  str n i outstr)
    (if	(= nil path)
	(setq path	 (strcat gl_BLOCKLIB_PATH "config.ini")
	      tmpfilestr (strcat gl_BLOCKLIB_PATH "lineinfotmp.dat")
	) ;_ end_setq
    ) ;_ End_if
    (vl-file-delete tmpfilestr)

    (if	(and (/= nil labellist) (setq file (open path "r")) (setq tmpfile (open tmpfilestr "w")))
	(progn
	    (while (setq str (read-line file))
		(write-line str tmpfile)
		(if (= str "[Type_Label]")
		    (progn
			(foreach e labellist
			    (setq n (length e)
					outstr ""
					i 0)
				(repeat n
					(setq outstr (strcat outstr "," (nth i e))
						i (1+ i)
					)
				)
				(write-line (vl-string-trim "," outstr) tmpfile)
			    
			) ;_ end_foreach
			(while (/= "[/Type_Label]" (setq str (read-line file)))
			) ;_ end_while
			(write-line str tmpfile)
		    ) ;_ end_progn
		) ;if
	    ) ;while
	    (prompt "\n��Ŀ��Ϣд�������ļ�.")
	    (close file)
	    (close tmpfile)
	    (vl-file-delete path)
	    (vl-file-rename tmpfilestr path)
	) ;progn
	;;else
	(prompt (strcat "\nFile_Error��д����Ŀ��Ϣʧ��!" path))
    ) ;if
    (princ)
) ;_ end_defun
 ;***************************************************************  ******************************
 ;��������:AddLineTextLabel()
 ;���ܣ��������ֱ�ǩ���������ֵ� Key:Label,Data:ͼ�ζ���ľ���б�
 ;������lent ֱ�߶�ʵ��,depth1 ������  depth2 �յ����
 ;���أ�
 ;����ʱ�䣺2014/08/28   13:00
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun AddLineTextLabel (lent       depth1     depth2     /          direction  field      i          label      labelfields           labelstr   ldata      linea      layername  linelen
                         linelist   linep1     linep2     ptext      ptext2     textlength textobj    tyname     d1         d2         blockobj   enttext    tpos       dist0      tstr
                         bnewtext   oldtextobj enttype
                        )
    ;;1 if lent has properties
    (setq tyname (ldata-get lent "Main_Type"))
    (if (setq label (ldata-get lent "Label"))
        (setq enttext (handent label))
    ) ;_ end_if
    (if enttext
        (setq oldtextobj (vlax-ename->vla-object enttext)
			enttype (cdr (assoc 0 (entget enttext))))
    ) ;_ end_if
	;;����п���ָ����������
	(if (/= enttype "TEXT") 
		(progn
			(setq oldtextobj nil)
			(ldata-put lent "Label" nil)
		)
	)	
    ;; length and angle of the line 
    (setq linelist  (entget lent)
          layername (cdr (assoc 8 linelist))
          linep1    (cdr (assoc 10 linelist))
          linep2    (cdr (assoc 11 linelist))
          linelen   (distance linep1 linep2)
          linea     (angle linep1 linep2)
          ptext     (list (/ (+ (car linep1) (car linep2)) 2)
                          (/ (+ (cadr linep1) (cadr linep2)) 2)
                          0
                    ) ;_ end_list
    ) ;_ end_setq
    (if (= nil gl_Type_Label_List)
        (setq gl_Type_Label_List (ReadTypeLabelList nil))
    ) ;_ end_if
    (if (= gl_TableColorList nil)
        (setq gl_TableColorList (ReadColorConfig nil))
    ) ;_ end_if

    (if (and (/= nil tyname) gl_Type_Label_List)
        (progn
            (setq labelfields (assoc tyname gl_Type_Label_List)
            ) ;_ end_setq
            (if labelfields
                ;;Draw a label
                (progn
                    (setq labelstr ""
                          i 0
                          d1 nil ;start_deep
                          d2 nil ;the end_deep
                          textobj nil
						  bshowlabel (atoi (nth 1 labelfields))
                    ) ;_ end_setq
					(if (and (= 0 bshowlabel) oldtextobj (= "TEXT" enttype))
						(progn
							(vla-delete oldtextobj)
							(setq oldtextobj nil)
						)
					)
					(if (= 1 bshowlabel)
						(progn
							(repeat (length labelfields)
								(setq field (nth i labelfields))

								(if (= field "TYPE")
									(setq labelstr (strcat labelstr (nth 2 (assoc tyname gl_TableColorList))))
								) ;����
								(if (= field "D_S")
									(setq labelstr (strcat labelstr " DN" (ldata-get lent "D_S")))
								) ;�ܾ�
								(if (= field "LENGTH")
									(setq labelstr (strcat labelstr "-" (rtos linelen 2 1)))
								) ;����
								(if (= field "Material")
									(setq labelstr (strcat labelstr " " (ldata-get lent "Material")))
								) ;_ end_if
								(if (= field "Pressure")
									(setq labelstr (strcat labelstr " " (ldata-get lent "Pressure")))
								) ;_ end_if
								(if (= field "Voltage")
									(setq labelstr (strcat labelstr " " (ldata-get lent "Voltage")))
								) ;_ end_if
								(if (= field "Start_Deep")
									(setq d1 (rtos depth1 2 2))
								) ;_ end_if
								(if (= field "End_Deep")
									(setq d2 (rtos depth2 2 2))
								) ;_ end_if
								(setq i (1+ i))
							) ;repeat

							(setq linea_t linea) ;label text angle
							(if (and (>= linea (* pi 0.5)) (< linea (* pi 1.5)))
								(setq linea_t (- linea pi))
							) ;text direction:left to right
							(if (and d1 d2)
								(if (= d1 d2)
									(setq labelstr (strcat labelstr " " d1 "m"))
									(if (= linea linea_t)
										(setq labelstr (strcat labelstr " " d1 "~" d2 "m"))
										(setq labelstr (strcat labelstr " " d2 "~" d1 "m"))
									) ;_ end_if
								) ;_ end_if
							) ;_ end_if

							(setq labelstr (vl-string-trim " " labelstr))

							(setq textlength (* (strlen labelstr) (/ 2.0 gl_MAP_SCALE))
								  ptext2     (polar (list (car ptext) (cadr ptext)) (+ linea_t (/ pi 2.0)) (/ 1.5 gl_MAP_SCALE))
							) ;_ end_setq

							(setq bnewtext nil)
							(if oldtextobj
								(progn
									(setq tpos  (ldata-get lent "Text_Pos")
										  tpos  (list (car tpos) (cadr tpos) (caddr tpos))
										  dist0 (distance tpos ptext2)
										  tstr  (vla-get-textstring oldtextobj)
									) ;_ end_setq
									(if (> dist0 (/ 5 gl_MAP_SCALE));;��ע���߶��ֵ�ľ���>5,�ػ�.
										(setq bnewtext T)
									) ;_ end_if
									(if (or (>= textlength linelen) (<= linelen 20)) ;;�����˵�λ�ú�����̫����ɾ����ע��
										(vla-delete oldtextobj)
									)
									(if (/= tstr labelstr)
										(setq bnewtext T)
									) ;_ end_if
								) ;_ end_progn
								(setq bnewtext T) ;else enttext nil
							) ;_ end_if

							(if (and bnewtext (> linelen 20) (< textlength linelen)) ;���ֳ���С���߶γ���,���߶γ��ȴ���20m
								(progn
									(setq textobj (vla-addtext (vla-get-modelspace
																   (vla-get-Activedocument (vlax-get-acad-object))
															   ) ;_ end_vla-get-modelspace
															   labelstr
															   (vlax-3D-point ptext2)
															   (/ 2.0 gl_MAP_SCALE)
												  ) ;_ end_vla-addtext
									) ;_ end_setq
									(if (and textobj oldtextobj)
										(vla-delete oldtextobj)
									) ;_ end_if
									(vla-put-layer textobj layername)
									(vla-put-alignment textobj acAlignmentMiddle)
									(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext2))
									(vla-put-Rotation textobj linea_t)
									;;attach the text handle to line
									(if (= nil label)
										(setq label "")
									) ;_ end_if
									(ldata-put lent "Label" (vla-get-handle textobj))
									(ldata-put lent "Text_Pos" (append ptext2 (list linea_t)))
									(ldata-put lent "M_Text" labelstr)
									(ldata-put textobj
													"LINE"
													(list (cdr (assoc 5 linelist)))
									) ;_ end_vlax-ldata-put
								) ;_ end_progn
							) ;if
						)
					)
                ) ;progn
            ) ;if
        ) ;progn
    ) ;if
) ;_ end_defun

 
 ;;ɾ�����ֱ�ע
 ;;����:�߶�ʵ������
 (defun DelTextLabel(entl / label enttext oldtextobj)
	(if (setq label (ldata-get entl "Label"))
        (if (setq enttext (handent label))
			(if (setq oldtextobj (vlax-ename->vla-object enttext))
				(progn
					(vla-delete oldtextobj)
					(ldata-put entl "Label" nil)
					(ldata-put entl "Text_Pos" (list 0 0 0 0))
				)
			)
		)
    ) ;_ end_if
 )
 ;*********************************************************************************************
 ;��������:AddLineFlowdirect()
 ;���ܣ���������,�������������������FlowBlock ��
 ;������lent ֱ�߶�ʵ��
 ;���أ�
 ;����ʱ�䣺2014/12/20  13:00
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun AddLineFlowdirect (lent / blockobj entf flowdirect fb layername linea linelist linemid
						  		linep1 linep2 tyname entobj etype entlist)
    (setq tyname (ldata-get lent "Main_Type"))
    (if (or (= tyname "G") (= tyname "P") (= tyname "Y") (= tyname "W"))
        (progn
            ;; length and angle of the line 
            (setq linelist   (entget lent)
                  layername  (cdr (assoc 8 linelist))
                  linep1     (cdr (assoc 10 linelist))
                  linep2     (cdr (assoc 11 linelist))
                  linea      (angle linep1 linep2)

                  Flowdirect (ldata-get lent "Flowdirect")
            ) ;_ end_setq
			;;del old flowdirection block
			(if (setq fb (ldata-get lent "FlowBlock"))
                (if (setq entf (handent fb))
					(progn
						(setq entlist (entget entf))
						(if (setq etype (cdr (assoc 0 entlist)))
							(if (= "INSERT" etype)
								(if (= "lx" (cdr (assoc 2 entlist)))
									(vla-delete  (vlax-ename->vla-object entf))
								)
							)
						
						)
					)	
                ) ;_ end_if
            ) ;_ end_if
			
			(if (= 0 Flowdirect)
			     (ldata-delete lent "FlowBlock" )
			)
            ;;��������
            (setq blockobj nil
                  linea    (angle linep1 linep2)
                  linemid  (list (/ (+ (car linep1) (car linep2)) 2)
                                 (/ (+ (cadr linep1) (cadr linep2)) 2)
                                 (/ (+ (caddr linep1) (caddr linep2)) 2)
                           ) ;_ end_list
            ) ;_ end_setq
			
            (if (= 1 Flowdirect)
                (setq blockobj (Drawblock linemid linea "lx.dwg"))
            ) ;_ end_if
            (if (= 2 Flowdirect)
                (setq blockobj (Drawblock linemid (+ linea pi) "lx.dwg"))
            ) ;_ end_if
            (if blockobj
                (progn
                    (vla-put-layer blockobj layername)
		    		(ldata-put lent "FlowBlock" (vla-get-handle blockobj))
                ) ;_ end_progn
            ) ;_ end_if
        ) ;_ end_progn
    ) ;_ end_if
) ;_ end_defun

 ;*********************************************************************************************
 ;��������:C:SetShowlabel (ent)
 ;���ܣ������Ƿ���ʾ���ֱ�ע
 ;������
 ;���أ�
 ;����ʱ�䣺2015/01/17   10:35
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun C:SetShowlabel()
	(if (= nil gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	(setq typelist nil)
	(foreach e gl_TableColorList
		(setq typelist (cons (strcat (car e) "-" (caddr e)) typelist))
	)
	(setq typelist (reverse typelist))
	
	(print typelist)
	(setq types (Fun_InPutString "ALL"
									  "USERS1"
									  "\n����Ҫ���ƵĹ������ͣ���������ö���','�ָ���ȫ��ѡ������'All'����")
		  )
 )
 ;*********************************************************************************************
 ;��������:UpdateLabel (ent)
 ;���ܣ����߶�λ�ñ仯ʱ,�޸ı�ע,������������ֱ�ע
 ;��������ʵ����
 ;���أ�
 ;����ʱ�䣺2015/01/17   00:05
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun UpdateLabel(ent d1 d2 / deeps)
 ;;��ע���֣���ʾ�����������ļ�����
	(if	(and (/= nil d1) (/= nil d2))
		(progn
			(AddLineTextLabel ent d1 d2)
			(AddLineFlowdirect ent)
		)
		(if (setq deeps (GetDeeps ent))
			(progn
				(AddLineTextLabel ent (car deeps) (cadr deeps))
				(AddLineFlowdirect ent)
			)
		)
	)
 )
 
(defun C:Addlabel (/ ent)
    (if (setq ent (car (entsel "\n��ӱ�ע����ѡ����߶Σ�")))
        (if (= "LINE" (cdr (assoc 0 (entget ent))))
            (AddLineTextLabel ent)
            (prompt "\n����ѡ��Ķ�����ֱ�߶Ρ�")
        ) ;_ end_if
    ) ;_ end_if
    (prin1)
) ;_ end_defun

;*********************************************************************************************
 ;��������:C:InsertOneHeightMark()
 ;���ܣ������������߶εĸ߳�
 ;������
 ;���أ�
 ;����ʱ�䣺2015/03/21   00:05
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun C:InsertOneHeightMark( / bpoint entlist ep1 sp typename )
	(prompt "\n���뵥�����ߵ�̱߳�ע.\n")
	(setq ep1 nil 
		bpoint nil)
	(Addfont 'acstyle)
	(while (not bpoint)
		(if (setq ep1 (car (entsel "\nѡ����ߵ�:")))
			(progn
				(setq entlist (entget ep1)
					typename (cdr (assoc 0 entlist)))
				(if (and (= "INSERT" typename) (vlax-ldata-get ep1 gl_AppName))
					(setq bpoint T)
					(progn 
						(setq ep1 nil
							bpoint nil)
						(prompt "\nѡ��Ķ����ǹ��ߵ㣡������ѡ��.")
					)
				)
			)
		)
	)
	(if ep1
		(if (setq sp (getpoint (cdr (assoc 10 (entget ep1))) (strcat "\nѡ��̱߳�ע���λ�ã�")))
			(progn
				;;save sp
				(ldata-put ep1 "GC_X" (cadr sp))
				(ldata-put ep1 "GC_Y" (car sp))
				(AddOneHeightMark ep1)
			);;
		)
	)
	(princ)
 )
 ;*********************************************************************************************
 ;��������:C:InsertOneHeightMark()
 ;���ܣ������������߶εĸ߳�
 ;������
 ;���أ�
 ;����ʱ�䣺2015/03/21   00:05
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun C:InsertOneSHeightMark( / bpoint entlist ep1 sp typename )
	(prompt "\n�������ͨ���ߵ�ĵر�̱߳�ע.\n")
	(setq ep1 nil 
		bpoint nil)
	(Addfont 'acstyle)
	(while (not bpoint)
		(if (setq ep1 (car (entsel "\nѡ����ߵ�:")))
			(progn
				(setq entlist (entget ep1)
					typename (cdr (assoc 0 entlist)))
				(if (and (= "INSERT" typename) (vlax-ldata-get ep1 gl_AppName))
					(setq bpoint T)
					(progn 
						(setq ep1 nil
							bpoint nil)
						(prompt "\nѡ��Ķ����ǹ��ߵ㣡������ѡ��.")
					)
				)
			)
		)
	)
	(if ep1
		(if (setq sp (getpoint (cdr (assoc 10 (entget ep1))) (strcat "\nѡ��̱߳�ע���λ�ã�")))
			(progn
				;;save sp
				(ldata-put ep1 "SGC_X" (cadr sp))
				(ldata-put ep1 "SGC_Y" (car sp))
				(AddOneSHeightMark ep1)
			);;
		)
	)
	(princ)
 )
 ;*********************************************************************************************
 ;��������:AddOneHeightMark(ep1)
 ;���ܣ������������߶εĸ߳�
 ;������
 ;���أ�
 ;����ʱ�䣺2015/03/21   00:05
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun AddOneHeightMark(ep1 / connectpoints depth depth1 depth2 depthe depths edge el 
	entlist ep2  height linea linea_t lineobj llength maintype midp newarray pent pl10 pl11 
	plineobj pos pos1 pose ptext sp subsidstr textobj typename etext)
	(if (and (setq gcx (ldata-get  ep1 "GC_X")) (setq gcy (ldata-get  ep1 "GC_Y")))
		(if (and (/= gcx 0) (/= gcy 0))	;;;BUG (0,0,0)�޷��Զ������ע
			(progn
				;;save sp
				(setq sp (list gcy gcx 0))
				;;add layer
				(setq maintype (ldata-get ep1 "Main_Type"))
				(Addlayer maintype (strcat maintype "_GC"))
				;;add line and gc text
				(setq pos (cdr (assoc 10 (entget ep1)))
					ConnectPoints	(GetConnectPoints (car pos) (cadr pos) "INSERT")
					edge nil
					llength (/ 11.0 gl_MAP_SCALE);;��ע�߳���5.5
				)
				(if	(listp ConnectPoints)
					(progn
						;;�ö�������ӵ�����㣬����ֱ�ߡ�
						(setq newarray (vlax-make-safearray vlax-vbDouble (cons 0 3) )) ;_ End_vlax-make-safearray
						(vlax-safearray-fill newarray (list (car pos) (cadr pos) (car sp) (cadr sp))) ;_ End_vlax-safearray-fill
						(setq plineobj (vla-addlightweightpolyline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
																   (vlax-make-variant newarray))
						) ;_ End_vla-addlightweightpolyline
						(setq npoint (length ConnectPoints))
						(foreach pent  ConnectPoints
							;;add line
							(setq ep2 (car pent)
								pos1 (cdr (assoc 10 (entget ep2)))
								linea (angle pos pos1)	;;mark line angle
								pose (polar sp linea llength);;end point
								el (cadr pent)
								pl10 (cdr (assoc 10 (entget el)))
								pl11 (cdr (assoc 11 (entget el)))
								height 0;;mark height
							)
							(if (vlax-ldata-get el gl_AppName);;ֱ�߱����ǹ��߶Σ�������ֱ������
								(progn
									
									;;and text
									(setq linea_t linea) ;label text angle
										(if (and (>= linea (* pi 0.5)) (< linea (* pi 1.5)))
											(setq linea_t (- linea pi))
										) ;text direction:left to right
									(if (and (setq depths (ldata-get el "Start_Deep")) (setq depthe (ldata-get el "End_Deep")));;the line has depth 
										;;depth in line
										(if (< (distance (list (car pos) (cadr pos) 0) (list (car pl10) (cadr pl10) 0)) gl_MIN);;same point
											(setq depth depths
												height (- (last pos) depth)
											)
											(setq depth depthe
												height (- (last pos) depth)
											)
										)
										;;else depth in point
										(if (and (setq depth1 (ldata-get ep1 "Depth")) (setq depth2 (ldata-get ep2 "Depth")))
											(progn
												(setq height (- (last pos) depth1))
												;;��ˮ������ˮ��
												(if (setq subsidstr (ldata-get ep2 "Subsid"))
													(if (or (= subsidstr "��ˮ��") (= subsidstr "��ˮ��"))
														(setq height (- (last pos1) depth2)
															height (- height 0.06));;�̵߳�10cm�ڵ������
													)
												)
												;;���������,2��
												(if (> depth2 gl_MIN)
													(if (> (/ depth1 depth2) 2)
														(setq height (- (last pos1) depth2)
															height (- height 0.06)
														)
														(setq height (- (last pos) depth1))
													)
												)
											)
										)
									)
									(if (= 1 npoint)
										(progn
											
											(setq pose (polar sp 0 llength)
												midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
												ptext1 (polar (list (car midp) (cadr midp)) (* pi 0.5) (/ 1.5 gl_MAP_SCALE))
												ptext2 (polar (list (car midp) (cadr midp)) (* pi 1.5) (/ 1.5 gl_MAP_SCALE)))
											(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
													 (vlax-3D-point sp)
													 (vlax-3D-point pose)))	
											;;�ܵ׸߳�
											(setq textobj (vla-addtext (vla-get-modelspace
																		   (vla-get-Activedocument (vlax-get-acad-object))
																	   ) ;_ end_vla-get-modelspace
																	   (rtos height 2 3)
																	   (vlax-3D-point ptext2)
																	   (/ 2.0 gl_MAP_SCALE)
														  ) ;_ end_vla-addtext
											) ;_ end_setq
											(vla-put-alignment textobj acAlignmentMiddle)
											(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext2))
											;;����߳�
											(Addlayer maintype (strcat maintype "_SGC"))
											(setq textobj (vla-addtext (vla-get-modelspace
																		   (vla-get-Activedocument (vlax-get-acad-object))
																	   ) ;_ end_vla-get-modelspace
																	   (rtos (last pos) 2 3)
																	   (vlax-3D-point ptext1)
																	   (/ 2.0 gl_MAP_SCALE)
														  ) ;_ end_vla-addtext
											) ;_ end_setq
											(vla-put-alignment textobj acAlignmentMiddle)
											;(setq ecolor (vla-get-truecolor textobj))
											;(vla-put-colorindex ecolor acRed)
											;(vla-put-truecolor textobj ecolor)
											(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext1))
											
											(setq etext (vlax-vla-object->ename textobj))
											(ldata-put etext "SGC_X" (cadr pos))
											(ldata-put etext "SGC_Y" (car pos))
											
										)
										(progn
											(setq midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
												ptext (polar (list (car midp) (cadr midp)) (+ linea_t (/ pi 2.0)) (/ 1.5 gl_MAP_SCALE)))
											(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
													 (vlax-3D-point sp)
													 (vlax-3D-point pose)))
											(setq textobj (vla-addtext (vla-get-modelspace
																		   (vla-get-Activedocument (vlax-get-acad-object))
																	   ) ;_ end_vla-get-modelspace
																	   (rtos height 2 3)
																	   (vlax-3D-point ptext)
																	   (/ 2.0 gl_MAP_SCALE)
														  ) ;_ end_vla-addtext
											) ;_ end_setq
											(vla-put-alignment textobj acAlignmentMiddle)
											(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext))
											(vla-put-Rotation textobj linea_t)
										)
									)
									
									

								)
							)
						)
					)
					(progn
						(prompt "\nѡ��Ĺ��ߵ�δ�����������������ܱ�ע�̡߳�")
						(exit)
					)
				)
		
			)
		)	
	)
 )
 
  ;*********************************************************************************************
 ;��������:C:ReAddHeightMark()
 ;���ܣ����ݲ�����������±�ע
 ;������
 ;���أ�
 ;����ʱ�䣺2015/03/22  00:05
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun C:ReAddHeightMark( / allp entp gcx i np)
	(prompt "\n���ݲ�����������±�ע���ߵ�̡߳�")
	(Addfont 'acstyle)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0
			)
			(repeat np
				(setq entp (ssname allp i)
					i (1+ i)
				)
				(if (and  (setq gcx (ldata-get entp "GC_X")) (setq gcx (ldata-get entp "GC_X")))
					(AddOneHeightMark entp)
				)
				(if (and  (setq gcx (ldata-get entp "SGC_X")) (setq gcx (ldata-get entp "SGC_X")))
					(AddOneSHeightMark entp)
				)
			)
		)
	)
	(princ)
 )
 
  (defun C:ContinueAddHeightMark( /  allp app con entp  i np p0 pmax pmin sp gcx gcy)
	(prompt "\n������ע���ߵ�ܵ׸̡߳�")
	(Addfont 'acstyle)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0
				con "Y"
				app	 (vlax-get-acad-object)
			)
			(while (and (< i np) (= con "Y"))
				(setq entp (ssname allp i)
					i (1+ i)
					p0 (cdr (assoc 10 (entget entp)))
					p0 (list (car p0) (cadr p0) 0)
					pmin (polar p0 (* 1.25 pi) 30 )
					pmax (polar p0 (* 0.25 pi) 30)
					gcx (ldata-get entp "GC_X")
					gcy (ldata-get entp "GC_Y")
				)
				(if (or (= nil gcx) (= nil gcy))
					(progn
						(vla-zoomwindow app (vlax-3D-point pmin) (vlax-3D-point pmax))
						(if (setq sp (getpoint p0 (strcat "\nѡ��̱߳�ע���λ�ã�")))
							(progn
								;;save sp
								(ldata-put entp "GC_X" (cadr sp))
								(ldata-put entp "GC_Y" (car sp))
								(AddOneHeightMark entp)
							)
						);;(defaultvalue useri promotstr / tmpvalue)
						(if (setq con (Fun_InPutString "Y" "USERS1" "������ע(N/Y):"))
							(setq con (strcase con))
						)
					)	
				)	
			)
			(prompt (strcat "\n��ע��" (rtos i 2 0) "�����ߵ�̡߳�")) 
		)
	)
	(princ)
 )
 
 ;;��ע����߳�
  (defun C:ContinueAddSHeightMark( /  allp app con entp  i np p0 pmax pmin sp gcx gcy)
	(prompt "\n������ע���ߵ����̡߳�")
	(Addfont 'acstyle)
	(if (setq allp (ssget "X" (list (cons 0 "INSERT" ) (list -3 (list gl_AppName)))))
		(progn
			(setq np (sslength allp)
				i 0
				con "Y"
				app	 (vlax-get-acad-object)
			)
			(while (and (< i np) (= con "Y"))
				(setq entp (ssname allp i)
					i (1+ i)
					p0 (cdr (assoc 10 (entget entp)))
					p0 (list (car p0) (cadr p0) 0)
					pmin (polar p0 (* 1.25 pi) 30 )
					pmax (polar p0 (* 0.25 pi) 30)
					gcx (ldata-get entp "SGC_X")
					gcy (ldata-get entp "SGC_Y")
				)
				(if (or (= nil gcx) (= nil gcy))
					(progn
						(vla-zoomwindow app (vlax-3D-point pmin) (vlax-3D-point pmax))
						(if (setq sp (getpoint p0 (strcat "\nѡ�����̱߳�ע���λ�ã�")))
							(progn
								;;save sp
								(ldata-put entp "SGC_X" (cadr sp))
								(ldata-put entp "SGC_Y" (car sp))
								(AddOneSHeightMark entp)
							)
						);;(defaultvalue useri promotstr / tmpvalue)
						(if (setq con (Fun_InPutString "Y" "USERS1" "������ע(N/Y):"))
							(setq con (strcase con))
						)
					)	
				)	
			)
			(prompt (strcat "\n��ע��" (rtos i 2 0) "�����ߵ�ر�̡߳�")) 
		)
	)
	(princ)
 )
 
 (defun AddOneSHeightMark(ep1 / connectpoints edge el gcsx gcsy gcx gcy  height lineobj llength maintype 
				midp newarray npoint plineobj pos pose ptext sp textob)
	(if (and (setq gcsx (ldata-get  ep1 "SGC_X")) (setq gcsy (ldata-get  ep1 "SGC_Y")))
		(if (and (/= gcsx 0) (/= gcsy 0))	;;;BUG (0,0,0)�޷��Զ������ע
			(progn
				;;save sp
				(setq sp (list gcsy gcsx 0))
				;;add layer
				(setq maintype (ldata-get ep1 "Main_Type"))
				(Addlayer maintype (strcat maintype "_SGC"))
				;;add line and gc text
				(setq pos (cdr (assoc 10 (entget ep1)))
					ConnectPoints	(GetConnectPoints (car pos) (cadr pos) "INSERT")
					edge nil
					llength (/ 11.0 gl_MAP_SCALE);;��ע�߳���5.5
					npoint 0
				)
				(if	(listp ConnectPoints)
					(progn
						(setq npoint (length ConnectPoints))
						(if (and (= 1 npoint) (setq gcx (ldata-get  ep1 "GC_X")) (setq gcy (ldata-get  ep1 "GC_Y")))
							(setq sp (list gcy gcx 0))
						)
					)	
				)
				;;�ö�������ӵ�����㣬����ֱ�ߡ�
				(setq newarray (vlax-make-safearray vlax-vbDouble (cons 0 3) )) ;_ End_vlax-make-safearray
				(vlax-safearray-fill newarray (list (car pos) (cadr pos) (car sp) (cadr sp))) ;_ End_vlax-safearray-fill
				(setq plineobj (vla-addlightweightpolyline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
														   (vlax-make-variant newarray))
				) ;_ End_vla-addlightweightpolyline
			
				;(foreach pent  ConnectPoints
					;;add line
				(setq pose (polar sp 0 llength);;end point
					height (last pos);;mark height
				)
				(if (vlax-ldata-get ep1 gl_AppName);;ֱ�߱����ǹ��߶Σ�������ֱ������
					(if (> npoint 1 )
						(progn
							(setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
												 (vlax-3D-point sp)
												 (vlax-3D-point pose)))
							;;and text
							
							(setq midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
								ptext (polar (list (car midp) (cadr midp)) (* pi 0.5) (/ 1.5 gl_MAP_SCALE)))
							(setq textobj (vla-addtext (vla-get-modelspace
														   (vla-get-Activedocument (vlax-get-acad-object))
													   ) ;_ end_vla-get-modelspace
													   (rtos (last pos) 2 3)
													   (vlax-3D-point ptext)
													   (/ 2.0 gl_MAP_SCALE)
										  ) ;_ end_vla-addtext
							) ;_ end_setq
							(vla-put-alignment textobj acAlignmentMiddle)
							(vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext))
						)
						; (progn
							; (setq pose (polar sp 0 llength)
								; midp (list (/ (+ (car sp) (car pose)) 2) (/ (+ (cadr sp) (cadr pose)) 2) 0)
								; ptext1 (polar (list (car midp) (cadr midp)) (* pi 0.5) (/ 1.5 gl_MAP_SCALE))
								; ptext2 (polar (list (car midp) (cadr midp)) (* pi 1.5) (/ 1.5 gl_MAP_SCALE)))
							
							; ;;����߳�
							
							; (setq textobj (vla-addtext (vla-get-modelspace
														   ; (vla-get-Activedocument (vlax-get-acad-object))
													   ; ) ;_ end_vla-get-modelspace
													   ; (rtos (last pos) 2 3)
													   ; (vlax-3D-point ptext1)
													   ; (/ 2.0 gl_MAP_SCALE)
										  ; ) ;_ end_vla-addtext
							; ) ;_ end_setq
							; (vla-put-alignment textobj acAlignmentMiddle)
							; ;(setq ecolor (vla-get-truecolor textobj))
							; ;(vla-put-colorindex ecolor acRed)
							; ;(vla-put-truecolor textobj ecolor)
							; (vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext1))
							
							; (setq etext (vlax-vla-object->ename textobj))
							; (ldata-put etext "SGC_X" (cadr pos))
							; (ldata-put etext "SGC_Y" (car pos))
							; ;;�ܵ׸߳�
							; (Addlayer maintype (strcat maintype "_GC"))
							; (setq lineobj (vla-addline (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
									 ; (vlax-3D-point sp)
									 ; (vlax-3D-point pose)))	
							; (setq textobj (vla-addtext (vla-get-modelspace
														   ; (vla-get-Activedocument (vlax-get-acad-object))
													   ; ) ;_ end_vla-get-modelspace
													   ; (rtos height 2 3)
													   ; (vlax-3D-point ptext2)
													   ; (/ 2.0 gl_MAP_SCALE)
										  ; ) ;_ end_vla-addtext
							; ) ;_ end_setq
							; (vla-put-alignment textobj acAlignmentMiddle)
							; (vla-put-TextAlignmentPoint textobj (vlax-3D-point ptext2))
							; (ldata-put etext "GC_X" (cadr pos))
							; (ldata-put etext "GC_Y" (car pos))
							
						; )
					)
				)
			)
		)	
	)
 )
 
 ;;����̱߳�ע����
 (defun C:ClearGCData( / bpoint entlist ep1 i j pset typename)
	(prompt "\n����������ߵ�̱߳�ע���ԣ�����ɾ����ע�Ĺܵ׻�ܶ��߳�����.\n��ѡ����Ҫ����߳����ԵĹ��ߵ㣺")
	(setq ep1 nil 
		bpoint nil
		i 0 j 0)
	
	(if (setq pset (ssget))
		(repeat (sslength pset)
			(setq ep1 (ssname pset i)
				i (1+ i))
			(setq entlist (entget ep1)
				typename (cdr (assoc 0 entlist)))
			(if (and (= "INSERT" typename) (vlax-ldata-get ep1 gl_AppName))
				(progn
					(ldata-delete ep1 "GC_X")
					(ldata-delete ep1 "GC_Y")
					(ldata-delete ep1 "SGC_X")
					(ldata-delete ep1 "SGC_Y")
					(setq j (1+ j))
				)
			)
		)
	)
	(prompt (strcat "\n�����" (rtos j 2 0) "�����ߵ�ĸ߳����ԡ�"))
	(princ)
 )
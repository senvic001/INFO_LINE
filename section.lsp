;*********************************************************************************************
;��������:C:db_section( )
;���ܣ��ڹ���ƽ��ͼ�У�ѡ��һ������ߣ�������������߶εĽ��㣬Ȼ����������ͼ��Ӧ���ڹ������
;����:
;����ֵ��nil
;����ʱ�䣺2016/01/15 11:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:db_section( / block blockname border datalist dimz dist e e1 e2 ent2 
index interps newblock objblocks p0 para0 pl plineobj pmax pmin startzh lengthpl
 ts x1 xend xrange xrefset xscale xstep y1 yend yrange yscale ystep zlist)
;;2ѡ������ֱ��
	(princ "\nsection:�������档")
	(if (not (setq pl (car (entsel "\n ��ѡ�����ߣ�"))))
		(*error* "δѡ�񵽶���")
	)
	;;1.check type
	(if (/= "LWPOLYLINE" (cdr (assoc 0 (entget pl))))
		(*error* "ѡ��Ķ����Ƕ���ߣ�")
	)
	(setq plineobj (vlax-ename->vla-object pl)
		para0 nil)
	(while (= nil para0)
		(setq point0 (getpoint "\nѡ��������㣺")
			para0 (vlax-curve-getParamAtPoint plineobj point0)
			lengthpl (vla-get-length plineobj)
		)
	)
	(while (not (setq	startzh (getreal "\n ����������׮�ţ���������λΪ�ף�:"))))
	
	;;2.choose lines in range
	(vla-GetBoundingBox plineobj 'pmin 'pmax)
	(vla-zoomwindow (vlax-get-acad-object) pmin pmax)
	(setq pmin (vlax-safearray->list pmin)
		  pmax (vlax-safearray->list pmax)
		  pmin (list (nth 0 pmin) (nth 1 pmin))
		  pmax (list (nth 0 pmax) (nth 1 pmax))
	) ;_ end setq

	(setq xrefset (ssget "C"
						 pmin
						 pmax
						 (list (cons 0 "LINE") (list -3 (list gl_AppName)))
						 ) ;_ End_ssget
	) ;_ end setq

	(if	(or (= nil xrefset) (= 0 (sslength xrefset)))
		(*error* "δ�ҵ����߶ζ���")
	) ;_ End_if

	(setq index	   0
		  datalist nil
	) ;_ end setq

	;;3���㽻��
	(repeat	 (sslength xrefset)
		(setq ent2 (ssname xrefset index))
		(if	(setq interps (m_IntersectWith pl ent2))
			(foreach p0 interps
				(setq ;;p0	   (car interps) ;ֱ�߶�ֻ��Ψһ�Ľ���
					  dist	   (vlax-curve-getDistAtPoint plineobj (list (car p0) (cadr p0) 0) ) ;_ End_vlax-curve-getDistAtPoint
				)
				(if (> para0 0.0) (setq dist (- lengthpl dist)))
				(setq  datalist (append datalist (list (list dist ent2 p0)))) ;λ�ã�ͼԪ�����������ꣻ
			) ;_ End_progn
		) ;_ End_if
		(setq index (1+ index))
	) ;_ End_repeat
	
	;;4���ݾ�������
	(setq datalist (vl-sort
					   datalist
					   (function
						   (lambda (e1 e2)
							   (< (car e1)
								  (car e2)
								) ;_ End_<
				) ;_ End_lambda
			) ;_ End_function
		) ;_ End_vl-sort
	) ;_ End_setq
	
	;;5.�������档
	;;5.1ͼ�η�Χ
	(setq x1 (car (car datalist))
		  xend (car (last datalist))
		  xrange (- xend x1)	
	)
	(setq zlist (vl-sort datalist (function (lambda (e1 e2) (< (caddr (caddr e1)) (caddr (caddr e2))))))
		y1 (caddr (caddr (car zlist)))
		yend (caddr (caddr (last zlist)))
		yrange (- yend y1)
	)
	;;5.2������
	(setq xscale (Fun_InPutValue 1000 "USERR1"  "\n ������X������ߣ�1��" 2 )
		xscale (/ 1000.0 xscale)
		yscale (Fun_InPutValue 1000 "USERR2"  "\n ������Y������ߣ�1��" 2 )
		yscale (/ 1000.0 yscale)
	)
	;;5.3
	;;�������������һ����
	(setq dimz (getvar "DIMZIN"))
	(setvar "DIMZIN" 1)

	(setq ts (AddFont 'acts))
	(setq objBlocks (vla-get-Blocks (vla-get-Activedocument (vlax-get-acad-object))))
	(setq BlockName (strcat "SECTION_" (rtos (* 100000000 (ZL-RAND)) 2 0)))

	;;������,�������
	(while (not
			   (setq newblock (vla-Add objBlocks (vlax-3D-point 0 0 0) BlockName))
			   ) ;_ End_not
		(setq BlockName (strcat "SECTION_" (rtos (* 100000000 (ZL-RAND)) 2 0)))
		) ;_ End_while
	;;5.4��������
	(setq ystep 5 xstep 20
		border 2
	)
	(DrawHruler newblock (list x1 y1) y1 yend yscale "L" ystep )
	;;DrawZH (block basepoint zh1 range scale step
	(DrawZH newblock (list x1 y1) (+ startzh x1) xrange xscale xstep)
	;;5.5���Ƶ�	 datalist (cons (list dist ent2 p0)
	(foreach e datalist
		(DrawPipeSection newblock (cadr e) (car e) (caddr (caddr e)) (list x1 y1) xscale yscale)
	)
	;;����ͼ��
	(vla-insertblock (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
					 (vlax-3D-point (getpoint "\��ѡ���������㣺"))
					 BlockName 1 1 1 0)
	;;�ָ�
	(setvar "DIMZIN" dimz)
	;;�ָ���ǰͼ��
	;;(vla-ZoomPrevious (vlax-get-acad-object))
	(princ)
)
;*********************************************************************************************
;��������:DrawPipeSection (block entl xpos depth basepoint xscale yscale )
;���ܣ�����һ�����ߺ���棬ʶ��ԭ�ͺͷ��κ����
;����:block:����󣨱���ģ�Ϳռ䣬ͼֽ�ռ䣬ͼ��ȣ���entl �������ڵ�entityname;
;	xpos �����x���ꣻdepth �������ȣ�
;	basepoint:����㣬Ҳ��Ϊ����Ļ�׼0��; 
;	xscale yscale:������ 1:1000 =1  1:2000=0.5  1:500 =2  
;����ֵ��nil
;����ʱ�䣺2016/01/15 11:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun DrawPipeSection (block entl xpos depth basepoint xscale yscale  / aclayer app center doc ds dx dy endp 
firstpos gaoch1 gaoch2 gaochtext gl_tablecolorlist h height hend insertp insertpoint ipoint k layers maintype 
material mspace newarray nmark p1 p2 pend plobj pos pstart pt-array range rowheigt scale side sidepoint tname depthpos
startp step strk strpos text text-obj textpoint width wtlayer x0 xend xrem xstart y0 ystart zh1 zhijing)
	(setq DS (ldata-get entl "D_S")
		Material (ldata-get entl "Material")
		width nil height nil zhijing nil
		tname (ldata-get entl "Main_Type")
	)
	(if (not gl_TableColorList) (setq gl_TableColorList (ReadColorConfig nil)))
	(setq maintype (nth 2 (assoc tname gl_TableColorList)))
	(if (not maintype) (setq maintype ""))
	;;���ֹ��ӣ�Բ�ܺͷ���
	(if (= 'STR (type DS))
		(if (setq pos (vl-string-search "*" DS))
			(setq width (substr DS 1 pos)
				height (substr DS (+ 2 pos))
			)
			(setq zhijing ds)
		)
	)
	(if zhijing (setq zhijing (* xscale (/ (atof zhijing) 1000))))	;;ֱ����ʹ��x�������
	(if (< zhijing gl_MIN) (setq zhijing 0.01))
	(if width (setq width (* xscale (/ (atof width ) 1000))))
	(if height (setq height (* yscale (/ (atof height) 1000))))
	;;
	(setq x0 (car basepoint)
		y0 (cadr basepoint)
		ratio (/ (* 1.0 yscale) xscale)
	)
	(if (or (= tname "Y") (= tname "W") (= tname "P"))
		(progn	;;�ܵ�
			(setq depthpos (list (+ x0 (* xscale (- xpos x0))) (+ y0 (* yscale (- depth y0))) 0))	;;line endp
			(if zhijing (setq center (list (car depthpos) (+ (cadr depthpos) (* ratio (/ zhijing 2.0))))))
			(if height (setq center (list (car depthpos) (+ (cadr depthpos) (/ height 2.0)))))
			(setq endp (list (car center) (+ (cadr center) 20) 0))
		)
		(progn	;;�ܶ�
			(setq depthpos (list (+ x0 (* xscale (- xpos x0))) (+ y0 (* yscale (- depth y0))) 0))	;;line endp
			(if zhijing (setq center (list (car depthpos) (- (cadr depthpos) (* ratio (/ zhijing 2.0))))))
			(if height (setq center (list (car depthpos) (- (cadr depthpos) (/ height 2.0)))))
			(setq endp (list (car center) (+ (cadr center) 20) 0))
		)
	)
	; (setq center (list (+ x0 (* xscale (- xpos x0))) (+ y0 (* yscale (- depth y0))) 0)
		; endp (list (car center) (+ (cadr center) 20) 0))
	(vla-addline block (vlax-3D-point depthpos) (vlax-3D-point endp))
	(if zhijing
		(progn
			;(vla-addcircle block (vlax-3D-point center) (/ zhijing 2.0))	;;��Բ��ʾ�����ڲ�ͬ����ı�����
			(if (> ratio 1)
				(progn
					(setq vector (vlax-3D-point (polar '(0 0 0) (/ pi 2.0) (* ratio (/ zhijing 2.0)))))
					(vla-addEllipse block (vlax-3D-point center) vector (/ 1 ratio))
				)
				(progn
					(setq vector (vlax-3D-point (polar '(0 0 0) 0.0 (/ zhijing 2.0))))
					(vla-addEllipse block (vlax-3D-point center) vector ratio)
				)
			)
			;;�ܾ���ע
			(setq insertp (vlax-3D-point (list (+ (car endp) 2.6) (cadr endp) 0)))
			(setq text-obj (vla-addtext block (strcat Material "" DS) insertp 2.5))
			(vla-put-alignment text-obj acAlignmentTopLeft)
			(vla-put-TextAlignmentPoint text-obj insertp)
			(vla-rotate text-obj insertp (* pi 1.5))
		)
	)
	(if (and width height)
		(progn
			(setq pt-array (list x0 y0 (+ x0 width) y0 (+ x0 width) (- y0 height) x0 (- y0 height)) 
				newarray (vlax-make-safearray vlax-vbDouble (cons 0 (- (length pt-array) 1)))) 
			(vlax-safearray-fill newarray pt-array)
			(setq plobj (vla-AddLightweightPolyline block (vlax-make-variant newarray)))
			(vla-put-closed plobj :vlax-true)
			(vla-move plobj (vlax-3D-point x0 y0) 
							(vlax-3D-point (- (+ x0 (* xscale (- xpos x0))) (/ width 2.0)) (+ (* yscale (- depth y0)) y0 (/ height 2.0))))
			
			;;�ܾ���ע
			(setq insertp (vlax-3D-point (list (+ (car endp) 2.5) (cadr endp) 0)))
			(setq text-obj (vla-addtext block (strcat Material DS) insertp 2.5))
			(vla-put-alignment text-obj acAlignmentTopLeft)
			(vla-put-TextAlignmentPoint text-obj insertp)
			(vla-rotate text-obj insertp (* pi 1.5))
		)
	)
	;;�����ȱ�ע
	(setq insertp (vlax-3D-point (list (car endp) (cadr endp) 0)))
	(setq text-obj (vla-addtext block (strcat maintype (rtos depth 2 2)) insertp 2.5))
	(vla-put-alignment text-obj acAlignmentTopLeft)
	(vla-put-TextAlignmentPoint text-obj insertp)
	(vla-rotate text-obj insertp (* pi 1.5))
)
;;;�������׮��
;*********************************************************************************************
;��������:DrawZH (block basepoint zh1 range scale step)
;���ܣ�����ˮƽ��������׮��
;����:block:����󣨱���ģ�Ϳռ䣬ͼֽ�ռ䣬ͼ��ȣ��� basepoint:����㣬Ҳ��Ϊ����Ļ�׼0��; 
;	zh1:���׮�ţ�һ��ʵ����СXֵ�� range��ʵ��׮�ŷ�Χ, 
;	scale:������ 1:1000 =1  1:2000=0.5  1:500 =2  
;	step:���ڱ�ע���׮��֮��
;����ֵ��nil
;����ʱ�䣺2016/01/15 11:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun DrawZH (block basepoint zh1 range scale step / aclayer doc dx dy firstpos insertp k layers 
nmark pend pstart rowheigt strk strpos text-obj wtlayer x0 xend xpos xrem xstart y0 ystart)
	(setq x0 (car basepoint)
		y0 (cadr basepoint)
		dx 2 dy 2	;;������ƫ��basepointλ��
		xstart (- x0 dx)
		xend (+ x0 (* scale range) dx)
		ystart (- y0 dy)
		pstart (list xstart ystart)
		pend (list xend ystart)
	)
	;;����ͼ��
	(setq	Doc (vla-get-Activedocument (vlax-get-acad-object))
		Layers	(vla-get-layers Doc)
		AcLayer	(vla-get-activelayer Doc)
	)
	(setq wtlayer (vla-add Layers "WT-���"))
	(vla-put-activelayer Doc (vla-item Layers "WT-���"))
	(if (= nil block) (setq block (vla-get-modelspace Doc)))
	;;�������
	(setq rowHeigt 10)
	(vla-addline block (vlax-3D-point pstart)(vlax-3D-point pend))
	(vla-addline block (vlax-3D-point (list xstart (- ystart rowHeigt))) (vlax-3D-point (list xend (- ystart rowHeigt))))
	(setq nmark (fix (/ range step)))
	(if (< nmark 3) ;;׮�Ų���2����������
		(setq step (/ step 2.0)
			nmark (fix (/ range step))
		)
	)
	(setq firstPos (+ step (- zh1 (rem zh1 step))))	;;first ׮��λ�� 
	(repeat nmark
		(setq xpos (+ x0 (* scale (- firstPos zh1))))
		(vla-addline block (vlax-3D-point (list xpos ystart)) (vlax-3D-point (list xpos (- ystart rowHeigt))))
		(setq xrem (rem firstPos 1000)
			K (fix (/ firstPos 1000))
		)
		;;K
		(setq insertp (vlax-3D-point (list (- xpos 1.25) (- ystart (/ rowHeigt 2.0))))
			strK (strcat "K" (rtos K 2 0))
		)
		(setq text-obj (vla-addtext block strK insertp 2.5))
		(vla-put-alignment text-obj acAlignmentMiddleCenter)
		(vla-put-TextAlignmentPoint text-obj insertp)
		(vla-rotate text-obj insertp (* pi 1.5))
		;;+xxx
		(setq insertp (vlax-3D-point (list (+ xpos 1.25) (- ystart (/ rowHeigt 2.0))))
			strPos (strcat "+" (rtos (rem firstPos 1000) 2 0))
		)			
		(if (= xrem 0)
			(setq text-obj (vla-addtext block "+000" insertp 2.5))
			(setq  text-obj (vla-addtext block strPos insertp 2.5))
		)
		(vla-put-alignment text-obj acAlignmentMiddleCenter)
		(vla-put-TextAlignmentPoint text-obj insertp)
		(vla-rotate text-obj insertp (* pi 1.5))
		;;next pos
		(setq firstPos (+ firstPos step))
	)
	
	;;�ָ���ǰͼ��
	(vla-put-activelayer Doc AcLayer)
	(princ)
)

;;;���ƴ�ֱ����ĸ߳�ֵ���ɷ��������Ҳ�
;*********************************************************************************************
;��������:DrawHruler (insertpoint gaoch1 gaoch2 scale side)
;���ܣ���datalist ���뵽����
;����:block:����󣨱���ģ�Ϳռ䣬ͼֽ�ռ䣬ͼ��� insertpoint:�����; gaoch1:��С�̣߳� gaoch2�����߳�, 
;	scale:������ 1:1000 =1  1:2000=0.5  1:500 =2  side����L"or"R"
;	step:���ڱ�ע��ĸ߳�ֵ֮��
;����ֵ��bool
;����ʱ�䣺2011/03/27 11:00
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(vl-load-com)
(defun DrawHruler (block insertpoint gaoch1 gaoch2 scale side step / aclayer app doc endp gaochtext h
 hend insertp ipoint layers mspace p1 p2 sidepoint startp text text-obj textpoint wtlayer x0 y0)
  (setq	x0 (- (car insertpoint) 5)
	y0 (nth 1 insertpoint)
	H  (- gaoch2 gaoch1)
  )

  (setq	app    (vlax-get-acad-object)
	Doc    (vla-get-Activedocument app)
	Mspace (vla-get-modelspace Doc)
  )
  (if block (setq Mspace block))
;;;����ͼ��
  (setq	Layers	(vla-get-layers Doc)
	AcLayer	(vla-get-activelayer Doc)
  )
  (setq wtlayer (vla-add Layers "WT-���"))
  ;;(vla-put-color wtlayer AcWhite)
  ;;(vla-put-color wtlayer 0)��Ч����
  (vla-put-activelayer Doc (vla-item Layers "WT-���"))

;;;��ֱ�߱�ʾ��ߣ����3mm
  (setq	startp	  (vlax-3D-point x0 y0)
	endp	  (vlax-3D-point x0 (+ y0 (* H scale)))
	textpoint (vlax-3D-point x0 (+ 2 (+ y0 (* H scale))))
  )
  (if (= "L" (strcase side))
    (setq sidepoint (vlax-3D-point (- x0 1.5) (- (+ y0 (* H scale)) 3)))
    (setq sidepoint (vlax-3D-point (+ x0 1.5) (- (+ y0 (* H scale)) 3)))
  )
  ;;if
  (vla-addline Mspace startp endp)
  (vla-addline Mspace endp sidepoint)
  (setq gaochtext (vla-addtext Mspace "�߳�(m)" textpoint 3))
  (if	(= "L" (strcase side))
      (vla-put-alignment gaochtext acAlignmentMiddleRight)
      (vla-put-alignment gaochtext acAlignmentMiddleLeft)
    )		
  ;(vla-put-alignment gaochtext acAlignmentBottomCenter)
  (vla-put-TextAlignmentPoint gaochtext textpoint)
;;;prepare data
  (setq	ipoint 0
	hend gaoch1
  )
  (setq nmark (fix (/ H step)))
	(while (< nmark 3) ;;׮�Ų���2����������
		(setq step (/ step 2.0)
			nmark (fix (/ H step))
		)
	)
	(setq hend (+ step (- gaoch1 (rem gaoch1 step))))	;;first ׮��λ�� 
	
  (while (< hend (- gaoch2 step))
    (setq  hend   (+ hend (* ipoint step))
		endy (+ y0 (* scale (- hend y0)))
		ipoint (+ ipoint 1)
    )
    (setq p1 (vlax-3D-point x0 endy))
    (if	(= "L" (strcase side))
		(progn
			(setq p2 (vlax-3D-point (- x0 2) endy))
			(setq insertp (vlax-3D-point (- x0 3) endy ))
		)
		(progn
			(setq p2 (vlax-3D-point (+ x0 2) endy))
			(setq insertp (vlax-3D-point (+ x0 3) endy ) )
		)
    )					;if
    (vla-addline Mspace p1 p2)
    (setq text (rtos hend 2 2))
    (setq text-obj (vla-addtext Mspace text insertp 2.5))

    (if	(= "L" (strcase side))
      (vla-put-alignment text-obj acAlignmentMiddleRight)
      (vla-put-alignment text-obj acAlignmentMiddleLeft)
    )					;if
    (vla-put-TextAlignmentPoint text-obj insertp)
  )
  ;;while

;;;�ָ���ǰͼ��
  (vla-put-activelayer Doc AcLayer)
  (princ)
)

































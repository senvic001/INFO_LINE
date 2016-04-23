;;;����̽��CAD-���ݿ�ϵͳ2014-07-08
;;;

;;;ȫ�ֱ��� gl_[��д����]
(setq gl_DEBUG T) ;���Ա�־
(setq gl_INFO_LINE_PATH	 "H:\\INFO_LINE\\" ;����·��
      gl_BLOCKLIB_PATH	 (strcat gl_INFO_LINE_PATH "BlockLib\\") ;����ͼ��·��
      gl_PPROJECT_PATH	 (strcat gl_INFO_LINE_PATH "project\\") ;��Ŀ·����������ݿ��ͼ�Ρ���Ŀ�����ļ�

      gl_TableColorList	 nil ;�����ͣ���ɫ
      gl_SymbolBlockList nil ;����������͸�����ͼ��
      gl_UserSymbolList	 nil ;�û��Զ���ͼ��,����V0.0.1,����ʹ��
	  gl_FeaturesList nil	;�������͵��������((J (Sym1 ������) (..)....) (W () ()..)....)
      gl_SubList	 nil ; ������-�������б�((subname mainname ������)....)

      gl_AllDBPointList	 nil ;�������ݿ��еĵ������������
	  gl_MapNameList	nil	 ;ͼ�����й��ߵ�����ƣ���֤����Ψһ�ԣ�
	  gl_ExpNameList nil	;������̽���
      gl_PointFieldList	 nil ;���ֶ������б�((Map_No.ͼ��)...)
	  gl_LineFieldList	 nil ;���߶��ֶ������б�

      gl_Material_List	 (list "��" "UPVC" "PVC" "PE" "ͭ" "��" "����" "����" "ͭ/��" "��" "��п" "ש" "����" "������" "�մ�")
	  gl_JG_Material (list "��" "��" "����" "��" "����" )
	  gl_JG_Shape (list "��" "Բ" "��")
	  ;;0-ֱ��\n1-���ιܹ�\n2-Բ�ιܹ�\n3-���ιܹ�\n4-�˷�\n5-�ܿ�\n6-�׹�\n7-Сͨ��(d<1m)\n8-����(�ܿ�)\n9-����
	  gl_BuryWay (list "0-ֱ��" "1-���ιܹ�" "2-Բ�ιܹ�" "3-���ιܹ�" "4-�˷�" "5-�ܿ�" "6-�׹�" "7-Сͨ��(d<1m)" "8-����(�ܿ�)" "9-����")
	  gl_Line_Style (list "0-�ǿչ�" "1-�չ�" "2-��������" "3-��ֱ���߶�" "4-�ܿ�(����)" "5-�ǿ���(����)" "6-���ڹܵ�")
	  gl_JS_Type (list "��" "100-Բ��״" "101-Բ��(ƫ��)" "102-Բ��(��խ�¿�)" "103-Բ��+���;���" "104-Բ��+���ξ���" "105-������" "200-����(����)+��" "201-����(�Ա�)+��" "202-����(ƽ�沢��)")
	  gl_Well_Memo	(list "��" "����" "������" "����")
      gl_ErrorList	 nil ;��¼���� (("Handle" "ErrorInfo")....)
	  gl_ErrorInfoString "" ;�����ڶԻ���ײ��Ĵ�����Ϣ
	  gl_EditInfo	""		;�༭ʱ��ʾ��ʾ��Ϣ
      gl_Time		 0 ;��¼����ʱ��
	  gl_DataBaseTime nil	;��¼���ݿⱣ��ʱ��
) ;_ End_setq
(setq gl_MIN 0.0001) ;��ֵ
(setq gl_AppName "INFO_LINE") ;xrecord ���ֵ�,����lata-put key
(setq gl_Version "5.0.0")

;;ͼ�����п�������б�,���ڵ�������,�ӿ쵼���ٶ�
(setq gl_BlockNameList nil)

(regapp gl_AppName)
;;ͼ�α�����(1:1000����1��1:500����2��1:2000����0.5
(if (Xrecord-Get gl_AppName "SCALE")
		(setq gl_MAP_SCALE (atof (car (Xrecord-Get gl_AppName "SCALE"))))
    (progn
		(Xrecord-Rebuild gl_AppName "SCALE" "2")
		(setq gl_MAP_SCALE 2.0)
    ) ;_ end_progn
) ;_ end_if

(LineInfo_GetSupportPath)

;;;----------------------��д���������ļ�---------------------------------------------

 ;*********************************************************************************************
 ;��������:ReadColorConfig()
 ;���ܣ���ȡ���������ļ��е������ͣ���ɫ���������ơ��ܵ��������
 ;������filepath
 ;���أ����  eg:((J,5,��ˮ,0,0,255))
 ;����ʱ�䣺2014/07/09   12:16
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun ReadColorConfig (path / b colorindex file g hname msm pos r str tablecolorlist typename)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ End_if
    (setq TableColorList nil)
    (if	(setq file (open path "r"))
		(progn
			(while (setq str (read-line file))
				(if (= str "[TableColorConfig]")
					(while (/= "[/TableColorConfig]" (setq str (read-line file)))
						(if (/= "//" (substr str 1 2));;ע����
							(setq pos	     (vl-string-search "," str)
								  typename	     (substr str 1 pos)

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  colorindex     (atoi (substr str 1 pos))

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  Hname	     (substr str 1 pos)

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  R		     (atoi (substr str 1 pos))

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  G		     (atoi (substr str 1 pos))

								  str	     (substr str (+ 2 pos))
								  pos	     (vl-string-search "," str)
								  B		     (atoi (substr str 1 pos))

								  msm	     (atoi (substr str (+ 2 pos))) ;������ 1 �ܶ�� 3 �ܵ����ˮ����ˮ����ˮ��
								  TableColorList (cons
										 (list typename colorindex Hname (list R G B) msm)
										 TableColorList
										 ) ;_ End_append
							) ;setq
						)
					) ;while
				) ;if
			) ;while
			(close file)
		) ;progn
	;;else
	(prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if
    (reverse TableColorList)
) ;_ End_defun

;*********************************************************************************************
;��������:ReadTypedFeatureList()
;���ܣ���ȡ���������ļ��е�������FeatureName����
;������
;���أ�((J (Sym1 ������) (...)) (W (Sym1 ������))...) ��� ������ ������
;����ʱ�䣺2014/12/16   12:40
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun ReadTypedFeatureList( path / alllist bend blockname cname featurelist file  maintype 
							newlist pos str outlist)
	(if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq FeatureList nil
		AllList nil
		newlist nil
		bEnd nil)
    (if	(setq file (open path "r"))
		(progn
			(while (and (not bEnd)(setq str (read-line file)))
				(if (= str "[FeatureName]")
					(progn
						(while (/= "[/FeatureName]" (setq str (read-line file)))
							(if (/= "//" (substr str 1 2));;ע����
								(progn
									(setq pos	 (vl-string-search "," str)
										  maintype (substr str 1 pos) ;first letter is the class name;

										  str	 (substr str (+ 2 pos))
										  pos	 (vl-string-search "," str)
										  blockname	 (substr str 1 pos)
										  blockname	 (strcat blockname ".dwg")
										  cname	 (substr str (+ 2 pos))
									) ;_ end_setq
									(if (setq FeatureList (assoc maintype AllList))
										(setq newlist (cons (list blockname cname) (cdr FeatureList))
											AllList (subst (cons maintype newlist) FeatureList AllList)
										)
										;;else
										(setq AllList (cons (cons maintype (list (list blockname cname))) AllList ))
									)
								)
							)
						) ;while
						(setq bEnd T)
					)
				) ;if
			) ;while
			(close file)
		) ;progn
		;;else
		(prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if
    ;;��ת˳��
	(setq outlist nil)
	(foreach e AllList
		(setq FeatureList (cdr e)
			FeatureList (reverse FeatureList)
			outlist (cons (cons (car e) FeatureList) outlist)
		)
	)
	outlist
)
 ;*********************************************************************************************
 ;��������:ReadSymbolConfig()
 ;���ܣ���ȡ���������ļ��е��ͼ������
 ;������filepath��Ĭ��ϵͳĿ¼
 ;���أ����  eg:(("J21" "sym1.dwg"  "���ž�")...)
 ;����ʱ�䣺2014/07/09   21:10
 ;�޸�ʱ�䣺2014/07/11   21:00 �޸���config.ini��[BlockName] ���+S_codeʶ����ſ�
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun ReadSymbolConfig	(path / blocklist blockname file cname pos str symbolname)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq BlockList nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (setq str (read-line file))
		(if (= str "[BlockName]")
		    (while (/= "[/BlockName]" (setq str (read-line file)))
				(if (/= "//" (substr str 1 2));;ע����
					(setq pos	 (vl-string-search "," str)
						  symbolname (substr str 1 pos) ;first letter is the class name;

						  str	 (substr str (+ 2 pos))
						  pos	 (vl-string-search "," str)
						  blockname	 (substr str 1 pos)
						  blockname	 (strcat blockname ".dwg")
						  cname	 (substr str (+ 2 pos))

						  BlockList	 (cons (list symbolname blockname cname) BlockList)
					) ;_ end_setq
				)
		    ) ;while
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if
    (reverse BlockList)
) ;_ end_defun
;;;��ȡ������-�������б�((subname mainname ������)....)
(defun ReadSubsymbolConfig (path / blocklist cname file mainname pos str subname)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq BlockList nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (setq str (read-line file))
		(if (= str "[Sub_Type]")
		    (while (/= "[/Sub_Type]" (setq str (read-line file)))
				(if (/= "//" (substr str 1 2));;ע����
					(setq
						pos	      (vl-string-search "," str)
						subname   (substr str 1 pos)

						str	      (substr str (+ 2 pos))
						pos	      (vl-string-search "," str)
						mainname  (substr str 1 pos)
						cname     (substr str (+ 2 pos))

						BlockList (cons (list subname mainname cname) BlockList)
					) ;_ end_setq
				)
		    ) ;while
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if
    (reverse BlockList)
) ;_ end_defun
;;�������͡�J���õ�subsidlist
(defun getsubsidlist-fromtype  (typestr / e symstr subsidlist typename) ;"J"
	(setq subsidlist (list "��")
		typename (substr typestr 1 1) )
	(if (= nil gl_SymbolBlockList)
		(setq gl_SymbolBlockList (ReadSymbolConfig nil))
		)
	(foreach e  gl_SymbolBlockList
		(if (/= typestr nil) ;���ݵ����ͣ�����subsid����
			(progn
				(setq symstr  (substr (car e) 1 1)
					  ) ;��һ���ַ�Ϊ���
				(if (= typename symstr)
					(setq subsidlist (append subsidlist (list (nth 2 e))))
					)
				) ;progn
			) ;if
		) ;foreach
	subsidlist
) ;defun

;;�������͡�J���õ�subsidlist
(defun getfeaturelist-fromtype  (typestr / e FeatureList outlist) ;"J"
	(if (= nil gl_FeaturesList)
		(setq gl_FeaturesList (ReadTypedFeatureList nil))
		)
	(setq outlist nil)
	(setq FeatureList (cdr (assoc typestr gl_FeaturesList))
		FeatureList (reverse FeatureList))
	(foreach e FeatureList
		(setq outlist (cons (cadr e) outlist))
	)
	outlist
) ;defun

;;;�����ַ�����ȡ������
(defun GetTypeFromPointName (strName / typename)
    (if	(null gl_SubList)
	(setq gl_SubList (ReadSubsymbolConfig nil))
    ) ;if
    ;;ʹ���������ж�����
    (setq strName  (vl-string-trim "0123456789-_+." strName) ;ȥ���������
	  typename (cadr (assoc strName gl_SubList))
    ) ;_ end_setq
    ;;ʹ�õ�һ���ַ��ж�����
    (if	(= nil typename)
	(setq typename (cadr (assoc (substr strName 1 1) gl_SubList)))
    ) ;_ end_if
	
	 ;;ʹ�õ�����˵���ж�����
    (if	(= nil typename)
	(setq typename (cadr (assoc2  strName  gl_SubList 2)))
    ) ;_ end_if
	
    (if	(= nil typename)
	(setq typename "B") ;Ĭ�ϲ���
    ) ;_ end_if
    typename
) ;defun

;;���������߸����� ��ȡ���ŵ��ļ���
;;sstr-�����������������,maintype-��������
(defun GetSymbolFileName (featurestr subsidstr maintype / bsubsid bfeature e e1 i scode outstr 
							len FeatureList)
    (if	(= nil gl_SymbolBlockList)
		(setq gl_SymbolBlockList (ReadSymbolConfig nil))
    ) ;_ end_if
	(if	(= nil gl_FeaturesList)
		(setq gl_FeaturesList (ReadTypedFeatureList nil))
    ) ;_ end_if
	(setq FeatureList (cdr (assoc maintype gl_FeaturesList)))

    (setq bfeature   nil
	  bsubsid    nil
	  outstr "Sym1.dwg"
	  i	     0
	  len	     (length gl_SymbolBlockList)
    ) ;_ end_setq
    (repeat len
		(setq e	 (nth i gl_SymbolBlockList)
			  e1 (strcase (car e))
		) ;_ end_setq
		(if (= maintype (substr e1 1 1))
			(if	(= subsidstr (caddr e))
				(setq outstr (cadr e)
					  bsubsid T
				) ;_ end_setq
			) ;_ end_if
		)
		(setq i (1+ i))
    ) ;_ end_repeat
	(if (not bsubsid)
		(foreach e FeatureList
			(if (= featurestr (cadr e))
				(setq outstr (car e)
				) ;_ end_setq
			) ;if
		)
	)
    outstr
) ;_ end_defun

;;�汾���£�3.3->3.4
;;�����п���߶ε�ldata���һ��gl_App����
(defun OneAllLdata( / AllPointSet AllLineSet i ent count)
	(setq AllPointSet (ssget "X" (list (cons 0 "INSERT") (list -3 (list gl_AppName))))
		AllLineSet (ssget "X" (list (cons 0 "LINE") (list -3 (list gl_AppName))))
		i 0
		count 0
		)
	(repeat (sslength AllLineSet)
		(setq ent (ssname AllLineSet i ))
		(if (setq listdata (vlax-ldata-list ent))
			(progn
				(foreach e listdata
					(vlax-ldata-delete ent (car e))
				)
				(setq listdata (subst (cons "Version" gl_Version) (assoc gl_AppName listdata) listdata))
				(vlax-ldata-put ent gl_AppName listdata)
			)
		)
		(setq i (1+ i))
	)	
	(setq count i 
		i 0)
	(repeat (sslength AllPointSet)
		(setq ent (ssname AllPointSet i ))
		(if (setq listdata (vlax-ldata-list ent))
			(progn
				(foreach e listdata
					(vlax-ldata-delete ent (car e))
				)
				(setq listdata (subst (cons "Version" gl_Version) (assoc gl_AppName listdata) listdata))
				(vlax-ldata-put ent gl_AppName listdata)
			)
		)
		(setq i (1+ i))
	)
	(setq count (+ count i))
)

;;*********************************************************************************************
 ;��������:UpdateVerion()
 ;���ܣ����¾ɰ汾��ͼ���°汾,����gl_AppName ���޸�;
 ;������
 ;���أ�
 ;����ʱ�䣺2014/12/01   15:40
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun C:UpdateVersion  (/ allset ename i j ldata oldappname typename mapname maintype ldata1 
							ldata2 depth mdate)
    (print "���ڸ���ͼ�ΰ汾...")
    (setq AllSet     (ssget "X")
	  oldAppName "INFO_LINE0.0.1"
	  i	     0
	  j	     0
    ) ;_ end_setq



    (repeat (sslength AllSet)
	(setq ename    (ssname AllSet i)
	      typename (cdr (assoc 0 (entget ename)))
	      ldata1   (vlax-ldata-get ename oldAppName)
	      ldata2   (vlax-ldata-get ename gl_AppName)
	      ldata    nil
	      version  (ldata-get ename "Version")
	) ;_ end_setq 
	(if (or ldata1 ldata2)
		    ;;����AppID ����ssget
		    (setxdata ename gl_AppName (list (cons 1000 gl_Version)))
		) ;_ end_if
	(if (/= gl_Version Version)
	    (progn
		
			;;0.01��
			(if (listp ldata1)
				(if	(> (length ldata1) 0)
					(setq ldata ldata1)
				) ;_ end_if
			) ;_ end_if
			;;1.0-3.2
			(if (and (listp ldata2) (/= version nil))
				(if	(> (length ldata2) 0)
					(setq ldata ldata2)
				) ;_ end_if
				;;else new version
			) ;_ end_if
			;;�޸�MdataΪSur_Date
			(if (= version "3.4.0")
				(progn 
					(setq ldata nil)
					(ldata-put ename "Version" gl_Version)
					(setq tname (cdr (assoc 0 (entget ename))))
					(cond
						((= tname "INSERT") 
							(progn
								(ldata-delete ename "Sur_Date")
								(ldata-delete ename "Mdate")
							))
						((= tname "LINE") 
							(progn
								(ldata-put ename "Sur_Date" "")
								(if (setq mdate (ldata-get ename "Mdate"))
									(progn
										(ldata-put ename "Sur_Date" mdate)
										(ldata-put ename "Mdate" "")
									)
								)
							))	
					)
					(ldata-delete ename gl_AppName)
					
					(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
	      		(if (= version "4.0.0")
			  (setq ldata nil)
			  )
			(if (= ldata2 "3.3.0")
				(progn 
					(setq ldata nil)
					(if (setq listdata (vlax-ldata-list ename))
						(progn
						(foreach e listdata
							(vlax-ldata-delete ename (car e))
						) ;_ end_foreach
						(setq listdata (subst (cons "Version" gl_Version)
									  (assoc gl_AppName listdata)
									  listdata
								   ) ;_ end_subst
						) ;_ end_setq
						(vlax-ldata-put ename gl_AppName listdata)
						) ;_ end_progn
					) ;_ end_if
					(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
			(if (= ldata2 "3.0.0")
				;;3.2.0�������������ֶ�
				(progn
				(if (= "LINE" typename)
					(vlax-ldata-put ename "Road_Name" "")
				) ;_ end_if
				(if (= "INSERT" typename)
					(vlax-ldata-put ename "Point_Size" "")
				) ;_ end_if
				(vlax-ldata-put ename gl_AppName "3.3.0")

				(setq ldata nil)
				(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
			(if ldata
				;;0.0.1�� 2.0��
				(progn
				(vlax-ldata-delete ename oldAppName)
				(vlax-ldata-put ename gl_AppName ldata)
				(setq maintype (substr (car ldata) 1 1))
				;;
				(if (= "LINE" typename)
					(progn
					;;ͨ������
					(InitLineAttribs ename)
					(if (setq data (nth 1 ldata))
						(vlax-ldata-put ename "Start_Point" data) ;�����
					) ;_ end_if
					(if (setq data (nth 2 ldata))
						(vlax-ldata-put ename "End_Point" data) ;�յ���
					) ;_ end_if
					(if (setq data (nth 5 ldata))
						(vlax-ldata-put ename "Material" data) ;����
					) ;_ end_if

					(vlax-ldata-put ename "Main_Type" maintype) ;������,13��,�����ַ�
					;;(vlax-ldata-put ename "Sub_Type" "")		;������,1-3���ַ�
					(if (setq data (nth 20 ldata))
						(vlax-ldata-put ename "Use_Status" data) ;0-����;1-ʹ��;2-ʩ����
					) ;_ end_if

					;;����
					(if (setq data (nth 7 ldata))
						(vlax-ldata-put ename "D_S" data) ;�ܾ���ߴ�DN100Բ��,����100*100���� mm
					) ;_ end_if
					;;������:�ǵ��ŵ����ܵ�
					(if (setq data (nth 15 ldata))
						(progn (vlax-ldata-put ename "Cab_Count" data) ;������:���������								
						   (vlax-ldata-put ename "Hole_Count" data) ;����:���������
						) ;_ end_progn
					) ;_ end_if
					(if (setq data (nth 18 ldata))
						(vlax-ldata-put ename "Hole_Used" data) ;ʹ�ÿ���:���������
					) ;_ end_if
					(if (setq data (nth 17 ldata))
						(vlax-ldata-put ename "Pressure" data) ;ѹ��:
					) ;_ end_if
					(if (setq data (nth 16 ldata))
						(vlax-ldata-put ename "Voltage" data) ;��ѹ;kV V
					) ;_ end_if
					(if (setq data (nth 25 ldata))
						(vlax-ldata-put ename "Flowdirect" data) ;����:ʹ������ˮ�͸�ˮ,����ʯ��ȼ����ˮ
					) ;_ end_if
					;;���� 0 start-end ,1 end-start-end

					;;���
					(vlax-ldata-put ename "Text_Pos" (list 0 0 0 0)) ;��ŵı��λ��,4������(x y z angle)
					(vlax-ldata-put ename "M_Text" "") ;�����������;

					;;project info
					(if (setq data (nth 24 ldata))
						(vlax-ldata-put ename "Sur_Date" data) ;����ʱ��
					) ;_ end_if
					(if (setq data (nth 22 ldata))
						(vlax-ldata-put ename "SProject" data) ;����Ŀ����
					) ;_ end_if
					(if (setq data (nth 23 ldata))
						(vlax-ldata-put ename "Project" data) ;����Ŀ����
					) ;_ end_if
					(if (setq data (nth 19 ldata))
						(vlax-ldata-put ename "Location" data) ;����λ��
					) ;_ end_if
					(if (setq data (nth 21 ldata))
						(vlax-ldata-put ename "Unit" data) ;���鵥λ
					) ;_ end_if
					) ;_ end_progn
				) ;_ end_if

				(if (= "INSERT" typename)
					(progn
					(setq depth (vlax-ldata-get ename "DEPTH"))
					(InitPointAttribs ename)
					(setq mapname (GetLPointMapNo ename))
					(vlax-ldata-put ename "Map_No" (car mapname)) ;��̽���
					(vlax-ldata-put ename "Exp_No" (car mapname)) ;��̽���
					;;Depth

					(if depth
						(if	(listp depth)
						(vlax-ldata-put ename "Depth" (car depth))
						(vlax-ldata-put ename "Depth" depth)
						) ;_ end_if
					) ;���

					(if (setq data (nth 7 ldata))
						(vlax-ldata-put ename "Feature" data) ;������
					) ;_ end_if
					(if (setq data (nth 8 ldata))
						(vlax-ldata-put ename "Subsid" data) ;������
					) ;_ end_if

					(vlax-ldata-put ename "Main_Type" maintype) ;������,13��,�����ַ�
					;;(vlax-ldata-put ename "Sub_Type" "")		;������,1-3���ַ�
					(vlax-ldata-put ename "Use_Status" 1) ;0-����;1-ʹ��;2-ʩ����

					(vlax-ldata-put ename "Text_Pos" (list 0 0 0 0)) ;��ŵı��λ��,4������(x y z angle)
					(vlax-ldata-put ename "Mark_Angle" 0) ;ͼ��ı�׼�Ƕ�

					;;project info
					(if (setq data (nth 11 ldata))
						(vlax-ldata-put ename "Sur_Date" data) ;����ʱ��
					) ;_ end_if
					(if (setq data (nth 12 ldata))
						(vlax-ldata-put ename "SProject" data) ;����Ŀ����
					) ;_ end_if
					(if (setq data (nth 13 ldata))
						(vlax-ldata-put ename "Project" data) ;����Ŀ����
					) ;_ end_if
					(vlax-ldata-put ename "Location" "") ;����λ��
					(if (setq data (nth 9 ldata))
						(vlax-ldata-put ename "Unit" data) ;���鵥λ
					) ;_ end_if
					) ;_ end_progn
				) ;_ end_if
				(vlax-ldata-put ename gl_AppName "3.3.0")
				(setq j (1+ j))
				) ;_ end_progn
			) ;_ end_if
	    ) ;_ end_progn
	)    
	    (setq i (1+ i))
	) ;_ end_repeat
	;;2015-3-9���£�����ldata����һ������
	;;���ϲ�����µ�3.3.0,������µ�3.4
	;;(setq ldata2 (vlax-ldata-get gl_AppName))
;;;	(if (= ldata2 "3.3.0")
;;;		(setq j (OneAllLdata))
;;;	)

	(prompt	(strcat	"\n�޸���"
			(rtos j 2 0)
			"������ļ�����.\n��ǰ�汾Ϊ:"
			gl_AppName
			"-"
			gl_Version
			"."
		) ;_ end_strcat
	) ;_ end_prompt
    (princ)
) ;_ end_defun



 ;*********************************************************************************************
 ;��������:ReadProjectInfo (path)
 ;���ܣ���ȡ��Ŀ��Ϣ
 ;�����������ļ�·��
 ;���أ�(("Project"."��Ŀ����") ()...)
 ;����ʱ�䣺2014/12/19   13:40
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
(defun ReadProjectInfo (path / file info pos projectinfolist str typename)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ End_if
    (setq ProjectInfoList nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (setq str (read-line file))
		(if (= str "[ProjectInfo]")
		    (while (/= "[/ProjectInfo]" (setq str (read-line file)))
				(if (/= "//" (substr str 1 2));;ע����
					(setq pos	      (vl-string-search "," str)
						  typename	      (substr str 1 pos)
						  info	      (substr str (+ 2 pos))
						  ProjectInfoList (cons
								  (cons typename info)
								  ProjectInfoList
								  ) ;_ End_append
					) ;setq
				)
		    ) ;while
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if

    ProjectInfoList
) ;_ end_defun
;;����Ŀ��Ϣд�������ļ�
;;����:��Ŀ��Ϣ��Ա�
(defun WriteProjectInfo	(projectinfolist path / tmpfile tmpfilestr e file newdata olddata oldinfolist str strname)
    (if	(= nil path)
	(setq path	 (strcat gl_BLOCKLIB_PATH "config.ini")
	      tmpfilestr (strcat gl_BLOCKLIB_PATH "lineinfotmp.dat")
	) ;_ end_setq
    ) ;_ End_if
    (setq oldInfoList (ReadProjectInfo path))
    (vl-file-delete tmpfilestr)

    (if	(and (/= nil projectinfolist) (setq file (open path "r")) (setq tmpfile (open tmpfilestr "w")))
	(progn
	    (while (setq str (read-line file))
		(write-line str tmpfile)
		(if (= str "[ProjectInfo]")
		    (progn
			(foreach e oldInfoList
			    (setq strname (car e)
				  olddata (cdr e)
				  newdata (cdr (assoc strname projectinfolist))
			    ) ;_ end_setq
			    (if	(= olddata newdata)
				(write-line (strcat strname "," olddata) tmpfile)
				(write-line (strcat strname "," newdata) tmpfile)
			    ) ;_ end_if
			) ;_ end_foreach
			(while (/= "[/ProjectInfo]" (setq str (read-line file)))
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

;;����ͼ�α�����
;;��ʾ��ǰ�����ߣ����޸ı����ߣ����޸����п����ĳ߶�
(defun C:SetNewScale (/ scale)
    (setq scale (Xrecord-Get gl_AppName "SCALE"))
    (if	scale
	(setq gl_MAP_SCALE (atof (car scale)))
    ) ;_ end_if

    (prompt (strcat "\n��ǰ������Ϊ1:" (rtos (/ 1000.0 gl_MAP_SCALE) 2 0) ".\n"))
    (setq scale (Fun_InPutValue 1000 "USERR2" "\n�����µ�ͼ�α�����(1:1000����1000��1:500����500��)��" 2))
    (if	(= 'REAL (type scale))
	(progn
	    (setq gl_MAP_SCALE (/ 1000.0 scale))
	    (Xrecord-Rebuild gl_AppName "SCALE" (rtos gl_MAP_SCALE 2 4))
	    (SetBlockScale (/ 1.0 gl_MAP_SCALE))
	) ;progn
    ) ;_ end_if
) ;_ end_defun
;;���ù��ߵ�ͼ�����
(defun SetBlockScale (newscale / AllPointBlockSet i j ent obj)
    (setq AllPointBlockSet (ssget "X" (list (cons 0 "INSERT")))
	  i		   0
	  j		   0
    ) ;_ end_setq
    (if	AllPointBlockSet
	(repeat	(sslength AllPointBlockSet)
	    (setq ent (ssname AllPointBlockSet i)
		  obj (vlax-ename->vla-object ent)
	    ) ;_ e_setq
	    (if	(vlax-ldata-get obj gl_AppName)
		(progn
		    ;;�޸ĵ������
;;;                    (if (= :vlax-true (vla-get-HasAttributes obj))
;;;                        (progn
;;;                            (setq attrib_objs (vlax-safearray->list
;;;                                                  (vlax-variant-value (vla-GetAttributes obj))
;;;                                              ) ;_ e_vlax-safearray->list
;;;                            ) ;_ e_setq
;;;                            (if (> (length attrib_objs) 0)
;;;                                (progn
;;;                                    (setq attrib_obj (car attrib_objs))
;;;;;;						(vla-put-TextString attrib_obj strP);����
;;;                                    (vla-put-Height attrib_obj (/ 2 newscale)) ;�ָ�
;;;				  (vla-put-ScaleFactor attrib_obj 0.8)
;;;;;;				                (if ts (vla-put-stylename attrib_obj (vla-get-name ts)))
;;;                                ) ;progn
;;;                            ) ;if
;;;                        ) ;progn
;;;                    ) ;if
		    (vla-put-XScaleFactor obj newscale)
		    (vla-put-YScaleFactor obj newscale)
		    (vla-put-ZScaleFactor obj newscale)
		    (setq j (1+ j))
		) ;progn
	    ) ;if
	    (setq i (1+ i))
	) ;repeat
    ) ;if
    (prompt (strcat "\n�޸���" (rtos j 2 0) "��ͼ��ı�����."))
) ;defun

;;��ȡͼԪ��Ϣ
;;��ʾͼԪ����
(defun C:LineInfo_GetObjInfo (/ e ldatas typename attribstr)
    (setq dbstr	nil
	  mstr nil
    ) ;_ End_setq
    (if	(setq e (car (entsel "\nѡ��ͼԪ��")))
	(if (setq ldatas(vlax-ldata-get e gl_AppName))
	    (progn
		(setq typename	(cdr (assoc 0 (entget e)))
		      attribstr	"\n��������:"
		) ;_ end_setq
		(if	(and gl_DEBUG e)
			(princ (vlax-ldata-list e))
		) ;_ end_if
		(if (= "INSERT" typename)
		    (progn
				(if (not gl_PointFieldList)
					(setq gl_PointFieldList (ReadPointFieldsInfo nil))
				) ;_ end_if
				
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Map_No" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Map_No" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Exp_No" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Exp_No" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Depth" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Depth" ldatas))
					(setq attribstr (strcat attribstr (rtos (cdr data) 2 4)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Point_Size" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Point_Size" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Feature" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Feature" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Subsid" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Subsid" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Project" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Project" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "SProject" gl_PointFieldList)) ":"))
				(if (setq data (assoc "SProject" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Unit" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Unit" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Sur_Date" gl_PointFieldList)) ":"))
				(if (setq data (assoc "Sur_Date" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
		    ) ;_ end_progn
		) ;_ end_if

		(if (= "LINE" typename)
		    (progn
				(if (not gl_LineFieldList)
					(setq gl_LineFieldList (ReadLineFieldsInfo nil))
				) ;_ end_if
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Start_Point" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Start_Point" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "End_Point" gl_LineFieldList)) ":"))
				(if (setq data (assoc "End_Point" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "D_S" gl_LineFieldList)) ":"))
				(if (setq data (assoc "D_S" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Material" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Material" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Pressure" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Pressure" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Voltage" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Voltage" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Cab_Count" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Cab_Count" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Hole_Count" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Hole_Count" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Hole_Used" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Hole_Used" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Flowdirect" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Flowdirect" ldatas))
					(setq attribstr (strcat attribstr (rtos (cdr data) 2 0)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Road_Name" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Road_Name" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Project" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Project" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "SProject" gl_LineFieldList)) ":"))
				(if (setq data (assoc "SProject" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Unit" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Unit" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
				(setq attribstr	(strcat attribstr "\n" (cdr (assoc "Sur_Date" gl_LineFieldList)) ":"))
				(if (setq data (assoc "Sur_Date" ldatas))
					(setq attribstr (strcat attribstr (cdr data)))
				)
		    ) ;_ end_progn
		) ;_ end_if
		(princ attribstr)
	    ) ;_ end_progn
	    (prompt "\nͼԪ��������������Ϣ��")
	) ;_ end_if
    ) ;if
    
	(textpage)
    (princ)
) ;defun

;;��ȡ����ֶ���Ϣ
;;����:��Ա� ((Map_No.ͼ��)...)
(defun ReadPointFieldsInfo (path / fieldslist file str pos name1 name2 bEnd)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq fieldslist nil
		bEnd nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (and (not bEnd)(setq str (read-line file)))
		(if (= str "[Point_Attributes]")
			(progn
				(while (/= "[/Point_Attributes]" (setq str (read-line file)))
					(if (/= "//" (substr str 1 2));;ע����
						(setq pos	 (vl-string-search "," str)
							  name1	 (substr str 1 pos) ;first letter is the field name;
							  str	 (substr str (+ 2 pos))
							  pos	 (vl-string-search "," str)
							  name2	 (substr str 1 pos) ;field name (Chinese)

							  fieldslist (cons (cons name1 name2) fieldslist)
						) ;_ end_setq
					)
				) ;while
				(setq bEnd T)
			)
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if
    fieldslist
) ;_ end_defun

;;��ȡ�߶ε��ֶ���Ϣ
;;����:��Ա� ((Start_Point.�����)...)
(defun ReadLineFieldsInfo (path / fieldslist file str pos name1 name2 bEnd)
    (if	(= nil path)
	(setq path (strcat gl_BLOCKLIB_PATH "config.ini"))
    ) ;_ end_if
    (setq fieldslist nil
		bEnd nil)
    (if	(setq file (open path "r"))
	(progn
	    (while (and (not bEnd)(setq str (read-line file)))
		(if (= str "[Line_Attributes]")
			(progn
				(while (/= "[/Line_Attributes]" (setq str (read-line file)))
					(if (/= "//" (substr str 1 2));;ע����
						(setq pos	 (vl-string-search "," str)
							  name1	 (substr str 1 pos) ;first letter is the field name;
							  str	 (substr str (+ 2 pos))
							  pos	 (vl-string-search "," str)
							  name2	 (substr str 1 pos) ;field name (Chinese)

							  fieldslist (cons (cons name1 name2) fieldslist)
						) ;_ end_setq
					)
				) ;while
				(setq bEnd T)
			)
		) ;if
	    ) ;while
	    (close file)
	) ;progn
	;;else
	(prompt (strcat "\nError�����ļ�ʧ�ܣ�" path))
    ) ;if
    fieldslist
) ;_ end_defun
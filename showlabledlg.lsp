;*********************************************************************************************
;函数定义: C:ShowlabelDlg()
;功能：设置管线段标注设置:是否显示管线标柱
;参数：
;返回：
;创建时间：2015/17/01   13:14
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:ShowlabelDlg( /  e id newe std tilelist tilevalue AllLineList i entl typename shiftlist linelist)
	(defun SLD_getdata( / outdata)
		(setq outdata nil
			outdata (cons (cons "J" (get_tile "J" )) outdata)
			outdata (cons (cons "Y" (get_tile "Y" )) outdata)
			outdata (cons (cons "P" (get_tile "P" )) outdata)
			outdata (cons (cons "W" (get_tile "W" )) outdata)
			outdata (cons (cons "D" (get_tile "D" )) outdata)
			outdata (cons (cons "L" (get_tile "L" )) outdata)
			outdata (cons (cons "R" (get_tile "R" )) outdata)
			outdata (cons (cons "Q" (get_tile "Q" )) outdata)
			outdata (cons (cons "X" (get_tile "X" )) outdata)
			outdata (cons (cons "H" (get_tile "H" )) outdata)
			outdata (cons (cons "G" (get_tile "G" )) outdata)
			outdata (cons (cons "F" (get_tile "F" )) outdata)
			outdata (cons (cons "Z" (get_tile "Z" )) outdata)
		)
		outdata
	)

	;;-------------------------start dlg------------------------------------
	(if (= nil gl_Type_Label_List)
			(setq gl_Type_Label_List (ReadTypeLabelList nil))
	) ;_ end_if
	(if (= gl_TableColorList nil)
		(setq gl_TableColorList (ReadColorConfig nil))
	) ;_ end_if
		
	(setq id (load_dialog (strcat gl_INFO_LINE_PATH "dlg\\showlabel.dcl")))
    
    (if (< id 0)
        (exit)
    ) ;_ End_if
	
	(if (not (new_dialog "showlabeldlg" id ))
		(exit)
	) 
	
	;;设置控件的值:tilename与类型名称相同
	(foreach e gl_Type_Label_List
		(set_tile (car e) (cadr e))
	)
	(setq tilelist nil)
 
	(action_tile "cancel" "(done_dialog 0)")
	(action_tile "accept" "(setq tilelist (SLD_getdata))(done_dialog 1)")

	(setq std (start_dialog))

	(setq shiftlist nil);;变化的类别
	(if (= std 1) ;ok
		(if tilelist
			(progn
				(foreach e gl_Type_Label_List
					(setq 
						typename (car e)
						tilevalue (cdr (assoc typename tilelist))
					)
					(if (/= tilevalue (cadr e))
						(setq shiftlist (cons (assoc typename tilelist) shiftlist)
							newe (subst tilevalue (cadr e) e)
							gl_Type_Label_List (subst newe e gl_Type_Label_List)
						)
					)
				)
				
				(if shiftlist
				    (progn
						(WriteTypeLabelList gl_Type_Label_List nil)
						(if(setq AllLineList (GetTypedLineList))
							(foreach e shiftlist
								(if (setq linelist (cadr (assoc (car e) AllLineList)))
									(foreach entl linelist
										(UpdateLabel entl (ldata-get entl "Start_Deep") (ldata-get entl "End_Deep"))
									)
								)
							)
						)
					)
				)
			)
	    )
	)
	(princ)
)
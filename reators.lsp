;;;ʹ�÷�Ӧ����ʹͼ�κ����ݿ���ȫͬ����
;;;�����ʵ䣬���汻���Ĺ���ͼ�ζ�������
;;;ȫ�ַ�Ӧ�� glr_ǰ׺
;;;�ص�����ǰ׺:lir:
;;;��ͼ���н���һ�����÷�Ӧ����Openfile�����س���

(vl-load-reactors)

;;*ModifyDic*  ȫ�ִʵ�,�����޸Ĺ��Ķ�����,״̬
(defun GetModifyDic ( / app Doc dicts )
	(if (not *ModifyDic*)
		(setq app	 (vlax-get-acad-object)
			  Doc	 (vla-get-Activedocument app)
			  dicts  (vla-get-Dictionaries doc)
			*ModifyDic* (vla-add dicts "LI-ModifyDic")
		)
	) ;_ end_if
	 *ModifyDic*
)

;;when entity added erased or modified ,add entity to MoidifyDic;
;;e-ename or entity object
;;status-"DELETE" "ADD" "MODIFY"
(defun AddtoModifyDic (e status / obj prestatus)
    (setq obj nil 
		prestatus nil)
	(if e
        (cond
            ((= "ENAME" (type e))
             (setq obj (vlax-ename->vla-object e))
            )
            ((= "VLA-OBJECT" (type e))
             (setq obj e)
            )
            (T (setq obj nil))
        ) ;_ end_cond
    ) ;_ end_if
    (if obj
        (progn
	        (setq handle (vla-get-handle obj)
				prestatus (Xrecord-Get (GetModifyDic) handle))
            (cond
				((= nil prestatus)
					(Xrecord-Add (GetModifyDic) handle status))
				((and (= prestatus "ADD")(= status "DELETE"))
					(Xrecord-Delete (GetModifyDic) handle))
				((and (= prestatus "ADD")(= status "MODIFY"))
					(Xrecord-Rebuild (GetModifyDic) handle prestatus))	
				((and (= prestatus "DELETE")(= status "ADD"))
					(Xrecord-Rebuild (GetModifyDic) handle "MODIFY"))
				(T
					(Xrecord-Rebuild (GetModifyDic) handle status))	
            )
        )
    )
) ;_ end_defun

;;Create reactors entity and reactor
;;object reactor
;;how and when  ???  when the file is opened then link it;
;;if the entities mount to 5000,time would be a problem
(defun Link-ojbect-reactor (obj)
	(cond (not obj)
		(princ "\n Link-ojbect-reactor parameter obj is nil." )
	)
	(if obj
		(progn
			;;modified
			(if glr-obj-modified
				(vlr-owner-add glr-obj-modified obj)
				(setq glr-obj-modified (vlr-object-reactor (list obj) "MODIFY" '((:VLR-objectModified . fr-obj-changed))))
			)
			;;erased
			(if glr-obj-erased
				(vlr-owner-add glr-obj-erased obj)
				(setq glr-obj-erased (vlr-object-reactor (list obj) "DELETE" '((:VLR-objectErased . fr-obj-changed))))
			)
			;;unerased
			(if glr-obj-unerased
				(vlr-owner-add glr-obj-unerased obj)
				(setq glr-obj-unerased (vlr-object-reactor (list obj) "MODIFY" '((:VLR-objectUnErased . fr-obj-changed))))
			)
			;;no "ADD" reactor,so add obj in AddEntity 
		)
	)
)
;;callback function:obj changed
(defun fr-obj-changed(obj-sender reactor data)
	 (AddtoModifyDic obj-sender data)
)


 (defun report-dictionary (dic /)
  (vlax-for e dic
    (if  (= "AcDbDictionary" (vla-get-objectname e))
      (progn
  (princ "\n")
  (if (vlax-property-available-p e 'name)
    (princ (strcat (vla-get-name e) ": " (itoa (vla-get-count e))))
    (princ (strcat "AcDbDictionary: " (itoa (vla-get-count e))))
  )
  (report-dictionary e)
      )
    )
  )
)
 
(defun c:tt ()
  (report-dictionary (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))))
  (princ)
)

	
(defun lir-save-lineinfo (reactor cmdinfo / dwgname filesize)
	(setq dwgname (cadr cmdinfo)
        filesize (vl-file-size dwgname)
   )
   (alert (strcat "����ļ���"  dwgname  "���Ĵ�С�ǣ�"
                    (itoa filesize) "�ֽ�."
          )
   )
   (princ
    (list 'lir-save-lineinfo  reactor cmdinfo)
  ) 
   (princ)
)
;;
(defun fr-open-lineinfo (reactor cmdinfo / dwgname filesize)
	(setq dwgname (cadr cmdinfo)
        filesize (vl-file-size dwgname)
   )
   (alert (strcat "���ļ���"  dwgname  "���Ĵ�С�ǣ�"
                    (itoa filesize) "�ֽ�." "\n Reactor is :"
          )
   )
   (princ
    (list 'lir-open-lineinfo  reactor cmdinfo)
	) 
   (princ)
)

(defun lir-close-lineinfo (reactor cmdinfo / dwgname filesize)
    (alert "�ر��ļ�-reactor!"
          )
   (princ
    (list 'lir-close-lineinfo  reactor cmdinfo)
	) 
   (princ)
)

(if (not glr_dwgFileSave)
	(setq glr_dwgFileSave (VLR-DWG-Reactor nil '((:vlr-saveComplete . lir-save-lineinfo)))
		;glr_dwgFileSave (vlr-pers glr_dwgFileSave)
	)
    )

(if (not glr_dwgFileOpen)
	(setq glr_dwgFileOpen (VLR-document-reactor nil '((:VLR-documentCreated . fr-open-lineinfo)))
		;glr_dwgFileSave (vlr-pers glr_dwgFileSave)
	)
    )
	
; (if (not glr_dwgFileClose)
	; (setq glr_dwgFileClose (VLR-DWG-Reactor nil '((:vlr-beginClose . lir-close-lineinfo)))
		; ;glr_dwgFileSave (vlr-pers glr_dwgFileSave)
	; )
    ; )	


(defun print-reactors-and-events ()
	(foreach rtype (vlr-types)
		(princ (strcat "\n" (vl-princ-to-string rtype)))
		(foreach rname (vlr-reaction-names rtype)
			(princ (strcat "\n\t" (vl-princ-to-string rname)))
		)
	)
	(princ)
)
;;callback function
(defun cb:obj-del(notifier-object reactor-object parameter-list)
	(if (ldata-get notifier-object gl_AppName)
		(ldata-put notifier-object "Status_Modify" 2)
	)
)
;;;--------------------------------------------------------------------;
;;;     Function: CleanReactors                                        ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a general utility function used for cleaning ;
;;;               up reactors. It can be used during debugging, as     ;
;;;               well as cleaning up any open reactors before a       ;
;;;               drawing is closed.                                   ;
;;;--------------------------------------------------------------------;
(defun CleanReactors ()
  ; (setq	*commandReactor* nil		; clear the variable
	; *DrawingReactor* nil		; clear the variable
	; )
  (mapcar 'vlr-remove-all
	  '(:VLR-AcDb-reactor		 :VLR-Editor-reactor
	    :VLR-Linker-reactor		 :VLR-Object-reactor
	    ;; new reactors
	    ;; New for AutoCAD 2000
	    :VLR-Command-Reactor	 :VLR-DeepClone-Reactor
	    :VLR-DocManager-Reactor	 :VLR-DWG-Reactor
	    :VLR-DXF-Reactor		 :VLR-Editor-reactor
	    :VLR-Insert-Reactor		 :VLR-Linker-Reactor
	    :VLR-Lisp-Reactor		 :VLR-Miscellaneous-Reactor
	    :VLR-Mouse-Reactor		 :VLR-Object-Reactor
	    :VLR-SysVar-Reactor		 :VLR-Toolbar-Reactor
	    :VLR-Undo-Reactor		 :VLR-Wblock-Reactor
	    :VLR-Window-Reactor		 :VLR-XREF-Reactor
	    )
	  ) ;_ end of mapcar
  ) ;_ end of defun
  
; :VLR-Linker-Reactor
	; :VLR-rxAppLoaded
	; :VLR-rxAppUnLoaded
; :VLR-Editor-Reactor
	; :VLR-unknownCommand
	; :VLR-commandWillStart
	; :VLR-commandEnded
	; :VLR-commandCancelled
	; :VLR-commandFailed
	; :VLR-lispWillStart
	; :VLR-lispEnded
	; :VLR-lispCancelled
	; :VLR-beginClose
	; :VLR-beginDxfIn
	; :VLR-abortDxfIn
	; :VLR-dxfInComplete
	; :VLR-beginDxfOut
	; :VLR-abortDxfOut
	; :VLR-dxfOutComplete
	; :VLR-beginDwgOpen
	; :VLR-endDwgOpen
	; :VLR-dwgFileOpened
	; :VLR-databaseConstructed
	; :VLR-databaseToBeDestroyed
	; :VLR-beginSave
	; :VLR-saveComplete
	; :VLR-sysVarWillChange
	; :VLR-sysVarChanged
; :VLR-AcDb-Reactor
	; :VLR-objectAppended
	; :VLR-objectUnAppended
	; :VLR-objectReAppended
	; :VLR-objectOpenedForModify
	; :VLR-objectModified
	; :VLR-objectErased
	; :VLR-objectUnErased
; :VLR-DocManager-Reactor
	; :VLR-documentCreated
	; :VLR-documentToBeDestroyed
	; :VLR-documentLockModeWillChange
	; :VLR-documentLockModeChangeVetoed
	; :VLR-documentLockModeChanged
	; :VLR-documentBecameCurrent
	; :VLR-documentToBeActivated
	; :VLR-documentToBeDeactivated
; :VLR-Command-Reactor
	; :VLR-unknownCommand
	; :VLR-commandWillStart
	; :VLR-commandEnded
	; :VLR-commandCancelled
	; :VLR-commandFailed
; :VLR-Lisp-Reactor
	; :VLR-lispWillStart
	; :VLR-lispEnded
	; :VLR-lispCancelled
; :VLR-DXF-Reactor
	; :VLR-beginDxfIn
	; :VLR-abortDxfIn
	; :VLR-dxfInComplete
	; :VLR-beginDxfOut
	; :VLR-abortDxfOut
	; :VLR-dxfOutComplete
; :VLR-DWG-Reactor
	; :VLR-beginDwgOpen
	; :VLR-endDwgOpen
	; :VLR-dwgFileOpened
	; :VLR-databaseConstructed
	; :VLR-databaseToBeDestroyed
	; :VLR-beginSave
	; :VLR-saveComplete
	; :VLR-beginClose
; :VLR-Insert-Reactor
	; :VLR-beginInsert
	; :VLR-beginInsertM
	; :VLR-otherInsert
	; :VLR-abortInsert
	; :VLR-endInsert
; :VLR-Wblock-Reactor
	; :VLR-wblockNotice
	; :VLR-beginWblockPt
	; :VLR-beginWblockId
	; :VLR-beginWblock
	; :VLR-otherWblock
	; :VLR-abortWblock
	; :VLR-endWblock
	; :VLR-beginWblockObjects
; :VLR-SysVar-Reactor
	; :VLR-sysVarWillChange
	; :VLR-sysVarChanged
; :VLR-DeepClone-Reactor
	; :VLR-beginDeepClone
	; :VLR-beginDeepCloneXlation
	; :VLR-abortDeepClone
	; :VLR-endDeepClone
; :VLR-XREF-Reactor
	; :VLR-beginAttach
	; :VLR-otherAttach
	; :VLR-abortAttach
	; :VLR-endAttach
	; :VLR-redirected
	; :VLR-comandeered
	; :VLR-beginRestore
	; :VLR-abortRestore
	; :VLR-endRestore
	; :VLR-xrefSubcommandBindItem
	; :VLR-xrefSubcommandAttachItem
	; :VLR-xrefSubcommandOverlayItem
	; :VLR-xrefSubcommandDetachItem
	; :VLR-xrefSubcommandPathItem
	; :VLR-xrefSubcommandReloadItem
	; :VLR-xrefSubcommandUnloadItem
; :VLR-Undo-Reactor
	; :VLR-undoSubcommandAuto
	; :VLR-undoSubcommandControl
	; :VLR-undoSubcommandBegin
	; :VLR-undoSubcommandEnd
	; :VLR-undoSubcommandMark
	; :VLR-undoSubcommandBack
	; :VLR-undoSubcommandNumber
; :VLR-Window-Reactor
	; :VLR-docFrameMovedOrResized
	; :VLR-mainFrameMovedOrResized
; :VLR-Toolbar-Reactor
	; :VLR-toolbarBitmapSizeWillChange
	; :VLR-toolbarBitmapSizeChanged
; :VLR-Mouse-Reactor
	; :VLR-beginDoubleClick
	; :VLR-beginRightClick
; :VLR-Miscellaneous-Reactor
	; :VLR-pickfirstModified
	; :VLR-layoutSwitched
; :VLR-Object-Reactor
	; :VLR-cancelled
	; :VLR-copied
	; :VLR-erased
	; :VLR-unerased
	; :VLR-goodbye
	; :VLR-openedForModify
	; :VLR-modified
	; :VLR-subObjModified
	; :VLR-modifyUndone
	; :VLR-modifiedXData
	; :VLR-unappended
	; :VLR-reappended
	; :VLR-objectClosed
	
;(vlr-reactors :vlr-object-reactor)
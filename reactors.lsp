(defun print-reactors-and-events ()
	(foreach rtype (vlr-types)
		(princ (strcat "\n" (vl-princ-to-string rtype)))
		(foreach rname (vlr-reaction-names rtype)
			(princ (strcat "\n\t" (vl-princ-to-string rname)))
		)
	)
	(princ)
)

(defun DelReactorAll (enObj)
  (foreach a (vlr-reactors :VLR-Object-Reactor) ;_ 所有对象反应器
    (foreach b (cdr a) ;_ 各类反应器对象
      (vl-some (function
		 (lambda (c)
		   (if (equal c enobj)
		     (vlr-remove b)
		   )
		 )
	       )
	       (vlr-owners b) ;_ 反应器拥有者对象
      )
    )
  )
)


;;测试
;(DelReactorAll (vlax-ename->vla-object (car(entsel "\n选择对象:"))))

;;测试


;;test acdb reators
(vl-load-com)
(setq gl_dbReactor1 (vlr-acdb-reactor "LINEINFO-Append"  '((:vlr-objectAppended . cb-DbAppend))))
(setq gl_dbReactor2 (vlr-acdb-reactor "LINEINFO-UnAppend"  '((:vlr-objectUnAppended  . cb-DbUnAppend))))
(setq gl_dbReactor3 (vlr-acdb-reactor "LINEINFO-ReAppend"  '((:vlr-objectReAppended  . cb-DbReAppend))))
(setq gl_dbReactor4 (vlr-acdb-reactor "LINEINFO-Modified"  '((:vlr-objectModified  . cb-DbModified))))
(setq gl_dbReactor5 (vlr-acdb-reactor "LINEINFO-Erased"  '((:vlr-objectErased  . cb-DbErased))))
(setq gl_dbReactor6 (vlr-acdb-reactor "LINEINFO-UnErased"  '((:vlr-objectUnErased  . cb-DbUnErased))))
(defun cb-DbAppend (actor data / obj)
	(print "Append")
	(print data)
	(setq obj (last data))
	(if (vlax-ldata-get obj gl_AppName)
		(progn
			(princ gl_AppName)
		)
	)
	(princ)
)

(defun cb-DbUnAppend (actor data / obj)
	(print "UnAppend")
	(print data)
	(setq obj (last data))
	(if (vlax-ldata-get obj gl_AppName)
		(progn
			(princ gl_AppName)
		)
	)
	(princ)
)

(defun cb-DbReAppend (actor data / obj)
	(print "ReAppend")
	(print data)
	(setq obj (last data))
	(if (vlax-ldata-get obj gl_AppName)
		(progn
			(princ gl_AppName)
		)
	)
	(princ)
)

(defun cb-DbModified (actor data / obj)
	(print "Modified")
	(print data)
	(setq obj (last data))
	(if (vlax-ldata-get obj gl_AppName)
		(progn
			(princ gl_AppName)
		)
	)
	(princ)
)

(defun cb-DbErased (actor data / obj)
	(print "Erased")
	(print data)
	(setq obj (last data))
	(if (vlax-ldata-get obj gl_AppName)
		(progn
			(princ gl_AppName)
		)
	)
	(princ)
)

(defun cb-DbUnErased (actor data / obj)
	(print "UnErased")
	(print data)
	(setq obj (last data))
	(if (vlax-ldata-get obj gl_AppName)
		(progn
			(princ gl_AppName)
		)
	)
	(princ)
)


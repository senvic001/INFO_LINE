;;;The Visual LISP Developers Bible
;;;Visual LISP Development with AutoCAD 2002
;;;David M. Stein
;;;May 31, 2002
(vl-load-com)

(defun Xrecord-Rebuild (dict name data)
    (Xrecord-Delete dict name)
    (Xrecord-Add dict name data)
)

(defun Xrecord-Get (dict name / app doc dcs odc xrec #typ #dat out)
    (setq app (vlax-get-acad-object)
	  doc (vla-get-activedocument app)
	  dcs (vla-get-dictionaries doc)
    ) ;_ end_setq
    (cond
	((setq odc (dsx-item dcs dict))
	 (cond
	     ((setq xrec (dsx-item odc name))
	      (vla-getXrecordData xrec '#typ '#dat)
	      (setq #typ (vlax-safearray->list #typ)
		    #dat (vlax-safearray->list #dat)
	      ) ;_ end_setq
	      (setq out	(mapcar
			    'vlax-variant-value
			    #dat
			) ;_ end_mapcar
	      ) ;_ end_setq
	      (vlax-release-object odc)
	     )
	 ) ;_ end_cond
	 (vlax-release-object dcs)
	)
    ) ;_ end_cond
    out
) ;_ end_defun


;;先得到词典，再在词典中操作Xrecord。
(defun Xrecord-Delete (dict name / dcs odc xr)
    (setq dcs (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))))
    (cond
        ((setq odc (dsx-item dcs dict))
            (cond
                ((setq xr (dsx-item odc name))
                    (vla-delete xr)
                    (vlax-release-object xr)
                )
            )
            (vlax-release-object odc)
        )
    )
    (vlax-release-object dcs)
)

(defun Xrecord-Add (dict name data / app doc dicts  xrec #typ #dat)
    (setq app (vlax-get-acad-object)
          doc (vla-get-activedocument app)
          dicts (vla-get-Dictionaries doc)
          dict (vla-add dicts  dict)
          xrec (vla-AddXrecord dict name)
    )
    (if (not (listp data))
        (setq data (list data))
    ); ensure list!
    (vla-setXrecordData xrec (List->VariantArray (List->IntList data)
                                                 'vlax-vbInteger
                             ) (List->VariantArray data 'vlax-vbVariant)
    )
    (vla-getXrecordData xrec '#typ '#dat)
    (setq #typ (vlax-safearray->list #typ)
          #dat (vlax-safearray->list #dat)
    )
    (mapcar
        'vlax-variant-value
        #dat
    )
)

;;;The two functions (List->VariantArray) and (List->IntList) are used to define the safearray contents and dimension respectively.
;;;The second argument to (List->VariantArray) must be a single-quoted ActiveX data type declaration such as ‘vlax-vbString.
(defun List->VariantArray (lst datatype / arraySpace sArray)
    (setq arraySpace (vlax-make-safearray (eval datatype) (cons 0
                                                                (1-
                                                                    (length lst)
                                                                )
                                                          )
                     )
    )
    (setq sArray (vlax-safearray-fill arraySpace lst))
    (vlax-make-variant sArray)
)

(defun List->IntList (lst / n)
;;;    (setq n 0)
    (mapcar
        (function (lambda (x)
                      (setq n (eval 'vlax-vbstring))
                  )
        )
        lst
    )
;;;   (setq outlist nil)
;;;   (foreach e lst
;;;      (cond
;;;	 ((= (type e) 'STR) (setq outlist (append (list (eval 'vlax-vbstring)))))
;;;	 ((= (type e) 'REAL) (setq outlist (append (list (eval 'vlax-vbDouble)))))
;;;	 ((= (type e) 'INT) (setq outlist (append (list (eval 'vlax-vbInteger)))))
;;;	 )
;;;      )
)

;;;Thefunction (dsx-item) is used to fetch (or attempt to fetch) an item using the Item method of a collection.
;;;This function includes error catching in case the fetch fails, which returns an ActiveX error instead of something like nil.
;;;In this case, we trap an error and return nil if the fetch fails. Otherwise, we return the object from the collection.
(defun DSX-Item (collection item / out)
    (if (not (vl-catch-all-error-p (setq out (vl-catch-all-apply 'vla-item
                                                                 (list collection
                                                                       item
                                                                 )
                                             )
                                   )
             )
        )
        out ; return object or nil
    )
)


;*********************************************************************************************
;vlax-ldata操作函数，LineInfo系统中，所有数据放在关键字为gl_appname的表中，每个元素代表一个值
;主要目的是为了提高大数据量下的CAD运行速度
;创建时间：2015/03/09   15:00
;修改时间：
;创建人：沈雄君
;*******************************************************************************************
(defun ldata-put (ename key data / tdata ldata)
	(if (setq ldata (vlax-ldata-get ename gl_AppName))
		(progn
			(if (setq tdata (assoc key ldata))
				(setq ldata (subst (cons key data) tdata  ldata))
				(setq ldata (cons (cons key data) ldata))
			)
			(vlax-ldata-put ename gl_AppName ldata)
		)
		(progn
			(vlax-ldata-put ename key data)
		)
	)
)

(defun ldata-get (ename key  / tdata ldata)
	(if (setq ldata (vlax-ldata-get ename gl_AppName))
		(if (setq tdata (assoc key ldata))
			(setq tdata (cdr tdata))
			(setq tdata nil)
		)
		(setq tdata (vlax-ldata-get ename key))
	)
	tdata
)

(defun ldata-delete (ename key  / tdata ldata)
	(if (setq ldata (vlax-ldata-get ename gl_AppName))
		(if (setq tdata (assoc key ldata))
				(vlax-ldata-put ename gl_AppName (vl-remove  tdata  ldata))
		)
		(vlax-ldata-delete ename key)
	)
	tdata
)
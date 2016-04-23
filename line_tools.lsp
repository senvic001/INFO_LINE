;*********************************************************************************************
;函数定义:C:SearchPoint()
;功能：查找指定点号的图元对象：TEXT or INSERT
;参数：nil
;返回：nil
;创建时间：2014/08/04   20:48
;修改时间：
;创建人：沈雄君
;*********************************************************************************************
(defun C:SearchPoint ( / allpointset alltextset app bfind ent i ldata p0 p1 p2 pstr)
    (prompt "\nSearchPoint查找指定点号的点。")
    (setq pstr        (getstring "\n输入点号：")
          AllPointSet (ssget "X" (list (cons 0 "INSERT")))
          AllTextSet  (ssget "X" (list (cons 0 "TEXT")))
    ) ;_ end_setq
    (if pstr
        (Progn
            (setq bfind nil
                  i 0
            ) ;_ endsetq
            (if AllPointSet
                (while (and (= nil bfind) (< i (sslength AllPointSet)))
                    (setq ent (ssname AllPointSet i)
                          i   (1+ i)
                    ) ;_ end_setq
                    (if (vlax-ldata-get ent gl_AppName)
                        (if (= pstr (ldata-get ent "Map_No"))
                            (setq bfind T)
                        ) ;_ end_if
                    ) ;_ end_if
                ) ;_ end_while
            ) ;_ end_if
            (setq i 0)
            (if AllTextSet
                (while (and (= nil bfind) (< i (sslength AllTextSet)))
                    (setq ent (ssname AllTextSet i)
                          i   (1+ i)
                    ) ;_ end_setq
                    (if (= pstr (cdr (assoc 1 (entget ent))))
                        (setq bfind T)
                    ) ;_ end_if
                ) ;_ end_while
            ) ;_ end_if
            ;;居中
            (if (and ent bfind)
                (progn
                    (setq p0  (cdr (assoc 10 (entget ent)))
                          p1  (list (- (car p0) 10) (- (cadr p0) 10) 0)
                          p2  (list (+ (car p0) 10) (+ (cadr p0) 10) 0)
                          app (vlax-get-acad-object)
                    ) ;_ end_setq
                    (vla-zoomwindow app (vlax-3D-point p1) (vlax-3D-point p2))
                    (prompt (strcat "\n点坐标：(" (rtos  (car p0) 2 4)","(rtos  (cadr p0) 2 4)","(rtos  (caddr p0) 2 4) ")"))
                ) ;_ end_progn
                (prompt (strcat "\n未找到点 " pstr))
            ) ;_ end_if
        ) ;_ endProgn
    ) ;_ endif
) ;_ enddefun

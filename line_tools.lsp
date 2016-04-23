;*********************************************************************************************
;��������:C:SearchPoint()
;���ܣ�����ָ����ŵ�ͼԪ����TEXT or INSERT
;������nil
;���أ�nil
;����ʱ�䣺2014/08/04   20:48
;�޸�ʱ�䣺
;�����ˣ����۾�
;*********************************************************************************************
(defun C:SearchPoint ( / allpointset alltextset app bfind ent i ldata p0 p1 p2 pstr)
    (prompt "\nSearchPoint����ָ����ŵĵ㡣")
    (setq pstr        (getstring "\n�����ţ�")
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
            ;;����
            (if (and ent bfind)
                (progn
                    (setq p0  (cdr (assoc 10 (entget ent)))
                          p1  (list (- (car p0) 10) (- (cadr p0) 10) 0)
                          p2  (list (+ (car p0) 10) (+ (cadr p0) 10) 0)
                          app (vlax-get-acad-object)
                    ) ;_ end_setq
                    (vla-zoomwindow app (vlax-3D-point p1) (vlax-3D-point p2))
                    (prompt (strcat "\n�����꣺(" (rtos  (car p0) 2 4)","(rtos  (cadr p0) 2 4)","(rtos  (caddr p0) 2 4) ")"))
                ) ;_ end_progn
                (prompt (strcat "\nδ�ҵ��� " pstr))
            ) ;_ end_if
        ) ;_ endProgn
    ) ;_ endif
) ;_ enddefun

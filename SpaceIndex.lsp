;;�������������Ϳռ�����
;;��߰��յ������ң�����λ�ò��ҵ��ٶ�

(setq 
	;gl_MAXLEAVES 5 	;;���Ҷ�ӵĽڵ����
	gl_PointSpaceIndex nil 	;;���ߵ�Ŀռ������ĸ��ڵ�
	gl_LineSpaceIndex nil	;;���߶εĿռ������ĸ��ڵ�
	gl_AllPointSpaceIndex nil 	;;��������,�����Ŀռ��������ڵ�,����ɾ����ͬͼԪ
	gl_MaxLevel 32		;;������������
	gl_MinSpace 0.1		;;�ռ���������С��ࣨ����С��෶Χ�ڣ��������Ҷ����������
)
;;bReset�Ƿ����´�������
(defun GetLineRoot(bReset / allLine)
	(if bReset
		(setq allLine (ssget "X" (list (cons 0 "LINE") (cons 67 0)))
			gl_LineSpaceIndex (CreateLineSpaceIndex allLine)
		)
		(if (= nil gl_LineSpaceIndex)
			(setq allLine (ssget "X" (list (cons 0 "LINE") (cons 67 0)))
				gl_LineSpaceIndex (CreateLineSpaceIndex allLine)
			)
		)
	)
	
	gl_LineSpaceIndex
)
;;
;;����Ϊģ�Ϳռ��е����ж���
(defun GetInsertRoot(bReset / allp)
	(if bReset
		(setq allp (ssget "X" (list (cons 0 "INSERT") (cons 67 0)))
			gl_PointSpaceIndex (CreatePointSpaceIndex allp "INSERT")
		)
		(if (= nil gl_PointSpaceIndex)
			(setq allp (ssget "X" (list (cons 0 "INSERT") (cons 67 0)))
				gl_PointSpaceIndex (CreatePointSpaceIndex allp "INSERT")
			)
		)
	)
	
	gl_PointSpaceIndex
)

(defun GetAllPointRoot(bReset / allp)
	(if bReset
		(setq allp (ssget "X" (list (cons -4 "<AND")(cons -4 "<OR")(cons 0 "INSERT") (cons 0 "TEXT") (cons -4 "OR>") (cons 67 0) (cons -4 "AND>")))
			gl_AllPointSpaceIndex(CreatePointSpaceIndex allp "INSERT"))
		(if (= nil gl_AllPointSpaceIndex)
			(setq allp (ssget "X" (list (cons -4 "<AND")(cons -4 "<OR")(cons 0 "INSERT") (cons 0 "TEXT") (cons -4 "OR>") (cons 67 0) (cons -4 "AND>")))
				gl_AllPointSpaceIndex(CreatePointSpaceIndex allp "INSERT"))
		)
	)
	gl_AllPointSpaceIndex
)

;;ResetSpaceIndex
;;�������ÿռ�����
(defun C:ResetSpaceIndex( / t1 )
	(setq t1 (StartWatch))
	(GetLineRoot T)
	(GetInsertRoot T)
	;;һ�㲻����ȫ����������������ʱ�Ϸ�ʱ
	(setq gl_AllPointSpaceIndex nil)
	(setq t1 (TimeCosted "�����ռ�����" t1))
	(prin1)
)

;;������Ŀռ�����
(defun CreatePointSpaceIndex (sset entityType / ent entNum  Height i LDcoord position RUcoord root 
				Width Xcenter Ycenter)
	;;			
	(setq 
		;allp (ssget "X" (list (cons -4 "<OR")(cons 0 "INSERT") (cons 0 "TEXT") (cons -4 "OR>") ))
		LDcoord (getvar "EXTMIN")	;;left down corner
		RUcoord (getvar "EXTMAX")	;;right up corner
		Width (- (car RUcoord) (car LDcoord))
		Height (- (cadr RUcoord) (cadr LDcoord))
		Xcenter (/ (+ (car RUcoord) (car LDcoord)) 2)
		Ycenter (/ (+ (cadr RUcoord) (cadr LDcoord)) 2)
		root nil
	)
	
	;;�½�ͼ�κ�Width<0 
	(if (or (< Width 1) (< Height 1))
		(setq Width 100000 Height 100000)
	)
	;;create top root
	(setq root (CreateRoot Xcenter Ycenter (/ (max Width Height) 2.0) (InitLeavesCapacity entityType nil) nil nil))
	
	(if sset
		(progn
			(setq entNum (sslength sset)
				i 0)
			(repeat entNum
				(setq ent (ssname sset i)
					position (cdr (assoc 10 (entget ent)))
					root (PutEntityIndex root ent (car position) (cadr position))
					i (1+ i)
				)
			)
		)
	)
	root
)
;;�������߶εĿռ�����
;;ֱ�ߵ������˵��������,����Ϊ0��ֱ��,ֱ��ɾ��.
(defun CreateLineSpaceIndex (sset /  ent entnum  height i ldcoord pos1 pos2 
								rucoord  root width xcenter ycenter midx midy)
	(setq ;allLine (ssget "X" (list (cons 0 "LINE")))
		LDcoord (getvar "EXTMIN")	;;left down corner
		RUcoord (getvar "EXTMAX")	;;right up corner
		Width (- (car RUcoord) (car LDcoord))
		Height (- (cadr RUcoord) (cadr LDcoord))
		Xcenter (/ (+ (car RUcoord) (car LDcoord)) 2)
		Ycenter (/ (+ (cadr RUcoord) (cadr LDcoord)) 2)
		root nil
	)
	;;�½�ͼ�κ�Width<0 
	(if (or (< Width 1) (< Height 1))
		(setq Width 100000 Height 100000)
	)
	;;create top root
	(setq root (CreateRoot Xcenter Ycenter (/ (max Width Height) 2.0) (InitLeavesCapacity "LINE" nil) nil nil))
	
	(if sset
		(progn
			(setq entNum (sslength sset)
				i 0)
			(repeat entNum
				(setq ent (ssname sset i)
					pos1 (cdr (assoc 10 (entget ent)))
					pos2 (cdr (assoc 11 (entget ent)))
					i (1+ i)
				)	
					;midx (/ (+ (car pos1) (car pos2)) 2.0)
					;midy (/ (+ (cadr pos1) (cadr pos2)) 2.0)
					;;delete zero line
				(if (> (distance pos1 pos2) gl_MIN)
					(setq root (PutEntityIndex root ent (car pos1) (cadr pos1))
						root (PutEntityIndex root ent (car pos2) (cadr pos2)))
					;;else delete it
					(vla-delete (vlax-ename->vla-object ent))
				)
			)
		)
	)
	root
)
;*********************************************************************************************
 ;��������:CreateRoot (x0 y0 step leafNum zoneNumber parentRoot)
 ;���ܣ�����PR�Ĳ����ĸ��ڵ㡣���zoneNumber PutEntityIndex ��Ϊ��,��ǰ��������Ϊ��;
 ;���ڵ������ӱ����,ǰ���ĸ�ΪҶ�ڵ�,Ҳ�������Ӹ��ڵ�
 ;���ڵ�����ݽṹ(RU LU LD RD (Xcenter Ycenter Step Level leafNum)) �ĸ����� �������꣬����,����,Ҷ�ӽڵ�����
 ;Level:������������0,��������+1
 ;Xcenter Ycenter ������
 ;Step:������>0,����(max Width Height)/2 
 ;zoneNumber:�ĸ�������,�����Ͽ�ʼ,��ʱ�뷽��,Ϊ0 1 2 3
 ;Ҷ�ӽڵ���һ����,����ENAME�͵����� ((x0 y0 ENAME)...)
 ;���أ��½��ĸ��ڵ� ()
 ;����ʱ�䣺2015/11/04   9:16
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun CreateRoot (x0 y0 step leafNum zoneNumber parentRoot / x1 y1 step1 level pos ret)
	(setq ret nil)
	;;RootNode child root
	(if (and zoneNumber (listp parentRoot))
		(progn
			(setq pos (GetRootPosition parentRoot)
				x1(car pos)
				y1 (cadr pos)
				step1 (/ (GetRootStep parentRoot) 2.0)
				level (+ 1 (GetRootLevel parentRoot))
				leafNum (GetRootLeafNum parentRoot)
			)
			(cond
				((= zoneNumber 0) (setq ret (list nil nil nil nil (list (+ x1 step1) (+ y1 step1) step1 level leafNum))))
				((= zoneNumber 1) (setq ret (list nil nil nil nil (list (- x1 step1) (+ y1 step1) step1 level leafNum))))
				((= zoneNumber 2) (setq ret (list nil nil nil nil (list (- x1 step1) (- y1 step1) step1 level leafNum))))
				((= zoneNumber 3) (setq ret (list nil nil nil nil (list (+ x1 step1) (- y1 step1) step1 level leafNum))))
			)
		)
	  	;;else top root
		(if (and x0 y0 step leafNum (= nil parentRoot))
			(setq ret (list nil nil nil nil (list x0 y0 step 0  leafNum)))
		)
	)
	
	ret
 )
 ;;��ȡ���ڵ�����ĵ�
 (defun GetRootPosition (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (list (car data) (cadr data))
		)
	)
	ret
 )
 
 ;;��ȡ���ڵ�ļ���
 (defun GetRootLevel (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (cadddr data)
		)
	)
	ret
 )
 
  ;;��ȡ���ڵ�Ĳ���
 (defun GetRootStep (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (caddr data)
		)
	)
	ret
 )
  ;;��ȡ���ڵ��Ҷ������
 (defun GetRootLeafNum (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (nth 4 data)
		)
	)
	ret
 )
 
 ;;��ʼ��Ҷ�Ӵ�С
;;�������Ϊ3-8
;InitLeavesCapacity (entityType num / ret)
;;����:entityType:ʵ������;num:�û��Զ����С
 (defun InitLeavesCapacity (entityType num / ret)
	(if (= nil num)
		(cond
			((= entityType "INSERT") (setq ret 6))
			((= entityType "TEXT") (setq ret 6))
			((= entityType "LINE") (setq ret 12))	;;����߶����ӵ�ͬһ��
			(T  8)
		)
		(setq ret num)
	)
	ret
 )
 ;*********************************************************************************************
 ;��������:GetZone (myRoot x0 y0 )
 ;���ܣ����ݵ�����x0 y0 �жϵ����ڵ�4������֮һ
 ;������x0,y0 ����,����Ϊnil
 ;���أ�((list 0 RU) (list 1 LU)...) or nil
 ;����ʱ�䣺2015/11/04   11:20
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun GetZone(myRoot x0 y0 / x1 y1 ret para)
	(setq ret nil)
	(if (and x0 y0 (listp myRoot))
		(progn
			(setq para (last myRoot)
				x1(car para)
				y1 (cadr para)
			)
			(cond ;;��λ�ڱ߽�Ĵ���:λ�ڱ߽�ĵ�,���صı���>1
				((and (>= x0 x1) (>= y0 y1)) (setq ret (list (list 0 (nth 0 myRoot)))))
				((and (<= x0 x1) (>= y0 y1)) (setq ret (cons (list 1 (nth 1 myRoot)) ret)))
				((and (<= x0 x1) (<= y0 y1)) (setq ret (cons (list 2 (nth 2 myRoot)) ret)))
				((and (>= x0 x1) (<= y0 y1)) (setq ret (cons (list 3 (nth 3 myRoot)) ret)))
			)
		)
	)
	ret
 )
 
 ;;�ж�һ�����Ƿ񳬳������ռ䷶Χ
 (defun IsOutRange (myRoot x0 y0 / x1 y1 p0 ret)
	(setq ret nil)
	(if (and x0 y0 (listp myRoot))
		(progn
			(setq p0 (GetRootPosition myRoot)
				step (GetRootStep myRoot)
				x1 (car p0)
				y1 (cadr p0)
			)
			(if (or (> x0 (+ x1 step))
					(> y0 (+ y1 step))
					(< x0 (- x1 step))
					(< y0 (- y1 step))
				)
				(setq ret T)
			)
		)
	)	
	ret
 )
  ;*********************************************************************************************
 ;��������:PutEntityIndex (myRoot ent x0 y0)
 ;���ܣ���ʵ���������,ʹ���˵���
 ;myRoot ���ڵ�,����Ϊ��
 ;������ent,x0,y0 ENAME,����,����Ϊnil
 ;���أ���ǰ���ڵ�
 ;����ʱ�䣺2015/11/04   11:20
 ;�޸�ʱ�䣺
 ;�����ˣ����۾�
 ;*********************************************************************************************
 (defun PutEntityIndex (myRoot ent x0 y0 / childroot e iZone myzone myzones newelement node)
	(setq myZones (GetZone myRoot x0 y0)
		newElement (list x0 y0 ent)
		leafNum (GetRootLeafNum myRoot))
	(foreach myZone myZones
		(setq iZone (car myZone)
			node (cadr myZone)
		)
		(if (IsRootNode node)
			;;Ϊ���ڵ�,����
			(setq node (PutEntityIndex node ent x0 y0)
				myRoot (ListSubstIndex myRoot node iZone)
			)
			;;else ΪҶ�ӽڵ�,����Ҷ�ӽڵ�.�ڵ�ռ�����,����ѽڵ�
			(if (> leafNum (length node))
				(setq  myRoot (ListSubstIndex myRoot (cons newElement node) iZone))
				;;��Ҷ�ڵ�,����
				(progn
					;;����㼯��һ��λ�ý�������ѭ��
					(if (> (GetRootLevel myRoot) gl_MaxLevel)
						(*error* (strcat "�����ռ�������������" (rtos gl_MaxLevel 2 0)))
					)
					(if (< (GetRootStep myRoot) gl_MinSpace)
						(*error* (strcat "����" (rtos leafNum 2 0) "�㼯����" (vl-princ-to-string (GetRootPosition myRoot))))
					)
					
					(setq childRoot (CreateRoot nil nil nil nil iZone myRoot))
					(foreach e (cons newElement node)
						(setq childRoot (PutEntityIndex childRoot (caddr e) (car e) (cadr e)))
					)
					(setq myRoot (ListSubstIndex myRoot childRoot iZone))
				)
			)
		)
	)
	myRoot
 )
 
 ;;�жϽڵ��Ƿ��Ǹ��ڵ�
 (defun IsRootNode (node / data ret)
	(setq ret nil)
	(if (listp node)
		(if (= 5 (length node))
			(if (listp (setq data (last node)))
				(if (and (= 5 (length data)) (= 'INT (type(last data))))
					(setq ret T)
				)
			)
		)
	)
	ret
 )
 
 ;;���ظ��ڵ�ӵ�е�Ҷ��Ԫ�ر�
 ;;����nil or ((x0 y0 ENAME)...)
 ;;����nil ���������,not root,all nodes are roots,all leaves are empty
 (defun GetLeavesInRoot(myRoot / ret node)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(progn
			(if (IsRootNode (setq node (car myRoot)))
				(setq ret (append ret (GetLeavesInRoot node)) )
				(setq ret (append ret node))
			)
			(if (IsRootNode (setq node (cadr myRoot)))
				(setq ret (append ret (GetLeavesInRoot node)) )
				(setq ret (append ret node))
			)
			(if (IsRootNode (setq node (caddr myRoot)))
				(setq ret (append ret (GetLeavesInRoot node)) )
				(setq ret (append ret node))
			)
			(if (IsRootNode (setq node (cadddr myRoot)))
				(setq ret (append ret (GetLeavesInRoot node)) )
				(setq ret (append ret node))
			)
		)
		(setq ret (append ret myRoot))
	)
	ret
 )
 
 ;;�жϸ��ڵ����нڵ㶼�ǿ�Ҷ��,ɾ���ڵ�ʱʹ�øú��������ж�
 (defun IsEmptyLeaves(myRoot / ret node)
	(setq ret T)
	;;every region is not a root or an empty leaf
	(if (IsRootNode (setq node (car myRoot)))
		(setq ret nil)
		(if (> (length node) 0)
			(setq ret nil)
		)
	)
	(if ret 
		(if (IsRootNode (setq node (cadr myRoot)))
			(setq ret nil)
			(if (> (length node) 0)
				(setq ret nil)
			)
		)	
	)
	(if ret 
		(if (IsRootNode (setq node (caddr myRoot)))
			(setq ret nil)
			(if (> (length node) 0)
				(setq ret nil)
			)
		)	
	)
	(if ret 
		(if (IsRootNode (setq node (cadddr myRoot)))
			(setq ret nil)
			(if (> (length node) 0)
				(setq ret nil)
			)
		)	
	)
	ret
 )
 
 ;;�жϽڵ��Ƿ�����Ҷ�ڵ�,���ٽ����ж��Ƿ��Ǹ��ڵ�
 ;;ʹ��֮ǰ,�������ж��Ƿ��Ǹ��ڵ�.
 ; (defun IsFullLeaves (node / ret)
	; (setq ret nil)
	; (if (= gl_MAXLEAVES (length node))
		; (setq ret T)
	; )
	; ret
 ; )
 
 ;;�滻�б��е�ָ��λ��Ԫ��
 ;;data Ϊlist; 	ne:new element;	pos:position index from 0
 (defun ListSubstIndex (data ne pos / newdata num i)
	(if (and (listp data) pos)
		(progn
			(setq num (length data))
			(if (< pos num)
				(progn
					(setq newdata nil 
						i (- num 1))
					(while (> i pos)
						(setq newdata (cons (nth i data) newdata)
							i (1- i)
						)
					)
					(if (= i pos) 
						(setq newdata (cons ne newdata)
							i (1- i))
					)
					
					(while (and (>= i 0) (< i pos))
						(setq newdata (cons (nth i data) newdata)
							i (1- i)
						)
					)
					
					(setq data newdata)
				)
			)
		)
	)
	data
 )
 ;;ר����Ը��ڵ�Ĵ����㷨
 (defun ListSubstIndex2 (data ne pos / ret)
	(setq ret data)
	(if (and (listp data) pos)
		(cond
			((= pos 0) (setq ret (cons ne (cdr data))))
			((= pos 1) (setq ret (cons (car data) (cons ne (cddr data)))))
			((= pos 2) (setq ret (cons (car data) (cons (cadr data) (cons ne (cdddr data))))))
			((= pos 3) (setq ret (cons (car data) (cons (cadr data) (cons (cadddr data) (cons ne (cddddr data)))))))
			(T (setq ret data))
		)
	)
	ret
 )
 
 ;;ɾ��һ��ʵ�������
 ;;���ر��޸ĺ�ĸ��ڵ�
 (defun DelEntityIndex (myRoot ent x0 y0 / delElement entlist izone myzone myzones node)
	(setq delElement (list x0 y0 ent) 
		myZones (GetZone myRoot x0 y0)
	)
	(foreach myZone myZones
		(setq iZone (car myZone)
			node (cadr myZone)
		)
		(if (IsRootNode node)
			;;Ϊ���ڵ�,����
			(setq node (DelEntityIndex node ent x0 y0)
				myRoot (ListSubstIndex myRoot node iZone)
			)
			;;else ΪҶ�ӽڵ�,ɾ��Ԫ��,��Ҷ��ɾ����Ϊ��,���������3�������Ƿ�Ϊ��,����,���ϼ����ڵ��ΪҶ�ӽڵ�
			(progn
				;;�Ƴ�Ԫ��
				(setq node (vl-remove delElement node)
					myRoot (ListSubstIndex myRoot node iZone)
				)
				;;�ж��Ƿ�Ϊ�սڵ�,����,��myRoot ��Ϊ�յ�Ҷ��
				(if (IsEmptyLeaves myRoot)
					(if (< 0 (GetRootLevel myRoot))	;;top root can't be modifyed to leaf
						(setq myRoot nil)
					)
				)
			)
		)
	)
	myRoot
 )
 
 ;;����ʵ���������ռ�λ�÷����仯��
 ;;ʵ������ֻ����Insert Line
 (defun UpdatePointIndex (myRoot ent p10 p20)
	(setq myRoot (DelEntityIndex myRoot ent (car p10) (cadr p10))
		myRoot (PutEntityIndex myRoot ent (car p20) (cadr p20))
	)
 )
 (defun UpdateLineIndxe (myRoot ent p10 p11 p20 p21)
	(setq myRoot (DelEntityIndex myRoot ent (car p10) (cadr p10))
		myRoot (DelEntityIndex myRoot ent (car p11) (cadr p11))
		myRoot (PutEntityIndex myRoot ent (car p20) (cadr p20))
		myRoot (PutEntityIndex myRoot ent (car p21) (cadr p21))
	)
 )
 
 ;;���Ҿ���һ����ĵ�Ԫ�б�,
 ;;�����ҵ���Ԫ���б�,Ҷ�ӱ��ʽ:((x0 y0 ENAME)...) or nil
 ;;�����߽��ϵ��ظ�Ԫ��
 (defun SearchPoint (myRoot x0 y0 / entlist myZones myZone iZone node  p0 e)
	(setq myZones (GetZone myRoot x0 y0)
		entlist nil
		p0 (list x0 y0)
		)
	(foreach myZone myZones
		(setq iZone (car myZone)
			node (cadr myZone)
		)
		(if (IsRootNode node)
			;;Ϊ���ڵ�,����
			(setq entlist (append  entlist (SearchPoint node x0 y0)))
			;;else ΪҶ�ӽڵ�
			(foreach e node
				(if (< (setq dist (distance (list (car e) (cadr e)) p0)) gl_MIN)
					(setq entlist (cons e entlist))
				  ;(princ (strcat "\n" (rtos dist 2 6)))
				)
			)
		)
	)
	
	;;ȥ���߽��ϵ��ظ�Ԫ��
	;(setq entlist (ListRemoveSameElement entlist))
	entlist
 )
 
;;���Ҷϵ���pt1,pt2���߶�
;;���أ��������������ֱ�߱�
(defun SearchLine (pt1 pt2 /  e1 e2 lines1 lines2 ret )
	(setq ret nil)
	(if (and pt1  pt2)
		(progn
			(setq lines1 (SearchPoint (GetLineRoot nil) (car pt1) (cadr pt1))
				lines2 (SearchPoint (GetLineRoot nil) (car pt2) (cadr pt2))
			)
			(if (and lines1 lines2)
				(foreach e1 lines1
					(foreach e2 lines2
						(if (equal (nth 2 e1) (nth 2 e2))
							(setq ret (cons (nth 2 e1) ret))
						)
					)
				)
			)
		)
	)
	(if ret
		(ListRemoveSameElement ret)
	)
	ret
)
 
 
 ;;������ͬͼԪ,����ÿ��Ҷ��,����Ҷ������ͬͼԪ
 ;;�п��ܰ����߽��ϵ��ظ�ͼԪ
 ;;����:ʵ��� (ename1 ename2 ...)
 ;;Dependence:geometry.lsp:IsSameEntity
 (defun Index-GetSamePositionEntity (myRoot  / dellist e1 e2 node nodelist)
	(setq ;rootData (last myRoot)
		nodelist (list (car myRoot) (cadr myRoot) (caddr myRoot) (cadddr myRoot))
		delList nil
	)
	(foreach node nodelist
		(if (IsRootNode node)
			(setq delList (append delList (Index-GetSamePositionEntity node)))
			;;a leaf
			(while (> (length node) 1)
				(setq e1 (car node)
					node (cdr node)
				)
				(foreach e2 node
					(if (IsSameEntity (last e1) (last e2))
						(setq delList (cons (last e2) delList)
							node (vl-remove e2 node)
						)
					)
				)
			)
		)
	)
	delList
 )
 
 ;;Test
 (defun C:TestIndex( / dellist e lineroot pointroot time1 time2)
	;;ɾ����ͬͼԪ
	;;����ÿ��Ҷ��,ɾ��Ҷ������ͬͼԪ����
	(setq time1 (StartWatch))
	
	(setq pointRoot (GetAllPointRoot T))
	(DrawSpaceIndex pointRoot)
	; (princ "\n �����:")
	; (princ (length pointList))
	
	(setq lineRoot (GetLineRoot T))
	;(DrawSpaceIndex lineRoot)
	
	(setq time2 (TimeCosted "�������ߵ�����" time1))
	
	;;search a point ent
	(repeat 1
		(setq npoint (SearchPoint pointRoot 2401.2601 1619.2218))
		(princ (strcat "�ҵ���" (rtos (length npoint) 2 0) "����"))
		(if (> (length npoint) 0)
		  	(progn
				(princ npoint)
				;;delete point
				(setq ent (last (car npoint))
					p1 (cdr (assoc 10 (entget ent)))
				)
				(vla-delete (vlax-ename->vla-object ent))
				(DelEntityIndex pointRoot ent (car p1) (cadr p1))
			)
		)
	)
   	
	(setq time2 (TimeCosted "����һ����֪��1��" time2))
	
	; (repeat 10000
		; (SearchPoint pointRoot 66259.915 71087.845)
	; )
	; (setq time2 (TimeCosted "����һ�������ڵĵ�10000��" time2))
	
	; (setq lineRoot (CreateLineSpaceIndex)
		; lineList (GetLeavesInRoot lineRoot)
	; )
	; (princ "\n �߶θ���:")
	; (princ (length lineList))
	
	; (setq time2 (TimeCosted "�������߶εĿռ�����" time2))
	
	; (setq delList (Index-GetSamePositionEntity pointRoot)
		; delList (ListRemoveSameElement delList)
	; )
	; (foreach e delList
		 ; (vla-delete (vlax-ename->vla-object (caddr e)))
	; )
	; (setq time2 (TimeCosted (strcat "ɾ��" (rtos (length delList) 2 0) "���ظ����ߵ����,") time2))
	
	; (setq delList (Index-GetSamePositionEntity lineRoot)
		; delList (ListRemoveSameElement delList)
	; )
	; (foreach e delList
		 ; (vla-delete (vlax-ename->vla-object (caddr e)))
	; )
	; (setq time2 (TimeCosted (strcat "ɾ��" (rtos (length delList) 2 0) "���ظ����߶�,") time2))
	
	; (TimeCosted "ɾ����ͬͼԪ" time1)
	(princ)
 )
 
 ;;���� distance �� �Ƚ��ٶ� 10w�μ���
; ����: TESTD
; Distance��ʱ:0.457s.
; �Ƚϵķ�����ʱ:1.175s.
 (defun C:TestD( / dist m p10 p20 ret t1 t2)
	;;distance
	(setq t1 (StartWatch))
	(setq p10 '(10.1 100.1 1.0)
		p20 '(10.11 100.11 1.1)
		ret nil
		m 1.0)
	(repeat 100000
		(setq dist (distance p10 p20))
		(if (< dist m)
			(setq ret(cons dist ret))
		)
	)
	(setq t2 (TimeCosted "Distance" t1))
	
	;;bijiao
	(repeat 100000
		(if (and (< (abs (- (car p10) (car p20))) m) (< (abs (- (cadr p10) (cadr p20))) m))
			(setq ret (cons T ret))
		)
	)
	(setq t2 (TimeCosted "�Ƚϵķ���" t2))
   	(princ)
 )
 
 ;;Draw SpaceIndex center point and line 
 ;;�������еĸ��ڵ�
 (defun DrawSpaceIndex(myRoot / color node p0 step x0 y0)
	(if (IsRootNode myRoot)
		(progn
			;;Draw myRoot
			(setq p0 (GetRootPosition myRoot)
				x0 (car p0)
				y0 (cadr p0)
				step (GetRootStep myRoot)
				color (+ 1 (GetRootLevel myRoot))
			)
			;;horizon line
			(entmake (list '(0 . "LINE") 
						   '(8 . "0") 
						   (cons 62 color) 
						   (cons 10 (list (- x0 step) y0 0)) 
						   (cons 11 (list (+ x0 step) y0 0))))
			;;vertical line
			(entmake (list '(0 . "LINE") 
						   '(8 . "0") 
						   (cons 62 color) 
						   (cons 10 (list x0 (+ y0 step) 0)) 
						   (cons 11 (list x0 (- y0 step) 0))))
						   
			;;Then check leaves if they are root
			(if (IsRootNode (setq node (car myRoot)))
				(DrawSpaceIndex node)
			)
			(if (IsRootNode (setq node (cadr myRoot)))
				(DrawSpaceIndex node)
			)
			(if (IsRootNode (setq node (caddr myRoot)))
				(DrawSpaceIndex node)
			)
			(if (IsRootNode (setq node (cadddr myRoot)))
				(DrawSpaceIndex node)
			)
		)
	)
 )
 
 
 ; ;;���´����ڶ�������������ڱ༭����ʱ��ֱ����CAD��������ɾ���˶��󣬶�����δ���µ��µ����⡣
 ; ;;�����µ����index
 ; ;;pt1 ���£�pt2 ����
 ; (defun UpdateRegionIndex (pt1 pt2)

 ; )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
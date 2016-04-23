;;建立点名索引和空间索引
;;提高按照点名查找，按照位置查找的速度

(setq 
	;gl_MAXLEAVES 5 	;;最大叶子的节点个数
	gl_PointSpaceIndex nil 	;;管线点的空间索引的根节点
	gl_LineSpaceIndex nil	;;管线段的空间索引的根节点
	gl_AllPointSpaceIndex nil 	;;包含文字,块对象的空间索引根节点,用于删除相同图元
	gl_MaxLevel 32		;;索引的最大层数
	gl_MinSpace 0.1		;;空间索引的最小间距（在最小间距范围内，超过最大叶子数将报错）
)
;;bReset是否重新创建索引
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
;;限制为模型空间中的所有对象；
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
;;重新设置空间索引
(defun C:ResetSpaceIndex( / t1 )
	(setq t1 (StartWatch))
	(GetLineRoot T)
	(GetInsertRoot T)
	;;一般不重置全部索引，大数据量时较费时
	(setq gl_AllPointSpaceIndex nil)
	(setq t1 (TimeCosted "建立空间索引" t1))
	(prin1)
)

;;创建点的空间索引
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
	
	;;新建图形后，Width<0 
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
;;创建管线段的空间索引
;;直线的两个端点放入索引,长度为0的直线,直接删除.
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
	;;新建图形后，Width<0 
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
 ;函数定义:CreateRoot (x0 y0 step leafNum zoneNumber parentRoot)
 ;功能：创建PR四叉树的根节点。如果zoneNumber PutEntityIndex 不为空,则前面三参数为空;
 ;根节点六个子表组成,前面四个为叶节点,也可能是子根节点
 ;根节点的数据结构(RU LU LD RD (Xcenter Ycenter Step Level leafNum)) 四个区域 中心坐标，级别,步长,叶子节点容量
 ;Level:整数顶级根是0,以下依次+1
 ;Xcenter Ycenter 浮点数
 ;Step:浮点数>0,本级(max Width Height)/2 
 ;zoneNumber:四个区域编号,从右上开始,逆时针方向,为0 1 2 3
 ;叶子节点是一个表,包含ENAME和点坐标 ((x0 y0 ENAME)...)
 ;返回：新建的根节点 ()
 ;创建时间：2015/11/04   9:16
 ;修改时间：
 ;创建人：沈雄君
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
 ;;获取根节点的中心点
 (defun GetRootPosition (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (list (car data) (cadr data))
		)
	)
	ret
 )
 
 ;;获取根节点的级别
 (defun GetRootLevel (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (cadddr data)
		)
	)
	ret
 )
 
  ;;获取根节点的步长
 (defun GetRootStep (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (caddr data)
		)
	)
	ret
 )
  ;;获取根节点的叶子容量
 (defun GetRootLeafNum (myRoot / ret data)
	(setq ret nil)
	(if (IsRootNode myRoot)
		(setq data (last myRoot)
			ret (nth 4 data)
		)
	)
	ret
 )
 
 ;;初始化叶子大小
;;最佳容量为3-8
;InitLeavesCapacity (entityType num / ret)
;;参数:entityType:实体类型;num:用户自定义大小
 (defun InitLeavesCapacity (entityType num / ret)
	(if (= nil num)
		(cond
			((= entityType "INSERT") (setq ret 6))
			((= entityType "TEXT") (setq ret 6))
			((= entityType "LINE") (setq ret 12))	;;多个线段连接到同一点
			(T  8)
		)
		(setq ret num)
	)
	ret
 )
 ;*********************************************************************************************
 ;函数定义:GetZone (myRoot x0 y0 )
 ;功能：根据点坐标x0 y0 判断点所在的4个区域之一
 ;参数：x0,y0 坐标,不能为nil
 ;返回：((list 0 RU) (list 1 LU)...) or nil
 ;创建时间：2015/11/04   11:20
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
 (defun GetZone(myRoot x0 y0 / x1 y1 ret para)
	(setq ret nil)
	(if (and x0 y0 (listp myRoot))
		(progn
			(setq para (last myRoot)
				x1(car para)
				y1 (cadr para)
			)
			(cond ;;点位于边界的处理:位于边界的点,返回的表长度>1
				((and (>= x0 x1) (>= y0 y1)) (setq ret (list (list 0 (nth 0 myRoot)))))
				((and (<= x0 x1) (>= y0 y1)) (setq ret (cons (list 1 (nth 1 myRoot)) ret)))
				((and (<= x0 x1) (<= y0 y1)) (setq ret (cons (list 2 (nth 2 myRoot)) ret)))
				((and (>= x0 x1) (<= y0 y1)) (setq ret (cons (list 3 (nth 3 myRoot)) ret)))
			)
		)
	)
	ret
 )
 
 ;;判断一个点是否超出索引空间范围
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
 ;函数定义:PutEntityIndex (myRoot ent x0 y0)
 ;功能：把实体加入索引,使用了迭代
 ;myRoot 根节点,不能为空
 ;参数：ent,x0,y0 ENAME,坐标,不能为nil
 ;返回：当前根节点
 ;创建时间：2015/11/04   11:20
 ;修改时间：
 ;创建人：沈雄君
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
			;;为根节点,迭代
			(setq node (PutEntityIndex node ent x0 y0)
				myRoot (ListSubstIndex myRoot node iZone)
			)
			;;else 为叶子节点,加入叶子节点.节点空间已满,则分裂节点
			(if (> leafNum (length node))
				(setq  myRoot (ListSubstIndex myRoot (cons newElement node) iZone))
				;;满叶节点,分裂
				(progn
					;;过多点集中一个位置将引发死循环
					(if (> (GetRootLevel myRoot) gl_MaxLevel)
						(*error* (strcat "超过空间索引的最大层数" (rtos gl_MaxLevel 2 0)))
					)
					(if (< (GetRootStep myRoot) gl_MinSpace)
						(*error* (strcat "超过" (rtos leafNum 2 0) "点集中在" (vl-princ-to-string (GetRootPosition myRoot))))
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
 
 ;;判断节点是否是根节点
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
 
 ;;返回根节点拥有的叶子元素表
 ;;返回nil or ((x0 y0 ENAME)...)
 ;;返回nil 有三种情况,not root,all nodes are roots,all leaves are empty
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
 
 ;;判断根节点所有节点都是空叶子,删除节点时使用该函数进行判断
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
 
 ;;判断节点是否是满叶节点,不再进行判断是否是根节点
 ;;使用之前,必须先判断是否是根节点.
 ; (defun IsFullLeaves (node / ret)
	; (setq ret nil)
	; (if (= gl_MAXLEAVES (length node))
		; (setq ret T)
	; )
	; ret
 ; )
 
 ;;替换列表中的指定位置元素
 ;;data 为list; 	ne:new element;	pos:position index from 0
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
 ;;专门针对根节点的代换算法
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
 
 ;;删除一个实体的索引
 ;;返回被修改后的根节点
 (defun DelEntityIndex (myRoot ent x0 y0 / delElement entlist izone myzone myzones node)
	(setq delElement (list x0 y0 ent) 
		myZones (GetZone myRoot x0 y0)
	)
	(foreach myZone myZones
		(setq iZone (car myZone)
			node (cadr myZone)
		)
		(if (IsRootNode node)
			;;为根节点,迭代
			(setq node (DelEntityIndex node ent x0 y0)
				myRoot (ListSubstIndex myRoot node iZone)
			)
			;;else 为叶子节点,删除元素,若叶子删除后为空,则查找其他3个区域是否为空,若是,则上级根节点变为叶子节点
			(progn
				;;移除元素
				(setq node (vl-remove delElement node)
					myRoot (ListSubstIndex myRoot node iZone)
				)
				;;判断是否为空节点,若是,则myRoot 变为空的叶子
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
 
 ;;更新实体索引（空间位置发生变化）
 ;;实体类型只能是Insert Line
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
 
 ;;查找经过一个点的点元列表,
 ;;返回找到的元素列表,叶子表格式:((x0 y0 ENAME)...) or nil
 ;;包含边界上的重复元素
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
			;;为根节点,迭代
			(setq entlist (append  entlist (SearchPoint node x0 y0)))
			;;else 为叶子节点
			(foreach e node
				(if (< (setq dist (distance (list (car e) (cadr e)) p0)) gl_MIN)
					(setq entlist (cons e entlist))
				  ;(princ (strcat "\n" (rtos dist 2 6)))
				)
			)
		)
	)
	
	;;去掉边界上的重复元素
	;(setq entlist (ListRemoveSameElement entlist))
	entlist
 )
 
;;查找断点是pt1,pt2的线段
;;返回：经过两点的所有直线表
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
 
 
 ;;查找相同图元,遍历每个叶子,查找叶子中相同图元
 ;;有可能包含边界上的重复图元
 ;;返回:实体表 (ename1 ename2 ...)
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
	;;删除相同图元
	;;遍历每个叶子,删除叶子中相同图元即可
	(setq time1 (StartWatch))
	
	(setq pointRoot (GetAllPointRoot T))
	(DrawSpaceIndex pointRoot)
	; (princ "\n 点个数:")
	; (princ (length pointList))
	
	(setq lineRoot (GetLineRoot T))
	;(DrawSpaceIndex lineRoot)
	
	(setq time2 (TimeCosted "创建管线点索引" time1))
	
	;;search a point ent
	(repeat 1
		(setq npoint (SearchPoint pointRoot 2401.2601 1619.2218))
		(princ (strcat "找到了" (rtos (length npoint) 2 0) "个点"))
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
   	
	(setq time2 (TimeCosted "查找一个已知点1次" time2))
	
	; (repeat 10000
		; (SearchPoint pointRoot 66259.915 71087.845)
	; )
	; (setq time2 (TimeCosted "查找一个不存在的点10000次" time2))
	
	; (setq lineRoot (CreateLineSpaceIndex)
		; lineList (GetLeavesInRoot lineRoot)
	; )
	; (princ "\n 线段个数:")
	; (princ (length lineList))
	
	; (setq time2 (TimeCosted "创建管线段的空间索引" time2))
	
	; (setq delList (Index-GetSamePositionEntity pointRoot)
		; delList (ListRemoveSameElement delList)
	; )
	; (foreach e delList
		 ; (vla-delete (vlax-ename->vla-object (caddr e)))
	; )
	; (setq time2 (TimeCosted (strcat "删除" (rtos (length delList) 2 0) "个重复管线点对象,") time2))
	
	; (setq delList (Index-GetSamePositionEntity lineRoot)
		; delList (ListRemoveSameElement delList)
	; )
	; (foreach e delList
		 ; (vla-delete (vlax-ename->vla-object (caddr e)))
	; )
	; (setq time2 (TimeCosted (strcat "删除" (rtos (length delList) 2 0) "个重复管线段,") time2))
	
	; (TimeCosted "删除相同图元" time1)
	(princ)
 )
 
 ;;测试 distance 与 比较速度 10w次计算
; 命令: TESTD
; Distance耗时:0.457s.
; 比较的方法耗时:1.175s.
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
	(setq t2 (TimeCosted "比较的方法" t2))
   	(princ)
 )
 
 ;;Draw SpaceIndex center point and line 
 ;;遍历所有的根节点
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
 
 
 ; ;;更新窗口内对象的索引，用于编辑对象时，直接在CAD内新增或删除了对象，而索引未更新导致的问题。
 ; ;;仅更新点和线index
 ; ;;pt1 左下，pt2 右上
 ; (defun UpdateRegionIndex (pt1 pt2)

 ; )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
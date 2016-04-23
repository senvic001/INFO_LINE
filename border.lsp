;;区域管线对象
;;规范:宽度大于2m的管线点或段,应测量边界
;;边界:直线边界,点边界
;;边界的测量:按照闭合曲线顺序测量边界,点号前缀加入"-1","-"表示边界,1表示第一个边界点,-1表示边界的第一个点
;;边界限定:(1)边界为多段线,包含属性(类别,深度,内部的点和线对象) ;(2)边界必须闭合;(3)边界存储数据为点表
;;

;;function:AddBorder(pointList mainType depth )
;;para:pointList:3D点表 (x1 y1 z1 x2 y2 z2...)用于创建多段线
;;para:mainType:管线主类型
;;para:depth:范围内的代表深度
;;para:insidePointName:	内部点名称
;;para:bClosed:	是否闭合
;;return:nil or ename of a polyline.
;;condition:Layer name mainType+Border
(defun AddBorder(pointList mainType depth insidePointName bClosed / ename layerobj plobj outlist ret)
	(setq ret nil)
	(if (and (listp pointList) mainType)
		(progn
			(setq layerobj (AddLayer mainType (strcat mainType "_Border")) 
			)
			(if (setq plobj (Add3DPolyLineObject pointList))
				(progn
					(if bClosed (vla-put-Closed plobj :vlax-true))
					(setq ename (vlax-vla-object->ename plobj))
					;;设置AppID 用于ssget
					(setxdata ename gl_AppName (list (cons 1000 gl_Version))) 
					
					(setq outlist (list (cons "MainType" mainType) (cons "bClosed" "1")))
					;;set depth
					(if depth 
						(cond 
							((= (type depth) 'STR) (setq outlist (cons (cons "Depth" depth) outlist)))
							((= (numberp depth)) (setq outlist (cons (cons "Depth" (rtos depth 2 2)) outlist)))
						)
					)
					;;insideEntity
					(if insidePointName
						(setq outlist (cons (cons "Inside_Point" insidePointName) outlist))
					)
					(vlax-ldata-put ename gl_AppName outlist)
					(setq ret enmae)
				)
			)
		)
	)
	ret
)
;;Vectors from 2D to 3D
;;LightWeightPolyLine to 3DPolyLine
(defun Vectors2Dto3D(Vectors2D / i len vector3d)
	(setq vector3D nil)
	(if (> (setq len (length Vectors2D)) 3);;at least has two points
		(progn
			(setq len (/ len 2)
				i 0
			)
			(repeat len
				(setq vector3D (append vector3D (list (nth i Vectors2D) (nth (+ i 1) Vectors2D) 0.0))
					i (+ i 2)
				)
			)
		)
	)
	vector3D
)

;;保存到数据库中
;;Create Table BORDER (ID IDENTITY,Type TEXT(10),Inside_Point Text(14),Depth Float,Data Text,bClosed BIT)
(defun CreateBorderTable ( / connectionobject createtablestr result)
	(if (setq ConnectionObject (GetConnectionObject))
        (progn
			(setq createTableStr "Create Table BORDER (ID IDENTITY,Type TEXT(10),Inside_Point Text(14),Depth Float,Data Text,bClosed BIT)")
			(if(setq Result (ADOLISP_DoSQL ConnectionObject createTableStr))
				(progn
					(prompt "\n创建Border表成功！")
					;(princ Result)
				)
				 (ADOLISP_ErrorPrinter )
			)
			(ADOLISP_DisconnectFromDB ConnectionObject)
			(setq connectionobject nil)
		)
	)
	Result
)
;;保存边界数据到数据库
(defun SaveBorderData( / allpoly connectionobject data depth e i obj result sqlstr vectores vectoresstr bClosed idata)
	(if (setq ConnectionObject (GetConnectionObject))
        (progn
			;;1.Border表是否存在，否则创建表
			(if (setq Tables (car (ADOLISP_GetTablesAndViews ConnectionObject)))
				(if (not (vl-position "BORDER" Tables))
					(CreateBorderTable )
				)
				(CreateBorderTable)
			)
			(ADOLISP_DisconnectFromDB ConnectionObject)
			(setq ConnectionObject (GetConnectionObject))
			;;2.插入数据
			(setq i 0)
			(if (setq allPoly (ssget "X" (list (cons 0 "POLYLINE")(list -3 (list gl_AppName)))))
				(repeat (sslength allPoly)
					(setq e (ssname allPoly i)
						obj (vlax-ename->vla-object e)
						vectores (vlax-safearray->list(vlax-variant-value(vla-get-Coordinates obj)))
						vectoresStr ""
						sqlstr "Insert Into BORDER (Type,Inside_Point,Depth,Data,bClosed) VALUES ("
					)
					;;坐标列表
					(setq idata 0)
					(repeat (length vectores)
						(setq vectoresStr (strcat vectoresStr (rtos (nth idata vectores) 2 4)  " ")
							idata (1+ idata)
						)
					)
					(setq vectoresStr (strcat "(" vectoresStr ")"))
					
					(if (= :vlax-true (vla-get-closed obj ))
						(setq bClosed "1")
						(setq bClosed "0")
					)
					(if (setq data (ldata-get e "MainType"))
						(setq sqlstr (strcat sqlstr " '" data "'"))
						(setq sqlstr (strcat sqlstr " NULL"))
					)
					(if (setq data (ldata-get e "Inside_Point"))
						(setq sqlstr (strcat sqlstr " ,'" data "'"))
						(setq sqlstr (strcat sqlstr " ,NULL"))
					)
					(if (setq data (ldata-get e "Depth"))
						(if (> (strlen data ) 0)
							(setq sqlstr (strcat sqlstr "," data))
							(setq sqlstr (strcat sqlstr ",NULL"))
						)
						(setq sqlstr (strcat sqlstr ",NULL"))
					)
					(if vectoresStr
						(setq sqlstr (strcat sqlstr ",'" vectoresStr "'"))
						(setq sqlstr (strcat sqlstr ",NULL"))
					)
					(setq sqlstr (strcat sqlstr "," bClosed ")"))
					;;
					(if (setq Result (ADOLISP_DoSQL ConnectionObject sqlstr))
						(setq i (1+ i))
						;(*error* (strcat "保存数据出错！\n" sqlstr ))
						(ADOLISP_ErrorPrinter)
					)
				)
			)	
			(ADOLISP_DisconnectFromDB ConnectionObject)
		)
	)
	(princ (strcat "\n保存了" (rtos i 2 0) "个管线边界线段。"))
	i
)

;;根据类型绘制边界
;;para:typelist:("J" "L" "Y"...),if typelist is nil,then draw all borders
;;return:number of polylines
(defun DrawBorderFromDb(typelist / Inside_Point connectionobject constr data datum depth e ename i
					mspace newarray plobj result sqlstr typename vectores vectoresstr bClosed)
	(setq i 0)
	(if (setq ConnectionObject (GetConnectionObject))
        (progn
			;;where 
			(setq constr "")
			(if (> (length typelist) 0)
				(progn
					(setq constr (strcat "Where Type='" (car typelist) "'" )
						typelist (cdr typelist)
					)
					(foreach e typelist
						(setq constr (strcat constr " Or Type='" e "'"))
					)
				)
			)
			(setq sqlstr (strcat "Select * from BORDER " constr))
			;;get data and read
			(if(setq Result (ADOLISP_DoSQL ConnectionObject sqlstr))
				(progn
					(setq datum (cdr Result)
						mSpace (vla-get-modelspace (vla-get-Activedocument (vlax-get-acad-object)))
					)
					(foreach data datum
						(setq  Inside_Point (nth 2 data)
							vectoresStr (nth 4 data)
							bClosed (nth 5 data)
						)
						(if (= bClosed "True") (setq bClosed T) (setq bClosed nil))
						(if (setq typename (nth 1 data))
							(if (= (strlen typename) 0)
								(setq typename nil)
							)
						)
						(if (setq depth (nth 3 data))
							(if (/= (type depth) 'REAL)
								(setq depth "")
							)
						)
						;;draw polyline
						(if (> (strlen vectoresStr) 0)	;;pointList mainType depth insidePointName bClosed 
							(progn
								(AddBorder (read vectoresStr) typename Inside_Point depth bClosed)
								(setq i (1+ i))
							)
						)
					)
				)
			)
			(ADOLISP_DisconnectFromDB ConnectionObject)
		)
	)
	(princ (strcat "\n从数据库中读取了" (rtos i 2 0) "个边界。"))
)

;;把图中带”-“边界标志的点自动生成边界，并删除点的块
;;为了对5.6之前的资料进行处理
(defun C:CreateBorderFromEntity( /  allborders borderflag bordergroup borderpoints depth e entp firstname firstpoint 
 i iborder maintype namep npoint p10 s1 s2 vectors)
	(setq borderFlag (getstring "\n管线点的边界点以中心点号+边界标识符+顺序号表示，如DL101-1。\n请输入管线边界标志："))
	(setq i 0
		allBorders nil
	)
	(if(setq borderPoints (ssget "X" (list (cons 0 "INSERT")(list -3 (list gl_AppName)))))
		(progn
			;;0.找到所有含边界标志的点
			(repeat (setq nPoint (sslength borderPoints))
				(setq entp (ssname borderPoints i)
					namep (ldata-get entp "Map_No")
				      i (1+ i)
				)
				(if (vl-string-search borderFlag namep)
					(setq allBorders (cons (cons namep entp) allBorders))
				)
			)	
			;;1.根据点名排序
			(setq allBorders (vl-sort allBorders (function (lambda (s1 s2) (< (car s1) (car s2))))))
			;;2.按照"-"前面的点号分组
			(setq iBorder 0)
			(while allBorders
				(setq borderGroup nil		;;1个边界
					firstPoint (car allBorders)
					firstName (car firstPoint)
					borderGroup (cons firstPoint borderGroup)
					firstName (substr firstName 1 (vl-string-position (ascii borderFlag) firstName))
					allBorders (cdr allBorders)
				)
				(while (and allBorders (vl-string-search firstName (car (car allBorders))))
					(setq borderGroup (cons (car allBorders) borderGroup)
						allBorders (cdr allBorders)
					)
				)
				;;绘制边界
				(if (> (length bordergroup) 2)
					(progn
						(setq vectors nil
							 maintype (GetTypeFromPointName firstName)
							 i 0
						)
						(repeat (length borderGroup)
							(setq p10 (cdr (assoc 10 (entget (cdr (nth i borderGroup)))))
								vectors (append vectors p10)
								i (1+ i)
							)
						)
						(AddBorder vectors maintype depth firstName  T)
						(setq iBorder (1+ iBorder))
						;;删除边界点
						(foreach e borderGroup
							(DelEntity (cdr e))
							(if gl_MapNameList (vl-remove (car e) gl_MapNameList))
						)
					)
				)
			)
			(prompt (strcat "\n绘制了" (rtos  iBorder 2 0) "个边界多段线。并删除了所在的管线点。"))
		)
		(prompt "\n未发现管线点。")
	)
	(princ)
)


;;多段线转换为管线边界
;;二维多段线或者三维多段线
;;非闭合多段线，增加闭合属性
(defun C:PLtoBorder( / elist entpl insidept maintype npt ptlist tname i ptList3D)
	(prompt "\nPLtoBorder:多段线转为管线边界。")
	(if (setq entpl (entsel "\n选择多段线："))
		(progn 
			(setq entpl (car entpl)
				elist (entget entpl)
			)
			;;线段不是多段线
			(setq tname (cdr (assoc 0 elist)))
			(if (and (/= tname "LWPOLYLINE") (/= tname "POLYLINE"))
				(*error* "选择的对象不是多段线!")
			)
			;;线段已经是边界对象
			(if (vlax-ldata-get entpl "gl_AppName")
				(*error* "该对象已经是边界！")
			)
			;;线段是多段线，但节点数目小于3个，不能构成边界
			(setq ptList nil
				npt 0 i 0
				ptList (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates (vlax-ename->vla-object entpl))))
				ptList3D nil
			)	
			(cond 
				((= tname "LWPOLYLINE") 
					(progn
						(repeat (setq npt (/ (length ptList) 2))
							(setq ptList3D (append ptList3D (list(nth i ptList) (nth (+ 1 i) ptList) 0))
								i (+ 2 i)
							)
						)
						(setq ptList ptList3D)
					)
				)
				((= tname "POLYLINE") (setq npt (/ (length ptList) 3)))
			)			
			
			(if (< npt 3)
				(*error* "多段线节点个数少于3个，不能构成边界。")
			)
			;;选择内部点
			(if (setq insidePt (entsel "\n请选择边界内部的管线点："))
				(progn
					(setq insidePt (car insidePt))
					(if (setq mainType (ldata-get insidePt "Main_Type"))
						(progn
							;;转为边界对象；
							(AddBorder ptList mainType (ldata-get insidePt "Well_Deep") (ldata-get insidePt "Map_No") T)
							;;删除原有对象
							(entdel entpl)
							(prompt "\n多段线已经被变为边界！")
						)
						(*error* "选择的对象不是有效管线点！")
					)
				)
			)
		)
	)
	(princ)
)

(defun C:DelAllPointsInBorder ( / allpl npt i ent)
	(prompt "\n删除边界点的点实体。")
	(setq npt 0 i 0)
	(GetInsertRoot T)
	(if	(setq allpl (ssget "X" (list (cons 0 "POLYLINE") (list -3 (list gl_AppName)))))
		(repeat	(sslength allpl)
            (setq ent (ssname allpl i)
				i  (1+ i)
			)
			(setq npt (+ npt (DelPointsInBorder ent)))
		) ;_ end_repeat
	) ;_ end_if
	(prompt (strcat "\n 删除了" (rtos npt 2 0) "个边界点对象。"))
	(princ)
) 
;;;删除构成边界点实体
;;;pline POLYLINE ename
;;;
(defun DelPointsInBorder(entpl / enttype ptList dim i x0 y0 e eobj npt)
	(setq npt 0)
	(if (= (type entpl) 'ENAME)
		(progn
			(setq  enttype (cdr (assoc 0 (entget entpl)))
				ptList (vlax-safearray->list (vlax-variant-value (vla-get-Coordinates (vlax-ename->vla-object entpl))))
				dim 3
				i 0
			)
			(cond 
				((= "LWPOLYLINE" enttype) (setq dim 2))
				((= "POLYLINE" enttype) (setq dim 3))
				(T (setq dim 3))
			)
			(repeat (/ (length ptList) dim)
				(setq x0 (nth i ptList) 
					y0 (nth (+ i 1) ptList)
					i (+ i 3)
				)
				(foreach e (SearchPoint (GetInsertRoot nil) x0 y0)
					(setq eobj (vlax-ename->vla-object  (caddr e)))
					(if (not (vlax-erased-p  eobj))
						(progn
							(DelEntity (caddr e))
							(setq npt (1+ npt))
						)
					)
				)
			)
		)
	)
	npt
)































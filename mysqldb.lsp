;;;使用MySql作为数据库

 ;*********************************************************************************************
 ;函数定义:mysql-ConnectString()
 ;功能：连接到mysql数据库
;;用ADO方式利用一般都有的组件，access的，无需配置本地
 ;参数：
 ;返回：
 ;创建时间：2015/04/09   
 ;修改时间：
 ;创建人：沈雄君
 ;*********************************************************************************************
(defun mysql-ConnectString (user pwd server db / ConnectString)
    (if (not server)
        (setq server "rdsj12jvbsyqzqhu0avbd.mysql.rds.aliyuncs.com")
    ) ;_ end_if
    (if (not user)
        (setq user "admin_sen")
    ) ;_ end_if
    (if (not pwd)
        (setq pwd "lineinfo123")
    ) ;_ end_if
    (if (not db)
        (setq db "lineinfo")
    ) ;_ end_if

    (if (and user server pwd db)
        (setq ConnectString (strcat "Provider=MSDASQL.1;Persist Security Info=True;Extended Properties=\"Driver=MySQL ODBC 5.3 Unicode Driver"
                                    ";SERVER="                                        server                                            ";UID="
                                    user                                              ";PWD="                                           pwd
                                    ";DATABASE="                                      db                                                ";PORT=3306;\""
                                   ) ;_ end_strcat
        ) ;_ end_setq
        ;;esle
        (setq ConnectString
                 "Provider=MSDASQL.1;Persist Security Info=True;Extended Properties=\"Driver=MySQL ODBC 5.3 Unicode Driver;SERVER=rdsj12jvbsyqzqhu0avbd.mysql.rds.aliyuncs.com;UID=admin_sen;PWD=lineinfo123;DATABASE=lineinfo;PORT=3306;\""
        ) ;_ end_setq
    ) ;_ end_if
    ConnectString
) ;_ end_defun

(defun mysql-GetConnectionObject (cstr / ConnectionObject)
    (setq ConnectionObject nil)

    (if (not
            (setq ConnectionObject (ADOLISP_ConnectToDB cstr "" ""))
        ) ;_ end_not
        (progn
            (prompt "\nConnection failed!")
            (ADOLISP_ErrorPrinter)
        ) ;_ end_progn
    ) ;if
    ConnectionObject
) ;defun


(defun C:mysql-CreateUserTable (/ sqlstr cstr cobj)
    (setq sqlstr "CREATE TABLE Persons
					(
					Id_P int,
					LastName varchar(255),
					FirstName varchar(255),
					Address varchar(255),
					City varchar(255)
					)"
    ) ;_ end_setq
    (setq cstr (mysql-ConnectString nil nil nil nil))
    (if (setq cobj (mysql-GetConnectionObject cstr))
        (progn
            (ADOLISP_DoSQL cobj sqlstr)
            (ADOLISP_DisconnectFromDB cobj)
        ) ;_ end_progn
    ) ;_ end_if
) ;_ end_defun

;;用户管理


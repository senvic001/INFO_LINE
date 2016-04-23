;;;============={  超级getpoint函数 BY-wowan1314  }=======================;; 
;;功能：得到鼠标点击位置坐标,如果按键就返回按键值。
;;用法：(YY:getpoint '(NIL 5 0)) 
;;参数：表。 表内元素同GRREAD函数的参数意义.
;;返回值：PT或STR.  PT是左键点击时鼠标坐标点，STR是按键的值.
;;用途：尚不明确。 
(DEFUN YY:getpoint (LST / LOOP CODE)
 (setq loop T) 
 (while loop
  (setq code (APPLY 'grread LST))
  (cond
    ((= (car code) 5)     NIL )                                       ;;; 鼠标移动
    ((= (car code) 3)     (SETQ LOOP NIL CODE (CADR CODE)))           ;;; 鼠标左键
    ((= (car code) 11)    (SETQ LOOP NIL CODE NIL))                   ;;; 鼠标右键，右键设置为回车时
    ((= (car code) 25)    (SETQ LOOP NIL CODE NIL))                   ;;; 鼠标右键，右键设置为屏幕菜单时
    ((equal code '(2 0))  (SETQ LOOP NIL CODE "CTRL-@"))              ;;; CTRL-@
    ((equal code '(2 1))  (SETQ LOOP NIL CODE "CTRL-A"))              ;;; CTRL-A 
    ((equal code '(2 2))  (SETQ LOOP NIL CODE "CTRL-B或F9"))          ;;; CTRL-B或F9
    ((equal code '(2 3))  (SETQ LOOP NIL CODE "CTRL-C或F12"))         ;;; CTRL-C或F12
    ((equal code '(2 4))  (SETQ LOOP NIL CODE "CTRL-D或F6"))          ;;; CTRL-D或F6
    ((equal code '(2 5))  (SETQ LOOP NIL CODE "CTRL-E或F5"))          ;;; CTRL-E或F5
    ((equal code '(2 6))  (SETQ LOOP NIL CODE "CTRL-F或F3"))          ;;; CTRL-F或F3
    ((equal code '(2 7))  (SETQ LOOP NIL CODE "CTRL-G或F7"))          ;;; CTRL-G或F7
    ((equal code '(2 8))  (SETQ LOOP NIL CODE "CTRL-H或退格"))        ;;; CTRL-H或退格
    ((equal code '(2 9))  (SETQ LOOP NIL CODE "CTRL-I或TAB"))         ;;; CTRL-I或Tab
    ((equal code '(2 10)) (SETQ LOOP NIL CODE "CTRL-J"))              ;;; CTRL-J
    ((equal code '(2 11)) (SETQ LOOP NIL CODE "CTRL-K"))              ;;; CTRL-K
    ((equal code '(2 12)) (SETQ LOOP NIL CODE "CTRL-L"))              ;;; CTRL-L
    ((equal code '(2 13)) (SETQ LOOP NIL CODE "CTRL-M或回车"))        ;;; CTRL-M或回车
    ((equal code '(2 14)) (SETQ LOOP NIL CODE "CTRL-N"))              ;;; CTRL-N
    ((equal code '(2 15)) (SETQ LOOP NIL CODE "CTRL-O或F8"))          ;;; CTRL-O或F8
    ((equal code '(2 16)) (SETQ LOOP NIL CODE "CTRL-P"))              ;;; CTRL-P
    ((equal code '(2 17)) (SETQ LOOP NIL CODE "CTRL-Q"))              ;;; CTRL-Q
    ((equal code '(2 18)) (SETQ LOOP NIL CODE "CTRL-R"))              ;;; CTRL-R
    ((equal code '(2 19)) (SETQ LOOP NIL CODE "CTRL-S"))              ;;; CTRL-S
    ((equal code '(2 20)) (SETQ LOOP NIL CODE "CTRL-I或F4"))          ;;; CTRL-T或F4
    ((equal code '(2 21)) (SETQ LOOP NIL CODE "CTRL-U或F10"))         ;;; CTRL-U或F10
    ((equal code '(2 22)) (SETQ LOOP NIL CODE "CTRL-V"))              ;;; CTRL-V
    ((equal code '(2 23)) (SETQ LOOP NIL CODE "CTRL-W或F11"))         ;;; CTRL-W或F11
    ((equal code '(2 24)) (SETQ LOOP NIL CODE "CTRL-X"))              ;;; CTRL-X
    ((equal code '(2 25)) (SETQ LOOP NIL CODE "CTRL-Y"))              ;;; CTRL-Y
    ((equal code '(2 26)) (SETQ LOOP NIL CODE "CTRL-Z"))              ;;; CTRL-Z
    ((equal code '(2 27)) (SETQ LOOP NIL CODE "CTRL-[或ESC"))         ;;; CTRL-[或ESC
    ((equal code '(2 28)) (SETQ LOOP NIL CODE "CTRL-反斜杠"))         ;;; CTRL-\
    ((equal code '(2 29)) (SETQ LOOP NIL CODE "CTRL-]"))              ;;; CTRL-]
    ((equal code '(2 30)) (SETQ LOOP NIL CODE "CTRL-^"))              ;;; CTRL-^
    ((equal code '(2 31)) (SETQ LOOP NIL CODE "CTRL-_"))              ;;; CTRL-_
    ((equal code '(2 32)) (SETQ LOOP NIL CODE "空格"))                ;;; 空格键
    ((equal code '(2 33)) (SETQ LOOP NIL CODE "!"))                   ;;; !键
    ((equal code '(2 34)) (SETQ LOOP NIL CODE "双引号"))              ;;; "键
    ((equal code '(2 35)) (SETQ LOOP NIL CODE "#"))                   ;;; #键
    ((equal code '(2 36)) (SETQ LOOP NIL CODE "$"))                   ;;; $键
    ((equal code '(2 37)) (SETQ LOOP NIL CODE "%"))                    ;;; %键
    ((equal code '(2 38)) (SETQ LOOP NIL CODE "&"))                   ;;; &键
    ((equal code '(2 39)) (SETQ LOOP NIL CODE "单引号"))              ;;; '键
    ((equal code '(2 40)) (SETQ LOOP NIL CODE "("))                   ;;; (键
    ((equal code '(2 41)) (SETQ LOOP NIL CODE ")"))              ;;; )键
    ((equal code '(2 42)) (SETQ LOOP NIL CODE "星号"))                ;;; *键
    ((equal code '(2 43)) (SETQ LOOP NIL CODE "+"))                   ;;; +键
    ((equal code '(2 44)) (SETQ LOOP NIL CODE ","))                   ;;; ,键
    ((equal code '(2 45)) (SETQ LOOP NIL CODE "-"))                   ;;; -键
    ((equal code '(2 46)) (SETQ LOOP NIL CODE "点"))                  ;;; .键
    ((equal code '(2 47)) (SETQ LOOP NIL CODE "/"))                   ;;; /键
    ((equal code '(2 48)) (SETQ LOOP NIL CODE "0"))                   ;;; 0键
    ((equal code '(2 49)) (SETQ LOOP NIL CODE "1"))                   ;;; 1键
    ((equal code '(2 50)) (SETQ LOOP NIL CODE "2"))                   ;;; 2键
    ((equal code '(2 51)) (SETQ LOOP NIL CODE "3"))                   ;;; 3键
    ((equal code '(2 52)) (SETQ LOOP NIL CODE "4"))                   ;;; 4键
    ((equal code '(2 53)) (SETQ LOOP NIL CODE "5"))                   ;;; 5键
    ((equal code '(2 54)) (SETQ LOOP NIL CODE "6"))                   ;;; 6键
    ((equal code '(2 55)) (SETQ LOOP NIL CODE "7"))                   ;;; 7键
    ((equal code '(2 56)) (SETQ LOOP NIL CODE "8"))                   ;;; 8键
    ((equal code '(2 57)) (SETQ LOOP NIL CODE "9"))                   ;;; 9键
    ((equal code '(2 58)) (SETQ LOOP NIL CODE "冒号"))                ;;; :键
    ((equal code '(2 59)) (SETQ LOOP NIL CODE "分号"))                ;;; ;键
    ((equal code '(2 60)) (SETQ LOOP NIL CODE "小于号"))              ;;; <键
    ((equal code '(2 61)) (SETQ LOOP NIL CODE "等号"))                ;;; =键
    ((equal code '(2 62)) (SETQ LOOP NIL CODE "大于号"))              ;;; >键
    ((equal code '(2 63)) (SETQ LOOP NIL CODE "问号"))                ;;; ?键
    ((equal code '(2 64)) (SETQ LOOP NIL CODE "@"))                   ;;; @键
    ((equal code '(2 65)) (SETQ LOOP NIL CODE "A"))                   ;;; A键
    ((equal code '(2 66)) (SETQ LOOP NIL CODE "B"))                   ;;; B键
    ((equal code '(2 67)) (SETQ LOOP NIL CODE "C"))                   ;;; C键
    ((equal code '(2 68)) (SETQ LOOP NIL CODE "D"))                   ;;; D键
    ((equal code '(2 69)) (SETQ LOOP NIL CODE "E"))                   ;;; E键
    ((equal code '(2 70)) (SETQ LOOP NIL CODE "F"))                   ;;; F键
    ((equal code '(2 71)) (SETQ LOOP NIL CODE "G"))                   ;;; G键
    ((equal code '(2 72)) (SETQ LOOP NIL CODE "H"))                   ;;; H键
    ((equal code '(2 73)) (SETQ LOOP NIL CODE "I"))                   ;;; I键
    ((equal code '(2 74)) (SETQ LOOP NIL CODE "J"))                   ;;; J键
    ((equal code '(2 75)) (SETQ LOOP NIL CODE "K"))                   ;;; K键
    ((equal code '(2 76)) (SETQ LOOP NIL CODE "L"))                   ;;; L键
    ((equal code '(2 77)) (SETQ LOOP NIL CODE "M"))                   ;;; M键
    ((equal code '(2 78)) (SETQ LOOP NIL CODE "N"))                   ;;; N键
    ((equal code '(2 79)) (SETQ LOOP NIL CODE "O"))                   ;;; O键
    ((equal code '(2 80)) (SETQ LOOP NIL CODE "P"))                   ;;; P键
    ((equal code '(2 81)) (SETQ LOOP NIL CODE "Q"))                   ;;; Q键
    ((equal code '(2 82)) (SETQ LOOP NIL CODE "R"))                   ;;; R键
    ((equal code '(2 83)) (SETQ LOOP NIL CODE "S"))                   ;;; S键
    ((equal code '(2 84)) (SETQ LOOP NIL CODE "T"))                   ;;; T键
    ((equal code '(2 85)) (SETQ LOOP NIL CODE "U"))                   ;;; U键
    ((equal code '(2 86)) (SETQ LOOP NIL CODE "V"))                   ;;; V键
    ((equal code '(2 87)) (SETQ LOOP NIL CODE "W"))                   ;;; W键
    ((equal code '(2 88)) (SETQ LOOP NIL CODE "X"))                   ;;; X键
    ((equal code '(2 89)) (SETQ LOOP NIL CODE "Y"))                   ;;; Y键
    ((equal code '(2 90)) (SETQ LOOP NIL CODE "Z"))                   ;;; Z键
    ((equal code '(2 91)) (SETQ LOOP NIL CODE "["))                   ;;; [键
    ((equal code '(2 92)) (SETQ LOOP NIL CODE "反斜杠"))              ;;; \键
    ((equal code '(2 93)) (SETQ LOOP NIL CODE "]"))                ;;; ]键
    ((equal code '(2 94)) (SETQ LOOP NIL CODE "^"))                   ;;; ^键
    ((equal code '(2 95)) (SETQ LOOP NIL CODE "_"))                   ;;; _键
    ((equal code '(2 96)) (SETQ LOOP NIL CODE "单引号"))              ;;; `键
    ((equal code '(2 97)) (SETQ LOOP NIL CODE "a"))                   ;;; a键
    ((equal code '(2 98)) (SETQ LOOP NIL CODE "b"))                   ;;; b键
    ((equal code '(2 99)) (SETQ LOOP NIL CODE "c"))                   ;;; c键
    ((equal code '(2 100))(SETQ LOOP NIL CODE "d"))                   ;;; d键
    ((equal code '(2 101))(SETQ LOOP NIL CODE "e"))                   ;;; e键
    ((equal code '(2 102))(SETQ LOOP NIL CODE "f"))                   ;;; f键
    ((equal code '(2 103))(SETQ LOOP NIL CODE "g"))                   ;;; g键
    ((equal code '(2 104))(SETQ LOOP NIL CODE "h"))                   ;;; h键
    ((equal code '(2 105))(SETQ LOOP NIL CODE "i"))                   ;;; i键
    ((equal code '(2 106))(SETQ LOOP NIL CODE "j"))                   ;;; j键
    ((equal code '(2 107))(SETQ LOOP NIL CODE "k"))                   ;;; k键
    ((equal code '(2 108))(SETQ LOOP NIL CODE "l"))                   ;;; l键
    ((equal code '(2 109))(SETQ LOOP NIL CODE "m"))                   ;;; m键
    ((equal code '(2 110))(SETQ LOOP NIL CODE "n"))                   ;;; n键
    ((equal code '(2 111))(SETQ LOOP NIL CODE "o"))                   ;;; o键
    ((equal code '(2 112))(SETQ LOOP NIL CODE "p"))                   ;;; p键
    ((equal code '(2 113))(SETQ LOOP NIL CODE "q"))                   ;;; q键
    ((equal code '(2 114))(SETQ LOOP NIL CODE "r"))                   ;;; r键
    ((equal code '(2 115))(SETQ LOOP NIL CODE "s"))                   ;;; s键
    ((equal code '(2 116))(SETQ LOOP NIL CODE "t"))                   ;;; t键
    ((equal code '(2 117))(SETQ LOOP NIL CODE "u"))                   ;;; u键
    ((equal code '(2 118))(SETQ LOOP NIL CODE "v"))                   ;;; v键
    ((equal code '(2 119))(SETQ LOOP NIL CODE "w"))                   ;;; w键
    ((equal code '(2 120))(SETQ LOOP NIL CODE "x"))                   ;;; x键
    ((equal code '(2 121))(SETQ LOOP NIL CODE "y"))                   ;;; y键
    ((equal code '(2 122))(SETQ LOOP NIL CODE "z"))                   ;;; z键
    ((equal code '(2 123))(SETQ LOOP NIL CODE "大开括号"))            ;;; {键
    ((equal code '(2 124))(SETQ LOOP NIL CODE "|"))                ;;; |键
    ((equal code '(2 125))(SETQ LOOP NIL CODE "大闭括号"))            ;;; }键
    ((equal code '(2 126))(SETQ LOOP NIL CODE "~"))                   ;;; ~键
    ((equal code '(2 127))(SETQ LOOP NIL CODE "删除键"))              ;;; Delete键
  )
 )
 CODE
)      ;;;END DEFUN




(setq loop T) 
(while loop
  (setq code (grread T 8))
  (cond
    ((= (car code) 5)     (do_Move))                ;;; 鼠标移动
    ((= (car code) 3)     (do_Left))                ;;; 鼠标左键
    ((= (car code) 11)    (do_Right))               ;;; 鼠标右键，右键设置为回车时
    ((= (car code) 25)    (do_Right))               ;;; 鼠标右键，右键设置为屏幕菜单时
    ((equal code '(2 0))  (do_CTRL-@))              ;;; CTRL-@
    ((equal code '(2 1))  (do_CTRL-A))              ;;; CTRL-A 
    ((equal code '(2 2))  (do_F9))                  ;;; CTRL-B或F9
    ((equal code '(2 3))  (do_F12))                 ;;; CTRL-C或F12
    ((equal code '(2 4))  (do_F6))                  ;;; CTRL-D或F6
    ((equal code '(2 5))  (do_F5))                  ;;; CTRL-E或F5
    ((equal code '(2 6))  (do_F3))                  ;;; CTRL-F或F3
    ((equal code '(2 7))  (do_F7))                  ;;; CTRL-G或F7
    ((equal code '(2 8))  (do_Back))                ;;; CTRL-H或退格
    ((equal code '(2 9))  (do_Tab))                 ;;; CTRL-I或Tab
    ((equal code '(2 10)) (do_CTRL-J))              ;;; CTRL-J
    ((equal code '(2 11)) (do_CTRL-K))              ;;; CTRL-K
    ((equal code '(2 12)) (do_CTRL-L))              ;;; CTRL-L
    ((equal code '(2 13)) (do_Return))              ;;; CTRL-M或回车
    ((equal code '(2 14)) (do_CTRL-N))              ;;; CTRL-N
    ((equal code '(2 15)) (do_F8))                  ;;; CTRL-O或F8
    ((equal code '(2 16)) (do_CTRL-P))              ;;; CTRL-P
    ((equal code '(2 17)) (do_CTRL-Q))              ;;; CTRL-Q
    ((equal code '(2 18)) (do_CTRL-R))              ;;; CTRL-R
    ((equal code '(2 19)) (do_CTRL-S))              ;;; CTRL-S
    ((equal code '(2 20)) (do_F4))                  ;;; CTRL-T或F4
    ((equal code '(2 21)) (do_F10))                 ;;; CTRL-U或F10
    ((equal code '(2 22)) (do_CTRL-V))              ;;; CTRL-V
    ((equal code '(2 23)) (do_F11))                 ;;; CTRL-W或F11
    ((equal code '(2 24)) (do_CTRL-X))              ;;; CTRL-X
    ((equal code '(2 25)) (do_CTRL-Y))              ;;; CTRL-Y
    ((equal code '(2 26)) (do_CTRL-Z))              ;;; CTRL-Z
    ((equal code '(2 27)) (do_CTRL-[))              ;;; CTRL-[或ESC
    ((equal code '(2 28)) (do_CTRL-\))              ;;; CTRL-\
    ((equal code '(2 29)) (do_CTRL-]))              ;;; CTRL-]
    ((equal code '(2 30)) (do_CTRL-^))              ;;; CTRL-^
    ((equal code '(2 31)) (do_CTRL-_))              ;;; CTRL-_
    ((equal code '(2 32)) (do_Space))               ;;; 空格键
    ((equal code '(2 33)) (do_ExclamationMark))     ;;; !键
    ((equal code '(2 34)) (do_DoubleQuote))         ;;; "键
    ((equal code '(2 35)) (do_Hash))                ;;; #键
    ((equal code '(2 36)) (do_Dollar))              ;;; $键
    ((equal code '(2 37)) (do_Percent))             ;;; %键
    ((equal code '(2 38)) (do_Ampersand))           ;;; &键
    ((equal code '(2 39)) (do_Apostrophe))          ;;; '键
    ((equal code '(2 40)) (do_OpenParenthesis))     ;;; (键
    ((equal code '(2 41)) (do_CloseParenthesis))    ;;; )键
    ((equal code '(2 42)) (do_Asterisk))            ;;; *键
    ((equal code '(2 43)) (do_Plus))                ;;; +键
    ((equal code '(2 44)) (do_Comma))               ;;; ,键
    ((equal code '(2 45)) (do_Minus))               ;;; -键
    ((equal code '(2 46)) (do_Dot))                 ;;; .键
    ((equal code '(2 47)) (do_Slash))               ;;; /键
    ((equal code '(2 48)) (do_0))                   ;;; 0键
    ((equal code '(2 49)) (do_1))                   ;;; 1键
    ((equal code '(2 50)) (do_2))                   ;;; 2键
    ((equal code '(2 51)) (do_3))                   ;;; 3键
    ((equal code '(2 52)) (do_4))                   ;;; 4键
    ((equal code '(2 53)) (do_5))                   ;;; 5键
    ((equal code '(2 54)) (do_6))                   ;;; 6键
    ((equal code '(2 55)) (do_7))                   ;;; 7键
    ((equal code '(2 56)) (do_8))                   ;;; 8键
    ((equal code '(2 57)) (do_9))                   ;;; 9键
    ((equal code '(2 58)) (do_Colon))               ;;; :键
    ((equal code '(2 59)) (do_Semicolon))           ;;; ;键
    ((equal code '(2 60)) (do_LessThan))            ;;; <键
    ((equal code '(2 61)) (do_Equals))              ;;; =键
    ((equal code '(2 62)) (do_GreatThan))           ;;; >键
    ((equal code '(2 63)) (do_QuestionMark))        ;;; ?键
    ((equal code '(2 64)) (do_At))                  ;;; @键
    ((equal code '(2 65)) (do_A))                   ;;; A键
    ((equal code '(2 66)) (do_B))                   ;;; B键
    ((equal code '(2 67)) (do_C))                   ;;; C键
    ((equal code '(2 68)) (do_D))                   ;;; D键
    ((equal code '(2 69)) (do_E))                   ;;; E键
    ((equal code '(2 70)) (do_F))                   ;;; F键
    ((equal code '(2 71)) (do_G))                   ;;; G键
    ((equal code '(2 72)) (do_H))                   ;;; H键
    ((equal code '(2 73)) (do_I))                   ;;; I键
    ((equal code '(2 74)) (do_J))                   ;;; J键
    ((equal code '(2 75)) (do_K))                   ;;; K键
    ((equal code '(2 76)) (do_L))                   ;;; L键
    ((equal code '(2 77)) (do_M))                   ;;; M键
    ((equal code '(2 78)) (do_N))                   ;;; N键
    ((equal code '(2 79)) (do_O))                   ;;; O键
    ((equal code '(2 80)) (do_P))                   ;;; P键
    ((equal code '(2 81)) (do_Q))                   ;;; Q键
    ((equal code '(2 82)) (do_R))                   ;;; R键
    ((equal code '(2 83)) (do_S))                   ;;; S键
    ((equal code '(2 84)) (do_T))                   ;;; T键
    ((equal code '(2 85)) (do_U))                   ;;; U键
    ((equal code '(2 86)) (do_V))                   ;;; V键
    ((equal code '(2 87)) (do_W))                   ;;; W键
    ((equal code '(2 88)) (do_X))                   ;;; X键
    ((equal code '(2 89)) (do_Y))                   ;;; Y键
    ((equal code '(2 90)) (do_Z))                   ;;; Z键
    ((equal code '(2 91)) (do_OpenSquareBracket))   ;;; [键
    ((equal code '(2 92)) (do_BackSlash))           ;;; \键
    ((equal code '(2 93)) (do_CloseSquareBracket))  ;;; ]键
    ((equal code '(2 94)) (do_Caret))               ;;; ^键
    ((equal code '(2 95)) (do_UnderScore))          ;;; _键
    ((equal code '(2 96)) (do_BackQuote))           ;;; `键
    ((equal code '(2 97)) (do_a))                   ;;; a键
    ((equal code '(2 98)) (do_b))                   ;;; b键
    ((equal code '(2 99)) (do_c))                   ;;; c键
    ((equal code '(2 100))(do_d))                   ;;; d键
    ((equal code '(2 101))(do_e))                   ;;; e键
    ((equal code '(2 102))(do_f))                   ;;; f键
    ((equal code '(2 103))(do_g))                   ;;; g键
    ((equal code '(2 104))(do_h))                   ;;; h键
    ((equal code '(2 105))(do_i))                   ;;; i键
    ((equal code '(2 106))(do_j))                   ;;; j键
    ((equal code '(2 107))(do_k))                   ;;; k键
    ((equal code '(2 108))(do_l))                   ;;; l键
    ((equal code '(2 109))(do_m))                   ;;; m键
    ((equal code '(2 110))(do_n))                   ;;; n键
    ((equal code '(2 111))(do_o))                   ;;; o键
    ((equal code '(2 112))(do_p))                   ;;; p键
    ((equal code '(2 113))(do_q))                   ;;; q键
    ((equal code '(2 114))(do_r))                   ;;; r键
    ((equal code '(2 115))(do_s))                   ;;; s键
    ((equal code '(2 116))(do_t))                   ;;; t键
    ((equal code '(2 117))(do_u))                   ;;; u键
    ((equal code '(2 118))(do_v))                   ;;; v键
    ((equal code '(2 119))(do_w))                   ;;; w键
    ((equal code '(2 120))(do_x))                   ;;; x键
    ((equal code '(2 121))(do_y))                   ;;; y键
    ((equal code '(2 122))(do_z))                   ;;; z键
    ((equal code '(2 123))(do_OpenBrace))           ;;; {键
    ((equal code '(2 124))(do_VerticalBar))         ;;; |键
    ((equal code '(2 125))(do_CloseBrace))          ;;; }键
    ((equal code '(2 126))(do_Tilde))               ;;; ~键
    ((equal code '(2 127))(do_Delete))              ;;; Delete键
  )
)
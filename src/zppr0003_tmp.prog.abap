************************************************************************
* Program Name      : ZPPR0003_TMP
* Descriptions      : 途程清單
* Updates Tables    :
* Input  Parameters :
* Output Parameters :
* Return Codes      :
* Special Logic     :
* Includes          :
************************************************************************
* Modification Log
************************************************************************
*   Date     Ver. Programmer   Descriptions
* ---------- ---- ------------ -----------------------------------------
* 20230404   1.0  Derek Huang
************************************************************************
REPORT ZPPR0003_TMP NO STANDARD PAGE HEADING
                   MESSAGE-ID 00 LINE-SIZE 80 LINE-COUNT 65.
************************************************************************
* Tables Definitions
************************************************************************
TABLES: CYPLAF,MARA,MAKT,KBED,PLPO,PLAS,CYCRHD,CRTX,CRCA,KAKO,ITOB,
        RC65D,CRHD,MARC,SCAL,MBEW,MAPL,PLKO,PLFH,CRVE_A,PLFL,T435T .
************************************************************************
* Data Definitions
************************************************************************

DATA: BEGIN OF T_MAPL OCCURS 0,
        MATNR LIKE MAPL-MATNR,
        WERKS LIKE MAPL-WERKS,
        PLNTY LIKE MAPL-PLNTY,
        PLNNR LIKE MAPL-PLNNR,
        PLNAL LIKE MAPL-PLNAL,
        ANDAT LIKE MAPL-ANDAT,
        AEDAT LIKE MAPL-AEDAT,

      END OF T_MAPL.

DATA: BEGIN OF T_DATA OCCURS 0,

        WERKS   LIKE  MAPL-WERKS,      "廠別
        BISMT   LIKE MARA-BISMT, "舊物料號碼
        MATNR   LIKE  MAPL-MATNR,      "料號
        MAKTX   LIKE MAKT-MAKTX,       "料號說明
        VERWE   LIKE PLKO-VERWE,
        STATU   LIKE PLKO-STATU,     "途程狀態
        LOSVN   LIKE PLKO-LOSVN,
        LOSBS   LIKE PLKO-LOSBS,
        PLNME   LIKE PLKO-PLNME,
        VORNR   LIKE  PLPO-VORNR,      "作業
        ARBPL   LIKE  CRHD-ARBPL,      "工作中心
        STEUS   LIKE PLPO-STEUS , "控制#
        KTSCH   LIKE PLPO-KTSCH , "標準內文碼
        LTXA1   LIKE  PLPO-LTXA1,      "作業說明
        BMSCH   LIKE  PLPO-BMSCH,      "基礎數量
        MEINH   LIKE  PLPO-MEINH,      "作業單位
        UMREZ   LIKE PLPO-UMREZ,       "比例-表頭單位數
        UMREN   LIKE PLPO-UMREN,        "比例-作業單位數
        VGW01   LIKE  PLPO-VGW01,           "標準值1
        VGE01   LIKE  PLPO-VGE01,           "標準值單位1
        VGW02   LIKE  PLPO-VGW02,           "標準值2
        VGE02   LIKE  PLPO-VGE02,           "標準值單位2
        VGW03   LIKE  PLPO-VGW03,           "標準值3
        VGE03   LIKE  PLPO-VGE03,           "標準值單位3
        VGW04   LIKE  PLPO-VGW04,           "標準值4
        VGE04   LIKE  PLPO-VGE04,           "標準值單位4
        FRDLB   LIKE PLPO-FRDLB,
        INFNR   LIKE PLPO-INFNR,
        EKORG	  LIKE PLPO-EKORG,
        EKGRP	  LIKE PLPO-EKGRP,
        MATKL	  LIKE PLPO-MATKL,
        PEINH	  LIKE PLPO-PEINH,
        PREIS   LIKE PLPO-PREIS,
        WAERS	  LIKE PLPO-WAERS,
        SAKTO   LIKE PLPO-SAKTO,


        PLNTY   LIKE MAPL-PLNTY,       "工作細項清單類型
        PLNNR   LIKE  MAPL-PLNNR,      "途程群組
        PLNAL   LIKE  MAPL-PLNAL,      "群組計數器
        ANDAT   LIKE MAPL-ANDAT,       "建立日期
        AEDAT   LIKE MAPL-AEDAT,      "更改日期
        KTEXT   LIKE  PLKO-KTEXT,      "途程短文
        KTEXT1  LIKE  CRTX-KTEXT,      "工作中心說明
        TXT     LIKE T435T-TXT, "標準內文碼
        LAR01   LIKE  PLPO-LAR01,           "作業類型1

        LAR02   LIKE  PLPO-LAR02,           "作業類型2

        LAR03   LIKE  PLPO-LAR03,           "作業類型3

        LAR04   LIKE  PLPO-LAR04,           "作業類型4

        LAR05   LIKE  PLPO-LAR05,           "作業類型5
        VGE05   LIKE  PLPO-VGE05,           "標準值單位5
        VGW05   LIKE  PLPO-VGW05,           "標準值5
        LAR06   LIKE  PLPO-LAR06,            "作業類型6
        VGE06   LIKE  PLPO-VGE06,           "標準值單位6
        VGW06   LIKE  PLPO-VGW06,            "標準值6
        DELKZ   LIKE PLKO-DELKZ, "途程#除##
        LVORM   LIKE MARC-LVORM, "工厂##除##
        LIFNR   LIKE PLPO-LIFNR,


        PLNFL   LIKE PLAS-PLNFL, "順序
        FLGAT   LIKE PLFL-FLGAT, "順序類別
        SEL     TYPE CHAR1,


        MEINS   LIKE  MARA-MEINS, "料號主檔單位
        UMREZ_M LIKE MARM-UMREZ,  "主檔轉換比例
        UMREN_M LIKE MARM-UMREN,  "主檔基礎數量比例

      END OF T_DATA.

*data : T_DATA  like T_CYPLAF occurs 0 with header line.

** ALV Report
TYPE-POOLS: SLIS.
* 宣告ALV-SLIS
DATA:
  GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
  GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
  GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
  GT_SORT     TYPE SLIS_T_SORTINFO_ALV.


************************************************************************
* Includes Module
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK COMM1 WITH FRAME TITLE TEXT-000.
  PARAMETERS : P_PLNTY LIKE MAPL-PLNTY DEFAULT 'N' OBLIGATORY. "途程類型
  SELECT-OPTIONS:  S_WERKS  FOR MAPL-WERKS.  "工廠
  SELECT-OPTIONS:  S_MATNR  FOR MAPL-MATNR.  "料號
  SELECT-OPTIONS:  S_BISMT  FOR MARA-BISMT.  "舊料號
  SELECT-OPTIONS:  S_MMSTA  FOR MARC-MMSTA.
  SELECT-OPTIONS:  S_ARBPL FOR CRHD-ARBPL. "工作中心
  SELECT-OPTIONS:  S_VERWE FOR CRHD-VERWE.
  SELECT-OPTIONS:  S_ANDAT  FOR MAPL-ANDAT.  "建立日期
  SELECT-OPTIONS:  S_PLNNR FOR MAPL-PLNNR.   "途程群組
  SELECT-OPTIONS:  S_PLNAL FOR MAPL-PLNAL.   "群組計數器
  PARAMETERS :     P_DELKZ LIKE PLKO-DELKZ AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK COMM1.


************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* At Selection Screen Output
************************************************************************

************************************************************************
* At Selection Screen
************************************************************************
AT SELECTION-SCREEN.

AT USER-COMMAND.

AT LINE-SELECTION.

************************************************************************
* Report Format
************************************************************************
TOP-OF-PAGE.


END-OF-PAGE.

************************************************************************
* Main Process
************************************************************************
START-OF-SELECTION.

  PERFORM GET_DATA.
  PERFORM WRITE_DATA.


END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
FORM WRITE_DATA.
  PERFORM BUILD_LAYOUT.
  PERFORM BUILD_FIELDCAT.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-CPROG           "此支程式的名稱
      IS_LAYOUT                = GS_LAYOUT          "ALV呈現相關參數
      IT_FIELDCAT              = GT_FIELDCAT        "ALV欄位呈現相關參數
      IT_SORT                  = GT_SORT "AVL欄位相同資料不顯示
      I_SAVE                   = 'A'                "固定為A
*     I_CALLBACK_PF_STATUS_SET = 'STANDARD_FULLSCREEN'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
    TABLES
      T_OUTTAB                 = T_DATA[]          "呈現資料的Table
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      I_CALLBACK_PROGRAM       = SY-CPROG
*      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*      I_SAVE                   = 'A'
*      IS_LAYOUT                = GS_LAYOUT
*      IT_FIELDCAT              = GT_FIELDCAT
*      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
*      "IT_SORT                  = GT_SORT
*    TABLES
*      T_OUTTAB                 = T_DATA
*    EXCEPTIONS
*      PROGRAM_ERROR            = 1
*      OTHERS                   = 2.

ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB..

  SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB .

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  download_file
*&---------------------------------------------------------------------*
FORM DOWNLOAD_FILE.

ENDFORM.                    " download_file
*&---------------------------------------------------------------------*
*&      Form  geT_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: W_INDEX    LIKE SY-TABIX,W_PLNAL(2).
  DATA:   W_SWEEK   LIKE SCAL-WEEK.
  DATA:   W_EWEEK   LIKE SCAL-WEEK.
  DATA:   W_WEEK   LIKE SCAL-WEEK.
  DATA:   W_WEEKS   TYPE N.
  DATA:   W_DATE   LIKE CYPLAF-PSTTR.
  DATA: BEGIN OF I_TAB OCCURS 0 ,
          PLNNR LIKE PLFH-PLNNR,
          PLNAL LIKE PLFH-PLNAL,
          OBJID LIKE PLFH-OBJID,
        END OF I_TAB.

*--> 依條件取得相關途程
  SELECT MAPL~MATNR MAPL~WERKS MAPL~PLNTY
         MAPL~PLNNR MAPL~PLNAL MAPL~ANDAT MAPL~AEDAT
      INTO CORRESPONDING FIELDS OF TABLE T_MAPL
      FROM MAPL INNER JOIN MARA
      ON MAPL~MATNR EQ MARA~MATNR
      INNER JOIN MARC ON MAPL~MATNR EQ MARC~MATNR
                          AND MAPL~WERKS EQ MARC~WERKS
      WHERE MARA~BISMT IN S_BISMT
        AND MAPL~MATNR IN S_MATNR
        AND MAPL~WERKS IN S_WERKS
        AND MAPL~PLNNR IN S_PLNNR
        AND MAPL~PLNAL IN S_PLNAL
        AND MAPL~PLNTY = 'N'         "途程
        AND MAPL~LOEKZ <> 'X'
        AND MARC~MMSTA IN S_MMSTA.
*        AND ANDAT IN S_ANDAT.


  LOOP AT T_MAPL.

    CLEAR PLKO.

    IF P_DELKZ = 'X'. "顯示已刪除的途程
      SELECT SINGLE * FROM PLKO
              WHERE PLNTY = T_MAPL-PLNTY
                AND PLNNR = T_MAPL-PLNNR
                 AND PLNAL = T_MAPL-PLNAL
                 AND LOEKZ <> 'X'
                 AND ANDAT IN S_ANDAT.
    ELSE."不顯示
      SELECT SINGLE * FROM PLKO
              WHERE PLNTY = T_MAPL-PLNTY
                AND PLNNR = T_MAPL-PLNNR
                 AND PLNAL = T_MAPL-PLNAL
                 AND LOEKZ <> 'X'
                 AND ANDAT IN S_ANDAT
                 AND DELKZ <> 'X'.
    ENDIF.

    IF SY-SUBRC = 0.
      SELECT * FROM PLAS
        WHERE PLNTY = PLKO-PLNTY
          AND PLNNR = PLKO-PLNNR
          AND PLNAL = PLKO-PLNAL
          AND LOEKZ <> 'X'.

        SELECT SINGLE * FROM PLFL
                      WHERE PLNTY = PLAS-PLNTY
                            AND PLNNR = PLAS-PLNNR
                            AND PLNAL = PLAS-PLNAL
                            AND PLNFL = PLAS-PLNFL
                            AND LOEKZ <> 'X'.

        IF SY-SUBRC <> 0.
          CONTINUE.
        ENDIF.

        "取得作業資料
        SELECT SINGLE * FROM PLPO
        WHERE PLNTY = PLAS-PLNTY
          AND PLNNR = PLAS-PLNNR
          AND PLNKN = PLAS-PLNKN
          AND ZAEHL = PLAS-ZAEHL
          AND LOEKZ <> 'X'.

       "取得工作中心資料
        SELECT SINGLE * FROM CRHD
                WHERE OBJID = PLPO-ARBID
                  AND OBJTY = 'A'.

        CHECK CRHD-ARBPL IN S_ARBPL  .
        CHECK CRHD-VERWE IN S_VERWE.

        SELECT SINGLE * FROM CRTX
         WHERE OBJID = CRHD-OBJID
           AND SPRAS = SY-LANGU.

        IF SY-SUBRC <> 0.
          SELECT SINGLE * FROM CRTX
                         WHERE OBJID = CRHD-OBJID.
        ENDIF.

        CLEAR MAKT.
        SELECT SINGLE * FROM MAKT
                       WHERE MATNR = T_MAPL-MATNR
                         AND SPRAS = SY-LANGU.

        IF SY-SUBRC <> 0.
          SELECT SINGLE * FROM MAKT
              WHERE MATNR = T_MAPL-MATNR.
        ENDIF.


*        CLEAR  PLFH.CLEAR CRVE_A.
*
*        SELECT SINGLE * FROM PLFH
*                       WHERE PLNTY = PLAS-PLNTY
*                         AND PLNNR = PLAS-PLNNR
*                         AND PLNKN = PLAS-PLNKN
*                         AND LOEKZ <> 'X'
*                         AND PLNAL = PLKO-PLNAL
*                         AND  PSNFH IN (
*                             SELECT MIN( PSNFH ) FROM  PLFH
*                                  WHERE PLNTY = PLAS-PLNTY
*                                    AND PLNNR = PLAS-PLNNR
*                                    AND PLNKN = PLAS-PLNKN
*                                    AND LOEKZ <> 'X'
*                                    AND   PLNAL = PLKO-PLNAL ).
*
*        SELECT SINGLE * FROM  CRVE_A
*                       WHERE OBJID = PLFH-OBJID
*                         AND OBJTY = 'FH'.

        CLEAR : MARA,MARC.
        SELECT SINGLE * FROM MARA WHERE  MATNR =  T_MAPL-MATNR.


        CLEAR : MARC,T435T.
        SELECT SINGLE * FROM T435T
                       WHERE VLSCH = PLPO-KTSCH
                         AND SPRAS = SY-LANGU.

        T_DATA-MATNR = T_MAPL-MATNR.      "料號
        T_DATA-MAKTX = MAKT-MAKTX.
        T_DATA-WERKS = T_MAPL-WERKS.      "廠別
        T_DATA-PLNTY = T_MAPL-PLNTY.      "途程類型
        T_DATA-PLNNR = T_MAPL-PLNNR.      "途程群組
        T_DATA-PLNAL = T_MAPL-PLNAL.     "群組計數器
        T_DATA-STATU = PLKO-STATU.
        T_DATA-VERWE = PLKO-VERWE.
        T_DATA-ANDAT = PLKO-ANDAT.  " 建立日期
        T_DATA-AEDAT = PLKO-AEDAT.  " 建立日期
        T_DATA-KTEXT = PLKO-KTEXT.      "途程短文
        T_DATA-ARBPL = CRHD-ARBPL.      "工作中心
        T_DATA-KTEXT1 = CRTX-KTEXT.    "工作中心說明
        T_DATA-VORNR = PLPO-VORNR.      "作業
        T_DATA-LTXA1 = PLPO-LTXA1.      "作業說明
        T_DATA-KTSCH  = PLPO-KTSCH . "標準內文碼
        T_DATA-TXT = T435T-TXT. "標準內文碼說明
        T_DATA-BMSCH = PLPO-BMSCH.      "基礎數量
        T_DATA-LAR01 = PLPO-LAR01.       "作業類型1
        T_DATA-VGE01 = PLPO-VGE01.        "標準值單位1
        T_DATA-VGW01 = PLPO-VGW01.           "標準值1
        T_DATA-LAR02 = PLPO-LAR02.       "作業類型2
        T_DATA-VGE02 = PLPO-VGE02.        "標準值單位2
        T_DATA-VGW02 = PLPO-VGW02.       "標準值2
        T_DATA-LAR03 = PLPO-LAR03.            "作業類型3
        T_DATA-VGE03 = PLPO-VGE03.          "標準值單位3
        T_DATA-VGW03 = PLPO-VGW03.            "標準值3
        T_DATA-LAR04 = PLPO-LAR04.               "作業類型4
        T_DATA-VGE04 = PLPO-VGE04.             "標準值單位4
        T_DATA-VGW04 = PLPO-VGW04.         "標準值4
        T_DATA-LAR05 = PLPO-LAR05.            "作業類型5
        T_DATA-VGE05 = PLPO-VGE05.         "標準值單位5
        T_DATA-VGW05 = PLPO-VGW05.           "標準值5
        T_DATA-LAR06 = PLPO-LAR06.       "作業類型6
        T_DATA-VGE06 = PLPO-VGE06.            "標準值單位6
        T_DATA-VGW06 = PLPO-VGW06.        "標準值6

        T_DATA-LOSVN = PLKO-LOSVN.
        T_DATA-LOSBS = PLKO-LOSBS.

        T_DATA-UMREZ = PLPO-UMREZ. "表頭數量
        T_DATA-UMREN = PLPO-UMREN.  "作業數量

        T_DATA-STEUS = PLPO-STEUS. "控制#
        T_DATA-PLNME = PLKO-PLNME.
        T_DATA-DELKZ = PLKO-DELKZ."途程#除##
        T_DATA-LVORM = MARC-LVORM.
        T_DATA-MEINH = PLPO-MEINH.  "作業單位
        T_DATA-MEINS = MARA-MEINS. "料號主檔單位
        T_DATA-PLNFL = PLAS-PLNFL.   "順序
        T_DATA-FLGAT =  PLFL-FLGAT. "順序類別
        T_DATA-BISMT = MARA-BISMT.


        T_DATA-EKORG = PLPO-EKORG.
        T_DATA-EKGRP = PLPO-EKGRP.
        T_DATA-MATKL = PLPO-MATKL.
        T_DATA-PEINH = PLPO-PEINH.
        T_DATA-PREIS = PLPO-PREIS.
        T_DATA-WAERS = PLPO-WAERS.
        T_DATA-SAKTO = PLPO-SAKTO.
        T_DATA-LIFNR = PLPO-LIFNR.

        "料號主檔轉換比例
        SELECT SINGLE UMREZ UMREN
                 INTO ( T_DATA-UMREZ_M , T_DATA-UMREN_M )
                 FROM MARM
                WHERE MATNR = T_DATA-MATNR
                  AND MEINH = T_DATA-MEINH.


        APPEND T_DATA. CLEAR T_DATA.

        CLEAR : PLAS,PLPO,CRHD,CRTX,PLFL.

      ENDSELECT.
    ENDIF.
  ENDLOOP.


  IF T_DATA[] IS INITIAL.
    MESSAGE S001 WITH '未取得條件內資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.


ENDFORM.                    " geT_DATA

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  DATA:LS_SORT     TYPE SLIS_SORTINFO_ALV.

  LS_SORT-FIELDNAME = 'MATNR'.
  APPEND LS_SORT TO GT_SORT.

  LS_SORT-FIELDNAME = 'PLNNR'.
  APPEND LS_SORT TO GT_SORT.

  LS_SORT-FIELDNAME = 'PLNAL'.
  APPEND LS_SORT TO GT_SORT.

  LS_SORT-FIELDNAME = 'VORNR'.
  APPEND LS_SORT TO GT_SORT.

  GS_LAYOUT-ZEBRA = 'X'.                    "顏色交替檢視
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.      "自動最佳化長度
  GS_LAYOUT-BOX_FIELDNAME = 'SEL'.

ENDFORM.                    " build_layout

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT .
  REFRESH: GT_FIELDCAT.


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-CPROG     "程式名稱
      I_INTERNAL_TABNAME     = 'T_DATA'  "
      I_INCLNAME             = SY-CPROG   "程式名稱
      I_CLIENT_NEVER_DISPLAY = 'X'        "固定X
      I_BYPASSING_BUFFER     = 'X'        "固定X
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT "
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  DATA: L_INDEX  TYPE I,
        L_DELETE TYPE C.

*--> 調整欄位說明
  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    L_INDEX = SY-TABIX.

    CLEAR L_DELETE.

    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'WERKS'.
      WHEN 'MATNR'.
      WHEN 'MAKTX'.
      WHEN 'VERWE'.
      WHEN 'STATU'.
      WHEN 'LOSVN'.
      WHEN 'LOSBS'.
      WHEN 'PLNME'.
      WHEN 'VORNR'.
      WHEN 'ARBPL'.
      WHEN 'STEUS'.
      WHEN 'KTSCH'.
      WHEN 'LTXA1'.

      WHEN 'UMREZ'.
        PERFORM MODIFY_ALV_FIELD USING '表頭數量'
                       CHANGING GS_FIELDCAT.

      WHEN 'UMREN'.
        PERFORM MODIFY_ALV_FIELD USING '作業轉換數量'
                       CHANGING GS_FIELDCAT.




      WHEN 'VGW01'.
        PERFORM MODIFY_ALV_FIELD USING '整備'
                                 CHANGING GS_FIELDCAT.
      WHEN 'VGE01'.
        PERFORM MODIFY_ALV_FIELD USING '整備單位'
                                 CHANGING GS_FIELDCAT.
      WHEN 'VGW02'.
        PERFORM MODIFY_ALV_FIELD USING '機器'
                                 CHANGING GS_FIELDCAT.
      WHEN 'VGE02'.
        PERFORM MODIFY_ALV_FIELD USING '機器單位'
                                 CHANGING GS_FIELDCAT.

      WHEN 'VGW03'.
        PERFORM MODIFY_ALV_FIELD USING '人工'
                                 CHANGING GS_FIELDCAT.
      WHEN 'VGE03'.
        PERFORM MODIFY_ALV_FIELD USING '人工單位'
                                 CHANGING GS_FIELDCAT.

      WHEN 'VGW04'.
        PERFORM MODIFY_ALV_FIELD USING '單位產值'
                                 CHANGING GS_FIELDCAT.
      WHEN 'VGE04'.
        PERFORM MODIFY_ALV_FIELD USING '產值單位'
                                 CHANGING GS_FIELDCAT.

      WHEN 'PEINH'.
        PERFORM MODIFY_ALV_FIELD USING '價格單位'
                                 CHANGING GS_FIELDCAT.
      WHEN 'PREIS'.
        PERFORM MODIFY_ALV_FIELD USING '單價'
                                 CHANGING GS_FIELDCAT.

      WHEN 'PLNNR'.
      WHEN 'PLNAL'.

*      WHEN OTHERS.
*        L_DELETE = 'X'.

    ENDCASE.

    IF L_DELETE = 'X'.
      DELETE GT_FIELDCAT INDEX L_INDEX.
    ELSE.
      MODIFY GT_FIELDCAT FROM GS_FIELDCAT INDEX L_INDEX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  get_WEEK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_CYPLAF_PSTTR  text
*----------------------------------------------------------------------*
FORM GET_WEEK  USING    P_T_CYPLAF_PSTTR W_WEEK.

  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      DATE         = P_T_CYPLAF_PSTTR
    IMPORTING
      WEEK         = W_WEEK
    EXCEPTIONS
      DATE_INVALID = 1
      OTHERS       = 2.


ENDFORM.                    " get_WEEK
*&---------------------------------------------------------------------*
*&      Form  get_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SWEEK  text
*      -->P_W_DATE  text
*----------------------------------------------------------------------*
FORM GET_MONTH  USING    P_W_SWEEK
                         P_W_DATE.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      WEEK         = P_W_SWEEK
    IMPORTING
      DATE         = P_W_DATE
    EXCEPTIONS
      DATE_INVALID = 1
      OTHERS       = 2.


ENDFORM.                    " get_MONTH

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                  RS_SELFIELD TYPE SLIS_SELFIELD.
  FIELD-SYMBOLS: <ITAB> LIKE LINE OF T_DATA.

  CASE R_UCOMM.
    WHEN '&IC1'.
*      MESSAGE '##' TYPE 'I'.
      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'MATNR' OR 'WERKS'.
          READ TABLE T_DATA INDEX RS_SELFIELD-TABINDEX
          ASSIGNING <ITAB>.

          SET PARAMETER ID 'MAT'  FIELD <ITAB>-MATNR.
          SET PARAMETER ID 'WRK'  FIELD <ITAB>-WERKS.
          CALL TRANSACTION 'CA03' AND SKIP FIRST SCREEN.
      ENDCASE.
  ENDCASE.
ENDFORM. "USER_COMMAND
*&---------------------------------------------------------------------*
*& Form MODIFY_ALV_FIELD
*&---------------------------------------------------------------------*
FORM MODIFY_ALV_FIELD  USING    IN_FTEXT
                       CHANGING LW_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  LW_FIELDCAT-SELTEXT_M = IN_FTEXT.
  LW_FIELDCAT-SELTEXT_L = IN_FTEXT.
  LW_FIELDCAT-SELTEXT_S = IN_FTEXT.
  LW_FIELDCAT-REPTEXT_DDIC = IN_FTEXT.

ENDFORM.

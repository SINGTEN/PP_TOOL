************************************************************************
* Program Name      : ZPPMIG003
* Descriptions      : 未結工單轉檔
* Updates Tables    :
* Input  Parameters :
* Output Parameters :
* Return Codes      :
* Special Logic     :
************************************************************************
* Modification Log
************************************************************************
*   Date   Ver. Programmer   Descriptions
* -------- ---- ------------ -------------------------------------------
* 20201207  1.0 Derek    New Create
*
************************************************************************
REPORT ZPPMIG003 NO STANDARD PAGE HEADING MESSAGE-ID 00.
************************************************************************
* Tables Definitions
************************************************************************
TABLES: AFKO , MARC , PLKO , MARA,AUFK,AFPO,RESB.
************************************************************************
* Data Definitions
************************************************************************
*-->ITab:上傳檔案
DATA: BEGIN OF GT_HEADER OCCURS 0,
        ABLAD LIKE AFPO-ABLAD,  "舊工單號碼
        AUART LIKE AUFK-AUART,
        WERKS LIKE AUFK-WERKS,
        MATNR LIKE MARC-MATNR,
        GAMNG LIKE AFKO-GAMNG,  "工單數量
        GSTRP LIKE AFKO-GSTRP,  "開始日
        GLTRP LIKE AFKO-GLTRP,  "完成日
        WEMPF LIKE AFPO-WEMPF,  "最後異動日
        KDAUF LIKE AUFK-KDAUF,
        KDPOS LIKE AUFK-KDPOS,
      END   OF GT_HEADER.

DATA: BEGIN OF GT_ROUT OCCURS 0,
        ABLAD LIKE AFPO-ABLAD,  "舊工單號碼
        VORNR LIKE AFVC-VORNR,  "作業號碼
        ARBPL LIKE CRHD-ARBPL,  "工作中心
        STEUS LIKE AFVC-STEUS,  "控制碼
        KTSCH LIKE AFVC-KTSCH,  "標準內文碼
        LTXA1 LIKE AFVC-LTXA1,  "作業短文
        BMSCH LIKE AFVV-BMSCH,  "基礎數量
        MEINH LIKE AFVV-MEINH,  "作業計量單位
        UMREZ LIKE AFVV-UMREZ,  "轉換數量
        UMREN LIKE AFVV-UMREN,  "作業數量
        VGW02 LIKE AFVV-VGW02,  "機器時間
        VGE02 LIKE AFVV-VGE02,  "機器時間單位
        VGW03 LIKE AFVV-VGW03,  "人工時間
        VGE03 LIKE AFVV-VGE03,  "人工時間單位
        VGW04 LIKE AFVV-VGW04,  "單位產值
        VGE04 LIKE AFVV-VGE04,  "單位產值單位
        EKORG LIKE AFVC-EKORG,  "採購組織
        EKGRP LIKE AFVC-EKGRP,  "採購群組
        MATKL LIKE AFVC-MATKL,  "物料群組
        PEINH LIKE AFVC-PEINH,  "價格單位
        "PREIS LIKE AFVC-PREIS,  "價格
        PREIS TYPE C LENGTH 11,
        WAERS LIKE AFVC-WAERS,  "幣別
        USR04 LIKE AFVU-USR04,  "剩餘數量
        USE04 LIKE AFVU-USE04,  "剩餘數量單位
      END OF GT_ROUT.

DATA: BEGIN OF GT_BOM OCCURS 0,
        ABLAD LIKE AFPO-ABLAD,  "舊工單號碼
        POSNR LIKE RESBD-POSNR,
        VORNR LIKE RESBD-VORNR,
        MATNR LIKE RESBD-MATNR,
        LGORT LIKE RESBD-LGORT,
        CHARG LIKE RESBD-CHARG,
        MENGE LIKE RESBD-MENGE,
        MEINS LIKE RESBD-MEINS,
      END OF GT_BOM.

*-->ITab:ALV
DATA: BEGIN OF GT_LOG OCCURS 0.
        INCLUDE STRUCTURE GT_HEADER.
DATA:
        AUFNR   LIKE AUFK-AUFNR,
        TYPE    TYPE C LENGTH 1,
        MESSAGE TYPE C LENGTH 200,

        BISMT   LIKE MARA-BISMT,

        STAT    LIKE JEST-STAT,
        INACT   LIKE JEST-INACT,
      END   OF GT_LOG.



*-->itab for BDC
DATA: GS_PR_HEADER LIKE BAPISITEMR,
      GT_ITEM      LIKE BAPISSHDIN  OCCURS 0 WITH HEADER LINE,
      GT_RETURN    LIKE BAPIRETURN1 OCCURS 0 WITH HEADER LINE,
      GT_BDCTAB    LIKE BDCDATA     OCCURS 0 WITH HEADER LINE,
      GT_MSG       LIKE BDCMSGCOLL  OCCURS 0 WITH HEADER LINE.

*-->itab for ALV
TYPE-POOLS: SLIS.
DATA: G_REPID     TYPE SY-REPID.
DATA: GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

************************************************************************
* Includes Module
************************************************************************

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.
  PARAMETERS: P_R1   RADIOBUTTON GROUP G1 DEFAULT 'X',
              P_R2   RADIOBUTTON GROUP G1,
              P_R3   RADIOBUTTON GROUP G1,
              P_PATH TYPE RLGRAP-FILENAME OBLIGATORY.
  SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END   OF BLOCK B2.
************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* At Selection Screen Output
************************************************************************
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PATH.
  PERFORM GET_FILENAME USING P_PATH.


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

  CASE 'X'.
    WHEN P_R1.
      PERFORM UPLOAD_HEADER.
      PERFORM CREATE_WORK_ORDER_HEADER.
    WHEN P_R2.
      PERFORM UPLOAD_ROUTING.
      PERFORM GET_ORDER_HEADER.
      PERFORM CHANGE_ORDER_ROUTER.
    WHEN P_R3.
      PERFORM UPLOAD_BOM.
      PERFORM GET_ORDER_HEADER.
      PERFORM CHANGE_ORDER_BOM.
  ENDCASE.

  PERFORM SHOW_ALV.

END-OF-SELECTION.
************************************************************************
* Forms
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
FORM GET_FILENAME CHANGING EX_FILE.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      DEF_PATH         = 'D:/'
      MASK             = ',XLSX Files,*.xlsx,XLS Files,*.xls.'
      MODE             = 'O'
      TITLE            = 'Choose Input file'
    IMPORTING
      FILENAME         = EX_FILE
    EXCEPTIONS
      INV_WINSYS       = 1
      NO_BATCH         = 2
      SELECTION_CANCEL = 3
      SELECTION_ERROR  = 4
      OTHERS           = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_FILE TABLES T_DATA STRUCTURE ALSMEX_TABLINE
                  USING IN_FILE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = IN_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 100
      I_END_ROW               = 65536
    TABLES
      INTERN                  = T_DATA
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE S001(00) WITH '讀取檔案失敗' SY-SUBRC DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT T_DATA BY ROW COL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_HEADER
*&---------------------------------------------------------------------*
FORM UPLOAD_HEADER .
  DATA: LT_DATA LIKE TABLE OF ALSMEX_TABLINE WITH HEADER LINE,
        L_INDEX TYPE I.

  FIELD-SYMBOLS: <FS>.

  PERFORM UPLOAD_FILE TABLES LT_DATA
                      USING  P_PATH.

  LOOP AT LT_DATA.

    MOVE LT_DATA-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE GT_HEADER TO <FS>.
    MOVE LT_DATA-VALUE TO <FS>.


    AT END OF ROW.
      APPEND GT_HEADER. CLEAR: GT_HEADER.
    ENDAT.
  ENDLOOP.

  IF GT_HEADER[] IS INITIAL.
    MESSAGE S001 WITH '未上傳任何檔案' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_routing
*&---------------------------------------------------------------------*
FORM UPLOAD_ROUTING .
  DATA: LT_DATA LIKE TABLE OF ALSMEX_TABLINE WITH HEADER LINE,
        L_INDEX TYPE I.

  FIELD-SYMBOLS: <FS>.


  PERFORM UPLOAD_FILE TABLES LT_DATA
                      USING  P_PATH.

  LOOP AT LT_DATA.

    MOVE LT_DATA-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE GT_ROUT TO <FS>.
    MOVE LT_DATA-VALUE TO <FS>.

    AT END OF ROW.
      "UNPACK GT_ROUT-AUFNR TO GT_ROUT-AUFNR.
      APPEND GT_ROUT. CLEAR:GT_ROUT.
    ENDAT.
  ENDLOOP.

  SORT GT_ROUT BY ABLAD VORNR.

  IF GT_ROUT[] IS INITIAL.
    MESSAGE S001 WITH '未上傳任何檔案' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_VALUE_TO_CHAR
*&---------------------------------------------------------------------*
FORM CONVERT_VALUE_TO_CHAR  USING    IN_VALUE IN_UNIT
                            CHANGING EX_CHAR.

  CLEAR EX_CHAR.

  IF IN_UNIT IS NOT INITIAL.
    WRITE IN_VALUE TO EX_CHAR UNIT IN_UNIT.
  ELSE.
    WRITE IN_VALUE TO EX_CHAR.
  ENDIF.

  CONDENSE EX_CHAR NO-GAPS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_CURR_TO_CHAR
*&---------------------------------------------------------------------*
FORM CONVERT_CURR_TO_CHAR  USING    IN_CURR IN_WAERS
                            CHANGING EX_CHAR.

  CLEAR EX_CHAR.

  WRITE IN_CURR TO EX_CHAR CURRENCY IN_WAERS.

  CONDENSE EX_CHAR NO-GAPS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_WORK_ORDER_HEADER
*&---------------------------------------------------------------------*
FORM CREATE_WORK_ORDER_HEADER .
  DATA: L_BDCMODE  TYPE C LENGTH 01 VALUE 'N',
        LS_OPETION LIKE CTU_PARAMS,
        L_MES      TYPE C LENGTH 200,
        L_MENGE    TYPE C LENGTH 13,
        L_MEINS    LIKE MARA-MEINS,
        L_ABLAD    LIKE AFPO-ABLAD.

  DATA: LT_LOG LIKE GT_LOG OCCURS 0 WITH HEADER LINE.


  LOOP AT GT_HEADER.

    MOVE-CORRESPONDING GT_HEADER TO GT_LOG.

*--> 確認是否已轉進系統
    CLEAR: AFPO,L_ABLAD,LT_LOG[].

    CONCATENATE GT_HEADER-ABLAD '%' INTO L_ABLAD.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE @LT_LOG
       FROM AFPO INNER JOIN AUFK ON AUFK~AUFNR = AFPO~AUFNR
                 INNER JOIN AFKO ON AUFK~AUFNR = AFKO~AUFNR
                 LEFT OUTER JOIN JEST ON AUFK~OBJNR = JEST~OBJNR
  WHERE AFPO~ABLAD LIKE @L_ABLAD
    AND AUFK~LOEKZ = @SPACE
    AND AUFK~IDAT2 = '00000000'        " TECO
    AND ( JEST~STAT  = 'I0001' OR JEST~STAT = 'I0002' )
    AND JEST~INACT = @SPACE .

    IF SY-SUBRC = 0.
      READ TABLE LT_LOG INDEX 1.

      GT_LOG-TYPE    = 'W'.
      GT_LOG-MESSAGE = '此舊工單已轉過至系統'.
      GT_LOG-AUFNR   = LT_LOG-AUFNR.

      APPEND GT_LOG. CLEAR GT_LOG.
      CONTINUE.
    ENDIF.

    CLEAR:GT_MSG[],GT_BDCTAB[],L_MEINS,L_MENGE,L_ABLAD.

    "CO08初始畫面
    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0220'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=ENTK'.
    PERFORM BDC_FIELD  USING  'AFPOD-KDAUF'      GT_HEADER-KDAUF.
    PERFORM BDC_FIELD  USING  'AFPOD-KDPOS'      GT_HEADER-KDPOS.
    PERFORM BDC_FIELD  USING  'CAUFVD-MATNR'     GT_HEADER-MATNR.
    PERFORM BDC_FIELD  USING  'CAUFVD-WERKS'     GT_HEADER-WERKS.
    PERFORM BDC_FIELD  USING  'AUFPAR-PP_AUFART' GT_HEADER-AUART.

    SELECT SINGLE BISMT MEINS INTO (GT_LOG-BISMT,L_MEINS)
                              FROM MARA
                             WHERE MATNR = GT_HEADER-MATNR.

    PERFORM CONVERT_VALUE_TO_CHAR USING    GT_HEADER-GAMNG
                                           L_MEINS
                                  CHANGING L_MENGE.

    "工單基本資料
    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0115'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '/00'.
    PERFORM BDC_FIELD  USING  'CAUFVD-GAMNG'     L_MENGE.
    PERFORM BDC_FIELD  USING  'CAUFVD-GLTRP'     GT_HEADER-GLTRP.
    PERFORM BDC_FIELD  USING  'CAUFVD-GSTRP'     GT_HEADER-GSTRP.
    PERFORM BDC_FIELD  USING  'CAUFVD-TERKZ'     '3'.

    "切換到收貨頁籤
    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0115'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=KOWE'.

    "儲存
    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0115'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=BU'.

    "若工單類型ZOPN，先將借用欄位註記符號，
    "待後續途程也改完後再將符號移除
    "IF GT_HEADER-AUART = 'ZOPN'.
    "CONCATENATE GT_HEADER-ABLAD '@' INTO L_ABLAD.
    "ELSE.
    MOVE GT_HEADER-ABLAD TO L_ABLAD.
    "ENDIF.


    PERFORM BDC_FIELD  USING  'AFPOD-WEMPF'      GT_HEADER-WEMPF.
    PERFORM BDC_FIELD  USING  'AFPOD-ABLAD'      L_ABLAD.

    LS_OPETION-DISMODE = L_BDCMODE.
    LS_OPETION-UPDMODE = 'S'.
    LS_OPETION-DEFSIZE = ''.

    CALL TRANSACTION 'CO08' USING         GT_BDCTAB
                            OPTIONS FROM  LS_OPETION
                            MESSAGES INTO GT_MSG.

    READ TABLE GT_MSG WITH KEY MSGID = 'CO'
                               MSGNR = '100'
                               MSGTYP = 'S'.
    IF SY-SUBRC = 0.
      GT_LOG-AUFNR = GT_MSG-MSGV1.
      GT_LOG-TYPE  = 'S'.

    ELSE.
      GT_LOG-TYPE  = 'E'.

      LOOP AT GT_MSG.
        CLEAR L_MES.
        MESSAGE ID GT_MSG-MSGID TYPE GT_MSG-MSGTYP NUMBER GT_MSG-MSGNR
           WITH GT_MSG-MSGV1 GT_MSG-MSGV2
                GT_MSG-MSGV3 GT_MSG-MSGV4
           INTO L_MES.

        CONCATENATE GT_LOG-MESSAGE L_MES
               INTO GT_LOG-MESSAGE.
      ENDLOOP.
    ENDIF.

    APPEND GT_LOG. CLEAR GT_LOG.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING IN_PROGRAM IN_DYNPRO.
  CLEAR GT_BDCTAB.

  GT_BDCTAB-PROGRAM  = IN_PROGRAM.
  GT_BDCTAB-DYNPRO   = IN_DYNPRO.
  GT_BDCTAB-DYNBEGIN = 'X'.
  APPEND GT_BDCTAB.
ENDFORM.                    " BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING IN_FNAM IN_FVAL.
  CLEAR GT_BDCTAB.

  GT_BDCTAB-FNAM = IN_FNAM.
  GT_BDCTAB-FVAL = IN_FVAL.
  APPEND GT_BDCTAB.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*& Form SHOW_ALV
*&---------------------------------------------------------------------*
FORM SHOW_ALV .

*--> 設定欄位
  PERFORM SET_GT_FIELDCAT.

  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.    "根據欄位內容,調整欄位寬度
  GS_LAYOUT-ZEBRA             = 'X'.    "逐行變色


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS'  "CNS_PF_STATUS
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT
    TABLES
      T_OUTTAB                 = GT_LOG
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_GT_FIELDCAT
*&---------------------------------------------------------------------*
FORM SET_GT_FIELDCAT .

  CLEAR:GT_FIELDCAT[].

  PERFORM GET_FIELDNAME USING 'ABLAD'  '舊工單號碼'.
  PERFORM GET_FIELDNAME USING 'AUART'  '工單類型'.
  PERFORM GET_FIELDNAME USING 'WERKS'   '工廠'.

  PERFORM GET_FIELDNAME USING 'MATNR'   '工單料號'.
  PERFORM GET_FIELDNAME USING 'BISMT'   '舊料號'.
  PERFORM GET_FIELDNAME USING 'GAMNG'   '工單數量'.
  PERFORM GET_FIELDNAME USING 'GSTRP'   '基本開始日期'.
  PERFORM GET_FIELDNAME USING 'GLTRP'   '基本完工日期'.
  PERFORM GET_FIELDNAME USING 'WEMPF'   '最後異動日'.
  PERFORM GET_FIELDNAME USING 'KDAUF'   '銷售訂單'.
  PERFORM GET_FIELDNAME USING 'KDPOS'   '銷售訂單項次'.
  PERFORM GET_FIELDNAME USING 'AUFNR'   '工單號碼'.
  PERFORM GET_FIELDNAME USING 'TYPE'    '結果'.
  PERFORM GET_FIELDNAME USING 'MESSAGE' '訊息'.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
FORM GET_FIELDNAME  USING IN_FNAME
                          IN_FTEXT.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  LS_FIELDCAT-FIELDNAME = IN_FNAME.
  LS_FIELDCAT-SELTEXT_M = IN_FTEXT.


  APPEND LS_FIELDCAT TO GT_FIELDCAT.
  CLEAR: LS_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS
*&---------------------------------------------------------------------*
FORM ALV_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDARD'.            " ALV中控制欄位

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_ORDER_ROUTER
*&---------------------------------------------------------------------*
FORM CHANGE_ORDER_ROUTER .
  DATA: LT_ROUT LIKE GT_ROUT OCCURS 0 WITH HEADER LINE.
  DATA: L_INDEX    TYPE I,
        L_ABLAD    LIKE AFPO-ABLAD,
        LS_OPETION LIKE CTU_PARAMS,
        L_MES      TYPE C LENGTH 200,

        L_BMSCH    TYPE C LENGTH 13,
        L_UMREZ    TYPE C LENGTH 5,
        L_UMREN    TYPE C LENGTH 5,
        L_VGW02    TYPE C LENGTH 9,
        L_VGW03    TYPE C LENGTH 9,
        L_VGW04    TYPE C LENGTH 9,
        L_PEINH    TYPE C LENGTH 5,
        L_PREIS    TYPE C LENGTH 11,
        L_USR04    TYPE C LENGTH 13.

  LOOP AT GT_LOG WHERE TYPE <> 'E'.
    L_INDEX = SY-TABIX.

*    IF GT_LOG-ABLAD NS '@'.
*      GT_LOG-TYPE = 'W'.
*      GT_LOG-MESSAGE = '此工單途程已調整過'.
*
*      MODIFY GT_LOG INDEX L_INDEX.
*      CONTINUE.
*    ENDIF.
*
*    CLEAR: LT_ROUT[],L_ABLAD.
*
*    PERFORM DELETE_CHAR_IN_STRING USING   GT_LOG-ABLAD
*                                          '@'
*                                  CHANGING L_ABLAD.

    L_ABLAD = GT_LOG-ABLAD.

    MOVE-CORRESPONDING GT_ROUT[] TO LT_ROUT[].
    DELETE LT_ROUT WHERE ABLAD <> L_ABLAD.


    IF LT_ROUT[] IS INITIAL.
      GT_LOG-TYPE = 'E'.
      GT_LOG-MESSAGE = '未結工單未上傳對應途程資訊'.

      MODIFY GT_LOG INDEX L_INDEX.
      CONTINUE.
    ENDIF.

    "修改工單初始畫面
    CLEAR:GT_MSG[],GT_BDCTAB[].

    "CO08初始畫面
    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0110'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=KPU2'.
    PERFORM BDC_FIELD  USING  'CAUFVD-AUFNR'      GT_LOG-AUFNR.

    CLEAR RESB.
    SELECT SINGLE * FROM RESB
                   WHERE AUFNR = GT_LOG-AUFNR.

    IF SY-SUBRC = 0.

      "全選BOM表
      PERFORM BDC_DYNPRO USING  'SAPLCOMK'         '0120'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=AMAK'.

      "刪除BOM表
      PERFORM BDC_DYNPRO USING  'SAPLCOMK'         '0120'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=DEL'.

    ENDIF.

    "點選作業
    PERFORM BDC_DYNPRO USING  'SAPLCOMK'         '0120'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=VGUE'.

    "全選作業
    PERFORM BDC_DYNPRO USING  'SAPLCOVG'         '0100'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=AMAK'.

    "第一項取消選擇後，刪除其他作業
    PERFORM BDC_DYNPRO USING  'SAPLCOVG'           '0100'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'         '=LOE'.
    PERFORM BDC_FIELD  USING  'RC27X-FLG_SEL(01)'  ''.

    PERFORM BDC_DYNPRO USING  'SAPLSPO2'           '0200'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'         '=OPT1'.

    "將第一筆作業代碼改成9999
    PERFORM BDC_DYNPRO USING  'SAPLCOVG'         '0100'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '/00'.
    PERFORM BDC_FIELD  USING  'AFVGD-VORNR(01)'  '9999'.

    LOOP AT LT_ROUT.
      "點選尋找
      PERFORM BDC_DYNPRO USING  'SAPLCOVG'         '0100'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=AUFS'.

      "尋找9999固定在第一項
      PERFORM BDC_DYNPRO USING  'SAPLCO05'         '0100'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=MORE'.
      PERFORM BDC_FIELD  USING  'RCOSU-VORNR'      '9999'.

      "選擇第一項後插入一筆
      PERFORM BDC_DYNPRO USING  'SAPLCOVG'          '0100'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'        '=VEIN'.
      PERFORM BDC_FIELD  USING  'RC27X-FLG_SEL(01)' 'X'.

      "填入資料
      PERFORM BDC_DYNPRO USING  'SAPLCOVG'        '0100'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.
      PERFORM BDC_FIELD  USING  'AFVGD-VORNR(01)' LT_ROUT-VORNR.
      PERFORM BDC_FIELD  USING  'AFVGD-ARBPL(01)' LT_ROUT-ARBPL.
      PERFORM BDC_FIELD  USING  'AFVGD-STEUS(01)' LT_ROUT-STEUS.
      PERFORM BDC_FIELD  USING  'AFVGD-KTSCH(01)' LT_ROUT-KTSCH.


      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-BMSCH
                                             LT_ROUT-MEINH
                                    CHANGING L_BMSCH.

      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-UMREZ
                                             ''
                                    CHANGING L_UMREZ.

      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-UMREN
                                             ''
                                    CHANGING L_UMREN.

      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-VGW02
                                             LT_ROUT-VGE02
                                    CHANGING L_VGW02.

      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-VGW03
                                             LT_ROUT-VGE03
                                    CHANGING L_VGW03.

      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-VGW04
                                             LT_ROUT-VGE04
                                    CHANGING L_VGW04.

      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-PEINH
                                             ''
                                    CHANGING L_PEINH.

      PERFORM CONVERT_CURR_TO_CHAR USING    LT_ROUT-PREIS
                                             ''
                                    CHANGING L_PREIS.

      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_ROUT-USR04
                                             LT_ROUT-USE04
                                    CHANGING L_USR04.

      IF LT_ROUT-STEUS = 'PP01'.
        "自製項目明細
        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.

        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.
        PERFORM BDC_FIELD  USING  'AFVGD-BMSCH'    L_BMSCH.
        PERFORM BDC_FIELD  USING  'AFVGD-MEINH'    LT_ROUT-MEINH.
        PERFORM BDC_FIELD  USING  'AFVGD-UMREZ'    L_UMREZ.
        PERFORM BDC_FIELD  USING  'AFVGD-UMREN'    L_UMREN.

        IF LT_ROUT-VGE02 IS NOT INITIAL.
          PERFORM BDC_FIELD  USING  'AFVGD-VGW02'    L_VGW02.
          PERFORM BDC_FIELD  USING  'AFVGD-VGE02'    LT_ROUT-VGE02.
        ENDIF.

        IF LT_ROUT-VGE03 IS NOT INITIAL.
          PERFORM BDC_FIELD  USING  'AFVGD-VGW03'    L_VGW03.
          PERFORM BDC_FIELD  USING  'AFVGD-VGE03'    LT_ROUT-VGE03.
        ENDIF.

        IF LT_ROUT-VGE04 IS NOT INITIAL.
          PERFORM BDC_FIELD  USING  'AFVGD-VGW04'    L_VGW04.
          PERFORM BDC_FIELD  USING  'AFVGD-VGE04'    LT_ROUT-VGE04.
        ENDIF.

        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.

        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.

        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.

      ELSE.
        "委外項目明細
        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.
        PERFORM BDC_FIELD  USING  'AFVGD-EKORG'    LT_ROUT-EKORG.
        PERFORM BDC_FIELD  USING  'AFVGD-EKGRP'    LT_ROUT-EKGRP.
        PERFORM BDC_FIELD  USING  'AFVGD-PEINH'    L_PEINH.
        PERFORM BDC_FIELD  USING  'AFVGD-PREIS'    L_PREIS.
        PERFORM BDC_FIELD  USING  'AFVGD-WAERS'    LT_ROUT-WAERS.
        PERFORM BDC_FIELD  USING  'AFVGD-MATKL'    LT_ROUT-MATKL.
        PERFORM BDC_FIELD  USING  'AFVGD-MEINH'    LT_ROUT-MEINH.
        PERFORM BDC_FIELD  USING  'AFVGD-UMREZ'    L_UMREZ.
        PERFORM BDC_FIELD  USING  'AFVGD-UMREN'    L_UMREN.
      ENDIF.

      "剩餘數量
      IF LT_ROUT-USR04 IS NOT INITIAL .
        PERFORM BDC_DYNPRO USING  'SAPLCOVG'       '0100'.
        PERFORM BDC_FIELD  USING  'BDC_CURSOR'     'AFVGD-VORNR(01)'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'     '=PICK'.

        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '=VOUS'.

        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '/00'.
        PERFORM BDC_FIELD  USING  'AFVGD-SLWID'     'SMK0001'.

        PERFORM BDC_DYNPRO USING  'SAPLCOVF'        '0100'.
        PERFORM BDC_FIELD  USING  'BDC_OKCODE'      '=BACK'.
        PERFORM BDC_FIELD  USING  'AFVGD-USR04'     L_USR04.
        PERFORM BDC_FIELD  USING  'AFVGD-USE04'     LT_ROUT-USE04.
      ENDIF.

      "清空選擇
      PERFORM BDC_DYNPRO USING  'SAPLCOVG'       '0100'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'     '=AMAL'.
      PERFORM BDC_FIELD  USING  'AFVGD-LTXA1(01)'   LT_ROUT-LTXA1.
    ENDLOOP.

*   刪除9999項目
    PERFORM BDC_DYNPRO USING  'SAPLCOVG'         '0100'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=AUFS'.

    PERFORM BDC_DYNPRO USING  'SAPLCO05'         '0100'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=MORE'.
    PERFORM BDC_FIELD  USING  'RCOSU-VORNR'      '9999'.

    PERFORM BDC_DYNPRO USING  'SAPLCOVG'          '0100'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'        '=LOE'.
    PERFORM BDC_FIELD  USING  'RC27X-FLG_SEL(01)' 'X'.

    PERFORM BDC_DYNPRO USING  'SAPLSPO2'          '0200'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'        '=OPT1'.

    "補回舊工單號碼並儲存
    PERFORM BDC_DYNPRO USING  'SAPLCOVG'         '0100'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=KOZE'.

    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'         '0115'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=KOWE'.

    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'         '0115'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=BU'.
    PERFORM BDC_FIELD  USING  'AFPOD-ABLAD'      L_ABLAD.


    LS_OPETION-DISMODE = 'N'.
    LS_OPETION-UPDMODE = 'S'.
    LS_OPETION-DEFSIZE = ''.

    CALL TRANSACTION 'CO02' USING         GT_BDCTAB
                            OPTIONS FROM  LS_OPETION
                            MESSAGES INTO GT_MSG.

    READ TABLE GT_MSG WITH KEY MSGID = 'CO'
                               MSGNR = '100'
                               MSGTYP = 'S'.

    IF SY-SUBRC = 0.
      GT_LOG-TYPE  = 'S'.

    ELSE.
      GT_LOG-TYPE  = 'E'.

      LOOP AT GT_MSG.
        CLEAR L_MES.
        MESSAGE ID GT_MSG-MSGID TYPE GT_MSG-MSGTYP NUMBER GT_MSG-MSGNR
           WITH GT_MSG-MSGV1 GT_MSG-MSGV2
                GT_MSG-MSGV3 GT_MSG-MSGV4
           INTO L_MES.

        CONCATENATE GT_LOG-MESSAGE L_MES
               INTO GT_LOG-MESSAGE.
      ENDLOOP.
    ENDIF.

    MODIFY GT_LOG INDEX L_INDEX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_CHAR_IN_STRING
*&---------------------------------------------------------------------*
FORM DELETE_CHAR_IN_STRING  USING    IN_STRING
                                     IN_CHAR
                            CHANGING EX_STRING.

  DATA: LV_LENGTH TYPE I,
        LV_INDEX  TYPE I,
        LV_CHAR   TYPE C.

  LV_LENGTH = STRLEN( IN_STRING ).

  DO LV_LENGTH TIMES.
    LV_INDEX = SY-INDEX - 1.
    LV_CHAR = IN_STRING+LV_INDEX(1).

    IF LV_CHAR <> IN_CHAR.
      CONCATENATE EX_STRING LV_CHAR INTO EX_STRING.
    ENDIF.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ORDER_HEADER
*&---------------------------------------------------------------------*
FORM GET_ORDER_HEADER .
  DATA: L_ABLAD LIKE AFPO-ABLAD.

  DATA: BEGIN OF LT_ORDER OCCURS 0,
          ABLAD LIKE AFPO-ABLAD,
        END OF LT_ORDER.

  IF P_R2 = 'X'.
    MOVE-CORRESPONDING GT_ROUT[] TO LT_ORDER[].
  ELSE.
    MOVE-CORRESPONDING GT_BOM[] TO LT_ORDER[].
  ENDIF.


  SORT LT_ORDER BY ABLAD.
  DELETE ADJACENT DUPLICATES FROM LT_ORDER COMPARING ABLAD.

  LOOP AT LT_ORDER.

    L_ABLAD = LT_ORDER-ABLAD.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE @GT_LOG
      FROM AFPO INNER JOIN AUFK ON AUFK~AUFNR = AFPO~AUFNR
                INNER JOIN AFKO ON AUFK~AUFNR = AFKO~AUFNR
                LEFT OUTER JOIN JEST ON AUFK~OBJNR = JEST~OBJNR
    WHERE AFPO~ABLAD = @L_ABLAD
      AND AUFK~LOEKZ = @SPACE
      AND AUFK~IDAT2 = '00000000'        " TECO
      AND ( JEST~STAT  = 'I0001' OR JEST~STAT = 'I0002' )
      AND JEST~INACT = @SPACE .

    IF SY-SUBRC <> 0.
      GT_LOG-ABLAD   = LT_ORDER-ABLAD.
      GT_LOG-TYPE    = 'E'.
      GT_LOG-MESSAGE = '此舊工單表頭尚未轉至SAP'.
      APPEND GT_LOG. CLEAR GT_LOG.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM UPLOAD_BOM .

  DATA: LT_DATA LIKE TABLE OF ALSMEX_TABLINE WITH HEADER LINE,
        L_INDEX TYPE I.

  FIELD-SYMBOLS: <FS>.


  PERFORM UPLOAD_FILE TABLES LT_DATA
                      USING  P_PATH.

  LOOP AT LT_DATA.

    MOVE LT_DATA-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE GT_BOM TO <FS>.
    MOVE LT_DATA-VALUE TO <FS>.

    AT END OF ROW.
      APPEND GT_BOM. CLEAR:GT_BOM.
    ENDAT.
  ENDLOOP.

  SORT GT_BOM BY ABLAD POSNR.

  IF GT_BOM[] IS INITIAL.
    MESSAGE S001 WITH '未上傳任何檔案' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_ORDER_BOM
*&---------------------------------------------------------------------*
FORM CHANGE_ORDER_BOM .
  DATA: LT_BOM LIKE GT_BOM OCCURS 0 WITH HEADER LINE.
  DATA: L_INDEX    TYPE I,
        L_ABLAD    LIKE AFPO-ABLAD,
        LS_OPETION LIKE CTU_PARAMS,
        L_MENGE    TYPE C LENGTH 13,
        L_MES      TYPE C LENGTH 200.

  LOOP AT GT_LOG WHERE TYPE <> 'E'.
    L_INDEX = SY-TABIX.

    L_ABLAD = GT_LOG-ABLAD.

    MOVE-CORRESPONDING GT_BOM[] TO LT_BOM[].
    DELETE LT_BOM WHERE ABLAD <> L_ABLAD.


    IF LT_BOM[] IS INITIAL.
      GT_LOG-TYPE = 'E'.
      GT_LOG-MESSAGE = '未結工單未上傳對應BOM資訊'.

      MODIFY GT_LOG INDEX L_INDEX.
      CONTINUE.
    ENDIF.

    "修改工單初始畫面
    CLEAR:GT_MSG[],GT_BDCTAB[].

    "CO02初始畫面
    PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0110'.
    PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=KPU2'.
    PERFORM BDC_FIELD  USING  'CAUFVD-AUFNR'      GT_LOG-AUFNR.

    "填寫發料元件
    LOOP AT LT_BOM.
      PERFORM CONVERT_VALUE_TO_CHAR USING    LT_BOM-MENGE
                                             LT_BOM-MEINS
                                    CHANGING L_MENGE.

      PERFORM BDC_DYNPRO USING  'SAPLCOMK'        '0120'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '/00'.
      PERFORM BDC_FIELD  USING  'RESBD-POSNR(01)'       LT_BOM-POSNR.
      PERFORM BDC_FIELD  USING  'RESBD-MATNR(01)'       LT_BOM-MATNR.
      PERFORM BDC_FIELD  USING  'RESBD-MENGE(01)'       L_MENGE.
      PERFORM BDC_FIELD  USING  'RESBD-EINHEIT(01)'       LT_BOM-MEINS.
      PERFORM BDC_FIELD  USING  'RESBD-POSTP(01)'       'L'.
      PERFORM BDC_FIELD  USING  'RESBD-VORNR(01)'       LT_BOM-VORNR.
      PERFORM BDC_FIELD  USING  'RESBD-WERKS(01)'       '1101'.
      PERFORM BDC_FIELD  USING  'RESBD-LGORT(01)'       LT_BOM-LGORT.

      PERFORM BDC_DYNPRO USING  'SAPLCOMD'        '0110'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '/00'.

      PERFORM BDC_DYNPRO USING  'SAPLCOMK'        '0120'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=KOZE'.

      PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0115'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=KOWE'.

      PERFORM BDC_DYNPRO USING  'SAPLCOKO1'        '0115'.
      PERFORM BDC_FIELD  USING  'BDC_OKCODE'       '=BU'.
    ENDLOOP.


    LS_OPETION-DISMODE = 'N'.
    LS_OPETION-UPDMODE = 'S'.
    LS_OPETION-DEFSIZE = ''.

    CALL TRANSACTION 'CO02' USING         GT_BDCTAB
                            OPTIONS FROM  LS_OPETION
                            MESSAGES INTO GT_MSG.

    READ TABLE GT_MSG WITH KEY MSGID = 'CO'
                               MSGNR = '100'
                               MSGTYP = 'S'.

    IF SY-SUBRC = 0.
      GT_LOG-TYPE  = 'S'.

    ELSE.
      GT_LOG-TYPE  = 'E'.

      LOOP AT GT_MSG.
        CLEAR L_MES.
        MESSAGE ID GT_MSG-MSGID TYPE GT_MSG-MSGTYP NUMBER GT_MSG-MSGNR
           WITH GT_MSG-MSGV1 GT_MSG-MSGV2
                GT_MSG-MSGV3 GT_MSG-MSGV4
           INTO L_MES.

        CONCATENATE GT_LOG-MESSAGE L_MES
               INTO GT_LOG-MESSAGE.
      ENDLOOP.
    ENDIF.

    MODIFY GT_LOG INDEX L_INDEX.
  ENDLOOP.

ENDFORM.

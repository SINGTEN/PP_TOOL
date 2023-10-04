************************************************************************
* Program Name      : ZPPR0002_TMP
* Descriptions      : 展BOM清單
* T-Code            :
* Updates Tables    :
* Input  Parameters :
* Output Parameters :
* Return Codes      :
* Special Logic     :
* Includes          :
************************************************************************
* Modification Log
************************************************************************
*   Date   Ver. Programmer   Descriptions
* -------- ---- ------------ -------------------------------------------
* 20230302 1.0  derek       Original Create
*
************************************************************************
REPORT ZPPR0002_TMP NO STANDARD PAGE HEADING
                MESSAGE-ID 00  LINE-SIZE 230.

************************************************************************
* Tables Definitions
************************************************************************
TABLES: MARA,MAST,MARC.

************************************************************************
* Data Definitions
************************************************************************
*--> MESSAGE ID
DATA: G_MSGID LIKE SY-MSGID VALUE 'ZPP'.

DATA: BEGIN OF GT_HEADER OCCURS 0,
        WERKS   LIKE MAST-WERKS,
        MATNR   LIKE VBAP-MATNR, "
        STLAN   LIKE MAST-STLAN,
        STLAL   LIKE MAST-STLAL,
        MATKL   LIKE MARA-MATKL, "
        MEINS   LIKE MARA-MEINS, "
        MAKTX   LIKE MAKT-MAKTX, "
        BMENG   LIKE STKO-BMENG, "
        BMEIN   LIKE STKO-BMEIN,
        DISLS   LIKE MARC-DISLS, "
        DISPO   LIKE MARC-DISPO, "
        PRCTR   LIKE MARC-PRCTR,
        BISMT   LIKE MARA-BISMT,
        BESKZ   LIKE MARC-BESKZ,
        SOBSL   LIKE MARC-SOBSL,
        CHAR1   TYPE C LENGTH 40,
        CHAR2   TYPE C LENGTH 40,
        CHAR3   TYPE C LENGTH 40,
        ERROR   TYPE C LENGTH 1,
      END OF GT_HEADER.

DATA: BEGIN OF GT_ITEM OCCURS 0,
        WERKS   LIKE MAST-WERKS,
        MATNR   LIKE VBAP-MATNR,
        STLAN   LIKE MAST-STLAN,
        STUFE   LIKE STPOX-STUFE,
        POSNR   LIKE STPOX-POSNR,
        IDBIS   LIKE MARA-BISMT,
        IDNRK   LIKE STPOX-IDNRK,
        OJTXB   LIKE STPOX-OJTXB,
        XMENG   LIKE STPOX-XMENG,
        DSPST   LIKE STPOX-DSPST,
        ITSOB   LIKE STPOX-ITSOB,
        MNGLG   LIKE STPOX-MNGLG,
        MENGE   LIKE STPOX-MENGE,
        AUSCH   LIKE STPO-AUSCH,
        MEINS   TYPE STPOX-MEINS,
        LABST   LIKE MARD-LABST,
        POSTP   LIKE STPOX-POSTP,
        BEIKZ   LIKE STPOX-BEIKZ,
        SCHGT   LIKE STPOX-SCHGT,
        SANKA   LIKE STPOX-SANKA,
        SORTF   LIKE STPOX-SORTF,
        ALPGR   LIKE STPOX-ALPGR,  "群組
        ALPRF   LIKE STPOX-ALPRF,  "優先順序
        BESKZ   LIKE MARC-BESKZ,
        SOBSL   LIKE MARC-SOBSL,
        PARTL   TYPE C LENGTH 3000,
      END OF GT_ITEM.



*--> EXCEL DOWNLOAD PATH
DATA: G_PATH LIKE RLGRAP-FILENAME.

*--> itab for ALV.
DATA: BEGIN OF GT_ALV OCCURS 0,
        WERKS  LIKE MARC-WERKS,
        MATNR  LIKE MAST-MATNR,
        BISMT  LIKE MARA-BISMT,
        MAKTX  LIKE MAKT-MAKTX,
        BESKZ  LIKE MARC-BESKZ,
        SOBSL  LIKE MARC-SOBSL,
        STLAN  LIKE MAST-STLAN,
        POSNR  LIKE STPOX-POSNR,
        POSTP  LIKE STPOX-POSNR,
        IDBIS  LIKE MARA-BISMT,
        IDNRK  LIKE STPOX-IDNRK,
        OJTXB  LIKE STPOX-OJTXB,
        IBESKZ LIKE MARC-BESKZ,
        ISOBSL LIKE MARC-SOBSL,
        MENGE  LIKE STPOX-MENGE,
        MEINS  LIKE STPOX-MEINS,
        BMENG  LIKE STKO-BMENG,
        AUSCH  LIKE STPO-AUSCH,
        ALPGR  LIKE STPO-ALPGR,
        ALPRF  LIKE STPO-ALPRF,
        ALPST  LIKE STPO-ALPST,
        EWAHR  LIKE STPO-EWAHR,
        SANKA  LIKE STPO-SANKA,
      END   OF GT_ALV.

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
PARAMETERS    : P_WERKS LIKE MARC-WERKS OBLIGATORY .
SELECT-OPTIONS: S_MATNR FOR MAST-MATNR MODIF ID R02,
                S_BISMT FOR MARA-BISMT,
                S_STLAN FOR MAST-STLAN DEFAULT '1',
                S_STLAL FOR MAST-STLAL DEFAULT '01',
                S_MMSTA FOR MARC-MMSTA,
                S_BESKZ FOR MARC-BESKZ,
                S_SOBSL FOR MARC-SOBSL.
PARAMETERS    : P_DATUV LIKE STKO-DATUV OBLIGATORY.

************************************************************************
* Initialization
************************************************************************
INITIALIZATION.
  P_WERKS = '1101'.
  P_DATUV = SY-DATUM.

************************************************************************
* At Selection Screen Output
************************************************************************
AT SELECTION-SCREEN OUTPUT.

************************************************************************
* User Common
************************************************************************
AT USER-COMMAND.



************************************************************************
* Report Format
************************************************************************
TOP-OF-PAGE.

END-OF-PAGE.

************************************************************************
* Main Process
************************************************************************
START-OF-SELECTION.
*--> 查詢表頭資料
  PERFORM GET_HEADER.

*--> 查詢BOM相關資料
  PERFORM GET_OTHER_INFO.

*--> 處理ALV資料
  PERFORM GET_ALV.

*--> ALV呈現
  PERFORM DISPLAY_ALV.

END-OF-SELECTION.

************************************************************************
* Form
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_OTHER_INFO
*&---------------------------------------------------------------------*
FORM GET_OTHER_INFO .
  DATA: L_INDEX    LIKE SY-TABIX.

  LOOP AT GT_HEADER.
    L_INDEX = SY-TABIX.

*   BOM infor (item)
    PERFORM GET_BOM.

    IF GT_HEADER-ERROR = 'X'.
      DELETE GT_HEADER INDEX L_INDEX.
      CONTINUE.
    ENDIF.

*   料號說明--->
    PERFORM GET_MAKTX USING    GT_HEADER-MATNR
                      CHANGING GT_HEADER-MAKTX.


    MODIFY GT_HEADER INDEX L_INDEX.
  ENDLOOP.

  IF GT_HEADER[] IS INITIAL.
    MESSAGE S001 WITH '未取得條件內資料' DISPLAY LIKE 'E'.

    STOP.
  ENDIF.

ENDFORM.                    " GET_OTHER_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_MAKTX
*&---------------------------------------------------------------------*
FORM GET_MAKTX  USING    IN_MATNR
                CHANGING EX_MAKTX.

  SELECT SINGLE MAKTX INTO EX_MAKTX
                      FROM MAKT
                     WHERE MATNR = IN_MATNR
                       AND SPRAS = SY-LANGU.

ENDFORM.                    " GET_MAKTX
*&---------------------------------------------------------------------*
*&      Form  GET_BOM
*&---------------------------------------------------------------------*
FORM GET_BOM .
  DATA: LT_STPOX  LIKE STPOX  OCCURS 0 WITH HEADER LINE,
        LS_STPOX  LIKE STPOX,
        LT_MATCAT LIKE CSCMAT OCCURS 0 WITH HEADER LINE,
        LS_CSTMAT LIKE CSTMAT,
        LS_MARC   LIKE MARC,
        L_STUFE   LIKE STPOX-STUFE,
        L_CAPID   LIKE TC04-CAPID,
        L_INDEX   TYPE I.

  IF GT_HEADER-STLAN = '1'.
    L_CAPID = 'PP01'.  "量產BOM
  ELSE.
    L_CAPID = 'PP02'.  "研發BOM
  ENDIF.


  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = L_CAPID
      "MEHRS                 = 'X'
      DATUV                 = P_DATUV
      MTNRV                 = GT_HEADER-MATNR
      WERKS                 = GT_HEADER-WERKS
      STLAL                 = GT_HEADER-STLAL
      STLAN                 = GT_HEADER-STLAN
      EMENG                 = '1'
      RNDKZ                 = '1'
    IMPORTING
      TOPMAT                = LS_CSTMAT
    TABLES
      STB                   = LT_STPOX
      MATCAT                = LT_MATCAT
    EXCEPTIONS
      ALT_NOT_FOUND         = 1
      CALL_INVALID          = 2
      MATERIAL_NOT_FOUND    = 3
      MISSING_AUTHORIZATION = 4
      NO_BOM_FOUND          = 5
      NO_PLANT_DATA         = 6
      NO_SUITABLE_BOM_FOUND = 7
      OTHERS                = 8.



  IF LT_STPOX[] IS INITIAL.
    GT_HEADER-ERROR = 'X'.
    EXIT.
  ENDIF.


  LOOP AT LT_STPOX.
    L_INDEX = SY-TABIX.

    PERFORM SET_ITEM USING LT_STPOX.
  ENDLOOP.

  GT_HEADER-BMENG = LS_CSTMAT-BMENG.
  GT_HEADER-BMEIN = LS_CSTMAT-BMEIN.

ENDFORM.                    " GET_BOM

*&---------------------------------------------------------------------*
*&      Form  CHECK_MAST
*&---------------------------------------------------------------------*
FORM CHECK_MAST  CHANGING LW_HEADER LIKE GT_HEADER.

  CLEAR MAST.
  SELECT SINGLE * FROM MAST
                 WHERE WERKS = LW_HEADER-WERKS
                   AND MATNR = LW_HEADER-MATNR
                   AND STLAN = LW_HEADER-STLAN
                   AND STLAL = LW_HEADER-STLAL.

  IF SY-SUBRC <> 0.
    LW_HEADER-ERROR = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_MAST

*&---------------------------------------------------------------------*
*&      Form  GET_STOCK
*&---------------------------------------------------------------------*
FORM GET_STOCK  CHANGING LW_ITEM LIKE GT_ITEM.

  DATA: BEGIN OF LT_MARD OCCURS 0,
          LABST LIKE MARD-LABST,
        END OF LT_MARD.

  SELECT LABST INTO CORRESPONDING FIELDS OF TABLE LT_MARD
               FROM MARD
              WHERE WERKS = LW_ITEM-WERKS
                AND MATNR = LW_ITEM-IDNRK.

  LOOP AT LT_MARD.
    LW_ITEM-LABST = LW_ITEM-LABST + LT_MARD-LABST.
  ENDLOOP.

ENDFORM.                    " GET_STOCK
*&---------------------------------------------------------------------*
*&      Form  GET_HEADER
*&---------------------------------------------------------------------*
FORM GET_HEADER.

  SELECT MAST~MATNR MAST~WERKS MAST~STLAN MAST~STLAL
         MARA~BISMT MARA~MEINS MARA~MATKL
         MARC~BESKZ MARC~SOBSL MARC~DISLS MARC~DISPO
         MARC~PRCTR
    INTO CORRESPONDING FIELDS OF TABLE GT_HEADER
    FROM MAST INNER JOIN MARC
      ON MAST~MATNR = MARC~MATNR AND MAST~WERKS = MARC~WERKS
    INNER JOIN MARA ON MAST~MATNR EQ MARA~MATNR
   WHERE MAST~MATNR IN S_MATNR
     AND MAST~WERKS  = P_WERKS
     AND MAST~STLAN IN S_STLAN
     AND MAST~STLAL IN S_STLAL
     AND MARC~MMSTA IN S_MMSTA
     AND MARC~BESKZ IN S_BESKZ
     AND MARC~SOBSL IN S_SOBSL
     AND MARA~BISMT IN S_BISMT.

ENDFORM.                    " GET_HEADER
*&---------------------------------------------------------------------*
*& Form GET_PURTYPE
*&---------------------------------------------------------------------*
FORM GET_PURTYPE  USING    IN_IDNRK
                  CHANGING EX_PURTYPE.

  DATA: LS_MARC LIKE MARC.

  SELECT SINGLE *
    INTO LS_MARC
    FROM MARC
   WHERE MATNR EQ IN_IDNRK
     AND WERKS EQ P_WERKS.

  IF LS_MARC-SOBSL IS NOT INITIAL.
    CONCATENATE LS_MARC-BESKZ '/' LS_MARC-SOBSL INTO EX_PURTYPE.
  ELSE.
    EX_PURTYPE = LS_MARC-BESKZ.
  ENDIF.

ENDFORM.                    "GET_PURTYPE
*&---------------------------------------------------------------------*
*& Form CONVERSION_UNIT_OUTPUT
*&---------------------------------------------------------------------*
FORM CONVERSION_UNIT_OUTPUT  USING    IN_MEINS
                             CHANGING EX_MEINS.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      INPUT          = IN_MEINS
      LANGUAGE       = SY-LANGU
    IMPORTING
*     LONG_TEXT      =
      OUTPUT         = EX_MEINS
*     SHORT_TEXT     =
    EXCEPTIONS
      UNIT_NOT_FOUND = 1
      OTHERS         = 2.

ENDFORM.                    "CONVERSION_UNIT_OUTPUT
*&---------------------------------------------------------------------*
*& Form EXPL_MAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM EXPL_MAT  TABLES   IT_STPOX STRUCTURE STPOX
               USING    IN_MATNR.

  DATA: LT_STPOX  LIKE STPOX  OCCURS 0 WITH HEADER LINE,
        LT_MATCAT LIKE CSCMAT OCCURS 0 WITH HEADER LINE,
        L_CAPID   LIKE TC04-CAPID.


  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      AUFSW                 = ' '
      CAPID                 = 'PP01'
      DATUV                 = SY-DATUM
*     EHNDL                 = '1'
      EMENG                 = 1
      MBWLS                 = ' '
      MKTLS                 = 'X'
      MEHRS                 = 'X'
      MMORY                 = 'X'
*     FBSTP                 = P_BNF
      MTNRV                 = IN_MATNR
      STLAL                 = '01'
      STLAN                 = '1'
      WERKS                 = P_WERKS
    TABLES
      STB                   = LT_STPOX
      MATCAT                = LT_MATCAT
    EXCEPTIONS
      ALT_NOT_FOUND         = 1
      CALL_INVALID          = 2
      MATERIAL_NOT_FOUND    = 3
      MISSING_AUTHORIZATION = 4
      NO_BOM_FOUND          = 5
      NO_PLANT_DATA         = 6
      NO_SUITABLE_BOM_FOUND = 7
      CONVERSION_ERROR      = 8
      OTHERS                = 9.

  LOOP AT LT_STPOX.
    DELETE IT_STPOX WHERE IDNRK EQ LT_STPOX-IDNRK.
  ENDLOOP.

ENDFORM.                    "EXPL_MAT
*&---------------------------------------------------------------------*
*&      Form  GET_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_ALV .

  DATA: LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        L_NAME      LIKE SY-CPROG,
        L_INDEX     LIKE SY-TABIX.

  PERFORM SET_ALV_TABLE .

  L_NAME = SY-CPROG.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = L_NAME     "程式名稱
      I_INTERNAL_TABNAME     = 'GT_ALV'  "
      I_INCLNAME             = SY-CPROG   "程式名稱
      I_CLIENT_NEVER_DISPLAY = 'X'        "固定X
      I_BYPASSING_BUFFER     = 'X'        "固定X
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT "
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

  APPEND LINES OF LT_FIELDCAT TO GT_FIELDCAT.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    L_INDEX = SY-TABIX.

    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'MATNR'.
        GS_FIELDCAT-SELTEXT_L = '主料'.
*        GS_FIELDCAT-COL_POS = 1.        "此欄位在ALV報表中的呈現順序
*      WHEN 'MAKTX'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-002.
*        GS_FIELDCAT-COL_POS = 2.        "此欄位在ALV報表中的呈現順序
*      WHEN 'BMENG'.
*        GS_FIELDCAT-SELTEXT_L  = TEXT-009.
*        GS_FIELDCAT-COL_POS    = 3.        "此欄位在ALV報表中的呈現順序
*        GS_FIELDCAT-DDICTXT    = 'S'.
*        GS_FIELDCAT-QFIELDNAME = 'MEINS'.
*      WHEN 'MEINS'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-010.
*        GS_FIELDCAT-COL_POS = 4.        "此欄位在ALV報表中的呈現順序
*      WHEN 'MATKL'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-011.
*        GS_FIELDCAT-COL_POS = 5.        "此欄位在ALV報表中的呈現順序
*      WHEN 'DISPO'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-012.
*        GS_FIELDCAT-COL_POS = 6.        "此欄位在ALV報表中的呈現順序
*      WHEN 'DISLS'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-013.
*        GS_FIELDCAT-COL_POS = 7.        "此欄位在ALV報表中的呈現順序
      WHEN 'PURTYPE'.
        GS_FIELDCAT-SELTEXT_L = '採購類型'.
*        GS_FIELDCAT-COL_POS = 8.        "此欄位在ALV報表中的呈現順序
      WHEN 'STUFE_C'.
        GS_FIELDCAT-SELTEXT_L = '階層'.
*      WHEN 'ANTXT'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-017.
*        GS_FIELDCAT-COL_POS = 9.
*      WHEN 'STUFE_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-003.
*        GS_FIELDCAT-COL_POS = 10.
*      WHEN 'POSTP_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-018.
*        GS_FIELDCAT-COL_POS = 11.
*      WHEN 'INDRK_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-004.
*        GS_FIELDCAT-COL_POS = 12.
*      WHEN 'MAKTX_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-005.
*        GS_FIELDCAT-COL_POS = 13.
*      WHEN 'BEIKZ_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-019.
*        GS_FIELDCAT-COL_POS = 14.
*      WHEN 'SCHGT_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-020.
*        GS_FIELDCAT-COL_POS = 15.
*      WHEN 'SANKA_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-021.
*        GS_FIELDCAT-COL_POS = 16.
      WHEN 'PURTYPE_C'.
        GS_FIELDCAT-SELTEXT_L = '元件採購類型'.
*        GS_FIELDCAT-COL_POS = 17.
*      WHEN 'DSPST_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-023.
*        GS_FIELDCAT-COL_POS = 18.
*      WHEN 'ITSOB_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-024.
*        GS_FIELDCAT-COL_POS = 19.
*      WHEN 'MNGLG_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-006.
*        GS_FIELDCAT-COL_POS = 20.
*      WHEN 'MENGE_C'.
*        GS_FIELDCAT-SELTEXT_L = TEXT-016.
*        GS_FIELDCAT-COL_POS = 21.
*      WHEN 'LABST_C'.
*        GS_FIELDCAT-SELTEXT_L = '可用庫存'.
*        GS_FIELDCAT-COL_POS = 22.
      WHEN 'MEINS_C'.
        GS_FIELDCAT-SELTEXT_L = '單位'.
*        GS_FIELDCAT-COL_POS = 23.
*      WHEN 'PARTL'.
*        GS_FIELDCAT-SELTEXT_L = '零件位置'.
        "GS_FIELDCAT-COL_POS = 23.
      WHEN 'IBESKZ'.
        GS_FIELDCAT-SELTEXT_L = '元件採購類型'.
      WHEN 'ISOBSL'.
        GS_FIELDCAT-SELTEXT_L = '元件特殊採購'.

    ENDCASE.
*
**-->下面這兩個參數則是表示所有欄位預設的參數
*
*    GS_FIELDCAT-DDICTXT = 'L'.   "
*    GS_FIELDCAT-KEY = SPACE.
*
    MODIFY GT_FIELDCAT FROM GS_FIELDCAT INDEX L_INDEX.
    CLEAR  GS_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " GET_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV .
  GS_LAYOUT-ZEBRA = 'X'.                    "顏色交替檢視
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.      "自動最佳化長度

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-CPROG           "此支程式的名稱
      IS_LAYOUT                = GS_LAYOUT          "ALV呈現相關參數
      IT_FIELDCAT              = GT_FIELDCAT        "ALV欄位呈現相關參數
*     IT_SORT                  = GT_SORT"AVL欄位相同資料不顯示
      I_SAVE                   = 'A'                "固定為A
*     I_CALLBACK_PF_STATUS_SET = 'STANDARD_FULLSCREEN'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
    TABLES
      T_OUTTAB                 = GT_ALV[]          "呈現資料的Table
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB..

  SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB .

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_ALV_TABLE.

  LOOP AT GT_HEADER.
    LOOP AT GT_ITEM WHERE MATNR = GT_HEADER-MATNR
                      AND STLAN = GT_HEADER-STLAN.

      MOVE-CORRESPONDING GT_HEADER TO GT_ALV.
      MOVE-CORRESPONDING GT_ITEM   TO GT_ALV.

      GT_ALV-BESKZ = GT_HEADER-BESKZ.
      GT_ALV-SOBSL = GT_HEADER-SOBSL.

      GT_ALV-IBESKZ = GT_ITEM-BESKZ.
      GT_ALV-ISOBSL = GT_ITEM-SOBSL.

      APPEND GT_ALV. CLEAR: GT_ALV.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " SET_ALV_TABLE
*&---------------------------------------------------------------------*
*& Form SET_ITEM
*&---------------------------------------------------------------------*
FORM SET_ITEM  USING    LW_STPOX LIKE STPOX.


  MOVE-CORRESPONDING LW_STPOX TO GT_ITEM.

  SELECT SINGLE BISMT INTO GT_ITEM-IDBIS
                      FROM MARA
                     WHERE MATNR = GT_ITEM-IDNRK.

  PERFORM GET_MAKTX USING    GT_ITEM-IDNRK
                    CHANGING GT_ITEM-OJTXB.

  SELECT SINGLE BESKZ SOBSL INTO ( GT_ITEM-BESKZ,GT_ITEM-SOBSL )
         FROM MARC
        WHERE MATNR = GT_ITEM-IDNRK
          AND WERKS = GT_ITEM-WERKS.


  GT_ITEM-WERKS = GT_HEADER-WERKS.
  GT_ITEM-MATNR = GT_HEADER-MATNR.
  GT_ITEM-STLAN = GT_HEADER-STLAN.


  APPEND GT_ITEM. CLEAR GT_ITEM.

ENDFORM.

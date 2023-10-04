************************************************************************
* Program Name      :  ZPPMIG0002
* Descriptions      :  途程元件指派
* Updates Tables    :
* Input  Parameters :
* Output Parameters :
* Return Codes      :
* Special Logic     :
* Includes          :
************************************************************************
REPORT ZPPMIG0002 NO STANDARD PAGE HEADING MESSAGE-ID 00.
************************************************************************
* Tables Definitions
************************************************************************
TABLES:MAST,MAPL.
************************************************************************
* Data Definitions
************************************************************************
DATA: GT_UPLOAD LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF GT_COMP_ALL OCCURS 0,
        WERKS LIKE MAST-WERKS,  "工廠
        MATNR LIKE MAST-MATNR,  "成/半品料號
        VORNR LIKE PLPO-VORNR,  "作業代碼
        LTXA1 LIKE PLPO-LTXA1,  "說明
        POSNR LIKE STPO-POSNR,  "BOM項目代碼
        IDNRK LIKE STPO-IDNRK,  "元件料號
      END OF GT_COMP_ALL.

DATA: BEGIN OF GT_LOG OCCURS 0.
        INCLUDE STRUCTURE GT_COMP_ALL.
DATA:   SEQNO TYPE N LENGTH 4,
        TYPE  TYPE C LENGTH 4,
        MESG  TYPE C LENGTH 100,
        SEL   TYPE C LENGTH 1,
        MAKTX LIKE MAKT-MAKTX,
        IDMKT LIKE MAKT-MAKTX,
        STLNR LIKE PLMZ-STLNR,  "BOM號碼
        STLAN LIKE MAST-STLAN,  "BOM使用
        STLAL LIKE PLMZ-STLAL,  "BOM替代
        PLNNR LIKE PLKO-PLNNR,
        PLNAL LIKE PLKO-PLNAL,
      END OF GT_LOG.

DATA: BEGIN OF GT_PLAN OCCURS 0,
        MATNR LIKE MARC-MATNR,
        WERKS LIKE MARC-WERKS,
        PLNNR LIKE PLKO-PLNNR,
        PLNAL LIKE PLKO-PLNAL,
      END OF GT_PLAN.

DATA: G_PLNTY LIKE MAPL-PLNTY VALUE 'N',
      G_PLNFL LIKE PLMZ-PLNFL VALUE '000000',
      G_STLAN LIKE MAST-STLAN VALUE '1',
      G_STLAL LIKE MAST-STLAL VALUE '01'.

*--ALV
TYPE-POOLS SLIS.
DATA: G_REPID   TYPE SY-REPID,
      GS_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
  PARAMETERS: P_FNAME LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* At Selection Screen
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FNAME.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      FILE_NAME = P_FNAME.


************************************************************************
* Main Process
************************************************************************
START-OF-SELECTION.
  PERFORM UPLOAD_EXCEL USING P_FNAME.
  PERFORM CHECK_DATA.
  PERFORM SHOW_RESULT.

END-OF-SELECTION.
************************************************************************
* Form
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM UPLOAD_EXCEL  USING IN_PATH.
  FIELD-SYMBOLS:<FS>.

  CLEAR: GT_UPLOAD[],GT_COMP_ALL[].

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = IN_PATH
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 200
      I_END_ROW               = 65536
    TABLES
      INTERN                  = GT_UPLOAD
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  SORT GT_UPLOAD BY ROW COL.

  LOOP AT GT_UPLOAD.
    ASSIGN COMPONENT GT_UPLOAD-COL OF STRUCTURE GT_COMP_ALL TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.
      APPEND GT_COMP_ALL. CLEAR GT_COMP_ALL.
    ENDAT.
  ENDLOOP.

  IF GT_COMP_ALL[] IS INITIAL.
    MESSAGE S001 WITH '檔案上傳失敗' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_data
*&---------------------------------------------------------------------*
FORM CHECK_DATA .
  DATA: L_INDEX TYPE I,
        LW_LOG  LIKE GT_LOG.

  LOOP AT GT_COMP_ALL.
    L_INDEX = SY-TABIX.

    CLEAR: LW_LOG.
    MOVE-CORRESPONDING GT_COMP_ALL TO LW_LOG.

    LW_LOG-SEQNO = L_INDEX.

*--> 檢查途程是否存在
    CLEAR: MAPL.
    SELECT SINGLE * FROM MAPL
                   WHERE MATNR = LW_LOG-MATNR
                     AND WERKS = LW_LOG-WERKS
                     AND PLNTY = G_PLNTY
                     AND LOEKZ = ''.

    IF SY-SUBRC <> 0.
      PERFORM SET_LOG USING LW_LOG
                            'E'
                            '途程不存在'.
      DELETE GT_COMP_ALL INDEX L_INDEX.
      CONTINUE.
    ELSE.
      LW_LOG-PLNNR = MAPL-PLNNR.
      LW_LOG-PLNAL = MAPL-PLNAL.
    ENDIF.

*--> 檢查BOM是否存在
    CLEAR: MAST.
    SELECT SINGLE * FROM MAST
                   WHERE MATNR = LW_LOG-MATNR
                     AND WERKS = LW_LOG-WERKS
                     AND STLAN = G_STLAN
                     AND STLAL = G_STLAL.

    IF SY-SUBRC <> 0.
      PERFORM SET_LOG USING LW_LOG
                            'E'
                            'BOM不存在'.
      DELETE GT_COMP_ALL INDEX L_INDEX.
      CONTINUE.
    ELSE.
      LW_LOG-STLNR = MAST-STLNR.
      LW_LOG-STLAL = MAST-STLAL.
    ENDIF.

*--> 取得說明
    SELECT SINGLE MAKTX INTO LW_LOG-MAKTX
                        FROM MAKT
                       WHERE MATNR = LW_LOG-MATNR
                         AND SPRAS = SY-LANGU.

    SELECT SINGLE MAKTX INTO LW_LOG-IDMKT
                        FROM MAKT
                       WHERE MATNR = LW_LOG-IDNRK
                         AND SPRAS = SY-LANGU.

    PERFORM SET_LOG USING LW_LOG
                          'S'
                          '資料檢核成功,待執行元件指派'.

*--> 統整途程資訊
    GT_PLAN-MATNR = MAPL-MATNR.
    GT_PLAN-WERKS = MAPL-WERKS.
    GT_PLAN-PLNNR = MAPL-PLNNR.
    GT_PLAN-PLNAL = MAPL-PLNAL.
    COLLECT GT_PLAN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_LOG  USING LW_LOG LIKE GT_LOG
                    IN_TYPE
                    IN_MESG.

  MOVE IN_MESG TO LW_LOG-MESG.

  IF IN_TYPE = 'S'.
    LW_LOG-TYPE = '@01@'. "成功
  ELSE.
    LW_LOG-TYPE = '@02@'. "錯誤
  ENDIF.

  APPEND LW_LOG TO GT_LOG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_RESULT
*&---------------------------------------------------------------------*
FORM SHOW_RESULT .

  PERFORM SET_ALV_FIELD.
  PERFORM ALV_DISPLAY .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM ALV_DISPLAY.

  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-BOX_FIELDNAME     = 'SEL'.
  GS_LAYOUT-BOX_TABNAME       = 'GT_LOG'.
  GS_LAYOUT-ZEBRA             = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT                = GS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT[]
    TABLES
      T_OUTTAB                 = GT_LOG.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ALV_FIELD
*&---------------------------------------------------------------------*
FORM SET_ALV_FIELD .


  PERFORM GET_FIELDNAME USING 'WERKS'   '工廠'.
  PERFORM GET_FIELDNAME USING 'MATNR'   '物料號碼'.
  PERFORM GET_FIELDNAME USING 'MAKTX'   '物料號碼'.
  PERFORM GET_FIELDNAME USING 'PLNNR'   '途程群組'.
  PERFORM GET_FIELDNAME USING 'PLNAL'   '群組計數器'.
  PERFORM GET_FIELDNAME USING 'VORNR'   '作業號碼'.
  PERFORM GET_FIELDNAME USING 'LTXA1'   '作業說明'.
  PERFORM GET_FIELDNAME USING 'STLNR'   '物料表'.
  PERFORM GET_FIELDNAME USING 'STLAN'   'BOM使用'.
  PERFORM GET_FIELDNAME USING 'STLAL'   'BOM替代'.
  PERFORM GET_FIELDNAME USING 'POSNR'   '元件項目'.
  PERFORM GET_FIELDNAME USING 'IDNRK'   '元件號碼'.
  PERFORM GET_FIELDNAME USING 'IDMKT'   '元件說明'.
  PERFORM GET_FIELDNAME USING 'TYPE'    '處理結果'.
  PERFORM GET_FIELDNAME USING 'MESG'    '訊息'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
FORM GET_FIELDNAME  USING  IN_STR  IN_FIL.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  LS_FIELDCAT-FIELDNAME   = IN_STR .
  LS_FIELDCAT-SELTEXT_M   = IN_FIL.

  CASE IN_STR.
    WHEN  'MATNR' OR 'WERKS' OR
         'PLNNR' OR 'PLNAL' OR 'VORNR'.
      LS_FIELDCAT-KEY = 'X'.
  ENDCASE.

  APPEND LS_FIELDCAT TO GT_FIELDCAT.
  CLEAR  LS_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
FORM SET_PF_STATUS USING TT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM USER_COMMAND  USING F_UCOMM     LIKE SY-UCOMM
                         FS_SELFIELD TYPE SLIS_SELFIELD.

  DATA : REF_GRID TYPE REF TO CL_GUI_ALV_GRID.

*----更新畫面--->
  IF REF_GRID IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        E_GRID = REF_GRID.
  ENDIF.

  IF NOT REF_GRID IS INITIAL.
    CALL METHOD REF_GRID->CHECK_CHANGED_DATA.
  ENDIF.


  CASE F_UCOMM.
    WHEN 'ALCT'.
      PERFORM CREATE_ASSIGNMENT.
  ENDCASE.

  FS_SELFIELD-REFRESH = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ASSIGNMENT
*&---------------------------------------------------------------------*
FORM CREATE_ASSIGNMENT .

  READ TABLE GT_LOG WITH KEY TYPE = '@02@'.

  IF SY-SUBRC = 0.
    MESSAGE S001 WITH '有檢核錯誤資料，無法執行元件指派作業'
                 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  LOOP AT GT_PLAN.
    PERFORM COMPONENTALLOCATION USING GT_PLAN-MATNR
                                      GT_PLAN-WERKS
                                      G_STLAL
                                      GT_PLAN-PLNNR
                                      GT_PLAN-PLNAL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form COMPONENTALLOCATION
*&---------------------------------------------------------------------*
FORM COMPONENTALLOCATION  USING I_MATNR
                                I_WERKS
                                I_STLAL
                                I_PLNNR
                                I_PLNAL.

  DATA: FT_ITM_TREE_DATA    TYPE STANDARD TABLE OF ITM_TREE_CLASS_DATA,
        FT_OPR_CLASS_DATA   TYPE STANDARD TABLE OF OPR_CLASS_DATA,
        FT_COM_CLASS_DATA   TYPE STANDARD TABLE OF COM_CLASS_DATA,
        FT_OPR_ALL_DATA_DEL TYPE CPCL_OPR_TAB_TYPE,
        FS_COM_IDENT_DELETE TYPE CMCL_COM_IDENT_TYPE,
        FS_COM_DATA         TYPE COM_CLASS_DATA.

  DATA: LT_STB  LIKE STPOX  OCCURS 0 WITH HEADER LINE,
        LT_LOG  LIKE GT_LOG OCCURS 0 WITH HEADER LINE,
        L_INDEX TYPE I,
        L_MESG  TYPE C LENGTH 100.

  FIELD-SYMBOLS: <ITMDATA>        LIKE LINE OF FT_ITM_TREE_DATA,
                 <OPRDATA>        LIKE LINE OF FT_OPR_CLASS_DATA,
                 <COM_CLASS_DATA> LIKE LINE OF FT_COM_CLASS_DATA,
                 <ASSIGN>         LIKE LINE OF GT_LOG.



*--> 讀取BOM & routing 到記憶體
  PERFORM LOAD_TASK_LIST USING I_MATNR
                               I_WERKS
                               I_PLNNR
                               I_PLNAL.
*--> 取得BOM結構
  CALL FUNCTION 'CP_CC_S_PROVIDE_ITM_BY_PATH'
    EXPORTING
      I_KEY_DATE_S              = SY-DATUM
      I_APPLICATION             = 'PP01'
      I_MATERIAL_ROOT           = I_MATNR
      I_PLANT_ROOT              = I_WERKS
      I_USAGE_ROOT              = G_STLAN
      I_ALTERNATIVE_ROOT        = I_STLAL
      I_FLG_PATH_EXPAND_TOTALLY = 'X'
    TABLES
      C_ITM_TREE_CLASS_DATA     = FT_ITM_TREE_DATA
    EXCEPTIONS
      KEY_DATE_REQUIRED_FOR_ECM = 1
      OTHERS                    = 2.


*--> 取得元件指派資訊
  CALL FUNCTION 'CP_CC_S_PROVIDE_COM_BY_OPR'
    EXPORTING
      I_DATE_FROM        = SY-DATUM
      I_DATE_TO          = SY-DATUM
      I_APPLICATION      = 'PP01'
      I_MATERIAL_ROOT    = I_MATNR
      I_PLANT_ROOT       = I_WERKS
      I_USAGE_ROOT       = G_STLAN
      I_ALTERNATIVE_ROOT = G_STLAL
      I_PLNTY            = G_PLNTY
      I_PLNNR            = I_PLNNR
      I_PLNAL            = I_PLNAL
      I_PLNFL            = G_PLNFL
    TABLES
      E_COM_CLASS_DATA   = FT_COM_CLASS_DATA
    EXCEPTIONS
      NO_AUTHORITY       = 1
      WRONG_KEY          = 2
      BOM_NOT_FOUND      = 3
      NO_VALID_BOM       = 4
      BOM_NOT_ACTIVE     = 5
      OTHERS             = 6.

*--> 刪除既有元件指派資訊
  IF SY-SUBRC EQ 0.
    "
    LOOP AT FT_COM_CLASS_DATA ASSIGNING <COM_CLASS_DATA>.
      CALL FUNCTION 'CP_CL_P_OPR_ALLOCATION_PROVIDE'
        EXPORTING
          I_DATE_FROM = SY-DATUM
          I_PLNTY     = <COM_CLASS_DATA>-PLNTY
          I_PLNNR     = <COM_CLASS_DATA>-PLNNR
          I_PLNKN     = <COM_CLASS_DATA>-PLNKN
        IMPORTING
          E_OPR_IDENT = FT_OPR_ALL_DATA_DEL
        EXCEPTIONS
          WRONG_KEY   = 1.

      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING <COM_CLASS_DATA> TO FS_COM_IDENT_DELETE.
*       delete current component allocation
        CALL FUNCTION 'CM_CL_P_COM_DELETE'
          EXPORTING
            I_KEY_DATE_S     = SY-DATUM
            I_COM_IDENT      = FS_COM_IDENT_DELETE
            I_OPR_ALLOC_DATA = FT_OPR_ALL_DATA_DEL[]
          EXCEPTIONS
            NO_VALID_COM     = 1
            PATH_INCOMPLETE  = 2.
      ENDIF.
    ENDLOOP.

    "save data (deletion of current assignment)
    CALL FUNCTION 'CP_CC_S_SAVE'.
    COMMIT WORK.
  ENDIF.


*--> 取得途程作業資訊
  CALL FUNCTION 'CP_CC_S_OPR_PROVIDE_BY_MTK'
    EXPORTING
      I_DATE_FROM      = SY-DATUM
      I_DATE_TO        = SY-DATUM
      I_MATERIAL       = I_MATNR
      I_PLANT          = I_WERKS
      I_PLNTY          = G_PLNTY
      I_PLNNR          = I_PLNNR
      I_PLNAL          = I_PLNAL
      I_PLNFL          = G_PLNFL
    TABLES
      E_OPR_CLASS_DATA = FT_OPR_CLASS_DATA
    EXCEPTIONS
      WRONG_KEY        = 1
      OTHERS           = 2.

  IF SY-SUBRC EQ 0.
    SORT FT_OPR_CLASS_DATA BY PLNAL PLNFL PLNTY PLNNR VORNR.
  ENDIF.

*--> bom expand
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = 'PP01'
      DATUV                 = SY-DATUM
      MEHRS                 = 'X'
      MTNRV                 = I_MATNR
      WERKS                 = I_WERKS
    TABLES
      STB                   = LT_STB
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
  IF SY-SUBRC <> 0.
    "Implement suitable error handling here
  ENDIF.


*--> create components allocation
  LT_LOG[] = GT_LOG[].

  LOOP AT LT_LOG WHERE TYPE <> '@02@'
                   AND MATNR = I_MATNR
                   AND WERKS = I_WERKS
                   AND PLNNR = I_PLNNR
                   AND PLNAL = I_PLNAL
                   AND STLAL = I_STLAL.
    L_INDEX = SY-TABIX.

    DELETE GT_LOG WHERE SEQNO = LT_LOG-SEQNO.

    CLEAR FS_COM_DATA.

    READ TABLE FT_ITM_TREE_DATA ASSIGNING <ITMDATA>
                                 WITH KEY STLNR = LT_LOG-STLNR
                                          STLAL = LT_LOG-STLAL
                                          POSNR = LT_LOG-POSNR.

    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING <ITMDATA> TO FS_COM_DATA.
    ENDIF.

    "read operation data
    READ TABLE FT_OPR_CLASS_DATA ASSIGNING <OPRDATA>
                                  WITH KEY PLNNR = LT_LOG-PLNNR
                                           PLNAL = LT_LOG-PLNAL
                                           PLNTY = G_PLNTY
                                           VORNR = LT_LOG-VORNR
                                           BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING <OPRDATA>  TO FS_COM_DATA.
      MOVE: <OPRDATA>-IDENT         TO FS_COM_DATA-IDENT.
      MOVE: <ITMDATA>-IDENT         TO FS_COM_DATA-IDENT_ITM.
      MOVE: <ITMDATA>-KANTE_K       TO FS_COM_DATA-KANTE.
      MOVE: <ITMDATA>-STLTY         TO FS_COM_DATA-STLTY.
      MOVE: <ITMDATA>-STLNR         TO FS_COM_DATA-STLNR.
      MOVE: <ITMDATA>-STLAL         TO FS_COM_DATA-STLAL.
      MOVE: <ITMDATA>-STVKN         TO FS_COM_DATA-STLKN.
      MOVE: I_MATNR                 TO FS_COM_DATA-BOMAT.
      MOVE: I_WERKS                 TO FS_COM_DATA-WERK_STL.

      READ TABLE LT_STB WITH KEY STLNR = FS_COM_DATA-STLNR
                                 STLAL = FS_COM_DATA-STLAL
                                 STLKN = FS_COM_DATA-STLKN.

      FS_COM_DATA-STLST = LT_STB-STUFE - 1.
      FS_COM_DATA-STLWG = LT_STB-VWEGX.

      "create allocation
      CALL FUNCTION 'CP_CC_S_CREATE_COM'
        EXPORTING
*         I_ECN_S                   = ' '
          I_KEY_DATE_S              = SY-DATUM
          I_COM_CLASS_DATA          = FS_COM_DATA
*       IMPORTING
*         E_ITM_LOCK                = E_ITM_LOCK
*         E_OPR_LOCK                = E_OPR_LOCK
*         E_OPR_IDENT_ALLOC_INV     = E_OPR_IDENT_ALLOC_INV
*         E_ZUONR                   = E_ZUONR
*         E_COM_HANDLE_DEL_TAB      = E_COM_HANDLE_DEL_TAB
*         E_ECM_DATA_ERROR_TYPE     = E_ECM_DATA_ERROR_TYPE
        EXCEPTIONS
          NO_AUTHORITY              = 1
          NO_VALID_OPERATION        = 2
          NO_VALID_ITEM             = 3
          OPERATION_ALREADY_LOCKED  = 4
          ITEM_ALREADY_LOCKED       = 5
          OPERATION_NOT_LOCKED      = 6
          ITEM_NOT_LOCKED           = 7
          ITEM_NOT_SPECIFIED        = 8
          OPERATION_NOT_SPECIFIED   = 9
          ECM_DATA_NOT_SUITABLE     = 10
          ALLOCATION_NOT_VALID      = 11
          LINK_TO_REF_SET_OF_OPR    = 12
          LINK_TO_WC_REF_SET_OF_OPR = 13
          OTHERS                    = 14.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 INTO L_MESG.

        PERFORM SET_LOG USING LT_LOG
                              'E'
                              L_MESG.
      ELSE.
        PERFORM SET_LOG USING LT_LOG
                              'S'
                              '元件指派成功'.
      ENDIF.
    ELSE.
      PERFORM SET_LOG USING LT_LOG
                            'E'
                            '資料讀取失敗'.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'CP_CC_S_SAVE'.
  COMMIT WORK.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  load_task_list2
*&---------------------------------------------------------------------*
FORM LOAD_TASK_LIST  USING  I_MATNR LIKE MARC-MATNR
                            I_WERKS LIKE MARC-WERKS
                            I_PLNNR LIKE PLKO-PLNNR
                            I_PLNAL LIKE PLKO-PLNAL.

  DATA: FS_CLASSES_IN_WORKAREA LIKE CLASSES_IN_WORKAREA,
        FS_TSK_PLNTY_SELECTION TYPE CPSC_PLNTY_TYPE,
        FT_TSK_PLNTY_SELECTION TYPE CPSC_PLNTY_TYPE OCCURS 0,
        FS_TSK_PLNNR_SELECTION TYPE CPSC_PLNNR_TYPE,
        FT_TSK_PLNNR_SELECTION TYPE CPSC_PLNNR_TYPE OCCURS 0,
        FS_TSK_PLNAL_SELECTION TYPE CPSC_PLNAL_TYPE,
        FT_TSK_PLNAL_SELECTION TYPE CPSC_PLNAL_TYPE OCCURS 0,
        FS_TSK_SELECTION       TYPE CPSC_TSK_SEL_TYPE,
        FS_MBM_SEL_TYPE        TYPE CPSC_MBM_SEL_TYPE,
        FS_MBM_MATNR           TYPE CPSC_MATNR_TYPE,
        FS_MBM_WERKS           TYPE CPSC_WERKS_TYPE,
        FS_MBM_VBELN           TYPE CPSC_VBELN_TYPE,
        FS_MBM_VBPOS           TYPE CPSC_POSNR_TYPE.


  CONCATENATE 'I' 'EQ' I_MATNR INTO FS_MBM_MATNR.
  APPEND FS_MBM_MATNR TO FS_MBM_SEL_TYPE-MATNR.

  CONCATENATE 'I' 'EQ' I_WERKS INTO FS_MBM_WERKS.
  APPEND FS_MBM_WERKS TO FS_MBM_SEL_TYPE-WERKS.

* objects for loading
  FS_CLASSES_IN_WORKAREA-MTK_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-TSK_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-SEQ_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-OPR_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-SUO_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-PRT_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-COM_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-ITM_INAREA = 'X'.
  FS_CLASSES_IN_WORKAREA-BOM_INAREA = 'X'.

* fill selection conditions - PLNTY
  CONCATENATE 'I' 'EQ' G_PLNTY INTO FS_TSK_PLNTY_SELECTION.
  APPEND FS_TSK_PLNTY_SELECTION TO FT_TSK_PLNTY_SELECTION.
  FS_TSK_SELECTION-PLNTY = FT_TSK_PLNTY_SELECTION.

  CONCATENATE 'I' 'EQ' I_PLNNR INTO FS_TSK_PLNNR_SELECTION.
  APPEND FS_TSK_PLNNR_SELECTION TO FT_TSK_PLNNR_SELECTION.
  FS_TSK_SELECTION-PLNNR = FT_TSK_PLNNR_SELECTION.

  CONCATENATE 'I' 'EQ' I_PLNAL INTO FS_TSK_PLNAL_SELECTION.
  APPEND FS_TSK_PLNAL_SELECTION TO FT_TSK_PLNAL_SELECTION.
  FS_TSK_SELECTION-PLNAL = FT_TSK_PLNAL_SELECTION.

* load objects into EWB
  CALL FUNCTION 'CP_CC_S_LOAD_COMPLEX_BY_TSK'
    EXPORTING
      I_CLASS                        = 'P'
      I_CLASSES_IN_WORKAREA          = FS_CLASSES_IN_WORKAREA
      I_CPSC_TSK_SEL                 = FS_TSK_SELECTION
      I_DATE_FROM                    = SY-DATUM
      I_DATE_TO                      = SY-DATUM
    EXCEPTIONS
      WORKAREA_NOT_FOUND             = 1
      WORKAREA_WRONG_TYPE            = 2
      CLASS_IN_WORKAREA_INCONSISTENT = 3
      WORKAREA_NOT_SPECIFIED         = 4
      OPR_NOT_FOUND                  = 5
      NO_SELECTION_CRITERIA          = 6
      INVALID_SELECTION_PERIOD       = 7
      KEY_DATE_REQUIRED_FOR_ECM      = 8
      OTHERS                         = 9.

  IF SY-SUBRC NE 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'CP_CC_S_LOAD_COMPLEX_BY_BOM'
    EXPORTING
      I_CLASS                        = 'P'
      I_CLASSES_IN_WORKAREA          = FS_CLASSES_IN_WORKAREA
      I_CPSC_MBM_SEL                 = FS_MBM_SEL_TYPE
      I_DATE_FROM                    = SY-DATUM
      I_DATE_TO                      = SY-DATUM
    EXCEPTIONS
      WORKAREA_NOT_FOUND             = 1
      CLASS_WRONG_TYPE               = 2
      WORKAREA_WRONG_TYPE            = 3
      CLASS_IN_WORKAREA_INCONSISTENT = 4
      WORKAREA_NOT_SPECIFIED         = 5
      BOM_NOT_FOUND                  = 6
      NO_SELECTION_CRITERIA          = 7
      INVALID_SELECTION_PERIOD       = 8
      KEY_DATE_REQUIRED_FOR_ECM      = 9
      OTHERS                         = 10.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.

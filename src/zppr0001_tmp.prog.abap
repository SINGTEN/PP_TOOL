************************************************************************
* Program Name      : ZPPR0001_TMP
* Descriptions      : 物料主檔清單
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
* 20230210 1.0  Derek         Original Create
*
************************************************************************
REPORT ZPPR0001_TMP NO STANDARD PAGE HEADING
                MESSAGE-ID 00 LINE-SIZE 180 LINE-COUNT 65.
************************************************************************
* Tables Definitions
************************************************************************
TABLES: MARA, MARC, MAKT, MAST, MAPL, EINA, EINE, MBEW, PLPO, A017,
        MKAL, PLKO, EORD, STPO, T001K, T001 , T023T , T134T.

************************************************************************
* Data Definitions
************************************************************************
DATA: BEGIN OF GT_DATA OCCURS 0,
*--> 基本view
        MATNR      LIKE MARA-MATNR,
        MTART      LIKE MARA-MTART,
        MTBEZ      LIKE T134T-MTBEZ,   "物料類型說明
        MAKTX      LIKE MAKT-MAKTX,    "料號說明
        EMAKT      LIKE MAKT-MAKTX,    "英文說明
        MEINS      LIKE MARA-MEINS,    "基礎單位
        BISMT      LIKE MARA-BISMT,    "舊物料號碼
        MATKL      LIKE MARA-MATKL,    "物料群組
        WGBEZ      LIKE T023T-WGBEZ,   "物料群組說明
        XCHPF      LIKE MARA-XCHPF,    "工廠批次
        SPART      LIKE MARA-SPART,    "部門
        BRGEW      LIKE MARA-BRGEW,   "毛重
        NTGEW      LIKE MARA-NTGEW,   "淨重
        GEWEI      LIKE MARA-GEWEI,   "重量單位
        AESZN      LIKE MARA-AESZN,   "線徑
        GROES      LIKE MARA-GROES,   "Dimensions
        ZEINR      LIKE MARA-ZEINR,    "HSCode
        ZEIVR      LIKE MARA-ZEIVR,   "圖面版次
        MSTAE      LIKE MARA-MSTAE,   "跨廠物料狀態
        LONGTEXT1  TYPE C LENGTH 160, "表面處理
        LONGTEXT2  TYPE C LENGTH 160, "熱處理條件
        LONGTEXT3  TYPE C LENGTH 160, "發票英文品名
        NORMT      LIKE MARA-NORMT,   "標準說明(暫時備註)

*--> 採購view
        EKGRP      LIKE MARC-EKGRP,    "採購群組
        BSTME      LIKE MARA-BSTME,    "訂購單位
        KAUTB      LIKE MARC-KAUTB,    "自動訂購允許
        KORDB      LIKE MARC-KORDB,    "貨源清單需求
        EKWSL      LIKE MARA-EKWSL,    "採購值代碼

*--> Salse view
        VKORG      LIKE MVKE-VKORG,
        VTWEG      LIKE MVKE-VTWEG,
        DWERK      LIKE MVKE-DWERK,
        TAXM1      LIKE MLAN-TAXM1,
        AUMNG      LIKE MVKE-AUMNG,
        MSTAV      LIKE MARA-MSTAV,
        MSTDV      LIKE MARA-MSTDV,
        VMSTA      LIKE MVKE-VMSTA,
        VMSTD      LIKE MVKE-VMSTD,
        KTGRM      LIKE MVKE-KTGRM,
        MTPOS      LIKE MVKE-MTPOS,
        MVGR1      LIKE MVKE-MVGR1,
        MVGR2      LIKE MVKE-MVGR2,
        MVGR3      LIKE MVKE-MVGR3,
        MVGR4      LIKE MVKE-MVGR4,
        MVGR5      LIKE MVKE-MVGR5,
        TRAGR      LIKE MARA-TRAGR,
        LADGR      LIKE MARC-LADGR,

*--> MRP view
        WERKS      LIKE MARC-WERKS,    "工廠
        MMSTA      LIKE MARC-MMSTA,    "工廠特定狀態
        DISMM      LIKE MARC-DISMM,    "MRP 類型
        MINBE      LIKE MARC-MINBE,    "再訂購點
        DISGR      LIKE MARC-DISGR,    "MRP 群組
        DISPO      LIKE MARC-DISPO,    "MRP 控制員
        DISLS      LIKE MARC-DISLS,    "批量
        BSTFE      LIKE MARC-BSTFE,   "固定批量
        BSTMI      LIKE MARC-BSTMI,   "最小批量
        BSTMA      LIKE MARC-BSTMA,   "最大批量
        BSTRF      LIKE MARC-BSTRF,    "捨入值
        AUSSS      LIKE MARC-AUSSS,    "裝配廢品
        BESKZ      LIKE MARC-BESKZ,    "採購類型
        SOBSL      LIKE MARC-SOBSL,    "特殊採購類型
        LGFSB      LIKE MARC-LGFSB,    "外部採購的預設儲存位置
        LGPRO      LIKE MARC-LGPRO,    "發貨儲存地點
        RGEKZ      LIKE MARC-RGEKZ,    "倒扣指示
        SCHGT      LIKE MARC-SCHGT,    "散裝物料
        DZEIT      LIKE MARC-DZEIT,    "廠內生產時間
        PLIFZ      LIKE MARC-PLIFZ,    "計劃交貨時間〈天〉
        WEBAZ      LIKE MARC-WEBAZ,    "收貨作業處理時間
        EISBE      LIKE MARC-EISBE,    "安全庫存
        MTVFP      LIKE MARC-MTVFP,    "可用度檢查
        SBDKZ      LIKE MARC-SBDKZ,    "個別及彙總
        KAUSF      LIKE MARC-KAUSF,    "零件廢品 (%)
        KZAUS      LIKE MARC-KZAUS,    "中止指示碼
        AUSDT      LIKE MARC-AUSDT,    "失效日期
        NFMAT      LIKE MARC-NFMAT,    "後續物料
        UNETO      LIKE MARC-UNETO,    "交貨不足允差限制
        UEETO      LIKE MARC-UEETO,    "過量交貨允差限制


        INSMK      LIKE MARC-INSMK,    "過帳至檢驗庫存

        NCOST      LIKE MARC-NCOST,    "不執行成本計算

*-->會計/成本檢視
        BKLAS      LIKE MBEW-BKLAS,    "評價類別
        EKLAS      LIKE MBEW-EKLAS,    "評價類別
        MLAST      LIKE MBEW-MLAST,    "價格決定
        STPRS      LIKE MBEW-STPRS,    "標準價格
        VPRSV      LIKE MBEW-VPRSV,    "價格控制
        PEINH      LIKE MBEW-PEINH,    "價格單位

        EKALR      LIKE MBEW-EKALR,    "含QS成本估算
        HKMAT      LIKE MBEW-HKMAT,    "物料來源
        HRKFT      LIKE MBEW-HRKFT,    "來源群組
        PRCTR      LIKE MARC-PRCTR,    "利潤中心
        LOSGR      LIKE MARC-LOSGR,    "成本計算批量

        ZPLP1      LIKE MBEW-ZPLP1,    "計劃價格1
        ZPLD1      LIKE MBEW-ZPLD1,    "計劃價格日期1
        ZPLP2      LIKE MBEW-ZPLP2,    "計劃價格2
        ZPLD2      LIKE MBEW-ZPLD2,    "計劃價格日期2
        WAERS      LIKE T001-WAERS,    "公司代碼幣別
        VERPR      LIKE MBEW-VERPR,    "定期單價(移動平均價)

*--> QM view
        ART01      TYPE C LENGTH 1,
        ACT01      TYPE C LENGTH 1,
        ART0130    TYPE C LENGTH 1,
        ACT0130    TYPE C LENGTH 1,
        ART0130Z   TYPE C LENGTH 1,
        ACT0130Z   TYPE C LENGTH 1,
        ART03      TYPE C LENGTH 1,
        ACT03      TYPE C LENGTH 1,
        ART05      TYPE C LENGTH 1,
        ACT05      TYPE C LENGTH 1,
        ART89      TYPE C LENGTH 1,
        ACT89      TYPE C LENGTH 1,
        PRFRQ      LIKE MARC-PRFRQ,   "間隔天數

*--> 其他資訊
        MASTS      TYPE C LENGTH 4,     "BOM 判斷
        MAPLS      TYPE C LENGTH 4,     "途程判斷
        VERID      TYPE C LENGTH 4,    "生產版本判斷
        EINAS      TYPE C LENGTH 4,    "價格主檔判斷
        EORDS      TYPE C LENGTH 4,    "檢查貨源清單
        "MBEWS TYPE C LENGTH 4,
*        QM01S  TYPE C LENGTH 4,
        INFNR      LIKE EINA-INFNR,    "採購資訊紀錄
        ERSDA      LIKE MARA-ERSDA,
        ERNAM      LIKE MARA-ERNAM,
        LAEDA      LIKE MARA-LAEDA,
        AENAM      LIKE MARA-AENAM,

        AVG_WEIGHT TYPE P LENGTH 8 DECIMALS 4, "單重
        BOM_WHUSER TYPE C LENGTH 4, "有上階
        BOM_NUM    TYPE I,          "上皆數量

      END OF GT_DATA.

DATA: GT_CP  LIKE GT_DATA OCCURS 0 WITH HEADER LINE.
DATA: GT_MBEW LIKE MBEW OCCURS 0 WITH HEADER LINE.

"DATA: G_IDX       LIKE SY-TABIX.
DATA: G_ALT_PLANT LIKE MARC-WERKS.
DATA: G_LIFNR     LIKE EORD-LIFNR.
RANGES: R_BOM FOR MARA-MATNR,
        R_CRBOM FOR MARA-MATNR.

*--> ALV 變數宣告 (REUSE_ALV_GRID_DISPLAY)
TYPE-POOLS: SLIS.
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV.

*--> itab for BDC
DATA: BEGIN OF BDCTAB OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA END OF BDCTAB.

************************************************************************
* Includes Module
************************************************************************
*INCLUDE :

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-H01.
  PARAMETERS : P_WERKS LIKE MARC-WERKS OBLIGATORY DEFAULT '1101'.
  SELECT-OPTIONS : S_MATNR FOR MARC-MATNR,
                   S_BISMT FOR MARA-BISMT,
                   S_MTART FOR MARA-MTART,
                   "S_GROES FOR MARA-GROES,
                   "S_WRKST FOR MARA-WRKST,
                   S_SPART FOR MARA-SPART,
                   S_MATKL FOR MARA-MATKL,
                   S_DISPO FOR MARC-DISPO,
                   S_DISMM FOR MARC-DISMM,
                   S_BESKZ FOR MARC-BESKZ,
                   S_SOBSL FOR MARC-SOBSL,
                   S_DISGR FOR MARC-DISGR,
                   S_MSTAE FOR MARA-MSTAE,
                   S_MMSTA FOR MARC-MMSTA,
                   S_ERSDA FOR MARA-ERSDA,
                   S_ERNAM FOR MARA-ERNAM,
                   S_LAEDA FOR MARA-LAEDA,
                   S_AENAM FOR MARA-AENAM.
SELECTION-SCREEN END OF BLOCK BLOCK1.


SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-H02.
  PARAMETERS : P_BOM AS CHECKBOX.            " 展BOM
SELECTION-SCREEN END OF BLOCK BLOCK2.

************************************************************************
* Initialization
************************************************************************
INITIALIZATION.

************************************************************************
* At Selection Screen Output
************************************************************************
AT SELECTION-SCREEN OUTPUT.

************************************************************************
* At Selection Screen
************************************************************************
AT SELECTION-SCREEN.

*AT USER-COMMAND.
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
************************************************************************
* Forms
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA .
*--> 替代工廠
*  IF P_WERKS = 'MO10'.
*    G_ALT_PLANT = 'MO11'.
*  ELSE.
*    G_ALT_PLANT = 'MO10'.
*  ENDIF.

*--> 篩選條件資料取得
  PERFORM GET_MATERIAL_DETAIL TABLES S_MATNR
                               USING '1'
                                     P_WERKS.
*--> 展BOM處理
  IF P_BOM EQ 'X'.
    PERFORM EXPO_BOM.
*
    IF R_BOM[] IS NOT INITIAL.
      PERFORM GET_MATERIAL_DETAIL TABLES R_BOM
                                   USING '2'
                                         P_WERKS.
    ENDIF.

*    IF R_CRBOM[] IS NOT INITIAL.
*      PERFORM GET_MATERIAL_DETAIL TABLES R_CRBOM
*                                   USING '3'
*                                         G_ALT_PLANT.
*    ENDIF.
  ENDIF.

  IF GT_DATA[] IS INITIAL.
    MESSAGE S001(00) WITH 'No DATA!!' DISPLAY LIKE 'E'.
    STOP.
  ELSE.

  ENDIF.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA
*&---------------------------------------------------------------------*
FORM WRITE_DATA.

  SORT GT_DATA BY MATNR.

  PERFORM SHOW_ALV_REPORT.

ENDFORM.                    " WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  EXPO_BOM
*&---------------------------------------------------------------------*
FORM EXPO_BOM.

  LOOP AT GT_DATA.

    IF GT_DATA-BESKZ EQ 'E' OR
      ( GT_DATA-BESKZ EQ 'F' AND GT_DATA-SOBSL EQ '30' ).

      PERFORM GET_BOM_DATA USING  GT_DATA-WERKS
                                  GT_DATA-MATNR.

*    ELSEIF GT_DATA-BESKZ = 'F' AND GT_DATA-SOBSL = 'Z1'.
*      PERFORM GET_BOM_DATA USING G_ALT_PLANT
*                                 GT_DATA-MATNR.
*
*      PERFORM SET_ALT_MAT USING GT_DATA-MATNR.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " EXPO_BOM

*&---------------------------------------------------------------------*
*&      Form  GET_BOM_DATA
*&---------------------------------------------------------------------*
FORM GET_BOM_DATA  USING    IN_WERKS
                            IN_MATNR.
  DATA: LT_STB    LIKE STPOX  OCCURS 0 WITH HEADER LINE,
        LT_MATCAT LIKE CSCMAT OCCURS 0 WITH HEADER LINE.

  DATA: LS_DATA LIKE GT_DATA,
        L_INDEX TYPE I.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = 'PP01'
      DATUV                 = SY-DATUM
*     EHNDL                 = '1'
      EMENG                 = 1
      MKTLS                 = 'X'
      MEHRS                 = 'X'
      "MDMPS                 = 'X' " 虛擬階
      MTNRV                 = IN_MATNR
      STLAL                 = '01'
      STLAN                 = '1'
      WERKS                 = IN_WERKS
    TABLES
      STB                   = LT_STB
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

*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*--> 排除已存在資料
  LOOP AT LT_STB.
    L_INDEX = SY-TABIX.

    READ TABLE GT_DATA INTO LS_DATA WITH KEY MATNR = LT_STB-IDNRK
                                             WERKS = LT_STB-WERKS.
    IF SY-SUBRC = 0.
      DELETE LT_STB INDEX L_INDEX.
    ENDIF.
  ENDLOOP.



**--> 跨廠調撥
*  LOOP AT LT_STB WHERE SOBSL = 'Z1'.
*
*    READ TABLE R_CRBOM WITH KEY LOW = LT_STB-IDNRK.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      PERFORM SET_ALT_MAT USING LT_STB-IDNRK.
*
*      PERFORM EXPL_MAT TABLES LT_STB
*                       USING  LT_STB-IDNRK.
*    ENDIF.
*  ENDLOOP.


  IF LT_STB[] IS NOT INITIAL.
    LOOP AT LT_STB WHERE WERKS = P_WERKS.
      READ TABLE R_BOM WITH KEY LOW = LT_STB-IDNRK.
      IF SY-SUBRC IS NOT INITIAL.
        PERFORM SET_ORIG_MAT USING LT_STB-IDNRK.
      ENDIF.
    ENDLOOP.


    LOOP AT LT_STB WHERE WERKS =  G_ALT_PLANT.
      READ TABLE R_CRBOM WITH KEY LOW = LT_STB-IDNRK.
      IF SY-SUBRC IS NOT INITIAL.
        PERFORM SET_ALT_MAT USING LT_STB-IDNRK.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_BOM_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV_REPORT
*&---------------------------------------------------------------------*
FORM SHOW_ALV_REPORT .
*--> ALV 變數宣告 (REUSE_ALV_GRID_DISPLAY)
  DATA: LS_LAYOUT    TYPE SLIS_LAYOUT_ALV.
  DATA: LS_TABNAME   TYPE SLIS_TABNAME VALUE 'GT_DATA'.

  PERFORM CREATE_FIELD_CATALOG .
  LS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LS_LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-CPROG
      IS_LAYOUT                = LS_LAYOUT
      IT_FIELDCAT              = GT_FIELDCAT[]
      "I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
      I_SAVE                   = 'A'
    TABLES
      T_OUTTAB                 = GT_DATA[]
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

ENDFORM.                    " SHOW_ALV_REPORT
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB..

  SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB .

ENDFORM.                    " PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM CREATE_FIELD_CATALOG .
  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

*--將itab 的欄位結構轉為alv 報表所需的表頭格式
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_INTERNAL_TABNAME     = 'GT_DATA'
      I_INCLNAME             = SY-REPID
      "I_CLIENT_NEVER_DISPLAY = 'X'
      I_BYPASSING_BUFFER     = 'X'
    CHANGING
      CT_FIELDCAT            = GT_FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

*--> 調整已產生的 ALV Title
  PERFORM SET_ALV_TITLE.

ENDFORM.                    " CREATE_FIELD_CATALOG

*&---------------------------------------------------------------------*
*&      Form  SET_ALV_TITLE
*&---------------------------------------------------------------------*
FORM SET_ALV_TITLE .
  DATA: L_IDX LIKE SY-TABIX.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
    L_IDX = SY-TABIX.

    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'MAKTX'.
        PERFORM CONVERT_FIELD USING '物料說明' '' ''.

      WHEN 'EMAKT'.
        PERFORM CONVERT_FIELD USING '英文說明' '' ''.

      WHEN 'VERID'.
        PERFORM CONVERT_FIELD USING '生產版本' '' ''.
*      WHEN 'STPRS'.                          "標準價格
*        GS_FIELDCAT-CFIELDNAME = 'WAERS'.    "依欄位參考幣別
*        GS_FIELDCAT-NO_ZERO    = 'X'.

      WHEN 'VERPR'.

        GS_FIELDCAT-CFIELDNAME = 'WAERS'.    "依欄位參考幣別
        GS_FIELDCAT-NO_ZERO    = 'X'.

      WHEN 'PEINH'.
        GS_FIELDCAT-NO_ZERO    = 'X'.

      WHEN 'LOSGR'.
        PERFORM CONVERT_FIELD USING '成本計算批量' '' ''.
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.    "依欄位參考單位
        GS_FIELDCAT-NO_ZERO    = 'X'.

      WHEN 'BMENG'.
        GS_FIELDCAT-QFIELDNAME = 'MEINS'.    "依欄位參考單位
        GS_FIELDCAT-NO_ZERO    = 'X'.

      WHEN 'LONGTEXT1'.
        PERFORM CONVERT_FIELD USING '表面處理'    '' ''.

      WHEN 'LONGTEXT2'.
        PERFORM CONVERT_FIELD USING '熱處理條件'    '' ''.

      WHEN 'LONGTEXT3'.
        PERFORM CONVERT_FIELD USING '發票英文品名'    '' ''.


      WHEN 'ZPLP1' OR 'ZPLP2'.
        GS_FIELDCAT-NO_ZERO    = 'X'.

      WHEN 'MASTS'.
        PERFORM CONVERT_FIELD USING 'BOM'    '' ''.
      WHEN 'MAPLS'.
        PERFORM CONVERT_FIELD USING '途程'   '' ''.
      WHEN 'EINAS'.
        PERFORM CONVERT_FIELD USING '採購資訊紀錄' '' ''.
      WHEN 'EORDS'.
        PERFORM CONVERT_FIELD USING '貨源清單' '' ''.
*      WHEN 'MBEWS'.  "PERFORM CONVERT_FIELD USING TEXT-T13   '' ''.
*      WHEN 'MMSTA'.  "PERFORM CONVERT_FIELD USING TEXT-T30   '' ''.
*      WHEN 'INFNR'. " PERFORM CONVERT_FIELD USING TEXT-T31   '' ''.
*      WHEN 'ERROR'.  "PERFORM CONVERT_FIELD USING TEXT-T32   '' ''.
*      WHEN 'WGBEZ'.  PERFORM CONVERT_FIELD USING TEXT-T37 '' ''.
*      WHEN 'MTBEZ'.  PERFORM CONVERT_FIELD USING TEXT-T38 '' ''.

      WHEN 'ART01'.
        PERFORM CONVERT_FIELD USING '檢驗類型01' '' ''.

      WHEN 'ART0130'.
        PERFORM CONVERT_FIELD USING '檢驗類型0130' '' ''.

      WHEN 'ART0130Z'.
        PERFORM CONVERT_FIELD USING '檢驗類型0130Z' '' ''.

      WHEN 'ART03'.
        PERFORM CONVERT_FIELD USING '檢驗類型03' '' ''.

      WHEN 'ART05'.
        PERFORM CONVERT_FIELD USING '檢驗類型05' '' ''.

      WHEN 'ART89'.
        PERFORM CONVERT_FIELD USING '檢驗類型89' '' ''.


      WHEN 'ACT01'.
        PERFORM CONVERT_FIELD USING '01啟用' '' ''.

      WHEN 'ACT0130'.
        PERFORM CONVERT_FIELD USING '0130啟用' '' ''.

      WHEN 'ACT0130Z'.
        PERFORM CONVERT_FIELD USING '0130Z啟用' '' ''.

      WHEN 'ACT03'.
        PERFORM CONVERT_FIELD USING '03啟用' '' ''.

      WHEN 'ACT05'.
        PERFORM CONVERT_FIELD USING '05啟用' '' ''.

      WHEN 'ACT89'.
        PERFORM CONVERT_FIELD USING '89啟用' '' ''.

      WHEN 'AVG_WEIGHT'.
        PERFORM CONVERT_FIELD USING '單重' '' ''.

      WHEN 'BOM_WHUSER'.
        PERFORM CONVERT_FIELD USING '有上階' '' ''.

      WHEN 'BOM_NUM'.
        PERFORM CONVERT_FIELD USING '上階有效數量' '' ''.

*      WHEN 'NORMT'.
*        PERFORM CONVERT_FIELD USING '暫時備註' '' ''.

      WHEN 'MANDT'.
        DELETE GT_FIELDCAT INDEX L_IDX.
        CONTINUE.
    ENDCASE.

    MODIFY GT_FIELDCAT FROM GS_FIELDCAT INDEX L_IDX.
  ENDLOOP.

ENDFORM.                    " SET_ALV_TITLE

*&---------------------------------------------------------------------*
*&      Form  CONVERT_FIELD
*&---------------------------------------------------------------------*
FORM CONVERT_FIELD  USING    IN_TEXT IN_KEY IN_POS.

  IF IN_TEXT IS NOT INITIAL.
    GS_FIELDCAT-SELTEXT_L    = IN_TEXT.
    GS_FIELDCAT-SELTEXT_M    = IN_TEXT.
    GS_FIELDCAT-SELTEXT_S    = IN_TEXT.
    GS_FIELDCAT-REPTEXT_DDIC = IN_TEXT.
  ENDIF.

  GS_FIELDCAT-KEY          = IN_KEY.

  IF IN_POS IS NOT INITIAL.
    GS_FIELDCAT-COL_POS      = IN_POS.
  ENDIF.

ENDFORM.                    " CONVERT_FIELD

*&---------------------------------------------------------------------*
*& Form CHECK_PROD_VERSION
*&---------------------------------------------------------------------*
FORM CHECK_PROD_VERSION CHANGING LW_DATA LIKE GT_DATA.

*  IF LW_DATA-BESKZ EQ 'E' OR LW_DATA-BESKZ EQ 'X'.
  SELECT SINGLE * FROM MKAL WHERE MATNR EQ LW_DATA-MATNR
                              AND WERKS EQ LW_DATA-WERKS
                              "AND VERID EQ '0001'
                              AND PRFG_S EQ '1'.

  IF SY-SUBRC <> 0.
    LW_DATA-VERID = '@02@'.
  ELSE.
    LW_DATA-VERID = '@01@'.
  ENDIF.
*  ENDIF.
*
*
*  IF LW_DATA-BESKZ EQ 'F' AND LW_DATA-SOBSL EQ '30'.
*    SELECT SINGLE * FROM MKAL WHERE MATNR EQ LW_DATA-MATNR
*                                AND WERKS EQ LW_DATA-WERKS
*                                AND  VERID EQ '0003'
*                                AND PRFG_S EQ '1'.
*    IF SY-SUBRC <> 0.
*      LW_DATA-VERID = '@02@'.
*    ELSE.
*      LW_DATA-VERID = '@01@'.
*    ENDIF.
*  ENDIF.
*
*  IF LW_DATA-BESKZ EQ 'E' AND LW_DATA-SOBSL EQ '50'.
*    SELECT SINGLE * FROM MKAL WHERE MATNR EQ LW_DATA-MATNR
*                                AND WERKS EQ LW_DATA-WERKS
*                                AND VERID EQ '0004'
*                                AND PRFG_S EQ '1'.
*    IF SY-SUBRC <> 0.
*      LW_DATA-VERID = '@02@'.
*    ELSE.
*      LW_DATA-VERID = '@01@'.
*    ENDIF.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXPL_MAT
*&---------------------------------------------------------------------*
FORM EXPL_MAT  TABLES   IT_STB STRUCTURE STPOX
               USING    IN_MATNR.

  DATA: LT_STB    LIKE STPOX  OCCURS 0 WITH HEADER LINE,
        LT_MATCAT LIKE CSCMAT OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      CAPID                 = 'PP01'
      DATUV                 = SY-DATUM
      EMENG                 = 1
      MKTLS                 = 'X'
      MEHRS                 = 'X'
      MTNRV                 = IN_MATNR
      STLAL                 = '01'
      STLAN                 = '1'
      WERKS                 = G_ALT_PLANT
    TABLES
      STB                   = LT_STB
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

  APPEND LINES OF LT_STB TO IT_STB.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MATERIAL_DETAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_MATERIAL_DETAIL  TABLES IT_MATNR STRUCTURE R_BOM
                          USING IN_TYPE
                                IN_WERKS.

  DATA: L_PLNNR LIKE MAPL-PLNNR,
        L_INDEX TYPE I.
  DATA: LT_DATA LIKE GT_DATA OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF LT_QMAT OCCURS 0,
          WERKS LIKE QMAT-WERKS,
          MATNR LIKE QMAT-MATNR,
          ART   LIKE QMAT-ART,
        END OF LT_QMAT.

  DATA: BEGIN OF LT_MARM OCCURS 0,
          MATNR LIKE MARM-MATNR,
          UMREZ LIKE MARM-UMREZ,  "分子
          UMREN LIKE MARM-UMREN,  "分母
        END OF LT_MARM  .


  IF IN_TYPE = '1'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_DATA
      FROM MARC LEFT OUTER JOIN MBEW ON MARC~MATNR EQ MBEW~MATNR
                                    AND MARC~WERKS EQ MBEW~BWKEY
                LEFT OUTER JOIN MVKE ON MARC~MATNR EQ MVKE~MATNR
                LEFT OUTER JOIN MLAN ON MARC~MATNR EQ MLAN~MATNR
                INNER JOIN MARA ON MARA~MATNR EQ MARC~MATNR
     WHERE MARA~MATNR IN IT_MATNR
       AND MARA~BISMT IN S_BISMT
       AND MARA~MTART IN S_MTART
       AND MARA~SPART IN S_SPART
       AND MARA~MATKL IN S_MATKL
       AND MARA~MSTAE IN S_MSTAE
       AND MARC~WERKS EQ IN_WERKS
       AND MARC~DISMM IN S_DISMM
       AND MARC~DISPO IN S_DISPO
       AND MARC~BESKZ IN S_BESKZ
       AND MARC~SOBSL IN S_SOBSL
       AND MARC~DISGR IN S_DISGR
       AND MARC~MMSTA IN S_MMSTA
       AND MARA~ERSDA IN S_ERSDA
       AND MARA~ERNAM IN S_ERNAM
       AND MARA~LAEDA IN S_LAEDA
       AND MARA~AENAM IN S_AENAM
       AND MARA~LVORM EQ SPACE
       AND MARC~LVORM EQ SPACE.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_DATA
      FROM MARC LEFT OUTER JOIN MBEW ON MARC~MATNR EQ MBEW~MATNR
                                    AND MARC~WERKS EQ MBEW~BWKEY
                LEFT OUTER JOIN MVKE ON MARC~MATNR EQ MVKE~MATNR
                LEFT OUTER JOIN MLAN ON MARC~MATNR EQ MLAN~MATNR
                INNER JOIN MARA ON MARA~MATNR EQ MARC~MATNR
     WHERE MARA~MATNR IN IT_MATNR
       AND MARC~WERKS EQ IN_WERKS
       AND MARA~LVORM EQ SPACE
       AND MARC~LVORM EQ SPACE.
  ENDIF.

  CHECK LT_DATA[] IS NOT INITIAL.

  "取得公司代碼
  SELECT SINGLE *
    FROM T001K
   WHERE BWKEY = IN_WERKS.

  "取得公司代碼幣別
  SELECT SINGLE *
    FROM T001
   WHERE BUKRS = T001K-BUKRS.


  SELECT MATNR WERKS ART INTO CORRESPONDING FIELDS OF TABLE LT_QMAT
           FROM QMAT
           FOR ALL ENTRIES IN LT_DATA
          WHERE MATNR = LT_DATA-MATNR
            AND WERKS = LT_DATA-WERKS.

  SORT LT_QMAT BY WERKS MATNR.

  SELECT MATNR UMREZ UMREN INTO CORRESPONDING FIELDS OF TABLE LT_MARM
         FROM MARM
         FOR ALL ENTRIES IN LT_DATA
        WHERE MATNR = LT_DATA-MATNR
          AND MEINH = 'KG'
          AND KZWSO = 'B'.

  SORT LT_DATA BY MATNR .

  LOOP AT LT_DATA.
    L_INDEX = SY-TABIX.

    READ TABLE GT_DATA WITH KEY WERKS = LT_DATA-WERKS
                                MATNR = LT_DATA-MATNR.

    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.


*--> 取得料號說明
    SELECT SINGLE MAKTX INTO LT_DATA-MAKTX
                        FROM MAKT
                       WHERE MATNR EQ LT_DATA-MATNR
                         AND SPRAS EQ 'M'.

    SELECT SINGLE MAKTX INTO LT_DATA-EMAKT
                        FROM MAKT
                       WHERE MATNR EQ LT_DATA-MATNR
                         AND SPRAS EQ 'E'.

*--> 新增物料類型&物料群組說明
    SELECT SINGLE WGBEZ INTO LT_DATA-WGBEZ
                        FROM T023T
                       WHERE MATKL EQ LT_DATA-MATKL
                         AND SPRAS EQ SY-LANGU.

    SELECT SINGLE MTBEZ INTO LT_DATA-MTBEZ
                        FROM T134T
                       WHERE MTART EQ LT_DATA-MTART
                         AND SPRAS EQ SY-LANGU.


*--> 檢查BOM
    IF LT_DATA-BESKZ EQ 'E' OR
        ( LT_DATA-BESKZ EQ 'F' AND LT_DATA-SOBSL EQ '30' ).

      SELECT SINGLE * FROM MAST WHERE MATNR EQ LT_DATA-MATNR
                                  AND WERKS EQ LT_DATA-WERKS
                                  AND STLAN EQ '1'.
      IF SY-SUBRC IS INITIAL.
        LT_DATA-MASTS = '@01@'.
      ELSE.
        LT_DATA-MASTS = '@02@'.
      ENDIF.

*--> 檢查生產版本
      PERFORM CHECK_PROD_VERSION CHANGING LT_DATA.
    ENDIF.

*--> 內製料號: 檢查Routing
    IF LT_DATA-BESKZ EQ 'E' AND LT_DATA-SOBSL EQ ''.
      SELECT SINGLE MAPL~PLNNR INTO L_PLNNR FROM MAPL
                         INNER JOIN PLKO ON MAPL~PLNTY EQ PLKO~PLNTY
                                AND MAPL~PLNNR EQ PLKO~PLNNR
                                AND MAPL~PLNAL EQ PLKO~PLNAL
                              WHERE MAPL~MATNR EQ LT_DATA-MATNR
                                AND MAPL~WERKS EQ LT_DATA-WERKS
                                AND MAPL~PLNTY EQ 'N'
                                AND MAPL~LOEKZ EQ ''.
      IF SY-SUBRC IS INITIAL.
        LT_DATA-MAPLS = '@01@'.
      ELSE.
        LT_DATA-MAPLS = '@02@'.
      ENDIF.
    ENDIF.

*--> 採購件
    IF LT_DATA-BESKZ EQ 'F'.
*--> 檢查貨源清單
      CASE LT_DATA-MTART.
        WHEN 'UNBW'. "客供件
          SELECT SINGLE * FROM EORD WHERE MATNR = LT_DATA-MATNR
                                      AND WERKS = LT_DATA-WERKS
                                      AND NOTKZ = SPACE
                                      AND ( VDATU <= SY-DATUM AND
                                            BDATU >= SY-DATUM ).
          IF SY-SUBRC = 0.
            G_LIFNR = EORD-LIFNR.
            LT_DATA-EORDS = '@01@'.
          ELSE.
            LT_DATA-EORDS = '@02@'.
          ENDIF.
        WHEN OTHERS.
          SELECT SINGLE * FROM EORD WHERE MATNR = LT_DATA-MATNR
                                      AND WERKS = LT_DATA-WERKS
                                      AND FLIFN = 'X'
                                      AND NOTKZ = SPACE
                                      AND ( VDATU <= SY-DATUM AND
                                            BDATU >= SY-DATUM ).
          IF SY-SUBRC = 0.
            G_LIFNR = EORD-LIFNR.
            LT_DATA-EORDS = '@01@'.
          ELSE.
            LT_DATA-EORDS = '@02@'.
          ENDIF.
      ENDCASE.

*--> 檢查價格主檔
*       檢查是否存在有效單價
      PERFORM GET_INFO_RECORD USING LT_DATA-MATNR
                                    LT_DATA-SOBSL
                              CHANGING LT_DATA-INFNR.

      IF LT_DATA-INFNR IS NOT INITIAL.
        LT_DATA-EINAS = '@01@'.

      ELSE.
        LT_DATA-EINAS = '@02@'.
      ENDIF.
    ENDIF.



*   表面處理
    PERFORM READ_TEXT USING LT_DATA-MATNR
                            'GRUN'
                            'MATERIAL'
                      CHANGING LT_DATA-LONGTEXT1.


*   熱處理
    PERFORM READ_TEXT USING LT_DATA-MATNR
                            'PRUE'
                            'MATERIAL'
                      CHANGING LT_DATA-LONGTEXT2.

*   發票英文品名
    PERFORM READ_TEXT USING LT_DATA-MATNR
                            'IVER'
                            'MATERIAL'
                      CHANGING LT_DATA-LONGTEXT3.

*--> QV View 啟用與否
    LOOP AT LT_QMAT WHERE WERKS = LT_DATA-WERKS
                      AND MATNR = LT_DATA-MATNR.

      CASE LT_QMAT-ART.
        WHEN '01'.
          LT_DATA-ART01 = 'X'.
        WHEN '0130'.
          LT_DATA-ART0130 = 'X'.
        WHEN '0130Z'.
          LT_DATA-ART0130Z = 'X'.
        WHEN '03'.
          LT_DATA-ART03 = 'X'.
        WHEN '05'.
          LT_DATA-ART05 = 'X'.
        WHEN '89'.
          LT_DATA-ART89 = 'X'.
      ENDCASE.

    ENDLOOP.

    LT_DATA-WAERS = T001-WAERS.

*--> 單重
    READ TABLE LT_MARM WITH KEY MATNR = LT_DATA-MATNR.
    IF SY-SUBRC = 0.
      LT_DATA-AVG_WEIGHT = LT_MARM-UMREN /  LT_MARM-UMREZ.
    ENDIF.

*--> 是否有上階
    PERFORM CS_WHERE_USED_MAT CHANGING LT_DATA.

    MODIFY LT_DATA INDEX L_INDEX.
  ENDLOOP.

  APPEND LINES OF LT_DATA TO GT_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_alt_mat
*&---------------------------------------------------------------------*
FORM SET_ALT_MAT  USING IN_MATNR.

  R_CRBOM-SIGN   = 'I'.
  R_CRBOM-OPTION = 'EQ'.
  R_CRBOM-LOW    = IN_MATNR.
  APPEND R_CRBOM.  CLEAR R_CRBOM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_orig_mat
*&---------------------------------------------------------------------*
FORM SET_ORIG_MAT  USING  IN_MATNR.

  R_BOM-SIGN   = 'I'.
  R_BOM-OPTION = 'EQ'.
  R_BOM-LOW    = IN_MATNR.
  APPEND R_BOM.  CLEAR R_BOM.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INFO_RECORD
*&---------------------------------------------------------------------*
FORM GET_INFO_RECORD  USING    IN_MATNR
                               IN_SOBSL
                      CHANGING EX_INFNR .
  DATA: BEGIN OF LT_PRICE OCCURS 0,
          INFNR LIKE EINE-INFNR,
          MATNR LIKE EINA-MATNR,
          LIFNR LIKE EINA-LIFNR,
        END OF LT_PRICE.

  DATA: L_ESOKZ LIKE EINE-ESOKZ.


*        SELECT SINGLE INFNR INTO LT_DATA-INFNR
*                            FROM EINA
*                           WHERE MATNR EQ LT_DATA-MATNR
*                             AND LIFNR EQ A017-LIFNR.

  "AND LIFNR EQ A017-LIFNR.
*      SELECT SINGLE * FROM A017 WHERE KAPPL = 'M'
*                                  AND KSCHL = 'PB00'
**                                      AND LIFNR = G_LIFNR
*                                  AND MATNR = LT_DATA-MATNR
*                                  AND EKORG = LT_DATA-WERKS
*                                  AND WERKS = LT_DATA-WERKS
*                                  AND ESOKZ = '0'
*                                  AND ( DATAB <= SY-DATUM AND
*                                        DATBI >= SY-DATUM ).

  IF IN_SOBSL = '30'.
    L_ESOKZ = '3'.
  ELSE.
    L_ESOKZ = '0'.
  ENDIF.

  SELECT A~INFNR B~MATNR B~LIFNR
   INTO CORRESPONDING FIELDS OF TABLE LT_PRICE
  FROM EINE AS A INNER JOIN EINA AS B
   ON A~INFNR = B~INFNR
 WHERE B~MATNR = IN_MATNR
   AND A~EKORG = '1100'
   AND A~ESOKZ = L_ESOKZ.

  READ TABLE LT_PRICE INDEX 1.

  IF SY-SUBRC = 0.
    EX_INFNR = LT_PRICE-INFNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_TEXT
*&---------------------------------------------------------------------*
FORM READ_TEXT  USING    IN_NAME
                         IN_ID LIKE THEAD-TDID
                         IN_OBJ LIKE THEAD-TDOBJECT
                CHANGING EX_TEXT.

  DATA: LT_LINES LIKE TLINE OCCURS 0 WITH HEADER LINE,
        L_NAME   LIKE THEAD-TDNAME.

  L_NAME = IN_NAME.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                  = SY-MANDT
      ID                      = IN_ID
      LANGUAGE                = SY-LANGU
      NAME                    = L_NAME
      OBJECT                  = IN_OBJ
*     ARCHIVE_HANDLE          = 0
*     LOCAL_CAT               = ' '
*     USE_OLD_PERSISTENCE     = ABAP_FALSE
*   IMPORTING
*     HEADER                  =
*     OLD_LINE_COUNTER        =
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  LOOP AT LT_LINES.
    CONCATENATE EX_TEXT LT_LINES-TDLINE INTO EX_TEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CS_WHERE_USED_MAT
*&---------------------------------------------------------------------*
FORM CS_WHERE_USED_MAT  CHANGING LW_DATA LIKE GT_DATA.

  DATA: WULTB   LIKE  STPOV OCCURS 0 WITH HEADER LINE,
        EQUICAT LIKE  CSCEQUI OCCURS 0 WITH HEADER LINE,
        KNDCAT  LIKE  CSCKND OCCURS 0 WITH HEADER LINE,
        MATCAT  LIKE  CSCMAT OCCURS 0 WITH HEADER LINE,
        STDCAT  LIKE  CSCSTD OCCURS 0 WITH HEADER LINE,
        TPLCAT  LIKE  CSCTPL OCCURS 0 WITH HEADER LINE.

  DATA: L_BESKZ LIKE MARC-BESKZ,    "採購類型
        L_SOBSL LIKE MARC-SOBSL.    "特殊採購類型

  CALL FUNCTION 'CS_WHERE_USED_MAT'
    EXPORTING
      DATUB                      = SY-DATUM
      DATUV                      = SY-DATUM
      MATNR                      = LW_DATA-MATNR
*     POSTP                      = ' '
*     RETCODE_ONLY               = ' '
      STLAN                      = '1'
      WERKS                      = LW_DATA-WERKS
*     MCLMT                      = ' '
*     MNSTL                      = ' '
*     MXSTL                      = ' '
*     STLTP                      = ' '
*     NEWSI                      = ' '
*   IMPORTING
*     TOPMAT                     =
    TABLES
      WULTB                      = WULTB
      EQUICAT                    = EQUICAT
      KNDCAT                     = KNDCAT
      MATCAT                     = MATCAT
      STDCAT                     = STDCAT
      TPLCAT                     = TPLCAT
*     PRJCAT                     =
    EXCEPTIONS
      CALL_INVALID               = 1
      MATERIAL_NOT_FOUND         = 2
      NO_WHERE_USED_REC_FOUND    = 3
      NO_WHERE_USED_REC_SELECTED = 4
      NO_WHERE_USED_REC_VALID    = 5
      OTHERS                     = 6.

  IF SY-SUBRC = 0.
    LW_DATA-BOM_WHUSER = '@01@'.
  ENDIF.

  LOOP AT WULTB.
    CLEAR:L_BESKZ,L_SOBSL.

    SELECT SINGLE BESKZ SOBSL
             INTO (L_BESKZ,L_SOBSL)
             FROM MARC
            WHERE MATNR = WULTB-MATNR
              AND WERKS = WULTB-WERKS.

    IF L_BESKZ = 'E' OR ( L_BESKZ = 'F' AND L_SOBSL = '30' ).
      LW_DATA-BOM_NUM = LW_DATA-BOM_NUM + 1.
    ENDIF.
  ENDLOOP.

ENDFORM.

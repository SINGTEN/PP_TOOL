************************************************************************
* Program Name      :
* Descriptions      : 物料主檔轉檔
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
* 20220302  1.0 Derek    New Create
* 20230130      Alice
************************************************************************
REPORT ZPPMIG001 NO STANDARD PAGE HEADING
                 MESSAGE-ID 00.
************************************************************************
* Tables Definitions
************************************************************************
TABLES : MARA, MARC.
************************************************************************
* Data Definitions
************************************************************************
*--> 基本
DATA : BEGIN OF T_BASIC OCCURS 0,
         MATNR     LIKE MARA-MATNR,  "物料號碼
         MTART     LIKE MARA-MTART,  "物料類型
         MAKTX     LIKE MAKT-MAKTX,  "物料說明-中文
         EMAKTX    LIKE MAKT-MAKTX,  "物料說明-英文
         MEINS     LIKE MARA-MEINS,   "基礎計量單位
         BISMT     LIKE MARA-BISMT,   "舊物料號碼
         MATKL     LIKE MARA-MATKL,   "物料群組(應用別)
         XCHPF     LIKE MARA-XCHPF,   "批次管理
         SPART     LIKE MARA-SPART,   "產品
         BRGEW     LIKE MARA-BRGEW,   "毛重
         NTGEW     LIKE MARA-NTGEW,   "淨重
         GEWEI     LIKE MARA-GEWEI,   "重量單位
         AESZN     LIKE MARA-AESZN,   "線徑
         GROES     LIKE MARA-GROES,   "Dimensions
         "         wrkst     LIKE mara-wrkst,
         ZEINR     LIKE MARA-ZEINR,   "HSCode
         ZEIVR     LIKE MARA-ZEIVR,   "圖面版次
*         mhdhb     LIKE mara-mhdhb,   "總有效期限
*         mhdrz     LIKE mara-mhdrz,   "最小剩餘有效期限
         MSTAE     LIKE MARA-MSTAE,   "跨廠物料狀態
         LONGTEXT1 TYPE C LENGTH 160, "表面處理
         LONGTEXT2 TYPE C LENGTH 160, "熱處理條件
         LONGTEXT3 TYPE C LENGTH 160, "發票英文品名
*         ekwsl     LIKE mara-ekwsl,   "採購值代碼
         " labor LIKE mara-labor,   "疊構
*         brgew LIKE mara-brgew,   "毛重
*         ntgew LIKE mara-ntgew,   "淨重
*         gewei LIKE mara-gewei,   "重量單位
*         groes LIKE mara-groes,   "材質
*         normt LIKE mara-normt,   "表面處理
*         zeinr LIKE mara-zeinr,   "HSCode
*         zeifo LIKE mara-zeifo,   "圖面版次
*         wrkst LIKE mara-wrkst,   "Dimensions
*         "ZEIVR LIKE MARA-ZEIVR,   "框色
*         ihivi LIKE mara-ihivi,   "主鍵
*         mhdhb LIKE mara-mhdhb,   "總有效期限
*         mhdrz LIKE mara-mhdrz,   "最小剩餘有效期限
*         mstae LIKE mara-mstae,   "跨廠物料狀態
       END OF T_BASIC.

*--> MRP & 工作排程
DATA : BEGIN OF T_MRP OCCURS 0,
         MATNR LIKE MARC-MATNR,   "物料號碼
         MAKTX LIKE MAKT-MAKTX,   "物料說明
         MTART LIKE MARA-MTART,   "物料類型
         WERKS LIKE MARC-WERKS,   "工廠
         MMSTA TYPE C LENGTH 10, "LIKE MARC-MMSTA,   "狀態
         DISMM LIKE MARC-DISMM,   "MRP類型
         MINBE LIKE MARC-MINBE,   "再訂購點
         DISGR LIKE MARC-DISGR,   "MRP群組
         DISPO LIKE MARC-DISPO,   "MRP控制員
         DISLS LIKE MARC-DISLS,   "批量
         BSTFE LIKE MARC-BSTFE,   "固定批量
         BSTMI LIKE MARC-BSTMI,   "最小批量
         BSTMA LIKE MARC-BSTMA,   "最大批量
         BSTRF LIKE MARC-BSTRF,   "捨入值
         AUSSS LIKE MARC-AUSSS,   "裝配廢品
         BESKZ LIKE MARC-BESKZ,   "採購類型
         SOBSL LIKE MARC-SOBSL,   "特殊採購類型
         LGFSB LIKE MARC-LGFSB,   "外部採購的儲存位置
         LGPRO LIKE MARC-LGPRO,   "發貨儲存位置
         RGEKZ LIKE MARC-RGEKZ,   "倒扣入帳
         SCHGT LIKE MARC-SCHGT,   "散裝物料
         DZEIT LIKE MARC-DZEIT,   "廠內生產時間
         PLIFZ LIKE MARC-PLIFZ,   "計劃交貨天數
         WEBAZ LIKE MARC-WEBAZ,   "收貨作業處理時間
*         eislo LIKE marc-eislo,   "安全庫存
         EISBE LIKE MARC-EISBE,   "安全庫存
         MTVFP LIKE MARC-MTVFP,   "可用度檢查
         SBDKZ LIKE MARC-SBDKZ,   "個別/彙整
         KAUSF LIKE MARC-KAUSF,   "零件廢品(%)
         KZAUS LIKE MARC-KZAUS,   "中止指示碼
         AUSDT LIKE MARC-AUSDT,   "失效日期
         NFMAT LIKE MARC-NFMAT,   "後續物料
         UNETO LIKE MARC-UNETO,   "交貨不足允差(%)
         UEETO LIKE MARC-UEETO,   "過量交貨允差(%)
*         bstfe LIKE marc-bstfe,   "固定批量
       END OF T_MRP.

*--> 品管
DATA : BEGIN OF T_QM OCCURS 0,
         MATNR    LIKE MARC-MATNR,   "物料號碼
         MAKTX    LIKE MAKT-MAKTX,   "物料說明
         WERKS    LIKE MARC-WERKS,   "工廠
         ART01    TYPE C LENGTH 1,
         ACT01    TYPE C LENGTH 1,
         ART0130  TYPE C LENGTH 1,
         ACT0130  TYPE C LENGTH 1,
         ART0130Z TYPE C LENGTH 1,
         ACT0130Z TYPE C LENGTH 1,
         ART03    TYPE C LENGTH 1,
         ACT03    TYPE C LENGTH 1,
         ART05    TYPE C LENGTH 1,
         ACT05    TYPE C LENGTH 1,
         ART89    TYPE C LENGTH 1,
         ACT89    TYPE C LENGTH 1,
         PRFRQ    LIKE MARC-PRFRQ,   "間隔天數
       END OF T_QM.

*--> 銷售
DATA : BEGIN OF T_SALES OCCURS 0,
         MATNR LIKE MARA-MATNR,      " 料號
         MAKTX LIKE MAKT-MAKTX,      "物料說明
         WERKS LIKE MARC-WERKS,      " 工廠
         VKORG LIKE VBAK-VKORG,      " 銷售組織
         VTWEG LIKE VBAK-VTWEG,      " 配銷通路
         DWERK LIKE MVKE-DWERK,      " 預計交貨廠
         TAXKM TYPE C LENGTH 1,      " 稅分類
         AUMNG LIKE MVKE-AUMNG,      " 最小訂購量
         MSTAV LIKE MARA-MSTAV,      "跨配銷鍊的物料狀態
         MSTDE LIKE MARA-MSTDE,      "跨工廠物料狀態的起始日期是有效的
         VMSTA LIKE MVKE-VMSTA,      "銷售鍊-相關的檢視的物料狀態
         VMSTD LIKE MVKE-VMSTD,      "銷售的物料狀態開始有效日
         KTGRM LIKE MVKE-KTGRM,      " 科目指派群組
         MTPOS LIKE MVKE-MTPOS,      " 項目類別群組
         MVGR1 LIKE MVKE-MVGR1,      " 物料群組 1(產品大分類)
         MVGR2 LIKE MVKE-MVGR2,      " 物料群組 2
         MVGR3 LIKE MVKE-MVGR3,      " 物料群組 3
         MVGR4 LIKE MVKE-MVGR4,      " 物料群組 4
         MVGR5 LIKE MVKE-MVGR5,      " 物料群組 5
         TRAGR LIKE MARA-TRAGR,      "運輸群組
         LADGR LIKE MARC-LADGR,
         MTVFP LIKE MARC-MTVFP,
       END OF T_SALES.

*--> 採購
DATA : BEGIN OF T_PURCH OCCURS 0,
         MATNR LIKE MARC-MATNR,   "物料號碼
         MAKTX LIKE MAKT-MAKTX,   "物料說明
         WERKS LIKE MARC-WERKS,   "工廠
         EKGRP LIKE MARC-EKGRP,   "採購群組
         BSTME LIKE MARA-BSTME,   "訂單單位
         KAUTB LIKE MARC-KAUTB,   "自動轉採購單
         "WEBAZ LIKE MARC-WEBAZ,   "收貨作業處理時間
         KORDB LIKE MARC-KORDB,   "貨源清單
         EKWSL LIKE MARA-EKWSL,   "採購值代碼
       END OF T_PURCH.

*--> 單位轉換
DATA : BEGIN OF T_UOM OCCURS 0,
         MATNR LIKE MARA-MATNR,   "物料號碼
         MAKTX LIKE MAKT-MAKTX,   "物料說明
         MEINS LIKE MARA-MEINS,   "基礎計量單位
         UMREZ LIKE MARM-UMREZ,   "轉換分子
         MEINH LIKE MARM-MEINH,   "庫存記錄單位的替代計量單位
         UMREN LIKE MARM-UMREN,   "轉換分母
         BRGEW LIKE MARM-BRGEW,  "毛重
         GEWEI LIKE MARM-GEWEI,  "毛重單位
         LAENG LIKE MARM-LAENG,  "長
         BREIT LIKE MARM-BREIT,  "寬
         HOEHE LIKE MARM-HOEHE,  "高
         MEABM LIKE MARM-MEABM,  "長度單位
       END OF T_UOM.

*--> 儲存
DATA : BEGIN OF T_SLOC OCCURS 0,
         MATNR LIKE MARC-MATNR,   "物料號碼
         MAKTX LIKE MAKT-MAKTX,   "物料說明
         WERKS LIKE MARC-WERKS,   "工廠
         AUSME LIKE MARC-AUSME,   "發貨單位
         MHDHB LIKE MARA-MHDHB,   "總有效期限
         MHDRZ LIKE MARA-MHDRZ,   "最小剩餘有效期限
       END OF T_SLOC.

*--> 財務
DATA : BEGIN OF T_COST OCCURS 0,
         MTART LIKE MARA-MTART,   "物料類型
         MATNR LIKE MARC-MATNR,   "物料號碼
         MAKTX LIKE MAKT-MAKTX,   "物料說明
         WERKS LIKE MARC-WERKS,   "工廠=評價範圍
         BKLAS LIKE MBEW-BKLAS,   "評價類別
         EKLAS LIKE MBEW-EKLAS,   "銷售訂單-評價類別(在途存貨)
*        mlmaa   LIKE mbew-mlmaa,  "ML 啟動
         MLAST LIKE MBEW-MLAST,   "價格決定
         VPRSV LIKE MBEW-VPRSV,   "價格控制
*         pvprs   LIKE mbew-verpr,   "定期單價
         PEINH LIKE MBEW-PEINH,   "公司代碼幣別價格單位
*         peinh_1 LIKE mbew-peinh,
*集團幣別價格單位(僅for Excel上傳備註用，
*因BAPI建立時會與公司代碼幣別一致)
*         peinh_2 LIKE mbew-peinh,   "利潤中心價格單位(同集團幣別邏輯)
         EKALR LIKE MBEW-EKALR,   "含QS成本估算
         HKMAT LIKE MBEW-HKMAT,   "物料來源
         HRKFT LIKE MBEW-HRKFT,   "來源群組
         PRCTR LIKE MARC-PRCTR,   "利潤中心
         LOSGR LIKE MARC-LOSGR,   "成本計算批量
         MMSTA LIKE MARC-MMSTA,   "工廠特定物料狀態
*         zplp1   LIKE mbew-zplp1,   "計劃價格一
*         zpld1   LIKE mbew-zpld1,   "計劃價格日期一
       END OF T_COST.


DATA:BEGIN OF T_KGMPC OCCURS 0,
       MATNR LIKE MARA-MATNR,
       KZWSM LIKE MARA-KZWSM,
       ATNAM TYPE ATNAM,
       ATWRT TYPE ATWRT,
       WSMEI TYPE LRMEI_WS,
       XFHDW TYPE XFHDW,
     END OF T_KGMPC.

*--> 檢驗內文
DATA : BEGIN OF T_LONT OCCURS 0,
         MATNR      LIKE MARA-MATNR,            "物料號碼
         TEXT_ID    LIKE BAPI_MLTX-TEXT_ID,     "'GRUN'
         LANGU      TYPE C LENGTH 2,            "ZF
         APPLOBJECT LIKE BAPI_MLTX-APPLOBJECT,  "'MATERIAL'
         TEXT_LINE  LIKE BAPI_MLTX-TEXT_LINE,
       END OF T_LONT.


*---> SORUCE DATA
DATA: BEGIN OF T_BASIC_LOG OCCURS 0.
        INCLUDE STRUCTURE T_BASIC.
DATA:   FLAG(1),
        ID(10),
        NUMBER(20),
        MESSAGE(100),
      END OF T_BASIC_LOG.


DATA: BEGIN OF T_LONT_LOG OCCURS 0.
        INCLUDE STRUCTURE T_LONT.
DATA:   FLAG(1),
        ID(10),
        NUMBER(20),
        MESSAGE(100),
      END OF T_LONT_LOG.

DATA : BEGIN OF T_MRP_LOG OCCURS 0.
         INCLUDE STRUCTURE T_MRP.
DATA:   FLAG(1),
         ID(10),
         NUMBER(20),
         MESSAGE(100),
         BISMT        LIKE MARA-BISMT,
       END OF T_MRP_LOG.

DATA : BEGIN OF T_PURCH_LOG OCCURS 0.
         INCLUDE STRUCTURE T_PURCH.
DATA:    FLAG(1),
         ID(10),
         NUMBER(20),
         MESSAGE(100),
       END OF T_PURCH_LOG.

DATA : BEGIN OF T_SALES_LOG OCCURS 0.
         INCLUDE STRUCTURE T_SALES.
DATA:    FLAG(1),
         ID(10),
         NUMBER(20),
         MESSAGE(100),
       END OF T_SALES_LOG.

DATA : BEGIN OF T_COST_LOG OCCURS 0.
         INCLUDE STRUCTURE T_COST.
DATA:    FLAG(1),
         ID(10),
         NUMBER(20),
         MESSAGE(100),
       END OF T_COST_LOG.

DATA : BEGIN OF T_QM_LOG OCCURS 0.
         INCLUDE STRUCTURE T_QM.
DATA:    FLAG(1),
         ID(10),
         NUMBER(20),
         MESSAGE(100),
       END OF T_QM_LOG.

DATA : BEGIN OF T_SLOC_LOG OCCURS 0.
         INCLUDE STRUCTURE T_SLOC.
DATA:    FLAG(1),
         ID(10),
         NUMBER(20),
         MESSAGE(100),
       END OF T_SLOC_LOG.


DATA: BEGIN OF T_UOM_LOG OCCURS 0.
        INCLUDE STRUCTURE T_UOM.
DATA:    FLAG(1),
        ID(10),
        NUMBER(20),
        MESSAGE(100),
      END OF T_UOM_LOG.


DATA: BEGIN OF T_KGMPC_LOG OCCURS 0.
        INCLUDE STRUCTURE T_KGMPC.
DATA:    FLAG(1),
        ID(10),
        NUMBER(20),
        MESSAGE(100),
      END OF T_KGMPC_LOG.

DATA : G_HEADDATA             LIKE BAPIMATHEAD,
       G_CLIENTDATA           LIKE BAPI_MARA,
       G_CLIENTDATAX          LIKE BAPI_MARAX,
       G_PLANTDATA            LIKE BAPI_MARC,
       G_PLANTDATAX           LIKE BAPI_MARCX,
       G_FORECASTPARAMETERS   LIKE BAPI_MPOP,
       G_FORECASTPARAMETERSX  LIKE BAPI_MPOPX,
       G_STORAGELOCATIONDATA  LIKE BAPI_MARD,
       G_STORAGELOCATIONDATAX LIKE BAPI_MARDX,
       G_VALUATIONDATA        LIKE BAPI_MBEW,
       G_VALUATIONDATAX       LIKE BAPI_MBEWX,
       G_SALESDATA            LIKE BAPI_MVKE,
       G_SALESDATAX           LIKE BAPI_MVKEX,
       G_RETURN               LIKE BAPIRET2,     " ERROR LOG
       G_BAPI_TE_MARA         TYPE BAPI_TE_MARA,
       G_BAPI_TE_MARAX        TYPE BAPI_TE_MARAX,
       G_BAPI_TE_MARC         TYPE BAPI_TE_MARC,
       G_BAPI_TE_MARCX        TYPE BAPI_TE_MARCX.
*--> 新增MRP VIEW 規劃物料/規劃工廠/計畫物料的轉換係數
DATA: G_PLANNINGDATA  TYPE BAPI_MPGD,
      G_PLANNINGDATAX TYPE BAPI_MPGDX.

*->檢驗類型指派BAPI使用
DATA: GT_INSP   LIKE BAPI1001004_QMAT OCCURS 0 WITH HEADER LINE.
DATA: GT_RETURN2 LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

DATA : GT_MATERIALDESCRIPTION LIKE BAPI_MAKT  OCCURS 1 WITH HEADER LINE,
       GT_RETURN              LIKE BAPI_MATRETURN2 OCCURS 1 WITH HEADER
       LINE,
       GT_UNITSOFMEASURE      LIKE BAPI_MARM  OCCURS 1 WITH HEADER LINE,
       GT_UNITSOFMEASUREX     LIKE BAPI_MARMX OCCURS 1 WITH HEADER LINE,
       GT_INTERNATIONALARTNOS LIKE BAPI_MEAN OCCURS 1 WITH HEADER LINE,
       GT_TAXCLASSIFICATIONS  LIKE BAPI_MLAN  OCCURS 1 WITH HEADER LINE,
       GT_RETURNMESSAGES      LIKE BAPI_MATRETURN2 OCCURS 0
                                               WITH HEADER LINE,
       GT_MATERIALLONGTEXT    LIKE BAPI_MLTX OCCURS 1 WITH HEADER LINE.

DATA : G_TOTAL(5),
       G_SUCCESS(5),
       G_FAIL(5).

DATA : GT_UPLOAD LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

*--> BDC TABLE
DATA: GT_BDCTAB  LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
      GT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
      P_MODE     LIKE CTU_PARAMS-DISMODE VALUE 'A'. "'N'.

* MACRO
DEFINE ALPHAIN.  "補0
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT         = &1
   IMPORTING
      OUTPUT        = &1.
END-OF-DEFINITION.
************************************************************************
* DATA DEFINITIONS
************************************************************************
TYPE-POOLS: SLIS.
DATA: COLOR TYPE SLIS_T_SPECIALCOL_ALV WITH HEADER LINE.
DATA: G_LAYOUT TYPE SLIS_LAYOUT_ALV.
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA: G_EVENTS TYPE SLIS_T_EVENT,
      GT_SORT  TYPE SLIS_T_SORTINFO_ALV,
      G_SORT   TYPE SLIS_SORTINFO_ALV.
DATA: G_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: GC_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
DATA: W_FCODE LIKE SY-UCOMM, W_TABIX LIKE SY-TABIX.

************************************************************************
*     SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-001.
*     上載檔案路徑及名稱
  PARAMETERS: P_FILE  LIKE RLGRAP-FILENAME,
              "     BASIC VIEW
              P_BASIC RADIOBUTTON GROUP GRP1,
              "     SALES VIEW
              P_SALES RADIOBUTTON GROUP GRP1,
              "     PURCHASE VIEW
              P_PURCH RADIOBUTTON GROUP GRP1,
              "     MRP & WORK SCHEDULE
              P_MRP   RADIOBUTTON GROUP GRP1,
              "     STORAGE VIEW
              P_SLOC  RADIOBUTTON GROUP GRP1,
              "     QM VIEW
              P_QM    RADIOBUTTON GROUP GRP1,
              "     ACCOUNTING & COSTING VIEW
              P_COST  RADIOBUTTON GROUP GRP1,

              "     UOM CONVERSION(計量單位轉換)
              P_UOM   RADIOBUTTON GROUP GRP1,
              "     長文
              P_LOT   NO-DISPLAY,
*--比例_產品單位(KG_MPC)
              P_UR RADIOBUTTON GROUP GRP1.

**REMARK
*  SELECTION-SCREEN SKIP 1.
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN COMMENT (70) TEXT-002.
*  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK.

*----------------------------------------------------------------------*
* AT SELECTION SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      STATIC        = 'X'
    CHANGING
      FILE_NAME     = P_FILE
    EXCEPTIONS
      MASK_TOO_LONG = 1
      OTHERS        = 2.

************************************************************************
*     START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM UPLOAD_EX_DATA.
  PERFORM READ_DATA.
  PERFORM PROCESS_DATA.
  PERFORM OUTPUT_DATA.

END-OF-SELECTION.
************************************************************************
*     SFORM
************************************************************************
*&---------------------------------------------------------------------*
*&      FORM  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA .
  IF P_BASIC = 'X'.
*   基本資料
    PERFORM READ_BASIC_DATA.
  ELSEIF P_MRP = 'X'.
*   MRP & 工作排程 & 預測
    PERFORM READ_MRP_DATA.
  ELSEIF P_QM = 'X'.
*   品質檢驗
    PERFORM READ_QM_DATA.
  ELSEIF P_SALES = 'X'.
*   銷售組織資料
    PERFORM READ_SALES_DATA.
  ELSEIF P_LOT = 'X'.
*   檢驗內文
    PERFORM READ_LONG_TEXT_DATA.
  ELSEIF P_PURCH = 'X'.
*   採購資料
    PERFORM READ_PURCHASE_DATA.
  ELSEIF P_COST = 'X'.
*   會計&成本檢視
    PERFORM READ_COST_DATA.
  ELSEIF P_SLOC = 'X'.
*   儲存檢視
    PERFORM READ_SLOC_DATA.
  ELSEIF P_UOM = 'X'.
*   基礎計量單位
    PERFORM READ_UOM_DATA.

*--比例_產品單位(KG_MPC)
  ELSEIF P_UR = 'X'.
    PERFORM READ_KGMPC_DATA.
  ENDIF.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      FORM  READ_BASIC_DATA
*&---------------------------------------------------------------------*
FORM READ_BASIC_DATA .
  DATA: L_INDEX   TYPE I.

  CLEAR : T_BASIC, T_BASIC[].

  FIELD-SYMBOLS: <FS>.
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_BASIC TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.
      "TRANSLATE T_BASIC-MATNR TO UPPER CASE.

*---> 轉換單位
      CALL FUNCTION 'CONVERSION_EXIT_RUNIT_INPUT'
        EXPORTING
          INPUT     = T_BASIC-MEINS
          LANGUAGE  = SY-LANGU
        IMPORTING
          OUTPUT    = T_BASIC-MEINS
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT  = T_BASIC-MATNR
        IMPORTING
          OUTPUT = T_BASIC-MATNR.

      APPEND T_BASIC. CLEAR T_BASIC.
    ENDAT.
  ENDLOOP.

  IF T_BASIC[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " READ_BASIC_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_DATA .
  CLEAR : G_TOTAL, G_SUCCESS, G_FAIL.
  IF P_BASIC = 'X'.
*   基本資料
    PERFORM PROCESS_BASIC_DATA.
  ELSEIF P_MRP = 'X'.
*   MRP&工作排程&預測
    PERFORM PROCESS_MRP_DATA.
  ELSEIF P_QM = 'X'.
*   品質檢驗
    PERFORM PROCESS_QM_DATA.
    PERFORM PROCESS_QPART.
  ELSEIF P_SALES = 'X'.
*   銷售組織資料
    PERFORM PROCESS_SALES_DATA.
  ELSEIF P_PURCH = 'X'.
*   採購資料
    PERFORM PROCESS_PURCHASE_DATA.
  ELSEIF P_UOM = 'X'.
*   基礎計量單位
    PERFORM PROCESS_UOM_DATA.
  ELSEIF P_SLOC = 'X'.
*   儲存位置
    PERFORM PROCESS_SLOC_DATA.

*  ELSEIF p_lot = 'X'.
**   檢驗內文
*    PERFORM process_long_text.

  ELSEIF P_COST = 'X'.
*   會計&成本資料
    PERFORM PROCESS_COST_DATA.

*--比例_產品單位(KG_MPC)
  ELSEIF P_UR = 'X'.
    PERFORM PROCESS_KGMPC_DATA.

  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_BASIC_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_BASIC_DATA .
  DATA LW_BASIC LIKE T_BASIC.

  LOOP AT T_BASIC ."INTO LW_BASIC.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_BASIC TO T_BASIC_LOG.

    PERFORM CLEAR_BAPI_DATA.
    G_HEADDATA-MATERIAL_LONG  = T_BASIC-MATNR.  "料號
    G_HEADDATA-IND_SECTOR     = 'M'.            "產業別M-機械工程
    G_HEADDATA-MATL_TYPE      = T_BASIC-MTART.  "物料類型
    G_HEADDATA-BASIC_VIEW     = 'X'.            "基本資料檢視

*   基本資料檢視
    PERFORM APPEND_BASICVIEW.
*--基本內文
    PERFORM GET_LONGTEXT.
*--檢驗內文
    PERFORM GET_LONGTEXT_1.

*--內部備註(發票英文品名)
    PERFORM GET_LONGTEXT_2.

*   CALL BAPI - 維護商品主檔
    IF T_BASIC_LOG-FLAG IS INITIAL.
      PERFORM CALL_BAPI CHANGING T_BASIC_LOG-FLAG
                                 T_BASIC_LOG-ID
                                 T_BASIC_LOG-NUMBER
                                 T_BASIC_LOG-MESSAGE.
    ENDIF.
    APPEND T_BASIC_LOG.   CLEAR T_BASIC_LOG.
  ENDLOOP.
ENDFORM.                    " PROCESS_BASIC_DATA
*&---------------------------------------------------------------------*
*&      FORM  CLEAR_BAPI_DATA
*&---------------------------------------------------------------------*
FORM CLEAR_BAPI_DATA .
  CLEAR: G_HEADDATA, G_CLIENTDATA, G_CLIENTDATAX, G_PLANTDATA,
         G_PLANTDATAX, G_SALESDATA, G_SALESDATAX, G_VALUATIONDATA,
         G_VALUATIONDATAX, GT_TAXCLASSIFICATIONS, G_RETURN,
         G_STORAGELOCATIONDATA ,G_STORAGELOCATIONDATAX,
         GT_MATERIALDESCRIPTION, GT_UNITSOFMEASURE,
         GT_UNITSOFMEASUREX, GT_RETURNMESSAGES,
         GT_TAXCLASSIFICATIONS, G_FORECASTPARAMETERS,
         G_FORECASTPARAMETERSX, GT_MATERIALLONGTEXT.

  REFRESH: GT_MATERIALDESCRIPTION, GT_UNITSOFMEASURE,
           GT_UNITSOFMEASUREX, GT_INTERNATIONALARTNOS,
           GT_RETURNMESSAGES, GT_TAXCLASSIFICATIONS,
           GT_MATERIALLONGTEXT.
ENDFORM.                    " CLEAR_BAPI_DATA
*&---------------------------------------------------------------------*
*&      FORM  APPEND_BASICVIEW
*&---------------------------------------------------------------------*
FORM APPEND_BASICVIEW.
  DATA: WL_GEWEI LIKE MARA-GEWEI.

* MATERIAL DESCRIPTION
  PERFORM APPEND_MATERIAL_DESCRIPT.

*********BASCI-VIEW1
  G_CLIENTDATA-BASE_UOM  = T_BASIC-MEINS.   "單位
  G_CLIENTDATAX-BASE_UOM = 'X'.

  IF T_BASIC-MATKL IS INITIAL.
    T_BASIC_LOG-FLAG = 'F'.
    T_BASIC_LOG-MESSAGE = '未填入物料群組'.
    EXIT.
  ELSE.
    G_CLIENTDATA-MATL_GROUP   = T_BASIC-MATKL.   "物料群組
    G_CLIENTDATAX-MATL_GROUP  = 'X'.
  ENDIF.

  IF T_BASIC-BISMT IS NOT INITIAL.
*    G_CLIENTDATA-OLD_MAT_NO   = T_BASIC-BISMT.   "舊物料號碼
*    G_CLIENTDATAX-OLD_MAT_NO  = 'X'.
    G_CLIENTDATA-OLD_MAT_NO_LONG = T_BASIC-BISMT.
    G_CLIENTDATAX-OLD_MAT_NO_LONG = 'X'.
  ENDIF.

  IF T_BASIC-SPART IS INITIAL.
    T_BASIC_LOG-FLAG    = 'F'.
    T_BASIC_LOG-MESSAGE = '未填入產品'.
    EXIT.
  ELSE.
    G_CLIENTDATA-DIVISION     = T_BASIC-SPART.   "產品
    G_CLIENTDATAX-DIVISION    = 'X'.
  ENDIF.

  IF T_BASIC-AESZN IS NOT INITIAL.
    G_CLIENTDATA-DOC_CHG_NO   = T_BASIC-AESZN.   "文件變更號碼
    G_CLIENTDATAX-DOC_CHG_NO  = 'X'.
  ENDIF.

*  IF t_basic-labor IS NOT INITIAL.
*    g_clientdata-dsn_office = t_basic-labor. "疊構
*    g_clientdatax-dsn_office  = 'X'.
*  ENDIF.

*---> 毛重
  IF NOT T_BASIC-BRGEW IS INITIAL.
    GT_UNITSOFMEASURE-ALT_UNIT = T_BASIC-MEINS.
    GT_UNITSOFMEASURE-GROSS_WT = T_BASIC-BRGEW.
    GT_UNITSOFMEASURE-UNIT_OF_WT = T_BASIC-GEWEI.
    APPEND GT_UNITSOFMEASURE.  CLEAR GT_UNITSOFMEASURE.

    GT_UNITSOFMEASUREX-ALT_UNIT = T_BASIC-MEINS.
    GT_UNITSOFMEASUREX-GROSS_WT = 'X'.
    GT_UNITSOFMEASUREX-UNIT_OF_WT = 'X'.
    APPEND GT_UNITSOFMEASUREX.  CLEAR GT_UNITSOFMEASUREX.
  ENDIF.

  IF T_BASIC-NTGEW IS NOT INITIAL.
    G_CLIENTDATA-NET_WEIGHT   = T_BASIC-NTGEW. "淨重
    G_CLIENTDATAX-NET_WEIGHT  = 'X'.
  ENDIF.

  IF T_BASIC-GEWEI IS NOT INITIAL.
    G_CLIENTDATA-UNIT_OF_WT   = T_BASIC-GEWEI.  "重量單位
    G_CLIENTDATAX-UNIT_OF_WT  = 'X'.
  ENDIF.

  IF T_BASIC-GROES IS NOT INITIAL.
    G_CLIENTDATA-SIZE_DIM = T_BASIC-GROES.     "Dimensions
    G_CLIENTDATAX-SIZE_DIM = 'X'.
  ENDIF.

*  IF t_basic-normt IS NOT INITIAL.
*    g_clientdata-std_descr = t_basic-normt. "表面處理
*    g_clientdatax-std_descr  = 'X'.
*  ENDIF.

  IF T_BASIC-ZEINR IS NOT INITIAL.
    G_CLIENTDATA-DOCUMENT = T_BASIC-ZEINR. "圖面編號
    G_CLIENTDATAX-DOCUMENT = 'X'.
  ENDIF.

  IF T_BASIC-ZEIVR IS NOT INITIAL.
    G_CLIENTDATA-DOC_VERS  = T_BASIC-ZEIVR. "圖面編號
    G_CLIENTDATAX-DOC_VERS = 'X'.
  ENDIF.

*  IF t_basic-zeifo IS NOT INITIAL.
*    g_clientdata-doc_format   = t_basic-zeifo.   "尺寸
*    g_clientdatax-doc_format  = 'X'.
*  ENDIF.

*  IF t_basic-wrkst IS NOT INITIAL.
*    g_clientdata-basic_matl = t_basic-wrkst. "
*    g_clientdatax-basic_matl  = 'X'.
*  ENDIF.

*  IF T_BASIC-ZEIVR IS NOT INITIAL.
*    G_CLIENTDATA-DOC_VERS   = T_BASIC-ZEIVR.  "框色
*    G_CLIENTDATAX-DOC_VERS  = 'X'.
*  ENDIF.

*  IF t_basic-ihivi IS NOT INITIAL.
*    g_clientdata-high_visc   = t_basic-ihivi.  "主鍵
*    g_clientdatax-high_visc  = 'X'.
*  ENDIF.

*--->批次管理
  IF T_BASIC-XCHPF IS NOT INITIAL.
    G_CLIENTDATA-BATCH_MGMT = 'X'.
    G_CLIENTDATAX-BATCH_MGMT = 'X'.
  ENDIF.


  IF T_BASIC-MSTAE IS NOT INITIAL.
    G_CLIENTDATA-PUR_STATUS  = T_BASIC-MSTAE.   "跨廠物料狀態
    G_CLIENTDATAX-PUR_STATUS = 'X'.
  ENDIF.

*  IF t_basic-mhdhb IS NOT INITIAL.
*    g_clientdata-shelf_life = t_basic-mhdhb.   "總有效期限
*    g_clientdatax-shelf_life = 'X'.
*  ENDIF.
*
*  IF t_basic-mhdrz IS NOT INITIAL.
*    g_clientdata-minremlife = t_basic-mhdrz.   "最小剩餘有效期限
*    g_clientdatax-minremlife = 'X'.
*  ENDIF.
*
*  IF t_basic-ekwsl IS NOT INITIAL.
*    g_clientdata-pur_valkey = t_basic-ekwsl.   "採購值代碼
*    g_clientdatax-pur_valkey = 'X'.
*  ENDIF.

ENDFORM.                    " APPEND_BASICVIEW
*&---------------------------------------------------------------------*
*&      FORM  APPEND_MATERIAL_DESCRIPT
*&---------------------------------------------------------------------*
FORM APPEND_MATERIAL_DESCRIPT .
** MARTERIAL DESCRIPTION

*--"物料說明-中文
  IF T_BASIC-MAKTX NE SPACE.
    REFRESH GT_MATERIALDESCRIPTION.
    GT_MATERIALDESCRIPTION-LANGU     = 'M'.
    GT_MATERIALDESCRIPTION-MATL_DESC = T_BASIC-MAKTX.
    APPEND GT_MATERIALDESCRIPTION.   CLEAR GT_MATERIALDESCRIPTION.
  ENDIF.

*--"物料說明-英文
  IF T_BASIC-EMAKTX NE SPACE.
    GT_MATERIALDESCRIPTION-LANGU     = 'E'.
    GT_MATERIALDESCRIPTION-MATL_DESC = T_BASIC-EMAKTX.
    APPEND GT_MATERIALDESCRIPTION.   CLEAR GT_MATERIALDESCRIPTION.
  ENDIF.

*  IF T_MRP-MAKTX NE SPACE.
*    REFRESH GT_MATERIALDESCRIPTION.
*    GT_MATERIALDESCRIPTION-LANGU     = 'E'.
*    GT_MATERIALDESCRIPTION-MATL_DESC = T_MRP-MAKTX.
*    APPEND GT_MATERIALDESCRIPTION.   CLEAR GT_MATERIALDESCRIPTION.
*
*    GT_MATERIALDESCRIPTION-LANGU     = 'M'.
*    GT_MATERIALDESCRIPTION-MATL_DESC = T_MRP-MAKTX.
*    APPEND GT_MATERIALDESCRIPTION.   CLEAR GT_MATERIALDESCRIPTION.
*  ENDIF.
ENDFORM.                    " APPEND_MATERIAL_DESCRIPT
*&---------------------------------------------------------------------*
*&      FORM  CALL_BAPI
*&---------------------------------------------------------------------*
FORM CALL_BAPI CHANGING OUT_FLAG OUT_ID OUT_NUMBER OUT_MESSAGE.

  CLEAR G_RETURN.
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      HEADDATA             = G_HEADDATA
      CLIENTDATA           = G_CLIENTDATA
      CLIENTDATAX          = G_CLIENTDATAX
      PLANTDATA            = G_PLANTDATA
      PLANTDATAX           = G_PLANTDATAX
      FORECASTPARAMETERS   = G_FORECASTPARAMETERS
      FORECASTPARAMETERSX  = G_FORECASTPARAMETERSX
      PLANNINGDATA         = G_PLANNINGDATA
      PLANNINGDATAX        = G_PLANNINGDATAX
      SALESDATA            = G_SALESDATA
      SALESDATAX           = G_SALESDATAX
      VALUATIONDATA        = G_VALUATIONDATA
      VALUATIONDATAX       = G_VALUATIONDATAX
      STORAGELOCATIONDATA  = G_STORAGELOCATIONDATA
      STORAGELOCATIONDATAX = G_STORAGELOCATIONDATAX
    IMPORTING
      RETURN               = G_RETURN
    TABLES
      MATERIALDESCRIPTION  = GT_MATERIALDESCRIPTION
      UNITSOFMEASURE       = GT_UNITSOFMEASURE
      UNITSOFMEASUREX      = GT_UNITSOFMEASUREX
      MATERIALLONGTEXT     = GT_MATERIALLONGTEXT
*     INTERNATIONALARTNOS  = GT_INTERNATIONALARTNOS
      TAXCLASSIFICATIONS   = GT_TAXCLASSIFICATIONS
      RETURNMESSAGES       = GT_RETURNMESSAGES.
*      EXTENSIONIN          = GT_EXTENSIONIN
*      EXTENSIONINX         = GT_EXTENSIONINX.


  IF G_RETURN-TYPE = 'S' AND G_RETURN-NUMBER = '356'.
    IF P_BASIC = 'X' AND T_BASIC-XCHPF IS NOT INITIAL.
      PERFORM CREATE_CLASSFICATION.
    ENDIF.
  ENDIF.

  IF G_RETURN-TYPE = 'S' AND G_RETURN-NUMBER = '356'.
    OUT_FLAG = 'S'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    G_SUCCESS = G_SUCCESS + 1.
    CONCATENATE '料號:' G_HEADDATA-MATERIAL
    '更新成功' INTO OUT_MESSAGE.
    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _SYNCHRON = 'X'.
  ELSE.
    OUT_FLAG = 'F'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    G_FAIL = G_FAIL + 1.
    OUT_ID = G_RETURN-ID.
    OUT_NUMBER = G_RETURN-NUMBER.
    CONCATENATE '料號:' G_HEADDATA-MATERIAL
    '更新失敗-' G_RETURN-ID G_RETURN-NUMBER G_RETURN-MESSAGE
    INTO OUT_MESSAGE.
  ENDIF.
ENDFORM.                    " CALL_BAPI
*&---------------------------------------------------------------------*
*&      FORM  OUTPUT_DATA
*&---------------------------------------------------------------------*
FORM OUTPUT_DATA .
  CLEAR: G_LAYOUT, GT_FIELDCAT[], GT_SORT[], G_EVENTS[].

  PERFORM SET_ALV_FIELD.

  IF P_BASIC = 'X'.
    PERFORM SHOW_ALV TABLES T_BASIC_LOG.
  ELSEIF P_MRP = 'X'.
    PERFORM SHOW_ALV TABLES T_MRP_LOG.
  ELSEIF P_QM = 'X'.
    PERFORM SHOW_ALV TABLES T_QM_LOG.
  ELSEIF P_UOM = 'X'.
    PERFORM SHOW_ALV TABLES T_UOM_LOG.
  ELSEIF P_PURCH = 'X'.
    PERFORM SHOW_ALV TABLES T_PURCH_LOG.
  ELSEIF P_SALES = 'X'.
    PERFORM SHOW_ALV TABLES T_SALES_LOG.
  ELSEIF P_SLOC = 'X'.
    PERFORM SHOW_ALV TABLES T_SLOC_LOG.
  ELSEIF P_COST = 'X'.
    PERFORM SHOW_ALV TABLES T_COST_LOG.

  ELSEIF P_LOT = 'X'.
    PERFORM SHOW_ALV TABLES T_LONT_LOG.

*--比例_產品單位(KG_MPC)
  ELSEIF P_UR = 'X'.
    PERFORM SHOW_ALV TABLES T_KGMPC_LOG.

  ENDIF.

ENDFORM.                    " OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      FORM  READ_UOM_DATA
*&---------------------------------------------------------------------*
FORM READ_UOM_DATA .
  DATA : L_INDEX    TYPE I,
         L_UMREN(5),
         L_UMREZ(5).

  CLEAR : T_UOM, T_UOM[].
  FIELD-SYMBOLS: <FS>.
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_UOM TO <FS>.

    IF GT_UPLOAD-COL = '00004' OR GT_UPLOAD-COL = '00006'.
*--去千分位
      SEARCH GT_UPLOAD-VALUE FOR ','.
      IF SY-SUBRC = 0.
        REPLACE ALL OCCURRENCES OF ',' IN GT_UPLOAD-VALUE WITH SPACE.
      ENDIF.
    ENDIF.

    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT  = T_UOM-MATNR
        IMPORTING
          OUTPUT = T_UOM-MATNR.

*---> TRANSLATE TO UPPER CASE
      TRANSLATE T_UOM-MEINS TO UPPER CASE.
      TRANSLATE T_UOM-MEINH TO UPPER CASE.

*---> 轉換單位
      CALL FUNCTION 'CONVERSION_EXIT_RUNIT_INPUT'
        EXPORTING
          INPUT     = T_UOM-MEINS
          LANGUAGE  = SY-LANGU
        IMPORTING
          OUTPUT    = T_UOM-MEINS
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
*---> 轉換單位
      CALL FUNCTION 'CONVERSION_EXIT_RUNIT_INPUT'
        EXPORTING
          INPUT     = T_UOM-MEINH
          LANGUAGE  = SY-LANGU
        IMPORTING
          OUTPUT    = T_UOM-MEINH
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.


      APPEND T_UOM. CLEAR T_UOM.
    ENDAT.
  ENDLOOP.

  IF T_UOM[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " READ_UOM_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_UOM_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_UOM_DATA .
  LOOP AT T_UOM.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_UOM TO T_UOM_LOG.
    PERFORM CLEAR_BAPI_DATA.
    G_HEADDATA-MATERIAL_LONG   = T_UOM-MATNR.  "料號
    G_HEADDATA-BASIC_VIEW = 'X'.            "基本資料檢視

    SELECT SINGLE * FROM MARA WHERE MATNR = T_UOM-MATNR.
    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ENDIF.

*--基礎計量單位改為抓mara
    G_CLIENTDATA-BASE_UOM = MARA-MEINS.   "單位
    G_CLIENTDATAX-BASE_UOM = 'X'.

    GT_UNITSOFMEASURE-ALT_UNIT    = T_UOM-MEINH. "轉換單位
    GT_UNITSOFMEASUREX-ALT_UNIT   = T_UOM-MEINH.

    GT_UNITSOFMEASURE-NUMERATOR   = T_UOM-UMREZ. "轉換單位的分子
    GT_UNITSOFMEASUREX-NUMERATOR  = 'X'.

    GT_UNITSOFMEASURE-DENOMINATR  = T_UOM-UMREN."轉換的分母
    GT_UNITSOFMEASUREX-DENOMINATR = 'X'.

    GT_UNITSOFMEASURE-LENGTH      = T_UOM-LAENG. "長
    GT_UNITSOFMEASUREX-LENGTH     = 'X'.

    GT_UNITSOFMEASURE-WIDTH       = T_UOM-BREIT. "寬
    GT_UNITSOFMEASUREX-WIDTH      = 'X'.

    GT_UNITSOFMEASURE-HEIGHT      = T_UOM-HOEHE.  "高
    GT_UNITSOFMEASUREX-HEIGHT     = 'X'.

    GT_UNITSOFMEASURE-UNIT_DIM    = T_UOM-MEABM.   "長度單位
    GT_UNITSOFMEASUREX-UNIT_DIM   = 'X'.

    GT_UNITSOFMEASURE-GROSS_WT    = T_UOM-BRGEW.  "毛重
    GT_UNITSOFMEASUREX-GROSS_WT   = 'X'.

    GT_UNITSOFMEASURE-UNIT_OF_WT  = T_UOM-GEWEI. "毛重單位
    GT_UNITSOFMEASUREX-UNIT_OF_WT = 'X'.

    APPEND GT_UNITSOFMEASURE.   CLEAR GT_UNITSOFMEASURE.
    APPEND GT_UNITSOFMEASUREX.   CLEAR GT_UNITSOFMEASUREX.
*   CALL BAPI - 維護商品主檔
    PERFORM CALL_BAPI CHANGING T_UOM_LOG-FLAG
                               T_UOM_LOG-ID
                               T_UOM_LOG-NUMBER
                               T_UOM_LOG-MESSAGE.
    APPEND T_UOM_LOG.   CLEAR T_UOM_LOG.
  ENDLOOP.
ENDFORM.                    " PROCESS_UOM_DATA
*&---------------------------------------------------------------------*
*&      FORM  READ_SALES_DATA
*&---------------------------------------------------------------------*
FORM READ_SALES_DATA .
  DATA: L_INDEX TYPE I,
        L_MVGR1 TYPE N LENGTH 3,
        L_NUMC4 TYPE N LENGTH 4.

  FIELD-SYMBOLS: <FS>.

  CLEAR : T_SALES, T_SALES[].
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_SALES TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.

* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = T_SALES-MATNR
        IMPORTING
          OUTPUT       = T_SALES-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.


      TRANSLATE T_SALES-MATNR TO UPPER CASE.
      TRANSLATE T_SALES-MTPOS TO UPPER CASE.
      TRANSLATE T_SALES-MTVFP TO UPPER CASE.

*--物料群組1_應用別
*--補0
      L_MVGR1 = T_SALES-MVGR1.
      T_SALES-MVGR1 = L_MVGR1.

      L_NUMC4 = T_SALES-TRAGR.
      T_SALES-TRAGR = L_NUMC4.
      L_NUMC4 = T_SALES-LADGR.
      T_SALES-LADGR = L_NUMC4.


      APPEND T_SALES. CLEAR T_SALES.
    ENDAT.
  ENDLOOP.

  IF T_SALES[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " READ_SALES_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_SALES_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_SALES_DATA .

  LOOP AT T_SALES.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_SALES TO T_SALES_LOG.
    PERFORM CLEAR_BAPI_DATA.

    G_HEADDATA-MATERIAL_LONG  = T_SALES-MATNR.  "料號

    SELECT SINGLE * FROM MARA WHERE MATNR = T_SALES-MATNR.

    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ENDIF.

    G_HEADDATA-SALES_VIEW = 'X'.            "銷售組織檢視
*   銷售檢視
    PERFORM APPEND_SALES_VIEW.

*   CALL BAPI - 維護商品主檔
    PERFORM CALL_BAPI CHANGING T_SALES_LOG-FLAG
                               T_SALES_LOG-ID
                               T_SALES_LOG-NUMBER
                               T_SALES_LOG-MESSAGE.
    APPEND T_SALES_LOG.   CLEAR T_SALES_LOG.
  ENDLOOP.
ENDFORM.                    " PROCESS_SALES_DATA
*&---------------------------------------------------------------------*
*&      FORM  APPEND_SALES_VIEW
*&---------------------------------------------------------------------*
FORM APPEND_SALES_VIEW .

  G_SALESDATA-SALES_ORG  = T_SALES-VKORG.   "銷售組織
  G_SALESDATAX-SALES_ORG = T_SALES-VKORG.

  G_SALESDATA-DISTR_CHAN  = T_SALES-VTWEG.  "配銷通路
  G_SALESDATAX-DISTR_CHAN = T_SALES-VTWEG.

  G_SALESDATA-DELYG_PLNT  = T_SALES-DWERK.  "交貨廠
  G_SALESDATAX-DELYG_PLNT = 'X'.

  G_SALESDATA-MIN_ORDER  = T_SALES-AUMNG.    "最小訂購量
  G_SALESDATAX-MIN_ORDER = 'X'.

*--跨配銷鍊的物料狀態
  IF T_SALES-MSTAV IS NOT INITIAL.
    G_CLIENTDATA-SAL_STATUS  = T_SALES-MSTAV.
    G_CLIENTDATAX-SAL_STATUS = 'X'.
    G_CLIENTDATA-SVALIDFROM  = T_SALES-MSTDE.
    G_CLIENTDATAX-SVALIDFROM = 'X'.
  ENDIF.

*--銷售 - 鍊 - 相關的檢視的物料狀態
  IF T_SALES-VMSTA IS NOT INITIAL.
    G_SALESDATA-SAL_STATUS  = T_SALES-VMSTA.
    G_SALESDATAX-SAL_STATUS = 'X'.
    G_SALESDATA-VALID_FROM  = T_SALES-VMSTD.
    G_SALESDATAX-VALID_FROM = 'X'.
  ENDIF.



  G_SALESDATA-ACCT_ASSGT  = T_SALES-KTGRM.  "科目指派群組
  G_SALESDATAX-ACCT_ASSGT = 'X'.

  G_SALESDATA-ITEM_CAT  = T_SALES-MTPOS.   "項目類別群組
  G_SALESDATAX-ITEM_CAT = 'X'.

  G_SALESDATA-MATL_GRP_1  = T_SALES-MVGR1.   "物料群組1
  G_SALESDATAX-MATL_GRP_1 = 'X'.

  G_SALESDATA-MATL_GRP_2  = T_SALES-MVGR2.   "物料群組2
  G_SALESDATAX-MATL_GRP_2 = 'X'.

  G_SALESDATA-MATL_GRP_3  = T_SALES-MVGR3.   "物料群組3
  G_SALESDATAX-MATL_GRP_3 = 'X'.

  G_SALESDATA-MATL_GRP_4 = T_SALES-MVGR4.   "物料群組4
  G_SALESDATAX-MATL_GRP_4 = 'X'.

  G_SALESDATA-MATL_GRP_5 = T_SALES-MVGR5.   "物料群組5
  G_SALESDATAX-MATL_GRP_5 = 'X'.

* 稅碼 :
  GT_TAXCLASSIFICATIONS-DEPCOUNTRY = 'TW'.    " T_SALES-ALAND.  "國家
  GT_TAXCLASSIFICATIONS-TAX_TYPE_1 = 'MWST'.  "稅別
  GT_TAXCLASSIFICATIONS-TAXCLASS_1 = T_SALES-TAXKM.  "稅
  APPEND GT_TAXCLASSIFICATIONS.   CLEAR GT_TAXCLASSIFICATIONS.

*-->工廠資料 :
  G_PLANTDATA-PLANT = T_SALES-WERKS.  "工廠
  G_PLANTDATAX-PLANT = T_SALES-WERKS.

*--運輸群組
  G_CLIENTDATA-TRANS_GRP  = T_SALES-TRAGR.
  G_CLIENTDATAX-TRANS_GRP = 'X'.
*--裝載群組
  G_PLANTDATA-LOADINGGRP  = T_SALES-LADGR.
  G_PLANTDATAX-LOADINGGRP = 'X'.

*--> 若MRP 已有, 以MRP 為準
  CLEAR MARC.
  SELECT SINGLE * FROM MARC WHERE MATNR EQ T_SALES-MATNR
                              AND WERKS EQ T_SALES-WERKS.
  IF SY-SUBRC IS NOT INITIAL OR MARC-MTVFP IS INITIAL.
    G_PLANTDATA-AVAILCHECK    = T_SALES-MTVFP.   "可用度檢查
    G_PLANTDATAX-AVAILCHECK   = 'X'.
  ENDIF.

  G_PLANTDATA-BATCH_MGMT    = MARA-XCHPF.   "批次
  G_PLANTDATAX-BATCH_MGMT   = 'X'.

ENDFORM.                    " APPEND_SALES_VIEW
*&---------------------------------------------------------------------*
*&      FORM  READ_PURCHASE_DATA
*&---------------------------------------------------------------------*
FORM READ_PURCHASE_DATA .
  DATA: L_INDEX   TYPE I.
  FIELD-SYMBOLS: <FS>.

  CLEAR : T_PURCH, T_PURCH[].
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_PURCH TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.

*---> TRANSLATE TO UPPER CASE
      TRANSLATE T_PURCH-MATNR TO UPPER CASE.
      TRANSLATE T_PURCH-BSTME TO UPPER CASE.
      TRANSLATE T_PURCH-EKWSL TO UPPER CASE.

*---> 轉換單位
      CALL FUNCTION 'CONVERSION_EXIT_RUNIT_INPUT'
        EXPORTING
          INPUT     = T_PURCH-BSTME
          LANGUAGE  = SY-LANGU
        IMPORTING
          OUTPUT    = T_PURCH-BSTME
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = T_PURCH-MATNR
        IMPORTING
          OUTPUT       = T_PURCH-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.

      APPEND T_PURCH. CLEAR T_PURCH.
    ENDAT.
  ENDLOOP.

  IF T_PURCH[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " READ_PURCHASE_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_PURCHASE_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_PURCHASE_DATA .
  LOOP AT T_PURCH.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_PURCH TO T_PURCH_LOG.
    PERFORM CLEAR_BAPI_DATA.
    SELECT SINGLE * FROM MARA WHERE MATNR = T_PURCH-MATNR.
    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ENDIF.

    G_HEADDATA-MATERIAL_LONG = T_PURCH-MATNR. "料號
    G_HEADDATA-PURCHASE_VIEW = 'X'.           "採購檢視

*   採購檢視
    PERFORM APPEND_PURCH_VIEW.

*   CALL BAPI - 維護商品主檔
    PERFORM CALL_BAPI CHANGING T_PURCH_LOG-FLAG
                               T_PURCH_LOG-ID
                               T_PURCH_LOG-NUMBER
                               T_PURCH_LOG-MESSAGE.

    APPEND T_PURCH_LOG.   CLEAR T_PURCH_LOG.
  ENDLOOP.
ENDFORM.                    " PROCESS_PURCHASE_DATA
*&---------------------------------------------------------------------*
*&      FORM  APPEND_PURCH_VIEW
*&---------------------------------------------------------------------*
FORM APPEND_PURCH_VIEW .
  G_PLANTDATA-PLANT       = T_PURCH-WERKS.  "工廠
  G_PLANTDATAX-PLANT      = T_PURCH-WERKS.

  G_PLANTDATA-PUR_GROUP   = T_PURCH-EKGRP.  "採購群組
  G_PLANTDATAX-PUR_GROUP  = 'X'.

  G_CLIENTDATA-PO_UNIT    = T_PURCH-BSTME.  "訂單單位
  G_CLIENTDATAX-PO_UNIT   = 'X'.

  G_PLANTDATA-AUTO_P_ORD  = T_PURCH-KAUTB.  "自動轉採購單
  G_PLANTDATAX-AUTO_P_ORD = 'X'.

*  G_PLANTDATA-GR_PR_TIME  = T_PURCH-WEBAZ.  "收貨作業處理時間
*  G_PLANTDATAX-GR_PR_TIME = 'X'.

  G_PLANTDATA-SOURCELIST  = T_PURCH-KORDB.  "貨源清單
  G_PLANTDATAX-SOURCELIST = 'X'.

  G_PLANTDATA-BATCH_MGMT    = MARA-XCHPF.   "批次
  G_PLANTDATAX-BATCH_MGMT   = 'X'.

*  "利潤中心
*  IF t_purch-werks = 'MO10'.
*    g_plantdata-profit_ctr = 'MTPM00'.
*  ELSEIF t_purch-werks = 'MO11'.
*    g_plantdata-profit_ctr = 'MTDM00'.
*  ENDIF.
*  g_plantdatax-profit_ctr = 'X'.

  IF T_PURCH-EKWSL IS NOT INITIAL.
    G_CLIENTDATA-PUR_VALKEY = T_PURCH-EKWSL.   "採購值代碼
    G_CLIENTDATAX-PUR_VALKEY = 'X'.
  ENDIF.

ENDFORM.                    " APPEND_PURCH_VIEW
*&---------------------------------------------------------------------*
*&      FORM  READ_MRP_DATA
*&---------------------------------------------------------------------*
FORM READ_MRP_DATA .
  DATA: L_INDEX   TYPE I.
  FIELD-SYMBOLS: <FS>.

  CLEAR : T_MRP, T_MRP[].
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_MRP TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT  = T_MRP-MATNR
        IMPORTING
          OUTPUT = T_MRP-MATNR.

      "CONDENSE T_MRP-MMSTA NO-GAPS.


      APPEND T_MRP. CLEAR T_MRP.
    ENDAT.
  ENDLOOP.

  IF T_MRP[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " READ_MRP_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_MRP_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_MRP_DATA .
  LOOP AT T_MRP.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_MRP TO T_MRP_LOG.
    PERFORM CLEAR_BAPI_DATA.

    G_HEADDATA-MATERIAL_LONG   = T_MRP-MATNR.  "料號
    CLEAR MARA.
    SELECT SINGLE * FROM MARA WHERE MATNR = T_MRP-MATNR.

    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ENDIF.

    IF NOT T_MRP-DISMM IS INITIAL.
      G_HEADDATA-MRP_VIEW = 'X'.            "MRP檢視
    ENDIF.

    IF MARA-MTART = 'FERT' OR MARA-MTART = 'HALB'.
*     成品/半成品要開工作排程視檢
      G_HEADDATA-WORK_SCHED_VIEW = 'X'.     "工作排程檢視
    ENDIF.

    T_MRP_LOG-BISMT = MARA-BISMT.

*   MRP&工作排程檢視&預測
    PERFORM APPEND_MRP_VIEW.
*   CALL BAPI - 維護商品主檔
    PERFORM CALL_BAPI CHANGING T_MRP_LOG-FLAG
                               T_MRP_LOG-ID
                               T_MRP_LOG-NUMBER
                               T_MRP_LOG-MESSAGE.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        INPUT  = T_MRP_LOG-MATNR
      IMPORTING
        OUTPUT = T_MRP_LOG-MATNR.


    APPEND T_MRP_LOG.   CLEAR T_MRP_LOG.
  ENDLOOP.
ENDFORM.                    " PROCESS_MRP_DATA
*&---------------------------------------------------------------------*
*&      FORM  APPEND_MRP_VIEW
*&---------------------------------------------------------------------*
FORM APPEND_MRP_VIEW .
* MRP VIEW
  IF G_HEADDATA-MRP_VIEW = 'X'.

    G_PLANTDATA-PLANT = T_MRP-WERKS.  "工廠
    G_PLANTDATAX-PLANT = T_MRP-WERKS.

    G_PLANTDATA-PUR_STATUS = T_MRP-MMSTA.  "狀態
    G_PLANTDATAX-PUR_STATUS = 'X'.

    G_PLANTDATA-MRP_TYPE = T_MRP-DISMM.  "MRP TYPE
    G_PLANTDATAX-MRP_TYPE = 'X'.

    G_PLANTDATA-REORDER_PT = T_MRP-MINBE.  "再訂購點
    G_PLANTDATAX-REORDER_PT = 'X'.

    G_PLANTDATA-MRP_GROUP = T_MRP-DISGR.  "MRP GROUP
    G_PLANTDATAX-MRP_GROUP = 'X'.

    G_PLANTDATA-MRP_CTRLER = T_MRP-DISPO.  "MRP控制員
    G_PLANTDATAX-MRP_CTRLER = 'X'.

    G_PLANTDATA-LOTSIZEKEY = T_MRP-DISLS.  "批量
    G_PLANTDATAX-LOTSIZEKEY = 'X'.

    G_PLANTDATA-MINLOTSIZE = T_MRP-BSTMI.  "最小批量
    G_PLANTDATAX-MINLOTSIZE = 'X'.

    G_PLANTDATA-MAXLOTSIZE = T_MRP-BSTMA.  "最大批量
    G_PLANTDATAX-MAXLOTSIZE = 'X'.

    G_PLANTDATA-FIXED_LOT = T_MRP-BSTFE.  "固定批量
    G_PLANTDATAX-FIXED_LOT = 'X'.

    G_PLANTDATA-ROUND_VAL = T_MRP-BSTRF.  "捨入值
    G_PLANTDATAX-ROUND_VAL = 'X'.

    G_PLANTDATA-ASSY_SCRAP = T_MRP-AUSSS.  "裝配廢品
    G_PLANTDATAX-ASSY_SCRAP = 'X'.


    G_PLANTDATA-PROC_TYPE = T_MRP-BESKZ.  "採購類型
    G_PLANTDATAX-PROC_TYPE = 'X'.

    G_PLANTDATA-SPPROCTYPE = T_MRP-SOBSL.  "特殊採購
    G_PLANTDATAX-SPPROCTYPE = 'X'.

    G_PLANTDATA-SLOC_EXPRC = T_MRP-LGFSB.  "外部採購儲存位置
    G_PLANTDATAX-SLOC_EXPRC = 'X'.

    G_PLANTDATA-ISS_ST_LOC = T_MRP-LGPRO.  "發貨儲存位置
    G_PLANTDATAX-ISS_ST_LOC = 'X'.

    G_PLANTDATA-BACKFLUSH = T_MRP-RGEKZ.  "倒扣入帳
    G_PLANTDATAX-BACKFLUSH = 'X'.

    G_PLANTDATA-BULK_MAT = T_MRP-SCHGT.  "散裝物料
    G_PLANTDATAX-BULK_MAT = 'X'.

*    IF T_MRP-BESKZ = 'E' AND T_MRP-SOBSL = '50'.  "E50本身須設定散裝
*      G_PLANTDATA-BULK_MAT = T_MRP-SCHGT.  "散裝物料
*    ENDIF.


    G_PLANTDATA-PLND_DELRY = T_MRP-PLIFZ.  "計劃交貨天數
    G_PLANTDATAX-PLND_DELRY = 'X'.

    G_PLANTDATA-INHSEPRODT = T_MRP-DZEIT.  "廠內生產時間
    G_PLANTDATAX-INHSEPRODT = 'X'.

    G_PLANTDATA-GR_PR_TIME = T_MRP-WEBAZ.  "收貨作業處理時間
    G_PLANTDATAX-GR_PR_TIME = 'X'.

    G_PLANTDATA-SAFETY_STK = T_MRP-EISBE.  "安全庫存
    G_PLANTDATAX-SAFETY_STK = 'X'.

*    g_plantdata-min_safety_stk  = t_mrp-eislo.  "最小安全庫存
*    g_plantdatax-min_safety_stk = 'X'.

    G_PLANTDATA-AVAILCHECK = T_MRP-MTVFP.  "可用量檢查
    G_PLANTDATAX-AVAILCHECK = 'X'.

*    g_plantdata-plan_strgp = 'ZM'.         "策略群組
*    g_plantdatax-plan_strgp = 'X'.

*    g_plantdata-consummode = '2'.         "耗用模式
*    g_plantdatax-consummode = 'X'.
*
*    g_plantdata-bwd_cons = '999'.         "消耗期間：向後
*    g_plantdatax-bwd_cons = 'X'.
*
*    g_plantdata-fwd_cons = '999'.         "消耗期間：向前
*    g_plantdatax-fwd_cons = 'X'.

    G_PLANTDATA-DEP_REQ_ID = T_MRP-SBDKZ. "個別/彙整
    G_PLANTDATAX-DEP_REQ_ID = 'X'.

*    IF MARA-MTART <> 'FERT' AND MARA-MTART <> 'HALB'.
*      G_PLANTDATA-DEP_REQ_ID = '2'.         "個別/彙整
*      G_PLANTDATAX-DEP_REQ_ID = 'X'.
*    ELSE.
*      G_PLANTDATA-DEP_REQ_ID = '1'.
*      G_PLANTDATAX-DEP_REQ_ID = 'X'.
*    ENDIF.

    G_PLANTDATA-COMP_SCRAP = T_MRP-KAUSF.  "零件廢品(%)
    G_PLANTDATAX-COMP_SCRAP = 'X'.

    G_PLANTDATA-DISCONTINU = T_MRP-KZAUS.  "中止指示碼
    G_PLANTDATAX-DISCONTINU = 'X'.

    G_PLANTDATA-EFF_O_DAY = T_MRP-AUSDT.  "失效日期
    G_PLANTDATAX-EFF_O_DAY = 'X'.

    G_PLANTDATA-FOLLOW_UP_LONG = T_MRP-NFMAT.  "後續物料
    G_PLANTDATAX-FOLLOW_UP_LONG = 'X'.

*    "利潤中心
*    IF t_mrp-werks = 'MO10'.
*      g_plantdata-profit_ctr = 'MTPM00'.
*    ELSEIF t_mrp-werks = 'MO11'.
*      g_plantdata-profit_ctr = 'MTDM00'.
*    ENDIF.
*    g_plantdatax-profit_ctr = 'X'.
  ENDIF.


* 工作排程 VIEW
  IF G_HEADDATA-WORK_SCHED_VIEW = 'X'.
    G_PLANTDATA-UNDER_TOL = T_MRP-UNETO.  "交貨不足允差(%)
    G_PLANTDATAX-UNDER_TOL = 'X'.

    G_PLANTDATA-OVER_TOL = T_MRP-UEETO.  "過量交貨允差(%)
    G_PLANTDATAX-OVER_TOL = 'X'.

    G_PLANTDATA-BATCH_MGMT    = MARA-XCHPF.   "批次
    G_PLANTDATAX-BATCH_MGMT   = 'X'.

*    G_PLANTDATA-BATCH_MGMT = 'X'.  "批次管理
*    G_PLANTDATAX-BATCH_MGMT = 'X'.

*    g_plantdata-prodprof = '000001'. "生產排程設定檔
*    g_plantdatax-prodprof = 'X'.
  ENDIF.

ENDFORM.                    " APPEND_MRP_VIEW
*&---------------------------------------------------------------------*
*&      FORM  READ_SLOC_DATA
*&---------------------------------------------------------------------*
FORM READ_SLOC_DATA .
  DATA : L_MHDHB(4),
         L_MHDRZ(4).
  DATA: L_INDEX   TYPE I.
  FIELD-SYMBOLS: <FS>.

  CLEAR : T_SLOC, T_SLOC[].
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_SLOC TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.
*---> TRANSLATE TO UPPER CASE
      "TRANSLATE T_SLOC-MATNR TO UPPER CASE.

* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = T_SLOC-MATNR
        IMPORTING
          OUTPUT       = T_SLOC-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1.

*---> 轉換單位
      TRANSLATE T_SLOC-AUSME TO UPPER CASE.

      CALL FUNCTION 'CONVERSION_EXIT_RUNIT_INPUT'
        EXPORTING
          INPUT     = T_SLOC-AUSME
          LANGUAGE  = SY-LANGU
        IMPORTING
          OUTPUT    = T_SLOC-AUSME
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      APPEND T_SLOC.
      CLEAR T_SLOC.
    ENDAT.
  ENDLOOP.

  IF T_SLOC[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " READ_SLOC_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_SLOC_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_SLOC_DATA .
  LOOP AT T_SLOC.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_SLOC TO T_SLOC_LOG.
    PERFORM CLEAR_BAPI_DATA.

    G_HEADDATA-MATERIAL_LONG = T_SLOC-MATNR.  "料號
    SELECT SINGLE * FROM MARA WHERE MATNR = T_SLOC-MATNR.
    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ENDIF.
    G_HEADDATA-STORAGE_VIEW = 'X'.         "儲存檢視
*   儲存檢視
    PERFORM APPEND_SLOC_VIEW.
*   CALL BAPI - 維護商品主檔
    PERFORM CALL_BAPI CHANGING T_SLOC_LOG-FLAG
                               T_SLOC_LOG-ID
                               T_SLOC_LOG-NUMBER
                               T_SLOC_LOG-MESSAGE.
    APPEND T_SLOC_LOG.   CLEAR T_SLOC_LOG.
  ENDLOOP.
ENDFORM.                    " PROCESS_SLOC_DATA
*&---------------------------------------------------------------------*
*&      FORM  APPEND_SLOC_VIEW
*&---------------------------------------------------------------------*
FORM APPEND_SLOC_VIEW .

*  G_STORAGELOCATIONDATA-PLANT = T_SLOC-WERKS.  "工廠
*  G_STORAGELOCATIONDATAX-PLANT = T_SLOC-WERKS.
*
*  G_STORAGELOCATIONDATA-STGE_LOC = T_SLOC-AUSME.  "發貨單位
*  G_STORAGELOCATIONDATAX-STGE_LOC = 'X'.

  G_PLANTDATA-PLANT       = T_SLOC-WERKS.
  G_PLANTDATAX-PLANT      = T_SLOC-WERKS.

  G_PLANTDATA-ISSUE_UNIT    = T_SLOC-AUSME."發貨單位
  G_PLANTDATAX-ISSUE_UNIT   = 'X'.

  G_PLANTDATA-BATCH_MGMT    = MARA-XCHPF.   "批次
  G_PLANTDATAX-BATCH_MGMT   = 'X'.

*  G_PLANTDATA-BATCH_MGMT    = 'X'.   "批次
*  G_PLANTDATAX-BATCH_MGMT   = 'X'.

  IF NOT T_SLOC-MHDHB IS INITIAL.
    G_CLIENTDATA-SHELF_LIFE = T_SLOC-MHDHB.   "總有效期限
    G_CLIENTDATAX-SHELF_LIFE = 'X'.

    G_CLIENTDATA-MINREMLIFE = T_SLOC-MHDRZ.   "最小剩餘有效期限
    G_CLIENTDATAX-MINREMLIFE = 'X'.
  ENDIF.

ENDFORM.                    " APPEND_SLOC_VIEW
*&---------------------------------------------------------------------*
*&      FORM  READ_COST_DATA
*&---------------------------------------------------------------------*
FORM READ_COST_DATA .
  DATA : L_STPRS(11),
         L_PEINH(5),
         L_ZPLP1(13).
  DATA: L_INDEX   TYPE I.
  FIELD-SYMBOLS: <FS>.

  CLEAR : T_COST, T_COST[].
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_COST TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.
*---> TRANSLATE TO UPPER CASE
      "TRANSLATE T_COST-MATNR TO UPPER CASE.
      TRANSLATE T_COST-VPRSV TO UPPER CASE.
* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT  = T_COST-MATNR
        IMPORTING
          OUTPUT = T_COST-MATNR.

* 補0
      ALPHAIN T_COST-PRCTR.

*--利潤中心補0
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = T_COST-PRCTR
        IMPORTING
          OUTPUT = T_COST-PRCTR.

      APPEND T_COST. CLEAR T_COST.
    ENDAT.
  ENDLOOP.

  IF T_COST[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.                    " READ_COST_DATA
*&---------------------------------------------------------------------*
*&      FORM  PROCESS_COST_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_COST_DATA .
  LOOP AT T_COST.
    G_TOTAL = G_TOTAL + 1.

    MOVE-CORRESPONDING T_COST TO T_COST_LOG.

    PERFORM CLEAR_BAPI_DATA.

    G_HEADDATA-MATERIAL_LONG = T_COST-MATNR.  "料號
    SELECT SINGLE * FROM MARA WHERE MATNR = T_COST-MATNR.
    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ELSE.
      G_HEADDATA-MATL_TYPE  = T_COST-MTART.
      G_HEADDATA-IND_SECTOR = 'M'.
    ENDIF.


    G_HEADDATA-ACCOUNT_VIEW = 'X'.

    IF G_HEADDATA-MATL_TYPE <> 'ZNLG'.
      G_HEADDATA-COST_VIEW = 'X'.
    ENDIF.

*   會計&成本檢視
    PERFORM APPEND_COST_VIEW.

*   CALL BAPI - 維護商品主檔
    PERFORM CALL_BAPI CHANGING T_COST_LOG-FLAG
                               T_COST_LOG-ID
                               T_COST_LOG-NUMBER
                               T_COST_LOG-MESSAGE.
    APPEND T_COST_LOG.   CLEAR T_COST_LOG.
  ENDLOOP.
ENDFORM.                    " PROCESS_COST_DATA
*&---------------------------------------------------------------------*
*&      FORM  APPEND_COST_VIEW
*&---------------------------------------------------------------------*
FORM APPEND_COST_VIEW .

  G_VALUATIONDATA-VAL_AREA = T_COST-WERKS.  "工廠=評價範圍
  G_VALUATIONDATAX-VAL_AREA = T_COST-WERKS.

  IF G_HEADDATA-ACCOUNT_VIEW = 'X'.
*   會計檢視
    G_VALUATIONDATA-VAL_CLASS = T_COST-BKLAS.  "評價類別
    G_VALUATIONDATAX-VAL_CLASS = 'X'.

*    g_valuationdata-ml_active = t_cost-mlmaa.  "ML 啟動
*    g_valuationdatax-ml_active = 'X'.

*--銷售訂單-評價類別(在途存貨)
    G_VALUATIONDATA-VM_SO_STK  = T_COST-EKLAS.  "評價類別
    G_VALUATIONDATAX-VM_SO_STK = 'X'.

    G_VALUATIONDATA-ML_SETTLE = T_COST-MLAST.  "價格決定
    G_VALUATIONDATAX-ML_SETTLE = 'X'.

    G_VALUATIONDATA-PRICE_UNIT = T_COST-PEINH.  "價格單位
    G_VALUATIONDATAX-PRICE_UNIT = 'X'.

    G_VALUATIONDATA-PRICE_CTRL = T_COST-VPRSV.  "價格控制
    G_VALUATIONDATAX-PRICE_CTRL = 'X'.

    "IF G_HEADDATA-MATL_TYPE = '1000' OR G_HEADDATA-MATL_TYPE = '5000'.
*    g_valuationdata-moving_pr = t_cost-pvprs.  "定期單價
*    g_valuationdatax-moving_pr = 'X'.
    "ENDIF.
  ENDIF.

  IF G_HEADDATA-COST_VIEW = 'X'.
*   成本檢視
    G_VALUATIONDATA-QTY_STRUCT = T_COST-EKALR.  "含QS成本估算
    G_VALUATIONDATAX-QTY_STRUCT = 'X'.

    G_VALUATIONDATA-ORIG_MAT = T_COST-HKMAT.  "物料來源
    G_VALUATIONDATAX-ORIG_MAT = 'X'.

*--來源群組
    G_VALUATIONDATA-ORIG_GROUP = T_COST-HRKFT.
    G_VALUATIONDATAX-ORIG_GROUP = 'X'.

    G_PLANTDATA-PLANT = T_COST-WERKS.  "工廠=評價範圍
    G_PLANTDATAX-PLANT = T_COST-WERKS.

    G_PLANTDATA-PROFIT_CTR = T_COST-PRCTR.   "利潤中心
    G_PLANTDATAX-PROFIT_CTR = 'X'.

    G_PLANTDATA-LOT_SIZE = T_COST-LOSGR.   "成本計算批量
    G_PLANTDATAX-LOT_SIZE = 'X'.


*    g_valuationdata-plndprice1 = t_cost-zplp1.  "計劃價格1
*    g_valuationdatax-plndprice1 = 'X'.
*
*    g_valuationdata-plndprdate1 = t_cost-zpld1.  "計劃價格日期1
*    g_valuationdatax-plndprdate1 = 'X'.
  ENDIF.

  G_PLANTDATA-PLANT       = T_COST-WERKS.  "工廠
  G_PLANTDATAX-PLANT      = T_COST-WERKS.

  G_PLANTDATA-BATCH_MGMT    = MARA-XCHPF.   "批次
  G_PLANTDATAX-BATCH_MGMT   = 'X'.

  IF T_COST-MMSTA IS NOT INITIAL.
    G_PLANTDATA-PUR_STATUS  = T_COST-MMSTA.  "狀態
    G_PLANTDATAX-PUR_STATUS = 'X'.
  ENDIF.

ENDFORM.                    " APPEND_COST_VIEW
****************************************************************
* FORM TOP_OF_PAGE                                             *
****************************************************************
FORM TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.

  IF G_TOTAL IS INITIAL.
    G_TOTAL = '0'.
  ENDIF.
  IF G_SUCCESS IS INITIAL.
    G_SUCCESS = '0'.
  ENDIF.
  IF G_FAIL IS INITIAL.
    G_FAIL = '0'.
  ENDIF.
*
  REFRESH G_LIST_TOP_OF_PAGE.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  CONCATENATE 'TOTAL筆數:' G_TOTAL INTO LS_LINE-INFO.
  APPEND LS_LINE TO G_LIST_TOP_OF_PAGE.
  LS_LINE-TYP  = 'S'.
  CONCATENATE '成功筆數 :' G_SUCCESS INTO LS_LINE-INFO.
  APPEND LS_LINE TO G_LIST_TOP_OF_PAGE.
  LS_LINE-TYP  = 'S'.
  CONCATENATE '失敗筆數 :' G_FAIL INTO LS_LINE-INFO.
  APPEND LS_LINE TO G_LIST_TOP_OF_PAGE.
  CLEAR : LS_LINE.
  APPEND LS_LINE TO G_LIST_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = G_LIST_TOP_OF_PAGE.
ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      FORM  UPLOAD_EX_DATA
*&---------------------------------------------------------------------*
FORM UPLOAD_EX_DATA .

  IF SY-MANDT = '100'.
    MESSAGE E000 WITH 'CAN NOT UPLOAD DATA IN CLIENT 100'.
    STOP.
  ENDIF.

  IF P_FILE = SPACE.
    MESSAGE E000 WITH 'PLEASE INPUT FILE PATH FOR UPLOAD FUNCTION'.
    STOP.
  ENDIF.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = P_FILE
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 100
      I_END_ROW               = 65536
    TABLES
      INTERN                  = GT_UPLOAD
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE S001(00) WITH '讀取檔案失敗' SY-SUBRC DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SORT GT_UPLOAD BY ROW COL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_LONG_TEXT_DATA
*&---------------------------------------------------------------------*
FORM READ_LONG_TEXT_DATA .
  DATA: L_INDEX   TYPE I.

  CLEAR : T_LONT, T_LONT[].
  FIELD-SYMBOLS: <FS>.
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_LONT TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.
      TRANSLATE T_LONT-MATNR TO UPPER CASE.

* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT  = T_LONT-MATNR
        IMPORTING
          OUTPUT = T_LONT-MATNR.

      APPEND T_LONT. CLEAR T_LONT.
    ENDAT.
  ENDLOOP.

  IF T_LONT[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_LONG_TEXT
*&---------------------------------------------------------------------*
FORM PROCESS_LONG_TEXT .
  LOOP AT T_LONT.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_LONT TO T_LONT_LOG.
    PERFORM CLEAR_BAPI_DATA.


    G_HEADDATA-MATERIAL_LONG   = T_LONT-MATNR.  "料號

    CLEAR MARA.
    SELECT SINGLE * FROM MARA WHERE MATNR = T_LONT-MATNR.

    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ENDIF.

*   長文資料
    PERFORM APPEND_LONG_TEXT.

*   CALL BAPI - 維護商品主檔
    PERFORM CALL_BAPI CHANGING T_LONT_LOG-FLAG
                               T_LONT_LOG-ID
                               T_LONT_LOG-NUMBER
                               T_LONT_LOG-MESSAGE.
    APPEND T_LONT_LOG.   CLEAR T_LONT_LOG.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_LONG_TEXT
*&---------------------------------------------------------------------*
FORM APPEND_LONG_TEXT .

  GT_MATERIALLONGTEXT-APPLOBJECT = T_LONT-APPLOBJECT.
  GT_MATERIALLONGTEXT-TEXT_NAME  = T_LONT-MATNR.
  GT_MATERIALLONGTEXT-TEXT_ID    = T_LONT-TEXT_ID.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      INPUT            = T_LONT-LANGU
    IMPORTING
      OUTPUT           = GT_MATERIALLONGTEXT-LANGU
    EXCEPTIONS
      UNKNOWN_LANGUAGE = 1
      OTHERS           = 2.

  GT_MATERIALLONGTEXT-TEXT_LINE = T_LONT-TEXT_LINE.

  APPEND GT_MATERIALLONGTEXT. CLEAR GT_MATERIALLONGTEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_ALV_FIELD
*&---------------------------------------------------------------------*
FORM SET_ALV_FIELD .

  PERFORM GET_FIELDNAME USING 'FLAG'     '執行結果'.
  PERFORM GET_FIELDNAME USING 'ID'       '訊息ID'.
  PERFORM GET_FIELDNAME USING 'NUMBER'   '訊息編碼'.
  PERFORM GET_FIELDNAME USING 'MESSAGE'  '訊息內容'.
  PERFORM GET_FIELDNAME USING 'MATNR'    '物料號碼'.

  IF P_BASIC = 'X'.
    PERFORM GET_FIELDNAME USING 'MTART'    '物料類型'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明-中文'.
    PERFORM GET_FIELDNAME USING 'EMAKTX'   '物料說明-英文'.
    PERFORM GET_FIELDNAME USING 'MEINS'    '基礎計量單位'.
    PERFORM GET_FIELDNAME USING 'BISMT'    '舊物料號碼'.
    PERFORM GET_FIELDNAME USING 'MATKL'    '物料群組'.
    PERFORM GET_FIELDNAME USING 'XCHPF'    '批次管理'.
    PERFORM GET_FIELDNAME USING 'SPART'    '產品'.
    PERFORM GET_FIELDNAME USING 'BRGEW'    '毛重'.
    PERFORM GET_FIELDNAME USING 'NTGEW'    '淨重'.
    PERFORM GET_FIELDNAME USING 'GEWEI'    '重量單位'.
    PERFORM GET_FIELDNAME USING 'AESZN'    '線徑'.
    PERFORM GET_FIELDNAME USING 'GROES'    'Dimensions'.
*    PERFORM get_fieldname USING 'WRKST'      'Dimensions'.
    PERFORM GET_FIELDNAME USING 'ZEINR'      'HSCode'.
    PERFORM GET_FIELDNAME USING 'ZEIVR'      '圖面版次'.
*    PERFORM get_fieldname USING 'MHDHB'      '總有效期限'.
*    PERFORM get_fieldname USING 'MHDRZ'      '最小剩餘有效期限'.
    PERFORM GET_FIELDNAME USING 'MSTAE'      '跨廠物料狀態'.
    PERFORM GET_FIELDNAME USING 'LONGTEXT1'  '表面處理'.
    PERFORM GET_FIELDNAME USING 'LONGTEXT2'  '熱處理條件'.
    PERFORM GET_FIELDNAME USING 'LONGTEXT3'  '發票英文品名'.
*    PERFORM get_fieldname USING 'EKWSL'      '採購值代碼'.

  ELSEIF P_MRP = 'X'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明'.
    PERFORM GET_FIELDNAME USING 'MTART'    '物料類型'.
    PERFORM GET_FIELDNAME USING 'WERKS'    '工廠'.
    PERFORM GET_FIELDNAME USING 'MMSTA'    '工廠特定狀態'.
    PERFORM GET_FIELDNAME USING 'DISMM'    'MRP類型'.
    PERFORM GET_FIELDNAME USING 'MINBE'    '再訂購點'.
    PERFORM GET_FIELDNAME USING 'DISGR'    'MRP群組'.
    PERFORM GET_FIELDNAME USING 'DISPO'    'MRP控制員'.
    PERFORM GET_FIELDNAME USING 'DISLS'    '批量'.
    PERFORM GET_FIELDNAME USING 'BSTFE'    '固定批量'.
    PERFORM GET_FIELDNAME USING 'BSTMI'    '最小批量'.
    PERFORM GET_FIELDNAME USING 'BSTMA'    '最大批量'.
    PERFORM GET_FIELDNAME USING 'BSTRF'    '捨入值'.
    PERFORM GET_FIELDNAME USING 'AUSSS'    '裝配廢品(%)'.
    PERFORM GET_FIELDNAME USING 'BESKZ'    '採購類型'.
    PERFORM GET_FIELDNAME USING 'SOBSL'    '特殊採購類型'.
    PERFORM GET_FIELDNAME USING 'LGFSB'    '外部採購的儲存位置'.
    PERFORM GET_FIELDNAME USING 'LGPRO'    '發貨儲存位置'.
    PERFORM GET_FIELDNAME USING 'RGEKZ'    '倒扣入帳'.
    PERFORM GET_FIELDNAME USING 'SCHGT'    '散裝物料'.
    PERFORM GET_FIELDNAME USING 'DZEIT'    '廠內生產時間'.
    PERFORM GET_FIELDNAME USING 'PLIFZ'    '計劃交貨天數'.
    PERFORM GET_FIELDNAME USING 'WEBAZ'    '收貨作業處理時間'.
    PERFORM GET_FIELDNAME USING 'EISBE'    '安全庫存'.
    PERFORM GET_FIELDNAME USING 'MTVFP'    '可用度檢查'.
    PERFORM GET_FIELDNAME USING 'KAUSF'    '零件廢品(%)'.
    PERFORM GET_FIELDNAME USING 'KZAUS'    '中止指示碼'.
    PERFORM GET_FIELDNAME USING 'AUSDT'    '失效日期'.
    PERFORM GET_FIELDNAME USING 'NFMAT'    '後續物料'.
    PERFORM GET_FIELDNAME USING 'UNETO'    '交貨不足允差'.
    PERFORM GET_FIELDNAME USING 'UEETO'    '過量交貨允差'.
    PERFORM GET_FIELDNAME USING 'BSTFE'    '固定批量'.

  ELSEIF P_QM = 'X'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明'.
    PERFORM GET_FIELDNAME USING 'WERKS'    '工廠'.
    PERFORM GET_FIELDNAME USING 'ART01'    '01類型'.
    PERFORM GET_FIELDNAME USING 'ACT01'    '01啟用'.
    PERFORM GET_FIELDNAME USING 'ART0130'  '0130類型'.
    PERFORM GET_FIELDNAME USING 'ACT0130'  '0130啟用'.
    PERFORM GET_FIELDNAME USING 'ART0130Z' '0130Z類型'.
    PERFORM GET_FIELDNAME USING 'ACT0130Z' '0130Z啟用'.
    PERFORM GET_FIELDNAME USING 'ART03'    '03類型'.
    PERFORM GET_FIELDNAME USING 'ACT03'    '03啟用'.
    PERFORM GET_FIELDNAME USING 'ART05'    '05類型'.
    PERFORM GET_FIELDNAME USING 'ACT05'    '05啟用'.
    PERFORM GET_FIELDNAME USING 'ART89'    '89類型'.
    PERFORM GET_FIELDNAME USING 'ACT89'    '89啟用'.
    PERFORM GET_FIELDNAME USING 'PRFRQ'    '間隔天數'.

  ELSEIF P_PURCH = 'X'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明'.
    PERFORM GET_FIELDNAME USING 'WERKS'    '工廠'.
    PERFORM GET_FIELDNAME USING 'EKGRP'    '採購群組'.
    PERFORM GET_FIELDNAME USING 'BSTME'    '訂單單位'.
    PERFORM GET_FIELDNAME USING 'KAUTB'    '自動轉採購單'.
    "PERFORM GET_FIELDNAME USING 'WEBAZ'    '收貨作業處理時間'.
    PERFORM GET_FIELDNAME USING 'KORDB'    '貨源清單'.
    PERFORM GET_FIELDNAME USING 'EKWSL'    '採購值代碼'.

  ELSEIF P_SALES = 'X'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明'.
    PERFORM GET_FIELDNAME USING 'WERKS'    '工廠'.
    PERFORM GET_FIELDNAME USING 'VKORG'    '銷售組織'.
    PERFORM GET_FIELDNAME USING 'VTWEG'    '配銷通路'.
    PERFORM GET_FIELDNAME USING 'DWERK'    '預計交貨廠'.
    PERFORM GET_FIELDNAME USING 'TAXKM'    '稅碼'.
    PERFORM GET_FIELDNAME USING 'AUMNG'    '最小訂購量'.

    PERFORM GET_FIELDNAME USING 'MSTAV'    '跨配銷鍊的物料狀態'.
    PERFORM GET_FIELDNAME USING 'MSTDE'    '跨工廠物料狀態的起始日期是有效的'.
    PERFORM GET_FIELDNAME USING 'VMSTA'    '銷售鍊-相關的檢視的物料狀態'.
    PERFORM GET_FIELDNAME USING 'VMSTD'    '銷售的物料狀態開始有效日'.

    PERFORM GET_FIELDNAME USING 'KTGRM'    '科目指派群組'.
    PERFORM GET_FIELDNAME USING 'MTPOS'    '項目類別群組'.
    PERFORM GET_FIELDNAME USING 'MVGR1'    '物料群組 1(產品大分類)'.
    PERFORM GET_FIELDNAME USING 'MVGR2'    '物料群組 2'.
    PERFORM GET_FIELDNAME USING 'MVGR3'    '物料群組 3'.
    PERFORM GET_FIELDNAME USING 'MVGR4'    '物料群組 4'.
    PERFORM GET_FIELDNAME USING 'MVGR5'    '物料群組 5'.
    PERFORM GET_FIELDNAME USING 'TRAGR'    '運輸群組'.
    PERFORM GET_FIELDNAME USING 'LADGR'    '裝載群組'.
    PERFORM GET_FIELDNAME USING 'MTVFP'    '可用度檢查'.

  ELSEIF P_UOM = 'X'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明'.
    PERFORM GET_FIELDNAME USING 'MEINS'    '基礎計量單位'.
    PERFORM GET_FIELDNAME USING 'UMREZ'    '轉換分子'.
    PERFORM GET_FIELDNAME USING 'MEINH'    '替代計量單位'.
    PERFORM GET_FIELDNAME USING 'UMREN'    '轉換分母'.
    PERFORM GET_FIELDNAME USING 'BRGEW'    '毛重'.
    PERFORM GET_FIELDNAME USING 'GEWEI'    '毛重單位'.
    PERFORM GET_FIELDNAME USING 'LAENG'    '長'.
    PERFORM GET_FIELDNAME USING 'BREIT'    '寬'.
    PERFORM GET_FIELDNAME USING 'HOEHE'    '高'.
    PERFORM GET_FIELDNAME USING 'MEABM'    '長度單位'.

  ELSEIF P_SLOC = 'X'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明'.
    PERFORM GET_FIELDNAME USING 'WERKS'    '工廠'.
    PERFORM GET_FIELDNAME USING 'AUSME'    '發貨單位'.
    PERFORM GET_FIELDNAME USING 'MHDHB'    '總有效期限'.
    PERFORM GET_FIELDNAME USING 'MHDRZ'    '最小剩餘有效期限'.

  ELSEIF P_COST = 'X'.
    PERFORM GET_FIELDNAME USING 'MTART'    '物料類型'.
    PERFORM GET_FIELDNAME USING 'MAKTX'    '物料說明'.
    PERFORM GET_FIELDNAME USING 'WERKS'    '工廠'.
    PERFORM GET_FIELDNAME USING 'BKLAS'    '評價類別'.
    PERFORM GET_FIELDNAME USING 'EKLAS'    '銷售訂單-評價類別(在途存貨)'.
*    PERFORM get_fieldname USING 'MLMAA'    'ML 啟動'.
    PERFORM GET_FIELDNAME USING 'MLAST'    '價格決定'.
    PERFORM GET_FIELDNAME USING 'VPRSV'    '價格控制'.
*    PERFORM get_fieldname USING 'PVPRS'    '定期單價'.
    PERFORM GET_FIELDNAME USING 'PEINH'    '公司幣別價格單位'.
*    PERFORM get_fieldname USING 'PEINH_1'  '利潤中心幣別價格單位'.
*    PERFORM get_fieldname USING 'PEINH_2'  '集團幣別價格單位'.

    PERFORM GET_FIELDNAME USING 'EKALR'    '含QS成本估算'.
    PERFORM GET_FIELDNAME USING 'HKMAT'    '物料來源'.
    PERFORM GET_FIELDNAME USING 'HRKFT'    '來源群組'.
    PERFORM GET_FIELDNAME USING 'PRCTR'    '利潤中心'.
    PERFORM GET_FIELDNAME USING 'LOSGR'    '成本計算批量'.
    PERFORM GET_FIELDNAME USING 'MMSTA'    '工廠特定物料狀態'.
*    PERFORM get_fieldname USING 'ZPLP1'    '計劃價格一'.
*    PERFORM get_fieldname USING 'ZPLD1'    '計劃價格日期一'.

  ELSEIF P_LOT = 'X'.
    PERFORM GET_FIELDNAME USING 'TEXT_ID'     '內文ID'.
    PERFORM GET_FIELDNAME USING 'LANGU'       '語系'.
    PERFORM GET_FIELDNAME USING 'APPLOBJECT'  '內文物件'.
    PERFORM GET_FIELDNAME USING 'TEXT_LINE'   '內文'.

*--比例_產品單位(KG_MPC)
  ELSEIF P_UR = 'X'.
    PERFORM GET_FIELDNAME USING 'MAKTX'       '物料說明'.
    PERFORM GET_FIELDNAME USING 'KZWSM'       '計量單位的使用'.
    PERFORM GET_FIELDNAME USING 'ATNAM'       '特性名稱'.
    PERFORM GET_FIELDNAME USING 'ATWRT'       '計劃值'.
    PERFORM GET_FIELDNAME USING 'WSMEI'       '特定批次物料計量單位'.
    PERFORM GET_FIELDNAME USING 'XFHDW'       '主要特定批次計量單位'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDNAME
*&---------------------------------------------------------------------*
FORM GET_FIELDNAME  USING  IN_STR  IN_FIL.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

  LS_FIELDCAT-FIELDNAME   = IN_STR .
  LS_FIELDCAT-SELTEXT_M   = IN_FIL.

  APPEND LS_FIELDCAT TO GT_FIELDCAT.
  CLEAR  LS_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_ALV
*&---------------------------------------------------------------------*
FORM SHOW_ALV  TABLES  T_ALV .

  G_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  G_LAYOUT-ZEBRA = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-CPROG
      I_CALLBACK_TOP_OF_PAGE = 'TOP_OF_PAGE'
*     I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
*     I_CALLBACK_USER_COMMAND  = 'USRCMD'
      IS_LAYOUT              = G_LAYOUT
      I_SAVE                 = 'A'
      IT_FIELDCAT            = GT_FIELDCAT[]
      IT_EVENTS              = G_EVENTS[]
      IT_SORT                = GT_SORT[]
    TABLES
      T_OUTTAB               = T_ALV
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CLASSFICATION
*&---------------------------------------------------------------------*
FORM CREATE_CLASSFICATION .
  DATA: L_OBJECT      LIKE BAPI1003_KEY-OBJECT_LONG,
        L_TABLE       LIKE BAPI1003_KEY-OBJECTTABLE,
        L_CLASSNUM    LIKE BAPI1003_KEY-CLASSNUM,
        L_CLASSTYPE   LIKE BAPI1003_KEY-CLASSTYPE,
        LT_RETURN     LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE,
        LT_ALLOC_LIST LIKE BAPI1003_ALLOC_LIST OCCURS 0 WITH HEADER LINE.


  L_OBJECT    = T_BASIC-MATNR.
  L_TABLE     = 'MARA'.
  L_CLASSNUM  = 'Z_BATCH'.
  L_CLASSTYPE = '023'.

  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
*     OBJECTKEY_IMP      = L_OBJECT
      OBJECTTABLE_IMP    = L_TABLE
      CLASSTYPE_IMP      = L_CLASSTYPE
*     READ_VALUATIONS    =
*     KEYDATE            = SY-DATUM
*     LANGUAGE           = SY-LANGU
      OBJECTKEY_IMP_LONG = L_OBJECT
    TABLES
      ALLOCLIST          = LT_ALLOC_LIST
*     ALLOCVALUESCHAR    =
*     ALLOCVALUESCURR    =
*     ALLOCVALUESNUM     =
      RETURN             = LT_RETURN.

  READ TABLE LT_ALLOC_LIST WITH KEY CLASSNUM = L_CLASSNUM.

  IF SY-SUBRC = 0.
    EXIT.
  ELSE.
    CLEAR LT_RETURN[].
  ENDIF.

  CALL FUNCTION 'BAPI_OBJCL_CREATE'
    EXPORTING
*     OBJECTKEYNEW      = L_OBJECT
      OBJECTTABLENEW    = L_TABLE
      CLASSNUMNEW       = L_CLASSNUM
      CLASSTYPENEW      = L_CLASSTYPE
*     STATUS            = '1'
*     STANDARDCLASS     =
*     CHANGENUMBER      =
*     KEYDATE           = SY-DATUM
*     NO_DEFAULT_VALUES = ' '
      OBJECTKEYNEW_LONG = L_OBJECT
*   IMPORTING
*     CLASSIF_STATUS    =
    TABLES
*     ALLOCVALUESNUM    =
*     ALLOCVALUESCHAR   =
*     ALLOCVALUESCURR   =
      RETURN            = LT_RETURN.

  READ TABLE LT_RETURN WITH KEY TYPE = 'E'.

  IF SY-SUBRC = 0.
    G_RETURN-TYPE    = LT_RETURN-TYPE.
    G_RETURN-ID      = LT_RETURN-ID.
    G_RETURN-NUMBER  = LT_RETURN-NUMBER.
    G_RETURN-MESSAGE = LT_RETURN-MESSAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_QM_DATA
*&---------------------------------------------------------------------*
FORM READ_QM_DATA .
  DATA: L_INDEX   TYPE I.
  FIELD-SYMBOLS: <FS>.

  CLEAR : T_QM, T_QM[].
  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_QM TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.
*---> TRANSLATE TO UPPER CASE
      TRANSLATE T_QM-MATNR TO UPPER CASE.

* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = T_QM-MATNR
        IMPORTING
          OUTPUT       = T_QM-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1
          OTHERS       = 2.

      APPEND T_QM. CLEAR T_QM.
    ENDAT.
  ENDLOOP.

  IF T_QM[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_QM_DATA
*&---------------------------------------------------------------------*
FORM PROCESS_QM_DATA .

  LOOP AT T_QM.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_QM TO T_QM_LOG.
    PERFORM CLEAR_BAPI_DATA.
    SELECT SINGLE * FROM MARA WHERE MATNR = T_QM-MATNR.
    IF SY-SUBRC = 0.
      G_HEADDATA-IND_SECTOR = MARA-MBRSH.
      G_HEADDATA-MATL_TYPE = MARA-MTART.
    ENDIF.
    G_HEADDATA-MATERIAL_LONG = T_QM-MATNR.  "料號
    G_HEADDATA-QUALITY_VIEW = 'X'.     "品管檢視

*   品管檢視
    PERFORM APPEND_QM_VIEW.
*   CALL BAPI
    PERFORM CALL_BAPI CHANGING T_QM_LOG-FLAG
                               T_QM_LOG-ID
                               T_QM_LOG-NUMBER
                               T_QM_LOG-MESSAGE.
    APPEND T_QM_LOG.   CLEAR T_QM_LOG.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  APPEND_QM_VIEW
*&---------------------------------------------------------------------*
FORM APPEND_QM_VIEW .

  G_PLANTDATA-PLANT  = T_QM-WERKS.  "工廠
  G_PLANTDATAX-PLANT = T_QM-WERKS.

  IF T_QM-PRFRQ IS NOT INITIAL.
    G_PLANTDATA-INSP_INT = T_QM-PRFRQ.
    G_PLANTDATAX-INSP_INT = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESS_QPART
*&---------------------------------------------------------------------*
FORM PROCESS_QPART .
  DATA: LS_MARA LIKE MARA,
        L_INDEX TYPE I.

  LOOP AT T_QM_LOG WHERE FLAG = 'S'.
    L_INDEX = SY-TABIX.

    CLEAR: LS_MARA.
    CLEAR: GT_INSP, GT_INSP[].

    SELECT SINGLE * INTO LS_MARA
      FROM MARA
     WHERE MATNR EQ T_QM_LOG-MATNR.


    IF T_QM_LOG-ART01 <> ''.
      PERFORM ASSIGN_QPART USING '01' T_QM_LOG-ACT01.
    ENDIF.

    IF T_QM_LOG-ART0130 <> ''.
      PERFORM ASSIGN_QPART USING '0130' T_QM_LOG-ACT0130.
    ENDIF.

    IF T_QM_LOG-ART0130Z <> ''.
      PERFORM ASSIGN_QPART USING '0130Z' T_QM_LOG-ACT0130Z.
    ENDIF.

    IF T_QM_LOG-ART03 <> ''.
      PERFORM ASSIGN_QPART USING '03' T_QM_LOG-ACT03.
    ENDIF.

    IF T_QM_LOG-ART05 <> ''.
      PERFORM ASSIGN_QPART USING '05' T_QM_LOG-ACT05.
    ENDIF.

    IF T_QM_LOG-ART89 <> ''.
      PERFORM ASSIGN_QPART USING '89' T_QM_LOG-ACT89.
    ENDIF.

    CALL FUNCTION 'BAPI_MATINSPCTRL_SAVEREPLICA'
      TABLES
        RETURN         = GT_RETURN2
        INSPECTIONCTRL = GT_INSP.

    READ TABLE GT_RETURN2 WITH KEY TYPE = 'E'.

    IF SY-SUBRC EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      LOOP AT GT_RETURN2 WHERE TYPE = 'E'.
        T_QM_LOG-FLAG    = 'F'.
        T_QM_LOG-MESSAGE = GT_RETURN2-MESSAGE.

      ENDLOOP.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.
    ENDIF.

    MODIFY T_QM_LOG INDEX L_INDEX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_QPART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ASSIGN_QPART  USING IN_ART IN_ACT.
  DATA: LS_TQ34   LIKE TQ34.

  CLEAR LS_TQ34.
  SELECT SINGLE * INTO LS_TQ34
    FROM TQ34
   WHERE ART EQ IN_ART.

  GT_INSP-INSPTYPE                   = IN_ART.
  GT_INSP-MATERIAL_LONG              = T_QM_LOG-MATNR.
  GT_INSP-PLANT                      = T_QM_LOG-WERKS.
  GT_INSP-IND_INSP_WITH_TSK_LIST     = LS_TQ34-PPL.
  "以工作細項清單檢驗
  GT_INSP-IND_SPEC_MATSPEC           = LS_TQ34-SPEZUEBER.
  "以物料規格檢驗
  GT_INSP-IND_SPEC_CONFIG            = LS_TQ34-CONF.
  "組態檢驗規格
  GT_INSP-IND_SPEC_BATCH             = LS_TQ34-TLS.
  "批次決定的檢驗規格
  GT_INSP-IND_AUTO_ASSIGN             = LS_TQ34-APP.
  "自動規格指派
  GT_INSP-IND_INSP_BY_CHARAC         = LS_TQ34-MER.
  "按特性檢驗
  GT_INSP-IND_POST_TO_INSP_STOCK     = LS_TQ34-INSMK.
  "過帳至檢驗庫存
  GT_INSP-IND_AUTOMATIC_UD           = LS_TQ34-AVE.
  "已計畫自動檢驗結果判定
  GT_INSP-SAMPLING_PROCEDURE         = LS_TQ34-STICHPRVER.
  "抽樣程序
  GT_INSP-DYN_MODIF_RULE             = LS_TQ34-DYNREGEL.
  "加嚴減量修正規則
  GT_INSP-INSP_PERCENTAGE            = LS_TQ34-SPROZ.
  "檢驗比例
  GT_INSP-IND_100_PERCENT_INSPECTION = LS_TQ34-HPZ.
  "100% 檢驗
  GT_INSP-IND_SKIPS_ALLOWED         = LS_TQ34-DYN.
  "允許跳過
  GT_INSP-IND_MANUAL_SAMPLE          = LS_TQ34-MPB.
  "請人工輸入樣本
  GT_INSP-IND_MANUAL_SAMPLE_CALC     = LS_TQ34-MST.
  "人工驅動樣本計算
  GT_INSP-IND_SINGLE_UNITS_POSSIBLE  = LS_TQ34-EIN.
  "可以做序號管理
  GT_INSP-AVE_INSP_DURATION          = LS_TQ34-MPDAU.
  "平均檢驗持續時間
  GT_INSP-CONTR_INSP_LOT_CREATE      = LS_TQ34-CHG.
  "檢驗批建立控制﹝批量彙總﹞
  GT_INSP-QUAL_SCORE_PROCEDURE       = LS_TQ34-QKZVERF.
  "計算品質評分的程序
  GT_INSP-ALLOWED_SCRAP_SHARE        = LS_TQ34-QPMAT.
  "檢驗批中所允許的報廢比例﹝百分比﹞
  GT_INSP-IND_HU_INSPECTION          = LS_TQ34-AFR.
  "處理單位檢驗

  GT_INSP-IND_INSPTYPE_MAT_ACTIVE   = 'X'.  "啟用檢驗類型

*--20230214 ALICE
*--優先檢驗類型不要勾
*  IF IN_ACT IS NOT INITIAL.
*    GT_INSP-PREFERRED_INSPTYPE        = 'X'.  "優先檢驗類型
*  ENDIF.

  APPEND GT_INSP. CLEAR GT_INSP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LONGTEXT
*&---------------------------------------------------------------------*
FORM GET_LONGTEXT .

*--基本內文
  GT_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
  GT_MATERIALLONGTEXT-TEXT_NAME  = T_BASIC-MATNR.
  GT_MATERIALLONGTEXT-TEXT_ID    = 'GRUN'.

*  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
*    EXPORTING
*      input            = t_lont-langu
*    IMPORTING
*      output           = gt_materiallongtext-langu
*    EXCEPTIONS
*      unknown_language = 1
*      OTHERS           = 2.

  GT_MATERIALLONGTEXT-LANGU = SY-LANGU.
  GT_MATERIALLONGTEXT-TEXT_LINE = T_BASIC-LONGTEXT1.

  APPEND GT_MATERIALLONGTEXT. CLEAR GT_MATERIALLONGTEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_longtext_1
*&---------------------------------------------------------------------*
FORM GET_LONGTEXT_1 .

  GT_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
  GT_MATERIALLONGTEXT-TEXT_NAME  = T_BASIC-MATNR.
  GT_MATERIALLONGTEXT-TEXT_ID    = 'PRUE'.

  GT_MATERIALLONGTEXT-LANGU = SY-LANGU.
  GT_MATERIALLONGTEXT-TEXT_LINE = T_BASIC-LONGTEXT2.

  APPEND GT_MATERIALLONGTEXT. CLEAR GT_MATERIALLONGTEXT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form read_kgmpc_data
*&---------------------------------------------------------------------*
FORM READ_KGMPC_DATA .
  DATA:L_INDEX   TYPE I.
  FIELD-SYMBOLS: <FS>.

  CLEAR:T_KGMPC, T_KGMPC[].

  LOOP AT GT_UPLOAD.
    MOVE GT_UPLOAD-COL TO L_INDEX.
    ASSIGN COMPONENT L_INDEX OF STRUCTURE T_KGMPC TO <FS>.
    MOVE GT_UPLOAD-VALUE TO <FS>.

    AT END OF ROW.

* 料號補0
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          INPUT        = T_KGMPC-MATNR
        IMPORTING
          OUTPUT       = T_KGMPC-MATNR
        EXCEPTIONS
          LENGTH_ERROR = 1.

*---> 轉換單位
      TRANSLATE T_KGMPC-WSMEI TO UPPER CASE.
      TRANSLATE T_KGMPC-XFHDW TO UPPER CASE.

      CALL FUNCTION 'CONVERSION_EXIT_RUNIT_INPUT'
        EXPORTING
          INPUT     = T_KGMPC-WSMEI
          LANGUAGE  = SY-LANGU
        IMPORTING
          OUTPUT    = T_KGMPC-WSMEI
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      APPEND T_KGMPC.
      CLEAR  T_KGMPC.
    ENDAT.
  ENDLOOP.

  IF T_KGMPC[] IS INITIAL.
    MESSAGE S001(00) WITH '檔案內無資料' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_kgmpc_data
*&---------------------------------------------------------------------*
FORM PROCESS_KGMPC_DATA .

  CLEAR:G_SUCCESS,G_FAIL,G_TOTAL.

  LOOP AT T_KGMPC.
    G_TOTAL = G_TOTAL + 1.
    MOVE-CORRESPONDING T_KGMPC TO T_KGMPC_LOG.

    PERFORM CALL_BAPI_KGMPC USING T_KGMPC-MATNR T_KGMPC-ATNAM
                                  T_KGMPC-WSMEI T_KGMPC-ATWRT
                                  T_KGMPC-XFHDW
                         CHANGING T_KGMPC_LOG-FLAG
                                  T_KGMPC_LOG-ID
                                  T_KGMPC_LOG-NUMBER
                                  T_KGMPC_LOG-MESSAGE.

    APPEND T_KGMPC_LOG.   CLEAR T_KGMPC_LOG.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_BAPI_KGMPC
*&---------------------------------------------------------------------*
FORM CALL_BAPI_KGMPC USING IN_MATNR IN_ATNAM
                           IN_WSMEI IN_ATWRT
                           IN_XFHDW
                  CHANGING OUT_FLAG OUT_ID OUT_NUMBER OUT_MESSAGE.

  DATA:LT_MEINH_WS_UPD  LIKE SMEINH_WSUPD  OCCURS 0 WITH HEADER LINE.
  DATA:LT_MEINH_WS_UPDX LIKE SMEINH_WSUPDX OCCURS 0 WITH HEADER LINE.
  DATA:LT_RETURN        LIKE BAPIRETURN1   OCCURS 0 WITH HEADER LINE.
  DATA:L_ERROR TYPE C.

  LT_MEINH_WS_UPD-WSMEI = IN_WSMEI.
  LT_MEINH_WS_UPD-ATNAM = IN_ATNAM.
  LT_MEINH_WS_UPD-ATWRT = IN_ATWRT.
  LT_MEINH_WS_UPD-XFHDW = IN_XFHDW.
  APPEND LT_MEINH_WS_UPD.

  LT_MEINH_WS_UPDX-WSMEI = IN_WSMEI.
  LT_MEINH_WS_UPDX-ATNAM = 'X'.
  LT_MEINH_WS_UPDX-ATWRT = 'X'.
  LT_MEINH_WS_UPDX-XFHDW = 'X'.

  APPEND LT_MEINH_WS_UPDX.

  CALL FUNCTION 'VBWS_UOM_MAINTAIN_DARK'
    EXPORTING
      I_MATNR               = IN_MATNR
      I_KZWSM               = 'B'
      I_KZWSMX              = 'X'
*     I_TYPE_OF_BLOCK       = 'E'
      I_EXIT_BY_FIRST_ERROR = 'X'
*     I_LIST_ERRORS_ONLY    = ' '
      I_USER                = SY-UNAME
      I_BUFFER_REFRESH      = 'X'
*     I_UPDATE_BUFFER_ONLY  = ' '
      I_NO_UPDATE           = ' '
*     I_RFC_SENDER          =
*     I_CALLING_METHOD      =
*   IMPORTING
*     E_KZWSM               =
*     E_KZWSM_OLD           =
    TABLES
      I_MEINH_WS_UPD        = LT_MEINH_WS_UPD
      I_MEINH_WS_UPDX       = LT_MEINH_WS_UPDX
*     I_MEINH_WS_SFN        =
*     I_MEINH_WS_SFNX       =
*     E_MEINH_WS            =
*     E_MEINH               =
*     E_MEINH_OLD           =
*     E_MESSAGE             =
      E_RETURN              = LT_RETURN
    EXCEPTIONS
      ERROR                 = 1
      OTHERS                = 2.

  CLEAR:L_ERROR.

  LOOP AT LT_RETURN WHERE TYPE  = 'E' .
    L_ERROR = 'X'.
    CONTINUE.
  ENDLOOP.

  IF L_ERROR IS INITIAL.
    OUT_FLAG = 'S'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
    G_SUCCESS = G_SUCCESS + 1.
    CONCATENATE '料號:' IN_MATNR
    '更新成功' INTO OUT_MESSAGE.
    CALL FUNCTION 'DEQUEUE_ALL'
      EXPORTING
        _SYNCHRON = 'X'.
  ELSE.
    OUT_FLAG = 'F'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    G_FAIL = G_FAIL + 1.
    OUT_ID = LT_RETURN-ID.
    OUT_NUMBER = LT_RETURN-NUMBER.
    CONCATENATE '料號:' IN_MATNR
    '更新失敗-' LT_RETURN-ID LT_RETURN-NUMBER LT_RETURN-MESSAGE
    INTO OUT_MESSAGE.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LONGTEXT_2
*&---------------------------------------------------------------------*
FORM GET_LONGTEXT_2 .

*--內部備註
  GT_MATERIALLONGTEXT-APPLOBJECT = 'MATERIAL'.
  GT_MATERIALLONGTEXT-TEXT_NAME  = T_BASIC-MATNR.
  GT_MATERIALLONGTEXT-TEXT_ID    = 'IVER'.

*  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
*    EXPORTING
*      input            = t_lont-langu
*    IMPORTING
*      output           = gt_materiallongtext-langu
*    EXCEPTIONS
*      unknown_language = 1
*      OTHERS           = 2.

  GT_MATERIALLONGTEXT-LANGU = SY-LANGU.
  GT_MATERIALLONGTEXT-TEXT_LINE = T_BASIC-LONGTEXT3.

  APPEND GT_MATERIALLONGTEXT. CLEAR GT_MATERIALLONGTEXT.

ENDFORM.

************************************************************************
* Program Name      : ZPPMIG004
* Descriptions      : 客製Table 資料上傳
* Updates Tables    :
* Input  Parameters :
* Output Parameters :
* Return Codes      :
* Special Logic     :
* Includes          :
************************************************************************
* Modification Log
************************************************************************
*   Date    Ver. Programmer   Descriptions
*---------- ---- ------------ ------------------------------------------
*2023/10/4      Derek        New Create
*
************************************************************************
REPORT ZPPMIG004 NO STANDARD PAGE HEADING
                MESSAGE-ID 00
                LINE-SIZE  500
                LINE-COUNT 65.

TYPE-POOLS : ABAP.
FIELD-SYMBOLS: <DYN_TABLE> TYPE STANDARD TABLE,
               <DYN_WA>,
               <DYN_FIELD>.
DATA: DY_TABLE TYPE REF TO DATA,
      DY_LINE  TYPE REF TO DATA,
      XFC      TYPE LVC_S_FCAT,
      IFC      TYPE LVC_T_FCAT.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
  PARAMETERS: P_TABLE TYPE DD02L-TABNAME OBLIGATORY,
              P_FNAME LIKE RLGRAP-FILENAME DEFAULT 'c:\temp\tab.xls'
                        OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FNAME.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      FILE_NAME = P_FNAME.

START-OF-SELECTION.
  PERFORM GET_STRUCTURE.
  PERFORM CREATE_DYNAMIC_ITAB.
***********Creates a dyanamic internal table**********
  PERFORM GET_DATA.
  PERFORM WRITE_OUT.
*&---------------------------------------------------------------------*
*&      Form  get_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_STRUCTURE.

  DATA : IDETAILS TYPE ABAP_COMPDESCR_TAB,
         XDETAILS TYPE ABAP_COMPDESCR.
  DATA : REF_TABLE_DES TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA : EF_IS_UNICODE LIKE  /BDL/TASKS-STATUS.
* Check the system is UNICODE or NONUNICODE
  CALL FUNCTION '/BDL/CHECK_UNICODE'
    IMPORTING
      EF_IS_UNICODE = EF_IS_UNICODE.
* Get the structure of the table.
  REF_TABLE_DES ?=
      CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( P_TABLE ).
  IDETAILS[] = REF_TABLE_DES->COMPONENTS[].
  LOOP AT IDETAILS INTO XDETAILS.
    CLEAR XFC.
    XFC-TABNAME = P_TABLE.
    XFC-FIELDNAME = XDETAILS-NAME .
    XFC-DATATYPE = XDETAILS-TYPE_KIND.
    XFC-INTTYPE = XDETAILS-TYPE_KIND.
    CASE XFC-DATATYPE.
      WHEN 'C' OR 'D' OR 'N'.
        IF EF_IS_UNICODE EQ 'X'.
          XFC-INTLEN = XDETAILS-LENGTH / 2. " Unicdoe
        ELSE.
          XFC-INTLEN = XDETAILS-LENGTH.     " Nonunicode
        ENDIF.
      WHEN OTHERS.                          " For Type P
        XFC-INTLEN = XDETAILS-LENGTH.
    ENDCASE.
    XFC-DECIMALS = XDETAILS-DECIMALS.
    APPEND XFC TO IFC.
  ENDLOOP.
ENDFORM.                    "get_structure
*&---------------------------------------------------------------------*
*&      Form  create_dynamic_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_DYNAMIC_ITAB.
* Create dynamic internal table and assign to FS
  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      I_LENGTH_IN_BYTE = 'X'
      IT_FIELDCATALOG  = IFC
    IMPORTING
      EP_TABLE         = DY_TABLE.
  ASSIGN DY_TABLE->* TO <DYN_TABLE>.
* Create dynamic work area and assign to FS
  CREATE DATA DY_LINE LIKE LINE OF <DYN_TABLE>.
  ASSIGN DY_LINE->* TO <DYN_WA>.
ENDFORM.                    "create_dynamic_itab

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA : FILENAME LIKE RLGRAP-FILENAME.

  FILENAME = P_FNAME.

*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      CODEPAGE                = '4103'
*      FILENAME                = FILENAME
*      FILETYPE                = 'ASC'
*      HAS_FIELD_SEPARATOR     = 'X'
*    TABLES
*      DATA_TAB                = <DYN_TABLE>
*    EXCEPTIONS
*      FILE_OPEN_ERROR         = 1
*      FILE_READ_ERROR         = 2
*      NO_BATCH                = 3
*      GUI_REFUSE_FILETRANSFER = 4
*      INVALID_TYPE            = 5
*      NO_AUTHORITY            = 6
*      UNKNOWN_ERROR           = 7
*      BAD_DATA_FORMAT         = 8
*      HEADER_NOT_ALLOWED      = 9
*      SEPARATOR_NOT_ALLOWED   = 10
*      HEADER_TOO_LONG         = 11
*      UNKNOWN_DP_ERROR        = 12
*      ACCESS_DENIED           = 13
*      DP_OUT_OF_MEMORY        = 14
*      DISK_FULL               = 15
*      DP_TIMEOUT              = 16
*      OTHERS                  = 17.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

*--ALSMEX_TABLINE上傳有限制最長只能到50而已

*  DATA: LT_UPLOAD  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
*--因為有超過長度50所以複製一個一樣的TABLE
  DATA: LT_UPLOAD  LIKE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

  DATA: L_INDEX TYPE I,
        L_NUM   TYPE I.

* ALSM_EXCEL_TO_INTERNAL_TABLE限制50所以複製個一樣的才行，詳細參考長度限制檔案(EXCEL上傳)
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      FILENAME                = FILENAME
      I_BEGIN_COL             = 1
      I_BEGIN_ROW             = 2
      I_END_COL               = 256
      I_END_ROW               = 65536
    TABLES
      INTERN                  = LT_UPLOAD
    EXCEPTIONS
      INCONSISTENT_PARAMETERS = 1
      UPLOAD_OLE              = 2
      OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE S001 WITH '上傳錯誤' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*  DATA:WA_STRUCTURE TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA:WA_STRUCTURE TYPE LVC_S_FCAT.
  DATA:L_NEW_LINE   TYPE REF TO DATA.
  CREATE DATA L_NEW_LINE LIKE LINE OF <DYN_TABLE>.
  ASSIGN L_NEW_LINE->* TO <DYN_WA> .

  CLEAR:L_NUM.
  LOOP AT LT_UPLOAD .
    L_NUM = LT_UPLOAD-COL + 1.
*--第一筆是MANDT所以都從第二筆開始
    READ TABLE IFC  INTO WA_STRUCTURE INDEX L_NUM.


    ASSIGN COMPONENT WA_STRUCTURE-FIELDNAME OF STRUCTURE
    <DYN_WA>  TO <DYN_FIELD>.
    CONDENSE LT_UPLOAD-VALUE.
    <DYN_FIELD> = LT_UPLOAD-VALUE .

    AT END OF ROW.
      APPEND <DYN_WA> TO <DYN_TABLE>.
      CLEAR:<DYN_WA>.

    ENDAT.

  ENDLOOP.

ENDFORM.                    "get_data

**Write out data from table.
FORM WRITE_OUT.
  DATA:L_SUCE TYPE I,
       L_STR  TYPE C LENGTH 20,
       L_FAIL TYPE I,
       L_STR1 TYPE C LENGTH 20.

  DATA:BEGIN OF LT_STR OCCURS 0,
         STR TYPE C LENGTH 800,
         NUM TYPE C,
       END OF LT_STR.

  CLEAR:L_SUCE,L_FAIL,LT_STR[],LT_STR,L_STR,L_STR1.

  LOOP AT <DYN_TABLE> INTO <DYN_WA>.
    DO.
      ASSIGN COMPONENT  SY-INDEX
         OF STRUCTURE <DYN_WA> TO <DYN_FIELD>.
      IF SY-SUBRC <> 0.

        EXIT.
      ENDIF.
      IF SY-INDEX = 1.
      ELSE.

      ENDIF.
    ENDDO.

    INSERT (P_TABLE) FROM <DYN_WA>.
    IF SY-SUBRC <> 0.
      L_FAIL = L_FAIL + 1.

*      LT_STR-STR = <DYN_WA>.
*      APPEND LT_STR.
*      CLEAR: LT_STR.

    ELSE.
      L_SUCE = L_SUCE + 1.

    ENDIF.

    AT LAST.
      L_STR  = L_SUCE.
      L_STR1 = L_FAIL.

      CONCATENATE '成功' L_STR  '筆,失敗' L_STR1  '筆'
      INTO LT_STR-STR.
      LT_STR-NUM = 'X'.
      APPEND LT_STR.
      CLEAR: LT_STR.
*      WRITE:/ '成功',L_SUCE ,'筆,失敗',L_FAIL, '筆'.
    ENDAT.

  ENDLOOP.

  SORT LT_STR BY NUM DESCENDING.

  LOOP AT LT_STR.

    WRITE:LT_STR,/.

  ENDLOOP.

*  insert (p_table) from table <dyn_table>.
ENDFORM.                    "write_out

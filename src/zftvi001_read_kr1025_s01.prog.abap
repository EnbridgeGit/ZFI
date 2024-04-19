*----------------------------------------------------------------------*
*   INCLUDE ZPMO_CCC010_S01                                            *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK A1 WITH FRAME TITLE text-s01.
  PARAMETERS: firmennr LIKE ccheader-compcode OBLIGATORY.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS: testlauf LIKE rprxxxxx-kr_feld4 DEFAULT 'X' USER-COMMAND
TEST.
*  PARAMETERS: D_Import LIKE RPRXXXXX-KR_Feld4 DEFAULT 'X'.
* Begin of Syntax-1.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: SSN_Proc as checkbox.
    SELECTION-SCREEN COMMENT 5(79) TEXT-S06
                     FOR FIELD SSN_Proc.
  SELECTION-SCREEN END OF LINE.
* Replaces:
*  PARAMETERS: SSN_Proc as checkbox.                 " SSN_Nb
* End of Syntax-1.
* Begin of Locl_Curr
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: Lcl_Conv AS CHECKBOX.
    SELECTION-SCREEN COMMENT 5(79) TEXT-S05
                   FOR FIELD Lcl_Conv.
  SELECTION-SCREEN END OF LINE.
* End of Locl_Curr
  SELECTION-SCREEN SKIP 1.                          " SSN_Nb
  PARAMETERS: Run_Nb LIKE CCHeader-LfdNum.
SELECTION-SCREEN END OF BLOCK A1.

SELECTION-SCREEN BEGIN OF BLOCK A2 WITH FRAME TITLE text-s02.
  PARAMETERS: FILE_INL LIKE rprxxxxx-kr_feld4 DEFAULT ' '.
  SELECTION-SCREEN SKIP 1.
* Begin of InOut.
  PARAMETERS: FILE_NIN LIKE rlgrap-filename.
* Replaces
*  PARAMETERS: FILE_NIN LIKE rprxxxxx-seq_in.
* End of InOut
SELECTION-SCREEN END OF BLOCK A2.

SELECTION-SCREEN BEGIN OF BLOCK A3 WITH FRAME TITLE TEXT-S03.
* Output data for successfully converted entries.
  PARAMETERS: FIL_ONL LIKE RPRXXXXX-KR_Feld4 DEFAULT ' '.
* Begin of InOut.
  PARAMETERS: FIL_NOUT LIKE rlgrap-filename.
* Replaces
*  PARAMETERS: FIL_NOUT LIKE rprxxxxx-seq_in.
* End of InOut.
  SELECTION-SCREEN SKIP 1.
* Output data for incorrect entries.
  PARAMETERS: FIL_FNL LIKE RPRXXXXX-KR_Feld4 DEFAULT ' '.
* Begin of InOut
  PARAMETERS: FIL_NF LIKE rlgrap-filename.
* Replaces
*  PARAMETERS: FIL_NF LIKE rprxxxxx-seq_in.
* End of InOut
SELECTION-SCREEN END OF BLOCK A3.

SELECTION-SCREEN BEGIN OF BLOCK A4 WITH FRAME TITLE TEXT-S04.
  PARAMETERS: P_Prot  LIKE RPRXXXXX-KR_Feld5 DEFAULT 'X'.
  PARAMETERS: RB_Fehl RADIOBUTTON GROUP RB1 DEFAULT 'X',
              RB_All  RADIOBUTTON GROUP RB1.
SELECTION-SCREEN END OF BLOCK A4.

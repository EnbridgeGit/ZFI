*&----------------------------------------------------------------------*
*& Report  ZFAPR001_WITHHOLD_TAX_UPD_SHOW
*&
*&----------------------------------------------------------------------*
*&----------------------------------------------------------------------*
*& Author             :  Amit Singh                                     *
*& Creation Date      :  06-Feb-2013                                    *
*& Application Area   :  FI-GL/FI-AP/FI-AR                              *
*& Transport Request  :  DECK908690                                     *
*& Description        :  Report to Update with holding tax in documents *
*&----------------------------------------------------------------------*

REPORT  ZFAPR001_WITHHOLD_TAX_UPD_SHOW.

* Top Include
INCLUDE ZFAPR001_WITHHOLD_TAX_UPD_TOP.

* Form definition
INCLUDE ZFAPR001_WITHHOLD_TAX_UPD_F01.

* PBO definition
INCLUDE ZFAPR001_WITHHOLD_TAX_UPD_O01.

* PAI definition
INCLUDE ZFAPR001_WITHHOLD_TAX_UPD_I01.


INITIALIZATION.

  GV_FUNCTXT-TEXT = 'Information Button'(067).
  GV_FUNCTXT-ICON_ID   = '@0S@'(068).             "'ICON_INFORMATION'.
  SSCRFIELDS-FUNCTXT_01 = GV_FUNCTXT.

AT SELECTION-SCREEN OUTPUT.

  IF SY-UCOMM NE 'ONLI'(069).
    CLEAR : GV_TITLE.

    IF PRB_BSK = 'X'.

      PERFORM BSK_INITIAL_VALUE.

    ELSEIF PRB_R10 = 'X'.

      PERFORM R10_INITIAL_VALUE.

    ELSEIF PRB_R20 = 'X'.

      PERFORM R20_INITIAL_VALUE.

    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.

  IF SY-UCOMM = 'FC01'(070).
    CALL SCREEN 100 STARTING AT 10 40.
  ENDIF.

START-OF-SELECTION.

* Checking Mandatory fields.
  PERFORM SELECTION_CHECK.

* Final call to display output
  PERFORM SUBMIT_AND_DISPLAY.

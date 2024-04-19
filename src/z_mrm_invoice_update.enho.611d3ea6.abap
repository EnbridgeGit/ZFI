"Name: \PR:SAPLFDCB\FO:CHECK_DOCUMENT_TYPE\SE:BEGIN\EI
ENHANCEMENT 0 Z_MRM_INVOICE_UPDATE.
*
IF sy-batch is INITIAL AND
   ( invfo-blart <> 'ZR' AND
     invfo-blart <> 'ZN' ).
  break sahmad.
  CONSTANTS: lc_msgno TYPE sy-msgno VALUE '021',
             lc_msgv2 TYPE sy-msgv2 VALUE 'ARIBA MESSAGE',
             lc_msgv3 TYPE sy-msgv3 VALUE 'SERVICE_PO',
             c_errprot(23)   TYPE c VALUE '(SAPLMRMF)TAB_ERRPROT[]'.
  DATA     : gt_errtab TYPE TABLE OF mrm_errprot,
             gs_errtab TYPE mrm_errprot.
  data     : lv_Ebeln type ekko-ebeln,
             lv_answer.

  data: lv_ariba_po type  c LENGTH 10,
        lv_memid type c LENGTH 17.

  CONCATENATE sy-uname 'ARIBA' INTO lv_memid.

IF sy-tcode <> 'MIR4' AND
   sy-tcode <> 'MRRL' AND
   sy-tcode <> 'MR8M'.

   import lv_ariba_po to lv_ariba_po FROM MEMORY id  lv_memid.
   IF lv_ariba_po is NOT INITIAL.
      IF invfo-blart <> 'RF' OR
         bkpf-blart  <> 'RF'.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
           TITLEBAR                    = 'NOTE: DOCUMENT TYPE RF MUST BE USED FOR ARIBA PO.'
*           DIAGNOSE_OBJECT             = ' '
            text_question               =
'DO YOU WANT TO SWITCH OVER TO DOCUMENT TYPE RF? NOTE: DOCUMENT TYPE RF MUST BE USED FOR ARIBA PO. IT WILL VALIDATE UPON SAVING'
           TEXT_BUTTON_1               = 'Yes'
*           ICON_BUTTON_1               = ' '
           TEXT_BUTTON_2               = 'No'
*           ICON_BUTTON_2               = ' '
*           DEFAULT_BUTTON              = '1'
           DISPLAY_CANCEL_BUTTON       = ' '
*           USERDEFINED_F1_HELP         = ' '
*           START_COLUMN                = 25
*           START_ROW                   = 6
*           POPUP_TYPE                  =
*           IV_QUICKINFO_BUTTON_1       = ' '
*           IV_QUICKINFO_BUTTON_2       = ' '
         IMPORTING
           ANSWER                      = lv_answer
*         TABLES
*           PARAMETER                   =
         EXCEPTIONS
           TEXT_NOT_FOUND              = 1
           OTHERS                      = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        IF lv_answer = '1'.
            bkpf-blart = 'RF'.
            invfo-blart = 'RF'.
        ELSE.
            FREE MEMORY ID lv_memid.
        ENDIF.
     ENDIF.
  ELSE.
*If Not Ariba Invoice then switch over to RE
     IF invfo-blart = 'RF' or bkpf-blart = 'RF'.
        bkpf-blart = 'RE'.
        invfo-blart = 'RE'.
     ENDIF.
  ENDIF.
ENDIF.
ENDIF.
ENDENHANCEMENT.

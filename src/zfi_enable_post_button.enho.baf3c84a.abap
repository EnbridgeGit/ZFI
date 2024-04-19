"Name: \FU:OPEN_FI_PERFORM_00001140_E\SE:END\EI
ENHANCEMENT 0 ZFI_ENABLE_POST_BUTTON.
*** Check for the Transaction Code;
  if sy-tcode <> 'FV53'.
    if sy-tcode <> 'FV63'.
    delete table t_exctab with table key okcod = 'BU'.
    endif.
  endif.
ENDENHANCEMENT.

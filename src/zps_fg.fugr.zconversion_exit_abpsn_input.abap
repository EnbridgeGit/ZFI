FUNCTION ZCONVERSION_EXIT_ABPSN_INPUT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  CHAR24
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  CHAR24
*"----------------------------------------------------------------------
  if flg_exists_abpsn = '?'.
    call function 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'CONVERSION_EXIT_PROJN_INPUT'
      EXCEPTIONS
        function_not_exist = 1.

    if sy-subrc <> 0.
      clear flg_exists_abpsn.
    else.
      flg_exists_abpsn = 'X'.
    endif.
  endif.
  if flg_exists_abpsn is initial.
*   The R/3 application PS is not installed: why do we access this point
*   at all?
    output = input.
    exit.
  endif.

  call function 'CONVERSION_EXIT_PROJN_INPUT'               "#EC EXISTS
       exporting
            input  = input
       importing
            output = output.

endfunction.

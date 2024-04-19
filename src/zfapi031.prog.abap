REPORT ZFAPI031 LINE-SIZE 132 LINE-COUNT 65.
* This program will read use the entered transaction to start a
* core transaction to use a transactional variant.


data:   tcode  like shdfv-tcode     value 'ZFB02PMNT',
        tcvari like shdfv-tcvariant value 'ZPMNTBLK'.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------

call function 'RS_HDSYS_CALL_TC_VARIANT'
    exporting
         tcode                     = tcode       "ZFB02PMNT
         variant                   = tcvari      "ZPMNTBLK
         l_flag_client_independent = ' '
         call_mode                 = 'X'
         variant_check             = ' '
         authority_check           = ' '
    exceptions
         no_variant                = 1
         no_authority_check        = 2.



    if sy-subrc <> ' 0'.
       write: 'sy-subrc = ', sy-subrc.
    endif.

exit.
end-of-selection.

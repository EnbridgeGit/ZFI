REPORT ZFAPI032 LINE-SIZE 132 LINE-COUNT 65.
* This program will read use the entered transaction to start a
* core transaction to use a transactional variant.

*parameters:
*    tcode like shdfv-tcode,
*   vtcode like shdfv-tcode,
*    tcvari like shdfv-tcvariant.
data:   tcode  like shdfv-tcode     value 'ZFBL1NPMNT',
        tcvari like shdfv-tcvariant value 'ZPMNTBLK3'.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------

call function 'RS_HDSYS_CALL_TC_VARIANT'
    exporting
         tcode                     = tcode       "ZFB02PMNT
         vtcode                    = ' '
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

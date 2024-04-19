"Name: \FU:HELP_START\SE:END\EI
ENHANCEMENT 0 ZMENU_ENHANCEMENT.
*
  case help_infos-call.
    when 'H'.
      case help_infos-menufunct.
        when 'ZHSK'.
          call function 'CALL_BROWSER'
             exporting
               url            = 'http://sidekick/sk/sidekick.htm'
               window_name    = 'Sidekick'
             exceptions
               frontend_not_supported = 1
               frontend_error         = 2
               prog_not_found         = 3
               no_batch               = 4
               unspecified_error      = 5
               others                 = 6.
          if sy-subrc <> 0.
            exit.
          endif.                       " neu 4.0B-
      ENDCASE.
  ENDCASE.
ENDENHANCEMENT.

FUNCTION-POOL ZFI_FILE.                     "MESSAGE-ID ..

* INCLUDE LZFI_FILED...                      " Local class definition

CONSTANTS:
        gc_x(1)              TYPE c VALUE 'X',
        gc_e(1)              TYPE c VALUE 'E',
        gc_c(1)              TYPE c VALUE 'C',
        gc_m(1)              TYPE c VALUE 'M',
        gc_r(1)              TYPE c VALUE 'R',
        gc_sep(1)            TYPE c VALUE '_',
        gc_arc(4)            TYPE c VALUE '.ARC',
        gc_new(4)            TYPE c VALUE '.NEW',
        gc_copy_command      TYPE sxpgcolist-name VALUE 'ZFILE_COPY',
        gc_move_command      TYPE sxpgcolist-name VALUE 'ZFILE_MOVE',
        gc_ren_command       TYPE sxpgcolist-name VALUE 'ZFILE_REN'.

DATA:   gv_subrc             TYPE sysubrc,
        gv_source_file       TYPE sxpgcolist-parameters,
        gv_target_file       TYPE sxpgcolist-parameters,
        gv_file              TYPE sxpgcolist-parameters,
        gv_command           TYPE sxpgcolist-name,
        gv_status            TYPE extcmdexex-status,
        gv_exitcode          TYPE extcmdexex-exitcode.

DATA:   gwa_exec_protocol    TYPE btcxpm,
        git_exec_protocol    TYPE TABLE OF btcxpm.

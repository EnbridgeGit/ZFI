FUNCTION zsample_process_00001110.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BUKRS) LIKE  BKPF-BUKRS
*"     VALUE(I_LIFNR) LIKE  BSEG-LIFNR
*"     VALUE(I_WAERS) LIKE  BKPF-WAERS
*"     VALUE(I_BLDAT) LIKE  BKPF-BLDAT
*"     VALUE(I_XBLNR) LIKE  BKPF-XBLNR
*"     VALUE(I_WRBTR) LIKE  BSEG-WRBTR OPTIONAL
*"     VALUE(I_BELNR) LIKE  BSEG-BELNR OPTIONAL
*"     VALUE(I_GJAHR) LIKE  BSEG-GJAHR OPTIONAL
*"     VALUE(I_BUZEI) LIKE  BSEG-BUZEI OPTIONAL
*"     VALUE(I_SHKZG) LIKE  BSEG-SHKZG
*"     VALUE(I_BLART) TYPE  BLART OPTIONAL
*"  EXPORTING
*"     VALUE(E_NOSTD) LIKE  BOOLE-BOOLE
*"----------------------------------------------------------------------
*** BTBOUNDY copy from US July/2013


*----------------------------------------------------------------------*
* Internal Tables                                                      *
*----------------------------------------------------------------------*

  DATA: git_bkpf      TYPE STANDARD TABLE OF bkpf,
        git_bseg      TYPE STANDARD TABLE OF bseg,
        git_vbsegk    TYPE STANDARD TABLE OF vbsegk,

        gv_dup_chkmsg TYPE string,
        gv_wrbtr(15)  TYPE c,
        gv1_waers     TYPE  bkpf-waers,
        gv1_wrbtr     TYPE  bseg-wrbtr,

        gvstr_xblnr   TYPE  string,
        gvstr_wrbtr   TYPE  string.


  CONSTANTS:  co_3(1)   TYPE c VALUE '3',
              co_bu(2)  TYPE c VALUE 'BU',
              co_h(1)   TYPE c VALUE 'H',
              co_s(1)   TYPE c VALUE 'S'.

  REFRESH:  git_bkpf,
            git_bseg.

  CLEAR:    git_bseg.


**warning not occur at while posting
  IF sy-ucomm <> 'BU' and sy-batch = ''.

    IF i_xblnr IS NOT INITIAL.

      SELECT * FROM  bkpf
               INTO TABLE git_bkpf
               WHERE xblnr = i_xblnr
                 AND belnr <> i_belnr..

      IF git_bkpf[] IS NOT INITIAL.

        SELECT * FROM bseg
                  INTO TABLE git_bseg
                  FOR ALL ENTRIES IN git_bkpf
                  WHERE bukrs = git_bkpf-bukrs
                  AND belnr = git_bkpf-belnr
                  AND gjahr = git_bkpf-gjahr
                  AND wrbtr = i_wrbtr.

        IF git_bseg[] IS NOT INITIAL.

          CLEAR:  gv_dup_chkmsg,
                  gvstr_wrbtr,
                  gvstr_xblnr.

          MOVE i_wrbtr TO gv_wrbtr.
          CONDENSE gv_wrbtr.
          CONCATENATE text-005 gv_wrbtr gv1_waers
          INTO gvstr_wrbtr
          SEPARATED BY space.

          CONCATENATE text-004 i_xblnr
          INTO gvstr_xblnr
          SEPARATED BY space.

          CONCATENATE text-001
          gvstr_xblnr
          gvstr_wrbtr
          INTO gv_dup_chkmsg SEPARATED BY space.

          MESSAGE gv_dup_chkmsg TYPE 'W' DISPLAY LIKE 'I'.

        ENDIF.


        " Check for Parked Documents "

        SELECT * FROM vbsegk
        INTO TABLE git_vbsegk
        FOR ALL ENTRIES IN git_bkpf
        WHERE bukrs = git_bkpf-bukrs
        AND belnr = git_bkpf-belnr
        AND gjahr = git_bkpf-gjahr
        AND wrbtr = i_wrbtr.

        IF git_vbsegk[] IS NOT INITIAL.

          DELETE git_vbsegk WHERE bukrs = i_bukrs
                            AND   belnr = i_belnr
                            AND   gjahr = i_gjahr
                            AND   wrbtr = i_wrbtr.

          IF git_vbsegk[] IS NOT INITIAL.

            CLEAR:  gv_dup_chkmsg,
                    gvstr_wrbtr,
                    gvstr_xblnr.

            MOVE i_wrbtr TO gv_wrbtr.
            CONDENSE gv_wrbtr.
            CONCATENATE text-005 gv_wrbtr gv1_waers
            INTO gvstr_wrbtr
            SEPARATED BY space.

            CONCATENATE text-004 i_xblnr
            INTO gvstr_xblnr
            SEPARATED BY space.

            CONCATENATE text-001
            gvstr_xblnr
            gvstr_wrbtr
            INTO gv_dup_chkmsg SEPARATED BY space.

            MESSAGE gv_dup_chkmsg TYPE 'W' DISPLAY LIKE 'I'.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

  e_nostd = 'X'.
ENDFUNCTION.

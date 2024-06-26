REPORT ZRSABAPPROGRAM .

data: STARTUP_MANAGER  TYPE REF TO CL_WB_STARTUP,
      L_WB_REQUEST     TYPE REF TO CL_WB_REQUEST,
      L_WB_REQUEST_SET TYPE REF TO CL_WB_REQUEST OCCURS 0.

    AUTHORITY-CHECK OBJECT 'S_TCODE'
               ID 'TCD' FIELD 'ZSE38'.
    IF SY-SUBRC NE 0.
      message e077(s#) with 'ZSE38'.
    ENDIF.

*   Request für Einstiegsbild SE38 erzeugen
    CALL METHOD CL_WB_PGEDITOR_INITIAL_SCREEN=>CREATE_REQUEST
      IMPORTING
        WB_REQUEST  = L_WB_REQUEST.
*   Startup Manager erzeugen
    CREATE OBJECT STARTUP_MANAGER.
*   Request(menge) an Manager geben
    APPEND L_WB_REQUEST TO L_WB_REQUEST_SET.
    CALL METHOD STARTUP_MANAGER->START
      EXPORTING
        P_WB_REQUEST_SET = L_WB_REQUEST_SET
      EXCEPTIONS
        OTHERS = 1.

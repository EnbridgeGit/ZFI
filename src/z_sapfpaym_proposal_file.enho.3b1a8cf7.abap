"Name: \PR:SAPFPAYM\IC:FPAYM_SEL\SE:END\EI
ENHANCEMENT 0 Z_SAPFPAYM_PROPOSAL_FILE.
**Comment out because Selection event collided with EhP5 upgrade
**This is control through F110 transaction

*Stop down load file if proposal run is ON.
*  at selection-screen on PAR_FILE.
*  if PM_XVORL = 'X' and
*     PAR_FILE is not initial.
*     LOOP AT SCREEN.
*          IF SCREEN-NAME = 'PM_XVORL'.
*          SCREEN-INPUT = '1'.
*          MODIFY SCREEN.
*          ENDIF.
*     ENDLOOP.
*     MESSAGE e000(zfi01) WITH
*           'At Proposal run filename field should be empty' .
**   & & & & &
*
*  endif.

ENDENHANCEMENT.

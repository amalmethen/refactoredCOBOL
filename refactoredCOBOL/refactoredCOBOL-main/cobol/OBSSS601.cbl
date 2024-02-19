       CBL TEST(NONE,SYM,SEPARATE)
       ID DIVISION.

      *****************************************************************
      *                                                               *
      *             SOCIAL  SECURITY  SERVICES  SYSTEM                *
      *                                                               *
      *          FILE MAINTENANCE SERVICE ROUTINE OBSSS601            *
      *                                                               *
      *****************************************************************

       PROGRAM-ID.    OBSSS601.
       AUTHOR.        PEXER JEFF.
           INSTALLATION.  PUBLIC INSTITUTION FOR SOCIAL SECURITY.
           DATE-WRITTEN.  30/05/79.
           DATE-COMPILED.

           REMARKS.

      *****************************************************************
      *                     *** OBSSS601 ***                          *
      *                                                               *
      *    FUNCTION -  THIS PROGRAM WILL CREATE THE ACCOUNTING        *
      *                TRANSACTIONS FROM THE FILE MAINTENANCE         *
      *                PEXS THAT CALL IT. THE ACCTG TRANS ARE         *
      *                CREATED FROM THE PEX-ACCTG-TABLE IN THE        *
      *                COMMUNICATIONS-REGION. THE TRANS RECORDS       *
      *                ARE SENT TO THE OUTPUT ANALYSIS PROGRAM WHERE  *
      *                THEY ARE REFORMATTED INTO G/L AND ACCTG        *
      *                REGISTER TRANSACTIONS. PEX ACCOUNTING TABLE IS *
      *                INTERROGATED FOR PREVIOUS YEAR OR FUTURE YEAR  *
      *                ACCOUNTING DEPENDING UPON EFFECTIVE DATE OF    *
      *                ACCOUNTING. FILE MAINTENANCE ACCOUNTING TABLE  *
      *                ENTRIES ARE UPDATED DEPENDING UPON THE         *
      *                ACCOUNTING INFORMATION IN THE PEX ACCOUNTING   *
      *                TABLE.                                         *
      *                                                               *
      *    INPUT PARMS -   CALLED PROGRAM NAME                        *
      *                    CALL WORK AREA                             *
      *                    RETURN CODE                                *
      *                    MESSAGE AREA                               *
      *                    TRANSACTION-LEAD                           *
      *                    C-ACCOUNTING-INFO                          *
      *                    C-SYSTEM-CONSTANTS                         *
      *                                                               *
      *    INPUT - NONE                                               *
      *                                                               *
      *****************************************************************
           EJECT
      *****************************************************************
      *                                                               *
      *    OUTPUT - SUCCESSFUL COMPLETION                             *
      *               ACCOUNTING TRANSACTION                          *
      *               UPDATED FILE MAINTENANCE ACCOUNTING TABLE       *
      *               RETURN CODE                                     *
      *           - UNSUCCESSFUL COMPLETION                           *
      *               ERROR CODE                                      *
      *               RETURN CODE                                     *
      *                                                               *
      *    TABLES - NONE                                              *
      *                                                               *
      *    SWITCHES - NONE                                            *
      *                                                               *
      *    EXITS -                                                    *
      *                                                               *
      *            NORMAL - CALL SFSSS210 TO WRITE G/L EXTRACT        *
      *                   - RETURN TO INVOKING PEX                    *
      *                                                               *
      *            ABNORMAL - RETURN TO INVOKING PEX WITH A RETURN    *
      *                       CODE.                                   *
      *                                                               *
      *    MODIFICATIONS -                                            *
      *****************************************************************
      * REAL TIME UPDATE PROJECT: NEW PROGRAM IS WRITTE IN ORDER TO   * REALTIME
      * ------------------------  APPLY THE REAL TIME UPDATE REQUIR-  * REALTIME
      *                           MENTS. MANAL AL-ADSANI  09/02/2003. * REALTIME
      *---------------------------------------------------------------* PRFX0703
      * PRODUCTION FIX :          TO AMEND THE PROGRAM IN ORDER TO    * PRFX0703
      * ----------------          KEEP THE ORIGINAL 7 BYTES TIME FIELD* PRFX0703
      * (PRFX0703)                AS IT IS ON THE OOD9999 TRANSACTION.* PRFX0703
      *                                  AKRAM FOUAD      14/07/2003. * PRFX0703
      *---------------------------------------------------------------* 00002700
      *      SMR 91754:-                                              * 00002710
      *      ===========                                              * 00002720
      *         AVOID USIND ACCT #  C-ACCT-TBL-COMPL-PREV-YEARS  AND  * 00002730
      *                             C-ACCT-TBL-COMPL-PREV-YEAR-OPT    * 00002740
      *         DUE TO DUPLICATE ACCOUNT NUMBER .                     * 00002770
      *                                   LUBNA AL-GHABRA 24/07/05.   * 00002780
      *---------------------------------------------------------------* SMR91779
      *  SMR#92197     AMEND THE PROGRAM TO IGNORE ACCOUNTS NOT ON    * SSR#2411
      *  -----------   FT-KK-ACCOUNT FILE.    ARWA ALFARES  AUG. 2006 * SSR#2411
      *---------------------------------------------------------------* SMR91779
S44431*  SMR#44431     CORRECT THE MOVE TO T-ACCT-CODE TO MAKE ZEROES *
S44431*  -----------   ON THE RIGHT SIDE.     AKRAM FOUAD  08/08/2008 *
      *---------------------------------------------------------------* SMR91779
S93208*  SMR#93208     UPDATE BASIC DATA TABLE M_BAS_EE_LAST_ACCT_DATE*
S93208*  -----------   COLUMN WHENEVER POSTING IS DONE THRU ONLINE.   *
S93208*                                       AKRAM FOUAD  26/10/2008 *
      *---------------------------------------------------------------* SMR91779
S93208*  SMR#95510     AMEND WRONG ACCOUNTS IN PROD FOR PREV OF MILT. *
S93208*  -----------   ACCOUNT 640545 AND PREV OF SELF EMP. 650540.   *
S93208*                                       MANAL ADSANI 27/09/2011 *
      ***************************************************************** 00002790

      ***************************************************************** 00002790
      *        E N V I R O N M E N T   D I V I S I O N                *
      *****************************************************************

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       OBJECT-COMPUTER. IBM-370.

           EJECT
      *****************************************************************
      *       D A T A   D I V I S I O N                               *
      *****************************************************************

       DATA DIVISION.
           EJECT
      *****************************************************************
      *                                                               *
      *            W O R K I N G   S T O R A G E  -  OBSSS601         *
      *                                                               *
      *****************************************************************
      *    THE WORKING-STORAGE SECTION CONTAINS THE FOLLOWING -       *
      *                                                               *
      *        -POINTERS                                              *
      *        -WORK-AREAS                                            *
      *        -ACCUMULATORS                                          *
      *        -CONSTANTS                                             *
      *        -SWITCHES                                              *
      *                                                               *
      *****************************************************************

       WORKING-STORAGE SECTION.

      ***************************************************************** DB2PROJ
      *        WORKING-STORAGE         POINTERS                       * DB2PROJ
      ***************************************************************** DB2PROJ
                                                                        DB2PROJ
       01  POINTERS.                                                    DB2PROJ
                                                                        DB2PROJ
         05  W-TRAN-PTR                      USAGE IS POINTER.          DB2PROJ
         05  W-TRAN-PTR-R   REDEFINES W-TRAN-PTR   PIC S9(8) COMP.      DB2PROJ
         05  W-COMM-PTR                      USAGE IS POINTER.          DB2PROJ
         05  W-COMM-PTR-R   REDEFINES W-COMM-PTR   PIC S9(8) COMP.      DB2PROJ

       01  NULLS-INDICATORS.                                            ELDANA
           05  NULL-IND-01             PIC S9(04) COMP.                 ELDANA

      *****************************************************************
      *        WORKING-STORAGE         ACCUMULATORS                   *
      *****************************************************************

       01  ACCUMULATORS.
           05  A-DUMMY                     PIC S9(04) COMP VALUE +0.
           EJECT
      *****************************************************************
      *        WORKING-STORAGE         CONSTANTS                      *
      *****************************************************************

       01  CONSTANTS.
             COPY SFCWSCON.                                             00001490
           05  K-MSG-CODES.
               10  K-MSG-ACCTG-BAL-ERR     PIC  X(06)    VALUE '060104'.
               10  K-MSG-BAD-INDEX         PIC  X(06)    VALUE '060106'.
               10  K-MSG-BAD-FM-TBL-POS    PIC  X(06)    VALUE '060107'.
               10  K-MSG-NO-EFF-DATE       PIC  X(06)    VALUE '060108'.
               10  K-MSG-FM-EXCEED-1000    PIC  X(06)    VALUE '060109'.
               10  K-MSG-DB2-ERROR         PIC  X(06)    VALUE '060110'.
               10  K-MSG-NO-AMOUNT         PIC  X(06)    VALUE '060111'.
               10  K-MSG-WRONG-ACCOUNT     PIC  X(06)    VALUE '060113'.
               10  K-NO-SPACE-IN-ODTXNOUT  PIC  X(06)    VALUE '060114'.
               10  K-ERROR-WRITTING-ODTXNOUT PIC X(6)    VALUE '060115'.
               10  K-ODTXNOUT-DUPREC       PIC  X(06)    VALUE '060116'.
               10  K-ODTXNOUT-CLOSE        PIC  X(06)    VALUE '060117'.
               10  K-ERROR-READ-CIVINQF    PIC  X(6)     VALUE '060118'.
               10  K-CIVINQF-CLOSE         PIC  X(6)     VALUE '060119'.
           EJECT
      ******************************************************************
      *        WORKING-STORAGE         SWITCHES
      ******************************************************************

       01  SWITCHES.
           05  SW-DUMMY                    PIC  X(01).
           EJECT
      *****************************************************************
      *        WORKING-STORAGE         WORK-AREAS                     *
      *****************************************************************

       01  WORK-AREAS.
           05  W-PEX-POS                   PIC  9(3)   VALUE ZERO.
           05  W-MODULE-S210               PIC  X(08) VALUE 'SFSSS210'.
           05  W-SPSCALL                   PIC  X(08) VALUE 'SPSCALLX'.
           05  W-PEX-ACCTG-ACCUM           PIC S9(08)V999 COMP-3
                                               VALUE +0.

       01  SSN-G-NUM-VSAM-REC.
           05  SSN-G-NUM                          PIC 9(11).
           05  FILLER                             PIC X(01).
           05  SSN-G-CIV                          PIC 9(12).
           05  FILLER                             PIC X(141).

       01  W-SYSTEM-DATE-TIME.
           05 W-SYSTEM-DATE   PIC 9(8).
           05 W-SYSTEM-TIME   PIC 9(8).
           05 FILLER REDEFINES W-SYSTEM-TIME.
              10 W-SYSTEM-HHMMSS PIC 9(6).
              10 W-SYSTEM-MILISS PIC 9(2).
      *
       01  ORSS0400-AREA.
      *
           02 ORSS0400-SQLCA          PIC X(136).
           02 F REDEFINES ORSS0400-SQLCA.
              03 F1                   PIC  X(12).
              03 ORSS0400-SQLCD       PIC S9(09)   COMP.
              03 F2                   PIC  X(120).
      *
           02 ORSS0400-SYSID          PIC  X(03).
      *
           02 ORSS0400-CLERK          PIC  X(05).
      *
           02 ORSS0400-TABNM          PIC  X(30).
      *
           02 ORSS0400-SSN            PIC  9(12).
           02 ORSS0400-SSN-X REDEFINES ORSS0400-SSN
                                    PIC  X(12).
      *
           02 ORSS0400-TRNID          PIC  X(04).
      *
       01  ABEND-ERR-MSG.
           02 ABEND-ERR-TEXT          PIC X(10).
           02 F                       PIC X(01) VALUE SPACES.
           02 ABEND-ERR-EIBDS         PIC X(30).
           02 F                       PIC X(01) VALUE SPACES.
           02 F                       PIC X(05) VALUE '—Â“Á '.
           02 F                       PIC X(01) VALUE SPACES.
           02 ABEND-ERR-CODE          PIC X(04).
      *
       01  W-EIBDS                   PIC X(30).
       01  EMPLER-IND                PIC S9(04) COMP.
       01  REGNUM-IND                PIC S9(04) COMP.
      *
       01  J                         PIC S9(04) COMP.
      *
       01  I                         PIC S9(04) COMP.
      *
       01  ABEND-CODE                PIC X(04).
      *
       01  ERROR-CODE                PIC X(12) VALUE SPACES.
      *
       01  SQLCODE-9                 PIC 9(08).
      *
       01  W-SQLCD                   PIC Z(09)-.
       01  W-SQLCD-C REDEFINES W-SQLCD
                                     PIC X(10).
      *
       01  W-DATE                       PIC X(10).
       01  W-DATEX REDEFINES W-DATE.
           02 W-YEAR                    PIC 9(04).
           02 F11                       PIC X(01).
           02 W-MNTH                    PIC 9(02).
           02 F22                       PIC X(01).
           02 W-DAY                     PIC 9(02).
      *
       01  W-DATE-9.
           02 W-YEAR9                   PIC 9(04).
           02 W-MNTH9                   PIC 9(02).
           02 W-DAY9                    PIC 9(02).
       01  W-DATE9 REDEFINES W-DATE-9   PIC 9(08).
      *
       01  W-CURRENT-DATE               PIC X(10).
       01  W-CURRENT-DATE-R REDEFINES W-CURRENT-DATE.
               05   W-CURRENT-YEAR    PIC XXXX.
               05   W-SL1             PIC X.
               05   W-CURRENT-MONTH   PIC XX.
               05   W-SL2             PIC X.
               05   W-CURRENT-DAY     PIC XX.
      *
           EXEC SQL INCLUDE SQLCA          END-EXEC.
           EXEC SQL INCLUDE LOKLTACC       END-EXEC.
           EXEC SQL INCLUDE SSSLTEEB       END-EXEC.
           EXEC SQL INCLUDE SSSLTERB       END-EXEC.
      *
      *****************************************************************

       01  XCTL-TO-1365-EE-RECON.                                       SMR#4133
         05  FILLER                                   PIC  X            SMR#4133
                                                 VALUE LOW-VALUE.       SMR#4133
         05  FILLER                                   PIC  X(4)         SMR#4133
                                                 VALUE '1365'.          SMR#4133
         05  FILLER                                   PIC  X            SMR#4133
                                                 VALUE '3'.             SMR#4133

           05  W-RESPONSE                  PIC S9(08) COMP.
           05  C-DATA-LENGTH               PIC S9(04) COMP VALUE +106.
           05  W-MISC-DATA-LENGTH          PIC S9(04) COMP VALUE +30.
           05  W-SYSTEM-ID                 PIC  X(02) VALUE '12'.
           05  W-CODE-DESTINATION          PIC  X(02) VALUE '03'.
           05  C-FUNCTION-CODE             PIC  X(01) VALUE '2'.
      **   05  K-JUNE-06                   PIC  9(02) VALUE 06.         FSCPROG1
      **   05  K-JUNE-30                   PIC  9(02) VALUE 30.         FSCPROG1
           05  K-MARCH-03                  PIC  9(02) VALUE 03.         FSCPROG1
           05  K-MARCH-31                  PIC  9(02) VALUE 31.         FSCPROG1

S44431     05  W-DB2-ACC-9                 PIC  9(09) VALUE  0.
S44431     05  W-DB2-ACC-R REDEFINES W-DB2-ACC-9.
S44431         10  W-DB2-ACC-6             PIC  X(06).
S44431         10  W-DB2-ACC-3             PIC  X(03).

           05  W-DATE-WORK                 PIC  9(08) VALUE  0.
                                                                        MVS-CHG
           05  W-DATE-WORK-R REDEFINES W-DATE-WORK.
               10  W-DATE-WK-R             PIC  9(08).

      **   05  W-FORCE-JUNE-DATE           PIC  9(08) VALUE ZERO.       FSCPROG1
      **   05  W-FORCE-DATE REDEFINES  W-FORCE-JUNE-DATE.               FSCPROG1
      **       10  W-FORCE-JUNE-YR         PIC  9(04).                  FSCPROG1
      **       10  W-FORCE-JUNE-MTH        PIC  9(02).                  FSCPROG1
      **       10  W-FORCE-JUNE-DAY        PIC  9(02).                  FSCPROG1
           05  W-FORCE-MARCH-DATE          PIC  9(08) VALUE ZERO.       FSCPROG1
           05  W-FORCE-DATE REDEFINES  W-FORCE-MARCH-DATE.              FSCPROG1
               10  W-FORCE-MARCH-YR        PIC  9(04).                  FSCPROG1
               10  W-FORCE-MARCH-MTH       PIC  9(02).                  FSCPROG1
               10  W-FORCE-MARCH-DAY       PIC  9(02).                  FSCPROG1
           05  W-9                         PIC S9(08)V999.
           05  W-Z                         PIC ZZZZZZZZ.999B-.
           EJECT
            COPY  SFCTBDST.                                             00002280
            COPY  SFCTBFMT.                                             00002290
            COPY  SFCTBSRC.                                             00002300
           EJECT

       01  FILE-MAINT-ACCTG-EXT.
            COPY  SFCFFD01.                                             00002340
       01  ACCT-TRANSACTION REDEFINES FILE-MAINT-ACCTG-EXT.
           05  FILLER                      PIC  X(104).

           EJECT
      *****************************************************************
      *            L I N K A G E   S E C T I O N                      *
      *                                                               *
      *****************************************************************

       LINKAGE SECTION.

       01  CWA-AREA.                                                    DB2PROJ
           COPY OOC0030.                                                00002470
           EJECT                                                        DB2PROJ
                                                                        DB2PROJ
       01  TWA-AREA.                                                    DB2PROJ
           05  TWA-EXPAND-PARMS.                                        DB2PROJ
               10  TWA-PARM1                     PIC S9(08) COMP.       DB2PROJ
               10  TWA-PARM2                     PIC S9(08) COMP.       DB2PROJ
               10  TWA-PARM3                     PIC S9(08) COMP.       DB2PROJ
               10  FILLER  REDEFINES  TWA-PARM3.                        DB2PROJ
                   20  TWA-REC-LENGTH            PIC S9(04) COMP.       DB2PROJ
                   20  TWA-RTRN-CODE             PIC S9(04) COMP.       DB2PROJ
               10  TWA-PARM4                     PIC S9(08) COMP.       DB2PROJ
               10  TWA-PARM5                     PIC S9(08) COMP.       DB2PROJ
                                                                        DB2PROJ
           05  TWA-CONDENSE-PARMS                                       DB2PROJ
                   REDEFINES  TWA-EXPAND-PARMS.                         DB2PROJ
               10  TWA-CNDSE-MASREC-ADDR        PIC S9(08) COMP.        DB2PROJ
               10  TWA-CNDSE-MASOCC-TBL         PIC S9(08) COMP.        DB2PROJ
               10  TWA-CNDSE-MASREC-LEN         PIC S9(04) COMP.        DB2PROJ
               10  TWA-CNDSE-MASRET-CODE        PIC S9(04) COMP.        DB2PROJ
       01  TWA-S-ROUTINE REDEFINES TWA-AREA.                            DB2PROJ
           COPY OBCSTWA.                                                00002680
           EJECT                                                        DB2PROJ
                                                                        DB2PROJ
           COPY OOC0010.                                                00002710
      *    05  TCTTE-TXN-COUNT          PIC 9(5).                       DB2PROJ
           EJECT                                                        DB2PROJ
                                                                        DB2PROJ
                                                                        DB2PROJ
       01  TRANSACTION                  PIC X(4000).                    DB2PROJ
       01  TRANSACTION-R   REDEFINES    TRANSACTION.                    DB2PROJ
           COPY SFCTTXNL.                                               00002780
                                                                        DB2PROJ
           EJECT                                                        DB2PROJ
           COPY SFCOMRG0.                                               00002810

           EJECT
      *****************************************************************
      *                                                               *
      *            P R O C E D U R E   D I V I S I O N                *
      *                                                               *
      *****************************************************************

       PROCEDURE DIVISION.

      *****************************************************************
      *                                                               *
      *            M A I N L I N E    -    OBSSS601                   *
      *                                                               *
      *****************************************************************

       A0000-MAINLINE  SECTION.

           PERFORM  B0000-INITIALIZATION.

           PERFORM  C0000-PROCESS-TRANSACTION.

           PERFORM  D0000-FINALIZATION.

       A0000-MAINLINE-EXIT.
           EXIT PROGRAM.
           EJECT
      *****************************************************************
      *                                                               *
      *            I N I T I A L I Z A T I O N   -   OBSSS601         *
      *                                                               *
      *****************************************************************

       B0000-INITIALIZATION SECTION.

           EXEC CICS ADDRESS                                            DB2PROJ
                CWA   (ADDRESS OF CWA-AREA)                             DB2PROJ
                TWA   (ADDRESS OF TWA-AREA)                             DB2PROJ
                TCTUA (ADDRESS OF TCTTE-USER-AREA)                      DB2PROJ
           END-EXEC.                                                    DB2PROJ
                                                                        DB2PROJ
           MOVE  TWA-PARM1  TO W-TRAN-PTR-R.                            DB2PROJ
           SET  ADDRESS OF TRANSACTION TO W-TRAN-PTR.                   DB2PROJ

           MOVE  TWA-PARM3  TO W-COMM-PTR-R.                            DB2PROJ
           SET   ADDRESS OF  COMMUNICATION-REGION  TO  W-COMM-PTR.      DB2PROJ
                                                                        DB2PROJ
           PERFORM  B1000-AUDIT-PEX-ACCT-TABLE.

           PERFORM  B3000-SETUP-PREV-FUT-YR-ACCTG
                    VARYING  C-PEX-ATBL-NDX  FROM  1  BY  1
                    UNTIL    C-PEX-ATBL-NDX  IS GREATER THAN
                             C-ACCTG-SAVE-NDX.

       B0000-INITIALIZATION-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *        *** B1000-AUDIT-PEX-ACCT-TABLE ***                     *
      *                                                               *
      *            THIS SECTION WILL AUDIT THE ENTRIES IN C-PEX-ATBL  *
      *            FOR THE BALANCE TO BE ZERO.                        *
      *                                                               *
      *****************************************************************

       B1000-AUDIT-PEX-ACCT-TABLE      SECTION.

           SET C-PEX-ATBL-NDX          TO  C-ACCTG-SAVE-NDX.

           IF  C-PEX-ATBL-NDX  IS GREATER THAN  C-SKC-MAX-PEXATBLS
                               OR IS LESS THAN  +1

               MOVE  K-MSG-BAD-INDEX    TO  C-MESSAGE-CODE
               PERFORM  S0100-RETURN-BACK
           ELSE
               NEXT SENTENCE.
      *
           PERFORM  B1050-EDIT-ACCTG-TABLE-POS
                    VARYING  C-PEX-ATBL-NDX  FROM  1  BY  1
                    UNTIL    C-PEX-ATBL-NDX IS GREATER THAN
                             C-ACCTG-SAVE-NDX.

      *RT  IF  C-ACCTG-EFFECTIVE-DATE  IS EQUAL TO  ZEROES
      *RT      MOVE  K-MSG-NO-EFF-DATE  TO  C-MESSAGE-CODE
      *RT      PERFORM  S0100-RETURN-BACK
      *RT  ELSE
      *RT      NEXT  SENTENCE.

           MOVE  ZEROES  TO  W-PEX-ACCTG-ACCUM.

           PERFORM  B1100-ACCUM-PEXATBL
                    VARYING  C-PEX-ATBL-NDX  FROM  1  BY  1
                    UNTIL    C-PEX-ATBL-NDX  IS GREATER THAN
                             C-ACCTG-SAVE-NDX.

           IF  W-PEX-ACCTG-ACCUM  IS EQUAL TO  ZEROES
               NEXT  SENTENCE
           ELSE
               MOVE  K-MSG-ACCTG-BAL-ERR  TO  C-MESSAGE-CODE
               PERFORM  S0100-RETURN-BACK.

       B1000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *        *** B1050-EDIT-ACCTG-TABLE-POS ***                     *
      *                                                               *
      *****************************************************************

       B1050-EDIT-ACCTG-TABLE-POS      SECTION.

           IF C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) IS EQUAL TO '000000' OR
              C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) IS NOT NUMERIC

               MOVE  K-MSG-WRONG-ACCOUNT          TO  C-MESSAGE-CODE
               PERFORM  S0100-RETURN-BACK
           ELSE
               NEXT  SENTENCE.

      *DB2
      *DB2 IF C-PEX-ATBL-POS (C-PEX-ATBL-NDX)
      *DB2                    IS GREATER THAN  C-SKC-MAX-FMATBLS
      *DB2                    OR IS LESS THAN  +1
      *DB2
      *DB2     MOVE  C-PEX-ATBL-POS (C-PEX-ATBL-NDX) TO W-PEX-POS
      *DB2     MOVE  K-MSG-BAD-FM-TBL-POS            TO  C-MESSAGE-CODE
      *DB2     PERFORM  S0100-RETURN-BACK
      *DB2 ELSE
      *DB2     NEXT  SENTENCE.

       B1050-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *        *** B1100-ACCUM-PEXATBL ***                            *
      *                                                               *
      *****************************************************************

       B1100-ACCUM-PEXATBL  SECTION.

           ADD      C-PEX-ATBL-DR-AMT (C-PEX-ATBL-NDX)
                                       TO     W-PEX-ACCTG-ACCUM.

           SUBTRACT  C-PEX-ATBL-CR-AMT (C-PEX-ATBL-NDX)
                                        FROM  W-PEX-ACCTG-ACCUM.

       B1100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *        *** B3000-SETUP-PREV-FUT-YR-ACCTG ***                  *
      *                                                               *
      *****************************************************************

       B3000-SETUP-PREV-FUT-YR-ACCTG   SECTION.

           IF  C-PEX-EFFECT-DATE (C-PEX-ATBL-NDX)  EQUAL  ZEROES
               MOVE  K-MSG-NO-EFF-DATE  TO  C-MESSAGE-CODE
               PERFORM  S0100-RETURN-BACK
           ELSE
               NEXT  SENTENCE.

           IF C-PEX-EFFECT-DATE (C-PEX-ATBL-NDX)  LESS THAN
                                       C-ACCTG-BEGIN-FISCAL-DATE
                   IF  C-PEX-TRAN-CODE (C-PEX-ATBL-NDX) EQUAL TO '6600' SMR3782
                                                              OR '6700' SMR3782
                                                              OR '6740' SMR3782
                       PERFORM B3100-PREV-YEAR-ACCTG-STP                SMR3782
                   ELSE                                                 SMR3782
                       PERFORM B3100-PREV-YEAR-ACCTG                    SMR3782
           ELSE
               IF  C-PEX-EFFECT-DATE (C-PEX-ATBL-NDX)  GREATER  THAN
                                          C-ACCTG-END-FISCAL-DATE
                   PERFORM  B3200-FUT-YEAR-ACCTG
               ELSE
                   NEXT  SENTENCE.

       B3000-EXIT.
           EXIT.

      ***************************************************************** SMR3782
      *                                                               * SMR3782
      *        *** B3100-PREV-YEAR-ACCTG-STP  ***                     * SMR3782
      *                                                               * SMR3782
      ***************************************************************** SMR3782
                                                                        SMR3782
       B3100-PREV-YEAR-ACCTG-STP       SECTION.                         SMR3782
                                                                        SMR3782
S44431     MOVE 'C-ACCT-TBL-CIVIL-INCR-2008'    TO  T-ACCT-NAME.        INCR2001
S44431     PERFORM S0500-GET-ACC-BY-NUM-NAM.
S44431                                                                  SMR3782
S44431     IF SQLCODE = +0
S44431         MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO  T-ACCT-NAME
S44431         PERFORM S0600-GET-ACCOUNT-NUMBER
S44431         GO   TO   B3100-STP-END                                  SMR3782
S44431     ELSE                                                         SMR3782
S44431         NEXT SENTENCE.                                           SMR3782
S44431                                                                  SMR3782
S44431     MOVE 'C-ACCT-TBL-MILT-INCR-2008'     TO  T-ACCT-NAME.        INCR2001
S44431     PERFORM S0500-GET-ACC-BY-NUM-NAM.
S44431                                                                  SMR3782
S44431     IF SQLCODE = +0
S44431*95510   MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO  T-ACCT-NAME
S95510         MOVE 'C-ACCT-TBL-MILT-NO-RIGHT-EXPS' TO  T-ACCT-NAME
S44431         PERFORM S0600-GET-ACCOUNT-NUMBER
S44431         GO   TO   B3100-STP-END                                  SMR3782
S44431     ELSE                                                         SMR3782
S44431         NEXT SENTENCE.                                           SMR3782
S44431                                                                  SMR3782
S44431     MOVE 'C-ACCT-TBL-SEI-INCR-2008'      TO  T-ACCT-NAME.        INCR2001
S44431     PERFORM S0500-GET-ACC-BY-NUM-NAM.
S44431                                                                  SMR3782
S44431     IF SQLCODE = +0
S44431*95510   MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO  T-ACCT-NAME
S95510         MOVE 'C-ACCT-TBL-SEI-OTHER-INCAME'  TO  T-ACCT-NAME
S44431         PERFORM S0600-GET-ACCOUNT-NUMBER
S44431         GO   TO   B3100-STP-END                                  SMR3782
S44431     ELSE                                                         SMR3782
S44431         NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-GOV-ER-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-ER-CONT-PREV-YR' TO  T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-OIL-ER-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-OIL-ER-CONT-PREV'    TO T-ACCT-NAME     SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-PRIV-ER-CONT-CURR'  TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-PRIV-ER-CONT-PREV'  TO T-ACCT-NAME      SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-GOV-EE-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-GOV-EE-CONT-PREV'   TO T-ACCT-NAME      SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-OIL-EE-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-OIL-EE-CONT-PREV'   TO T-ACCT-NAME      SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-PRIV-EE-CONT-CURR'  TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EE-CONT-PREV-YR' TO T-ACCT-NAME         SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-SPL-OIL-BENP-CR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-SPL-OIL-BENP-CR'   TO T-ACCT-NAME       SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-D-GRANT-CURR-YR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-D-GRANT-CURR-YR' TO T-ACCT-NAME         SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-D-GRANT-PREV-YR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-D-GRANT-PREV-YR' TO T-ACCT-NAME         SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-MILT-ER-CONTRIBUTIN' TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-MILT-ER-PAYMENT' TO T-ACCT-NAME         SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-MILT-EE-CONTRIBUTIN' TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-MILT-EE-PAYMENT' TO T-ACCT-NAME         SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
      **** CIVIL FUNDS A/C REVERSING ***********                        SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-CIVL-DISAB-AFT-TRM'  TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT'    TO T-ACCT-NAME    SMR41356
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR41356
               NEXT SENTENCE.                                           SMR41356
                                                                        SMR41356
           MOVE 'C-ACCT-TBL-CIVL-DEATH-AFT-TRM'  TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT'   TO T-ACCT-NAME     SMR41356
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR41356
               NEXT SENTENCE.                                           SMR41356
                                                                        SMR41356
           MOVE 'C-ACCT-TBL-OLD-AGE-CURR-YR'     TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT'   TO T-ACCT-NAME     SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-DEATH-CURR-YR'       TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT'   TO T-ACCT-NAME     SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-DISABL-CURR-YR'      TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT'   TO T-ACCT-NAME     SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-SICK-CURR-YR'        TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO T-ACCT-NAME       SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-RET-BEN-CURR-YR'     TO  T-ACCT-NAME.       SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO T-ACCT-NAME       SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
      **** MILT. FUNDS A/C REVERSING ***********                        SMR3782
                                                                        SMR3782
           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-MILT-OLD-AGE-PAY'         ,           SMR3782
                      'C-ACCT-TBL-MILT-BAD-HELTH-PAY'       ,           SMR3782
                      'C-ACCT-TBL-MILT-RETIRE-PAY'          ,           SMR3782
                      'C-ACCT-TBL-MILT-DEATH-PAY'           ,           SMR3782
                      'C-ACCT-TBL-MILT-OTHER-PAY'           ,           SMR3782
                      'C-ACCT-TBL-MILT-DISABLE-PAY'         ,           SMR3782
                      'C-ACCT-TBL-MILT-DISAB-AFT-TRM'       ,           SMR41356
                      'C-ACCT-TBL-MILT-DEATH-AFT-TRM' )                 SMR41356
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-MILT-NO-RIGHT-EXPS'   TO  T-ACCT-NAME   SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
      **** SEI.  FUNDS A/C REVERSING ***********                        SMR3782
                                                                        SMR3782
           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-SEI-OLD-AGE'              ,           SMR3782
                      'C-ACCT-TBL-SEI-INVALIDITY'           ,           SMR3782
                      'C-ACCT-TBL-SEI-DEATH'                ,           SMR3782
                      'C-ACCT-TBL-SEI-SICKNESS' )                       SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-SEI-OTHER-INCAME'    TO  T-ACCT-NAME    SMR3782
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         SMR3782
               NEXT SENTENCE.                                           SMR3782
                                                                        INCR2001
           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-CIVIL-INCR-2001'          ,           SMR3782
                      'C-ACCT-TBL-SEI-INCR-2001'            ,           SMR3782
                      'C-ACCT-TBL-MILT-INCR-2001' )                     SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
S44431*        MOVE 'C-ACCT-TBL-PRV-YR-INCR-2001'    TO T-ACCT-NAME     INCR2001
S44431         MOVE 'C-ACCT-TBL-NON-DUE-INCR'        TO T-ACCT-NAME     INCR2001
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-STP-END                                  SMR3782
           ELSE                                                         INCR2001
               NEXT SENTENCE.                                           INCR2001

       B3100-STP-END.                                                   SMR3782

           PERFORM   B3100-PREV-YEAR-COMPL.                             PROJCOMP

       B3100-STP-EXIT.                                                  SMR3782
           EXIT.                                                        SMR3782
                                                                        SMR3782
      *****************************************************************
      *                                                               *
      *        *** B3100-PREV-YEAR-ACCTG ***                          *
      *                                                               *
      *****************************************************************

       B3100-PREV-YEAR-ACCTG           SECTION.

S44431     MOVE 'C-ACCT-TBL-CIVIL-INCR-2008'    TO  T-ACCT-NAME.        INCR2001
S44431     PERFORM S0500-GET-ACC-BY-NUM-NAM.
S44431                                                                  SMR3782
S44431     IF SQLCODE = +0
S44431         MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO  T-ACCT-NAME
S44431         PERFORM S0600-GET-ACCOUNT-NUMBER
S44431         GO   TO   B3100-ACCTG-END                                SMR3782
S44431     ELSE                                                         SMR3782
S44431         NEXT SENTENCE.                                           SMR3782
S44431                                                                  SMR3782
S44431     MOVE 'C-ACCT-TBL-MILT-INCR-2008'     TO  T-ACCT-NAME.        INCR2001
S44431     PERFORM S0500-GET-ACC-BY-NUM-NAM.
S44431                                                                  SMR3782
S44431     IF SQLCODE = +0
S44431         MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO  T-ACCT-NAME
S44431         PERFORM S0600-GET-ACCOUNT-NUMBER
S44431         GO   TO   B3100-ACCTG-END                                SMR3782
S44431     ELSE                                                         SMR3782
S44431         NEXT SENTENCE.                                           SMR3782
S44431                                                                  SMR3782
S44431     MOVE 'C-ACCT-TBL-SEI-INCR-2008'      TO  T-ACCT-NAME.        INCR2001
S44431     PERFORM S0500-GET-ACC-BY-NUM-NAM.
S44431                                                                  SMR3782
S44431     IF SQLCODE = +0
S44431         MOVE 'C-ACCT-TBL-EXP-WITHOUT-RIGHT' TO  T-ACCT-NAME
S44431         PERFORM S0600-GET-ACCOUNT-NUMBER
S44431         GO   TO   B3100-ACCTG-END                                SMR3782
S44431     ELSE                                                         SMR3782
S44431         NEXT SENTENCE.                                           SMR3782
                                                                        SMR3782
           MOVE 'C-ACCT-TBL-GOV-ER-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-ER-CONT-PREV-YR' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-OIL-ER-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-OIL-ER-CONT-PREV'  TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-PRIV-ER-CONT-CURR'  TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-PRIV-ER-CONT-PREV' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-GOV-EE-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-GOV-EE-CONT-PREV'  TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-OIL-EE-CONT-CURR'   TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-OIL-EE-CONT-PREV'  TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-PRIV-EE-CONT-CURR'  TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-EE-CONT-PREV-YR'  TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-OLD-AGE-CURR-YR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-OLD-AGE-PREV-YR' TO  T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-SPL-OIL-BENP-CR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-SPL-OIL-BENP-CR' TO T-ACCT-NAME         SMR40655
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE                                                         SMR40655
               NEXT SENTENCE.                                           SMR40655

           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-DEATH-CURR-YR'            ,           SMR3782
                      'C-ACCT-TBL-CIVL-DEATH-AFT-TRM' )                 SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-DEATH-PREV-YR' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-DISABL-CURR-YR'     TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-DISABL-PREV-YR' TO  T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-SICK-CURR-YR'       TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-SICK-PREV-YR' TO  T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-RET-BEN-CURR-YR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-RET-BEN-PREV-YR' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-D-GRANT-CURR-YR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-D-GRANT-CURR-YR' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-D-GRANT-PREV-YR'    TO  T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-D-GRANT-PREV-YR' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-MILT-ER-CONTRIBUTIN' TO T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-MILT-ER-PAYMENT' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'C-ACCT-TBL-MILT-EE-CONTRIBUTIN' TO T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-MILT-EE-PAYMENT' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE
               NEXT SENTENCE.

           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-MILT-OLD-AGE-PAY'         ,           SMR3782
                      'C-ACCT-TBL-MILT-BAD-HELTH-PAY'       ,           SMR3782
                      'C-ACCT-TBL-MILT-RETIRE-PAY'          ,           SMR3782
                      'C-ACCT-TBL-MILT-DEATH-PAY'           ,           SMR3782
                      'C-ACCT-TBL-MILT-OTHER-PAY'           ,           SMR3782
                      'C-ACCT-TBL-MILT-DISABLE-PAY'         ,           SMR3782
                      'C-ACCT-TBL-MILT-DISAB-AFT-TRM'       ,           SMR3782
                      'C-ACCT-TBL-MILT-DEATH-AFT-TRM' )                 SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-MILT-BEN-PAY-PRV-YR' TO T-ACCT-NAME     COMPLAHM
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE                                                         COMPLAHM
               NEXT SENTENCE.                                           COMPLAHM

           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-SEI-OLD-AGE'              ,           SMR3782
                      'C-ACCT-TBL-SEI-INVALIDITY'           ,           SMR3782
                      'C-ACCT-TBL-SEI-DEATH'                ,           SMR3782
                      'C-ACCT-TBL-SEI-SICKNESS'  )                      SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-SEI-BEN-PAY-PREV-YR' TO T-ACCT-NAME     COMPLAHM
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE                                                         COMPLAHM
               NEXT SENTENCE.                                           COMPLAHM
                                                                        INCR2001
           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-CIVIL-INCR-2001'          ,           SMR3782
                      'C-ACCT-TBL-SEI-INCR-2001'            ,           SMR3782
                      'C-ACCT-TBL-MILT-INCR-2001' )                     SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
S44431*        MOVE 'C-ACCT-TBL-PRV-YR-INCR-2001'    TO T-ACCT-NAME     INCR2001
S44431         MOVE 'C-ACCT-TBL-NON-DUE-INCR'        TO T-ACCT-NAME     INCR2001
               PERFORM S0600-GET-ACCOUNT-NUMBER
               GO   TO   B3100-ACCTG-END
           ELSE                                                         INCR2001
               NEXT SENTENCE.                                           INCR2001
                                                                        INCR2001
       B3100-ACCTG-END.
                                                                        INCR2001
           PERFORM   B3100-PREV-YEAR-COMPL.                             PROJCOMP

       B3100-EXIT.
           EXIT.
           EJECT

      ***************************************************************** PROJCOMP
      *                                                               * PROJCOMP
      *        *** B3100-PREV-YEAR-COMPL ***                          * PROJCOMP
      *                                                               * PROJCOMP
      ***************************************************************** PROJCOMP
                                                                        PROJCOMP
       B3100-PREV-YEAR-COMPL           SECTION.                         PROJCOMP
                                                                        PROJCOMP
      ******************************************************            PROJCOMP
      **START OF  COMLEMENTARY PROJECT ACCOUNTS CHANGE                  PROJCOMP
      ******************************************************            PROJCOMP
                                                                        PROJCOMP
           IF C-PEX-TRAN-CODE (C-PEX-ATBL-NDX) EQUAL TO '6600'
                                                     OR '6700'
                                                     OR '6740'
                    NEXT SENTENCE
           ELSE
                    GO  TO   B3100-PREV-YEAR-SKIP.                      PROJCOMP
                                                                        PROJCOMP
           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-COMPL-LIFE-PENSION'       ,           SMR3782
                      'C-ACCT-TBL-COMPL-DEAD-PENSION' )                 SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
                MOVE 'C-ACCT-TBL-COMPL-REPAY-PRV-YR' TO T-ACCT-NAME     PROJCOMP
                PERFORM S0600-GET-ACCOUNT-NUMBER
           ELSE                                                         PROJCOMP
               NEXT SENTENCE.                                           PROJCOMP
                                                                        PROJCOMP
       B3100-PREV-YEAR-SKIP.                                            PROJCOMP
                                                                        PROJCOMP
           MOVE 'C-ACCT-TBL-COMPL-LIFE-PENSION'  TO T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-CMP-CVL-PRV-YR'      TO T-ACCT-NAME     SMR91754
               PERFORM S0600-GET-ACCOUNT-NUMBER
           ELSE                                                         PROJCOMP
               NEXT SENTENCE.                                           PROJCOMP
                                                                        PROJCOMP
           MOVE 'C-ACCT-TBL-COMPL-DEAD-PENSION'  TO T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-CMP-CVL-PRV-YR'      TO T-ACCT-NAME     SMR91754
               PERFORM S0600-GET-ACCOUNT-NUMBER
           ELSE                                                         PROJCOMP
               NEXT SENTENCE.                                           PROJCOMP
                                                                        PROJCOMP
           MOVE 'C-ACCT-TBL-COMPL-LIFE-PEN-OPT'  TO T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-CMP-MLT-PRV-YR'      TO T-ACCT-NAME     SMR91754
               PERFORM S0600-GET-ACCOUNT-NUMBER
           ELSE                                                         PROJCOMP
               NEXT SENTENCE.                                           PROJCOMP
                                                                        PROJCOMP
           MOVE 'C-ACCT-TBL-COMPL-DEAD-PEN-OPT'  TO T-ACCT-NAME.        SMR3782
           PERFORM S0500-GET-ACC-BY-NUM-NAM.
                                                                        SMR3782
           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-CMP-MLT-PRV-YR'      TO T-ACCT-NAME     SMR91754
               PERFORM S0600-GET-ACCOUNT-NUMBER
           ELSE                                                         PROJCOMP
               NEXT SENTENCE.                                           PROJCOMP
                                                                        PROJCOMP
      ******************************************************            PROJCOMP
      ** END  OF  COMLEMENTARY PROJECT ACCOUNTS CHANGE                  PROJCOMP
      ******************************************************            PROJCOMP
       B3100-COMPL-EXIT.                                                PROJCOMP
           EXIT.                                                        PROJCOMP
           EJECT
      *****************************************************************
      *                                                               *
      *        *** B3200-FUT-YEAR-ACCTG ***                           *
      *                                                               *
      *****************************************************************

       B3200-FUT-YEAR-ACCTG            SECTION.

           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-COMM-I-10-77'             ,           SMR3782
                      'C-ACCT-TBL-COMM-I-9-77'  )                       SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-COMM-INSTL-FUT-YR' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
           ELSE
               NEXT SENTENCE.

           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
                    ( 'C-ACCT-TBL-CVL-EEPEN-EEBEN'          ,           SMR3782
                      'C-ACCT-TBL-MLT-EEPEN-EEBEN'          ,           SMR3782
                      'C-ACCT-TBL-SEI-EEPEN-EEBEN'   )                  SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               MOVE 'C-ACCT-TBL-OTHER-INSTL-FUT-YR' TO T-ACCT-NAME
               PERFORM S0600-GET-ACCOUNT-NUMBER
           ELSE
               NEXT SENTENCE.

       B3200-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *            P R O C E S S   -   OBSSS601                       *
      *                                                               *
      *****************************************************************

       C0000-PROCESS-TRANSACTION    SECTION.
      *DB2
      *DB2 PERFORM C1000-UPDATE-FM-ATBL
      *DB2         VARYING C-PEX-ATBL-NDX  FROM 1 BY 1
      *DB2         UNTIL   C-PEX-ATBL-NDX IS GREATER THAN
      *DB2         C-ACCTG-SAVE-NDX.

           PERFORM C2000-DEVELOP-GL-TRANS.

      *    MOVE  ZEROES  TO  TCTTE-TXN-COUNT.
           PERFORM C3000-WRITE-GL-TRANS
                   VARYING C-PEX-ATBL-NDX  FROM 1 BY 1
                   UNTIL   C-PEX-ATBL-NDX IS GREATER THAN
                   C-ACCTG-SAVE-NDX.

           PERFORM C4000-ACCUM-ACCTG-SUSP-AMT
                   VARYING C-PEX-ATBL-NDX  FROM 1 BY 1
                   UNTIL   C-PEX-ATBL-NDX IS GREATER THAN
                   C-ACCTG-SAVE-NDX.

S93208     MOVE  FUNCTION CURRENT-DATE (1:4) TO  W-CURRENT-YEAR.
S93208     MOVE  FUNCTION CURRENT-DATE (5:2) TO  W-CURRENT-MONTH.
S93208     MOVE  FUNCTION CURRENT-DATE (7:2) TO  W-CURRENT-DAY.
S93208     MOVE  '-'                         TO  W-SL1 W-SL2.
S93208     MOVE  W-CURRENT-DATE              TO
S93208                                       T-M-BAS-EE-LAST-ACCT-DATE  ELDANA
S93208                                       T-M-BAS-ER-LAST-ACCT-DATE. ELDANA

S93208     IF  EMPLOYER-SSN
S93208              PERFORM C5000-UPDATE-LAST-AC-DAT-R
S93208         ELSE
S93208              PERFORM C5500-UPDATE-LAST-AC-DAT-E.

       C0000-PROCESS-EXIT.
           EXIT.
           EJECT

      ******************************************************************
      *                                                                *
      *      *** C1000-UPDATE-FM-ATBL  ***                             *
      *                                                                *
      ******************************************************************

       C1000-UPDATE-FM-ATBL            SECTION.
      *DB2
      *DB2 SET C-ACCTS-T-NDX           TO
      *DB2         C-PEX-ATBL-POS (C-PEX-ATBL-NDX).
      *DB2
      *DB2 ADD C-PEX-ATBL-DR-AMT (C-PEX-ATBL-NDX) TO
      *DB2                             C-ACCT-T-DR (C-ACCTS-T-NDX).
      *DB2
      *DB2 ADD C-PEX-ATBL-CR-AMT (C-PEX-ATBL-NDX) TO
      *DB2                             C-ACCT-T-CR (C-ACCTS-T-NDX).

       C1000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *      *** C2000-DEVELOP-GL-TRANS    ***                        *
      *                                                               *
      *****************************************************************

       C2000-DEVELOP-GL-TRANS          SECTION.

           MOVE TRANSACTION-LEAD       TO  FMA-TRANSACTION-LEAD.
           MOVE K-ACCT-REG-CODE        TO  FMA-LEAD-FORMAT-TYPE.
           MOVE W-MISC-DATA-LENGTH     TO  FMA-LEAD-TXN-DATA-LENGTH.
           MOVE  K-OA-DEST             TO  FMA-LEAD-DESTINATION.

           MOVE C-ACCTG-BUSINESS-DATE  TO  W-DATE-WORK.

      *****
      *    FOLLOWING CODE IS ADDED TO FORCE GL TXN DATE TO BE JUNE 30 *
      *    FOR EXTRCATS GENERATED BY BATCH COLL. SYSTEM.              *
      *****

           MOVE TRAN-LEAD-SSN-KEY      TO  FMA-AF-SS-NUMBER.
           MOVE C-ACCT-TRANS-EMPL-NUM  TO  FMA-AF-REF-NUM.

           PERFORM  READ-SSN-VSAM-FILE.

       C2000-EXIT.
           EXIT.
           EJECT
      ******************************************************************
      *                                                                *
      *     *** C3000-WRITE-GL-TRANS   ***                             *
      *                                                                *
      ******************************************************************

       C3000-WRITE-GL-TRANS            SECTION.

           IF  C-PEX-TRAN-CODE (C-PEX-ATBL-NDX) EQUAL TO '4308'  OR     SMR3782
               C-PEX-TRAN-CODE (C-PEX-ATBL-NDX) EQUAL TO '4309'         SMR3782

                  IF  FIN-YEAR-NOT-CLOSED
                      MOVE W-DATE-WK-R        TO W-FORCE-MARCH-DATE
                      MOVE K-MARCH-31         TO W-FORCE-MARCH-DAY
                      MOVE K-MARCH-03         TO W-FORCE-MARCH-MTH
                      MOVE W-FORCE-MARCH-DATE TO FMA-AF-EFFECTIVE-DATE
                  ELSE
                      MOVE W-DATE-WK-R        TO FMA-AF-EFFECTIVE-DATE

           ELSE

           IF  FIN-YEAR-NOT-CLOSED
                   AND
               FMA-LEAD-SOURCE IS EQUAL TO K-BCH-COLL-SOURCE-CODE

               MOVE W-DATE-WK-R TO W-FORCE-MARCH-DATE
               MOVE K-MARCH-31  TO W-FORCE-MARCH-DAY
               MOVE K-MARCH-03  TO W-FORCE-MARCH-MTH

               MOVE W-FORCE-MARCH-DATE TO FMA-AF-EFFECTIVE-DATE

           ELSE

               MOVE W-DATE-WK-R        TO FMA-AF-EFFECTIVE-DATE.

           IF  FIN-YEAR-NOT-CLOSED
                   AND
               FMA-LEAD-SOURCE IS EQUAL TO K-BCH-COLL-SOURCE-CODE
                        NEXT SENTENCE
                     ELSE
                        MOVE K-OTHR-FM-SOURCE-CODE  TO  FMA-LEAD-SOURCE.

           MOVE ZEROES                 TO  W-PEX-ACCTG-ACCUM.

           ADD C-PEX-ATBL-DR-AMT (C-PEX-ATBL-NDX) TO
                                       W-PEX-ACCTG-ACCUM.

           SUBTRACT C-PEX-ATBL-CR-AMT (C-PEX-ATBL-NDX)
               FROM W-PEX-ACCTG-ACCUM.

           IF W-PEX-ACCTG-ACCUM IS EQUAL TO ZEROES
               GO TO C3000-EXIT
           ELSE
               NEXT SENTENCE.
      *DB2
      *DB2 IF C-PEX-ATBL-POS (C-PEX-ATBL-NDX) IS EQUAL TO
      *DB2                             C-SKC-FMCLR
      *DB2     ADD W-PEX-ACCTG-ACCUM TO C-ACCTG-CY-CLEARING
      *DB2 ELSE
      *DB2 IF C-PEX-ATBL-POS (C-PEX-ATBL-NDX) IS EQUAL TO
      *DB2                             C-SKC-DISBCLR
      *DB2     ADD W-PEX-ACCTG-ACCUM TO C-ACCTG-CY-DISB-CLR
      *DB2 ELSE
      *DB2     NEXT SENTENCE.

           MOVE W-PEX-ACCTG-ACCUM      TO  FMA-AF-AMOUNT.
           MOVE C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO  FMA-AF-ACCT-CDE.
      ***  MOVE C-PEX-TRAN-CODE(C-PEX-ATBL-NDX) TO  TRAN-LEAD-TRAN-CODE.
           MOVE C-PEX-TRAN-CODE(C-PEX-ATBL-NDX) TO  FMA-LEAD-TRAN-CODE.

           EXEC CICS ASKTIME
           END-EXEC.

      *    MOVE  EIBTIME  TO  FMA-LEAD-TIME.
           MOVE FUNCTION CURRENT-DATE(1:16)
                                 TO  W-SYSTEM-DATE-TIME.
      *    MOVE W-SYSTEM-HHMMSS  TO  FMA-LEAD-TIME.                     PRFX0703
           MOVE W-SYSTEM-TIME    TO  FMA-LEAD-TIME-MILISEC.             0003190

           ADD  1  TO  C-TXN-COUNT.
           MOVE  C-TXN-COUNT  TO  FMA-LEAD-COUNT.

           PERFORM C3100-WRITE-GL-TXNS.

       C3000-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *        *** C3100-WRITE-GL-TRANS ***                           *
      *                                                               *
      *****************************************************************

       C3100-WRITE-GL-TXNS         SECTION.

           EXEC CICS WRITE
                FILE   ('ODTXNOUT')
                RIDFLD (FMA-KEY)
                FROM   (FILE-MAINT-ACCTG-EXT)
                LENGTH (LENGTH OF FILE-MAINT-ACCTG-EXT)
                RESP   (W-RESPONSE)
           END-EXEC.

           IF  W-RESPONSE EQUAL ZERO
               NEXT  SENTENCE
           ELSE
               IF W-RESPONSE = DFHRESP(NOTOPEN)
                  MOVE K-ODTXNOUT-CLOSE TO  C-MESSAGE-CODE
                  PERFORM S0100-RETURN-BACK                             SM90265
               ELSE
               IF W-RESPONSE = DFHRESP(NOTOPEN)
                  MOVE K-ODTXNOUT-CLOSE TO  C-MESSAGE-CODE
                  PERFORM S0100-RETURN-BACK                             SM90265
               ELSE
               IF W-RESPONSE = DFHRESP(DUPREC)
                  MOVE K-ODTXNOUT-DUPREC TO  C-MESSAGE-CODE
                  PERFORM S0100-RETURN-BACK                             SM90265
               ELSE
                  MOVE K-ERROR-WRITTING-ODTXNOUT TO  C-MESSAGE-CODE
                  PERFORM S0100-RETURN-BACK.                            SM90265

       C3100-EXIT.
           EXIT.
           EJECT
      *****************************************************************
      *                                                               *
      *        *** C4000-ACCUM-ACCTG-SUSP-AMT ***                     *
      *                                                               *
      *****************************************************************

       C4000-ACCUM-ACCTG-SUSP-AMT      SECTION.

           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS     IN(' ' , '0')
                AND   RTRIM(ACCT_NAME)     IN
           ( 'C-ACCT-TBL-ER-GOVT-CA-DB'                    ,            SMR3782
             'C-ACCT-TBL-SUSP-PAYMT'                       ,            SMR3782
             'C-ACCT-TBL-ER-OIL-CA-DB'                     ,            SMR3782
             'C-ACCT-TBL-ER-PRIV-CA-DB'                    ,            SMR3782
             'C-ACCT-TBL-ER-GOVT-CA-CR'                    ,            SMR3782
             'C-ACCT-TBL-ER-OIL-CA-CR'                     ,            SMR3782
             'C-ACCT-TBL-ER-PRIV-CA-CR'                    ,            SMR3782
             'C-ACCT-TBL-BETBEN'                           ,            SMR3782
             'C-ACCT-TBL-PEN-IN-ADV'                       ,            SMR3782
             'C-ACCT-TBL-UNCOLL-PENS'                      ,            SMR3782
             'C-ACCT-TBL-UNPAID-DEDNS'                     ,            SMR3782
             'C-ACCT-TBL-ER-FM-TRANS-SUSP'                 ,            SMR3782
             'C-ACCT-TBL-CIVIL-INCR-85'                    ,            SMR3782
             'C-ACCT-TBL-MILT-EXCEP-CLR-PENS'              ,            SMR3782
             'C-ACCT-TBL-EE-FM-TRSUSP-RCPTS'               ,            SMR3782
             'C-ACCT-TBL-MILT-EE-PENSR-CR'                 ,            SMR3782
             'C-ACCT-TBL-SEI-DEBIT-AMT'                    ,            SMR3782
             'C-ACCT-TBL-SEI-EE-CRED-AMT'          )                    SMR3782
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
               ADD C-PEX-ATBL-DR-AMT (C-PEX-ATBL-NDX) TO
                                       C-ACCTG-MR-FIN-CTRL
               SUBTRACT C-PEX-ATBL-CR-AMT (C-PEX-ATBL-NDX) FROM
                                       C-ACCTG-MR-FIN-CTRL
           ELSE
                GO TO  C4000-EXIT.

       C4000-EXIT.
           EXIT.

S93208 C5000-UPDATE-LAST-AC-DAT-R   SECTION.
S93208
S93208         MOVE 'ST_MST_EMPLOYER UPDATE ' TO W-EIBDS.               ELDANA
S93208         MOVE  TRAN-LEAD-SSN-KEY-9      TO T-M-BAS-ER-REG-NUM.    ELDANA
S93208         MOVE  ZEROES                   TO NULL-IND-01.           ELDANA
S93208
S93208         EXEC  SQL                                                ELDANA
S93208               UPDATE  ST_MST_EMPLOYER_BASIC                      ELDANA
S93208               SET  M_BAS_ER_LAST_ACCT_DATE                       ELDANA
S93208               =                                                  ELDANA
S93208                    :T-M-BAS-ER-LAST-ACCT-DATE:NULL-IND-01        ELDANA
S93208               WHERE                                              ELDANA
S93208                    M_BAS_ER_REG_NUM   =  :T-M-BAS-ER-REG-NUM     ELDANA
S93208         END-EXEC.                                                ELDANA
S93208
S93208     PERFORM DB2-ERROR.
S93208
S93208 C5000-EXIT.
S93208     EXIT.
S93208     EJECT

S93208 C5500-UPDATE-LAST-AC-DAT-E   SECTION.
S93208
S93208         MOVE 'ST_MST_EMPLOYEE UPDATE ' TO W-EIBDS.               ELDANA
S93208         MOVE  TRAN-LEAD-SSN-KEY-9      TO T-M-BAS-EE-SSN.        ELDANA
S93208         MOVE  ZEROES                   TO NULL-IND-01.           ELDANA
S93208
S93208         EXEC  SQL                                                ELDANA
S93208               UPDATE  ST_MST_EMPLOYEE_BASIC                      ELDANA
S93208               SET  M_BAS_EE_LAST_ACCT_DATE                       ELDANA
S93208               =                                                  ELDANA
S93208                    :T-M-BAS-EE-LAST-ACCT-DATE:NULL-IND-01        ELDANA
S93208               WHERE                                              ELDANA
S93208                    M_BAS_EE_SSN   =  :T-M-BAS-EE-SSN             ELDANA
S93208         END-EXEC.                                                ELDANA
S93208
S93208     PERFORM DB2-ERROR.
S93208
S93208 C5500-EXIT.
S93208     EXIT.
S93208     EJECT

       READ-SSN-VSAM-FILE   SECTION.

           MOVE   TRAN-LEAD-SSN-KEY   TO
                  SSN-G-NUM.

           EXEC  CICS  READ
                 FILE   ( 'SSNGNMB'     )
                 INTO   ( SSN-G-NUM-VSAM-REC  )
                 RIDFLD ( SSN-G-NUM     )
                 RESP   ( W-RESPONSE   )
           END-EXEC.

           IF  W-RESPONSE = DFHRESP (NORMAL)
               MOVE  SSN-G-CIV             TO  FMA-AF-CIVIL-ID
           ELSE
           IF  W-RESPONSE = DFHRESP (NOTFND)
               MOVE  ZEROES                TO  FMA-AF-CIVIL-ID
           ELSE
           IF  W-RESPONSE = DFHRESP (NOTOPEN)
               MOVE  K-CIVINQF-CLOSE       TO  C-MESSAGE-CODE
               PERFORM S0100-RETURN-BACK
           ELSE
               MOVE  K-ERROR-READ-CIVINQF  TO  C-MESSAGE-CODE
               PERFORM S0100-RETURN-BACK.

       READ-CIV-EXIT.
             EXIT.
      *****************************************************************
      *                                                               *
      *            F I N A L I Z A T I O N    -    OBSSS601           *
      *                                                               *
      *****************************************************************

       D0000-FINALIZATION SECTION.

           MOVE  ZEROES  TO  C-MESSAGE-CODE.
           PERFORM  S0100-RETURN-BACK.

       D0000-FINALIZATION-EXIT.
           EXIT.
           EJECT

       S0500-GET-ACC-BY-NUM-NAM    SECTION.

           MOVE 'LT_ACCOUNTS          '          TO W-EIBDS.
           MOVE  01                              TO T-ACCT-ORG-CODE.
S44431     MOVE  C-PEX-ATBL-NUM (C-PEX-ATBL-NDX) TO W-DB2-ACC-6.
S44431     MOVE  ZEROES                          TO W-DB2-ACC-3.
S44431     MOVE  W-DB2-ACC-9                     TO T-ACCT-CODE.

           EXEC SQL
                SELECT   ACCT_CODE
                INTO    :T-ACCT-CODE
                FROM     LT_ACCOUNTS
                WHERE  ACCT_CODE                   =
                      :T-ACCT-CODE
                AND   ACCT_ORG_CODE                =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS       IN(' ' , '0')
                AND   ACCT_NAME                    =
                      :T-ACCT-NAME
           END-EXEC.

           PERFORM DB2-ERROR.

       S0500-GET-AC-N-N-EXIT.
           EXIT.

       S0600-GET-ACCOUNT-NUMBER    SECTION.

           MOVE 'LT_ACCOUNTS          ' TO W-EIBDS.
           MOVE  01                     TO T-ACCT-ORG-CODE.

           EXEC SQL
                 SELECT   ACCT_CODE
                 INTO    :T-ACCT-CODE
                FROM  LT_ACCOUNTS
                WHERE ACCT_NAME                     =
                      :T-ACCT-NAME
                AND   ACCT_ORG_CODE                 =
                      :T-ACCT-ORG-CODE
                AND   ACCT_CODE_STATUS       IN(' ' , '0')
                ORDER BY
                      ACCT_CODE
                FETCH FIRST 1 ROW ONLY
           END-EXEC.

           PERFORM DB2-ERROR.

           IF SQLCODE = +0
                MOVE T-ACCT-CODE
                                TO C-PEX-ATBL-NUM      (C-PEX-ATBL-NDX)
              ELSE
                MOVE ZEROES     TO C-PEX-ATBL-NUM      (C-PEX-ATBL-NDX).


       S0600-GET-AC-NUM-EXIT.
           EXIT.

       DB2-ERROR            SECTION.

           IF NOT (SQLCODE = +0 OR +100)

              MOVE SQLCA TO ORSS0400-SQLCA

              EXEC CICS
                   SYNCPOINT ROLLBACK
              END-EXEC

              MOVE 'SSS'               TO ORSS0400-SYSID
              COMPUTE ORSS0400-SSN = FUNCTION NUMVAL(TRAN-LEAD-SSN-KEY)
              MOVE TCTTE-CURNT-TRAN-ID TO ORSS0400-TRNID
              MOVE TCTTE-DEPT          TO ORSS0400-CLERK(1:2)
              MOVE TCTTE-CLERK         TO ORSS0400-CLERK(3:3)
              MOVE W-EIBDS             TO ORSS0400-TABNM

              EXEC CICS
                   LINK
                   PROGRAM ('ORSS0400')
                   COMMAREA(ORSS0400-AREA)
                   LENGTH  (LENGTH OF ORSS0400-AREA)
              END-EXEC

              MOVE ORSS0400-SQLCD                   TO W-SQLCD
              MOVE FUNCTION REVERSE(W-SQLCD-C(7:4)) TO ABEND-ERR-CODE
              MOVE FUNCTION REVERSE(W-EIBDS)        TO ABEND-ERR-EIBDS
              MOVE 'Ø◊√ ÔŸ Ô‰·'                     TO ABEND-ERR-TEXT
              MOVE ORSS0400-SQLCA                   TO SQLCA

           END-IF.

       DB2-ERROR-EXIT.      EXIT.

      *****************************************************************
      *    S0100-RETURN-BACK      SECTION                                *
      *                                                               *
      *        THIS SECTION WILL ADD A NEW MESSAGE TO THE             *
      *        C-MSG-CODE-TABLE IN COM-REG                            *
      *                                                               *
      *****************************************************************

       S0100-RETURN-BACK              SECTION.
                                                                        RTURPROJ
               EXEC  CICS  RETURN
               END-EXEC.

       S0100-RETURN-BACK-EXIT.
           EXIT.

      ******************************************************************00000010
      *  INCLUDE IS OOC0010                                            *00000020
      *        COMMON DSECT FOR TCTTEUA USED FOR PROGRAM CONTROL       *00000030
      *    TCTTECIA       = ADDRESS OF TCTTE USER AREA                 *00000040
      *    TCTTECIL       = LENGTH OF TCTTE USER AREA                  *00000050
      *    TCTTE-ENTRY-SW = LOW-VALUE - FIRST ENTRY INTO MODULE        *00000060
      *                     HIGH-VALUE - SUBSEQUENT ENTRY INTO MODULE  *00000070
      *    TCTTE-ENTRY-COUNT = INDICATOR OF THE PROCESSING THAT HAS    *00000080
      *                        TAKEN PLACE AT THE TIME OF EXIT         *00000090
      *    TCTTETXN       = TXN ID REFLECTING CURRENT PROCESSING MODULE*00000100
      *    TCTTE-EXIT-SW  = LOW-VALUES  -  RETURN BY TYPE=RETURN       *00000110
      *                     HIGH-VALUES  -  RETURN BY TYPE=XCTL        *00000120
      *                                                                *00000130
      * WARNING: ADDING FIELDS TO OR MODIFYING THIS MEMBER COULD       *00000140
      *          CAUSE THE UNPREDICTABLE RESULTS AND COULD CAUSE        00000150
      *          THE SYSTEM TO CRASH........BEWARE.                     00000160
      *                                                                 00000170
      *----------------------------------------------------------------*00000180
      *                                                                *00000190
      ******************************************************************00000200
       SKIP2                                                            00000210
       01  TCTTE-USER-AREA.                                             00000220
         02  TCTTE-PROG-USER-AREA.                                      00000230
           05  TCTTE-CNTRL-AREA.                                        00000240
               10  TCTTE-ENTRY-SW      PIC X.                           00000250
                   88  ENTRY-SW-HIGH   VALUE  HIGH-VALUE.               00000260
                   88  ENTRY-SW-LOW    VALUE  LOW-VALUE.                00000270
               10  TCTTE-CURNT-TRAN-ID PIC X(4).                        00000280
               10  TCTTETXN REDEFINES TCTTE-CURNT-TRAN-ID  PIC X(4).    00000290
               10  TCTTE-ENTRY-COUNT   PIC X.                           00000300
               10  TCTTE-ENTRY-COUNT-N REDEFINES TCTTE-ENTRY-COUNT      00000310
                                       PIC 9.                           00000320
           05  TCTTE-RESERVED          PIC XX.                          00000330
           05  TCTTE-PARM              PIC S9(8)   COMP SYNC.           00000340
           05  TCTTE-RETURN            PIC S9(8)   COMP SYNC.           00000350
               88  RETURN-ZERO     VALUE ZERO.                          00000360
           05  TCTTE-ADDRS.                                             00000370
               10  TCTTE-USER-DATA     PIC S9(8)   COMP SYNC.           00000380
               10  TCTTE-ADDR2         PIC S9(8)   COMP SYNC.           00000390
               10  TCTTE-ADDR3         PIC S9(8)   COMP SYNC.           00000400
               10  TCTTE-ADDR4         PIC S9(8)   COMP SYNC.           00000410
           05  TCTTE-ADDRS-R  REDEFINES  TCTTE-ADDRS.                   00000420
               10  TCTTE-PGM-ID        PIC X(8).                        00000430
           05  TCTTE-SSN               PIC 9(11).                       00000440
           05  TCTTE-KEY-RED        REDEFINES TCTTE-SSN.                00000450
               10  FILLER              PIC XX.                          00000460
               10  TCTTE-PERSONNEL-KEY PIC 9(9).                        00000470
           05  TCTTE-SSN-X REDEFINES TCTTE-SSN  PIC X(11).              00000440
           05  TCTTE-TRLRNUM           PIC 99.                          00000480
           05  TCTTE-TRLRNUM-X  REDEFINES  TCTTE-TRLRNUM                00000490
                                       PIC XX.                          00000500
           05  TCTTE-TRLRSEQ           PIC 99.                          00000510
           05  TCTTE-EXIT-SW           PIC X.                           00000520
               88  RETURN-BACK         VALUE LOW-VALUES.                00000530
               88  XCTL-BACK           VALUE HIGH-VALUES.               00000540
           05  TCTTE-INDIC  REDEFINES  TCTTE-EXIT-SW                    00000550
                                       PIC X.                           00000560
           05  TCTTE-CODE              PIC X(3).                        00000570
           05  FILLER                  PIC X(49).                       00000580
       EJECT                                                            00000590
      ******************************************************************00000600
      *                                                                *00000610
      *        TCTTE USER AREA FOR USE ONLY BY TECHNICAL SUPPORT       *00000620
      *                                                                *00000630
      *  REMOVED FILLER X14 ADDED 2 CIV FLDS + FILLER X6  04/11/2009 RM*00000630
      ******************************************************************00000640
       SKIP2                                                            00000650
      *    05  FILLER                  PIC X(14).                       00000660
           05  TCTTE-CIV               PIC 9(12) COMP-3.                00000660
           05  TCTTE-DIS-SW            PIC X.                           00000660
           05  FILLER                  PIC X(6).                        00000660
           05  TCTTE-TERMTYP           PIC X.                           00000670
           05  TCTTE-DEPT              PIC XX.                          00000680
           05  TCTTE-CLERK             PIC XXX.                         00000690
       SKIP2                                                            00000700
         02  TCTTE-PROG-USER-AREA-R  REDEFINES  TCTTE-PROG-USER-AREA.   00000710
           05  FILLER                  PIC X(51).                       00000720


      *----------------------------------------------------------------*
      *                                               COPY SFCOMRG0    *
      *                   REX'S COMMUNICATION REGION                   *
      *                    ACCOUNTING INFORMATION                      *
      *                                                                *
      *    THIS MEMBER CONTAINS SEX MEMBERS IN ONE 01-LEVEL.           *
      *                                                                *
      *        1)  MESSAGE AREA                                        *
      *        2)  DEDUCTION AREA                                      *
      *        3)  OCCURANCE TABLE                        SFCOCCUR     *
      *        4)  ACCOUNTS INFORMATION TABLE             SFCRATBL     *
      *        5)  PEX ACCOUNTING TABLE                   SFCPEXRT     *
      *        6)  SYSTEM CONSTANTS                       SXCCOMSK     *
      *        7)  DISBURSEMENT EXTRACT WORK AREA         SFCWDISX     *
      *        8)  COUNTER-AREA                                        *
      *                                                                *
      ******************************************************************

       01  COMMUNICATION-REGION.

           05  C-MESSAGE-AREA.

               10  C-MESSAGE-CODE         PIC  S9(8) COMP.

           05  C-DEDUCTION-AREA.

               10  L-BAL-LI-AMT           PIC S9(6)V9(3) COMP-3.
               10  L-TOT-DED-TO-BE-ADDED  PIC S9(6)V9(3) COMP-3.
               10  L-GROSS-PENSION-AMT    PIC S9(6)V9(3) COMP-3.
               10  L-NEW-DED-TO-BE-ADDED  PIC S9(4)V9(3) COMP-3.

           COPY SFCOCCUR.                                               00000330

      *DB2 EJECT
      *DB2 COPY   SXCRATBL.                                             00000350
      *DB2 EJECT

           05  C-PEX-ACCOUNTING-TBL.

           COPY SFCPEXRT.                                               00000400

           COPY SXCCOMSK.                                               00000420

           COPY SFCWDISX.                                               00000440

           05  C-COUNTER-AREA.

               10  C-TXN-COUNT            PIC  S9(7) COMP-3.

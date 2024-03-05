      ***************************************************               00000010
      *                              COPY   SFCTTXNL    *               00000020
      *        STANDARD TRANSACTION LEAD                *               00000030
      *                                                 *               00000040
      *        TO BE USED WITH EXTERNAL TRANSACTIONS    *               00000050
      *        CREATED OUT OF ON-LINE AND ALSO WITH     *               00000060
      *        TRANSACTIONS CREATED INTERNALLY BY       *               00000070
      *        THE SYSTEM                               *               00000080
      ***************************************************               00000090
           SKIP2                                                        00000100
           05  TRANSACTION-LEAD.                                        00000110
               10  TRAN-LEAD-RECORD-TYPE   PIC  X(02).                  00000120
               10  TRAN-LEAD-KEY.                                       00000130
                   15  TRAN-LEAD-SSN-KEY   PIC  X(11).                  00000140
                   15  TRAN-LEAD-SSN-KEY-R REDEFINES                    00000140
                       TRAN-LEAD-SSN-KEY.                               00000140
                       25  TRAN-LEAD-FOUR-BYTES PIC X(04).
                       88  EMPLOYER-SSN    VALUE '0000'.
                       25  TRAN-LEAD-SEVN-BYTES PIC X(07).
                   15  TRAN-LEAD-SSN-KEY-9 REDEFINES                    00000140
                       TRAN-LEAD-SSN-KEY   PIC  9(11).                  00000140
                   15  TRAN-LEAD-EMPLYR    PIC  9(07).                  00000150
                   15  FILLER              PIC  X(22).                  00000150
               10  TRAN-LEAD-TRAN-CODE     PIC  X(04).                  00000160
               10  TRAN-LEAD-TRAN-DATE     PIC S9(08) COMP-3.           00000170
               10  TRAN-LEAD-TIME          PIC S9(07) COMP-3.           00000180
               10  TRAN-LEAD-TERM-ID       PIC  X(04).                  00000190
               10  TRAN-LEAD-DEPT-CLERK.                                00000200
                   15  TRAN-LEAD-DEPT      PIC  X(02).                  00000210
                   15  TRAN-LEAD-CLERK     PIC  X(03).                  00000220
               10  TRAN-LEAD-ACTION-CODE   PIC  X(02).                  00000230
               10  TRAN-LEAD-SOURCE        PIC  X(02).                  00000240
               10  TRAN-LEAD-DESTINATION   PIC  X(02).                  00000250
               10  TRAN-LEAD-BYPASS-CODE   PIC  X(02).                  00000260
               10  TRAN-LEAD-FORMAT-TYPE   PIC  X(02).                  00000270
               10  TRAN-LEAD-TXN-DATA-LENGTH   PIC S9(04) COMP.         00000280

      ***************************************************************** 00000010
      *               *** WORKING STORAGE CONSTANTS   ***             * 00000020
      *                                        COPY MEMBER - SFCWSCON * 00000030
      *    DESCRIPTION - WORKING STORAGE COMMONLY USED CONSTANTS      * 00000040
      ***************************************************************** 00000050
           05  K-RETURN-CODES.                                          00000060
               10  K-RET-GOOD              PIC S9(09) COMP VALUE +0.    00000070
               10  K-RET-WARNING           PIC S9(09) COMP VALUE +4.    00000080
               10  K-RET-TXN-ERR           PIC S9(09) COMP VALUE +8.    00000090
               10  K-RET-PURGE-TXN         PIC S9(09) COMP VALUE +12.   00000100
               10  K-RET-SYSTEM-ERR        PIC S9(09) COMP VALUE +16.   00000110
           05  K-SWITCH-VALUES.                                         00000120
               10  K-SWITCH-OFF            PIC  X(01)      VALUE '0'.   00000130
               10  K-SWITCH-ON             PIC  X(01)      VALUE '1'.   00000140
               10  K-SWITCH-HIGH           PIC  X(01)                   00000150
                                                    VALUE HIGH-VALUES.  00000160
           05  K-ACTION-CODES.                                          00000170
               10  K-ACTION-ADD            PIC  X(02)      VALUE '02'.  00000180
               10  K-ACTION-CHG            PIC  X(02)      VALUE '03'.  00000190
               10  K-ACTION-DEL            PIC  X(02)      VALUE '01'.  00000200
               10  K-ACTION-READ           PIC  X(02)      VALUE '04'.  00000210

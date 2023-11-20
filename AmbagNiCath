       IDENTIFICATION DIVISION.
       PROGRAM-ID. "ORDERINGSYSTEM".
       AUTHOR.     GROUP-5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT ORDER-FILE ASSIGN TO "ORDER-INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
            SELECT PRINT-FILE ASSIGN TO "RECEIPT-OUTPUT.DAT".

       DATA DIVISION.
       FILE SECTION.
         FD ORDER-FILE.
         01 CUSTOMER-DETAILS.
            88 END-RECEIPT VALUE HIGH-VALUES.
            02 CUSTOMER-NAME    PIC X(10).
            02 CUSTOMER-ADDRESS PIC X(15).
            02 FILLER           PIC X(2). 
            02 CUSTOMER-NUMBER  PIC 9(11).
         01 ORDER-DETAILS.
            02 FILLER           PIC X(2).
            02 CUSTOMER-ORDER1  PIC 99.
            02 FILLER           PIC X(2).
            02 ORDER-PCS1       PIC 99.
            02 CUSTOMER-ORDER2  PIC 99.
            02 FILLER           PIC X(2).
            02 ORDER-PCS2       PIC 99.
            02 FILLER           PIC X(2).
            02 ORDER-TOTAL      PIC 9(4)V99.
      
         FD PRINT-FILE.
         WORKING-STORAGE SECTION.
         01 HEADING-LINE.
            02 FILLER           PIC X(10) VALUE "CUSTOMER NAME".
            02 FILLER           PIC X(15) VALUE "ADRESS".
            02 FILLER           PIC X(15) VALUE "PHONE NUMBER".
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 FILLER           PIC X(2) VALUE "CHICKEN MEAL".
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 FILLER           PIC 9(2) VALUE "PIECES".
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 FILLER           PIC X(2) VALUE "PASTA MEAL".
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 FILLER           PIC 9(2) VALUE "PIECES".
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 FILLER           PIC X(11) VALUE "TOTAL".
      
         01 DETAIL-LINE.
            02 DET-NAME         PIC X(15) VALUE "N/A".
            02 DET-ADDRESS      PIC X(15).
            02 DET-CNUM         PIC X(15).
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 DET-ORDER1       PIC X(2).
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 DET-PCS1         PIC 9(2).
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 DET-ORDER2       PIC X(2).
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 DET-PCS2         PIC 9(2).
            02 FILLER           PIC X(2)  VALUE SPACES.
            02 FILLER           PIC X VALUE '₱'.
            02 DET-TOTAL        PIC 9(4)V99.
            
       PROCEDURE DIVISION.
       0050-OPEN-FILE.
           OPEN INPUT ORDER-FILE.
           OPEN OUTPUT PRINT-FILE.
           PERFORM 0100-PROCESS-RECEIPT.
           PERFORM 0400-STOP-RUN.
      
       0100-PROCESS-RECEIPT.
          PERFORM 0300-WRITE-HEADING-LINE.
          READ ORDER-FILE.
               AT END SET END-RECEIPT TO TRUE
               END READ.
          PERFORM 0200-CALCULATE-ORDER UNTIL END-RECEIPT.
      
       0200-CALCULATE-ORDER.
      * IKAW NA BAHALA DITU DAVE  

       0300-PRINT-HEADING-LINE.
           MOVE HEADING-LINE TO PRINT-LINE.
           MOVE SPACES TO PRINT-LINE.
           WRITE PRINT-LINE.
       0320-PRINT-DETAIL-LINE.
           MOVE DETAIL-LINE TO PRINT-LINE.
       0400-STOP-RUN.
           CLOSE ORDER-FILE.
           CLOSE PRINT-FILE.
      
           DISPLAY "███╗   ███╗███████╗███╗  ██╗██╗   ██╗██╗".
           DISPLAY "████╗ ████║██╔════╝████╗ ██║██║   ██║╚═╝".
           DISPLAY "██╔████╔██║█████╗  ██╔██╗██║██║   ██║   ".
           DISPLAY "██║╚██╔╝██║██╔══╝  ██║╚████║██║   ██║   ".
           DISPLAY "██║ ╚═╝ ██║███████╗██║ ╚███║╚██████╔╝██╗".
           DISPLAY "╚═╝     ╚═╝╚══════╝╚═╝  ╚══╝ ╚═════╝ ╚═╝".
           DISPLAY " ".
           DISPLAY "█▀▀ █░█ █ █▀▀ █▄▀ █▀▀ █▄░█  █▀▄▀█ █▀▀ ▄▀█ █░░ █▀ ▀".
           DISPLAY "█▄▄ █▀█ █ █▄▄ █░█ ██▄ █░▀█  █░▀░█ ██▄ █▀█ █▄▄ ▄█ ▄".
           DISPLAY "C1 - ₱120.00 1PC CHICKEN, 1PC RICE, REGULAR DRINK".
           DISPLAY "C2 - ₱180.50 2PCS CHICKEN, 1PC RICE, MEDIUM DRINK".
           DISPLAY "C3 - ₱210.90 3PCS CHICKEN, 1PC RICE, 1 MEDIUM FRIES, 1 LARGE DRINK".
           DISPLAY " ".
           DISPLAY "█▀█ ▄▀█ █▀ ▀█▀ ▄▀█ ▀".
           DISPLAY "█▀▀ █▀█ ▄█ ░█░ █▀█ ▄".
           DISPLAY "P1 - ₱160.25 1PC CHICKEN, SPAGHETTI, REGULAR DRINK".
           DISPLAY "P2 - ₱200.00 1PC CHICKEN, SPAGHETTI, REGULAR FRIES, MEDIUM DRINK".
           DISPLAY "P3 - ₱370.95 6PCS CHICKEN NUGGETS, SPAGHETTI, CHEESE BURGER, LARGE DRINK".
           DISPLAY " ".
       
      STOP RUN.
      END PROGRAM ORDERINGSYSTEM.
